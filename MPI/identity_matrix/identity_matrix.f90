! 6.2.5 Identity matrix distribution
! Write a program that distributes an identity matrix over the processes.  The
! dimension and the number of processes are given.  Distribute by rows
! (Fortran).
! The program reads the matrix size S, takes the number of processes N, and
! calculates how many columns (with remainder) must take every process.
! You can either operate with local or global indices on each process.  At the
! end, rank 0 collects parts of the matrix from other ranks and prints the
! entire thing.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -fcheck=bounds -fbacktrace identity_matrix.f90
! $ mpirun -np 3 --oversubscribe ./a.out
! Enter size 7.
program identity_matrix
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, top_rank, bottom_rank
    type(MPI_Status) :: status
    integer :: remainder, n_rows, n_rows_r, row, row_s, row_e, i, r
    integer :: s ! matrix size
    integer, allocatable :: e(:, :)
    integer, allocatable :: e_big(:, :)
    !type(MPI_Datatype) :: np1_rows_t
    !type(MPI_Datatype) :: n_rows_t
    integer(kind=MPI_ADDRESS_KIND) :: lb, integer_extent
    type(MPI_Datatype) :: row_t
    type(MPI_Datatype) :: row_resized_t

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    if (my_rank == 0) then
        read *, s
        remainder = mod(s, n_ranks)
        n_rows    = (s - remainder) / n_ranks
        do r = 0, n_ranks - 1
            ! Let first processes have one extra column for even division.
            if (r < remainder) then
                n_rows_r = n_rows + 1
            else
                n_rows_r = n_rows
            end if
            call MPI_Bsend(s,        1, MPI_INTEGER, r, 0, MPI_COMM_WORLD)
            call MPI_Bsend(n_rows_r, 1, MPI_INTEGER, r, 0, MPI_COMM_WORLD)
        end do
    end if ! my_rank == 0
    call MPI_Recv(s,      1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status)
    call MPI_Recv(n_rows, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status)

    ! Inefficient method with three branches:
    !if (my_rank == 0) then
    !    row = 1
    !    row_s = row
    !    row_e = row + n_rows - 1
    !    row = row_e + 1
    !    call MPI_Send(row, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD)
    !else if (0 < my_rank .and. my_rank < n_ranks - 1) then
    !    call MPI_Recv(row, 1, MPI_INTEGER, my_rank - 1, 0, MPI_COMM_WORLD, status)
    !    row_s = row
    !    row_e = row + n_rows - 1
    !    row = row_e + 1
    !    call MPI_Send(row, 1, MPI_INTEGER, my_rank + 1, 0, MPI_COMM_WORLD)
    !else ! n_ranks - 1
    !    call MPI_Recv(row, 1, MPI_INTEGER, my_rank - 1, 0, MPI_COMM_WORLD, status)
    !    row_s = row
    !    row_e = row + n_rows - 1
    !end if

    ! More efficient method using MPI_PROC_NULL to suppress unnecessary calls.

    top_rank = my_rank - 1
    if (top_rank == -1) top_rank = MPI_PROC_NULL
    bottom_rank = my_rank + 1
    if (bottom_rank == n_ranks) bottom_rank = MPI_PROC_NULL

    row = 1
    call MPI_Recv(row, 1, MPI_INTEGER, top_rank, 0, MPI_COMM_WORLD, status)
    row_s = row
    row_e = row + n_rows - 1
    row = row_e + 1
    call MPI_Send(row, 1, MPI_INTEGER, bottom_rank, 0, MPI_COMM_WORLD)

    ! Initialize the local matrix.
    allocate(e(row_s:row_e, s), source=0)
    do row = row_s, row_e
        e(row, row) = 1
    end do

    ! Define the strided type that will contain all local rows.  On all ranks it
    ! has n_rows.  On first ranks < R it has one extra row and on last ranks
    ! >= R it has one less.  When sending to rank~0 this will always work
    ! because the receive can get less than the specified count.
    ! Method 2: define a row type and resize its extent to be of 1 integer size.
    ! With such a type a contiguous type will exactly follow the row order.
    call MPI_Type_get_extent(MPI_INTEGER, lb, integer_extent)
    if (my_rank == 0) then
        ! Method 1:
        !call MPI_Type_vector(s, n_rows, s, MPI_INTEGER, np1_rows_t)
        !call MPI_Type_commit(np1_rows_t)
        !if (remainder /= 0) then
        !    call MPI_Type_vector(s, n_rows - 1, s, MPI_INTEGER, n_rows_t)
        !    call MPI_Type_commit(n_rows_t)
        !end if
        ! Method 2:
        call MPI_Type_vector(s, 1, s, MPI_INTEGER, row_t)
        call MPI_Type_create_resized(row_t, lb, integer_extent, row_resized_t)
        call MPI_Type_commit(row_resized_t)
    else
        ! On rank /= 0 this row type is for the local e array.
        call MPI_Type_vector(s, 1, n_rows, MPI_INTEGER, row_t)
        call MPI_Type_create_resized(row_t, lb, integer_extent, row_resized_t)
        call MPI_Type_commit(row_resized_t)
    end if

    if (my_rank == 0) then
        allocate(e_big(s, s))
        ! Copy from e to e_big first slice.
        e_big(row_s:row_e, :) = e(row_s:row_e, :)
        ! Collect the other slices from other processes.
        do r = 1, n_ranks - 1
            ! Get row_s from other ranks.
            call MPI_Recv(row_s, 1, MPI_INTEGER, r, 0, MPI_COMM_WORLD, status)
            ! Method 1 (using two types):
            !if (remainder /= 0 .and. r >= remainder) then
            !    call MPI_Recv(e_big(row_s, 1), 1, n_rows_t, r, 0, MPI_COMM_WORLD, status)
            !else
            !    call MPI_Recv(e_big(row_s, 1), 1, np1_rows_t, r, 0, MPI_COMM_WORLD, status)
            !end if
            ! Method 2 (using one resized row and contiguous type):
            call MPI_Recv(e_big(row_s, 1), n_rows + 1, row_resized_t, r, 0, MPI_COMM_WORLD, status)
        end do
    else
        call MPI_Send(row_s, 1,         MPI_INTEGER, 0, 0, MPI_COMM_WORLD)
        !call MPI_Send(e,     n_rows * s, MPI_INTEGER, 0, 0, MPI_COMM_WORLD)     ! Method 1: send all elements.
        call MPI_Send(e(row_s, 1), n_rows, row_resized_t, 0, 0, MPI_COMM_WORLD) ! Method 2: send resized rows.
    end if

    if (my_rank == 0) then
        do row = 1, s
            print '(*(i0, 1x))', e_big(row, :)
        end do
        deallocate(e_big)
    end if

    ! Method 1: release both types.
    !if (my_rank == 0) then
    !    call MPI_Type_free(np1_rows_t)
    !    if (remainder /= 0) then
    !        call MPI_Type_free(n_rows_t)
    !    end if
    !end if
    !call MPI_Type_free(n_rows_t)
    !call MPI_Type_free(np1_rows_t)
    ! Method 2: release only the resized row type.
    call MPI_Type_free(row_resized_t)

    deallocate(e)
    call MPI_Finalize()
end program identity_matrix
