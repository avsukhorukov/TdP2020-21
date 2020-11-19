! 6.2.4 Matrix distribution
! Distribute a global N x N matrix over P processes so that each process has a
! local portion of it in its memory.  Initialize such a portion with the process
! rank.  N is not necessarily divisible by P.
! Each process sends its first and last columns (in F) to neighbors: the fist to
! the left rank, the last to the right rank.  Each process must allocate extra
! ghost cells with two columns.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -fcheck=bounds -fbacktrace parallel_sum.f90
! $ mpirun -np 3 --oversubscribe ./a.out
! Enter size 10.
program matrix_distribution
    use mpi_f08
    implicit none

    integer, parameter :: N = 10
    integer :: my_rank, n_ranks, left_rank, right_rank, r
    type(MPI_Status) :: status
    type(MPI_Comm) :: comm = MPI_COMM_WORLD
    integer :: remainder, n_col, row
    integer, allocatable :: matrix(:, :)

    call MPI_Init()
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    left_rank  = mod(n_ranks + my_rank - 1, n_ranks)
    right_rank = mod(n_ranks + my_rank + 1, n_ranks)

    remainder = mod(N, n_ranks)
    n_col     = (N - remainder) / n_ranks ! this is the quotient
    ! First processes have one extra column for even division.
    if (my_rank < remainder) then
        n_col = n_col + 1
    end if

    allocate(matrix(N, 0:n_col + 1))
    matrix(:, :) = my_rank

    ! If you don't use MPI_Sendrecv but use a pair of MPI_Send--MPI_Recv, then
    ! you must take care of a possible deadlock yourself.
    call MPI_Sendrecv(                                       &
        matrix(1,         1), N, MPI_INTEGER, left_rank,  0, &
        matrix(1, n_col + 1), N, MPI_INTEGER, right_rank, 0, comm, status)
    call MPI_Sendrecv(                                   &
        matrix(1, n_col), N, MPI_INTEGER, right_rank, 1, &
        matrix(1,     0), N, MPI_INTEGER, left_rank,  1, comm, status)

    do r = 0, n_ranks - 1
        if (r == my_rank) then
            print '(a, i0)', "Rank ", my_rank
            do row = 1, N
                print '(*(i0, 2x))', matrix(row, :)
            end do
        end if
        call MPI_Barrier(comm)
    end do

    deallocate(matrix)
    call MPI_Finalize()
end program matrix_distribution
