! 7.2.5 Heat equation in 2D
! Parallel version of the serial code.  Run it with `-np 4`.
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace heat_parallel.f90
! $ mpirun -np 4 --oversubscribe ./a.out < in.txt
program heat_equation
    use mpi_f08
    implicit none
    type(MPI_Comm)      :: comm = MPI_COMM_WORLD
    type(MPI_Status)    :: status
    type(MPI_Datatype)  :: block_t, col_t, row_t
    integer :: my_rank, n_ranks, top_rank, bottom_rank, left_rank, right_rank, dst
    integer :: side, height, width, l, time
    integer :: n_rows, row, r, n_cols, col, c
    real, parameter :: alpha = 2.5e-4
    real :: diff_t, global_diff_t
    real, pointer :: prev_t(:, :), next_t(:, :), temp_t(:, :)
    real, allocatable :: t_global(:, :)

    call MPI_Init()
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    ! Create the topology.  The order is the same as in Fortran arrays, from top
    ! to bottom, from left to right.
    !       col=0 col=1
    ! row=0   r=0   r=2
    ! row=1   r=1   r=3
    n_rows = 2
    n_cols = 2
    if (n_rows * n_cols /= n_ranks) then
        call MPI_Abort(comm, MPI_ERR_TOPOLOGY)
        stop "Run this code with `-np 4`."
    end if
    row = mod(my_rank, n_rows)
    col = my_rank / n_rows

    ! Define neighbor ranks.  If they fall out of the allowed ranges, make them
    ! MPI_PROC_NULL.
    top_rank    = get_rank(row - 1, col)
    bottom_rank = get_rank(row + 1, col)
    left_rank   = get_rank(row, col - 1)
    right_rank  = get_rank(row, col + 1)

    ! Read in the array side and distribute it between processes.
    if (my_rank == 0) read *, side
    call MPI_Bcast(side, 1, MPI_INTEGER, 0, comm)
    height = side / n_rows
    width  = side / n_cols
    ! Allocate the local temperature arrays.
    allocate(prev_t(0:height + 1, 0:width + 1))
    allocate(next_t(0:height + 1, 0:width + 1))

    !print '(3(a, i0))', "Rank ", my_rank, ", height=", height, ", width=", width

    ! Read the input array and distribute.
    if (my_rank == 0) then
        allocate(t_global(0:side + 1, 0:side + 1))
        do l = 0, side + 1
            read *, t_global(l, 0:side + 1)
        end do
        call MPI_Type_vector(width + 2, height + 2, side + 2, MPI_REAL, block_t)
        call MPI_Type_commit(block_t)
        do r = 0, n_rows - 1
            do c = 0, n_cols - 1
                dst = get_rank(r, c)
                ! Standard send works in buffered mode only for small arrays
                ! with less than 1010 elements.
                call MPI_Send(t_global(r * height, c * width), 1, block_t, dst, 0, comm)
            end do
        end do
        call MPI_Type_free(block_t)
        deallocate(t_global)
    end if
    ! Receive sub-arrays. You can use the same type name as they don't overlap.
    call MPI_Type_vector(width + 2, height + 2, height + 2, MPI_REAL, block_t)
    call MPI_Type_commit(block_t)
    call MPI_Recv(prev_t(0, 0), 1, block_t, 0, 0, comm, status)
    call MPI_Type_free(block_t)
    ! B.C. won't change during interatinos, next_t must have the same as prev_t
    ! has after reading them from the input file.  Just copy the entire array
    ! for simplicity.
    next_t(:, :) = prev_t(:, :)

    ! Create derived types for columns and rows to communicate during the solution.
    call MPI_Type_contiguous(height, MPI_REAL, col_t)
    call MPI_Type_commit(col_t)
    call MPI_Type_vector(width + 2, 1, height + 2, MPI_REAL, row_t)
    call MPI_Type_commit(row_t)
    time = 0
    do
        next_t(1:height, 1:width) = &
            (1.0 - 4.0 * alpha) *   prev_t(1:height   ,  1:width    ) &
                       + alpha  * ( prev_t(0:height - 1, 1:width    ) &
                                  + prev_t(2:height + 1, 1:width    ) &
                                  + prev_t(1:height    , 0:width - 1) &
                                  + prev_t(1:height    , 2:width + 1) )
        call update_ghost()
        diff_t = maxval(abs(next_t(1:height, 1:width) - prev_t(1:height, 1:width)))
        call MPI_Allreduce(diff_t, global_diff_t, 1, MPI_REAL, MPI_MAX, comm)
        if (mod(time, 10000) == 0) then
            if (my_rank == 0) then
                print '(a, i0, a, es12.5)', "time = ", time, ", diff_t = ", global_diff_t
            end if
            call print_t()
        end if
        if (global_diff_t < epsilon(0.0)) exit
        temp_t => prev_t
        prev_t => next_t
        next_t => temp_t
        time = time + 1
    end do
    call MPI_Type_free(col_t)
    call MPI_Type_free(row_t)

    if (my_rank == 0) print '(a, i0)', "Final time is ", time
    deallocate(prev_t)
    deallocate(next_t)

    call MPI_Finalize()

contains
    !---------------------------------------------------------------------------
    function get_rank(row, col)
        implicit none
        integer :: get_rank
        integer, intent(in) :: row, col

        if (0 <= col .and. col < n_cols .and. 0 <= row .and. row < n_rows) then
            get_rank = row + col * n_rows
        else
            get_rank = MPI_PROC_NULL
        end if
        return
    end function get_rank
    !---------------------------------------------------------------------------
    subroutine print_t()
        implicit none
        integer :: rank, row
        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print '(a, i0)', "Rank ", my_rank
                do row = 0, height + 1
                    print '(*(f4.0, 1x))', next_t(row, :)
                end do
            end if
            call MPI_Barrier(comm)
        end do
    end subroutine print_t
    !---------------------------------------------------------------------------
    subroutine update_ghost()
        implicit none
        ! Columns
        call MPI_Sendrecv(&
            next_t(1,         1), 1, col_t, left_rank,  0, &
            next_t(1, width + 1), 1, col_t, right_rank, 0, comm, status)
        call MPI_Sendrecv(&
            next_t(1, width), 1, col_t, right_rank, 0, &
            next_t(1,     0), 1, col_t, left_rank,  0, comm, status)
        ! Rows
        call MPI_Sendrecv(&
            next_t(         1, 0), 1, row_t, top_rank,    0, &
            next_t(height + 1, 0), 1, row_t, bottom_rank, 0, comm, status)
        call MPI_Sendrecv(&
            next_t(height, 0), 1, row_t, bottom_rank,   0, &
            next_t(     0, 0), 1, row_t, top_rank, 0, comm, status)
    end subroutine update_ghost

end program heat_equation
