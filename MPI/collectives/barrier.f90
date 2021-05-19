! Run with -np 5.
program barrier
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, r
    type(MPI_Comm)   :: comm
    type(MPI_Status) :: status
    integer :: s, i
    integer, allocatable :: a(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    s = 0
    if (my_rank == 0) then
        read *, s
        do r = 0, n_ranks - 1
            call MPI_Send(s, 1, MPI_INTEGER, r, 0, comm)
        end do
    end if
    call MPI_Recv(s, 1, MPI_INTEGER, 0, 0, comm, status)
    allocate(a(s), source=0)
    if (my_rank == 0) then
        a(:) = [ (10 * my_rank + i, i = 1, s) ]
    end if

    call print_vals()

    if (allocated(a)) deallocate(a)
    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    subroutine print_vals()
        implicit none
        integer :: r

        do r = 0, n_ranks - 1
            if (r == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", a(:)=", a
            end if
            call MPI_Barrier(comm)
        end do
    end subroutine print_vals
end program barrier
