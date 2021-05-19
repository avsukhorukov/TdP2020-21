! Run with -np 5.
program broadcast
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    type(MPI_Comm) :: comm
    integer :: s, i
    integer, allocatable :: a(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Size s must be zero on all but rank 0, and then broadcast it.  Show it
    ! before and after.
    s = 0
    !call print_vals()
    if (my_rank == root) read *, s
    !call print_vals()
    call MPI_Bcast(s, 1, MPI_INTEGER, root, comm)
    !call print_vals()

    ! Allocate an array with size `s` on rank 0, initialize it to an arithmeric
    ! progression, and broadcast to other ranks.
    allocate(a(s), source=0)
    if (my_rank == root) a(:) = [ (i, i = 1, s) ]
    call print_vals()
    call MPI_Bcast(a(3), 2, MPI_INTEGER, root, comm) ! try a(2), 2 and so on.
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
                ! 1st: print `s` only
                print '(a, i0, a, i0)', "Rank ", r, ", s=", s
                ! 2nd: print `a(s)` only.
                !print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", a(:)=", a
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_vals
end program broadcast
