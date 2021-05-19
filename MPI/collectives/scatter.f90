! Run with -np 5 or 10
program scatter
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    type(MPI_Comm) :: comm
    integer :: s, i, count
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Allocate sbuf(s) only on rank 0 to scatter it later to all other ranks.
    s = 0
    if (my_rank == root) read *, s
    allocate(sbuf(s))
    if (my_rank == root) sbuf(:) = [ (i, i = 1, s) ]

    ! After broadcasting, create sbuf(s / n) and set it to 0 on all ranks.
    call MPI_Bcast(s, 1, MPI_INTEGER, root, comm)
    count = s / n_ranks
    allocate(rbuf(count), source=0)

    call print_vals()
    call MPI_Scatter(sbuf, count, MPI_INTEGER, rbuf, count, MPI_INTEGER, root, comm)
    call print_vals()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)
    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    subroutine print_vals()
        implicit none
        integer :: r

        if (my_rank == 0) print *
        do r = 0, n_ranks - 1
            if (r == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", sbuf(:)=", sbuf
            end if
            call MPI_Barrier(comm)
        end do
        do r = 0, n_ranks - 1
            if (r == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", rbuf(:)=", rbuf
            end if
            call MPI_Barrier(comm)
        end do
    end subroutine print_vals
end program scatter
