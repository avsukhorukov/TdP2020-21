! Run with -np 5 and s = 5, better use MPI_SUM or MPI_PROD.
program reduce
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    type(MPI_Comm) :: comm
    integer, allocatable :: rbuf(:), sbuf(:)
    integer :: i, s

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    s = 0
    if (my_rank == root) read *, s
    allocate(rbuf(s), source=0)

    call MPI_Bcast(s, 1, MPI_INTEGER, root, comm)

    allocate(sbuf(s))
    sbuf(:) = [ (10 * my_rank + i, i = 1, s) ]

    call print_vals()
    call MPI_Reduce(sbuf, rbuf, s, MPI_INTEGER, MPI_SUM, root, comm)
    !call MPI_Reduce(sbuf, rbuf, s, MPI_INTEGER, MPI_PROD, root, comm)
    !call MPI_Reduce(sbuf, rbuf, s, MPI_INTEGER, MPI_MAX, root, comm)
    call print_vals()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)
    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    subroutine print_vals()
        implicit none
        integer :: rnk

        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print '(a, i0, a, *(i7, 1x))', "Rank ", rnk, ", sbuf(:)=", sbuf
            end if
            call MPI_Barrier(comm)
        end do
        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print '(a, i0, a, *(i7, 1x))', "Rank ", rnk, ", rbuf(:)=", rbuf
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_vals
end program reduce
