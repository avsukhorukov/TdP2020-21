! Run with -np 5
! Enter 5 or 10
program alltoall
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks
    type(MPI_Comm)   :: comm = MPI_COMM_WORLD
    integer :: s, i, count
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    ! s is the size of both the receive buffer and the send buffer.
    if (my_rank == 0) read *, s
    call MPI_Bcast(s, 1, MPI_INTEGER, 0, comm)
    allocate(sbuf(s))
    sbuf(:) = [ (10 * my_rank + i, i = 1, s) ]
    allocate(rbuf(s), source=0)

    count = s / n_ranks

    call print_vals()
    call MPI_Alltoall(sbuf, count, MPI_INTEGER, rbuf, count, MPI_INTEGER, comm)
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

end program alltoall
