program three_sends
    use mpi
    implicit none
    integer :: my_rank, n_ranks, ierr
    integer :: status(MPI_STATUS_SIZE)
    integer :: a, b, c

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    if (my_rank == 0) then
        call MPI_Recv(a, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, status, ierr)
        call MPI_Recv(b, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, status, ierr)
        call MPI_Recv(c, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, status, ierr)
        print '(3(i0, 1x))', a, b, c
    else if (my_rank == 1) then
        call MPI_Send(1, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
        call MPI_Send(2, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
        call MPI_Send(3, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
    end if

    call MPI_Finalize(ierr)
end program three_sends