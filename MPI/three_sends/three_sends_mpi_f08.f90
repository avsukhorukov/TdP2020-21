program tree_sends
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks
    type(MPI_Status) :: status
    integer :: a, b, c

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    if (my_rank == 0) then
        call MPI_Recv(a, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, status)
        call MPI_Recv(b, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, status)
        call MPI_Recv(c, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, status)
        print '(3(i0, 1x))', a, b, c
    else if (my_rank == 1) then
        call MPI_Send(1, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD)
        call MPI_Send(2, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD)
        call MPI_Send(3, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD)
    end if

    call MPI_Finalize()
end program tree_sends