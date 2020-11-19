program main
    use mpi_f08
    integer :: my_rank, n_ranks, first, last
    integer :: x = 0
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    first = 0
    last  = n_ranks - 1
    print "(2(a, i0))", "Before: rank ", my_rank, ", x = ", x
    if (my_rank == first) then
        x = 9
        call MPI_Send(x, 1, MPI_INTEGER, last, 0, MPI_COMM_WORLD)
    else if (my_rank == last) then
        call MPI_Recv(x, 1, MPI_INTEGER, first, 0, MPI_COMM_WORLD, status)
    end if
    print "(2(a, i0))", "After:  rank ", my_rank, ", x = ", x
    call MPI_Finalize()
end program main
