program mpi_ranks
    use mpi_f08 ! mpi
    implicit none
    ! integer :: err
    integer :: n_ranks, my_rank

    call MPI_Init() ! (err)

    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks) ! , err)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank) ! , err)

    print '(2(a, i0))', "Rank ", my_rank, " of ", n_ranks

    call MPI_Finalize() ! (err)
end program mpi_ranks