program main
    use mpi_f08
    !call MPI_Init()
    print *, MPI_COMM_WORLD
    print *, MPI_ANY_TAG
    print *, MPI_INTEGER
    print *, MPI_REAL
    !call MPI_Finalize()
end program main
