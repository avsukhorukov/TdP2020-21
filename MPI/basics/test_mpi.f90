program main
    use mpi_f08
    call MPI_Init()
    print *, "test"
    call MPI_Finalize()
end program main
