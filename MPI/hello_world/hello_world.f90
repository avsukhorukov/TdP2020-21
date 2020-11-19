! Compile:
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace hello_world.f90
!
! Run:
!
! $ mpirun -np 4 --oversubscribe ./a.out
program hello_world
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank

    call MPI_Init()

    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    if (my_rank == 0) then
        print '(2(a, i0))', "Master rank is ", my_rank, " of ", n_ranks
    else
        print '(a, i0)', "Other rank is ", my_rank
    end if

    call MPI_Finalize()
end program hello_world