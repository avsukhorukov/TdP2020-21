! Run with -np 5 and s = 5, better use MPI_SUM or MPI_PROD.
program scan
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks
    type(MPI_Comm) :: comm = MPI_COMM_WORLD
    integer :: value, total

    call MPI_Init()
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    value = 1
    total = 0
    call print_vals()
    !call MPI_Scan(value, total, 1, MPI_INTEGER, MPI_SUM, comm)
    call MPI_Exscan(value, total, 1, MPI_INTEGER, MPI_SUM, comm)
    call print_vals()

    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    subroutine print_vals()
        implicit none
        integer :: rnk

        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print '(a, i0, 2(a, i4))', "Rank ", rnk, ", value=", value, ", total=", total
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_vals
end program scan
