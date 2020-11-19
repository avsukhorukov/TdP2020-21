! 6.2.3 Parallel sum
! Write a program to compute the sum of a 1D array.
! 1) Rank 0 read the size of a 1D array, it allocates the array and reads it in.
!    The array size must be divisible by the number of processes.
! 2) Rank 0 finds the number of array elements each process must receive, and
!    sends this number to every other rank as well as the data.
! 3) Each rank receives the data, stores it in a temporary array and calculates
!    the partial sum, then sends it back to rank 0.
! 4) Rank 0 calculates the total sum and prints the result.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace parallel_sum.f90
! $ mpirun -np 8 --oversubscribe ./a.out
program parallel_sum
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, src, dst
    integer :: n, n_r, total_sum, sumr
    integer, allocatable :: arr(:), arr_r(:)
    type(MPI_Status) :: status
    type(MPI_Comm) :: comm = MPI_COMM_WORLD

    call MPI_Init()
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    if (my_rank == 0) then
        read *, n
        n_r = n / n_ranks
        do dst = 1, n_ranks - 1
            call MPI_Send(n_r, 1, MPI_INTEGER, dst, 0, comm)
        end do
        allocate(arr_r(n_r))
        allocate(arr(n))
        read *, arr(:)
        arr_r(1:n_r) = arr(1:n_r)
        do dst = 1, n_ranks - 1
            call MPI_Send(arr(n_r * dst + 1), n_r, MPI_INTEGER, dst, 0, comm)
        end do
        deallocate(arr)
        total_sum = sum(arr_r(:))
        deallocate(arr_r)
        do src = 1, n_ranks - 1
            call MPI_Recv(sumr, 1, MPI_INTEGER, src, 0, comm, status)
            total_sum = total_sum + sumr
        end do
        print '(i0)', total_sum
    else ! my_rank /= 0
        call MPI_Recv(n_r, 1, MPI_INTEGER, 0, 0, comm, status)
        allocate(arr_r(n_r))
        call MPI_Recv(arr_r, n_r, MPI_INTEGER, 0, 0, comm, status)
        sumr = sum(arr_r(:))
        call MPI_Send(sumr, 1, MPI_INTEGER, 0, 0, comm)
        deallocate(arr_r)
    end if

    call MPI_Finalize()
end program parallel_sum