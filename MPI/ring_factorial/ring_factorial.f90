! 6.2.2 Ring data passing (ring factorial).
! Note: I've modified this problem from the old Ángel's assignments.  Now all
! ranks must multiply so if 0 reads 1, then last rank n - 1 will send back to
! rank 0 exactly the number of n!.
!
! 1) Rank 0 reads a number.
! 2) Each rank r sends to rank r + 1 the received number multiplied by r + 1,
!    except the last process with rank = n - 1.
! 3) Last rank n - 1 sends the result back to rank 0, which prints it.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace ring_factorial.f90
! $ mpirun -np 6 --oversubscribe ./a.out
program ring_factorial
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, src, dst, tag
    integer :: factorial
    type(MPI_Status) :: status
    type(MPI_Comm) :: comm

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    ! Students often code this as (even in serious MPI courses people do this so
    ! stupidly):
    !   src = r - 1; if (src == -1) src = n_r - 1
    !   dst = r + 1; if (dst == nr) dst = 0
    ! Don't do this way, use the modulo n division with the mod() function.
    ! This is the standard way to do this in a ring topology.
    src = mod(n_ranks + my_rank - 1, n_ranks)
    dst = mod(n_ranks + my_rank + 1, n_ranks)
    tag = 0

    ! Read the number on rank 0 or receive it on other ranks.
    if (my_rank == 0) then
        read *, factorial
    else
        call MPI_Recv(factorial, 1, MPI_REAL, src, tag, comm, status)
    end if

    ! All ranks multiply the number.
    factorial = factorial * (my_rank + 1)

    ! Send the number to the next rank in the ring.
    call MPI_Send(factorial, 1, MPI_REAL, dst, tag, comm)

    ! Now rank 0 receives the result and prints it.
    if (my_rank == 0) then
        call MPI_Recv(factorial, 1, MPI_REAL, src, tag, comm, status)
        print '(i0)', factorial
    end if

    call MPI_Finalize()
end program ring_factorial
