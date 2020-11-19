! Run this with -np 2.
!
! When running into a deadlock, demonstrate that the program has stopped at the
! same lines with MPI_Send.  Use two gdb sessions in separate terminals and
! attach them to the two running processes.  The backtrace will show that they
! are waiting inside the libopenmpi library.  Choose the right frame and go back
! to the program.
program deadlock
    use mpi_f08
    implicit none
    integer, parameter :: N = 10
    integer :: my_rank, n_ranks, tag
    integer, allocatable :: a(:), b(:)
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    allocate(a(N), source=my_rank)
    allocate(b(N), source=-1)

    tag = 0
    ! No deadlock.
    if (my_rank == 0) then
        call MPI_Send(a, N, MPI_INTEGER, n_ranks - 1, tag, MPI_COMM_WORLD)
        call MPI_Recv(b, N, MPI_INTEGER, n_ranks - 1, tag, MPI_COMM_WORLD, status)
        print '(a)', "Case I, rank 0 done."
    else if (my_rank == n_ranks - 1) then
        call MPI_Recv(b, N, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, status)
        call MPI_Send(a, N, MPI_INTEGER, 0, tag, MPI_COMM_WORLD)
        print '(a)', "Case I, rank last done."
    end if

    ! Exchange that relies on buffering.
    ! Deadlock at N > 1010 (4040 b).
    if (my_rank == 0) then
        call MPI_Send(a, N, MPI_INTEGER, n_ranks - 1, tag, MPI_COMM_WORLD)
        call MPI_Recv(b, N, MPI_INTEGER, n_ranks - 1, tag, MPI_COMM_WORLD, status)
        print '(a)', "Case II, rank 0 done."
    else if (my_rank == n_ranks - 1) then
        call MPI_Send(a, N, MPI_INTEGER, 0, tag, MPI_COMM_WORLD)
        call MPI_Recv(b, N, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, status)
        print '(a)', "Case II, rank last done."
    end if

    ! Total deadlock.
    if (my_rank == 0) then
        call MPI_Recv(b, N, MPI_INTEGER, n_ranks - 1, tag, MPI_COMM_WORLD, status)
        call MPI_Send(a, N, MPI_INTEGER, n_ranks - 1, tag, MPI_COMM_WORLD)
        print '(a)', "Case III, rank 0 done."
    else if (my_rank == n_ranks - 1) then
        call MPI_Recv(b, N, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, status)
        call MPI_Send(a, N, MPI_INTEGER, 0, tag, MPI_COMM_WORLD)
        print '(a)', "Case III, rank last done."
    end if

    deallocate(a)
    deallocate(b)
    call MPI_Finalize()
end program deadlock