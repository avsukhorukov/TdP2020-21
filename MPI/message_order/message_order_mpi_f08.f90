! The idea comes from Sect. 2.4.3 of Snir et al.
! Run with at least -np 3.
program message_order
    use mpi_f08
    implicit none
    integer, parameter :: N = 5
    integer :: my_rank, n_ranks
    type(MPI_Status) :: status
    integer :: a(N), b(N)

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    a = my_rank ! variable to send
    if (my_rank == 0) then
        call MPI_Recv(b, N, MPI_INTEGER, MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, status)
        print '(a, i0)', "Recv 1: src=*, tag=1, b=", b
        call MPI_Recv(b, N, MPI_INTEGER, MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, status)
        print '(a, i0)', "Recv 2: src=*, tag=1, b=", b
        call MPI_Recv(b, N, MPI_INTEGER, 2, MPI_ANY_TAG, MPI_COMM_WORLD, status)
        print '(a, i0)', "Recv 3: src=2, tag=*, b=", b
        call MPI_Recv(b, N, MPI_INTEGER, 2, MPI_ANY_TAG, MPI_COMM_WORLD, status)
        print '(a, i0)', "Recv 4: src=2, tag=*, b=", b
        call MPI_Recv(b, N, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status)
        print '(a, i0)', "Recv 5: src=*, tag=*, b=", b
    else if (my_rank == 1) then
        call MPI_Send(a, N, MPI_INTEGER, 0, 1, MPI_COMM_WORLD)
        call MPI_Send(a, N, MPI_INTEGER, 0, 4, MPI_COMM_WORLD)
    else if (my_rank == 2) then
        call MPI_Send(a, N, MPI_INTEGER, 0, 1, MPI_COMM_WORLD)
        call MPI_Send(a, N, MPI_INTEGER, 0, 2, MPI_COMM_WORLD)
        call MPI_Send(a, N, MPI_INTEGER, 0, 3, MPI_COMM_WORLD)
    end if

    call MPI_Finalize()
end program message_order
