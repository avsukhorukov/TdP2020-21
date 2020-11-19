! Compile:
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace main.f90
!
! Run:
! $ mpirun -np 4 --oversubscribe ./a.out
program mpi_ranks
    use mpi
    implicit none
    integer, parameter :: MSG_SIZE = 30
    integer :: n_ranks, my_rank, first, last, src, msg_len
    type(MPI_Status) :: status
    character(len=MSG_SIZE) :: message

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    first = 0
    last  = n_ranks - 1
    if (my_rank == first) then
        do src = 1, last
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, src, 0, MPI_COMM_WORLD, status)
            call MPI_Get_count(status, MPI_CHARACTER, msg_len)
            print '(a)', message(1:msg_len) // "|"
        end do
    else ! my_rank /= first
        write(message, '(a, i0)') "Greetings from rank ", my_rank
        msg_len = len_trim(message)
        call MPI_Ssend(message, msg_len, MPI_CHARACTER, first, 0, MPI_COMM_WORLD)
    end if
    call MPI_Finalize()
end program mpi_ranks