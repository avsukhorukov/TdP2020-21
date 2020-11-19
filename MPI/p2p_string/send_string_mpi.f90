! Compile:
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace main.f90
!
! Run:
! $ mpirun -np 4 --oversubscribe ./a.out
!
! Here the old Fortran bindings (use mpi) are used to trace this session in TAU
! profiler.
program mpi_ranks
    use mpi
    implicit none
    integer, parameter :: MSG_SIZE = 30
    integer :: n_ranks, my_rank, first, last, src, ierr, msg_len
    integer :: status(MPI_STATUS_SIZE)
    character(len=MSG_SIZE) :: message

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

    first = 0
    last  = n_ranks - 1
    if (my_rank == first) then
        do src = 1, last
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, src, 0, MPI_COMM_WORLD, status, ierr)
            call MPI_Get_count(status, MPI_CHARACTER, msg_len, ierr)
            print '(2a)', message(1:msg_len), "|"
        end do
    else ! my_rank /= first
        write(message, '(a, i0)') "Greetings from rank ", my_rank
        msg_len = len_trim(message)
        call MPI_Ssend(message, msg_len, MPI_CHARACTER, first, 0, MPI_COMM_WORLD, ierr)
    end if
    call MPI_Finalize(ierr)
end program mpi_ranks