! 6.1.2 Trapezoidal rule integration with I/O.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace trapezoidal_p2p.f90
! $ mpirun -np 4 --oversubscribe ./a.out
program trapezoidal_p2p
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, r, root
    type(MPI_Status) :: status
    type(MPI_Comm) :: comm = MPI_COMM_WORLD
    real :: a, b, a_r, b_r, integral, total, step
    integer :: m, m_r

    call MPI_Init()
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! a = 1.0e-3
    ! b = 1.0
    ! n = 1024
    if (my_rank == root) then
        print *, "Enter the interval [a, b] and the number of steps m:"
        read *, a, b, m
        do r = 1, n_ranks - 1
            call MPI_Send(a, 1, MPI_REAL,    r, 0, comm)
            call MPI_Send(b, 1, MPI_REAL,    r, 0, comm)
            call MPI_Send(m, 1, MPI_INTEGER, r, 0, comm)
        end do
    else ! my_rank /= root
        call MPI_Recv(a, 1, MPI_REAL,    root, 0, comm, status)
        call MPI_Recv(b, 1, MPI_REAL,    root, 0, comm, status)
        call MPI_Recv(m, 1, MPI_INTEGER, root, 0, comm, status)
    end if

    step = (b - a) / m
    m_r  = m / n_ranks

    a_r = a   + my_rank * m_r * step
    b_r = a_r +           m_r * step
    integral = trap_integral(a_r, b_r, m_r)

    if (my_rank /= root) then
        call MPI_Send(integral, 1, MPI_REAL, root, 0, comm)
    else ! my_rank == root
        total = integral
        do r = 1, n_ranks - 1
            call MPI_Recv(integral, 1, MPI_REAL, r, 0, comm, status)
            total = total + integral
        end do
    end if

    if (my_rank == root) then
        print '(a, i0, 3(a, es12.6))', &
            "With ", m, " trapezoids the integral from ", a, " to ", b, " is ", total
    end if

    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    function func(x)
        implicit none
        real :: func
        real, intent(in) :: x

        func = sin(1.0 / x)
        return
    end function func
    !---------------------------------------------------------------------------
    function trap_integral(x_s, x_e, steps)
        implicit none
        real                :: trap_integral
        real,    intent(in) :: x_s, x_e
        integer, intent(in) :: steps
        integer :: i
        real    :: x, dx

        dx = (x_e - x_s) / steps
        integral = (func(x_s) + func(x_e)) / 2.0
        x = x_s
        do i = 2, steps
            x = x + dx
            integral = integral + func(x)
        end do
        trap_integral = integral * step
        return
    end function trap_integral
end program trapezoidal_p2p
