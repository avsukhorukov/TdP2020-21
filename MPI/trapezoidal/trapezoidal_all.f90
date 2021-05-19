! 7.2.2 Trapezoidal rule integral with I/O.
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace trapezoidal_all.f90
! $ mpirun -np 4 --oversubscribe ./a.out
program trapezoidal_all
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks
    type(MPI_Comm) :: comm
    real :: a, b, a_r, b_r, integral, total, step
    integer :: m, m_r

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    if (my_rank == 0) then
        print '(a)', "Enter the interval [a, b] and the number of steps m (1.0e-3, 1.0, 1024):"
        read *, a, b, m
    end if
    call MPI_Bcast(a, 1, MPI_REAL, 0, comm)
    call MPI_Bcast(b, 1, MPI_REAL, 0, comm)
    call MPI_Bcast(m, 1, MPI_INTEGER, 0, comm)

    step = (b - a) / m
    m_r  = m / n_ranks

    a_r = a   + my_rank * m_r * step
    b_r = a_r +           m_r * step
    integral = trap_integral(a_r, b_r, m_r)

    call MPI_Reduce(integral, total, 1, MPI_REAL, MPI_SUM, 0, comm)

    if (my_rank == 0) then
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

        func = sin(1.0 / x) ! strongly oscillating, needs many intervals
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
        trap_integral = (func(x_s) + func(x_e)) / 2.0
        x = x_s
        do i = 2, steps
            x = x + dx
            trap_integral = trap_integral + func(x)
        end do
        trap_integral = trap_integral * step
        return
    end function trap_integral
end program trapezoidal_all
