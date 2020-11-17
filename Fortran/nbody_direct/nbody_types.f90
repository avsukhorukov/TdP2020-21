! Remeber the order in which you must update you data: first r, then half v,
! then a, then half v.  If you update v before r, you'll get a drift in
! coordinates.
program nbody_direct
    implicit none

    type timer_type
        real :: current ! current time
        real :: total   ! total time
        real :: step    ! integration time step
        real :: timer   ! cyclic time count to output results
        real :: lap     ! time cycle to output results
    end type timer_type

    type(timer_type) :: time

    type body_type
        real :: m
        real :: r(3)
        real :: v(3)
        real :: a(3)
    end type body_type

    type(body_type), allocatable :: bodies(:)

    integer :: n    ! number of bodies
    integer :: i, j
    real :: r_ij(3), a_i(3), a_ij(3)

    read *, time%step
    read *, time%lap
    read *, time%total
    read *, n
    allocate(bodies(n))
    do i = 1, n
        read *, bodies(i)%m, bodies(i)%r, bodies(i)%v
    end do

    ! Get the unknown accelerations.
    do i = 1, n
        a_i = 0.0
        do j = 1, n
            if (j == i) cycle
            r_ij = bodies(j)%r - bodies(i)%r
            a_ij = (bodies(j)%m / norm2(r_ij)**3) * r_ij
            a_i  = a_i + a_ij
        end do
        bodies(i)%a = a_i
    end do

    ! Start iterations.
    time%current = 0.0
    time%timer   = time%lap
    do while (time%current <= time%total)
        ! If the timer has elapsed, then increment it and print the current
        ! coordinates.
        if (time%timer <= 0.0) then
            do i = 1, n
                print *, bodies(i)%r
            end do
            time%timer = time%timer + time%lap
        end if
        do i = 1, n
            ! Update coordinates.
            bodies(i)%r = bodies(i)%r + time%step * (bodies(i)%v + (0.5 * time%step) * bodies(i)%a)
            ! Half-update velocities.
            bodies(i)%v = bodies(i)%v + (0.5 * time%step) * bodies(i)%a
        end do
        ! Get new accelerations with updated coordinates.
        do i = 1, n
            a_i = 0.0
            do j = 1, n
                if (j == i) cycle
                r_ij = bodies(j)%r - bodies(i)%r
                a_ij = (bodies(j)%m / norm2(r_ij)**3) * r_ij
                a_i  = a_i + a_ij
            end do
            bodies(i)%a = a_i
        end do
        ! Half-update velocities.
        do i = 1, n
            bodies(i)%v = bodies(i)%v + (0.5 * time%step) * bodies(i)%a
        end do
        ! Next time step.
        time%current = time%current + time%step
        time%timer   = time%timer   - time%step
    end do

    deallocate(bodies)
end program nbody_direct