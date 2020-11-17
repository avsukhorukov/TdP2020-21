program nbody_direct
    implicit none
    !real, parameter :: G = 6.6743015e-11
    real :: current ! current time
    real :: total   ! total time
    real :: step    ! integration time step
    real :: timer   ! cyclic time count to output results
    real :: lap     ! time cycle to output results
    integer :: n    ! number of bodies
    integer :: i, j
    real, allocatable :: m(:), r(:, :), v(:, :), a(:, :)
    real, allocatable :: rn(:, :), vn(:, :), an(:, :)
    real :: r_ij(3), a_i(3), a_ij(3)

    read *, step
    read *, lap 
    read *, total
    read *, n
    allocate(m(n))
    allocate( r(3, n),  v(3, n),  a(3, n))
    allocate(rn(3, n), vn(3, n), an(3, n))
    do i = 1, n
        read *, m(i), r(:, i), v(:, i)
        m(i) = m(i) !* G
    end do

    ! Get the unknown accelerations.
    do i = 1, n
        a_i(:) = 0.0
        do j = 1, n
            if (j == i) cycle
            r_ij(:) = r(:, j) - r(:, i)
            a_ij(:) = (m(j) / norm2(r_ij)**3) * r_ij(:)
            a_i(:) = a_i(:) + a_ij(:)
        end do
        a(:, i) = a_i(:)
    end do

    ! Start iterations.
    current = 0.0
    timer   = 0.0
    time: do while (current <= total)
        ! If the timer has elapsed, then increment it and print the current
        ! coordinates.
        if (timer <= 0.0) then
            do i = 1, n
                print *, r(:, i)
            end do
            timer = timer + lap
        end if
        ! Get new coordinates.
        do i = 1, n
            rn(:, i) = r(:, i) + step * (v(:, i) + (0.5 * step) * a(:, i))
        end do
        ! Get new accelerations and velocities from updated coordinates.
        do i = 1, n
            a_i(:) = 0.0
            do j = 1, n
                if (j == i) cycle
                r_ij(:) = rn(:, j) - rn(:, i)
                a_ij(:) = (m(j) / norm2(r_ij)**3) * r_ij(:)
                a_i(:) = a_i(:) + a_ij(:)
            end do
            an(:, i) = a_i(:)
            vn(:, i) = v(:, i) + (0.5 * step) * (a(:, i) + an(:, i))
            ! Save next to current.
            r(:, i) = rn(:, i)
            v(:, i) = vn(:, i)
            a(:, i) = an(:, i)
        end do
        current = current + step
        timer   = timer   - step
    end do time

    deallocate(m, r, v, a)
    deallocate(rn, vn, an)
end program nbody_direct