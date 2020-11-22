! 7.2.5 Heat equation in 2D
!
! Parallelize the following serial code.
!
! $ gfortran -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace heat_serial.f90
! $ ./a.out < in.txt
program heat_equation
    implicit none
    real, parameter :: alpha = 2.5e-4
    real, pointer   :: prev_t(:, :), next_t(:, :), temp_t(:, :)
    real            :: diff_t
    integer         :: side, line, time

    read *, side
    allocate(prev_t(0:side + 1, 0:side + 1))
    do line = 0, side + 1
        read *, prev_t(line, 0:side + 1)
    end do
    allocate(next_t(0:side + 1, 0:side + 1))
    next_t(:, :) = prev_t(:, :)

    time = 0
    do
        next_t(1:side, 1:side) = &
            (1.0 - 4.0 * alpha) *   prev_t(1:side,     1:side)     &
                       + alpha  * ( prev_t(0:side - 1, 1:side    ) &
                                  + prev_t(2:side + 1, 1:side    ) &
                                  + prev_t(1:side    , 0:side - 1) &
                                  + prev_t(1:side    , 2:side + 1) )
        diff_t = maxval(abs(next_t(1:side, 1:side) - prev_t(1:side, 1:side)))
        if (mod(time, 10000) == 0) then
            print '(a, i0, a, es12.5)', "time = ", time, ", diff_t = ", diff_t
            do line = 0, side + 1
                print '(*(f4.0, 1x))', prev_t(line, 0:side + 1)
            end do
        end if
        if (diff_t < epsilon(0.0)) exit
        temp_t => prev_t
        prev_t => next_t
        next_t => temp_t
        time = time + 1
    end do
    print '(a, i0)', "Final time is ", time
    deallocate(prev_t)
    deallocate(next_t)
end program heat_equation
