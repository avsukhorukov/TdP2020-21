! October 14, 2020: Lecture 02.
! Exercise 1: Solve the quadratic equation using...
program quadratic_equation
    implicit none
    integer, parameter :: SP = kind(0.e0)
    integer, parameter :: dP = kind(0.d0)
    integer, parameter :: QP = kind(0.q0)
    integer, parameter :: WP = SP
    real(kind=WP) :: a, b, c, zero
    real(kind=WP) :: D, x1, x2, tol
    complex(kind=WP) :: zD, z1, z2, z_zero
    character(len=7) :: mode

    print *, "For a quadratic equation a x^2 + b x + c = 0,"
    print *, "coefficients a, b, and c are:"
    read *, a, b, c
    print *, "Which mode, 'real' or 'complex'?"
    read *, mode

    select case (mode)
    case ("real")
        D = b**2 - 4.0_WP * a * c
        tol = 10.0_WP * epsilon(0.0_WP) ! tolerance = numerical precision x 10
        if (D > 0.0_WP) then ! two distinct roots
            x1 = (-b + sqrt(D)) / (2.0_WP * a)
            x2 = (-b - sqrt(D)) / (2.0_WP * a)
            print *, "1st distinct root is ", x1
            print *, "2nd distinct root is ", x2

            zero = a * x1**2 + b * x1 + c
            print *, "Test: a x_1^2 + b x_1 + c = ", zero
            zero = a * x2**2 + b * x2 + c
            print *, "Test: a x_2^2 + b x_2 + c = ", zero
        else if (abs(D) <= tol) then
            x1 = -b / (2.0_WP * a) ! equal roots
            print *, "One double root is ", x1

            zero = a * x1**2 + b * x1 + c
            print *, "Test: a x_1^2 + b x_1 + c = ", zero
        else ! D < 0.0: no real roots
            print *, "There are no real roots, D = ", D
        end if
    case ("complex")
        zD = b**2 - 4.0_wp * a * c
        z1 = (-b + sqrt(zD)) / (2.0_WP * a)
        z2 = (-b - sqrt(zD)) / (2.0_WP * a)
        print *, "1st complex root is ", z1
        print *, "2nd complex root is ", z2
        !print "(a, 2(1x, f0.8, sp, f0.8, 'i'))", "Two complex roots are ", z1, z2

        z_zero = a * z1**2 + b * z1 + c
        print *, "Test: a z_1^2 + b z_1^2 + c = ", z_zero
        z_zero = a * z2**2 + b * z2 + c
        print *, "Test: a z_2^2 + b z_2^2 + c = ", z_zero
    case default
        stop "Wrong mode = " // mode
    end select
end program quadratic_equation
