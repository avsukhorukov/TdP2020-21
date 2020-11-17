! Number F(46) is the highest allowed for integer(kind=4).  The next number,
! F(47) creates an integer overflow and produces a negative result.
!
! To test yourself, get the table of F(i) values from here:
!
!   https://oeis.org/A000045/b000045.txt
!
program fibonacci_sequential
    implicit none
    integer :: i

    print '(a)', "Enter index `i` for the Fibonacci sequence"
    read *, i
    print '(2(a, i0))', "F(", i, ") = ", fib_seq(i)
contains
    function fib_seq(n)
        integer :: fib_seq
        integer, intent(in) :: n
        integer :: j, f_pp, f_p, f

        f_pp = 0
        f_p  = 1
        do j = 2, n
            f    = f_p + f_pp
            f_pp = f_p
            f_p  = f
        end do
        fib_seq = f
        return
    end function fib_seq
end program fibonacci_sequential
