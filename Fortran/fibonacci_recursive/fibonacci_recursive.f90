! Number F(46) is the highest allowed for integer(kind=4).  The next number,
! F(47) creates an integer overflow and produces a negative result.
!
! To test yourself, get the table of F(i) values from here:
!
!   https://oeis.org/A000045/b000045.txt
!
program fibonacci_recursive
    implicit none
    integer :: i
    integer, parameter :: LI = kind(0_8) ! Long integer kind=8
    integer(kind=LI) :: counter = 0_LI

    print '(a)', "Enter index `i` for the Fibonacci sequence"
    read *, i
    print '(2(a, i0))', "F(", i, ") = ", fib_rec(i)
    print '(a, i0, a)', "F(i) is called ", counter, " times"
contains
    recursive function fib_rec(i) result(rslt)
        integer :: rslt
        integer, intent(in) :: i

        counter = counter + 1_LI
        if (i == 0) then
            rslt = 0
        else if (i == 1) then
            rslt = 1
        else
            rslt = fib_rec(i - 1) + fib_rec(i - 2)
        end if
        return
    end function fib_rec
end program fibonacci_recursive
