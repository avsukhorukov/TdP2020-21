! Number F(46) is the highest allowed for integer(kind=4).  The next number,
! F(47) creates an integer overflow and produces a negative result.
!
! To test yourself, get the table of F(i) values from here:
!
!   https://oeis.org/A000045/b000045.txt
!
! Note: static arrays with save attribute inside routines are not allowed
! because they live in the stack frame.  Either allocate a dynamic array in the
! main program (you can also use a big enough static array) or use two extra
! variables and two extra indices with save attributes to store F(i - 2) and
! F(i - 1) values between the calls.
program fibonacci_memorized
    implicit none
    integer :: i
    integer, allocatable :: f(:) ! A dynamic array in the heap.

    print '(a)', "Enter index `i` for the Fibonacci sequence"
    read *, i
    ! Use value of -1 to mark F(i) that have not been computed yet.
    if (.not.allocated(f)) allocate(f(0:i), source=-1)

    print '(2(a, i0))', "F(", i, ") = ", fib_mem(i)
    if (allocated(f)) deallocate(f)
contains
    recursive function fib_mem(n) result(rslt)
        integer :: rslt
        integer, intent(in) :: n

        ! F(n) is not computed yet?
        if (f(n) == -1) then
            select case(n)
            case(0)
                f(0) = 0
            case(1)
                f(1) = 1
            case(2:)
                if (f(n - 2) == -1) f(n - 2) = fib_mem(n - 2)
                if (f(n - 1) == -1) f(n - 1) = fib_mem(n - 1)
                f(n) = f(n - 2) + f(n - 1)
            case default
                print '(a, i0)', "Wrong index n: ", n
                stop
            end select
        end if
        rslt = f(n)
        return
    end function fib_mem
end program fibonacci_memorized
