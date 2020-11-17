! Things to remember:
!  1) Use the minimum possible floating-point number for max_sum before starting
!     the search.  This number is `-huge(0.0)`.  What follows in the negative
!     direction is `nearest(-huge(0.0), -1.0) = -Infinity`.
!  2) If the current index is `i` and the window has `width` then the range of
!     indices is [i:i + width - 1].  Remember to subtract or add 1 as the last
!     point is included.  In Python this will be just [i:i + width) as the last
!     point is excluded from the interval.
program summation_window
    implicit none
    integer :: n, width, i, index
    real, allocatable :: numbers(:)
    real :: current_sum, max_sum

    print '(a)', "How many floating-point numbers?"
    read *, n
    print '(a)', "What is the window width?"
    read *, width

    allocate(numbers(n))
    print '(a, i0, a)', "Read ", n, " numbers:"
    read *, numbers

    ! Search for the maximum sum.  Initialize it to the smallest integer number.
    ! Initialize the search index to zero.
    max_sum = -huge(max_sum)
    index   = 0
    do i = 1, n - width + 1
        current_sum = sum(numbers(i:i + width - 1))
        if (max_sum < current_sum) then
            max_sum = current_sum
            index   = i
        end if
    end do

    print '(a, f0.7, a)', "The maximum sum ", max_sum, " is from the following numbers:"
    print '(*(f0.7, 2x))', numbers(index:index + width - 1)

    deallocate(numbers)
end program summation_window