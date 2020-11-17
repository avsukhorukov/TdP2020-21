! This solution using integer numbers is
! a) more difficult as one needs to extract separate digits;
! b) is limited to the huge value of the default-kind integer, ~2.1 milliard.
program palindrome_integer
    implicit none
    integer :: num, num2, n_digits, digit, i
    integer, allocatable :: digits(:)
    logical :: is_palindromic

    print *, "Enter an integer number"
    read *, num

    ! Count digits in the number.
    num2 = num
    n_digits = 0
    do while (num2 > 0)
        num2 = num2 / 10
        n_digits = n_digits + 1
    end do

    allocate(digits(n_digits))

    ! Parse digits.
    n_digits = 0
    num2 = num
    do while (num2 > 0)
        n_digits = n_digits + 1
        digit = mod(num2, 10)
        digits(n_digits) = digit
        num2 = (num2 - digit) / 10
    end do

    ! Check if it is a palindrome.
    is_palindromic = .true.
    do i = 1, n_digits / 2
        if (digits(i) /= digits(n_digits - i + 1)) then
            is_palindromic = .false.
            exit
        end if
    end do

    if (is_palindromic) then
        print *, "The number is a palindrome."
    else
        print *, "The number is not a palindrome."
    end if

    deallocate(digits)
end program palindrome_integer