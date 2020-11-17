! This solution using a character type
! a) is easier as no counting and parsing of digits is needed;
! b) requires you to know character substrings and intrinsic functions for the
!    character type.
! c) uses a static (fixed-length) character string for simplicity.  If you want
!    to use a dynamic (variable-length) string, you must deallocate it at the
!    end and you must read it in carefully.
program palindrome_character
    implicit none
    integer, parameter :: HUNDRED = 100
    character(len=HUNDRED) :: num
    integer :: l, i, j
    logical :: is_palindromic

    print *, "Enter an integer number"
    read *, num

    num = adjustl(num)
    l   = len_trim(num)

    ! Check for a palindrome.
    is_palindromic = .true.
    do i = 1, l / 2
        j = l - i + 1
        if (num(i:i) /= num(j:j)) then
            is_palindromic = .false.
            exit
        end if
    end do

    if (is_palindromic) then
        print *, "The number is a palindrome."
    else
        print *, "The number is not a palindrome."
    end if
end program palindrome_character