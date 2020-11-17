program main
    implicit none
    integer :: k
    print "(a, $)", "Index k is "
    read *, k
    print "(i0)", factorial(k)
contains
    recursive function factorial(i) result(ifact)
        implicit none
        integer :: ifact
        integer, intent(in) :: i
        if (i == 1) then
            ifact = 1
        else
            ifact = i * factorial(i - 1)
        end if
    end function factorial
end program main