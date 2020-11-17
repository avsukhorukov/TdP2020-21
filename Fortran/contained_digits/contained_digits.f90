program contained_digits
    implicit none
    integer :: num1, num2

    print '(a)', "Number 1 is "
    read *, num1
    print '(a)', "Number 2 is "
    read *, num2
    print *, contains_digits(num1, num2)
contains
    recursive function contains_digits(n1, n2) result(rslt)
        integer, intent(in) :: n1, n2
        logical :: rslt
        integer :: n, r

        rslt = .false.
        if (n1 < 10) then ! no more recursion.
            n = n2
            do while(0 < n)
                r = mod(n, 10)
                if (r == n1) then
                    rslt = .true.
                    exit
                end if
                n = (n - r) / 10
            end do
        else
            n = n1
            r = mod(n, 10)
            if (contains_digits(r, n2)) then
                n = (n - r) / 10
                rslt = contains_digits(n, n2)
            end if
        end if
        return
    end function contains_digits
end program contained_digits
