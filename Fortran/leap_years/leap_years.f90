! Print all leap years within a period specified by given start and end years.
! A year is leap if divisible by 4 unless it is divisible by 100 except if it is
! divisible by 400.
!
! Using boolean logic this can be written as
!
!   y/4 ∧ [ ¬y/4 ∨ ( y/100 ∧ y/400 ) ]
!
! Using the distributivity of ∨ over ∧ and the identity ¬x ∨ x = 1 this can be
! simplified into two equivalent forms:
!
!   y/4 ∧ ( ¬y/100 ∨ y/400 ), or
!   y/400 ∨ ( y/4 ∧ ¬y/100 ).
program leap_years
    implicit none
    integer :: first_year, last_year, year

    print *, "First and last years?"
    read *, first_year, last_year
    do year = first_year, last_year
        if ( mod(year, 4) == 0 .and. ( mod(year, 100) /= 0 .or. mod(year, 400) == 0 )) then
            print *, year
        end if
    end do
end program leap_years
