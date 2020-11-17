program integer_kinds
    implicit none
    integer :: range
    integer :: kind

    do range = 0, 45
        kind = selected_int_kind(range)
        print *, range, kind
    end do
end program integer_kinds