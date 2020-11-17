program real_kinds
    implicit none
    integer :: range
    integer :: precision
    integer :: kind

    do precision = 0, 41
        do range = 0, 5100
            kind = selected_real_kind(precision, range)
            print *, range, precision, kind
        end do ! range
    end do ! precision
end program real_kinds