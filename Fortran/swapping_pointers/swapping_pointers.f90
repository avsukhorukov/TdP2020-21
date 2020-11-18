program swapping_pointers
    implicit none
    integer, target :: i1, i2
    integer, pointer :: pi1, pi2

    i1 = 1111
    i2 = 9999
    pi1 => i1
    pi2 => i2

    print '(a)', 'Before swapping pointers:'
    print '(2(a, i0))', ' i1 = ',  i1, ',  i2 = ',  i2
    print '(2(a, i0))', 'pi1 = ', pi1, ', pi2 = ', pi2

    call swap_ptrs(pi1, pi2)

    print '(a)', 'After swapping pointers:'
    print '(2(a, i0))', ' i1 = ',  i1, ',  i2 = ',  i2
    print '(2(a, i0))', 'pi1 = ', pi1, ', pi2 = ', pi2

    call swap_ptrs(pi1, pi2)

    print '(a)', 'After swapping pointers back:'
    print '(2(a, i0))', ' i1 = ',  i1, ',  i2 = ',  i2
    print '(2(a, i0))', 'pi1 = ', pi1, ', pi2 = ', pi2

    call swap_vals(pi1, pi2)

    print '(a)', 'After swapping values:'
    print '(2(a, i0))', ' i1 = ',  i1, ',  i2 = ',  i2
    print '(2(a, i0))', 'pi1 = ', pi1, ', pi2 = ', pi2

    call swap_vals(pi1, pi2)

    print '(a)', 'After swapping values back:'
    print '(2(a, i0))', ' i1 = ',  i1, ',  i2 = ',  i2
    print '(2(a, i0))', 'pi1 = ', pi1, ', pi2 = ', pi2
contains
    subroutine swap_ptrs(ptr1, ptr2)
        implicit none
        integer, intent(inout), pointer :: ptr1, ptr2
        integer, pointer :: tmp

        tmp  => ptr1
        ptr1 => ptr2
        ptr2 => tmp
        return
    end subroutine swap_ptrs

    subroutine swap_vals(ptr1, ptr2)
        implicit none
        integer, intent(inout), pointer :: ptr1, ptr2
        integer :: tmp

        tmp  = ptr1
        ptr1 = ptr2
        ptr2 = tmp
        return
    end subroutine swap_vals
end program swapping_pointers