program directed_list
    implicit none

    type list_type
        integer :: val
        type(list_type), pointer :: next => null()
    end type list_type

    type(list_type), pointer :: head => null()
    type(list_type), pointer :: crnt => null()
    type(list_type), pointer :: prev => null()

    integer :: number
    integer :: iostatus
    !head => null()
    !nullify(head)

    !allocate(crnt)
    !head => crnt
    !crnt%val = 1

    !allocate(crnt%next)
    !crnt => crnt%next
    !crnt%val = 2

    !allocate(crnt%next)
    !crnt => crnt%next
    !crnt%val = 3
    !crnt%next => null()

    do
        read(*, *, iostat=iostatus) number
        if (iostatus /= 0) exit
        if (.not.associated(head)) then
            allocate(crnt)
            head => crnt
        else
            allocate(crnt%next)
            crnt => crnt%next
        end if
        crnt%val = number
    end do

    ! Walk
    crnt => head
    do while (associated(crnt))
        print "('(', i0, ')->', $)", crnt%val
        crnt => crnt%next
    end do
    print '(a)', "null()"

    !crnt => null()
    !deallocate(head%next%next)
    !deallocate(head%next)
    !deallocate(head)
    do while (.not.associated(prev, head))
        crnt => head
        do while (associated(crnt%next))
            prev => crnt
            crnt => crnt%next
        end do
        deallocate(crnt)
        prev%next => null()
    end do
    prev => null()
    deallocate(head)
end program directed_list