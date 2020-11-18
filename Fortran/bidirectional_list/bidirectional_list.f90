program bidirectional_list
    implicit none

    type bd_type
        integer :: val = 0
        type(bd_type), pointer :: prev => null()
        type(bd_type), pointer :: next => null()
    end type bd_type

    type(bd_type), pointer :: p_head => null()
    type(bd_type), pointer :: p_new  => null()
    type(bd_type), pointer :: p_old  => null()

    integer :: k, iostatus
    character(len=:), allocatable :: prn_str

    do
        read(*, *, iostat=iostatus) k
        if (iostatus /= 0) exit
        allocate(p_new)                     ! new current leaf (target)
        p_new%val = k                       ! store read-in value in it
        if (.not.associated(p_head)) then
            p_head => p_new                 ! head is new leaf
            p_old  => p_new                 ! old is new leaf too
        else
            p_old%next => p_new             ! old leaf's next is new leaf
            p_new%prev => p_old             ! new leaf's prev is old leaf
            p_old      => p_new             ! advance old leaf to new leaf
        end if
    end do
    prn_str = ""
    call print_bdlist(p_head, .true., prn_str)
    prn_str = ""
    call print_bdlist(p_old, .false., prn_str)
    deallocate(prn_str)

    do while (.not.associated(p_head, p_old))
        p_old => p_old%prev
        deallocate(p_new)
        p_new => p_old
    end do
    p_new => null()
    p_old => null()
    deallocate(p_head)

contains

    recursive subroutine print_bdlist(bdlist, forward, pstr)
        type(bd_type), pointer, intent(in) :: bdlist
        logical, intent(in) :: forward
        character(len=:), allocatable, intent(inout) :: pstr
        character(len=8) :: t_str

        if (forward) then
            write(t_str, "('(', i0, ')->')") bdlist%val
            pstr = pstr // trim(t_str)
            if (associated(bdlist%next)) then
                call print_bdlist(bdlist%next, forward, pstr)
            else
                write(*, '(a, a)') pstr, 'null'
            end if
        else
            write(t_str, '("<-(", i0, ")")') bdlist%val
            pstr = trim(t_str) // pstr
            if (associated(bdlist%prev)) then
                call print_bdlist(bdlist%prev, forward, pstr)
            else
                write(*, '(a, a)') 'null', pstr
            end if
        end if
    end subroutine print_bdlist

end program bidirectional_list

