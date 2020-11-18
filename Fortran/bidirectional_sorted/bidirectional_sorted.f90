! This search assumes that we always have two pointers `p_l` and `p_r` pointing
! to the outmost elements of the ascending list.  When searching for the new
! position of element `k` we check if the r.h.s. is out of bounds
! (p_r%val <= k), or if the l.h.s. is out of bounds (k < p_l%val), and only then
! we test the internal case, which by definition excludes the case when
! p_r == p_l (in this case k will bet to the right of both).  This last case
! implies that there is at least one interval between p_l and p_r.  When we
! start searching from p_c => p_l to the right, p_c%nxt always exists and
! checking its association is not needed.
!
! Ángel's implementation does only a uni-directional insert from the head to the
! r.h.s.
program bidirectional_sorted
    implicit none

    type bd_type
        integer :: val = 0
        type(bd_type), pointer :: prv => null()
        type(bd_type), pointer :: nxt => null()
    end type bd_type

    type(bd_type), pointer :: p_head => null()
    type(bd_type), pointer :: p_l, p_c, p_r, p_t
    integer :: k, iostatus
    character(len=:), allocatable :: prn_str

    readleaf: do
        read(*, '(i10)', iostat=iostatus) k
        if (iostatus /= 0) exit readleaf
        if (.not.associated(p_head)) then
            allocate(p_c)
            p_c%val = k
            p_head => p_c
            p_l    => p_c
            p_r    => p_c
        else
            ! Find the current position.
            if (k < p_l%val) then
                allocate(p_c)
                p_c%val = k
                p_c%nxt => p_l
                p_l%prv => p_c
                p_l => p_c
            else if (p_r%val <= k) then
                allocate(p_c)
                p_c%val = k
                p_c%prv => p_r
                p_r%nxt => p_c
                p_r => p_c
            else ! at least one complete interval and p_r /= p_l
                p_c => p_l
                do
                    if (p_c%nxt%val <= k) then
                        p_c => p_c%nxt
                    else
                        exit
                    end if
                end do
                allocate(p_t) ! extra tmp pointer, p_c must not be allocated now.
                p_t%val = k
                p_t%nxt => p_c%nxt ! order is important, first the right pointer.
                p_c%nxt%prv => p_t
                p_c%nxt => p_t
                p_t%prv => p_c
            end if
        end if ! .not.associated(p_head)
    end do readleaf
    prn_str = ""
    call print_bdlist(p_l, .true., prn_str)
    prn_str = ""
    call print_bdlist(p_r, .false., prn_str)

    deallocate(prn_str)

    ! Destroy the list.
    ! Firstly, remove all leaves to the left from head
    do while (.not.associated(p_head, p_l))
        p_l => p_l%nxt
        deallocate(p_l%prv)
    end do
    ! Secondly, remove all leaves to the right from head.
    do while (.not.associated(p_head, p_r))
        p_r => p_r%prv
        deallocate(p_r%nxt)
    end do
    ! Now p_l => p_head => p_r.  Destroy the head.
    deallocate(p_head)
contains
    recursive subroutine print_bdlist(bdlist, forward, pstr)
        type(bd_type), pointer, intent(in) :: bdlist
        logical, intent(in) :: forward
        character(len=:), allocatable, intent(inout) :: pstr
        character(len=13) :: t_str

        if (forward) then
            write(t_str, '("(", i0, ")->")') bdlist%val
            pstr = pstr // trim(t_str)
            if (associated(bdlist%nxt)) then
                call print_bdlist(bdlist%nxt, forward, pstr)
            else
                write(*, '(a, a)') pstr, 'null'
            end if
        else
            write(t_str, '("<-(", i0, ")")') bdlist%val
            pstr = trim(t_str) // pstr
            if (associated(bdlist%prv)) then
                call print_bdlist(bdlist%prv, forward, pstr)
            else
                write(*, '(a, a)') 'null', pstr
            end if
        end if
    end subroutine print_bdlist
end program bidirectional_sorted
