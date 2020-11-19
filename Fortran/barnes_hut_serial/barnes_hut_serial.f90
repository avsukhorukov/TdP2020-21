module bh_mod
    implicit none

    real, parameter :: THETA = 0.5

    type body_type
        real :: m
        real :: r(3)
        real :: v(3)
        real :: a(3)
    end type body_type

    type(body_type), allocatable, target :: bodies(:)

    type timer_type
        real :: current ! current time
        real :: total   ! total integration time
        real :: step    ! integration time step
        real :: timer   ! cyclic time count to output coordinates
        real :: lap     ! time cycle to output results
    end type timer_type

    type(timer_type) :: time

    type dims_type
        real :: min(3)
        real :: max(3)
    end type dims_type

    type node_ptr
        type(node_type), pointer :: ptr
    end type node_ptr

    type node_type
        real :: M = 0.0
        real :: R(3) = [0.0, 0.0, 0.0]
        type(dims_type) :: dims
        type(body_type), pointer :: body => null()
        type(node_ptr), pointer :: children(:, :, :) => null()
    end type node_type

    type(node_type), pointer :: p_root => null()

contains
    !---------------------------------------------------------------------------
    subroutine read_data()
        implicit none
        integer :: nbodies, i
        real :: m, r(3), v(3)

        read *, time%step
        read *, time%lap
        read *, time%total
        read *, nbodies
        allocate(bodies(nbodies))
        do i = 1, nbodies
            !read *, bodies(i)%m, bodies(i)%r(:), bodies(i)%v(:)
            read *, m, r(:), v(:)
            bodies(i) = body_type(m, r(:), v(:), [0.0, 0.0, 0.0])
        end do
        return
    end subroutine read_data
    !---------------------------------------------------------------------------
    subroutine free_bodies()
        implicit none
        deallocate(bodies)
        return
    end subroutine free_bodies
    !---------------------------------------------------------------------------
    subroutine print_r()
        implicit none
        integer :: i
        do i = 1, size(bodies)
            print '(3(es10.3, 1x))', bodies(i)%r(:)
        end do
    end subroutine print_r
    !---------------------------------------------------------------------------
    function global_dims(p_bodies)
        implicit none
        type(dims_type) :: global_dims
        type(body_type), pointer, intent(in) :: p_bodies(:)
        real :: mins(3), maxs(3), centers(3)
        real :: span
        integer :: i

        mins(:) =  huge(0.0)
        maxs(:) = -huge(0.0)
        do i = 1, size(p_bodies)
            mins = min(mins, p_bodies(i)%r)
            maxs = max(maxs, p_bodies(i)%r)
        end do
        span = maxval(maxs - mins) * 1.001 ! +0.1% to put them inside the range.
        centers = (mins + maxs) / 2.0
        global_dims%min = centers - span / 2.0
        global_dims%max = centers + span / 2.0
        return
    end function global_dims
    !---------------------------------------------------------------------------
    subroutine find_subcell(r, octant, dims)
        implicit none
        real, intent(in) :: r(3)
        integer, intent(inout) :: octant(3)
        type(dims_type), intent(inout) :: dims
        integer :: rank
        real :: left, center, right

        do rank = 1, 3
            left  = dims%min(rank)
            right = dims%max(rank)
            center = (left + right) / 2.0
            if (left <= r(rank) .and. r(rank) < center) then
                octant(rank)   = 1
                dims%min(rank) = left
                dims%max(rank) = center
            else
                octant(rank) = 2
                dims%min(rank) = center
                dims%max(rank) = right
            end if
        end do
    end subroutine find_subcell
    !---------------------------------------------------------------------------
    subroutine create_tree()
        implicit none
        integer :: i
        type(dims_type) :: g_dims, dims
        type(body_type), pointer :: p_body

        g_dims = global_dims(bodies)
        do i = 1, size(bodies)
            p_body => bodies(i)
            dims = g_dims
            call add_body(p_root, p_body, dims)
        end do
    end subroutine create_tree
    !---------------------------------------------------------------------------
    recursive subroutine add_body(p_node, p_body, dims)
        implicit none
        type(node_type), pointer, intent(inout) :: p_node
        type(body_type), pointer, intent(in) :: p_body
        type(dims_type), intent(inout) :: dims
        real :: RM(3) ! barycenter's moment of mass
        integer :: octant(3), i, j, k

        if (.not.associated(p_node)) then ! empty node
            allocate(p_node)
            p_node%body => p_body
            p_node%M = p_body%m
            p_node%R(:) = p_body%r(:)
            p_node%dims = dims
            p_node%children => null()
        else ! 1-body external or 8-body internal
            if (associated(p_node%children)) then ! internal 8-body
                RM = p_node%R * p_node%M
                p_node%M = p_node%M + p_body%m
                p_node%R = (RM + p_body%r * p_body%m) / p_node%M
                call find_subcell(p_body%r, octant, dims)
                call add_body(p_node%children(octant(1), octant(2), octant(3))%ptr, p_body, dims)
            else ! external 1-body
                allocate(p_node%children(2, 2, 2))
                do k = 1, 2
                    do j = 1, 2
                        do i = 1, 2
                            p_node%children(i, j, k)%ptr => null()
                        end do ! i
                    end do ! j
                end do ! k
                ! Add the old (stored) body from the node.
                call find_subcell(p_node%R, octant, dims)
                call add_body(p_node%children(octant(1), octant(2), octant(3))%ptr, p_node%body, dims)
                p_node%body => null()
                ! Restore `dims` and add the new body.
                dims = p_node%dims
                call find_subcell(p_body%r, octant, dims)
                call add_body(p_node%children(octant(1), octant(2), octant(3))%ptr, p_body, dims)
                ! Update the barycenter.
                RM = p_node%R * p_node%M
                p_node%M = p_node%M + p_body%m
                p_node%R = (RM + p_body%r * p_body%m) / p_node%M
            end if
        end if
        return
    end subroutine add_body
    !---------------------------------------------------------------------------
    subroutine destroy_tree()
        implicit none
        call delete_node(p_root)
    end subroutine destroy_tree
    !---------------------------------------------------------------------------
    recursive subroutine delete_node(p_node)
        implicit none
        type(node_type), pointer, intent(inout) :: p_node
        integer :: i, j, k
        
        if (.not.associated(p_node)) then ! empty node
            return
        else ! 1-body external or 8-body internal
            if (associated(p_node%children)) then ! 8-body internal
                do k = 1, 2
                    do j = 1, 2
                        do i = 1, 2
                            call delete_node(p_node%children(i, j, k)%ptr)
                        end do ! i
                    end do ! j
                end do ! k
                deallocate(p_node%children)
                deallocate(p_node)
            else ! 1-body external
                deallocate(p_node)
            end if
        end if
        return
    end subroutine delete_node
    !---------------------------------------------------------------------------
    subroutine update_a()
        implicit none
        integer :: i
        type(body_type), pointer :: p_body

        do i = 1, size(bodies)
            p_body => bodies(i)
            p_body%a = get_a(p_root, p_body)
        end do
    end subroutine update_a
    !---------------------------------------------------------------------------
    recursive function get_a(p_node, p_body) result(res_a)
        implicit none
        real :: res_a(3)
        type(node_type), pointer, intent(in) :: p_node
        type(body_type), pointer, intent(in) :: p_body
        real :: side, r, r_ij(3), a_ij(3)
        type(node_type), pointer :: p_child
        integer :: i, j, k

        ! Assuming that p_node is always associated.
        res_a(:) = 0.0
        if (.not.associated(p_node%children)) then ! external 1-body
            if (.not.associated(p_node%body, p_body)) then
                r_ij(:)  = p_node%R(:) - p_body%r(:)
                a_ij(:)  = (p_node%M / norm2(r_ij)**3) * r_ij(:)
                res_a(:) = res_a(:) + a_ij(:)
            end if
        else ! internal 8-body node
            side    = p_node%dims%max(1) - p_node%dims%min(1)
            r_ij(:) = p_node%R(:) - p_body%r(:)
            r       = norm2(r_ij)
            if (side / r <= THETA) then
                a_ij(:)  = (p_node%M / r**3) * r_ij(:)
                res_a(:) = res_a(:) + a_ij(:)
            else
                do k = 1, 2
                    do j = 1, 2
                        do i = 1, 2
                            p_child => p_node%children(i, j, k)%ptr
                            if (associated(p_child)) then
                                res_a = res_a + get_a(p_child, p_body)
                            end if
                        end do ! i
                    end do ! j
                end do ! k
            end if
        end if
        return
    end function get_a
    !---------------------------------------------------------------------------
    subroutine update_v()
        implicit none
        integer :: i

        do i = 1, size(bodies)
            bodies(i)%v = bodies(i)%v + bodies(i)%a * (time%step / 2.0)
        end do
        return
    end subroutine update_v
    !---------------------------------------------------------------------------
    subroutine update_r()
        implicit none
        integer :: i

        do i = 1, size(bodies)
            ! update_r() before update_v()
            !bodies(i)%r(:) = bodies(i)%r(:) + time%step * ( bodies(i)%v(:) + (0.5 * time%step) * bodies(i)%a(:) )
            ! update_v() before update_r()
            bodies(i)%r(:) = bodies(i)%r(:) + time%step * bodies(i)%v(:)
        end do
        return
    end subroutine update_r
    !---------------------------------------------------------------------------
end module bh_mod

!===============================================================================

program main
    use :: bh_mod
    implicit none

    call read_data()
    call create_tree()
    call update_a()
    time%current = 0.0
    time%timer   = time%lap
    do while (time%current <= time%total)
        if (time%timer <= 0.0) then
            call print_r()
            time%timer = time%timer + time%lap
        end if
        call update_v()
        call update_r()
        call destroy_tree()
        call create_tree()
        call update_a()
        call update_v()
        time%current = time%current + time%step
        time%timer   = time%timer   - time%step
    end do
    call destroy_tree()
    call free_bodies()
end program main