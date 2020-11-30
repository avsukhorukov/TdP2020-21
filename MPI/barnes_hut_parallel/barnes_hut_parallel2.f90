! Run it with `-np 8`.
module parallel_mod
    use mpi_f08
    implicit none
    integer :: n_ranks
    integer :: my_rank
    integer, parameter :: root_rank = 0
    type(MPI_Comm)     :: comm = MPI_COMM_WORLD
    type(MPI_Datatype) :: body_t    ! Entire body structure
    type(MPI_Op)       :: sum_body_a_op
    integer, parameter :: n_rows = 2
    integer, parameter :: n_cols = 2
    integer, parameter :: n_deps = 2
    integer :: topology(3) ! [row, col, dep]
contains
    !---------------------------------------------------------------------------
    subroutine init_parallel()
        implicit none
        call MPI_Init()
        call MPI_Comm_size(comm, n_ranks)
        call MPI_Comm_rank(comm, my_rank)
        call create_topology()
    end subroutine init_parallel
    !---------------------------------------------------------------------------
    subroutine term_parallel()
        implicit none
        call MPI_Finalize()
    end subroutine term_parallel
    !---------------------------------------------------------------------------
    ! Create a 2x2x2 topology based on my_rank.  The decomposition into rows,
    ! columns, and depths follows the Fortran order of array elements: row index
    ! is the fastest in memory.  Current rank my_rank is the following
    ! expression:
    !   my_rank = r + n_rows (c + n_cols (d))
    subroutine create_topology()
        implicit none
        integer :: tmp, r, c, d

        if (n_rows * n_cols * n_deps /= n_ranks) then
            call MPI_Abort(comm, MPI_ERR_TOPOLOGY)
            stop "Run it with `-np 8`."
        end if
        tmp = my_rank ! my_rank = r + n_rows * c + n_rows * n_cols * d
        r   = mod(tmp, n_rows)
        tmp = (tmp - r) / n_rows ! c + n_cols * d
        c   = mod(tmp, n_cols)
        tmp = (tmp - c) / n_cols ! d
        d   = tmp
        topology = [r, c, d]
        return
    end subroutine create_topology
    !---------------------------------------------------------------------------
end module parallel_mod
!===============================================================================

module bh_mod
    use :: parallel_mod
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
    subroutine create_body_type()
        implicit none
        integer(kind=MPI_ADDRESS_KIND) :: addresses(4), body_lb, body_extent
        type(MPI_Datatype) :: body_struct_t ! temporary structure type
        integer :: i
        call MPI_Get_address(bodies(1), body_lb)
        call MPI_Get_address(bodies(1)%m, addresses(1))
        call MPI_Get_address(bodies(1)%r, addresses(2))
        call MPI_Get_address(bodies(1)%v, addresses(3))
        call MPI_Get_address(bodies(1)%a, addresses(4))
        do i = 1, 4
            addresses(i) = MPI_Aint_diff(addresses(i), body_lb)
        end do
        ! Entire body structure
        call MPI_Type_create_struct(4, [1, 3, 3, 3], addresses, [MPI_REAL, MPI_REAL, MPI_REAL, MPI_REAL], body_struct_t)
        call MPI_Type_get_extent(body_struct_t, body_lb, body_extent)
        call MPI_Type_create_resized(body_struct_t, body_lb, body_extent, body_t)
        call MPI_Type_commit(body_t)
        ! Create a new MPI operation based on sum_body_a subroutine.
        call MPI_Op_create(user_fn=sum_body_a, commute=.true., op=sum_body_a_op)
    end subroutine create_body_type
    !---------------------------------------------------------------------------
    ! This defines an MPI_User_function (procedure in Fortran) used to create a
    ! new MPI operation for reduction.  The header is standard: one must convert
    ! C pointers to F pointers for arrays and do the reduction in a loop over
    ! 1..len values.  In C examples this is 0..len-1 but in Fortran looping is
    ! based on index 1.
    subroutine sum_body_a(invec, inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value  :: invec, inoutvec
        integer             :: len
        type(MPI_Datatype)  :: datatype
        type(body_type), pointer :: f_invec(:), f_inoutvec(:)
        integer :: i

        if (datatype == body_t) then
            call c_f_pointer(invec,    f_invec,    [len])
            call c_f_pointer(inoutvec, f_inoutvec, [len])
            do i = 1, len
                f_inoutvec(i)%a(1:3) = f_inoutvec(i)%a(1:3) + f_invec(i)%a(1:3)
            end do
        end if
    end subroutine sum_body_a
    !---------------------------------------------------------------------------
    subroutine release_body_types()
        implicit none
        call MPI_Type_free(body_t)
        call MPI_Op_free(sum_body_a_op)
    end subroutine release_body_types
    !---------------------------------------------------------------------------
    subroutine read_data()
        implicit none
        integer :: nbodies, i

        if (my_rank == root_rank) then
            read *, time%step
            read *, time%lap
            read *, time%total
            read *, nbodies
        end if
        call MPI_Bcast(time%step,  1, MPI_REAL,    root_rank, comm)
        call MPI_Bcast(time%lap,   1, MPI_REAL,    root_rank, comm)
        call MPI_Bcast(time%total, 1, MPI_REAL,    root_rank, comm)
        call MPI_Bcast(nbodies,    1, MPI_INTEGER, root_rank, comm)
        allocate(bodies(nbodies))
        if (my_rank == root_rank) then
            do i = 1, nbodies
                read *, bodies(i)%m, bodies(i)%r(:), bodies(i)%v(:)
            end do
        end if
        call create_body_type()
        call MPI_Bcast(bodies, nbodies, body_t, root_rank, comm)
        return
    end subroutine read_data
    !---------------------------------------------------------------------------
    subroutine free_bodies()
        implicit none
        deallocate(bodies)
        call release_body_types()
        return
    end subroutine free_bodies
    !---------------------------------------------------------------------------
    subroutine print_r()
        implicit none
        integer :: i
        if (my_rank == root_rank) then
            !print '(a, es10.2)', "time=", time%current
            do i = 1, size(bodies)
                print '(3(es14.7, 1x))', bodies(i)%r(:)
            end do
        end if
    end subroutine print_r
    !---------------------------------------------------------------------------
    ! Parallelized version has a decomposition of the global ranges into 8
    ! sub-domains similar to octants on the first level of the tree.  In
    ! topology decomposition [r, c, d] values of row, column and depth are
    ! 0-based contrary to octants that are 1-based Fortran array indices.
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
        global_dims%min = centers + (span / 2.0) * (topology - 1)
        global_dims%max = centers + (span / 2.0) * (topology)
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
    ! Extra function to check if the current particle belongs to the global
    ! range of the partitioned tree.
    function belongs(r, g_dims)
        implicit none
        logical                     :: belongs
        real, intent(in)            :: r(3)
        type(dims_type), intent(in) :: g_dims

        belongs = ( g_dims%min(1) <= r(1) .and. r(1) < g_dims%max(1) &
            .and.   g_dims%min(2) <= r(2) .and. r(2) < g_dims%max(2) &
            .and.   g_dims%min(3) <= r(3) .and. r(3) < g_dims%max(3) )
        return
    end function belongs
    !---------------------------------------------------------------------------
    subroutine create_tree()
        implicit none
        integer :: i
        type(dims_type) :: g_dims, dims
        type(body_type), pointer :: p_body

        g_dims = global_dims(bodies)
        do i = 1, size(bodies)
            p_body => bodies(i)
            ! Add only bodies that belong to this topology partition in global
            ! dimensions.
            if (belongs(p_body%r, g_dims)) then
                dims = g_dims
                call add_body(p_root, p_body, dims)
            end if
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

        res_a(:) = 0.0
        if (associated(p_node)) then
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
        end if ! associated(p_node)
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
    ! Reduce accelerations of all bodies beteween different sub-domains.
    subroutine allreduce_a()
        implicit none
        !integer :: i

        !do i = 1, size(bodies)
        !    call MPI_Allreduce(MPI_IN_PLACE, bodies(i)%a, 3, MPI_REAL, MPI_SUM, comm)
        !end do
        call MPI_Allreduce(MPI_IN_PLACE, bodies, size(bodies), body_t, sum_body_a_op, comm)
    end subroutine allreduce_a
    !---------------------------------------------------------------------------
end module bh_mod
!===============================================================================

program main
    use :: parallel_mod
    use :: bh_mod
    implicit none

    call init_parallel()
    call read_data()
    call create_tree()
    call update_a()
    call allreduce_a()
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
        call allreduce_a()
        call update_v()
        time%current = time%current + time%step
        time%timer   = time%timer   - time%step
    end do
    call destroy_tree()
    call free_bodies()
    call term_parallel()
end program main