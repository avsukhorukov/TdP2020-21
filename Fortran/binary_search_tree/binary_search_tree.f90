module bst_mod
    use, intrinsic :: iso_fortran_env, only: stderr => ERROR_UNIT
    implicit none

    type bst_type
        integer :: value
        type(bst_type), pointer :: left   => null()
        type(bst_type), pointer :: parent => null()
        type(bst_type), pointer :: right  => null()
    end type bst_type
contains
    !---------------------------------------------------------------------------
    !+
    ! Print the values of BST in the inorder tree walk: left, value, right.
    ! Thus, the values are always printed in the ascending order.  You can check
    ! pointer association either for the node itself or for its children.  In
    ! his examples Ángel did it for for children.  Cormen et al. (2009) do it
    ! for the node itself, so as the existence of the ponter is always checked
    ! and the number of calls conforms to the definition of the tree depth.
    !-
    recursive subroutine bst_print(p_node)
        implicit none
        type(bst_type), pointer, intent(in) :: p_node
        
        if (associated(p_node)) then
            call bst_print(p_node%left)
            write(*, '(i0)') p_node%value
            call bst_print(p_node%right)
        end if
    end subroutine bst_print
    !---------------------------------------------------------------------------
    recursive subroutine bst_remove(p_node)
        implicit none
        type(bst_type), pointer, intent(inout) :: p_node
        type(bst_type), pointer :: p_prnt

        if (associated(p_node)) then
            if (associated(p_node%left))  call bst_remove(p_node%left)
            if (associated(p_node%right)) call bst_remove(p_node%right)
            if (associated(p_node%parent)) then
                p_prnt => p_node%parent
                if (associated(p_prnt%left, p_node)) then
                    deallocate(p_prnt%left)
                else
                    deallocate(p_prnt%right)
                end if
            else
                deallocate(p_node)
            end if
        end if
    end subroutine bst_remove
    !---------------------------------------------------------------------------
    !+
    ! The size of a tree is the total number of nodes in it.
    !-
    recursive function bst_size(p_node) result(res_value)
        implicit none
        integer :: res_value
        type(bst_type), pointer, intent(in) :: p_node

        if (associated(p_node)) then
            res_value = 1 + bst_size(p_node%left) + bst_size(p_node%right)
        else
            res_value = 0
        end if
        return
    end function bst_size
    !---------------------------------------------------------------------------
    !+
    ! The height of the node is the largest number of edges from this node down
    ! to a leaf (a node with no children).
    !-
    recursive function bst_height(p_node) result(res_value)
        implicit none
        integer :: res_value
        type(bst_type), pointer, intent(in) :: p_node

        if (.not.associated(p_node)) then
            res_value = 0
        else
            res_value = 1 + max(bst_height(p_node%left), bst_height(p_node%right))
        end if
        return
    end function bst_height
    !---------------------------------------------------------------------------
    !+
    ! Given an initial node pointer and a value `v`, search the tree for a node
    ! with with value `v`.  If this node exists, then return a pointer to it,
    ! otherwise return null.
    !-
    recursive function bst_search(p_node, v) result(res_value)
        implicit none
        type(bst_type), pointer :: res_value
        type(bst_type), pointer, intent(in) :: p_node
        integer, intent(in) :: v

        if (.not.associated(p_node)) then
            res_value => p_node              ! => null()
        else if (p_node%value == v) then
            res_value => p_node              ! the root node itself
        else if (v < p_node%value) then
            res_value => bst_search(p_node%left, v)
        else ! (p_node%value <= v)
            res_value => bst_search(p_node%right, v)
        end if
        return
    end function bst_search
    !---------------------------------------------------------------------------
    !+
    ! Iterative (non-recursive) version of `bst_search` function.
    !-
    function bst_find(p_node, v) result(p_crnt)
        implicit none
        type(bst_type), pointer :: p_crnt
        type(bst_type), pointer, intent(in) :: p_node
        integer, intent(in) :: v

        p_crnt => p_node
        do
            if (.not.associated(p_crnt)) exit ! is null()
            if (p_crnt%value == v) exit         ! is found
            if (v < p_crnt%value) then
                p_crnt => p_crnt%left
            else ! (p_crnt%value <= v)
                p_crnt => p_crnt%right
            end if
        end do
        return
    end function bst_find
    !---------------------------------------------------------------------------
    !+
    ! For a subtree given by pointer `p_node` follow all left children until
    ! there are not more left and return the pointer to this node holding the
    ! minimum value.
    !-
    function bst_find_min(p_node) result(p_min)
        implicit none
        type(bst_type), pointer :: p_min
        type(bst_type), pointer, intent(in) :: p_node

        if (associated(p_node)) then
            p_min => p_node
            do while (associated(p_min%left))
                p_min => p_min%left
            end do
        else
            write(stderr, *) "bst_mod::bst_find_min: input pointer p_node is not associated."
            stop 1
        end if
        return
    end function bst_find_min
    !---------------------------------------------------------------------------
    !+
    ! For a subtree given by the pointer `p_node` follow all right children
    ! until there are not more right and return the pointer to this node holding
    ! the maximum value.
    !-
    function bst_find_max(p_node) result(p_max)
        implicit none
        type(bst_type), pointer :: p_max
        type(bst_type), pointer, intent(in) :: p_node

        if (associated(p_node)) then
            p_max => p_node
            do while (associated(p_max%right))
                p_max => p_max%right
            end do
        else
            write(stderr, *) "bst_mod::bst_find_max: input pointer p_node is not associated."
            stop 1
        end if
        return
    end function bst_find_max
    !---------------------------------------------------------------------------
    !+
    ! Given a pointer to a node in a tree, find the successor of the node in an
    ! inorder tree walk.  Keys must be distinct.  The successor of a node
    ! `p_node` is the smallest value greater than p_node%value.  If this node
    ! doesn't exist or if the initial node has the largest value in the tree,
    ! then return null().
    !-
    function bst_successor(p_node) result(p_scsr)
        implicit none
        type(bst_type), pointer :: p_scsr
        type(bst_type), pointer, intent(in) :: p_node
        type(bst_type), pointer :: p_prnt

        p_scsr => null()
        if (associated(p_node%right)) then
            ! If the right subtree is not empty, then the successor is the
            ! leftmost node in it.
            p_scsr => bst_find_min(p_node%right)
        else
            ! If there is no right subtree and the successor exists then it is
            ! the lowest ancestor of a node whose left child is also
            ! an ancestor of x.  To find it just go up from `p_node` until you
            ! find a node that is a left child of its parent. 
            p_scsr => p_node
            p_prnt => p_node%parent
            do
                if (.not.associated(p_prnt)) exit
                if (.not.associated(p_prnt%right, p_scsr)) exit
                p_scsr => p_prnt
                p_prnt => p_scsr%parent
            end do
        end if
        return
    end function bst_successor
    !---------------------------------------------------------------------------
    !+
    ! Given a pointer to a node in a tree, find its predecessor in an inorder
    ! tree walk.  Keys must be distinct.  The predecessor of node `p_node` is
    ! the biggest value smaller than p_node%value.  If this node doesn't exist
    ! or if the initial node has the smallest value in the tree, then return
    ! null().
    !-
    function bst_predecessor(p_node) result(p_scsr)
        implicit none
        type(bst_type), pointer :: p_scsr
        type(bst_type), pointer, intent(in) :: p_node
        type(bst_type), pointer :: p_prnt

        if (associated(p_node%left)) then
            ! If the left subtree is not empty, then the predecessor is the
            ! rightmost node in it.
            p_scsr => bst_find_max(p_node%left)
        else
            ! If there is no left subtree and the predecessor exists then it is
            ! the lowest ancestor of the `p_node` node whose right child is also
            ! an ancestor of x.  To find it just go up from `p_node` until you
            ! find a node that is a right child of its parent.
            p_scsr => p_node
            p_prnt => p_node%parent
            do
                if (.not.associated(p_prnt)) exit
                if (.not.associated(p_prnt%left, p_scsr)) exit
                p_scsr => p_prnt
                p_prnt => p_scsr%parent
            end do
        end if
        return
    end function bst_predecessor
    !---------------------------------------------------------------------------
    !+
    ! Inserts a new (allocated) node into a tree starting from root.
    !-
    subroutine bst_insert(p_root, p_new)
        implicit none
        type(bst_type), pointer, intent(inout) :: p_root, p_new
        type(bst_type), pointer :: p_prnt, p_crnt
        
        p_prnt => null()
        p_crnt => p_root
        do while (associated(p_crnt))
            p_prnt => p_crnt
            if (p_new%value < p_crnt%value) then
                p_crnt => p_crnt%left
            else
                p_crnt => p_crnt%right
            end if
        end do
        p_new%parent => p_prnt
        if (.not.associated(p_prnt)) then ! tree p_root was empty
            p_root => p_new
        else if (p_new%value < p_prnt%value) then
            p_prnt%left => p_new
        else ! (p_prnt%value <= p_new%value)
            p_prnt%right => p_new
        end if
    end subroutine bst_insert
    !---------------------------------------------------------------------------
    !+
    ! Ancillary routine that for the tree, pointer by `p_root` replaces the
    ! old node (u in Cormen) with the new node (v in Cormen).
    !-
    subroutine bst_transplant(p_root, p_old, p_new)
        implicit none
        type(bst_type), pointer, intent(inout) :: p_root, p_new
        type(bst_type), pointer, intent(in) :: p_old
        type(bst_type), pointer :: p_prnt

        p_prnt => p_old%parent
        if (.not.associated(p_prnt)) then ! root
            p_root => p_new
        else if (associated(p_old, p_prnt%left)) then
            p_prnt%left => p_new
        else ! p_old is p_prnt%right
            p_prnt%right => p_new
        end if
        if (associated(p_new)) then
            p_new%parent => p_prnt
        end if
    end subroutine bst_transplant
    !---------------------------------------------------------------------------
    !+
    ! For a tree, pointed by root node, deletes node `p_z`.  See Cormen et al.
    ! (2009), p. 295.
    !-
    subroutine bst_delete(p_root, p_z)
        implicit none
        type(bst_type), pointer, intent(inout) :: p_root, p_z
        type(bst_type), pointer :: p_l, p_r, p_y

        if (.not.associated(p_z%left)) then
            call bst_transplant(p_root, p_z, p_z%right)
        else if (.not.associated(p_z%right)) then
            call bst_transplant(p_root, p_z, p_z%left)
        else
            p_y => bst_find_min(p_z%right)
            if (.not.associated(p_y%parent, p_z)) then
                call bst_transplant(p_root, p_y, p_y%right)
                p_r => p_z%right
                p_y%right  => p_r
                p_r%parent => p_y
            end if
            call bst_transplant(p_root, p_z, p_y)
            p_l => p_z%left
            p_y%left   => p_l
            p_l%parent => p_y
        end if
        deallocate(p_z)
    end subroutine bst_delete
    !---------------------------------------------------------------------------
end module bst_mod

! The test tree:
!       6
!      / \
!     /   \
!    /     \
!   3      15
!   /\     /\
!  /  \   /  \
! 2   5  7   20
!     /        \
!    4         30
program binary_search_tree
    use bst_mod, only: bst_type, bst_insert, bst_print, bst_size, bst_height, &
        bst_remove, bst_delete, bst_search
    implicit none
    type(bst_type), pointer :: bst_root => null()
    type(bst_type), pointer :: bst_tmp  => null()
    integer :: v, iostatus

    ! Read and create the tree.
    do
        read(*, *, iostat=iostatus) v
        if (iostatus /= 0) exit
        allocate(bst_tmp)
        bst_tmp%value = v
        call bst_insert(bst_root, bst_tmp)
    end do
    bst_tmp => null()

    call bst_print(bst_root)

    print '(a, i0)', "The size of root is ", bst_size(bst_root)
    print '(a, i0)', "The height of root is ", bst_height(bst_root)
    print '(a, i0)', "The height of root%left is ", bst_height(bst_root%left)
    print '(a, i0)', "The height of root%right is ", bst_height(bst_root%right)

    bst_tmp => bst_search(bst_root, 15)
    call bst_delete(bst_root, bst_tmp)
    call bst_print(bst_root)

    call bst_remove(bst_root)
    print *, associated(bst_root)
end program binary_search_tree

