program print_tree
    implicit none
contains
    recursive subroutine traverse_tree(tree)
        if (left_child_exists(tree)) traverse_tree(left_child(tree))
        print *, node_value(tree)
        if (right_child_exists(tree)) traverse_tree(right_child(tree))
        return
    end subroutine traverse_tree
end program print_tree
