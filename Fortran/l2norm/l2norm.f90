program l2norm
    implicit none
    integer :: n
    real, allocatable :: arr(:)
    print '(a, $)', "Size of the vector: "
    read *, n
    allocate(arr(n))
    print '(a, $)', "Vector: "
    read *, arr
    print '(a, g0)', "L2-norm is ", norm2(arr)
    deallocate(arr)
end program l2norm
