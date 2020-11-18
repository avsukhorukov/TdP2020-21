program array_pointers
    implicit none
    integer, parameter :: N = 10
    integer :: i
    integer, target :: vector(N) = [ (i, i = 1, N) ]
    !integer, pointer :: vector(:)
    integer, pointer :: ptr_s
    integer, pointer :: ptr_v1(:)
    integer, pointer :: ptr_v2(:)
    integer, pointer :: ptr_v3(:)

    !allocate(vector(N))
    !vector(:) = [ (i, i = 1, N) ]

    ptr_s => vector(5)
    print "(a, i0)", "Scalar ptr_s is ", ptr_s

    ptr_v1 => vector(7:7)
    print "(a, i0)", "Vector ptr_v1 is ", ptr_v1

    ptr_v2 => vector
    print "(a, *(i0, 1x))", "Vector ptr_v2 is ", ptr_v2

    ptr_v3 => vector(2:10:2)
    print "(a, *(i0, 1x))", "Vector ptr_v3 is ", ptr_v3

    !deallocate(vector)
end program array_pointers