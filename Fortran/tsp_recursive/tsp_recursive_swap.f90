program tsp_recursive
    implicit none
    integer :: n, min_distance, i
    integer, allocatable :: d(:, :)
    integer, allocatable :: min_route(:), route(:)

    print '(a)', "How many towns?"
    read *, n

    allocate(d(n, n))
    print '(a)', "Distances between the towns are"
    do i = 1, n
        read *, d(i, :)
    end do

    min_distance = huge(0)
    allocate(min_route(n))
    allocate(route(n))

    route(:) = [ (i, i = 1, n) ] ! [1, 2, 3, ..., n]
    call permute(route, 1)

    print '(a, i0, a)', "The shortest distance ", min_distance, " is for route:"
    print '(*(i0, 2x))', min_route(:)

    deallocate(d)
    deallocate(min_route)
    deallocate(route)
contains
    subroutine swap(a, b)
        implicit none
        integer, intent(inout) :: a, b
        integer :: temp
        temp = a
        a    = b
        b    = temp
    end subroutine swap

    recursive subroutine permute(a, m)
        implicit none
        integer, intent(inout) :: a(:)
        integer, intent(in) :: m
        integer :: j, l
        integer :: asize

        asize = size(a)
        if (m == asize) then
            !print '(*(i2, 2x))', a(:)
            l = d(a(asize), a(1))
            do j = 1, asize - 1
                l = l + d(a(j), a(j + 1))
            end do
            if (l < min_distance) then
                min_distance = l
                min_route(:) = a(:)
            end if
        else
            do j = m, asize
                call swap(a(m), a(j))
                call permute(a, m + 1)
                call swap(a(m), a(j))
            end do
        end if
    end subroutine permute
end program tsp_recursive