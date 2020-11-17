program tsp_5loops
    implicit none
    integer :: n, i, j, k, l, m, distance, min_distance
    integer, allocatable :: d(:, :), min_route(:)

    print '(a)', "How many towns?"
    read *, n
    if (n /= 5) stop "This version works only if n is 5."

    allocate(d(n, n))
    print '(a)', "Distances between the towns are"
    do i = 1, n
        read *, d(i, :)
    end do

    allocate(min_route(n))
    min_distance = huge(0)

    first: do i = 1, n
        second: do j = 1, n
            if (i == j) cycle
            third: do k = 1, n
                if (i == k .or. j == k) cycle
                fourth: do l = 1, n
                    if (i == l .or. j == l .or. k == l) cycle
                    fifth: do m = 1, n
                        if (i == m .or. j == m .or. k == m .or. l == m) cycle
                        distance = d(i, j) + d(j, k) + d(k, l) + d(l, m) + d(m, i)
                        if (distance < min_distance) then
                            min_distance = distance
                            min_route(:) = [i, j, k, l, m]
                        end if
                    end do fifth
                end do fourth
            enddo third
        end do second
    end do first

    print '(a, i0, a)', "The shortest distance ", min_distance, " is for the following route:"
    print '(*(i0, 2x))', min_route

    deallocate(d)
    deallocate(min_route)
end program tsp_5loops