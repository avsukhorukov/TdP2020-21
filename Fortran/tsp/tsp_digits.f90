program tsp_digits
    implicit none
    integer :: n, i, j, c, v, distance, min_distance
    integer, allocatable :: d(:, :), route(:), min_route(:)
    logical :: is_unique

    print '(a)', "How many towns?"
    read *, n

    allocate(d(n, n))
    print '(a)', "Distances between the towns are"
    do i = 1, n
        read *, d(i, :)
    end do

    allocate(route(n))
    route(:) = [(1, i = 1, n)]
    allocate(min_route(n))
    min_route(:) = huge(0)

    min_distance = huge(0)
    c = n
    permutation: do while (0 < c)
        ! Is permutation unique?
        is_unique = .true.
        unique: do i = 1, n
            v = route(i)
            do j = i + 1, n
                if (v == route(j)) then
                    is_unique = .false.
                    exit unique
                end if
            end do
        end do unique

        ! If it is unique, then find the distance, check if it is smaller than
        ! the one you've remembered, if so, then remember this distance and the
        ! corresponding route.
        if (is_unique) then
            distance = d(route(n), route(1))
            do i = 2, n
                distance = distance + d(route(i - 1), route(i))
            end do
            if (distance < min_distance) then
                min_distance = distance
                min_route(:) = route(:)
            end if
        end if
        ! Increment the permutation.  The last number is the fastest.  Index `c`
        ! points to the current city.  If route(c) exceeds the number of cities,
        ! it becomes 1 again and the cities to the left are incremented
        ! gradually.  Then the current city is again the last one.  Stop the
        ! process once the current city index is 0, that is, when the route is
        ! [n, ..., n].
        increment: do while (0 < c)
            route(c) = route(c) + 1
            if (route(c) > n) then
                route(c) = 1
                c = c - 1
            else
                exit increment
            end if
        end do increment
        if (0 < c) c = n
    end do permutation

    print '(a, i0, a)', "The shortest distance ", min_distance, " is for route:"
    print '(*(i0, 2x))', min_route(:)

    deallocate(min_route)
    deallocate(d)
    deallocate(route)
end program tsp_digits
