program array_valgrind
    integer, parameter :: n = 5
    integer :: i
    real, allocatable :: a(:)

    allocate(a(n))
    a = [ (real(i), i = 1, n) ]
    do i = 1, n
        a(i + 1) = a(i + 1)**2
        !a(i) = a(i)**2
    enddo
    !deallocate(a)
end program array_valgrind