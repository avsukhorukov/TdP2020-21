program aliasing_arrays
    implicit none
    integer, parameter :: N = 16
    integer :: i
    integer, target  :: a(N)
    integer, pointer :: even(:), odd(:)

    a(:) = [ (i, i = 1, N) ]
    write(*, '(a, *(1x, i2))') "Before: a =", a

    odd  => a(1:N:2)
    even => a(2:N:2)
    odd(:)  = -1
    even(:) = +1

    write(*, '(a, sp, *(x, i2))') "After:  a =", a

end program aliasing_arrays