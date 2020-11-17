program matrix_multiplication
    implicit none
    integer :: m, n, p
    integer, allocatable :: a(:, :), b(:, :), c(:, :)
    integer :: i, j, k, summa
    character(len=128) :: tmpstr

    ! Get extents of the matrices.
    read *, m, n, p

    allocate(a(m, n), b(n, p), c(m, p))

    do i = 1, m
        read *, a(i, :)
    end do
    do i = 1, n
        read *, b(i, :)
    end do

    ! 1st way: multiplication in explicit loops.
    first_1: do i = 1, m
        last_1: do j = 1, p
            summa = 0
            middle_1: do k = 1, n
                summa = summa + a(i, k) * b(k, j)
            end do middle_1
            c(i, j) = summa
        end do last_1
    end do first_1

    ! 2nd way: multiplication in two outer loops with a slice in between.
    first_2: do i = 1, m
        last_2: do j = 1, p
            c(i, j) = sum(a(i, :) * b(:, j))
        end do last_2
    end do first_2

    ! 3rd way: multiplication sing intrinsic function matmul().  It applies only
    ! for matrices up to rank 2.
    c = matmul(a, b)

    ! Print the result matrix.
    do i = 1, m
        print *, c(i, :)
    end do

    deallocate(a, b, c)
end program matrix_multiplication