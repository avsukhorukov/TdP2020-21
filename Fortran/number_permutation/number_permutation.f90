! You cannot use `digits` as a variable because this is an intrinsic function.
! n_perms at the end must give the factorial of n_digs.
!
! This problem is solved using two different permutations: either you rotate the
! array using the cfsit() intrinsic function, or you swap to array elements with
! backtracking swap back.
program number_permutation
    implicit none
    integer :: n, n_digs, n_perms, d, tmp, r
    integer, allocatable :: digs(:)

    print '(a)', "The number is"
    read *, n

    ! Decompose it into digits
    n_digs = floor(log10(real(n)) + 1)
    allocate(digs(n_digs))
    tmp = n
    do d = 1, n_digs
        r = mod(tmp, 10)
        digs(n_digs + 1 - d) = r
        tmp = (tmp - r) / 10
    end do
    n_perms = 0
    !call cshift_permute(digs, n_digs)
    call swap_permute(digs, 1)
    print '(i0, a)', n_perms, " permutations" 
    deallocate(digs)

contains

    recursive subroutine cshift_permute(a, k)
        implicit none
        integer, intent(inout) :: a(:)
        integer, intent(in) :: k
        integer :: i
        if (k == 1) then
            print '(*(i1))', a(:)
            n_perms = n_perms + 1
        else        
            do i = 1, k
                call cshift_permute(a, k - 1)
                a(1:k) = cshift(a(1:k), 1)
            end do
        end if
    end subroutine cshift_permute

    subroutine swap(a, b)
        implicit none
        integer, intent(inout) :: a, b
        integer :: temp
        temp = a
        a    = b
        b    = temp
    end subroutine swap

    recursive subroutine swap_permute(a, k)
        implicit none
        integer, intent(inout) :: a(:)
        integer, intent(in) :: k
        integer :: i
        if (k == size(a)) then
            print '(*(i1))', a(:)
            n_perms = n_perms + 1
        else
            do i = k, size(a)
                call swap(a(k), a(i))
                call swap_permute(a, k + 1)
                call swap(a(k), a(i))
            end do
        end if
    end subroutine swap_permute
end program number_permutation