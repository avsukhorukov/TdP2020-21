program hanoi
    implicit none
    integer :: n

    print *, "The number of disks is"
    read *, n
    call move_hanoi(n, "A", "B", "C")
contains
    recursive subroutine move_hanoi(m, src, tmp, dst)
        integer, intent(in) :: m
        character, intent(in) :: src, tmp, dst
        if (m == 1) then
            print '(i2, 4a)', m, ": ", src, " -> ", dst
        else
            call move_hanoi(m - 1, src, dst, tmp)
            print '(i2, 4a)', m, ": ", src, " -> ", dst
            call move_hanoi(m - 1, tmp, src, dst)
        end if
    end subroutine move_hanoi
end program hanoi