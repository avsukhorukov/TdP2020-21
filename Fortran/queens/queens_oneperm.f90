program queens
    implicit none
    integer :: n
    integer, allocatable :: pos(:)

    print '(a, $)', "The board size is "
    read *, n
    allocate(pos(n), source=0)
    call add_queen(pos, 1)
    deallocate(pos)
contains
    recursive subroutine add_queen(a, row)
        implicit none
        integer, intent(inout) :: a(:)
        integer, intent(in) :: row
        integer :: n, c, r, l

        n = size(a)
        if (n < row) then ! print the board
            block
                character(len=n) :: chess_row
                chess_row = repeat('.', n)
                do r = 1, n
                    chess_row(a(r):a(r)) = "Q"
                    write(*, '(a)') chess_row
                    chess_row(a(r):a(r)) = '.'
                end do
                print * ! new line
                !read *  ! wait for key pressed
            end block
        else
            columns: do c = 1, n
                ! Safe column?
                do l = 1, n
                    if (a(l) == c) cycle columns
                end do
                ! Safe diagonals?
                do l = 1, n
                    if (a(l) /= 0) then
                        if (abs(l - row) == abs(a(l) - c)) cycle columns
                    end if
                end do
                a(row) = c ! Add a new queen.
                call add_queen(a, row + 1)
                a(row) = 0 ! Remove the added queen.
            end do columns
        end if
    end subroutine add_queen
end program queens
