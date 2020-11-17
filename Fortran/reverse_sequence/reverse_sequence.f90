! 2.2.1
! Read numbers and print them in reverse order
! Write a program that reads first an integer N, and then N integer numbers,
! which should then be printed in reverse order.
!
! This is a simple solution for a beginner.
program reverse_sequence
    implicit none
    integer :: n_numbers
    integer, allocatable :: numbers(:)

    print *, "Sequence size?"
    read *, n_numbers

    allocate(numbers(n_numbers))

    print *, "Enter ", n_numbers, " integers:"
    read *, numbers

    ! Fortran compiler allows such tricks:
    numbers(1:n_numbers) = numbers(n_numbers:1:-1)

    print *, "Reversed sequence:"
    print *, numbers

    deallocate(numbers)
end program reverse_sequence
