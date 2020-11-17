! This exercies is based on Exercise 9 on p. 66 from Chirila & Lohmann (2015).
program time_array_add
    implicit none
    integer, parameter :: N_REPS = 8
    integer, parameter :: N = 256
    real :: a(N, N, N)
    real :: b(N, N, N)
    integer :: i, j, k, r
    real :: t1, t2
    integer :: seed_size
    integer, allocatable :: seed(:)

    call random_seed(size=seed_size)
    allocate(seed(seed_size), source=0)
    call random_seed(put=seed)
    deallocate(seed)
    call random_number(a)
    call random_number(b)

    call cpu_time(t1)
    kji: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                do k = 1, N
                    a(i, j, k) = a(i, j, k) + b(i, j, k)
                end do ! k
            end do ! j
        end do ! i
    end do kji
    call cpu_time(t2)
    print '(a, es8.2, a)', "Order k-j-i took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    ijk: do r = 1, N_REPS
        do k = 1, N
            do j = 1, N
                do i = 1, N
                    a(i, j, k) = a(i, j, k) + b(i, j, k)
                end do ! i
            end do ! j
        end do ! k
    end do ijk
    call cpu_time(t2)
    print '(a, es8.2, a)', "Order i-j-k took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    sum: do r = 1, N_REPS
        a(:, :, :) = a(:, :, :) + b(:, :, :) ! Equivalent to `a = a + b`.
    end do sum
    call cpu_time(t2)
    print '(a, es8.2, a)', "Array sum took ", (t2 - t1) / N_REPS, " s."

end program time_array_add