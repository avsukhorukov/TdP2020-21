program time_array_multiply
    implicit none
    integer :: seed_size
    integer, allocatable :: seed(:)
    integer, parameter :: N = 500
    integer, parameter :: N_REPS = 10
    real :: a(N, N)
    real :: b(N, N)
    real :: c(N, N)
    integer :: r, i, j, alpha
    real :: tmp
    real :: t1, t2

    call random_seed(size=seed_size)
    allocate(seed(seed_size), source=0)
    call random_seed(put=seed)
    deallocate(seed)
    call random_number(a)
    call random_number(b)

    call cpu_time(t1)
    iaja: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                tmp = 0.0
                do alpha = 1, N
                    tmp = tmp + a(i, alpha) * b(j, alpha)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do iaja
    call cpu_time(t2)
    print '(a, es8.2, a)', "Contraction (i, *)(j, *) took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    iaaj: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                tmp = 0.0
                do alpha = 1, N
                    tmp = tmp + a(i, alpha) * b(alpha, j)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do iaaj
    call cpu_time(t2)
    print '(a, es8.2, a)', "Contraction (i, *)(*, j) took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    aija: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                tmp = 0.0
                do alpha = 1, N
                    tmp = tmp + a(alpha, i) * b(j, alpha)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do aija
    call cpu_time(t2)
    print '(a, es8.2, a)', "Contraction (*, i)(j, *) took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    aiaj: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                tmp = 0.0
                do alpha = 1, N
                    tmp = tmp + a(alpha, i) * b(alpha, j)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do aiaj
    call cpu_time(t2)
    print '(a, es8.2, a)', "Contraction (*, i)(*, j) took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    icrj: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                c(i, j) = sum(a(i, :) * b(:, j))
            end do ! j
        end do ! i
    end do icrj
    call cpu_time(t2)
    print '(a, es8.2, a)', "Contraction (i, :)(:, j) took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    mat_mul: do r = 1, N_REPS
        c(:, :) = matmul(a(:, :), b(:, :))
    end do mat_mul
    call cpu_time(t2)
    print '(a, es8.2, a)', "With matmul (i, :)(:, j) took ", (t2 - t1) / N_REPS, " s."

end program time_array_multiply
