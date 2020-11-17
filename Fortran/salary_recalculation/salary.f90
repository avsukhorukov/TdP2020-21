program salary_recalculation
    implicit none
    real, parameter :: PERCENT = 0.01
    integer :: n, n_cat, i
    real    :: curr_cost, new_cost
    real,    allocatable :: salaries(:), increase(:)
    integer, allocatable :: categories(:)

    print '(a)', "The number of employees is"
    read *, n
    allocate(salaries(n))
    allocate(categories(n))

    print '(a)', "The number of categories is"
    read *, n_cat
    allocate(increase(n_cat))

    print '(a)', "Salaries of each empoyee are"
    read *, salaries
    print '(a)', "Categories of each empoyee are"
    read *, categories
    print '(a)', "Salary increase in each category is"
    read *, increase

    curr_cost = sum(salaries)
    employee: do i = 1, n
        salaries(i) = salaries(i) * (1.0 + PERCENT * increase(categories(i)))
    end do employee
    new_cost = sum(salaries)
    print '(a, f0.5)', "Current cost: ", curr_cost
    print '(a, f0.5)', "New cost: ", new_cost
    print '(a, f0.5)', "Increase: ", new_cost - curr_cost

    deallocate(salaries)
    deallocate(categories)
    deallocate(increase)
end program salary_recalculation