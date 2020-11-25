! 7.2.3 Scatter, update, and gather an array
!
! 1) Rank 0 reads in size S and initializeds a 1D array with an arithmetic
!    progression.
! 2) This array is divided in chunks and sent to other processes.
! 3) Each process updates the chunk by adding its rank times 100 and sends it
!    back to root.  Start from equal chunks, later try with unequal.
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace divide_array.f90
! $ mpirun -np 5 --oversubscribe ./a.out
! Enter size 17.
program divide_array
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    type(MPI_Comm) :: comm = MPI_COMM_WORLD
    integer :: s, count, i, q, r, disp
    integer, allocatable :: a(:), b(:), counts(:), displs(:)

    call MPI_Init()
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    s = 0
    if (my_rank == root) read *, s
    allocate(a(s))                  ! root: s=s, other: s=0
    if (my_rank == root) then
        a(:) = [ (i, i = 1, s) ]
        print '(*(i3, 1x))', a
    end if
    call MPI_Bcast(s, 1, MPI_INTEGER, root, comm)

    r = mod(s, n_ranks)
    q = (s - r) / n_ranks

    disp = 0
    allocate(counts(0:n_ranks - 1))
    allocate(displs(0:n_ranks - 1))
    do i = 0, n_ranks - 1
        if (i < r) then
            counts(i) = q + 1
        else
            counts(i) = q
        end if
        displs(i) = disp
        disp      = disp + counts(i)
        if (my_rank == i) count = counts(i)
    end do
    allocate(b(count))

    call MPI_Scatterv(a, counts, displs, MPI_INTEGER, b, count, MPI_INTEGER, root, comm)

    ! Update local array b.
    b(:) = b(:) + my_rank * 100

    call MPI_Gatherv(b, count, MPI_INTEGER, a, counts, displs, MPI_INTEGER, root, comm)

    if (my_rank == root) then
        print '(*(i3, 1x))', a
    end if

    if (allocated(a)) deallocate(a)
    if (allocated(b)) deallocate(b)
    if (allocated(counts)) deallocate(counts)
    if (allocated(displs)) deallocate(displs)
    call MPI_Finalize()
end program divide_array
