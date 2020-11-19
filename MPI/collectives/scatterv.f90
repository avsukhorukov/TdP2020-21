! Run with -np 5 and s = 18.  Sometimes MPI_Barrier doesn't help if the output
! buffer is not flushed on time.
program scatterv
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    type(MPI_Comm) :: comm = MPI_COMM_WORLD
    integer :: s, i, r, q, displacement, count
    integer, allocatable :: rbuf(:), sbuf(:)
    integer, allocatable :: counts(:), displacements(:)

    call MPI_Init()
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Read-in `s` on rank 0, allocate sbuf(s) only on rank 0 and later vector-
    ! scatter it to other ranks.
    s = 0
    if (my_rank == root) read *, s
    allocate(sbuf(s))
    if (my_rank == root) sbuf(:) = [ (i, i = 1, s) ]

    call MPI_Bcast(s, 1, MPI_INTEGER, root, comm)

    r = mod(s, n_ranks)
    q = (s - r) / n_ranks

    ! Count is significant on every rank and must be counts(r).
    ! Solution 1: global on all ranks.
    allocate(counts(0:n_ranks - 1))
    allocate(displacements(0:n_ranks - 1))
    displacement = 0
    do i = 0, n_ranks - 1
        if (i < r) then
            counts(i) = q + 1
        else
            counts(i) = q
        end if
        displacements(i) = displacement
        displacement     = displacement + counts(i)
        if (my_rank == i) count = counts(i) ! pick the local count
    end do
    !! Solution 2: local on root with counts scattered to other ranks.
    !if (my_rank == root) then
    !    allocate(counts(0:n_ranks - 1))
    !    allocate(displacements(0:n_ranks - 1))
    !    displacement = 0
    !    do i = 0, n_ranks - 1 ! start from 1 because of Fortran array indexing
    !        if (i < r) then
    !            counts(i) = q + 1
    !        else
    !            counts(i) = q
    !        end if
    !        displacements(i) = displacement
    !        displacement     = displacement + counts(i)
    !    end do
    !else ! my_rank /= 0
    !    allocate(counts(0))
    !    allocate(displacements(0))
    !end if
    !call MPI_Scatter(counts, 1, MPI_INTEGER, count, 1, MPI_INTEGER, root, comm)

    allocate(rbuf(count), source=0)

    call print_vals()
    call MPI_Scatterv(                            &
        sbuf, counts, displacements, MPI_INTEGER, &
        rbuf, count,                 MPI_INTEGER, &
        root, comm)
    call print_vals()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)
    if (allocated(counts)) deallocate(counts)
    if (allocated(displacements)) deallocate(displacements)
    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    subroutine print_vals()
        implicit none
        integer :: rank

        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", rank, ", sbuf(:)=", sbuf
            end if
            call MPI_Barrier(comm)
        end do
        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", rank, ", rbuf(:)=", rbuf
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_vals
end program scatterv
