program type_indexed
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank, first, last, current
    type(MPI_Status) :: status
    integer, parameter :: SIDE = 5
    integer :: a(SIDE, SIDE) = 0
    integer :: block_lens(SIDE), displacements(SIDE)
    integer :: i
    type(MPI_Datatype) :: my_ltriangle

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    first = 0
    last  = n_ranks - 1

    block_lens(:)    = [(SIDE - i + 1, i = 1, SIDE)] ! [5, 4, 3, 2, 1]
    displacements(:) = [((SIDE + 1) * (i - 1), i = 1, SIDE)] ! [0, 6, 12, 18, 24], diagonal

    call MPI_Type_indexed(SIDE, block_lens, displacements, MPI_INTEGER, my_ltriangle)
    call MPI_Type_commit(my_ltriangle)

    if (my_rank == first) then
        a(:, :) = reshape([(i, i = 1, SIDE * SIDE)], shape=[SIDE, SIDE])
        call MPI_Send(a, 1, my_ltriangle, last, 0, MPI_COMM_WORLD)
    else if (my_rank == last) then
        call MPI_Recv(a, 1, my_ltriangle, first, 0, MPI_COMM_WORLD, status)
    end if
    do current = 0, n_ranks - 1
        if (current == my_rank) then
            print '(a, i0)', "Rank ", my_rank
            do i = 1, SIDE
                print '(*(i2, 1x))', a(i, :)
            end do
        end if
        call MPI_Barrier(MPI_COMM_WORLD)
    end do

    call MPI_Type_free(my_ltriangle)
    call MPI_Finalize()
end program type_indexed
