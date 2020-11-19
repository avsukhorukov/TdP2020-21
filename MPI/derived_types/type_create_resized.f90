program type_create_resized
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Status) :: status
    integer(kind=MPI_ADDRESS_KIND) :: addrs(4), laddr, uaddr
    type(MPI_Datatype) :: my_struct_t, my_arr_struct_t

    type body_type
        real :: m    = 0.0
        real :: r(3) = [0.0, 0.0, 0.0]
        real :: v(3) = [0.0, 0.0, 0.0]
        real :: a(3) = [0.0, 0.0, 0.0]
    end type body_type

    type(body_type), allocatable :: bodies(:)

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    allocate(bodies(2))

    call MPI_Get_address(bodies(1), laddr)
    call MPI_Get_address(bodies(1)%m, addrs(1))
    call MPI_Get_address(bodies(1)%r, addrs(2))
    call MPI_Get_address(bodies(1)%v, addrs(3))
    call MPI_Get_address(bodies(1)%a, addrs(4))
    !call MPI_Get_address(bodies(2), uaddr)
    addrs(:) = addrs(:) - laddr
    !uaddr    = uaddr    - laddr
    call MPI_Type_create_struct(4, [1, 3, 3, 3], addrs, [MPI_REAL, MPI_REAL, MPI_REAL, MPI_REAL], my_struct_t)
    call MPI_Type_get_extent(my_struct_t, laddr, uaddr)
    call MPI_Type_create_resized(my_struct_t, laddr, uaddr, my_arr_struct_t)
    call MPI_Type_commit(my_arr_struct_t)

    first = 0
    last  = n_ranks - 1

    if (my_rank == first) then
        bodies(1) = body_type(1.0, &
            [1.0, 2.0, 3.0], [0.1, 0.2, 0.3], [9.0, 8.0, 7.0])
        bodies(2) = body_type(2.0, &
            [2.0, 4.0, 6.0], [0.2, 0.4, 0.6], [6.0, 5.0, 4.0])
        call MPI_Send(bodies(1), 2, my_arr_struct_t, last, 0, MPI_COMM_WORLD)
    else if (my_rank == last) then
        print '(a, i0, a, 10(f4.1, 1x))', "Rank ", my_rank, ": body=", bodies(1)
        print '(a, i0, a, 10(f4.1, 1x))', "Rank ", my_rank, ": body=", bodies(2)
        call MPI_Recv(bodies(1), 2, my_arr_struct_t, first, 0, MPI_COMM_WORLD, status)
        print '(a, i0, a, 10(f4.1, 1x))', "Rank ", my_rank, ": body=", bodies(1)
        print '(a, i0, a, 10(f4.1, 1x))', "Rank ", my_rank, ": body=", bodies(2)
    end if

    deallocate(bodies)
    call MPI_Type_free(my_arr_struct_t)
    call MPI_Finalize()
end program type_create_resized
