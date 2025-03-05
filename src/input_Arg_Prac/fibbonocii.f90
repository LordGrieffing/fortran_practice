recursive function fibbonocii(n) result (fibnum)
    integer(kind = 8) :: n, fibnum

    if(n == 0) then
        fibnum = 0
    else if (n == 1) then
        fibnum = 1
    else
        fibnum = fibbonocii(n-1) + fibbonocii(n-2)
    end if
end function fibbonocii
  
subroutine check_arg(n, arg_test)

    logical, intent(inout) :: arg_test
    integer(kind = 8), intent(inout) :: n
    integer :: ios

    if ((ios /= 0) .or. (n < 0) ) then
        print *, "Please input a valid integer"
        arg_test = .false.
    else
        arg_test = .true.
    end if
end subroutine check_arg


program Main

    ! Declare variables
    logical :: arg_test
    integer(kind=8) :: n, finalfib, fibbonocii
    integer :: num_args, i, ios
    character(len=12), dimension(:), allocatable :: args

    ! Build argument array
    num_args = command_argument_count()
    allocate(args(num_args))

    ! input arguments into agrument array
    do i = 1, num_args
        call get_command_argument(i, args(i))
    end do

    ! Check for valid arguments
    read(args(1),*, iostat = ios) n
    call check_arg(n, arg_test)

    ! Perform fibbonocii or end program
    if (arg_test) then
        finalfib = fibbonocii(n)
        print *, finalfib
    else
        print *, "Incorrect argument input closing program"
    end if

end program Main

