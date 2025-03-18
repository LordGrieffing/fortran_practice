! function for math
function f(x) result (y)
    real :: x, y

    y = x**3 + x**2 - (2*x) + 3
end function


! Subroutine that does the integration
subroutine trapIntegrate(n, a, b, estimate)
    
    ! Declare variables and intent
    real, intent(inout) :: estimate
    integer, intent(in) :: n
    real, intent(in) :: a, b
    real :: dx
    integer :: i

    ! Calculate delta x
    dx = (a + b) / n

    
    !For loop sum
    do i = 1, n
        if ((i == 1) .or. (i == n)) then
            estimate = estimate + f(a + (i * dx))
        else
            estimate = estimate + 2 * f(a + (i* dx))
        end if
    end do

end subroutine trapIntegrate

! Subroutine that checks to make sure the user has inputed the proper data
subroutine check_input(n)

    integer, intent(inout) :: n
    integer :: iunit, ios

    iunit = 5
    
    do while (.true.)
        read(iunit, *, iostat = ios) n

        if (ios /= 0) then
            print *, "Please input a valid integer"
        else
            exit
        end if
    end do
end subroutine check_input

! Check real input
subroutine check_real(n)

    real, intent(inout) :: n
    integer :: iunit, ios

    iunit = 5
    
    do while (.true.)
        read(iunit, *, iostat = ios) n

        if (ios /= 0) then
            print *, "Please input a valid integer"
        else
            exit
        end if
    end do
end subroutine check_real


program trapizoidIntegration
    implicit none
    
    ! Declare variables
    integer :: n
    real :: a, b, estimate

    ! Declare esitmate
    estimate = 0

    ! Ask for user input for lower bound
    print *, "Input the lower bound"
    call check_real(a)

    ! Ask for user input for upper bound
    print *, "Input the upper bound"
    call check_real(b)

    ! Ask for user input for the number of steps
    print *, "Input the number of steps"
    call check_input(n)

    ! Do trapazoid integration
    call trapIntegrate(n, a, b, estimate)

    ! Print result
    print *, estimate


end program trapizoidIntegration