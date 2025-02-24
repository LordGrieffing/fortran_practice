! Subroutine that checks to make sure the user has inputed the proper data
subroutine check_input(n)

    integer, intent(inout) :: n
    integer :: iunit, ios

    iunit = 5
    
    do while (.true.)
        read(iunit, *, iostat = ios) n

        if ((ios /= 0) .or. (n < 0) ) then
            print *, "Please input a valid integer"
        else
            exit
        end if
    end do
end subroutine check_input




program bubblesort
    implicit none

    ! Intialize variables
    integer, allocatable :: numbers(:)
    integer :: length
    integer :: i
    integer :: j
    integer :: leftN
    integer :: rightN

    ! Ask how long the array will be
    print *, "How many numbers would you like to sort? "
    call check_input(length)
    allocate(numbers(length))

    ! Inform the user of the allowed inputs
    print *, "Pleae input only positive integers"

    ! Ask to input variables
    do i = 1, length
        print *, "Please input the next number"
        call check_input(numbers(i))
    end do

    do i = 0, length - 1
        do j = 0, length - 1 - i
            leftN = numbers(j)
            rightN = numbers(j+1)

            if (leftN > rightN) then
                numbers(j) = rightN
                numbers(j+1) = leftN
            end if
        end do
    end do

    print *, "The Sorted Input"
    print *, numbers(:)


end program bubblesort

