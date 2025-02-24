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
    read(*,*) length
    allocate(numbers(length))

    ! Ask to input variables
    do i = 1, length
        print *, "Please input the next number"
        read(*,*) numbers(i)
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

