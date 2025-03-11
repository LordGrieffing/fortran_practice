module intSort
    implicit none

    private
    public bubble, countSort

    integer :: length
    integer :: i
    integer :: j


contains

subroutine bubble(list)

    ! Declare Variables 
    integer, intent(inout) :: list(:)
    integer :: leftN
    integer :: rightN

    ! Find size of array
    length = size(list)

    do i = 0, length - 1
        do j = 0, length - 1 - i
            leftN = list(j)
            rightN = list(j+1)

            if (leftN > rightN) then
                list(j) = rightN
                list(j+1) = leftN
            end if
        end do
    end do
end subroutine bubble

subroutine countSort(list)

    ! Declare Variables
    integer, intent(inout) :: list(:)
    integer :: maxNum, position 
    integer, allocatable :: tallyarray(:), reverselist(:), sortedlist(:)

    ! Construct tally array
    allocate(tallyarray(maxNum + 1))
    tallyarray = 0

    do i = 1, size(list)
        tallyarray(list(i)) = tallyarray(list(i)) + 1

    end do

    ! Sum the occurances
    do i = 2, size(tallyarray)
        tallyarray(i) = tallyarray(i-1)

    end do

    ! Reverse the Array
    allocate(reverselist(size(list)))
    do i = 1, size(list)
        reverselist(i) = list(size(list)-i + 1)

    end do 

    ! Place elements into their correct positions in the sorted array
    allocate(sortedlist(size(list)))
    do i = 1, size(list)
        position = tallyarray(reverselist(i)) - 1
        sortedlist(position) = list(i)
        tallyarray(list(i)) = tallyarray(list(i)) - 1


    end do

    list = sortedlist




end subroutine countSort

end module intSort