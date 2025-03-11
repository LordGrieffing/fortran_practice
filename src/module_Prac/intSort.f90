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

    ! Get maxNum 
    maxNum = maxval(list)

    ! Construct arrays
    allocate(tallyarray(maxNum + 1))
    allocate(reverselist(size(list)))
    allocate(sortedlist(size(list)))
    tallyarray = 0


    do i = 1, size(list)
        tallyarray(list(i)+1) = tallyarray(list(i)+1) + 1

    end do


    ! Sum the occurances
    do i = 2, size(tallyarray)
        tallyarray(i) = tallyarray(i-1) + tallyarray(i)

    end do

    ! Reverse the Array
    do i = 1, size(list)
        reverselist(i) = list(size(list)-i + 1)

    end do 

    ! Place elements into their correct positions in the sorted array
    do i = 1, size(list)
        position = tallyarray(reverselist(i)) + 1
        sortedlist(position) = reverselist(i)
        tallyarray(reverselist(i)) = tallyarray(reverselist(i)) - 1


    end do

    list = sortedlist




end subroutine countSort

end module intSort