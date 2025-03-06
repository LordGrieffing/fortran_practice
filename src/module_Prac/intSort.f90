module intSort
    implicit none

    private
    public bubble

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
    integer :: maxNum
    integer, allocatable :: tallyarray(:)

    



end subroutine countSort

end module intSort