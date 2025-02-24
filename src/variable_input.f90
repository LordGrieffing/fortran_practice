program read_values
    implicit none
    real :: x, y 

    print *, 'Please input a value for x: '
    read(*,*) x
    print *, 'Please input a value for y: '
    read(*,*) y
    
    print *, 'The sum of ', x, 'and ', y, 'is: ', x + y

end program read_values