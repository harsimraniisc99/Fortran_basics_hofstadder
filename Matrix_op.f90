! This code is intended to learn and understand arrays and matrix handling
! in fortran 

PROGRAM matrix_op
    IMPLICIT NONE
    integer ::i,j
    REAl, dimension(100) :: x
    REAl, dimension(10,10) :: Ham
    do i=1,10
        do j=1,10
            Ham(i,j)= (i-j)
    end do
    end do

    print *, Ham(:,1)
    end program

    