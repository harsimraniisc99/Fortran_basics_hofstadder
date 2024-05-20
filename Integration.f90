! This code is intended to lean to write functions
! that are more general and can be used in a variety of situations.
! This code aims at understanding the normal fortran code

PROGRAM integrate
    IMPLICIT NONE
    INTEGER :: i
    REAL :: x,y,sum
    ! PRINT *, "enter a number :"
    ! READ *, x
    ! PRINT *, " value of x is :", x
    ! CALL func(x,y)
    ! PRINT *, "value of y is :", y
    sum=0
    DO i=1,100
    x=i/100
    CALL func(x,y)
    sum = sum + y
          
    end do

    print *, "Integration is : ", sum*0.01

END PROGRAM integrate

! subroutine to integrate a function

SUBROUTINE func(x,y)
    REAL :: x,y
    ! square of x 
    y= EXP(-1*x**2)
    return
    END

