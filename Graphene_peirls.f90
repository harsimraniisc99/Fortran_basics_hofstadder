! This is main code for graphene in magnetic field
program main
    IMPLICIT NONE
    external zheev
    integer :: xk, yk, nb
    real, allocatable, dimension(:,:) :: lattice, delta_l,reciprocal_lattice
    real, dimension(2) :: k
    complex*16, dimension(2,2) :: ham_k
    complex*16 :: dummy(1)
    integer, parameter :: N = 2 ! Size of the matrix
    integer :: info, lwork, lda
    complex*16, allocatable, dimension(:) :: work
    complex*16 :: eigenvalues(N), eigenvectors(N,N), rwork(3*N-2)
    ! allocating 
    allocate(lattice(2,2))
    allocate(delta_l(2,3))
    allocate(reciprocal_lattice(2,2))
    call create_lattice(lattice,delta_l,reciprocal_lattice)
    nb=64
    lda=N
! open file to write
    open(unit=99,file="eigenvalues.dat")
    do xk=0,100,1
        DO yk=0,100,1
            k= (xk/100.0)*reciprocal_lattice(:,1)+(yk/100.0)*reciprocal_lattice(:,2)
            call create_Ham(lattice,delta_l,k,ham_k)
            print *,"Hamiltonian", ham_k
    ! Initialize your matrix A here
            lwork=-1
    ! Call LAPACK eigensolver
            call zheev('Vectors', 'U', N, ham_k, lda, eigenvalues, dummy,lwork, rwork, info)
            lwork = max((nb+1)*N, nint(real(dummy(1))))
            allocate(work(lwork))
            call zheev('Vectors', 'U', N, ham_k, lda, eigenvalues, work,lwork, rwork, info)
            write(99,*) eigenvalues(1)
            print *, eigenvalues(1)
            print *, shape(eigenvalues(1))
            deallocate(work)
        end do
    end do
end program main

subroutine create_lattice(lattice,delta_l,reciprocal_lattice)
    IMPLICIT NONE
    real, dimension(2,2) :: lattice
    real, dimension(2,3) :: delta_l
    real, dimension(2,2) :: reciprocal_lattice(2,2)

    ! defining
    lattice(:,1)= (/ 1.0, 0.0 /)
    lattice(:,2)= (/ 1.0/2.0, sqrt(3.0)/2.0 /)

    ! delta_l

    delta_l(:,1)= 0.3333*lattice(:,1) + 0.3333*lattice(:,2)
    delta_l(:,2)= (0.3333-1)*lattice(:,1) + 0.5*lattice(:,2)
    delta_l(:,3)= 0.5*lattice(:,1) + (0.333 -1)*lattice(:,2)

    reciprocal_lattice(:,1)= (2*3.14)*(/-1*lattice(2,1),lattice(1,1)/)
    reciprocal_lattice(:,2)= (2*3.14)*(/-1*lattice(2,2),lattice(1,2)/)
    return 
    END

subroutine create_Ham(lattice,delta_l,k,ham_k)
    IMPLICIT NONE
    real, dimension(2,2) :: lattice
    real, dimension(2,3) :: delta_l
    real, dimension(2) :: k
    integer ::i
    complex*16, dimension(2,2) :: ham_k
    ham_k(1,1)=0.0
    ham_k(2,2)=0.0
    ham_k(1,2)=0.0
    do i=1,3 
        ham_k(1,2)= ham_k(1,2)+ exp(cmplx(0,1)* dot_product(k,delta_l(:,i)))
    end do

    ham_k(2,1)=conjg(ham_k(1,2))
end subroutine create_Ham


