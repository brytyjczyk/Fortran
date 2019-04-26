program main

  !use bettermath
  !use naivemath
  !use dotmath

  implicit none

  real (kind = 4), allocatable, dimension (:,:) :: A4, B4, C4
  real (kind = 8), allocatable, dimension (:,:) :: A8, B8, C8
  real (kind = 16), allocatable, dimension (:,:) :: A16, B16, C16
  integer :: msize(8)
  integer :: i, j, k
  real (kind = 4) :: rand4
  real (kind = 8) :: rand8
  real (kind = 16) :: rand16
  
  msize=[10,20,40,80,160,320,640,1280]

  !alokowanie tablic i tworzenie macierzy
  do i=1,8 
     
     allocate(A4(msize(i),msize(i)))
     allocate(B4(msize(i),msize(i)))
     allocate(C4(msize(i),msize(i)))

     allocate(A8(msize(i),msize(i)))
     allocate(B8(msize(i),msize(i)))
     allocate(C8(msize(i),msize(i)))
     
     allocate(A16(msize(i),msize(i)))
     allocate(B16(msize(i),msize(i)))
     allocate(C16(msize(i),msize(i)))

     do j=1,msize(i) !wypelnianie macierzy
        do k=1,msize(i)
           
           CALL RANDOM_NUMBER(rand4)
           A4(j,k) = rand4
           CALL RANDOM_NUMBER(rand4)
           B4(j,k) = rand4

           CALL RANDOM_NUMBER(rand8)
           A8(j,k) = rand8
           CALL RANDOM_NUMBER(rand8)
           B8(j,k) = rand8

           CALL RANDOM_NUMBER(rand16)
           A16(j,k) = rand16
           CALL RANDOM_NUMBER(rand16)
           B16(j,k) = rand16
        end do
     end do


     ! dealokowanie tablicy zaalokowaniej
     if(allocated(A4)) deallocate(A4)
     if(allocated(B4)) deallocate(B4)
     if(allocated(C4)) deallocate(C4)

     if(allocated(A8)) deallocate(A8)
     if(allocated(B8)) deallocate(B8)
     if(allocated(C8)) deallocate(C8)

     if(allocated(A16)) deallocate(A16)
     if(allocated(B16)) deallocate(B16)
     if(allocated(C16)) deallocate(C16)
     
  end do
    
end program main
