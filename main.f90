program main

  use bettermath
  use naivemath
  use dotmath

  implicit none

  real (kind = 4), allocatable, dimension (:,:) :: A4, B4, C4
  real (kind = 8), allocatable, dimension (:,:) :: A8, B8, C8
  real (kind = 16), allocatable, dimension (:,:) :: A16, B16, C16
  integer :: msize(8)
  integer :: i, j, k
  real (kind = 4) :: rand4
  real (kind = 8) :: rand8
  real (kind = 16) :: rand16
  real :: start
  real :: finish
  
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

           C4(j,k)=0
           C8(j,k)=0
           C16(j,k)=0

        end do
     end do

     !naivemull
     
     call cpu_time(start)
     C4=naivemull(A4,B4)
     call cpu_time(finish)

     write(*,*) "naivemath kind=4",  msize(i), " ", finish-start

     call cpu_time(start)
     C8=naivemull(A8,B8)
     call cpu_time(finish)

     write(*,*) "naivemath kind=8",  msize(i), " ", finish-start

     call cpu_time(start)
     C16=naivemull(A16,B16)
     call cpu_time(finish)

     write(*,*) "naivemath kind=16",  msize(i), " ", finish-start
     
     !bettermull

     call cpu_time(start)
     C4=bettermull(A4,B4)
     call cpu_time(finish)

     write(*,*) "bettermath kind=4",  msize(i), " ", finish-start

     call cpu_time(start)
     C8=bettermull(A8,B8)
     call cpu_time(finish)

     write(*,*) "bettermath kind=8",  msize(i), " ", finish-start

     call cpu_time(start)
     C16=bettermull(A16,B16)
     call cpu_time(finish)

     write(*,*) "bettermath kind=16",  msize(i), " ", finish-start

     !dotmull

     call cpu_time(start)
     C4=dotmull(A4,B4)
     call cpu_time(finish)

     write(*,*) "dotmath kind=4",  msize(i), " ", finish-start

     call cpu_time(start)
     C8=dotmull(A8,B8)
     call cpu_time(finish)

     write(*,*) "dotmath kind=8",  msize(i), " ", finish-start

     call cpu_time(start)
     C16=dotmull(A16,B16)
     call cpu_time(finish)

     write(*,*) "dotmath kind=16",  msize(i), " ", finish-start

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
