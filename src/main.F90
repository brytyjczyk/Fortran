program main

  use naivemath
  use bettermath
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

  !pliki z wynikami
  open(unit=10, file = "/home/anna/Fortran/zad1/res/naive_kind4", action='write')
  open(unit=20, file = "/home/anna/Fortran/zad1/res/naive_kind8", action='write')
  open(unit=30, file = "/home/anna/Fortran/zad1/res/naive_kind16", action='write')
  
  open(unit=40, file = "/home/anna/Fortran/zad1/res/better_kind4", action='write')
  open(unit=50, file = "/home/anna/Fortran/zad1/res/better_kind8", action='write')
  open(unit=60, file = "/home/anna/Fortran/zad1/res/better_kind16", action='write')
  
  open(unit=70, file = "/home/anna/Fortran/zad1/res/dot_kind4", action='write')
  open(unit=80, file = "/home/anna/Fortran/zad1/res/dot_kind8", action='write')
  open(unit=90, file = "/home/anna/Fortran/zad1/res/dot_kind16", action='write')
  
  open(unit=93, file = "/home/anna/Fortran/zad1/res/mat_kind4", action='write')
  open(unit=96, file = "/home/anna/Fortran/zad1/res/mat_kind8", action='write')
  open(unit=99, file = "/home/anna/Fortran/zad1/res/mat_kind16", action='write')
  

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
     write(10,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C8=naivemull(A8,B8)
     call cpu_time(finish)

     write(*,*) "naivemath kind=8",  msize(i), " ", finish-start
     write(20,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C16=naivemull(A16,B16)
     call cpu_time(finish)

     write(*,*) "naivemath kind=16",  msize(i), " ", finish-start
     write(30,*)  msize(i), " ", finish-start

     
     !bettermull
     call cpu_time(start)
     C4=bettermull(A4,B4)
     call cpu_time(finish)

     write(*,*) "bettermath kind=4",  msize(i), " ", finish-start
     write(40,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C8=bettermull(A8,B8)
     call cpu_time(finish)

     write(*,*) "bettermath kind=8",  msize(i), " ", finish-start
     write(50,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C16=bettermull(A16,B16)
     call cpu_time(finish)

     write(*,*) "bettermath kind=16",  msize(i), " ", finish-start
     write(60,*)  msize(i), " ", finish-start

     
     !dotmull
     call cpu_time(start)
     C4=dotmull(A4,B4)
     call cpu_time(finish)

     write(*,*) "dotmath kind=4",  msize(i), " ", finish-start
     write(70,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C8=dotmull(A8,B8)
     call cpu_time(finish)

     write(*,*) "dotmath kind=8",  msize(i), " ", finish-start
     write(80,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C16=dotmull(A16,B16)
     call cpu_time(finish)

     write(*,*) "dotmath kind=16",  msize(i), " ", finish-start
     write(90,*)  msize(i), " ", finish-start

     
     !matmul
     call cpu_time(start)
     C4= matmul(A4,B4)
     call cpu_time(finish)

     write(*,*) "matmul kind=4",  msize(i), " ", finish-start
     write(93,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C8= matmul(A8,B8)
     call cpu_time(finish)

     write(*,*) "matmul kind=8",  msize(i), " ", finish-start
     write(96,*)  msize(i), " ", finish-start

     call cpu_time(start)
     C16= matmul(A16,B16)
     call cpu_time(finish)

     write(*,*) "matmul kind=16",  msize(i), " ", finish-start
     write(99,*)  msize(i), " ", finish-start
     

     
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

  close(10)
  close(20)
  close(30)
  close(40)
  close(50)
  close(60)
  close(70)
  close(80)
  close(90)
  close(93)
  close(96)
  close(99)

  
end program main
