module naivemath
  implicit none

  public :: naivemull
  private :: naive4, naive8, naive16
  
  interface naivemull
     procedure naive4, naive8, naive16
  end interface naivemull
  
     
  
contains

  function naive4 (A, B) result C
    real(kind=4), intent(in), dimension(:,:) :: A, B
    real(kind=4), intent(out), dimension(size(A,1), size(B,2)) :: C
    
    do i=1,size(A,1)
       do j=1,size(B,2)
          do k=1,size(A,2)
             C(i,j)=C(i,j)+A(i,k)*B(k,j)
          end do
       end do
    end do

  end function naive4

  function naive8 (A,B) result C
    real(kind=8), intent(in), dimension(:,:) :: A, B
    real(kind=8), intent(out), dimension(size(A,1), size(B,2)) :: C
    
    do i=1,size(A,1)
       do j=1,size(B,2)
          do k=1,size(A,2)
             C(i,j)=C(i,j)+A(i,k)*B(k,j)
          end do
       end do
    end do

  end function naive8

  function naive16 (A,B) result C
    real(kind=16), intent(in), dimension(:,:) :: A, B
    real(kind=16), intent(out), dimension(size(A,1), size(B,2)) :: C
    
    do i=1,size(A,1)
       do j=1,size(B,2)
          do k=1,size(A,2)
             C(i,j)=C(i,j)+A(i,k)*B(k,j)
          end do
       end do
    end do
  end function naive16
  
  end module naivemath
