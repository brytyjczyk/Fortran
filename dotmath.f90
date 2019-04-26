module dotmath
  implicit none

  public :: dotmull
  private:: dot4, dot8, dot16

  interface dotmull
     procedure dot4, dot8, dot16
  end interface dotmull

contains
  
  function dot4 (A, B) result (C)

    real(kind=4), intent(in), dimension(:,:) :: A, B
    real(kind=4), dimension(size(A,1),size(B,2)) :: C
    real(kind=4) :: vectorA(size(A,2)), vectorB(size(A,2))
    integer :: i, j

    do j=1,size(B,2)
       do i=1, size(A,1)
          vectorA = A(i,:)
          vectorB = B(:,j)
          C(i,j)=dot_product(vectorA, vectorB)
       end do
    end do   
  end function dot4

  function dot8 (A, B) result (C)

    real(kind=8), intent(in), dimension(:,:) :: A, B
    real(kind=8), dimension(size(A,1),size(B,2)) :: C
    real(kind=8) :: vectorA(size(A,2)), vectorB(size(A,2))
    integer :: i, j

    do j=1,size(B,2)
       do i=1, size(A,1)
          vectorA = A(i,:)
          vectorB = B(:,j)
          C(i,j)=dot_product(vectorA, vectorB)
       end do
    end do   
  end function dot8

  function dot16 (A, B) result (C)

    real(kind=16), intent(in), dimension(:,:) :: A, B
    real(kind=16), dimension(size(A,1),size(B,2)) :: C
    real(kind=16) :: vectorA(size(A,2)), vectorB(size(A,2))
    integer :: i, j

    do j=1,size(B,2)
       do i=1, size(A,1)
          vectorA = A(i,:)
          vectorB = B(:,j)
          C(i,j)=dot_product(vectorA, vectorB)
       end do
    end do   
  end function dot16
  
end module dotmath
