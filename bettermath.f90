module bettermath
implicit none
  
  public bettermull

  private better4, better8, better16

  interface bettermull
     procedure better4, better8, better16
  end interface bettermull
  
contains

  function better4 (A,B) result C
    real(kind=4), intent(in), dimension(:,:) :: A, B
    real(kind=4), intent(out), dimention(size(A,1), size(B,2)) :: C
    do j=1,size(B,2)
       do k=1, size(A,2)
          do i=1,size(A,1)
             C(i,j)=C9i,j)+A(i,k)*B(k,j)
          end do
       end do
    end do
    
    
  end function better4


  function better8 (A,B) result C
      real(kind=4), intent(in), dimension(:,:) :: A, B
    real(kind=4), intent(out), dimention(size(A,1), size(B,2)) :: C
    do j=1,size(B,2)
       do k=1, size(A,2)
          do i=1,size(A,1)
             C(i,j)=C9i,j)+A(i,k)*B(k,j)
          end do
       end do
    end do
  end function better8

  function better16 (A, B) result C
      real(kind=4), intent(in), dimension(:,:) :: A, B
    real(kind=4), intent(out), dimention(size(A,1), size(B,2)) :: C
    do j=1,size(B,2)
       do k=1, size(A,2)
          do i=1,size(A,1)
             C(i,j)=C9i,j)+A(i,k)*B(k,j)
          end do
       end do
    end do
  end function better16
  

  
end module bettermath
