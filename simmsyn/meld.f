
c***********************************************************************
      subroutine meld (char1, char2, char3, num3)
c***********************************************************************
c     this routine simply merges character strings char1 and char2 
c     to produce a third string char3, with its character count num3

      implicit real*8 (a-h,o-z)
      integer num1, num2, num3
      character*80 char1, char2, char3

      call countline (char1, num1)
      call countline (char2, num2)
      char3(1:num1) = char1(1:num1)
      char3(num1+1:num1+num2) = char2(1:num2)
      num3 = num1 + num2
      do i=num3+1,80
         char3(i:i) = ' '
      enddo
    
      return
      end

 
