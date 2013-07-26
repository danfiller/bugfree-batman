
c***********************************************************************
      subroutine substitute (char1, char2, char3)
c***********************************************************************
c     this routine concatenates the string between the quotation marks
c     in array char1 with the array char2, and puts the result into char3

      implicit real*8 (a-h,o-z)
      character*80 char1, char2, char3
 
      call findquote (char1, ilo, ihi)
      num1 = ihi - ilo - 1
      call countline (char2, num2)
      char3(1:ilo) = char1(1:ilo)
      char3(ilo+1:ilo+num2) = char2(1:num2)
      char3(ilo+num2+1:ilo+num2+num1) = char1(ilo+1:ilo+num1)
      char3(ilo+num2+num1+1:ilo+num2+num1+1) = 1h"
      do i=ilo+num2+num1+2,80
         char3(i:i) = ' '
      enddo
      
    
      return
      end

 
