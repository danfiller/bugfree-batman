
c***********************************************************************
      subroutine countline (string, numchar)
c***********************************************************************
c     this routine figures out how many characters a line of text has;
c     the string is presumed to have a max of 80 characters

      implicit real*8 (a-h,o-z)
      integer numchar
      character*80 string

      do i=80,1,-1
         if (string(i:i) .ne. ' ') then
            numchar = i
            return
         endif
      enddo
      write (*,*) 'empty string, I quit!'
      stop

      end

