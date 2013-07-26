
c***********************************************************************
      subroutine findquote (char1, ilo, ihi)
c***********************************************************************
c     this routine finds where the quotation marks are in the
c     string char1

      implicit real*8 (a-h,o-z)
      character*80 char1
 
      do i=1,80
         if (char1(i:i) .eq. '"') then
            ilo = i
            do j=ilo+1,80
               if (char1(j:j) .eq. '"') then
                  ihi = j
                  return
               endif
            enddo
            write (*,*) 'NO ENDING QUOTATION MARK, I QUIT!'
            stop
         endif
      enddo
      write (*,*) 'NO STARTING QUOTATION MARK, I QUIT!'
      stop

      end

 
