

c***********************************************************************
      subroutine nametrans (nameel, nameion, nel, nion)
c***********************************************************************
c     this routine translate element name and Roman numeral ionization
c     state into atomic number and ion number

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      character*2 nameel, nameion

      do i=1,95
         if (nameel .eq. names(i)) then
            nel = i
            if (nameion .eq. 'I ') then
               nion = 1
            else
               nion = 2
            endif
            return
         endif
      enddo
      write (*,1001) nameel, nameion
1001  format ('SOMETHING WRONG WITH ELEMENT OR ION NAME: ', a2, 3x, a2)
      stop

      end




