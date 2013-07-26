
c***********************************************************************
      subroutine readspec
c***********************************************************************
c     read in the long observed spectrum

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'spectr.com'
      include 'characts.com'

      character*80 templine
     

c*****open the observed spectrum file
      templine = '.spect'
      call meld (starname, templine, obsspecfile, ifile)
      write(*,*) obsspecfile
      pause
      open (21,file=obsspecfile(1:ifile))
      read (21,1001) templine
1001  format (a80)
     
c*****read the points
      do i=1,100000
         read (21,*,end=10) waveobs(i), fluxobs(i)
      enddo

c*****write results in log file and end
10    nspec = i-1
      write (99,1002) templine, nspec
1002  format (' read observed spectrum file: '/' ', a79/
     .        ' number of observed points = ', i6)
      return
      end


