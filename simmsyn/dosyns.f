
c***********************************************************************
      subroutine dosyns
c***********************************************************************
c     form a parameter file for all other desired syntheses
c     and execute the syntheses.

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      character*80 templine, synfile, batchfile
      write(6,*) 'I made it to dosyns'

c*****open the file with preset parfile commands common to all syntheses;
c     this is in the "parfile" subdirectory
      synfile = '/synstart.par'
      call meld (parfpath, synfile, templine, itot)
      open (31,file=templine(1:itot))

c*****open a new file for assembling all the batch synthesis commands
      write (templine,1001)
      templine(1:6) = '.apar3'
      call meld (starname, templine, batchfile, ifile)
      open (5,file=batchfile)
       

c*****fill the batch synthesis beginning lines with instructions
      do i=1,20
         write (templine,1001)
         write (line,1001)
         read (31,1002,end=10) templine
         if     (templine(1:8) .eq. 'speccomp') then
            call findquote (templine, ilo, ihi)
            line(1:ihi-ilo+1) = templine(ilo+1:ihi-1)
            call meld (starname, line, otherfile, ifile)
            call substitute (templine, starname, line)
            write (5,1002) line
         elseif (templine(1:8) .eq. 'model_in') then
            call substitute (templine, starname, line)
            write (5,1002) line
c        elseif (templine(1:8) .eq. 'observed') then
c           call substitute (templine, starname, line)
c           write (5,1002) line
         else
            write (5,1002) templine
         endif
      enddo
10    close (unit=31)
      
c*****in turn, open the files containing the wavelengths of individual
c     synthesis lines and of upper limit lines
      write(6,*) 'open file with waves of synth lines & ull' 
      j = 1
11    if (j .eq. 1) then
         open (30,file=ewfiles)
      else 
         open (30,file=ewfileu)
      endif
      read (30,1002) line

c*****fill in the batch synthesis file with the line-specific parfile 
c     information (from the "parfile" subdirectory) 
c     FIRST, synthesize two Mg I "b" lines to get accurate 
c     wavelength matches between observed and synthetic spectra
c     SECOND, synthesize the Na D lines with a large wavelength range
c     in order to display the ISM Na D components
c     THIRD, synthesize the O I triplet if desired
c     FOURTH, synthesize the CN violet bandheads if desired
c     AFTER all this, add in other lines to be synthesized
      if (j .eq. 1) then
         numsyn = 1
         wave = 5170
         call fillpar (wave,numsyn)
         numsyn = 2
         wave = 5893
         call fillpar (wave,numsyn)
         if (cnoflag(2:2) .eq. 'N') then
            numsyn = numsyn + 1
            wave = 3883
            call fillpar (wave,numsyn)
         endif
         if (cnoflag(3:3) .eq. 'O') then
            numsyn = numsyn + 1
            wave = 7773
            call fillpar (wave,numsyn)
         endif
         numsyn = numsyn + 1
      endif
12    read (30,*,end=30) wave, species
      if (int(species+0.00001) .eq. 106 .or. 
     .    int(species+0.00001) .eq.   8) go to 12
      call fillpar (wave,numsyn)
      if (wave .gt. 0.) numsyn = numsyn + 1
      go to 12


c*****loop back to get the other line file, or end
30    close (unit=30)
      if (j .eq. 1) then
         j = 2
         go to 11
      endif
      close (unit=5)
      write(6,*) 'did Synth of Mg I etc. ' 

c*****Copy batchfile to batch.par, which is what MOOG expects you to 
c     feed it; then invoke MOOG to do the syntheses
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if (ifile .lt. 10) then
         write(6,*) 'this is a comment wher ifile lt 10'       
         write (templine,1008) ifile
      else
         write(6,*) 'cmnt - ifile gt 10', templine       
         write (templine,1009) ifile
      endif
      write(6,*) 'cmnt - line: ',line
      write(6,*) 'cmnt - templine: ',templine
      write(6,*) 'cmnt - batchfile: ', batchfile
      write(6,*) 'cmnt - : ifile'
      write (line,templine) batchfile(1:ifile)
      write(6,*) 'Going to call moog. ' 
      call system (line)
      call system (moogpath)
      write(6,*) 'apprntly cpd to batch.par?? where is it??' 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c*****record in the log, clean up, and exit
      write (99,*) 'did other syntheses, made plot files and summary: ',
     .             otherfile(1:ifile)
c      call system ('rm -f batch.par')
      return

c*****if there is a problem opening a parfile, record a message in the
c     log file and go on to next line to be synthesized
50    write (99,1007) synfile(2:12)
      go to 12

1001  format (80(' '))
1002  format (a80)
1003  format ('/syn', i4, '.par', 69(' '))
1004  format ('RUN           ', i2)
1005  format (6f9.3)
1006  format (7x, a1, 5f9.3)
1007  format (' PROBLEM WITH PARFILE, SKIPPING IT: ', a11)
1008  format ('(3hcp ,a', i1, ', 10h batch.par', '$)')
1009  format ('(3hcp ,a', i2, ', 10h batch.par', '$)')

      end








