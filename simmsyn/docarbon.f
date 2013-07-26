
c***********************************************************************
      subroutine docarbon
c***********************************************************************
c     form a parameter file for a carbon synthesis grid, 
c     and execute the syntheses.

      include 'quants.com'
      include 'characts.com'

      real*8 wavelo, wavehi, ylo, yhi, 
     .       vshift, wavshift, obsadd, obsmult, 
     .       fwhmgauss, vsini, limbdark, vmac, fwhmloren
      character*80 templine, batchfile
      character*1 chars(80)
      character*1 smtype
      equivalence (chars,templine)

c*****on rare occasions the CH synthesis might not be desired
      if (cnoflag(1:1) .ne. 'C') return

c*****open the file with preset parfile commands common to all syntheses;
c     directory you're running it from
      line = '/batchch.par'
      call meld (parfpath, line, templine, itot)
      open (2,file=templine)

c*****open a new file for assembling all the batch synthesis commands
      write (templine,1010)
      templine(1:6) = '.apar2'
      call meld (starname, templine, batchfile, ifile)
      open (5,file=batchfile)

1     write (line,1010)
      read (2,1002,end=10) line
      if     (line(1:8) .eq. 'RUN     ') then
         read (line,*) templine, irun
         write (5,1002) line
      elseif (line(1:8) .eq. 'speccomp') then
         call findquote (line, ilo, ihi)
         templine = line(ilo+1:ihi-1)
         call meld (starname, templine, chfile, ifile)
         call substitute (line, starname, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'hardpost') then
         call substitute (line, starname, templine)
         write (5,1002) templine
         call findquote (templine, ilo, ihi)
         write (99,1001) (chars(j),j=ilo+1,ihi-1)
      elseif (line(1:8) .eq. 'lines_in') then
         call substitute (line, linespath, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'strongli') then
         call substitute (line, linespath, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'model_in') then
         call substitute (line, starname, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'observed') then
         call substitute (line, starname, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'abundanc') then
         if (elnum(8) .eq. 0) then
            write (5,1012)
         else
            write (5,1002) line
         endif
      elseif (line(1:8) .eq. 'plotpars') then
         write (5,1002) line
         read (2,1002) line
         read (line,*) wavelo, wavehi, ylo, yhi
         write (5,1003) wavelo, wavehi, ylo, yhi
         read (2,1002) line
         read (line,*) vshift, wavshift, obsadd, obsmult
         vshift = -vrad
         write (5,1003) vshift, wavshift, obsadd, obsmult
         read (2,1002) line
         read (line,*) smtype, fwhmgauss, vsini, limbdark, vmac, 
     .                 fwhmloren
         fwhmgauss = vbluesmooth/300000*(wavelo+wavehi)/2.
         write (5,1004) smtype, fwhmgauss, vsini, limbdark, vmac, 
     .                 fwhmloren
      else
         write (5,1002) line 
      endif
      go to 1

10    close (unit=5)

c*****Copy batchfile to batch.par, which is what MOOG expects you to
c     feed it; then invoke MOOG to do the syntheses
      if (ifile .lt. 10) then
         write (templine,1008) ifile
      else
         write (templine,1009) ifile
      endif
      write (line,templine) batchfile(1:ifile)
      call system (line)
      call system (moogpath)

c     record in the log, clean up, and exit
      write (99,*) 'did CH syntheses, made 2 plot files and summary: ',
     .             chfile(1:ifile)
      call system ('rm -f batch.par')
      return

1001     format (' wanting to make a carbon synthesis file: ',60a1)
1002  format (a80)
1003     format (6f9.3)
1004     format (7x, a1, 5f9.3)
1008  format ('(3hcp ,a', i1, ', 10h batch.par', '$)')
1009  format ('(3hcp ,a', i2, ', 10h batch.par', '$)')
1010  format (80(' '))
1012        format ('abundances     2     5'/
     .              '8       0.4   0.4   0.4   0.4   0.4')
      end



