
c***********************************************************************
      subroutine fillpar (wave,numsyn)
c***********************************************************************
c     for each desired synthesis, fill in the overall parameter file
c     with the appropriate information.

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      real*8 wavelo, wavehi, ylo, yhi,
     .       vshift, wavshift, obsadd, obsmult,
     .       fwhmgauss, vsini, limbdark, vmac, fwhmloren
      character*80 templine, synfile
      character*1 smtype

      iwave = int(wave+0.0001)
      write (synfile,1003) iwave
      call meld (parfpath, synfile, templine, itot)
      open (31,file=templine(1:itot),status='old',err=50)
15    read (31,1002,end=20) line
      if          (line(1:8) .eq. 'RUN     ') then
         write (5,1004) numsyn
      elseif (line(1:8) .eq. 'hardpost') then
         call substitute (line, starname, templine)
         write (5,1002) templine
         call findquote (templine, ilo, ihi)
         line(1:40) = 'wanting to make another synthesis file: '
         itot = ihi - ilo -1
         line(40+1:40+itot) = templine(ilo+1:ihi-1)
         write (99,*) line(1:40+itot)
      elseif (line(1:8) .eq. 'lines_in') then
         call substitute (line, linespath, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'strongli') then
         call substitute (line, linespath, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'observed') then
         call substitute (line, starname, templine)
        write (5,1002) templine
      elseif (line(1:8) .eq. 'plotpars') then
         write (5,1002) line
         read (31,1002) line
         read (line,*) wavelo, wavehi, ylo, yhi
         write (5,1005) wavelo, wavehi, ylo, yhi
         read (31,1002) line
         read (line,*) vshift, wavshift, obsadd, obsmult
         vshift = -vrad
         write (5,1005) vshift, wavshift, obsadd, obsmult
         read (31,1002) line
         read (line,*) smtype, fwhmgauss, vsini, limbdark, vmac,
     .                 fwhmloren
         if (wave .lt. wavebluered) then
            fwhmgauss = vbluesmooth/300000*(wavelo+wavehi)/2.
         else
            fwhmgauss = vredsmooth/300000*(wavelo+wavehi)/2.
         endif
         write (5,1006) smtype, fwhmgauss, vsini, limbdark, vmac,
     .                 fwhmloren
      else
         write (5,1002) line
      endif
      go to 15

c*****filling parameter file for this wavelength finished; exit normally
20    close (unit=31)
      return

c*****if there is a problem opening a parfile, record a message in the
c     log file and exit with a negative wavelength as a flag
50    write (99,1007) synfile(2:12)
      wave = -1.0
      return


1002  format (a80)
c1003  format ('/syn', i4, '.par', 69(' '))
1003  format ('/syn', i4, '.par', 69X)
1004  format ('RUN           ', i2)
1005  format (6f9.3)
1006  format (7x, a1, 5f9.3)
1007  format (' PROBLEM WITH PARFILE, SKIPPING IT: ', a11)


      end



