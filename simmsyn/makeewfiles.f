
c***********************************************************************
      subroutine makeewfiles
c***********************************************************************
c     this routine takes the received model guess and EW file, captures 
c     the model parameters, and produces three EW files:  
c     *.aewl = EWs for lines used for standard abundance analysis
c     *.aews = EWs for lines that will be synthesized later
c     *.aewu = 3-sigma upper limit EWs of undetected lines

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      real*8 wave, id, chi, loggf, ewsn, widthline
      character*80 templine
      character*1 chars(80)
      equivalence (chars,line)


      write (starfile,1006)
      write (ewfilel,1006)
      write (ewfiles,1006)
      write (ewfileu,1006)
1006  format (80(' '))

c*****open the input file and create/open the EW files
      templine = '.input'
      call meld (starname, templine, starfile, ifile)
      open (1,file=starfile(1:ifile))
      templine = '.aewl1'
      call meld (starname, templine, ewfilel, ifile)
      open (2,file=ewfilel(1:ifile))
      templine = '.aews1'
      call meld (starname, templine, ewfiles, ifile)
      open (3,file=ewfiles(1:ifile))
      templine = '.aewu1'
      call meld (starname, templine, ewfileu, ifile)
      open (4,file=ewfileu(1:ifile))

c*****read the model atmosphere info from the original input file
      read (1,1001) line
1001  format (a80)
      read (line(iname+1:80),*) teff, logg, feh, vt, gridtype, cnoflag,
     .                          vrad, vfwhmblueobs, vfwhmredobs, 
     .                          wavebluered
      if (feh .lt. -4.0) then
         write (99,1003) feh
1003     format (' WARNING: requested [M/H], ', f6.2, 
     .           ' is below grid limit; using -4.0')
         fehadopt = -4.0
      else
         fehadopt = feh
      endif

c*****compute velocity smoothing FWHM from observed total FWHM minus
c*****"intrinsic" line width (thermal+microturbulent)
      vintrinsic = dsqrt(1.6631d-2*teff/56. + vt**2)
      vbluesmooth = vfwhmblueobs - vintrinsic
      vredsmooth = vfwhmredobs - vintrinsic

c*****write out the model atmosphere stuff and date to the EW files
      call system('date > tempfile')
      open (97,file='tempfile')
      read (97,1001) line
      close (unit=97)
      call system ('rm -f tempfile')
      write (2,1012) starname(1:30), line(5:10), line(24:29)
1012  format (a25, 2x, 2a6, '   EWs of ordinary lines')
      write (3,1013) starname(1:30), line(5:10), line(24:29)
1013  format (a25, 2x, 2a6, '   EWs of lines to be synthesized')
      write (4,1014) starname(1:30), line(5:10), line(24:29)
1014  format (a25, 2x, 2a6, '   3-sigma EW upper limits')
     
c*****read the line data from original input file, write to EW files;
15    read (1,*,end=20) wave, id, chi, loggf, ewsn, linetype
      if     (linetype .eq. 'l') then 
         write (2,1004) wave, id, chi, loggf, ewsn
      elseif (linetype .eq. 's') then
         write (3,1004) wave, id, chi, loggf, ewsn
      elseif (linetype .eq. 'u') then
         if (wave .lt. wavebluered) then
            widthline = wave*vfwhmblueobs/3.d5
         else
            widthline = wave*vfwhmredobs/3.d5
         endif
         ew3sig = 3.*1000.*2.*widthline/ewsn
         write (4,1007) wave, id, chi, loggf, ew3sig, int(ewsn+0.0001)
      else
         write (*,*) 'SOMETHING WRONG WITH LINE TYPE; I QUIT!'
      endif
1004  format (f10.3, f10.1, 2f10.3, 20x, f10.1)
1007  format (f10.3, f10.1, 2f10.3, 20x, f10.1, '  S/N=',i3)
      go to 15

20    write (99,*) 'made EW ordinary line file: ', ewfilel(1:ifile)
      write (99,*) 'made EW synthesized line file: ', ewfiles(1:ifile)
      write (99,*) 'made EW upper limit file: ', ewfileu(1:ifile)
      write (99,1005) vfwhmblueobs, vbluesmooth,
     .                vfwhmredobs, vredsmooth, wavebluered
1005  format (' total FWHM, blue chip: ', f6.2, 
     .        ' km/s;   or smoothing FWHM: ', f6.2, ' km/s'/
     .        ' total FWHM, red: FWHM = ',  f6.2, 
     .        ' km/s;   or smoothing FWHM: ', f6.2, ' km/s'/
     .        ' blue/red crossover wavelength: ', f7.1, ' A')
      close (2)
      close (3)
      close (4)
      return

      end



