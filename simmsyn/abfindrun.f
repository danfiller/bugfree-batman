
c***********************************************************************
      subroutine abfindrun
c***********************************************************************
c     this routine sets up the parfile for MOOG abfind; executes it
c     for real lines, for lines that will later by synthesized, and
c     for upper limits to lines.

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      character*80 templine
      
c*****reads batchabfind.par from CODE directory and writes batch1.par to 
c     to the working directory
      line = '/batchabfind.par'
      call meld (parfpath, line, templine, itot)
      open (2,file=templine)
      open (5,file='batch1.par')

1     write (line,1010)
1010  format (80(' '))
      read (2,1002,end=10) line
1002  format (a80)
      if     (line(1:8) .eq. 'summary_') then
         call substitute (line, starname, templine)
         call findquote (templine, ilo, ihi)
         jline = ihi - ilo - 1
         if     (linetype .eq. 'l') then
            if (niter .lt. 10) then
               write (templine(ihi-2:ihi-1),1020) niter
1020           format ('0', i1)
            else
               write (templine(ihi-2:ihi-1),1021) niter
1021           format (i2)
            endif
            lnfilel(1:jline) = templine(ilo+1:ihi-1)
         elseif (linetype .eq. 's') then
            templine(ihi-2:ihi-1) = 's1'
            lnfiles(1:jline) = templine(ilo+1:ihi-1)
         else
            templine(ihi-2:ihi-1) = 'u1'
            lnfileu(1:jline) = templine(ilo+1:ihi-1)
         endif
         write (5,1002) templine
      elseif (line(1:8) .eq. 'model_in') then
         call substitute (line, starname, templine)
         write (5,1002) templine
      elseif (line(1:8) .eq. 'lines_in') then
         call substitute (line, starname, templine)
         call findquote (templine, ilo, ihi)
         if     (linetype .eq. 'l') then
            templine(ihi-2:ihi-1) = 'l1'
         elseif (linetype .eq. 's') then
            templine(ihi-2:ihi-1) = 's1'
         else
            templine(ihi-2:ihi-1) = 'u1'
         endif
         write (5,1002) templine
      else
         write (5,1002) line
      endif
      go to 1

10    close (unit=2)           
      close (unit=5)           

c     Copy batch1.par to batch.par, which is what MOOGSILENT expects you to 
c     feed it; then invoke MOOGSILENT to do the line EW analysis
      call system ('cp batch1.par batch.par')
       
      call system (moogpath)

c     record in the log, clean up, exit
      if     (linetype .eq. 'l') then
         line(1:37) = 'completed ordinary line EW analysis: '
         line(37+1:37+ifile) = lnfilel(1:ifile)
         write (99,*) line(1:37+ifile)
      elseif (linetype .eq. 's') then
         line(1:43) = 'completed EW analysis for synthesis lines: '
         line(43+1:43+ifile) = lnfiles(1:ifile)
      else
         line(1:48) = 'completed EW analysis for 3-sigma upper limits: '
         line(48+1:48+ifile) = lnfiles(1:ifile)
         write (99,*) line(1:48+ifile)
      endif
c      call system ('rm -f batch.par')

      return
      end



