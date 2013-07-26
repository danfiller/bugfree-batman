
c***********************************************************************
      subroutine startup
c***********************************************************************
c     this routine finds out what the current directory name is, and
c     uses it as prefix for the log file that is created and opened

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      character*80 templine
      character*1  chars(80)
      equivalence  (chars,line)

      starname = '                              '

c*****discover the directory name
      call system ('pwd > whereami')
      open (97,file='whereami')
      read (97,1001) line
1001  format (a80)
      close (unit=97)
      call system ('rm -f whereami')
      call countline (line, nchars)
      if (line(nchars:nchars) .eq. '/') then
         line(nchars:nchars) = ' '
         nchars = nchars - 1
      endif
      do i=nchars,1,-1
         if (line(i:i) .eq. '/') then
            iname = nchars - (i - 1)
            starname(1:iname) = line(i+1:nchars)
            do j=iname+1,80
               starname(j:j) = ' '
            enddo
            go to 1
         endif
      enddo
      stop 'SOMETHING WRONG WITH DIRECTORY NAME'
c*****clean out old abundance runs from the directory
1     call system ('rm -rf *.p* *.m* *.a*')
c*****create and open the log file
      templine = '.alogf'
      call meld (starname, templine, logfile, ifile)
      open (99,file=logfile(1:ifile))

      return
      end



