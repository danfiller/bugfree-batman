
c***********************************************************************
      subroutine lineplot
c***********************************************************************
c     this routine plots individual Fe and Ti line abundances versus
c     E.P. and log(R.W.)

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      integer ion10(4)
      data (ion10(i),i=1,4) / 221, 222, 261, 262 /
      real*4 xline(500), yline(500), zline(500)
      real*4 xmin, xmax, ymin, ymax, xpos, ypos, style(1) 
      real*4 ptypeneed(4)
      data (ptypeneed(i),i=1,4) / 40.6,  30.8, 243.7,  41.9 /
      integer sm_device
      character*60 shortline
      character*3 plotname(4)
      data (plotname(i),i=1,4) / 'Ti1', 'Ti2', 'Fe1', 'Fe2' /
      character*1 chars(80)
      equivalence (chars,lnpsfile)

c*****create and open the line plot postscript file
      line = '.plnxx'
      if (niter .lt. 10) then
         write (line(5:6),1020) niter
1020     format ('0', i1)
      else
         write (line(5:6),1021) niter
1021     format (i2)
      endif
      call meld (starname, line, lnpsfile, ifile)
      if (ifile .lt. 10) then
         write (shortline,1003) ifile
      else
         write (shortline,1004) ifile
      endif
1003  format ('(13hpostlandfile ,a',i1,'$)')
1004  format ('(13hpostlandfile ,a',i2,'$)')
      write (line,shortline) lnpsfile(1:ifile)
      if (sm_device(line(1:13+ifile)) .lt. 0) then
         write (*,1005) shortline(1:ifile+13)
1005     format ('FILE OPENING ERROR:',a60)
         stop
      endif

c*****set up, label, etc., the upper plot window for trends with RW
      call sm_graphics
      call sm_erase
      call sm_location (5500,30500,4500,30500)
      call sm_defvar ('y_gutter', '0.70')
      call sm_expand (1.0)
      call sm_window (1, 2, 1, 2, 1, 2)
      xmin = -6.2
      xmax = -4.2
      ymin = -0.5
      ymax = +0.5
      call sm_limits (xmin, xmax, ymin, ymax)
      call sm_lweight (4.0)
      call sm_ctype ('black')
      call sm_expand (1.01)
      call sm_box (1, 2, 0, 0)
      call sm_xlabel ('log RW')
      call sm_ylabel ('\\gD log(\\ge)')
      call sm_ltype (1)
      xpos = xmin
      ypos = 0.
      call sm_relocate (xpos, ypos)
      xpos = xmax
      call sm_draw (xpos, ypos)
      call sm_ltype (0)

c*****write the star name and the assumed stellar parameters in the box
      xpos = -5.20
      ypos = +0.40
      call sm_relocate (xpos, ypos)
      call sm_putlabel (5, starname(1:iname))
      write (shortline,1001) int(teff+0.001), logg, fehadopt, vt
1001  format ('T ', i4, ' g ', f4.2, ' M/H ', f5.2, 
     .        ' v_t ', f4.2)
      ypos = -0.40
      call sm_relocate (xpos, ypos)
      call sm_expand (0.85)
      call sm_putlabel (5, shortline(1:32))

c*****draw the mean trend of Fe I abundance with RW
      call sm_ctype ('green')
      xpos = xmin
      ypos = rwintercept(26,1) + xmin*rwslope(26,1) - abfe
      call sm_relocate (xpos, ypos)
      xpos = xmax
      ypos = rwintercept(26,1) + xmax*rwslope(26,1) - abfe
      call sm_draw (xpos, ypos)

      xpos = -6.25
      ypos = +0.55
      call sm_relocate (xpos, ypos)
      call sm_expand (0.80)
      call sm_ctype ('black')
      call sm_putlabel (6,'(mean,sigma,#)')

c*****find and plot the individual line abundances for Fe I,II, and Ti I,II
      do i=1,4
         nlo = 0
         nhi = 0
         if (ion10(i)/10 .eq. 22) then
            call sm_ctype ('red')
         else
            call sm_ctype ('blue')
         endif

         do j=1,nline
            if (lineid10(j) .eq. ion10(i)) then
               nlo = j
               do k=nlo+1,nline
                  if (lineid10(k) .ne. ion10(i)) then
                     nhi = k - 1
                     go to 10
                  endif
               enddo
               nhi = nline
               go to 10
            endif
         enddo

10       if (nlo .ne. 0) then
            nplot = 0
            do j=nlo,nhi
               nplot = nplot + 1
               yline(nplot) = real(linerw(j))
               if (ion10(i)/10 .eq. 22) then
                  zline(nplot) = real(lineab(j)-abti)
               else
                  zline(nplot) = real(lineab(j)-abfe)
               endif
            enddo
            style(1) = ptypeneed(i)
            call sm_expand (2.0)
            call sm_ptype (style,1)
            call sm_points (yline, zline, nplot)
            ii = ion10(i)/10
            jj = ion10(i) - 10*ii
            write (shortline,1010) plotname(i), specab(ii,jj), 
     .                             specdev(ii,jj), specnum(ii,jj)
1010        format (a3, '(', f5.2, ',', f4.2, ',', i3, ')')
            call sm_expand (0.80)
            xpos = -6.30 + 0.45*i
            ypos = +0.55
            call sm_relocate (xpos, ypos)
            call sm_putlabel (6,shortline(1:19))
         endif
      enddo

c*****set up, label, etc., the lower plot window for trends with EP
      call sm_expand (1.0)
      call sm_window (1, 2, 1, 1, 1, 1)
      xmin =  -0.05
      xmax =  5.00
      ymin = -0.5
      ymax = +0.5
      call sm_limits (xmin, xmax, ymin, ymax)
      call sm_lweight (4.0)
      call sm_ctype ('black')
      call sm_expand (1.01)
      call sm_box (1, 2, 0, 0)
      call sm_xlabel ('E.P.')
      call sm_ylabel ('\\gD log(\\ge)')
      call sm_ltype (1)
      xpos = xmin
      ypos = 0.
      call sm_relocate (xpos, ypos)
      xpos = xmax
      call sm_draw (xpos, ypos)
      call sm_ltype (0)

c*****draw the mean trend of Fe I abundance with EP
      call sm_ctype ('green')
      xpos = xmin
      ypos = epintercept(26,1) + xmin*epslope(26,1) - abfe
      call sm_relocate (xpos, ypos)
      xpos = xmax
      ypos = epintercept(26,1) + xmax*epslope(26,1) - abfe
      call sm_draw (xpos, ypos)

c*****find and plot the individual line abundances for Fe I,II, and Ti I,II
      do i=1,4
         nlo = 0
         nhi = 0
         if (ion10(i)/10 .eq. 22) then
            call sm_ctype ('red')
         else
            call sm_ctype ('blue')
         endif
         do j=1,nline
            if (lineid10(j) .eq. ion10(i)) then
               nlo = j
               do k=nlo+1,nline
                  if (lineid10(k) .ne. ion10(i)) then
                     nhi = k - 1
                     go to 20
                  endif
               enddo
               nhi = nline
               go to 20
            endif
         enddo
20       if (nlo .ne. 0) then
            nplot = 0
            do j=nlo,nhi
               nplot = nplot + 1
               xline(nplot) = real(lineep(j))
               if (ion10(i)/10 .eq. 22) then
                  zline(nplot) = real(lineab(j)-abti)
               else
                  zline(nplot) = real(lineab(j)-abfe)
               endif
            enddo
            style(1) = ptypeneed(i)
            call sm_expand (2.0)
            call sm_ptype (style,1)
            call sm_points (xline, zline, nplot)
         endif
      enddo

c*****add a legend to identify the species symbols
      do i=1,4
         if (ion10(i)/10 .eq. 22) then
            call sm_ctype ('red')
         else
            call sm_ctype ('blue')
         endif   
         call sm_relocate (0.40+real(i-1), -0.45)
         style(1) = ptypeneed(i)
         call sm_expand (2.0)
         call sm_ptype (style,1)
         call sm_dot
         call sm_expand (0.75)
         call sm_relocate (0.55+real(i-1), -0.45)
         call sm_putlabel (6,plotname(i))
         if (nlo .eq. 0) then
            call sm_relocate (0.55+real(i-1), -0.35)
            call sm_expand (0.75)
            call sm_putlabel (5,'(NO LINES)')
            if (i .eq. 1) then
             call sm_ctype ('black')
             call sm_relocate (2.2, -0.25)
             call sm_putlabel 
     .         (5,'--> model has been forced off the grid <--')
            endif
         endif
      enddo

c*****end the plot, close the file, note in the log 
      call sm_gflush
      call sm_alpha
      call sm_hardcopy
      write (99,1006) (chars(i),i=1,ifile)
1006  format (' made a plot of Fe and Ti line abundances: ', 40a1)
      if     (machine .eq. 'pcr') then
         if (ifile .lt. 10) then
            write (shortline,1011) ifile
1011        format ('(14hgv -landscape ,a', i1,
     .              ', 2h &', '$)')
         else
            write (shortline,1012) ifile
1012        format ('(14hgv -landscape ,a', i2,
     .              ', 2h &', '$)')
         endif
      elseif (machine .eq. 'pcl') then
         if (ifile .lt. 10) then
            write (shortline,1013) ifile
1013        format ('(26hgv -orientation=landscape ,a', i1, 
     .              ', 2h &', '$)')
         else
            write (shortline,1014) ifile
1014        format ('(26hgv -orientation=landscape ,a', i2, 
     .              ', 2h &', '$)')
         endif
      elseif (machine .eq. 'mac') then
         if (ifile .lt. 10) then
            write (shortline,1015) ifile
1015        format (
     .         '(37hgv -orientation=landscape -scale=0.5 ,a', i1, 
     .         ', 2h &', '$)')
         else
            write (shortline,1016) ifile
1016        format (
     .         '(37hgv -orientation=landscape -scale=0.5 ,a', i2,
     .         ', 2h &', '$)')
         endif
      endif
c*****comment next to lines to supress atmosphere pop-up plots
c      write (line,shortline) lnpsfile(1:ifile)
c      call system (line)

      return
      end



