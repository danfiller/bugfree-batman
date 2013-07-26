
c***********************************************************************
      subroutine abundplot
c***********************************************************************
c     this routine makes a summary abundance plot

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      real*4 xmin, xmax, ymin, ymax, xpoint, ypoint,
     .       smlxtic, bigxtic, smlytic, bigytic,
     .       xpos(100), ypos(100), zpos(100), err(100), erradopt(100),
     .       style(100), pstyle(2)
      data (pstyle(i),i=1,2) / 243.5, 240.4 /
      integer sm_device
      character*1 chars(80), namechars(80)
      equivalence (chars,line)
      equivalence (namechars,starname)
      character*60 shortline
      character*7 color
      character*5 namespec
      character*5 abtype
      write(6,*) 'I made to to abundplot'

c*****compute the relative [X/Fe] values
      do i=1,95
         do j=1,2
            if (i.eq.3 .and. j.eq.1) then
               if (specnum(i,j) .gt. 0) then
                  xfe1(i,j) = specab(i,j) - 2.2
                  xfe2(i,j) = specab(i,j) - 2.2
               else
                  xfe1(i,j) = 99.99
                  xfe2(i,j) = 99.99
               endif
            else
               if (specnum(i,j) .gt. 0) then
                  xfe1(i,j) = specab(i,j) - xsolar(i) - metallicity
                  if (xfe122563(i) .lt. 99.) then
                     xfe2(i,j) = xfe1(i,j) - xfe122563(i)
                  else
                     xfe2(i,j) = 99.99
                  endif
               endif
            endif
         enddo
      enddo

c*****open up the plot file and give opening incantations for plotting
      line = '.px2fe'
      call meld (starname, line, abpsfile, ifile)
      if (ifile .lt. 10) then
         write (shortline,1003) ifile
      else
         write (shortline,1004) ifile
      endif
1003  format ('(13hpostlandfile ,a',i1,'$)')
1004  format ('(13hpostlandfile ,a',i2,'$)')
      write (line,shortline) abpsfile(1:ifile)
      if (sm_device(line(1:13+ifile)) .lt. 0) then
         write (*,1005) shortline(1:ifile+13)
1005     format ('FILE OPENING ERROR:',a60)
         stop
      endif
      call sm_graphics
      call sm_erase
      call sm_location (5500,30500,4500,30500)
      call sm_defvar ('y_gutter', '0.10')

c*****prepare the plot with box, labels, etc.
      call sm_expand (1.0)
      call sm_window (1, 1, 1, 1, 1, 1)
      xmin = 0.
      xmax = 95.
      ymin = -1.6
      ymax = +1.6
      call sm_limits (xmin, xmax, ymin, ymax)
      smlxtic = 2.
      bigxtic = 10.
      smlytic = 0.1
      bigytic = 0.5
      call sm_ticksize (smlxtic, bigxtic, smlytic, bigytic)
      call sm_expand (1.001)
c      call sm_format ('i2')
      call sm_lweight (4.0)
      call sm_ctype ('black')
      call sm_box (1, 2, 0, 0)
      call sm_expand (1.1)
      call sm_ylabel ('[X/Fe] (Sun)')
      call sm_xlabel ('Atomic Number')
      call sm_ltype (1)
      ypoint = 0.
      call sm_relocate (xmin, ypoint)
      call sm_draw (xmax, ypoint)
      call sm_ltype (0)
      call sm_expand (1.01)
      xpoint = (xmax-xmin)/2.
      ypoint = -1.15
      call sm_relocate (xpoint, ypoint)
      call sm_putlabel (5, starname(1:iname))
      ypoint = real(-1.30)
      call sm_relocate (xpoint, ypoint)
      write (shortline,1006) int(teff+0.001), logg, fehadopt, vt
1006  format ('T ', i4, ' g ', f4.2, ' M/H ', f5.2,
     .        ' v_t ', f4.2)
      call sm_putlabel (5, shortline(1:32))
      xpoint = (xmax-xmin)/2.
      ypoint = 1.75
      call sm_relocate (xpoint, ypoint)
      write (shortline,1001)
1001  format ('Li offset is w.r.t. Spite plateau value of +2.2')
      call sm_putlabel (5, shortline(1:47))

c*****gather the abundances into plot-ready arrays
      style1 = 243.5
      style2 = 240.4
      do m=1,3
         nplot = 0
         if     (m .eq. 1) then
            abtype = 'l'
            color = 'blue   '
            xpoint = (xmax-xmin)/8.
            ypoint = -1.50
            call sm_ctype (color)
            call sm_expand (1.01)
            call sm_relocate (xpoint, ypoint)
            shortline(1:3) = 'EWs'
            call sm_putlabel (5, shortline(1:3))
         elseif (m .eq. 2) then
            abtype = 's'
            color = 'red    '
            xpoint = (xmax-xmin)/3.
            ypoint = -1.50
            call sm_ctype (color)
            call sm_expand (1.01)
            call sm_relocate (xpoint, ypoint)
            shortline(1:12) = 'syn. spectra'
            call sm_putlabel (5, shortline(1:12))
         else
            abtype = 'u'
            color = 'green  '
            xpoint = (xmax-xmin)/1.5
            ypoint = -1.50
            call sm_ctype (color)
            call sm_expand (1.01)
            call sm_relocate (xpoint, ypoint)
            shortline(1:10) = 'upper lim.'
            call sm_putlabel (5, shortline(1:10))
         endif
         do i=1,95
            do j=1,2
               if (specnum(i,j) .gt. 0 .and. 
     .             spectype(i,j) .eq. abtype) then
                  nplot = nplot + 1
                  style(nplot) = pstyle(j)
                  xpos(nplot) = real(i)
                  if     (xfe1(i,j) .ge. +1.6) then
                     ypos(nplot) = +1.59
                  elseif (xfe1(i,j) .le. -1.6) then
                     ypos(nplot) = -1.59
                  else
                     ypos(nplot) = real(xfe1(i,j))
                  endif
                  if     (xfe2(i,j) .ge. +99.) then
                     zpos(nplot) = 99.99
                  elseif (xfe2(i,j) .ge. +1.6) then
                     zpos(nplot) = +1.59
                  elseif (xfe2(i,j) .le. -1.6) then
                     zpos(nplot) = -1.59
                  else
                     zpos(nplot) = real(xfe2(i,j))
                  endif
                  err(nplot) = real(specdev(i,j))
                  erradopt(nplot) = real(specdevadopt(i,j))
               endif
            enddo
         enddo

c*****plot the abundances
         call sm_ptype (style,nplot)
         call sm_expand (3.0)
         call sm_points (xpos, ypos, nplot)
         call sm_expand (1.5)
         call sm_errorbar (xpos, ypos, erradopt, 2, nplot)
         call sm_errorbar (xpos, ypos, erradopt, 4, nplot)
      enddo

c*****finish the plot, close the plot file
100   call sm_gflush
      call sm_alpha
      call sm_hardcopy
      line(1:ifile) = abpsfile(1:ifile)
      write (99,1002) (chars(i),i=1,ifile)
1002  format (' created summary abundance plot: ',60a1)
 
c*****write a summary table in the log
      write (99,1008)
1008  format ('species   log eps     sigma  erradopt',
     .        '     [Sun]    #     type')
      do i=1,95
         do j=1,2
            if (specnum(i,j) .gt. 0) then
               if (i .eq. 6) then
                  namespec = 'CH   '
               else
                  namespec(1:2) = names(i)
                  if (j .eq. 2) then
                     namespec(3:5) = ' II'
                  else
                     namespec(3:5) = ' I '
                  endif
               endif
               write (99,1007) namespec, specab(i,j), specdev(i,j), 
     .                         specdevadopt(i,j), xfe1(i,j), 
     .                         specnum(i,j), spectype(i,j)
1007           format (a7, 4f10.2, i5, 7x, a1)
            endif
         enddo
      enddo


c*****maybe add in a long line with all results, suitable for spreadsheets
c      write (templine(1:80),1012)
c1012  format (80(' '))
c      templine(1:4) = '.sum'
c      call meld (starname, templine(1:80), sumfile, ifile)
c      open (22,file=sumfile(1:ifile))
c      write (22,1011) (namechars(i),i=1,40),
c     .                nametab(1), abtab(1), ablilim, 
c     .                devtab(1), devadoptab(1), xfe1tab(1), xfe2tab(1), 
c     .                (nametab(m), abtab(m), devtab(m), devadoptab(m),
c     .                xfe1tab(m), xfe2tab(m), m=2,20)
c1011  format (40a1, a6, 6f6.2, 100(a6, 5f6.2) )
c      close (unit=22)

      return
      end



