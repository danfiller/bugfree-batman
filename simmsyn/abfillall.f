
c***********************************************************************
      subroutine abfillall
c***********************************************************************
c     this routine puts mean derived abundances (output from abfind) 
c     into the model atmosphere file, and stores individual line
c     abundances and the means in arrays

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      real*8 elstack(50), abstack(50), abupper(50)
      character*1 chars(80)
      equivalence (chars,line)
      character*80 templine
      character*2 nameel, nameion
       
c*****clear the abundance arrays
      do i=1,95
         elnum (i)     = 0
         elab(i)       = 0.
         eldev(i)      = 0.
         eldevadopt(i) = 0.
         do j=1,2
            specnum(i,j)      = 0
            specab(i,j)       = 0.
            specdev(i,j)      = 0.
            specdevadopt(i,j) = 0.
            spectype(i,j)     = ' '
            epslope(i,j)      = 0.
            epintercept(i,j)  = 99.99
            rwslope(i,j)      = 0.
            rwintercept(i,j)  = 99.99
         enddo
      enddo

c*****open old model file and new model file
      templine = '.mod02'
      call meld (starname, templine, modfile2, ifile)
      open (11,file=modfile1(1:ifile))
      open (12,file=modfile2(1:ifile))

c*****first deal with the abundances that are derived purely from
c     EWs (file *.aln0l); open the appropriate abfind output file
      open (13,file=lnfilel(1:ifile))

c*****read the correct v_t from the abfind output; write it in the
c     new model file
1     read (13,1002) line
1002  format (a80)
      if (line(55:57) .ne. 'vt=') go to 1
      vtchars = line(58:62)
5     read (11,1002,end=100) line
      if (line(1:5).eq.'     ' .and. line(12:13).eq.'05') go to 10
      write (12,1002) line
      go to 5
10    line(5:9) = vtchars
      write (12,1002) line

c*****read individual line and mean abundance information for each species 
c     from the abfind output, and store that stuff in various arrays;
c     the line abundance info will be used in a plot
      nline = 0
15    read (13,1002,end=30) line
      if     (line(1:5) .eq. 'Abund') then
         if (line(32:33) .eq. 'CH') then
            nameel = 'C '
            nameion = 'I '
         else
            read (line(31:35),1001) nameel, nameion
         endif
1001     format (a2, 1x, a2) 
         call nametrans (nameel, nameion, nel, nion, atom1)
      elseif (line(1:5) .eq. 'wavel') then
20       read (13,1002) line
         if (line(1:5) .ne. 'avera') then
            nline = nline + 1
            lineid10(nline) = 10*nel + nion
            read (line,*) linewave(nline), lineep(nline), gf, ew, 
     .                    linerw(nline), lineab(nline)
            go to 20
         else
            read (line,1003) specab(nel,nion), specdev(nel,nion), 
     .                       specnum(nel,nion)
1003        format (20x, f5.2, 23x, f4.2, 14x, i3)
            spectype(nel,nion) = 'l'
         endif
      elseif (line(1:5) .eq. 'E.P. ') then
         read (line(28:55),1011) epslope(nel,nion), 
     .                           epintercept(nel,nion)
1011     format (f7.3, 14x, f7.3)
      elseif (line(1:5) .eq. 'R.W. ') then
         read (line(28:55),1011) rwslope(nel,nion), 
     .                           rwintercept(nel,nion)
      else
         go to 15
      endif
      go to 15

c*****then pull out the mean abundances that are derived from 
c     EWs of lines to be synthesized later (file *.alnxs)
30    close (unit=13)
      open (14,file=lnfiles(1:ifile))
35    read (14,1002,end=50) line
      if     (line(1:5) .eq. 'Abund') then
         if (line(32:33) .eq. 'CH') then
            nameel = 'C '
            nameion = 'I '
         else
            read (line(31:35),1001) nameel, nameion
         endif
         call nametrans (nameel, nameion, nel, nion, atom1)
      elseif (line(1:5) .eq. 'wavel') then
40       read (14,1002) line
         if (line(1:5) .ne. 'avera') then
            go to 40
         else
            read (line,1003) specab(nel,nion), specdev(nel,nion),
     .                       specnum(nel,nion)
            spectype(nel,nion) = 's'
         endif
      else
         go to 35
      endif
      go to 35

c*****then find the SMALLEST upper limit for species that have only
c     upper limit abundances (file *.alnxu)
50    close (unit=14)
      open (15,file=lnfileu(1:ifile))
55    nupper = 0
      read (15,1002,end=70) line
      if     (line(1:5) .eq. 'Abund') then
         if (line(32:33) .eq. 'CH') then
            nameel = 'C '
            nameion = 'I '
         else
            read (line(31:35),1001) nameel, nameion
         endif
         call nametrans (nameel, nameion, nel, nion, atom1)
      elseif (line(1:5) .eq. 'wavel') then
60       read (15,1002) line
         if (line(1:5) .ne. 'avera') then
            nupper = nupper + 1
            read (line,*) wave, ep, gf, ew, rw, abupper(nupper)
            go to 60
         else
            spectype(nel,nion) = 'u'
            specnum(nel,nion) = nupper
            if (nupper .eq. 1) then
               specab(nel,nion) = abupper(1)
            else
               ablow = 9999.
               do n=1,nupper
                  ablow = dmin1(abupper(n),ablow)
               enddo
               specab(nel,nion) = ablow
            endif
         endif
      else
         go to 55
      endif
      go to 55

c*****adjust the species sigma values to reflect reality: 
c     for 1 line,   devadopt = max(devavg,0.25)
c     for 2 lines,  devadopt = max(devavg,0.15)
c     for >2 lines, devadopt = max(devavg,0.10)
70    close (unit=15)
      do i=1,95
         do j=1,2
            if     (specnum(i,j) .eq. 1) then
               specdevadopt(i,j) = 0.25
            elseif (specnum(i,j) .eq. 2) then
               specdevadopt(i,j) = dmax1(dble(0.15),specdev(i,j))
            else
               specdevadopt(i,j) = dmax1(dble(0.10),specdev(i,j))
            endif
         enddo
      enddo

c*****search through the species mean abundance data; form elemental
c     mean abundances (straight averages of neutral and ion abundances);
c     and compute a metallicity in [] notation
      nelem = 0
      do i=1,95
         if     (specnum(i,1) .gt. 0) then
            if (specnum(i,2) .gt. 0) then
               elab(i) = (specab(i,1)+specab(i,2))/2.
            else
               elab(i) = specab(i,1)
            endif
            elnum(i) = 1
            nelem = nelem + 1
            elstack(nelem) = dble(i)
            abstack(nelem) = elab(i)
         elseif (specnum(i,2) .gt. 0) then
            elab(i) = specab(i,2)
            elnum(i) = 1
            nelem = nelem + 1
            elstack(nelem) = dble(i)
            abstack(nelem) = elab(i)
         endif
      enddo
      abti = elab(22)
      if     (specnum(26,1) .gt. 0) then
         abfe = specab(26,1)
      else
         abfe = specab(26,2)
      endif
      metallicity = abfe - xsolar(26)
      abc = 8.50 + metallicity

c*****write the elemental means into the new model file
      write (12,1004) nelem, metallicity,
     .                (elstack(n)+0.001, abstack(n), n=1,nelem)
1004  format ('natoms       ', i2, f9.2/(4(f10.1,f10.2)))


c*****finish the new model file
      write (12,1007)
1007  format ('nmol         12'/ 6x, '101.', 6x, '101.', 6x, '106.',
     .        6x, '107.', 6x, '108.', 6x, '606.', 6x, '607.',
     .        6x, '608.'/ 6x, '707.', 6x, '708.', 6x, '808.',
     .        4x, '10108.', 4x, '60808.')

c*****write the elemental means into the log file
      write (99,1005) metallicity, nelem
1005  format (' FeI+FeII metallicity = ', f6.2, 
     .        '    individual abundances for ', i2, ' elements')
      write (99,1006) (names(int(elstack(n)+0.001)), 
     .                abstack(n), n=1,nelem)
1006  format (4(a5,f8.2))

100   close (unit=11)
      close (unit=12)

      line(1:ifile) = modfile2(1:ifile)
      write (99,1010) abc, (chars(i),i=1,ifile)
1010  format (' put EW abundances and C guess (', f4.2, 
     .        ') in new model file: ', 80a1)
      return
      end



