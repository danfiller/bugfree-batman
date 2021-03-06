
c***********************************************************************
      subroutine extract
c***********************************************************************
c     this routine pick out best values of abundances from the synthesis
c     runs
                                                               
      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      real*8 absyn(5), sigmasyn(5) 
      real*8 eltab(100), wavetab(100), abtab(100) 
      real*8 epsyn(100), gfsyn(100)
      integer waveuplim(100), wavesyn(100)
      character*2 el1, ion1, el2
      character*1 chars(80)
      equivalence (chars,line)
      write(6,*) 'I made to to extract'

c*****open the file with synthesis fit statistics; re-open the synthesis
c     and upper limit line files to load those line wavelengths into an array
c     
      call countline (otherfile, ifile)
      open (14,file=otherfile(1:ifile))
      write (*,*) otherfile(1:ifile), ' otherfile'
      write (*,*) ewfileu(1:ifile), ' file'
      open (4,file=ewfileu(1:ifile))
      read (4,1001)
      nup = 0
2     read (4,*,end=3) wave
      nup = nup + 1
      waveuplim(nup) = int(wave+0.00001)
      go to 2
3     open (5,file=ewfiles(1:ifile))
      read (5,1001)
      nsyn = 1
4     read (5,*,end=7) wave, x, epsyn(nsyn), gfsyn(nsyn)
      wavesyn(nsyn) = int(wave+0.00001)
      nsyn = nsyn + 1
      go to 4
7     nsyn = nsyn -1 
      itot = 5

c*****read the statistics of the syntheses from a given feature
c     don't derive best abundances for Mg, Na, CN
      write (99,1003)
1003  format (4x, 'lambda', 5x, 'ID', 8x, 'EP', 5x, 'loggf',
     .        4x, 'logeps', 5x, 'FROM SYNTHESES')
      ntab = 1
1     read (14,1001,end=20) line
1001  format (a80)
      if (line(1:4) .eq. 'RUN ') then
         read (14,1001)
         read (14,1001)
         read (14,*) el1, ion1, wavetab(ntab)
         write (6,*) 'el1->',el1,' ion1->',ion1,' wtab-> ',wavetab(ntab)
         if (el1 .eq. 'Mg' .or.
     .       el1 .eq. 'Na') go to 1
         if (el1 .eq. 'N ') then
            spectype(7,1) = 's'
            go to 1
         endif
         iwave = int(wavetab(ntab)+0.00001)
         do n=1,nup
            if (iwave .eq. waveuplim(n)) go to 1
         enddo
         read (14,1001)
         read (14,1001)
         do i=1,itot
            read (14,1012) el2, absyn(i)
1012        format (a2, f5.2)
            write(6,*) el2, absyn(i)
            write(*,*) 'el1-> ', el1, ' el2-> ',el2, 'end'
            if (el2 .ne. el1) then
               write (*,*) 'SOMETHING WRONG WITH SYNTHESIS SUMMARY'
               stop
            endif
            read (14,1013) sigmasyn(i)
1013        format (7x, f8.5)
            write(6,*) 'I have continued the loop however'
         enddo
      else
         go to 1
      endif


c*****interpolate to estimate the best abundance from the statistics array;
c     put the results from each line into a list 
      call findmin (absyn,sigmasyn,itot,abtab(ntab))
      do j=1,95
         if (names(j) .eq. el2) then
            iel = j
            go to 10
         endif
      enddo
10    if (ion1 .eq. 'II') then
         eltab(ntab) = dble(iel) + 0.1
      else
         eltab(ntab)  = dble(iel)
      endif
      do n=1,nsyn
          if (iwave .eq. wavesyn(n)) 
     .    write (99,1002) wavetab(ntab), el1, ion1, epsyn(n), 
     .                    gfsyn(n), abtab(ntab)
1002      format (f10.1, 3x,2a2, 3f10.2)
      enddo
      ntab = ntab + 1
      go to 1

c*****go through this list, and form mean abundances for species
20    ntab = ntab - 1
      nlo = 1
21    do n=nlo+1,ntab
         if (eltab(n) .ne. eltab(nlo)) then
            nhi = n - 1
            go to 25
         endif
      enddo
      nhi = ntab
25    average = 0.
      kount = 0
      do n=nlo,nhi
         average = average + abtab(n)
         kount = kount + 1
      enddo
      average = average/kount
      deviate = 0.
      if (kount .gt. 1) then
         do n=nlo,nhi
            deviate = deviate + (abtab(n)-average)**2
         enddo
         deviate = dsqrt(deviate/(kount-1))
      endif

c*****add these abundances to the overall abundance arrays
30    el10 = 10.*eltab(nlo) + 0.00001
      iel = int(el10)/10
      ion = int(el10) - 10.*int(el10/10.) + 1
      specnum(iel,ion) =       kount
      specab(iel,ion) =        average
      specdev(iel,ion) =       deviate
      if     (specnum(iel,ion) .eq. 1) then
         specdevadopt(iel,ion) = 0.25
      elseif (specnum(iel,ion) .eq. 2) then
         specdevadopt(iel,ion) = dmax1(dble(0.15),specdev(iel,ion))
      else
         specdevadopt(eli,ion) = dmax1(dble(0.10),specdev(iel,ion))
      endif
      elnum(iel) =             specnum(iel,ion)
      elab(iel) =              specab(iel,ion)
      eldev(iel) =             specdev(iel,ion)
      eldevadopt(iel) =        specdevadopt(eli,ion)
            
c*****bookkeeping to loop back into the abundance array
      if     (nhi .lt. ntab-1) then
         nlo = nhi + 1
         go to 21
      elseif (nhi .eq. ntab-1) then
         nlo = ntab
         nhi = ntab
         kount = 1
         average = abtab(ntab)
         deviate = 0.
         go to 30
      endif

      return
      end




