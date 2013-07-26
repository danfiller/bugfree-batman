
c***********************************************************************
      subroutine fixcarbon
c***********************************************************************
c     pick out a best carbon, and stuff that result in the model, add
c     it to the derived abundance list

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      character*80 templine
      character*1 chars(80)
      equivalence (chars,templine)

      real*8 abcarb(50), sigmacarb(50)

c*****in the rare case where no CH synthesis has been done, do not
c     put any C abundance in the model file; then [C/Fe] = 0 will
c     be assumed in subsequent syntheses
      if (cnoflag(1:1) .ne. 'C') go to 20

c*****read the C abundances and the (obs-syn) sigmas from the C summary file
      call countline (chfile, ifile)
      open (14,file=chfile(1:ifile))
      itot = 0
1     read (14,1001,end=10) line
1001  format (a80)
      if (line(1:2) .ne. 'C ') go to 1
      itot = itot + 1
      read (line(3:7),1002) abcarb(itot)
1002  format (f5.2)
      read (14,1001,end=10) line
      read (line(8:15),1003) sigmacarb(itot)
1003  format (f8.5)
      go to 1

c*****interpolate among the C abundances to find the best value
10    call findmin (abcarb,sigmacarb,itot,abcarbon)

c*****open the old and new model files
20    open (15,file=modfile2(1:ifile))
      templine = '.mod03'
      call meld (starname, templine, modfile3, ifile)
      open (16,file=modfile3(1:ifile))

c*****just copy *.mod02 to *.mod03 if CH synthesis has not been done
      if (cnoflag(1:1) .ne. 'C') then
15       read(15,1001,end=100) line
         write (16,1001) line
         go to 15
      endif

c*****write the C abundance into a new version of the model atmosphere file
5     read (15,1001,end=100) line
      if (line(21:35) .eq. '       6.0     ') then
         write (16,1004) line (1:35), abcarbon, line(41:80)
1004     format (a35, f5.2, a40)
      else
         write (16,1001) line
      endif
      go to 5
100   close (unit=14)
      close (unit=15)
      close (unit=16)

c*****add C abundance data to the species and elemental abundance arrays
      specnum(6,1) =      1
      specab(6,1) =       abcarbon
      specdev(6,1) =      0.15
      specdevadopt(6,1) = 0.15
      spectype(6,1) =     's'
      elnum(6) =          1
      elab(6) =           abcarbon
      eldev(6) =          0.15
      eldevadopt(6) =     0.15
      templine = modfile3
      write (99,1005) abcarbon, (chars(i),i=1,ifile)
1005  format (' put derived C abundance (', f5.2,
     .        ') into new model file: ', 30a1)
      return
      end



