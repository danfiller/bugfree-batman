
c***********************************************************************
      subroutine makemodel
c***********************************************************************
c     this routine writes out an input file to the Kurucz model 
c     interpolator and runs the interpolator.

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      character*7 modelset

      character*80 templine
      character*1 chars(80)
      equivalence (chars,line)

      open (8,file='modelparams')
      if (int(100.*vt+0.001) .ne. 200) then
         write (99,1005) vt
1005     format (' WARNING: input v_t is ', f4.2, 
     .           ' but using 2.00 for model interpolation')
         vtuse = 2.00
      else
         vtuse = vt
      endif
      if (niter .gt. 1) then
         fehadopttemp = dmax1(metallicity,dble(-4.0))
         fehadopt = dble(int((fehadopttemp/0.05+0.5)))*0.05 + 0.00001
      endif
      if     (gridtype .eq. 'KUR') then
         modelset = 'AODFNEW'
      elseif (gridtype .eq. 'MAR') then
         if (logg .gt. 3.) then
            modelset = 'MARCSsp'
         else
            modelset = 'MARCSss'
         endif
      endif
      write (8,1009) int(teff+0.001), logg, fehadopt, vtuse, modelset
1009  format (i6, 3f8.2 / a7)
      close (unit=8)

      if     (gridtype .eq. 'KUR') then
          call system (modkurpath)
      elseif (gridtype .eq. 'MAR') then
         call system (modmarpath)
      endif
      call system ('rm -f M1 M2 MOD*')

      open (9,file='tempmodel')
      templine = '.mod01'
      call meld (starname, templine, modfile1, ifile)
      open (4,file=modfile1(1:ifile))
1001  format(80a1)

1     read (9,1002,end=100) line
1002  format (a80)
      if (line(1:5).eq.'     ' .and. line(12:13).eq.'05') 
     .   write (line(6:9),1006) vt
1006     format (f4.2)
      write (4,1002) line
      go to 1

100   close (unit=4)
      close (unit=9)
      call system ('rm -f tempmodel')

      line(1:ifile) = modfile1(1:ifile)
      write (99,1003) (chars(i),i=1,ifile) 
1003  format (' created initial model file: ', 60a1) 
      write (99,1004) int(teff+0.001), logg, fehadopt, vtuse
1004  format ('        Teff= ', i4, '   log(g)= ', f5.2, 
     .        '   [M/H]= ', f5.2, '   V_t= ', f5.2)
      return
      end



