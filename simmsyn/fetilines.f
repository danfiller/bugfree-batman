
c***********************************************************************
      subroutine fetilines
c***********************************************************************
c     this routine searches the line abundance output file for Fe I,
c     Fe II, Ti I, and Ti II lines, and stores the results for individual
c     transitions, the mean abundances, and trend-line parameters to
c     be then used as data for a plot

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      character*1 chars(80)
      equivalence (chars,line)
      character*2 nameel, nameion

c     open the abfind output file
      open (13,file=lnfilel(1:ifile))


c*****read individual line and mean abundance information for just
c     Fe I, Fe II, Ti I, and Ti II; these data will be used in a plot
      nline = 0
15    read (13,1002,end=30) line
1002  format (a80)
      if (line(31:35) .ne. 'Ti I ' .and.
     .    line(31:35) .ne. 'Ti II' .and.
     .    line(31:35) .ne. 'Fe I ' .and.
     .    line(31:35) .ne. 'Fe II') then
         go to 15
      else
         read (line(31:35),1001) nameel, nameion
1001     format (a2, 1x, a2) 
         call nametrans (nameel, nameion, nel, nion, atom1)
         read (13,1002,end=30) line
20       read (13,1002,end=30) line
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
         endif
         read (13,1002,end=30) line
         if (line(1:5) .eq. 'E.P. ') read (line(28:55),1011) 
     .                  epslope(nel,nion), epintercept(nel,nion)
1011     format (f7.3, 14x, f7.3)
         read (13,1002,end=30) line
         if (line(1:5) .eq. 'R.W. ') read (line(28:55),1011) 
     .                  rwslope(nel,nion), rwintercept(nel,nion)
         go to 15
      endif

c*****search through the species mean abundance data; form elemental
c     mean abundances (straight averages of neutral and ion abundances);
c     and compute a metallicity in [] notation
30    if     (specnum(22,1) .gt. 0) then
         if (specnum(22,2) .gt. 0) then
            abti = (specab(22,1)+specab(22,2))/2.
            
         else
            abti = specab(22,1)
         endif
      else
         abti = specab(22,2)
      endif
      if     (specnum(26,1) .gt. 0) then
         abfe = specab(26,1)
      else
         abfe = specab(26,2)
      endif
      metallicity = abfe - 7.50

      close (unit=13)

      return
      end



