
c***********************************************************************
      subroutine paramlimits
c***********************************************************************
c     Limits the model atmosphere parameters to the grid limits
c     to prevent runaways

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

c*****gravity
      if (logg .lt. 0.01) then
        logg = 0.0
      endif
      if (logg .gt. 5.0) then
        logg = 5.0
      endif

c*****temperature
      if (teff .lt. 3901.) then
        teff = 3900.
      endif
      if (teff .gt. 7500.) then
        teff = 7500.
      endif

c*****microturbulence
      if (vt .lt. 0.01) then
        vt = 0.0
      endif
      if (vt .gt. 4.0) then
        vt = 4.0
      endif

c*****note that there is a metallicity check elsewhere in the code

      if (gravkeep .eq. 1) logg = loggkeep


      return
      end
