
c***********************************************************************
      subroutine iterate
c***********************************************************************
c     this routine decides whether model iterations are needed, and
c     if so by how much; empirical fudge factors may be put in to
c     "epslope" and "deltamean" to get faster convergence

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      real*8 loggtemp, vttemp, tefftemp 
      integer loggflag


      fudgeteff = 0.5
      fudgevt   = 1.2
      fudgelogg = 1.3
      asym      = 1.0
      fracasym  = 0.2


c*****Set this flag to completely override any gravity determinations
c     (e.g., in case of very few or no Fe II lines).  Only the 
c     temperature, microturbulence, and metallicity will be altered, 
c     and the gravity will automatically revert to the input value.
c     0 = normal
c     1 = keep original input gravity
      gravkeep = 0

      if (gravkeep .eq. 1) loggkeep = logg


c*****Set this flag to indicate how to determine the gravity:
c     0 = weighted combination of Ti (1x) and Fe (2x)
c     1 = Ti only
c     2 = Fe only
      loggflag = 2


c*****is a logg iteration needed because the gravity is WAAAY off?
      if (gravkeep .eq. 1) go to 48
      deltafe12 = specab(26,2) - specab(26,1)
      deltati12 = specab(22,2) - specab(22,1)
      if (loggflag .eq. 0) then
            deltamean = (deltati12 + 2.*deltafe12)/3.
         elseif (loggflag .eq. 1) then
            deltamean = deltati12
         elseif (loggflag .eq. 2) then
            deltamean = deltafe12
      endif
      if (dabs(deltamean) .gt. 0.25) then 
         loggold = logg
         loggtemp = logg - fudgelogg*deltamean
         logg = dble(int((loggtemp/0.05+0.5)))*0.05 + 0.00001
         call paramlimits
      endif


c*****is a Teff iteration needed?
c 48   if (dabs(epslope(26,1)) .gt. 0.035) then
 48   if (dabs(epslope(26,1)) .gt. 0.01) then
         if (epslope(26,1) .lt. 0.) then
            asym = 1.0-fracasym
         else
            asym = 1.0+fracasym
         endif
         oldteff = teff
         oldniter = niter
         niter = niter + 1
         tefftemp = 5040./(5040./teff-fudgeteff*epslope(26,1)*asym)
         teff = dble(int((tefftemp/10.0+0.5)))*10.0 + 0.00001
         vttemp = vt + (teff-oldteff)/3000.
         vt = dble(int((vttemp/0.05+0.5)))*0.05 + 0.00001
         loggtemp = logg + (teff-oldteff)/2000.
         logg = dble(int((loggtemp/0.05+0.5)))*0.05 + 0.00001
         call paramlimits
         return
      endif


c*****is a v_t iteration needed?
c      if (dabs(rwslope(26,1)) .gt. 0.15) then
      if (dabs(rwslope(26,1)) .gt. 0.07) then
         if (rwslope(26,1) .gt. 0.) then
            asym = 1.0-fracasym
         else
            asym = 1.0+fracasym
         endif
         oldvt = vt
         oldniter = niter
         niter = niter + 1
         vttemp = vt + fudgevt*(rwslope(22,2)+2.*rwslope(26,1))/3.*asym
         vt = dble(int((vttemp/0.05+0.5)))*0.05 + 0.00001
         tefftemp = teff + (vt-oldvt)*1000.
         teff = dble(int((tefftemp/10.0+0.5)))*10.0 + 0.00001
         loggtemp = logg + (vt-oldvt)*2.
         logg = dble(int((loggtemp/0.05+0.5)))*0.05 + 0.00001
         call paramlimits
         return
      endif


c*****is a logg iteration needed?
      if (gravkeep .eq. 1) go to 49
      deltafe12 = specab(26,2) - specab(26,1)
      deltati12 = specab(22,2) - specab(22,1)
      if (loggflag .eq. 0) then
            deltamean = (deltati12 + 2.*deltafe12)/3.
         elseif (loggflag .eq. 1) then
            deltamean = deltati12
         elseif (loggflag .eq. 2) then
            deltamean = deltafe12
      endif
      if (deltamean .lt. 0.) then
         asym = 1.0-fracasym
      else
         asym = 1.0+fracasym
      endif
c     this functions as the convergence criterion: A(FeI)=A(FeII)+-0.12
c      if (dabs(deltafe12) .gt. 0.12) then
      if (dabs(deltafe12) .gt. 0.05) then
         oldniter = niter
         niter = niter + 1
         loggtemp = logg - 2.*fudgelogg*deltamean*asym
         logg = dble(int((loggtemp/0.05+0.5)))*0.05 + 0.00001
         call paramlimits
         return
      endif


*****is a more precise v_t iteration needed, using only Fe?
c 49   if (dabs(rwslope(26,1)) .gt. 0.07) then
 49   if (dabs(rwslope(26,1)) .gt. 0.005) then
         if (rwslope(26,1) .gt. 0.) then
            asym = 1.0-fracasym
         else
            asym = 1.0+fracasym
         endif
         oldvt = vt
         oldniter = niter
         niter = niter + 1
         vttemp = vt + 1.5*fudgevt*rwslope(26,1)*asym
         vt = dble(int((vttemp/0.05+0.5)))*0.05 + 0.00001
         tefftemp = teff + (vt-oldvt)*500.
         teff = dble(int((tefftemp/10.0+0.5)))*10.0 + 0.00001
         loggtemp = logg + (vt-oldvt)*1.5
         logg = dble(int((loggtemp/0.05+0.5)))*0.05 + 0.00001
         call paramlimits
         return
      endif


c*****is a more precise Teff iteration needed?
c      if (dabs(epslope(26,1)) .gt. 0.025) then
      if (dabs(epslope(26,1)) .gt. 0.005) then
         if (epslope(26,1) .lt. 0.) then
            asym = 1.0-fracasym
         else
            asym = 1.0+fracasym
         endif
         oldteff = teff
         oldniter = niter
         niter = niter + 1
         tefftemp = 5040./(5040./teff-fudgeteff*epslope(26,1)*asym)
         teff = dble(int((tefftemp/10.0+0.5)))*10.0 + 0.00001
         vttemp = vt + (teff-oldteff)/3000.
         vt = dble(int((vttemp/0.05+0.5)))*0.05 + 0.00001
         loggtemp = logg + (teff-oldteff)/2000.
         logg = dble(int((loggtemp/0.05+0.5)))*0.05 + 0.00001
         call paramlimits
         return
      endif


c*****iterations are done
      oldniter = niter


      return
      end


