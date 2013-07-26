
c***********************************************************************
      subroutine findmin (absyn,sigmasyn,itot,abbest)
c***********************************************************************
c     this routine interpolates within an array (abundance,sigma) 
c     values from synthetic/observed spectrum fits, and returns the 
c     interpolated abundance for the minimum sigma

      implicit real*8 (a-h,o-z)
      real*8 absyn(20), sigmasyn(20), abinterp(100), siginterp(100)
      real*8 abstep, abbest, sigminimum, x1, x2, x3, y1, y2, y3

      imin = 1
      sigminimum = sigmasyn(imin)
      do i=1,itot
         if (sigmasyn(i) .lt. sigminimum) then
            imin = i
            sigminimum = sigmasyn(imin)
         endif
      enddo
                                                 
      if     (imin .eq. 1) then
         abbest = absyn(1)
      elseif (imin .eq. itot) then
         abbest = absyn(itot)
      else
         abstep = (absyn(imin+1) - absyn(imin-1))/100.
         x1 = absyn(imin-1)
         x2 = absyn(imin)
         x3 = absyn(imin+1)
         y1 = sigmasyn(imin-1)
         y2 = sigmasyn(imin)
         y3 = sigmasyn(imin+1)
         do i=1,100
            abinterp(i) = x1 + abstep*(i-1)
            x = abinterp(i)
            p1 = (x-x2)*(x-x3)/((x1-x2)*(x1-x3))
            p2 = (x-x1)*(x-x3)/((x2-x1)*(x2-x3))
            p3 = (x-x1)*(x-x2)/((x3-x1)*(x3-x2))
            siginterp(i) = y1*p1 + y2*p2 + y3*p3
         enddo
         imin = 1
         sigminimum = siginterp(imin)
         do i=1,100
            if (siginterp(i) .lt. sigminimum) then
               imin = i
               sigminimum = siginterp(imin)
            endif
         enddo
         abbest = abinterp(imin)
      endif

      return
      end




