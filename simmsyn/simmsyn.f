
c***********************************************************************
      program simmodf
c***********************************************************************
c     this routine sets up, executes, and oversees the various steps 
c     needed for an end-to-end abundance run for Ian's MIKE spectra

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'
      character*33 homepath
      character*17 relativepath

c$$$$$$$$$$$$$$$$$$$$$$$ USER SETUP AREA $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

c*****put in the machine-dependent path to this code, and put in
c     the machine-dependent commands to the Kurucz model interpolation
c     code and to the MOOG batch synthesis code

c!!!!!PLEASE DECLARE STRINGS OF EXACT LENGHT FOR 'CODEPATH'
      homepath = '/uufs/astro.utah.edu/common/home/'
      relativepath = 'u0424149/bugfree-batman/simmsyn/'
      codepath = homepath // relativepath
c      write (*,*) codepath

      
      call countline (codepath, ncodep)
      write (moogpath,1001)
c**link to where silent moog live. ../products/ symbolic link in:
c1001  format ('/uufs/astro.utah.edu/common/astro_data/',
c     .        'astrougrad/u0424149/MGS',' < batch.par')
1001  format ('/uufs/astro.utah.edu/common/home/u0715212/MGS',
     .        ' < batch.par')
      call countline (moogpath, nmoogp)
      write (modkurpath,1002)
c**jennifers symbolic link to the silent model creation codes
c**these live in the /uufs//products/something
1002  format ('/uufs/astro.utah.edu/common/home/u0715212/kse',
     .        ' < modelparams')
      write (*,*) modkurpath
      call countline (modkurpath, nmodkp)
      write (modmarpath,1003)
1003  format ('/uufs/astro.utah.edu/common/home/u0715212/mse',
     .        ' < modelparams')
      call countline (modmarpath, nmodmp)
c      linespath = codepath//'linelists'
c      write (*,*) linespath
      linespath =
     .     '../simmsyn/linelists'
c     .     codepath//'linelists'
      call countline (linespath, nlinep)
c      parfpath = codepath // 'parfiles'
c      write (*,*) parfpath
      parfpath =
c     .     codepath//'parfiles'
     .     '../simmsyn/parfiles'
c      pause
      call countline (parfpath, nparfp)

c*****in order to get landscape-mode plots, "sm" must be given the 
c     appropriate flag in its call (look at the end of lineplot.f).  
c     This is machine-dependent, so here one specifies the machine.
c     "mac" = Intel-based Apple Mac
c     "pcr" = a PC or desktop running Redhat linux
c     "pcl" = a PC running another form of linux
c     "uni" = a machine running Unix, specifically Sun Solaris
      machine = "pcr"

c$$$$$$$$$$$$$$$$$$$$$$$ USER SETUP AREA $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

c*****do basic file setup
      call startup

c*****read in the long spectrum
c      call readspec

c*****make the equivalent width file from the received data file
      call makeewfiles

c*****generate the model atmosphere
      niter = 1
1     call makemodel

c*****run abfind for the EWs of ordinary lines
      linetype = 'l'
      call abfindrun

c*****pull out the Fe I, Fe II, Ti I, and Ti II abundance data
      call fetilines

c*****plot abundances of Fe and Ti versus EP and log(RW)
      call lineplot

c*****decide whether or not to iterate on this model
      call iterate
      if (niter.gt.oldniter .and. niter.lt.2) go to 1

c*****run abfind for the EWs of lines that will be synthesized
      linetype = 's'
      call abfindrun
c*****run abfind for the EWs of upper limits to lines
      linetype = 'u'
      call abfindrun

c*****put these approximate and upper limit abundances into the model
      call abfillall

c*****do a grid synthesis just for the carbon abundance
      call docarbon

c*****interpolate to derive the best carbon estimate, add it to model
      call fixcarbon

c*****do the other syntheses
      call dosyns

c*****interpolate the best abundances from these syntheses
      call extract

c*****make a summary plot of abundance ratios in [] notation
      call abundplot

c100   call system ('rm -f kurmodinput out* batch* ps* for*')
      write (*,*) 'DONE!   IGNORE "rm" WARNINGS.'

      end


