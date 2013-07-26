
      character*120     formstyle, linespath,
     .			parfpath
c*****variable codepath needs to be exact amount of characters
      character*67	codepath
      character*80      line, moogpath, modkurpath, 
     .                  modmarpath,starfile, logfile, starname,
c     .			linespath, parfpath, 
     .                  ewfilel, ewfiles, ewfileu,
     .                  lnfilel, lnfiles, lnfileu,
     .                  modfile1, modfile2, modfile3, modfile4,
     .                  lnpsfile, obsspecfile, chfile,
     .                  listfile, otherfile, sumfile, abpsfile
      character*5       vtchars
      character*3       machine, gridtype, cnoflag
      character*2       names(95)
      character*1       spectype(95,2), linetype

      common /characts/ formstyle,
     .                  line, codepath, moogpath, modkurpath, 
     .                  modmarpath, linespath,
     .                  parfpath, starfile, logfile, starname, 
     .                  ewfilel, ewfiles, ewfileu,
     .                  lnfilel, lnfiles, lnfileu,
     .                  modfile1, modfile2, modfile3, modfile4,
     .                  lnpsfile, obsspecfile, chfile,
     .                  listfile, otherfile, sumfile, abpsfile,
     .                  vtchars,
     .                  machine, gridtype, cnoflag,
     .                  names,
     .                  spectype, linetype

