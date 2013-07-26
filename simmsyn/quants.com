
c*****lineid10, lineab, lineep, linerw, and linewave are the parameters
c     for individual lines; linespec is the species
c*****specab, specdev, specdevadopt, and specnum are the 
c     parameters for average abundances of individual species
c*****elab, eldev, eldevadopt, and elnum are the parameters for
c     average abundances of individual elements

      real*8          lineab(1000), lineep(1000), 
     .                linerw(1000), linewave(1000), 
     .                xsolar(95), xfe122563(95), 
     .                xfe1(95,2), xfe2(95,2),
     .                specab(95,2), specdev(95,2), specdevadopt(95,2), 
     .                epslope(95,2), epintercept(95,2),
     .                rwslope(95,2), rwintercept(95,2),
     .                elab(95), eldev(95), eldevadopt(95),
     .                vrad, teff, logg, feh, fehadopt, vt, loggkeep, 
     .                vbluesmooth, vredsmooth, wavebluered, 
     .                vfwhmblueobs, vfwhmredobs, 
     .                ablilim, abcarbon, abti, abfe, metallicity 
      integer         lineid10(1000), specnum(95,2), elnum(95), 
     .                ncodep, nmoogp, nmodkp, nmodmp, nlinep, nparfp,
     .                iname, ifile, irun, nline, nspec, nelem, 
     .                niter, oldniter, gravkeep

      common /quants/ lineab, lineep,
     .                linerw, linewave,
     .                xsolar, xfe122563,
     .                xfe1, xfe2,
     .                specab, specdev, specdevadopt,
     .                epslope, epintercept,
     .                rwslope, rwintercept,
     .                elab, eldev, eldevadopt,
     .                vrad, teff, logg, feh, fehadopt, vt, loggkeep, 
     .                vbluesmooth, vredsmooth, wavebluered,
     .                vfwhmblueobs, vfwhmredobs,
     .                ablilim, abcarbon, abti, abfe, metallicity,
     .                lineid10, specnum, elnum,
     .                ncodep, nmoogp, nmodkp, nmodmp, nlinep, nparfp,
     .                iname, ifile, irun, nline, nspec, nelem, 
     .                niter, oldniter, gravkeep


