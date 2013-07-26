
c***********************************************************************
      block data preset
c***********************************************************************
c     this data area contains element names, solar abundances, and
c     Pop II star abundances

      implicit real*8 (a-h,o-z)
      include 'quants.com'
      include 'characts.com'

      data (names(i),i=1,95) /
     .           'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     .           'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca',
     .           'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn',
     .           'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr',
     .           'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',
     .           'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd',
     .           'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb',
     .           'Lu','Hf','Ta','Wl','Re','Os','Ir','Pt','Au','Hg',
     .           'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th',
     .           'Pa','U ','Np','Pu','Am'/

      data (xsolar(i),i=1,95) /
     * 12.00,10.99, 3.31, 1.42, 2.88, 8.56, 8.05, 8.93, 4.56, 8.09,
     1  6.33, 7.58, 6.47, 7.55, 5.45, 7.21, 5.5 , 6.56, 5.12, 6.36,
     2  3.10, 4.99, 4.00, 5.67, 5.39, 7.52, 4.92, 6.25, 4.21, 4.60,
     3  2.88, 3.41, 2.37, 3.35, 2.63, 3.23, 2.60, 2.90, 2.24, 2.60,
     4  1.42, 1.92, 0.00, 1.84, 1.12, 1.69, 1.24, 1.86, 0.82, 2.0 ,
     5  1.04, 2.24, 1.51, 2.23, 1.12, 2.13, 1.22, 1.55, 0.71, 1.50,
     6  0.00, 1.00, 0.51, 1.12, 0.33, 1.1 , 0.50, 0.93, 0.13, 1.08,
     7  0.12, 0.88, 0.13, 0.68, 0.27, 1.45, 1.35, 1.8 , 0.83, 1.09,
     8  0.82, 1.85, 0.71, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.12,
     9  5*0.0/

      data (xfe122563(i),i=1,95) /
     . 12.00,10.99,99.99,99.99,99.99,-0.46, 1.09, 0.61,99.99,99.99,
     . -0.15, 0.51,-0.32, 0.46,99.99,99.99,99.99,99.99,99.99, 0.29,
     .  0.15, 0.24, 0.10,-0.39,-0.26, 0.00, 0.32, 0.04,-0.75, 0.20,
     . 99.99,-0.70,99.99,99.99,99.99,99.99,99.99, 0.17,-0.25, 0.18,
     . 99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,
     . 99.99,99.99,99.99,99.99,99.99,-0.93,-0.70,-0.73,99.99,-0.30,
     . 99.99,99.99,-0.36,99.99,99.99,99.99,99.99,-0.46,99.99,-0.96,
     . 99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,
     . 99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,99.99,
     . 99.99,99.99,99.99,99.99,99.99/

      end
