	program efflength
c
c	see page 35 of Wind N.B. Vol 2 for derivation
c		L is monopole length, R is radius of spherical spacecraft
c		efflratio is ratio of effective length to L
c		ratiolr is ratio of L to R
c
	DO I = 1,50
	  RATIOLR = .1*I
	  EFFLRATIO  = (1. + 1./RATIOLR)**2 + 2./(RATIOLR)**3/(1. + 
     1		1./RATIOLR) - 3./(RATIOLR**2)
	  PRINT*,RATIOLR,EFFLRATIO,(1. + 2./RATIOLR)
	ENDDO
	  RATIOLR = 41.67			! WIND X
	  EFFLRATIO  = (1. + 1./RATIOLR)**2 + 2./(RATIOLR)**3/(1. + 
     1		1./RATIOLR) - 3./(RATIOLR**2)
	  PRINT*,RATIOLR,EFFLRATIO,(1. + 2./RATIOLR)
	  RATIOLR = 6.25
	  EFFLRATIO  = (1. + 1./RATIOLR)**2 + 2./(RATIOLR)**3/(1. + 
     1		1./RATIOLR) - 3./(RATIOLR**2)
	  PRINT*,RATIOLR,EFFLRATIO,(1. + 2./RATIOLR)
	  RATIOLR = 4.4/.9
	  EFFLRATIO  = (1. + 1./RATIOLR)**2 + 2./(RATIOLR)**3/(1. + 
     1		1./RATIOLR) - 3./(RATIOLR**2)
	  PRINT*,RATIOLR,EFFLRATIO,(1. + 2./RATIOLR)
	STOP
	END
