	PROGRAM ZETA
C
C	FOR POTENTIAL AROUND AN ELLIPSOID IN ANISOTROPIC PLASMA
C
	COMPLEX LAMBDA,BB,CC,DISC,SDISC,SRATIO,ZETAP,ZETAN
	REAL LOV2R
C
	DATA ROCKR,ROCKL /22.,500./
C	SRATIO IS SIGMA PARALLEL / SIGMA PERP
	DATA SRATIO /-1.E3/
C
C	RATIO OF LENGTH TO DIAMETER OF ROCKET
	LOV2R = .5*ROCKL/ROCKR
	LAMBDA = SRATIO/LOV2R**2
	PRINT*,'SRATIO',SRATIO
	PRINT*,'LAMBDA',LAMBDA
C
	Z = 0.
	R = 0.
	DO 101 I = 1,50
  100	CONTINUE
C	Z = 20.*I + 230.
	R = 20. + 2.*I
C	READ*,R,Z
C	TO FORCE R,Z TO BE ON THE SURFACE OF THE ELLIPSOID
c	R = SQRT(ROCKR**2 - Z**2/LOV2R**2)
	BB = ROCKR**2*(1. + 1./LAMBDA) - R**2 - Z**2/SRATIO
	CC = ROCKR**2*((ROCKR**2 - R**2)/LAMBDA - Z**2/SRATIO)
	DISC = BB**2 - 4.*CC
	SDISC = CSQRT(DISC)
C      PRINT*,'B,C,D,sd',BB,CC,DISC,sdisc
	ZETAP = .5*(-BB + SDISC)
	ZETAN = .5*(-BB - SDISC)
	PRINT 1001,R,Z,ZETAP,ZETAN
 1001	FORMAT(2E12.3,2(E13.3,E12.3))
C	GO TO 100
  101	CONTINUE
	STOP
	END