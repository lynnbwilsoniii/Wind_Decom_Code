	PROGRAM EMCOPL
C	SOLVES COLD PLASMA DISPERSION EQUATION, NOT ELECTROSTATIC
C	IN STIX'S NOTATION ER IS R,EL IS L,EP IS P,EPERP IS S
C	EPSL IS A
C
C	used to make constant w contours in the k plane
C	this routine makes a file for031.dat, to be plotted by mongo
C	after running this program, exit, and :
C
C	mongo
C	printer 6                           omit to plot on crt
C	set gy1 500                                "
C	set gx2 2700                                "
C	set gy2 2700                                "
C	set lx2 2900                                "
C	set ly2 2900                                "
C	input kplane.plt
C	hardcopy                                    "
C	end
C
	COMPLEX CNU,DN,FN,BN,EPSL,EN2P
	COMPLEX EN2M,XKP,XKM,ERN,ELN,EPN
	COMPLEX ER,EL,EP,EPERP,EPERPN,LAMBDA,ZPL,SQL
 	REAL NU 
C
	DIMENSION XTABLE(100,10),ZTABLE(100,10)
	DIMENSION WP(10),WC(10),NU(10),A(10),DENS(10),WP2(10)
	DATA NC /3/
C	DATA NU /1.E-5,1.E-7,8*0./
	DATA NU /10*0./
	DATA A /1.,16.,8*1./
	DATA DENS /1.,.05,.95,7*1./
	DATA WCYCL /.4/
	DATA TWOPI /6.28318531/
	DATA RSH /13.5/
	DATA ALT /400./
	DATA FP /3.6E6/
	DATA COSTK /1./
C	W,WC,WP2 ARE RATIO TO PLASMA FREQ. OF COMP. 1
C
	PRINT 701, ALT,FP,NU(1),NU(2)
 701	FORMAT('0ALT=',F8.1,' KM, FP=',E10.3,' HZ, E,I COLL F',2E10.3)
	PRINT 703, COSTK,(A(N),N=2,NC)
 703	FORMAT(' COS THETA ='F7.4,3X,'ION AT.WTS.',10F5.1)
	PRINT 704,(DENS(N),N=2,NC)
 704	FORMAT(20X,'ION REL.DENS.',10F5.3)
	PRINT 702
 702	FORMAT('0')
  	A(1) = -1./1836.
	DO 11 N = 1,NC
	WP2(N) = DENS(N)/1836./ABS(A(N))
	WC(N) = WCYCL/1836./A(N)
 11	CONTINUE
	W = 1.25E-7
	DO 1 NIT = 1,9
	W=NIT*WCYCL/10.
	FHZ = FP*W
C
	NCOS = 45
C     CALCULATE EPSILON AND MINIMUM COS FOR WHISTLER MODE
      ER = 1. 
      EL = 1. 
      EP = 1. 
      EMAG = 1. 
      DO 20 N = 1,NC  
C     REPLACE M BY M*(1. - I*NU/W)
      CNU = CMPLX(0.,NU(N))/FP/TWOPI
      ER = ER - WP2(N)/W/(W-CNU+WC(N))
      EL = EL - WP2(N)/W/(W-CNU-WC(N))
      EP = EP - WP2(N)/W/(W-CNU)
      EMAG = AMAX1(EMAG,CABS(ER),CABS(EL),CABS(EP)) 
 20   CONTINUE
      EPERP = .5*(ER + EL)
	COSMIN2 = 1./(1. - EP/EPERP)
	COSMIN = SQRT(AMAX1(COSMIN2,0.))
	DELCOS = (1. - COSMIN)/(NCOS-1)
	DO 2 ICOS = 1,NCOS
	COSTK = 1. - (ICOS-1)*DELCOS
C
C     CALCULATE EPSILON 
      ER = 1. 
      EL = 1. 
      EP = 1. 
      EMAG = 1. 
      DO 10 N = 1,NC  
C     REPLACE M BY M*(1. - I*NU/W)
      CNU = CMPLX(0.,NU(N))/FP/TWOPI
      ER = ER - WP2(N)/W/(W-CNU+WC(N))
      EL = EL - WP2(N)/W/(W-CNU-WC(N))
      EP = EP - WP2(N)/W/(W-CNU)
      EMAG = AMAX1(EMAG,CABS(ER),CABS(EL),CABS(EP)) 
 10   CONTINUE
	ERN = ER/EMAG
	ELN = EL/EMAG
	EPN = EP/EMAG
      EPERPN = .5*(ERN + ELN)
      EPERP = .5*(ER + EL)
	DN = .5*(ERN - ELN)
	SINTK2 = 1. - COSTK**2
	FN = CSQRT(((ERN*ELN-EPN*EPERPN)*SINTK2)**2
     1    + 4.*(EPN*DN*COSTK)**2)
	BN = ERN*ELN*SINTK2 + EPN*EPERPN*(1. + COSTK**2)
	EPSL = EPERP*SINTK2 + EP*COSTK**2
	EN2P = .5*EMAG**2*(BN + FN)/EPSL
	XKP = W*CSQRT(EN2P)
	EN2M = .5*EMAG*(BN - FN)/(EPSL/EMAG)
	XKM = W*CSQRT(EN2M)
C	PRINT 700,FHZ,EP,EPERP,ER,EL,EMAG
C	PRINT 700,FHZ,EPN,EPERPN,ERN,ELN
C	PRINT 700,FHZ,EN2P,XKP,EN2M,XKM
	SINTK = SQRT(AMAX1(SINTK2,0.))
	XKK = XKP
	XX2M = EN2M
	IF(XX2M.GT.0.) XKK = XKM
	XK = XKK*SINTK
	ZK = XKK*COSTK
	ZTABLE(ICOS,NIT) = ZK
	XTABLE(ICOS,NIT) = XK
 2	CONTINUE
 1	CONTINUE
	DO 3 I = 1,ICOS
	WRITE (31,2103) (XTABLE(I,NIT),ZTABLE(I,NIT),NIT=1,9)
 2103	FORMAT(18F7.4)
  3	CONTINUE
C     PRINT 100,W,X(1),EPSR,EPS0,EP 
  100 FORMAT(' COPLAS' 8E12.4)  
  700 FORMAT(' 'F12.4,4(E13.4,E12.4),E12.4)
C      RETURN
	STOP
      END 
