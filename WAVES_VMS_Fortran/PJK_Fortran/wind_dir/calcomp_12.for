	PROGRAM CALCOMP
C
C	CALCULATES GAIN AND PHASE SHIFT OF X AND Y,Z CAL SIGNALS
C
	COMPLEX VX,VYZ,Y2,Z1,Z2,ZTERM
	DATA R5X,R5YZ /3900.,1000./
	DATA R4 /1.E4/
	DATA R34X,R34YZ /150.,51./
	DATA C24X,C24YZ /680.E-12,2.2E-9/
	DATA TWOPI /6.2831853/
C
	DO N = 1,60
	  F = 1000.*N
	  W= TWOPI*F
C		DO X PREAMP
	  ZTERM = CMPLX(R34X,-1./W/C24X)
	  Y2 = 1./ZTERM + 1./R5X
	  Z2 = 1./Y2
	  VX = Z2/(R4 + Z2)	  	  
C		DO Y,Z PREAMP
	  ZTERM = CMPLX(R34YZ,-1./W/C24YZ)
	  Y2 = 1./ZTERM + 1./R5YZ
	  Z2 = 1./Y2
	  VYZ = Z2/(R4 + Z2)	  	  
	  RGAIN = CABS(VX)/CABS(VYZ)
	  DB = 20.*ALOG10(RGAIN)
	  REX = REAL(VX)
	  REYZ = REAL(VYZ)
	  XIM = AIMAG(VX)
	  YZIM = AIMAG(VYZ)
	  PHX = (360./TWOPI)*ATAN2(XIM,REX) 
	  PHYZ = (360./TWOPI)*ATAN2(YZIM,REYZ)
	  PHASE = (PHX - PHYZ)
	  PRINT 101,  F,RGAIN,DB,PHX,PHYZ,PHASE
	  WRITE(77,101) F,RGAIN,DB,PHX,PHYZ,PHASE
 101	  FORMAT(F10.1,F8.4,F6.2,3F7.2)
	ENDDO
	STOP
	END