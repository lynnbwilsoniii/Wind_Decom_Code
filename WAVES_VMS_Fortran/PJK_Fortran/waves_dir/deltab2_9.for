	PROGRAM DELTAB
C
C
	DATA TPAR /.15E-4/
	DATA ANG /11./
	DATA WRSAVE,XKSAVE /1.0014,7.13/
	DATA VGP0 /-1./
	DATA WCYCL /.0062/
C
	OPEN(UNIT=13,FILE='FOR013.DAT',STATUS='OLD',READONLY)
	READ(13,*) XKPR,WR,ETOT,ETRANS,BTOT
	WRSAVE = WR
	XKSAVE = XKPR
	ESAVE = ETOT
	BSAVE = BTOT
 100	CONTINUE
	READ(13,*,END=200) XKPR,WR,ETOT,ETRANS,BTOT
	VGP = (WR-WRSAVE)/(XKPR-XKSAVE)
	IF(VGP.LE.0.) THEN
	  WR = SQRT(1. + 3.*TPAR*XKPR**2)
	  EPS = 1.5*XKPR**2*TPAR
	  VGP = 3.*XKPR*TPAR/WR
	  XKAV = XKPR
	  WRA = WR
	ELSE
	  XKAV = .5*(XKSAVE + XKPR)
	  WRA = .5*(WRSAVE + WR)
	ENDIF
	IF(VGP0.LE.0.) VGP0 = VGP
C
C
C
	BT = SQRT(VGP0/VGP)*BTOT
	ET = SQRT(VGP0/VGP)*ETRANS
	WRITE(14,*) XKAV,WRA,VGP,BT,ET 
	WRSAVE = WR
	XKSAVE = XKPR
	ESAVE = ETOT
	BSAVE = BTOT
	GO TO 100
 200	CONTINUE
	STOP
	END