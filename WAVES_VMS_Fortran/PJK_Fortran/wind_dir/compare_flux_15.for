	PROGRAM COMPARE_FLUX
C
C	COMPARE ACCURATE FLUXES FROM STUART AND MY APPROXIMATION
C
	CHARACTER*50 FILENAME
	INTEGER DATE,YYYYMMDD,ANGLE
C
	OPEN(UNIT=89,FILE='ZTABLE.DAT',STATUS='OLD',READONLY)
	READ(89,1000) JUNK
	print*,junk
	READ(89,1000) JUNK
	print*,junk
 1000 	FORMAT(A)
	IS = 0
	ISZ = 0
	ITOT = 0
C
 100	CONTINUE
	READ(89,*,END=200,ERR=100) YYYYMMDD,TS,TEND,XDC,XPK,YDC,YPK,RZ,
     1   	SXDC,SXPK,SYDC,SYPK,STZ,ANGLE,DENS,SDENS,TE,STE,TI,STI,
     2		FLUX,RBASE,RX,RY
c
	print*,'ztable',yyyymmdd,flux,rx,ry
	IF(YYYYMMDD.LT.20000203) GO TO 100
	IF(YYYYMMDD.GT.20000314) stop 'end of stuart data'
	ITOT = ITOT+1
C
C
c	  IS = IS+1
c	  FF(IS) = FLUX
c	  DD(IS) = DENS
c 	  SIGDD(IS) = SDENS
c	  TTEE(IS) = TE
c	  TTEE(IS) = TE
C
	FLUX = 1.68E7*FLUX
C	
C
	WRITE(FILENAME,123) YYYYMMDD
 123	FORMAT('MONTHLY:[KELLOGG.EFLUX]FLUX_',I8.8,'.ASC')	
	OPEN(UNIT=83,FILE=FILENAME,STATUS='OLD',READONLY)
	TEFLUX = 0.
	SEFLUX = 0.
	ECOUNT = 0.
 140	CONTINUE
	READ(83,*,END=141,ERR=140) NYR,MO,NDAY,N4,N5,N6,EFLUX
	print*,'nyr,n4,n5,eflux',nyr,n4,n5,eflux
	SEFLUX = SEFLUX + EFLUX**2
	TEFLUX = TEFLUX + EFLUX
	ECOUNT = ECOUNT + 1.
	GO TO 140
 141	CONTINUE
	TEFLUX = TEFLUX/ECOUNT
	SEFLUX = SEFLUX/ECOUNT - TEFLUX**2
	SEFLUX = SQRT(AMAX1(SEFLUX,0.))
	WRITE(72,1072) YYYYMMDD,DENS,SDENS,TE,STE,FLUX,TEFLUX,SEFLUX,
     1		FLUX-TEFLUX
 1072	FORMAT(I9,F7.2,F7.3,F6.1,F6.2,E11.3,E11.3,E11.3,e11.3)
	GO TO 100
 200	CONTINUE
	PRINT*,'END OF ZTABLE'
	STOP
	END