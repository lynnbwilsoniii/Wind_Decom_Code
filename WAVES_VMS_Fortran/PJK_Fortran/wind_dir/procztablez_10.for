	PROGRAM PROCZTABLE
C
C	A SPECIAL VERSION FOR THE Z MEASUREMENTS
c		not done yet
C	MAKES VARIOUS PLOTS OF RESULTS OF ANTENNA RESISTANCE MEASUREMENTS
C		AND LEAST SQUARES FITS
C	WRITE FOR067.DAT = CALCULATED ANTENNA RESISTANCE, FOR PLOTTING
C	AND FOR062.DAT FOR Z ANTENNA
C	WITH ZTABLE.MGO OR ZTABLE3.MGO, OR ZEZ.MGO (IN [.PAPERS]) FOR Z
C
	COMMON /FUDGE/ FFACT,TPHI
C
	CHARACTER*120 JUNK
	INTEGER*4 YYYYMMDD,TS,TEND
	REAL XX(1000),YY(1000),BB(1000),FF(1000),DD(1000),SSTX(1000)
	REAL SSTY(1000),SIGDD(1000),TTEE(1000),RZZ(1000),FFZ(1000)
	REAL FINV(1000),WT(1000)
	REAL WINDLEN(3),WINDDIA(3),WINDEFF(3)
C
C	DATA PHYSLEN,DIA,TPHI /5000.,.038,.9/
C	DATA PHYSLEN,DIA,TPHI /600., 2., .9/
C	DATA PHYSLEN,DIA,TPHI /1000.,1.,.9/
C	DATA PHYSLEN,DIA,TPHI /1000.,2.8,.9/
	DATA WT /1000*1./
	DATA WINDEFF /4110.,379.,217./   	! NOT WINDLIB VALUES
	DATA WINDLEN /5000.,750.,750./
	DATA WINDDIA /.038,.038,2.8/		! Z NOT CERTAIN
	DATA FFACT /.9/	    !FUDGE FACTOR TO GIVE PHOTEMITTING AREA
C
c	OPEN(UNIT=89,FILE='ZTABLE.DAT;46',STATUS='OLD',READONLY)
c	OPEN(UNIT=89,FILE='[.PAPERS]ZTABLE.DAT;57',STATUS='OLD',READONLY) !xy
	OPEN(UNIT=89,FILE='[.PAPERS]ZTABLE.DAT',STATUS='OLD',READONLY) ! EZ
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
	READ(89,*,END=200,ERR=201) YYYYMMDD,TS,TEND,XDC,XPK,YDC,YPK,RZ,
     1   	SXDC,SXPK,SYDC,SYPK,STZ,ANGLE,DENS,SDENS,TE,STE,TI,STI,
     2		FLUX,RBASE,RX,RY
C
	ITOT = ITOT+1
C	print*,yyyymmdd,sxdc,sydc,flux
C
	IF(YYYYMMDD.EQ.20000330) GO TO 100
	CALL FLUX_3DP(YYYYMMDD,CNT3DP,FLUX3DP,FSTD3DP)
	FLUX = FLUX3DP
	FSTD = FSTD3DP
C
C	FIT RX AND RY DATA
C
C	IF(SXDC.LE..3.AND.SYDC.LE..3.AND.FSTD.LT..2) THEN
	IF(FSTD.LT..2) THEN
	  IS = IS+1
	  XX(IS) = RX
	  SSTX(IS) = SXDC
	  YY(IS) = RY	  
	  SSTY(IS) = SYDC
	  BB(IS) = RBASE
	  FF(IS) = FLUX
	  FINV(IS) = 1./FLUX
	  DD(IS) = DENS
  	  SIGDD(IS) = SDENS
	  TTEE(IS) = TE
C	MAKE TABLE FOR PLOTTING
	WRITE(37,1037) YYYYMMDD,RBASE,RX,RY,FLUX,FSTD3DP,DENS,SDENS
 1037	FORMAT(I10,4F9.2,F6.3,F9.2,F6.3)
	ELSE
	  print*,'xy check',yyyymmdd,sxdc,sydc,fstd
	ENDIF
	IF(STZ.LE.2.35.AND.FSTD.LT..2) THEN
	  ISZ = ISZ+1
	  RZZ(ISZ) = RZ
	  FFZ(ISZ) = FLUX
C	  DD(ISZ) = DENS
C  	  SIGDD(ISZ) = SDENS
C	  TTEE(ISZ) = TE
	WRITE(44,1044) YYYYMMDD,RZ,STZ,FLUX,FSTD3DP,DENS,SDENS
 1044	FORMAT(I10,F9.2,F6.3,F9.2,F6.3,F9.2,F6.3)
	ENDIF
C
C	CALCULATE RBASE, ETC. FOR UMBRA
C
c
c	this calculation assumes equal X and Y base resistances,
c	  measured R = base R and ant R in parallel, and R prop. to 1/L
c
	  IF(XPK.NE.0..AND.YPK.NE.0.) THEN
	    YXT = 1./XPK		! x total admittance
	    YXT = YXT - 1./1000.	! subtract preamp admittance
	    YYT = 1./YPK		! y total admittance
	    YYT = YYT - 1./10000.	! subtract preamp admittance
C	    solve for base and antenna resistances
	    ALPHA = (YXT-YYT)/42.5	! 42.5 = 50 m - 7.5 m
	    YB = YYT - 7.5*ALPHA	! base admittance
	    IF(YB.NE.0.) baseR = 1./YB
	    XR = 1./(50.*ALPHA)
	    YR = 1./(7.5*ALPHA)
	  ENDIF
	WRITE(38,1038) YYYYMMDD,BASER,XR,YR,FLUX,FSTD3DP,DENS,SDENS
 1038	FORMAT(I10,3F9.2,F9.2,F7.3,F9.2,F7.3)
C
	GO TO 100
 201	PRINT*,'ERROR IN READ DATA,YYYYMMDD=',YYYYMMDD
	GO TO 100
 200	CONTINUE
	CLOSE(UNIT=89)
C
C	MAKE FIT
C
	PRINT*,'TOTAL POINTS',ITOT,' GOOD POINTS',IS
	PRINT*,'TOTAL POINTS',ITOT,' GOOD Z POINTS',ISZ
C	CALL PLOTZTABLE
	DO N = 1,30
	  SUMSQ = 0.
	  TPHI = .3 + N/20.
	  DO I = 1,IS	
	    PHYSLEN = WINDLEN(1)
	    DIA = WINDDIA(1)
	    DENSE =  DD(I)
	    TE = TTEE(I)
	    CALL ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
	    SSTXT = AMAX1(SSTX(I),.005)
	    ERR = (ANTRES - 1.E6*XX(I))/(ANTRES + 1.E6*XX(I))/SSTXT
	    SUMSQ = SUMSQ + ERR**2
	  ENDDO
	   write(75,*) 'x',TPHI,antres,1.e6*xx(i),err,sumsq
C
	  DO I = 1,IS	
	    PHYSLEN = WINDLEN(2)
	    DIA = WINDDIA(2)
	    CALL ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
	    ERR = (ANTRES - 1.E6*YY(I))/(ANTRES + 1.E6*YY(I))/SSTY(I)
	    SUMSQ = SUMSQ + ERR**2
	  ENDDO
	  WRITE(99,*) TPHI,SUMSQ
	ENDDO
C
	PRINT*,'IS,YYYYMMDD,RX,RY',IS,YYYYMMDD,RX,RY
	ICS = 1
C	DO N = 1,100
C	  DENSE = 2. + (N-1)*.5
C	  FLUX = DENSE*SQRT(TE)
C	  CALL ANTENNA(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
C     1		ANTRES,ANTCAP)
C	  WRITE(67,*) DENSE,FLUX,ANTCURR,1.E-6*ANTRES	  
C	ENDDO
C
C	MAKE CURVES FOR PLOTTING
C
	TPHI = 1.3
	PRINT*,'FOR FOR067 AND 68,TPHI=',TPHI
	DO N = 1,200
	  DENSE = .5 + (N-1)*.25
	  FLUX = DENSE*SQRT(TE)
	  PHYSLEN = WINDLEN(1)
	  DIA = WINDDIA(1)
	  CALL ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
	  WRITE(67,*) DENSE,FLUX,ANTCURR,1.E-6*ANTRES,ANTPOT	  
C
	  PHYSLEN = WINDLEN(2)
	  DIA = WINDDIA(2)
	  CALL ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
	  WRITE(68,*) DENSE,FLUX,ANTCURR,1.E-6*ANTRES,ANTPOT	  
C
	  PHYSLEN = WINDLEN(3)
	  DIA = WINDDIA(3)
	  CALL ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
	  WRITE(69,*) DENSE,FLUX,ANTCURR,1.E-6*ANTRES,ANTPOT	  
	ENDDO
C
C	FIT BASE RESISTANCE DATA TO Rbase = A + B/FLUX
C
C	do ii = 1,is
C	  print*,'ls',ii,bb(ii),finv(ii)
C	enddo
C	CALL LSFIT(BB,FINV,WT,IS,BBF,RBASE0,FRMS)
C	PRINT*,'BBF,RBAS0',BBF,RBASE0
C
C	RESULTS FROM XY RUNS AND PAPER
C
	AXYSUN = 42.7
	AXYSHD = 110.
	BXYSUN = 184.
	BXYSHD = 654.
	AZSUN = AXYSUN*(.38/28.)
	AZSHD = AXYSHD*(.38/28.)
	BZSUN = BXYSUN*(.38/28.)
	BZSHD = BXYSHD*(.38/28.)
C
C	MAKE CURVES FOR PLOTTING
C
	DO N = 1,200
	  DENSE = .5 + (N-1)*.25
	  FLUX = DENSE*SQRT(TE)
C	  REQ = RBASE0 + BBF/FLUX
	  RBSUN = AZSUN + BZSUN/FLUX
	  RBSHD = AZSHD + BZSHD/FLUX
C
	  PHYSLEN = WINDLEN(3)
	  DIA = WINDDIA(3)
	  CALL ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
	  REQSUN = 1./(1./ANTRES + 1./RBSUN)
	  REQSHD = 1./(1./ANTRES + 1./RBSHD)
	  WRITE(62,*) N,REQSUN,REQSHD,FLUX,ANTRES
	ENDDO
	STOP
	END
	SUBROUTINE ANTENNAS(PHYSLEN,DIA,ICS,DENSE,TE,TPHI,ANTCURR,
     1		ANTPOT,ANTRES,ANTCAP)
C
	DATA PI /3.14159265/
	DATA EVME /.51E6/
	DATA QE /4.8E-10/            ! ELECTRON CHARGE IN ESU
	DATA QESI /1.60206E-19/        ! ELECTRON CHARGE IN SI
	DATA EOMSI /-1.75882E11/        ! E OVER M FOR ELECTRONS, SI UNITS
	DATA BOLTZK /1.38044E-23/       ! BOLTZMANN CONST IN SI
	DATA AION /1./                  ! ION MASS in AMU
	DATA CLIGHT /2.9979E10/
C
C	ICS = 1 IS CYLINDER, 2 IS SPHERE
C
	VMTE = CLIGHT*SQRT(8.*TE/PI/EVME) 
C	TPHI1 = TPHI			    ! PHOTOELECTRON TEMP IN EV
	EAREA = PI*DIA*PHYSLEN
	SHADOWL = 36.			! LENGTH OF S/C SHADOW
	PAREA = DIA*(PHYSLEN - SHADOWL)*SIND(45.)
	FLUX = .25*DENSE*EAREA*VMTE       ! IN ELECTRONS/SEC, INDEP OF UNITS
	ANTCURR = FLUX*QESI
C
C	ABOVE IN CGS, NOW DO IN MKS
C
C	  ANTENNA CAPACITANCE AND RESISTANCE
C
	CALL CALCARES(ANTCURR,EAREA,PAREA,ANTPOT,ANTRES)
	ANTCAP = (.241E-12)*PHYSLEN/(ALOG10(2.*PHYSLEN/DIA) - .4)
C
C	PRINT*,'PHYSLEN,DIA',PHYSLEN,DIA
C	PRINT*,'L,CURR,RES,CAP',PHYSLEN,ANTCURR,ANTRES,ANTCAP
	REFF = ANTRES
C	REFF = 1./(1./RINPUT + 1./ANTRES)
C	CEFF = CINPUT + ANTCAP
C
	VSN2 = FLUX*(QESI*REFF)**2                      ! RMS
	EFFSI = PHYSLEN/100.
	ESN2 = VSN2/EFFSI**2
C
	RATIO = 1.15
	FREQ = 1./30./RATIO
C	DO NF = 1,100
C	  FREQ = FREQ*RATIO
C	  W = 2.*PI*FREQ
C	  SHOTNOISE =  ESN2/(1. + (W*REFF*CEFF)**2)
C	  PRINT*,'SHOT NOISE, V/M**2/HZ',ESN2
C	  IF(IANT.EQ.2) WRITE(66,*) FREQ,10.*ALOG10(SHOTNOISE(NF,1)),
C     1		10.*ALOG10(SHOTNOISE(NF,2))			!dB
C	ENDDO
C
	RETURN
	END
	SUBROUTINE CALCARES(ANTCURR,EAREA,PAREA,ANTPOT,ANTRES)
C
C	CALCULATES ANTENNA POTENTIAL AND RESISTANCE, TO BALANCE 
C		PHOTOEMISSION AGAINST ANTCURR
C
	COMMON /FUDGE/ FFACT,TPHI
C
	ANTPOT = 0.
C	print*,'antcurr',antcurr
C	CALL SCUDDER(ANTPOT,CURDENS)
	CALL HINTER(TPHI,ANTPOT,CURDENS)
C	CALL CUBE(ANTPOT,CURDENS)
	PCUR = FFACT*1.E-4*PAREA*CURDENS	
	DIFFSV = PCUR - ANTCURR
	DELTAV = .1
C
	DO N = 1,500
	  ANTPOT = DELTAV*N
C	  CALL SCUDDER(ANTPOT,CURDENS)
	  CALL HINTER(TPHI,ANTPOT,CURDENS)
C	  CALL CUBE(ANTPOT,CURDENS)
	  PCUR = FFACT*1.E-4*PAREA*CURDENS	
	  DIFF = PCUR - ANTCURR
C	print*,'antpot,pcur,diff',antpot,pcur,diff
	  IF(DIFF*DIFFSV.LE.0.) THEN		!ROOT FOUND
	    DIDV = (DIFF-DIFFSV)/DELTAV
	    ANTPOTT = ANTPOT 
	    IF(DIDV.NE.0) ANTRES = -1./DIDV
	    ANTPOTT = ANTPOT + DIFF*ANTRES
C	print*,'antpot,pcur,didv,antres',antpot,pcur,didv,antres
	    CALL HINTER(TPHI,ANTPOTT,CURDENS1)
	    DELTAVT = .001*PCUR*ANTRES
	    CALL HINTER(TPHI,ANTPOTT+DELTAVT,CURDENS2)
	    DIDV = 1.E-4*PAREA*(CURDENS2-CURDENS1)/DELTAVT	
	    IF(DIDV.NE.0) ANTRES = -1./DIDV
	    ANTPOT = ANTPOTT
	    RETURN
	  ENDIF
	  DIFFSV = DIFF
	ENDDO
	RETURN
	END
	SUBROUTINE HINTER(TPHI,ANTPOT,CURDENS)
C
C	FROM HINTEREGGER, DAMON AND HALL, JGR 64, 961, 1959
C
c	DATA TPHI /.9/		! HINTEREGGER GAVE ABOUT 1 EV
C	DATA TPHI /1.4/		! BEST FIT TO WIND DATA
C
	CURDENS = 4.E-5*EXP(-ANTPOT/TPHI)		! IN AMP/M**2
	RETURN
	END
	SUBROUTINE CUBE(ANTPOT,CURDENS)
C
C	AN ATTEMPT TO MATCH ULYSSES AND WIND MEASUREMENTS, WHICH
C		SHOW LOWER RESISTANCE AT LOW ELECTRON FLUX, AND VV.
C
	DATA TPHI /2./		! HINTEREGGER GAVE ABOUT 1 EV
C
	CURDENS = 2.E-6*EXP(-(ANTPOT/TPHI)**2)		! IN AMP/M**2
	RETURN
	END
	SUBROUTINE PLOTZTABLE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
C
	COMMON /FUDGE/ FFACT,TPHI
C
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	CHARACTER*50 COM(20)
	INTEGER  YYYYMMDD(200)
	REAL RX(1),RY(1),FLUX(1),DENS(1),STDRX(1)
	REAL STDRT(1),STDDDENS(1),TE(1)
	REAL ZTAB(22,200)
C
	  ITERM = 3
	  ITERM = -1
	  ICOM = 0
C
	OPEN(UNIT=89,FILE='[.PAPERS]ZTABLE.DAT',STATUS='OLD',READONLY)
	READ(89,1000) JUNK
	print*,junk
	READ(89,1000) JUNK
	print*,junk
 1000 	FORMAT(A)
	IS = 0
C
 100	CONTINUE
C	READ(89,*,END=200,ERR=201) YYYYMMDD,TS,TEND,XDC,XPK,YDC,YPK,SXDC,SXPK,
C     1   	SYDC,SYPK,ANGLE,DENS,SDENS,TE,STE,TI,STI,FLUX,
C     2		RBASE,RX,RY
c
	IS = IS+1
	READ(89,*,END=200,ERR=201) YYYYMMDD(IS),(ZTAB(I,IS),I=2,22)
	GO TO 100
 201	PRINT*,'ERROR IN SUBROUTINE PLOTZTABLE READIN AT IS=',IS
	PRINT*,YYYYMMDD(IS),ZTAB(2,IS),ZTAB(21,IS),ZTAB(22,IS)
	GO TO 100
C
 200	CONTINUE	
C
	  IS = IS-1
	  ICOM = ICOM+1
	  COM(ICOM) = 'TERMINAL 3'
	  IF(ITERM.LT.0) COM(ICOM) = 'PRINTER 1'
C
C	  WRITE(COM(2),1001) NW
C 1001	  FORMAT('WINDOW 2 2 ',I2)
	  ICOM = ICOM+1
	  COM(ICOM) = 'WINDOW 1 3 3'
	  ICOM = ICOM+1
	  COM(ICOM) = 'limits  0.  30.   0.  500.'
	  ICOM = ICOM+1
	  COM(ICOM) = 'RELOCATE 6. 450.'
C	  ICOM = ICOM+1
C	  COM(ICOM) = 'EXPAND .8'
	  ICOM = ICOM+1
 	  WRITE(COM(ICOM),1020) YYYYMMDD(1),YYYYMMDD(IS)
 1020	  FORMAT('LABEL YYYYMMDD',I10,'  TO',I10)
C	  ICOM = ICOM+1
C	  COM(ICOM) = 'EXPAND 1.'
	  ICOM = ICOM+1
	  COM(ICOM) = 'XCOLUMN 19'
	  ICOM = ICOM+1
	  COM(ICOM) = 'YCOLUMN 21'
	  ICOM = ICOM+1
	  COM(ICOM) = 'ptype 5 0'
	  ICOM = ICOM+1
	  COM(ICOM) = 'points'
	  ICOM = ICOM+1
	  COM(ICOM) = 'box'
	  ICOM = ICOM+1
	  COM(ICOM) = 'xlabel electron flux'
	  ICOM = ICOM+1
	  COM(ICOM) = 'ylabel RX M\gW'
	  ICOM = ICOM+1
	  COM(ICOM) = 'id [.WIND]PROCZTABLE'
	  IF(ITERM.LT.0) THEN
	    ICOM = ICOM+1
	    COM(ICOM) = 'HARDCOPY'
	  ENDIF
	  ICOM = ICOM+1
	  COM(ICOM) = 'end'
	  CALL MONGO(ICOM,COM,IS,22,ZTAB)
C
	  RETURN
	  END
