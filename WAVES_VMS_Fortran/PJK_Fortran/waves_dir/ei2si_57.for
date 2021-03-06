      PROGRAM EI2SI
C
C	THIS ROUTINE PLOTS OR PRINTS THE DISPERSION RELATION FOR
C	THE ELECTRON-ION TWO-ROTATING-STREAM INSTABILITY, DAVIDSON
C	TNNP SECT 2.9.3, EQN. 2.9.13  (LHS-RHS) WITH KZ = 0, L = 1.
C	FREQUENCIES ARE RATIOS TO ION PLASMA FREQUENCY.
C
C	LINK WITH PLTLIB,FLILO, OR FAKEPL IF DESIRED
C
      COMMON /BEAMBL/ XD(100),XNUA2(10),WALF(10),WC(10),WP2(10),NC,RPT2
     1  ,WP(10),WT,WCE,RP,RC,WEP,WEN,WIP,WIN
      COMMON /POLEBK/ WPOLE(20),NPOLE
      COMPLEX W,XNUA2,DEQ,DEQ1,W1,DW,RW,WROOT(20),RPT2,CRECIP,SUM 
      COMPLEX Z,ZS,RJLZ,RJNZ,DUM1,DUM2,RILX,RKLX,RILS,RKLS,CSURA
	BYTE STRING(132),TITL(4)
	LOGICAL*1 TIM(8),DAT(9)
	LOGICAL ITITL(8)
	EXTERNAL BEAMWK
      DIMENSION RME(10),DENS(10),SWALF(10),BETAZ(10),WCS(20)
      DIMENSION XX(800),YR(800),YI(800)
      DIMENSION NDEQP(5),IXDATA(2)
C	EQUIVALENCE (XD(1),XKZ),(XD(2),WCE),(XD(3),RP),(XD(4),RC)
	EQUIVALENCE (XD(1),XKZ),(XD(2),FHZCE),(XD(10),FHZPE)
	EQUIVALENCE (XD(5),WR),(XD(6),WI),(XD(7),RWR)
	EQUIVALENCE (XD(8),AZL),(XD(9),XNC)
	EQUIVALENCE (XD(11),RME(1)),(XD(21),DENS(1)),(XD(31),BETAZ(1))
	EQUIVALENCE (XD(41),SWALF(1))
     	EQUIVALENCE (XD(54),BBX),(XD(55),BBY),(XD(56),XSOLV)
	EQUIVALENCE (XD(59),BEAMD)
	EQUIVALENCE (XD(71),WROOT(1))
	EQUIVALENCE (XD(69),TERMNL)
      DATA ITITL /'MISC','MASS','DENS',' VZ ','SIGN',2*'MISC','ROOT'/
      DATA NC /2/ 
      DATA AZL /1./ 
      DATA XKZ /0./ 
      DATA XD(3),XD(4) /1.,40./
      DATA WR,WI /.00023,.00/
      DATA RWR /1.01/
      DATA WCE /50.E6/ 
      DATA RC /40./
      DATA RME /-1.,51400.,8*1. / 
      DATA DENS /1.,.95,8*1. / 
      DATA SWALF /-1.,1.,-1.,1.,6*1./ 
      DATA BETAZ /3*0.,.0198,6*0./              ! 100 eV
      DATA BBX,BBY /.001,.1/
      DATA FHZPE /5.4E6/
      DATA FHZCE /25.E6/
      DATA XD(52), XD(53) /-10., 10./
      DATA XD(57) /4./
C     XD(58) IS NPP = NO. OF POINTS TO BE PLOTTED
      DATA XD(58) /100./
      DATA XSOLV /0./
C      DATA BEAMD /.363/
      DATA BEAMD /.156/
      DATA NDEQP /3*512,32,768/
      DATA XD(67) /0./
      DATA TERMNL /1./
      DATA XD(66) /1./
      DATA XD(71) /-50.7/
      DATA XD(73) /504./
      DATA XD(76) /.001/
      DATA XD(78) /-.001/
      DATA TWOPI /6.2831859/
C
C	
C       XSOLV = 0 IS PLOT AND PRINT DEQ FOR W REAL
C                   XD(52),(53) ARE MIN AND MAX FREQS TO PLOT
C                   XD(58) IS NUMBER OF POINTS
C	            XD(67) = 1.IS PRINT,   =0.  IS PLOT
C             = 1 IS FIND ONE VALUE OF DEQ AND PRINT ON SCREEN
C                   XD(5),(6) ARE REAL AND IMAG PARTS OF FREQ.
C             = 2 IS FIND ONE ROOT
C                   XD(5),(6) ARE STARTING VALUES 
C             = 3 IS FIND SEVERAL ROOTS (NO. OF ROOTS = XD(57))
C             = 4 IS PLOT RADIAL POTENTIAL FUNCTION
C	XSOLV = XD(56)
C
	CALL DATE(DAT)
	CALL TIME(TIM)
	ENCODE(132,104,STRING) DAT,TIM
	CALL PLTPRT(STRING)
  104   FORMAT('0 PROGRAM EI2SI',5X,9A1,5X,8A1)
        ENCODE(132,105,STRING)
  105   FORMAT(' INPUT VARIABLES')
	CALL PLTPRT(STRING)
	DO 106 JJ = 1,7
	KK = 10*JJ-9
        ENCODE(132,1016,STRING) KK,ITITL(JJ),(XD(KK+N),N=0,9)
	CALL PLTPRT(STRING)
  106   CONTINUE
 1016   FORMAT(I5,2X,A4,10E12.4)
	NROOT = XD(57)+.5
        XNC = NC
 200	WRITE (6,701)
 701	FORMAT(' STEP 1, MANUAL CHOICE OF INITIAL PARAMETERS')
 101	WRITE (6,702)
 702	FORMAT(' TYPE NO. AND NEW VALUE OF VARIABLE TO BE CHANGED, 0,0  
     1FOR NO CHANGE ')
C	READ (5,501) ND,XTT
	READ (5,*) ND,XTT
 501	FORMAT(I5,F15.5)
	IF(ND.EQ.0) GO TO 102
	ENCODE(132,502,STRING) ND,XD(ND),XTT
 502	FORMAT(' CHANGE VARIABLE NO.',I3,' FROM',F15.5' TO',F15.5)
	CALL PLTPRT(STRING)
	WRITE(6,502) ND,XD(ND),XTT
	XD(ND) = XTT
	GO TO 101
 102	NC = XNC + .5
	ITER = TERMNL + .5
C
C     CALCULATE RC AND RP
      XD(3) = BEAMD*BETAZ(4)*3.E10/FHZCE/TWOPI
      RP = XD(3)*FHZPE*TWOPI/3.E10
      RC = XD(4)*FHZPE*TWOPI/3.E10
      ENCODE(132,1023,STRING) XD(3),XD(4)
 1023 FORMAT(' RP ='F8.2' CM,  RC='F8.2' CM')
	CALL PLTPRT(STRING)
	PRINT 1023,XD(3),XD(4)
C
C     CALCULATE PLASMA AND CYCLOTRON FREQS. 
      ENCODE(132,1003,STRING)
 1003 FORMAT(3X,'NO.',5X,'DENS',7X,'BETAZ',9X,
     1  'WP',10X,'WC',7X,'W ALPHA',6X,'W A/WC') 
	CALL PLTPRT(STRING)
      RTRM = SQRT(ABS(RME(2)))
      WCE = XD(2)/XD(10)
      DO 10 N = 1,NC  
      WC(N) = RTRM*WCE/RME(N)
      WP2(N) = ABS(RME(2))*DENS(N)/ABS(RME(N))
      WP(N) = SQRT(ABS(WP2(N))) 
   10 CONTINUE
      IROOT = 0
  201 CONTINUE
C 
C     CALCULATE WALF (TNP EQ. 2.7.3) AND POLES
C     M IS ETA, N IS ALPHA
C
      DO 20 N = 1,NC  
      ST = 0. 
      DO 21 M = 1,NC  
      ST = ST + SIGN(1.,RME(M))*DENS(M)/WC(N)**2/RME(N) 
   21 CONTINUE
C	PRINT 1002,N,ST
      STT = SQRT(1. - 2.*ST)
      WALF(N) = -.5*WC(N)*(1. + STT*SWALF(N)) 
      WAWC = WALF(N)/WC(N)
	NEN = N
      IF(IROOT.EQ.0) ENCODE(132,1002,STRING)
     1 NEN,DENS(N),BETAZ(N),WP(N),WC(N),WALF(N),WAWC
      IF(IROOT.EQ.0) CALL PLTPRT(STRING)
C      IF(IROOT.EQ.0) 
C    1 PRINT 1002,N,DENS(N),BETAZ(N),WP(N),WC(N),WALF(N),WAWC
      WCS(2*N-1) = ABS(WC(N))
      WCS(2*N)   = ABS(WC(N))
   20 CONTINUE
	F = DENS(2)/DENS(1)
	ENCODE(132,1025,STRING) F,WCE
 1025	FORMAT(' f = ',F7.3,',   Wce/Wpe =',F7.3)
	CALL PLTPRT(STRING)
	FR2I = 2.*RME(2)/WCE**2
	FR2E = 2./WCE**2
	WEP = .5*WCE*RTRM*(1.+SQRT(1.- FR2E*(1.-F)))
	WEN = .5*WCE*RTRM*(1.-SQRT(1.- FR2E*(1.-F)))
	WIP = -.5*WCE*(1.+SQRT(1.+ FR2I*(1.-F)))/RTRM
	WIN = -.5*WCE*(1.-SQRT(1.+ FR2I*(1.-F)))/RTRM
 	WPOLE(1) = WEP
 	WPOLE(2) = WEN
 	WPOLE(3) = WIP
 	WPOLE(4) = WIN
 1002 FORMAT(I5,6E12.4)
      IF(IROOT.EQ.0) ENCODE(132,1012,STRING) XKZ
      IF(IROOT.EQ.0) CALL PLTPRT(STRING)
 1012 FORMAT(' KZ*C/WP =',E13.5)
       IF(IROOT.EQ.0) ENCODE(132,1014,STRING)
       IF(IROOT.EQ.0) CALL PLTPRT(STRING)
 1014 FORMAT(' POLES, Wep,Wen,Wip,Win')
      NPOLE = 2*NC
      IF(IROOT.EQ.0) ENCODE(132,1013,STRING) (WPOLE(N),N=1,NPOLE)
      IF(IROOT.EQ.0) CALL PLTPRT(STRING)
C     ORDER POLES IN ORDER OF INCREASING W
C     ROUGH AND TEMPORARY ESTIMATE OF ROOTS
C     THESE ESTIMATES ARE USED ON FIRST PASS ONLY
      IF(IROOT.NE.0) GO TO 25
      IF(XD(66).GT..5) GO TO 25
      DO 24 N = 2,NPOLE
      WT = .5*(WPOLE(N)+WPOLE(N-1))
      WROOT(N) = CMPLX(WT,.01*WT)
   24 CONTINUE
      WROOT(1) = 2.*WPOLE(1)
      WROOT(NPOLE) = 1.1*WPOLE(NPOLE)
   25 IROOT = IROOT + 1
 1013 FORMAT(8E16.7)
C
      IF(XSOLV.GT..5) GO TO 60
C
C	PLOT AND PRINT DEQ V.S. WR FOR WI = XD(6)
C
      DO 3 JJ = 1,7
      KK = 10*JJ-9
      ENCODE(132,1016,STRING) KK,ITITL(JJ),(XD(KK+N),N=0,9)
      CALL PLTPRT(STRING)
    3 CONTINUE
      NPP = XD(58)+.5
      WS = XD(52)
      CMIN = FLILO(XD(52),BBX)
      CMAX = (FLILO(XD(53),BBX) - CMIN)/MAX0(NPP,1)
      SUM = 0.
      DEQ1 = 0.
      DO 1 JJ=0,NPP
      WTT=CMIN+CMAX*JJ
      WT = AFLILO(WTT,BBX)
      W =CMPLX(WT,WI)
      CALL BEAMWK(W,DEQ,DMAG)
      JEN = JJ
      XX(JEN) = WT
      YR(JEN) = DEQ
      YI(JEN) = AIMAG(DEQ)
      ENCODE(132,1017,STRING) JEN,W,DEQ,DMAG,RPT2
      IF(XD(67).GT..5)	CALL PLTPRT(STRING)
C      CALL PLTPRT(STRING)
      IF(XD(67).GT..5) WRITE(6,1020) JJ,W,DEQ,DMAG
C      WRITE(3,1020,IERR = 121) JJ,W,DEQ,DMAG,RPT2
 1017 FORMAT(I6,' W='2E13.5'  DEQ='2E13.5'  DMAG='E11.3,
     1 '  RPT2=',2E10.2)
 1020 FORMAT(I6,' W='E12.4,E10.2,'  DEQ='E12.4,E10.2,' DMAG=',E10.2)
C     CALCULATE INTEGRAL
  121 JK = JJ.AND.1
      SIMFAC = JK + 1
      SUM = SUM + SIMFAC*(DEQ-DEQ1)/(DEQ+DEQ1)
      DEQ1 = DEQ
    1 CONTINUE
      CALL LILOPLOT(JEN,ITER,BBX,BBY,XX,YR,YI)
      ENCODE(132,1018,STRING)
 1018 FORMAT('1')
      CALL PLTPRT(STRING)
C     NORMALIZE INTEGRAL
      SUM = (4./3.)*SUM/TWOPI
      ENCODE(132,1019,STRING) SUM
	CALL PLTPRT(STRING)
      WRITE(7,1019) SUM
 1019 FORMAT(' NO. OF GROWING ROOTS IS IMAG. PART OF: ',2E13.5)
      ENCODE(120,1018,STRING)
	CALL PLTPRT(STRING)
C      IF(XD(67).LT..5) CALL PLTPRT(STRING)
      GO TO 200
C
   60 IF(XSOLV.GT.1.5) GO TO 70
C
C     EVALUATE DEQ FOR ONE VALUE OF W
C
      W = CMPLX(WR,WI)
      CALL BEAMWK(W,DEQ,DMAG)
      WRITE(6,1006) XKZ,W,DEQ
 1022 FORMAT(' (W-KV)/WC',E15.7)
      ENCODE(132,1006,STRING) XKZ,W,DEQ
      CALL PLTPRT(STRING)
C      PRINT 1006,XKZ,W,DEQ
      GO TO 200
C
   70 IF(XSOLV.GT.2.5) GO TO 40
C
C     LOOK FOR A ROOT OF EQ.2.7.15
C
      NPP = XD(58) + .5
      W=CMPLX(WR,WI)
      RW=CMPLX(RWR,0.)
      CALL BEAMWK(W,DEQ,DMAG)
      W1=W
      DEQ1=DEQ
   32 W=W*RW
      CALL BEAMWK(W,DEQ,DMAG)
      IF(CABS(DEQ-DEQ1).EQ.0.) GO TO 32
      DO 30 I=1,20
      DW=-DEQ*(W-W1)*CRECIP(DEQ-DEQ1)
      W1=W
      DEQ1=DEQ
      W=W+DW
      CALL BEAMWK(W,DEQ,DMAG)
      TEST = ABS(DEQ-DEQ1) + ABS(AIMAG(DEQ-DEQ1))
      IF(TEST.EQ.0.) GO TO 31
C 1008 FORMAT('  W1',2E15.7,'  DEQ1',2E15.7,'  DW',2E15.7)
      TEST = ABS(DEQ) + ABS(AIMAG(DEQ))
      IF(TEST.LT.3.E-7*DMAG) GO TO 31
   30 CONTINUE
   31 CONTINUE
      FHZ = W*FHZPE
      XKZCM = XKZ*TWOPI*FHZPE/3.E10
      ENCODE(132,1006,STRING) XKZ,W,DEQ,DMAG,FHZ,XKZCM
	CALL PLTPRT(STRING)
 1006 FORMAT(' KZ'F10.4'  W',2E15.7,'  DEQ',2E11.4,'  DMAG',E11.4
     1  ,'  F(HZ)',E11.4,'  KZ(CM)',E11.4)
      WRITE(6,1006)  XKZ,W,DEQ
      WR = W
      WI = AIMAG(W)
      XKZ = XD(52)+IROOT*(XD(53)-XD(52))/MAX0(NPP,1)
      IF(IROOT.GT.NPP) GO TO 200
      GO TO 201
C
 40   IF(XSOLV.GT.3.5) GO TO 50
C
C	TRY TO FIND SEVERAL ROOTS
C
      NROOT = XD(57)+.5
      CALL ROOTS(NROOT,BEAMWK,WROOT,NBAD,IBAD)
      GO TO 200
C
 50   IF(XSOLV.GT.4.5) GO TO 80
C
C      PLOT RADIAL POTENTIAL FUNCTION
C      CALL CLOSEF (1)
	NDEQP(4) = 32
   80 CONTINUE
      IF(XD(56).GT.4.5) STOP
	GO TO 200
      END 
      SUBROUTINE BEAMWK(W,DEQ,DMAG)
C
      COMMON /BEAMBL/ XD(100),XNUA2(10),WALF(10),WC(10),WP2(10),NC,RPT2  
     1  ,WP(10),WT,WCE,RP,RC,WEP,WEN,WIP,WIN
      COMMON /POLEBK/ WPOLE(20),NPOLE
	COMPLEX W,DEQ,DEN1,DEN2,CRECIP,XNUA2,RPT2
	EQUIVALENCE (XD(5),WR),(XD(6),WI),(XD(7),RW)
	EQUIVALENCE (XD(8),AZL),(XD(9),XNC)
C
      DMAG = 1./(1.-(RP/RC)**2)
      DEQ = CMPLX(DMAG,0.)
      DEN1 = 2.*(W-WEP)*(W-WEN)
      DMT = WP2(1)/CABS(DEN1)
      DMAG = AMAX1(DMAG,DMT)
C      IF(DEN1.NE.0.) DEQ = DEQ - WP2(1)/DEN1
      DEQ = DEQ - WP2(1)/DEN1
      DEN2 = 2.*(W-WIP)*(W-WIN)
      DMT = WP2(2)/CABS(DEN2)
      DMAG = AMAX1(DMAG,DMT)
C      IF(DEN2.NE.0.) DEQ = DEQ - WP2(2)/DEN2
      DEQ = DEQ - WP2(2)/DEN2
	RETURN
	END
      SUBROUTINE ROOTS(NR,FUNCT,XK,NBAD,IBAD) 
C     THIS ROUTINE FINDS NR ROOTS OF THE FUNCTION FUNCT 
C     FUNCT IS CALCULATED BY A SUBROUTINE WHICH ALSO RETURNS  
C     AN ESTIMATE OF ITS MAGNITUDE, EMAG  
      COMPLEX EPS,XK,XKT,EPSR,XKL,DER,EPS0,CRECIP
      BYTE STRING(132)
      EXTERNAL FUNCT  
      DIMENSION NRS(20),XK(20)
      DATA ERR /6.E-9/
      DATA RK /.99/ 
      IBAD = 0
      NBAD = 0
C     FIND NR ROOTS 
      DO 1 N = 1,NR 
      XKL = XK(N)*RK  
      CALL FUNCT(XKL,EPSR,EMAG) 
      XKT=XKL/RK
      IF(CABS(XKT).EQ.0.) XKT = .1*XK(N+1)
      IF(CABS(XKT).EQ.0.) XKT = 1.E-6
C     REMOVE ROOTS ALREAD FOUND 
      N1 = N - 1
      IF(N1.EQ.0) GO TO 3 
      DO 4 I = 1,N1 
      EPSR = EPSR*CRECIP(XKT - XK(I)) 
      EMAG = EMAG/CABS(XKT - XK(I)) 
    4 CONTINUE
    3 CONTINUE
C     ITERATE TO FIND ROOTS 
      DO 2 J = 1,20 
      NRS(N) = J
      CALL FUNCT(XKT,EPS,EMAG)
C     REMOVE ROOTS ALREADY FOUND
      IF(N1.EQ.0) GO TO 5 
      DO 6 I = 1,N1 
      EPS = EPS*CRECIP(XKT-XK(I)) 
      EMAG = EMAG/CABS(XKT-XK(I)) 
    6 CONTINUE
    5 IF(CABS(XKT-XKL).GT.1.0E-15) GO TO 20
	ENCODE(132,101,STRING) XKT,XKL,EPS,EPSR
	CALL PLTPRT(STRING)
	WRITE(6,101) XKT,XKL,EPS,EPSR
        GO TO 21
   20 DER = (EPS-EPSR)/(XKT-XKL)
   21 XKL = XKT 
      EPSR = EPS
      IF(CABS(DER).EQ.0.) XKT = XKT*1.000001
      IF(CABS(DER).NE.0.) XKT = XKT - EPS/DER 
      IF(CABS(XKT-XKL).EQ.0.) GO TO 7 
      AERR = ERR*(EMAG + CABS(DER*XKL))
      IF(CABS(EPS).LT.AERR) GO TO 7 
    2 CONTINUE
      IF(NBAD.EQ.0) IBAD = N
      NBAD = NBAD + 1 
      ENCODE(120,103,STRING) N,XKT,EPS 
      CALL PLTPRT(STRING)
      WRITE(6,103) N,XKT,EPS
  103 FORMAT(I4,'TH ROOT MISSED, GOT W=',2E14.6,', VAL= '2E12.3) 
    7 XK(N) = XKT 
    1 CONTINUE
C     NOW NR ROOTS ARE FOUND
      ENCODE(120,100,STRING) (NRS(N),XK(N),N=1,NR) 
      CALL PLTPRT(STRING)
  100 FORMAT(' ITER. AND ROOTS',4(I4,2E10.3)) 
  101 FORMAT(' AT5',8E16.8) 
      RETURN
      END 
      SUBROUTINE SORTN(IXS,FOLLOW,NS)
      REAL IXS(20),IXT
      DIMENSION FOLLOW(20)
C     ORDER X COORDS IN INCREASING ORDER
      DO 11 N = 1,NS-1
      N1 = N + 1
      DO 12 M = N1,NS
      IF((IXS(M)-IXS(N)).GT.0) GO TO 12
C     EXCHANGE
      IXT = IXS(N)
      IXS(N) = IXS(M)
      IXS(M) = IXT
      IXT = FOLLOW(N)
      FOLLOW(N) = FOLLOW(M)
      FOLLOW(M) = IXT
   12 CONTINUE
   11 CONTINUE
      RETURN
      END
      FUNCTION CRECIP(X)
C
C	THIS FUNCTION RETURNS THE COMPLEX RECIPROCAL OF X
C	IT AVOIDS THE PROBLEMS OF SQUARING LARGE NUMBERS
C
      COMPLEX X,CRECIP
      BNORM = X
      BNORM = ABS(BNORM) + ABS(AIMAG(X))
      IF(BNORM.LT.1.E-30) GO TO 1
      BNORM = 1./BNORM
      CRECIP = X*BNORM
      CRECIP = BNORM/CRECIP
      RETURN
    1 CRECIP = 1.E30
      RETURN
      END
	FUNCTION FLILO(X,B) 
C 
C 
C       FLILO MEANS LINEAR FUNCTION FOR SMALL ARGUMENT
C       AND LOGARITHM FOR LARGE ARGUMENT
C       SIGN IS PRESERVED 
C       FOR /X/.LT.B   FLILO=X
C       FOR /X/.GT.B   FLILO=A*LOG(X)+C IN SUCH A WAY 
C       THAT FLILO AND ITS DERIVATIVE ARE CONTINUOUS AT /X/=/B/ 
C 
        XM=ABS(X) 
        IF(XM.GT.B) GO TO 1 
	 FLILO=X
        RETURN
    1   FT=B*ALOG(XM/B)+B 
        FLILO=SIGN(FT,X)
        RETURN          
	END 
         FUNCTION AFLILO(X,B) 
C 
C       INVERSE OF FLILO
C 
        XM=ABS(X) 
        IF(XM.GT.B) GO TO 1 
        AFLILO=X
        RETURN
    1   FT=B*EXP(XM/B-1.) 
        AFLILO=SIGN(FT,X) 
        RETURN
        END 
	SUBROUTINE PLTPRT(STRING)
	BYTE STRING(132) 
	WRITE (7,100) STRING 
  100 FORMAT(' '132A1) 
	DO 10 N = 1,132
   10 STRING(N) = ' ' 
	RETURN 
	END
