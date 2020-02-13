      SUBROUTINE BESIZ (ZIN,M,IOM,IRP,GJIM,GJIN,GYKM,GYKN,LERR) 
C 
C  MODIFIED FROM BESSEL TO SAVE SPACE IN 1985, JUNE 
C 
C     ORDINARY AND MODIFIED BESSEL FUNCTIONS OF INTEGRAL ORDER
C     FOR COMPLEX ARGUMENT
C 
C     COMPLEX VARIABLE NOTATION     (I = SQUARE ROOT OF -1) 
C 
C (IN ) RECTANGULAR COORDINATES  -  Z = X+I*Y, PHI = X, CHI = Y 
C (IN ) POLAR COORDINATES  -  Z = RHO*COS(ANGLE)+I*RHO*SIN(ANGLE) 
C (IN )                    -  PHI = RHO,  THE RADIUS VECTOR 
C (IN )                    -  CHI = ANGLE IN DEGREES
C (IN )                    -  180 < CHI <= 180  (PROGRAM CORRECTS)
C (IN ) M  = ORDER TO BE COMPUTED - PROGRAM COMPUTES ORDERS M AND M+1.  
C (IN )        EX.  IF N = M+1 THEN WILL GIVE JM(Z) AND JN(Z) 
C (IN ) IOM = 1  COMPUTE ORDINARY BESSEL FUNCTIONS  
C (IN ) IOM = 2  COMPUTE MODIFIED BESSEL FUNCTIONS  
C (IN ) IRP = 1  ARGUMENT IS IN RECTANGULAR COORDINATES 
C (IN ) IRP = 2  ARGUMENT IS IN POLAR COORDINATES - ANGLE IN DEGREES
C 
C (OUT) GJIM,GJIN - BESSEL FUNCTIONS OF THE FIRST KIND
C (OUT)     GJIM = JM(Z) OR IM(Z) 
C (OUT)     GJIN = JN(Z) OR IN(Z) 
C (OUT) GYKM,GYKN - BESSEL FUNCTIONS OF THE SECOND KIND 
C (OUT)     GYKM = YM(Z) OR KM(Z) 
C (OUT)     GYKN = YN(Z) OR KN(Z) 
C 
C (OUT) LERR IS AN INTEGER INDICATING IF THERE WAS A RUN ERROR IN THE 
C (OUT)      SUBROUTINE.  LERR = 0, NO ERROR.  LERR > 1, THERE WAS AN 
C (OUT)      ERROR.  USER CAN TEST THIS NUMBER AND THEN DECIDE WHETHER  
C (OUT)      TO CONTINUE OR NOT.
C 
      REAL EPSI(9)
      COMPLEX JIM,YKM,JIN,YKN,Z,ZL,OH,X,Y,CM1,CN1,CM2,CN2,
     1 CM3,CN3,CM4,CN4,T,TMK,TMK1,TNK,TNK1,TVK,TVK1,TWK,
     2 TWK1,ST,SU,SV,SW,GJIM,GJIN,GYKM,GYKN,
     3 JHN1,JHM1,JHM,SJM1,SJN1,W,FL1,GL1,NF,DF,FL,NG,DG,GL, 
     4 QN,KN,KM,I,ZIN,PN,HM,HN,ZT3,ZT4, 
     5 S1,S2,S3,S4,FM1,FM2,FN1,FN2,HM1,HM2, 
     6 HN1,HN2,A1,A2,A3,A4
C 
C 
      DATA PI/3.1415926535897932/,PSI/-.1159315156584124488107/ 
      DATA EPSI/ 1.E-9, 1.E-18, 1.E-30, 1.E-30, 1.E+30, 1.E-10, 1.E-14, 
     1     1.E+10, 85.0/
      DATA I/(0.0,1.0)/ 
C 
C**MAKE UP LOCAL VARIABLES
      Z=ZIN 
      ZR=REAL(Z)
      ZI=AIMAG(Z) 
	MORD=M
	IBM=MORD
	RM=FLOAT(MORD)
	NORD=MORD+1 
	IBN=NORD
	RN=FLOAT(NORD)
      LIOM=IOM
      JERR=0
C**JERR=1, Z=0
      IF(ZR .EQ. 0.0 .AND. ZI .EQ. 0.0)  GOTO 5000  
C**JERR=8., TYPE B.F. WRONG 
      IF(LIOM.NE.1.AND.LIOM.NE.2) GOTO 5070 
      N=IRP 
C**JERR=2., COORDINATE SYSTEM WRONG 
      IF(N .NE. 1 .AND. N .NE. 2) GOTO 5010 
      IF(N .EQ. 2) GOTO 130 
C*******************************************************************
C**RECTANGULAR COORDINATES
      RHO=CABS(Z) 
      THETA=ATAN2(ZI,ZR)
      GOTO 140
C****************************************************************** 
C**POLAR COORDINATES
  130 PHI=ZR
      CHI=ZI
      CHI=AMOD(CHI,360.0) 
C**PUT CHI INTO (-180.0,180.0)
      IF(ABS(CHI).GT.180.0) CHI=CHI-SIGN(360.0,CHI) 
      RHO=PHI 
      THETA=PI*(CHI/180.) 
      ZR=RHO*COS(THETA) 
      ZI=RHO*SIN(THETA) 
      Z=CMPLX(ZR,ZI)
C*******************************************************************
C**JERR=3., PREVENTS GETTING LOG 0
C**EPSI(3)=1.E-48 
  140 IF(RHO .LT. EPSI(3))  GOTO 5020 
      ZL=CMPLX(ALOG(RHO)+PSI,THETA) 
      IF(LIOM .EQ. 2) GOTO 160  
C****************************************************************** 
C**SIGNS FOR ORDINARY B.F.
      SGN=-1.0
      SGPI=2./PI
      GOTO 170
C****************************************************************** 
C**SIGNS FOR MODIFIED B.F.
  160 SGN=1.0 
      SGPI=1.0
C****************************************************************** 
  170 OH=.5*Z 
      X=OH**2 
      Y=1.0/Z 
C*****************************************************************
C**WEBER-SCHLAFLI 
      IF(RHO .LT. 21.0)  GOTO  180
C**HANKEL ASYMPTOTIC SERIES 
      IF(FLOAT(IBN**2)/RHO .LE. 1.95)  GOTO  1000 
C**GAUSS CONTINUED FRACTION AND RECURRENCE
  180 IF(RHO .GT. 2.5)  GOTO  600 
C 
C     SET UP INTIIAL CONDITIONS FOR  M=0  N=1 
C 
C**RETURN FOR SMALL CABS(Z)<2.5 
      ASSIGN 2000 TO JS1
  200 IF(IBM-1) 201,210,220 
C*********************************************************************
C**COMMON FACTOR OF J(0) AND I(0) 
  201 CM1=1.
C**COMMON FACTOR OF J(1) AND I(1) 
      CN1=OH
C**COMMON FACTOR OF INFINITE SERIES FOR Y(0) AND K(0) 
      CM2=SGN*.5
C**COMMON FACTOR OF INFINITE SERIES FOR Y(1) AND K(1) 
      CN2=SGN*.5*OH 
C*******************************************************************
      SRV=0.0 
      SRW=1.0 
C*******************************************************************
C**COMMON FACTOR OF FINITE SERIES FOR Y(0) AND K(0) 
      CM3=0.0 
C**COMMON FACTOR OF FINITE SERIES FOR Y(1) AND K(1) 
      CN3=SGN*Y 
C**SUM OF TERMS OF FINITE SERIES FOR Y(0) AND K(0)  
      CM4=0.0 
C**SUM OF TERMS OF FINITE SERIES FOR Y(1) AND K(1)  
      CN4=1.0 
C*****************************************************************
      GOTO 500
C 
C     SET UP INITIAL CONDITIONS FOR  M=1  N=2 
C 
  210 CM1=OH
      CN1=X*.5
      CM2=SGN*.5*OH 
      CN2=SGN*.5*CN1
      SRV=1.0 
      SRW=1.5 
      CM3=SGN*Y 
      CN3=SGN*2.*Y**2 
      CM4=1.0 
      CN4=1.-SGN*X
      GOTO 500
C 
C     SET UP INITIAL CONDITIONS FOR  M=2  N=3 
C 
  220 IF(IBM .GT. 2)  GOTO 300  
      CM1=.5*X
      CN1=X*OH/6. 
      CM2=SGN*.5*CM1
      CN2=SGN*.5*CN1
      SRV=1.5 
      SRW=11./6.
      CM3=SGN*2.*Y**2 
      CN3=1.0/(SGN*6.*CN1)
      CM4=1.-SGN*X
      CN4=1.-SGN*.5*X+(.5*X)**2 
      GOTO 500
C 
C     CODING FOR THE I TH ORDER FOR RHO < 2.5 
C 
  300 CM1=X*.5
      T3=2. 
      SRV=1.5 
  310 T3=T3+1.
      T4=1./T3
      CM1=OH*CM1*T4 
      T5=CABS(CM1)
C**JERR=4., CONSTANT TERM OF FINITE SERIES TOO LARGE
C**EPSI(5)=1.E+75, EPSI(4)=1.E-75 
      IF(T5 .GT. EPSI(5) .OR. T5 .LT. EPSI(4))  GOTO 5030 
      SRV=SRV+T4
      IF(INT(T3) .NE. IBM)  GOTO 310
      T4=1./(T3+1.) 
      SRW=SRV+T4
      CN1=OH*CM1*T4 
      CM3=1.0/(FLOAT(2*IBM)*SGN*CM1)
      CN3=1.0/(FLOAT(2*IBN)*SGN*CN1)
      CM2=SGN*.5*CM1
      CN2=SGN*.5*CN1
C 
C     COMPUTE THE FINITE SERIES 
C 
      TMK=1.0 
      TNK=1.0 
      CM4=0.
      CN4=TMK 
      IFR=1 
      K=0 
  400 K=K+1 
	IF ((IFR .EQ. 0) .OR. (IBM .EQ. IFR) .OR. (IBN .EQ. IFR)) 
     +       TYPE 10 ,IFR,IBM,IBN 
 10   FORMAT (' WARNING (BESSEL) -- IFR, IBM, IBN',3I10) 
      TMK1=-SGN*TMK*X/FLOAT(IFR*(IBM-IFR))
      TNK1=-SGN*TNK*X/FLOAT(IFR*(IBN-IFR))
      CN4=CN4+TNK1
      CM4=CM4+TMK 
      TNK = TNK1
      TMK = TMK1
      IFR=IFR+1 
      T2=CABS(TNK1) 
C**EPSI(2)=1.E-18 
      IF(T2 .GE. EPSI(2) .AND. K .LT. 100 .AND. IFR .NE. IBM)  GOTO 400 
      IF(T2 .GE. EPSI(2) .AND. K .EQ. 100 .AND. IFR .NE. IBM)  STOP 777 
      CN4=CN4+TNK1
C**SUM THE SERIES TERMS IN REVERSE ORDER  
C  440 K=K-1
C      CN4=CN4+TN(K)
C      CM4=CM4+TM(K)
C      IF(K .GT. 1)  GOTO 440 
C 
C     COMPUTE THE INFINITE SERIES 
C 
  500 TMK=1.0 
      ST = TMK
      TNK=1.0 
      SU = TNK
      SV=SRV
      SW = SRW
      K=0 
      IFR=0 
      T1=FLOAT(IBN) 
      T3=SRW
      T4=0.0
C****************************************************************** 
C**BEGIN.. CALCULATE TERMS IN SERIES UNTIL IT CONVERGES 
  510 K=K+1 
      IFR=IFR+1 
      T1=T1+1.0 
      T2=T3 
      T3=T2+1.0/T1
      T4=T4+1.0/FLOAT(IFR)
      P=T4+T2 
      Q=T4+T3 
C**J(M) OR I(M) 
      TMK1=SGN*TMK*X/FLOAT(IFR*(IBM+IFR)) 
C**Y(M) OR K(M) 
      TVK1=P*TMK1 
C**J(N) OR I(N) 
      TNK1=SGN*TNK*X/FLOAT(IFR*(IBN+IFR)) 
C**Y(N) OR K(N) 
      TWK1=Q*TNK1 
      T5=CABS(TWK1) 
      ST=ST+TMK1
      SU=SU+TNK1
      SV=SV+TVK1
      SW=SW+TWK1
      TMK=TMK1
      TNK=TNK1
C**OUT OF LOOP
C**EPSI(2)=1.E-18 
CPRINT *,T5,SGN,TWK1,TNK1,X,K 
      IF(T5 .GE. EPSI(2) .AND. K .LT. 100)  GOTO 510
      IF(T5 .GE. EPSI(2) .AND. K .EQ. 100) STOP 776 
C***********************************************************
C**J(M) OR I(M) 
C      ST=TM(K+1) 
C**J(N) OR I(N) 
C      SU=TN(K+1) 
C**Y(M) OR K(M) 
C      SV=TV(K+1) 
C**Y(N) OR K(N) 
C      SW=TW(K+1) 
C**SUM THE SERIES TERMS IN REVERSE ORDER  
C  540 ST=ST+TM(K)
C      SU=SU+TN(K)
C      SV=SV+TV(K)
C      SW=SW+TW(K)
C      K=K-1
C      IF(K .GT. 0) GOTO 540
C**RESULTS STORED IN OUTPUT ARRAYS
      JIM=CM1*ST
      JIN=CN1*SU
      SGN1=1.0
      SGN2=1.0
C**ORDINARY B.F.
      IF(LIOM .EQ. 1)  GOTO 560 
C**MODIFIED B.F. DIFFER IN SIGNS OF EVEN TERMS
C**M EVEN, N ODD, SGN1=1.0
C**M ODD, N EVEN
      IF(MOD(MORD,2).EQ.0) SGN1=-1.0
      SGN2=-SGN1
C**SUM THE LOGARITHMIC TERM, THE FINITE TERM, AND THE 
C**THE INFINITE SERIES TERM.  THEN MULTIPLY BY THEIR
C**COMMON FACTOR. 
C**FIRST, DO Y(M) OR K(M) 
  560 YKM=SGPI*(SGN1*JIM*ZL+CM3*CM4+SGN2*CM2*SV)
C**THEN DO Y(N) OR K(N) 
      YKN=SGPI*(SGN2*JIN*ZL+CN3*CN4+SGN1*CN2*SW)
      GOTO  JS1,(2000,630)
C 
C     COMPUTATION FOR Z > 2.5 BY USE OF RECURSION FORMULAS
C 
C**RETURN TO RECURSION AFTER USING SERIES TO CALCULATE
C**STARTING VALUE FOR RECURRENCE PROCEDURES.
  600 ASSIGN 630 TO JS1 
      TX=RHO*RHO/4.0
      T1=1.0
      IFR=0 
      IFN=NORD
C**INCREASE ORDER UNTIL T1 < 1.E-10 
  610 IFR=IFR+1 
      IFN=IFN+1 
      T1=T1*TX/FLOAT(IFR*IFN) 
C**EPSI(6)=1.E-10 
      IF(T1 .GE. EPSI(6))  GOTO 610 
      IBM=IFN 
      IBN=IFN+1 
C**GO COMPUTE B.F. OF ORDER IFN AND IFN+1 USING SERIES. 
      GOTO 200
C**RETURN FROM SERIES.  USE B.F. OF HIGH ORDER TO 
C**START DOWNWARD RECURSION.
  630 JHN1=JIN
      JHM1=JIM
C**J(M-1)=(2M/Z)*J(M)-J(N)
      JHM=FLOAT(2*IBM)*Y*JHM1+SGN*JHN1
      IBM=IBM-1 
      IF(IBM.LE.MORD) GOTO 650  
C**J(N)=J(M)=J(N-1) 
  640 JHN1=JHM1 
C**J(M)=J(M-1)
      JHM1=JHM
      JHM=FLOAT(2*IBM)*Y*JHM1+SGN*JHN1
C**DECREASE ORDER TO COMPUTE J(M-2) = ((2*(M-1))/Z)*J(M-1)-J(N-1) 
C**UNTIL IBM = MORD.  
      IBM=IBM-1 
C**FINISHED RECURSION 
      IF(IBM .NE. MORD)  GOTO 640 
C**J OR I COMPUTED BY RECURSION ARE STORED IN OUTPUT ARRAYS 
  650 JIM=JHM 
      JIN=JHM1
C**SAVE VALUES OF J(M) AND J(N) COMPUTED BY DOWNWARD RECURSION. 
      SJM1=JIM
      SJN1=JIN
C 
C     COMPUTE Y AND K BY USING CONTINUED FRACTIONS OF GAUSS 
C 
      SGN1=1.0
      IF(LIOM .EQ. 2)  GOTO 700 
C**ORDINARY B.F.
      SGN3=1.0
      IF(ZI .GE. 0.0)  GOTO 730 
      SGN1=-1.0 
      SGN3=-1.0 
C**1/2Z., ROTATED BY SGN1 TO RIGHT HALF-PLANE IF NECESSARY. 
  730 W=.5*SGN1*Y*I 
      GO TO 740 
C**MODIFIED B.F.
C**Z IN RIGHT HALF-PLANE
  700 IF(ZR .GE. 0.0)  GOTO 710 
C**MODIFIED B.F.., LEFT HALF-PLANE
      SGN1=-1.0 
C**QUAD II  
      SGN3=-1.0 
C**QUAD III 
      IF(ZI .LT. 0.0) SGN3=1.0  
C**1/2Z., ROTATED BY SGN1 TO RIGHT HALF-PLANE IF NECESSARY. 
  710 W=SGN1*.5*Y 
  740 AA=.5+RN
      BB=.5-RN
      CLL=50.0
C**M ODD, N EVEN
      SGN1=1.0
C**M EVEN, N ODD
      IF(MOD(NORD,2).NE.0) SGN1=-1.0
      SGN2=-SGN1
      FL=0.0
      GL=0.0
C**BEGIN USING CONTINUED FRACTION FOR Y OR K
  760 FL1=FL
      GL1=GL
      BL=BB+CLL 
      BL1=BL+1.0
      ALL=AA+CLL-1.0
C**************************************************************** 
C**H(L)(A,B.,V) 
      NF=1.0+BL*W*FL1 
      DF=NF+ALL*W 
      FL=NF/DF
C***************************************************************
C**G(L)(A,B.,V) 
      NG=1.0+BL1*W*GL1
      DG=NG+ALL*W 
      GL=NG/DG
C*************************************************************
      CLL=CLL-1.0 
      IF(CLL .GE. .5)  GOTO 760 
      QN=FL*(1.0+(BB+1.0)*W*GL) 
C************************************************************ 
C**ORDINARY B.F.
      IF(LIOM .EQ. 2)  GOTO 780 
C**ORDINARY B.F.
C**ANALYTIC CONTINUATION TO FIND CORRECT VALUE FOR Y
      PN=I*SGN3*QN
      HN=4.0*W/(PI*(PN*JIN-JIM))
      HM=HN*PN
C*********************************************************
C**Y STORED IN OUTPUT ARRAYS
      YKM=SGN3*I*(JIM-HM) 
      YKN=SGN3*I*(JIN-HN) 
C******************************************************** 
C**RETURN FROM SUBROUTINE 
      GOTO  2000
C**MODIFIED B.F.
C**Z IN RIGHT-HALF PLANE
  780 IF(ZR .GE. 0.0)  GOTO 790 
C**-I(N) WHEN N IS ODD
      SJN1=SGN1*SJN1
C**I(M) WHEN M IS EVEN
      SJM1=SGN2*SJM1
C**Q(N)*I(N)+I(M)=Q(M+1)*I(M+1)+I(M)
  790 KN=2.0*W/(QN*SJN1+SJM1) 
C**K(M)=K(N)*Q(N)=K(N-1)
      KM=QN*KN
C*************************************************************
      IF(ZR .GE. 0.0) GO TO 810 
C**ANALYTIC CONTINUATION USED TO FIND CORRECT VALUE FOR K 
      T1=SGN3*PI
      KN=SGN1*KN+I*T1*SJN1
      KM=SGN2*KM+I*T1*SJM1
C************************************************************ 
C**RESULTS STORED IN OUTPUT ARRAYS. 
  810 YKM=KM
      YKN=KN
C**RETURN FROM SUBROUTINE 
      GOTO 2000 
C 
C     COMPUTE BESSEL FUNCTIONS USING HANKEL ASYMPTOTIC SERIES 
C 
 1000 SGN1=1.0
      T=-I  
      ZETA=.5*THETA 
C**RIGHT HALF-PLANE 
      IF(ZR .GE. 0.0)  GOTO 1010
C**ROTATE Z TO RIGHT HALF-PLANE.
      SGN1=-1.0 
      T=I 
C**ROTATE -180 DEGREES FROM QUAD II TO QUAD IV
      ZETA=.5*(THETA-PI)
C**ROTATE 180 DEGREES FROM QUAD III TO QUAD I 
      IF(ZI .LT. 0.0) ZETA=.5*(THETA+PI)
C**PUTS Z IN RIGHT HALF-PLANE 
 1010 RED=SGN1*ZR 
      CMD=SGN1*ZI 
C**ORDINARY B.F.
C**+-I/(8*Z)
      IF(LIOM .EQ. 1) W=T*Y*0.125 
C**MODIFIED B.B.
C**+-1/(8*Z)
      IF(LIOM .EQ.2) W=SGN1*Y*0.125 
      K=2 
      TC1=1.0 
      TC2=1.0 
C**4*M**2 
      RFM2=4.0*RM*RM
C**4*N**2 
      RFN2=4.0*RN*RN
      SGN=-1.0
      S1=1.0
      S2=1.0
      S3=1.0
      S4=1.0
C**(4*M**2-1)*(-I/(8*Z))
      ZT3=(RFM2-1.0)*W
C**(4*N**2-1)*(I/(8*Z)) 
      ZT4=(RFN2-1.0)*W
      TMK=-ZT3
      TNK=ZT3 
      TVK=-ZT4
      TWK=ZT4 
      T5=CABS(ZT4)
 1050 K=K+1 
      TC1=TC1+1.0 
      TC2=TC2+2.0 
      SGN=-SGN
C**M-TH TERM OF SERIES P(M) +- I*Q(M) OR P(M) +- Q(M) 
      ZT3=((RFM2-TC2**2)/TC1)*ZT3*W 
C**N-TH TERM OF SERIES P(N) +- I*Q(N) OR P(N) +- Q(N) 
      S1=S1+TMK 
      S2=S2+TNK 
      S3=S3+TVK 
      S4=S4+TWK 
      ZT4=((RFN2-TC2**2)/TC1)*ZT4*W 
      TMK=SGN*ZT3 
      TNK=ZT3 
      TVK=SGN*ZT4 
      TWK=ZT4 
C**EPSI(2)=1.E-18 
      IF(CABS(ZT3) .GE. EPSI(2) .AND. K .LT. 100
     1   .AND. T5 .GE. CABS(ZT4)) GOTO 1050 
      IF(CABS(ZT3) .GE. EPSI(2) .AND. K .EQ. 100
     1   .AND. T5 .GE. CABS(ZT4)) STOP 775
      IF(CABS(ZT3) .GE. EPSI(2) .AND. T5 .LT. CABS(ZT4)) GO TO 1080 
      S1=S1+TMK 
      S2=S2+TNK 
      S3=S3+TVK 
      S4=S4+TWK 
C**SUM THE SERIES TERMS IN REVERSE ORDER  
 1080 K=K-1 
C**P(M)+I*Q(M) FOR J(M) OR P(M)+Q(M) FOR I(M) 
C      S1=S1+TM(K)
C**P(M)-I*Q(M) FOR Y(M) OR P(M)-Q(M) FOR K(M) 
C      S2=S2+TN(K)
C**P(N)+I*Q(N) FOR J(N) OR P(N)+Q(N) FOR I(N) 
C      S3=S3+TV(K)
C**P(N)-I*Q(N) FOR Y(N) OR P(N)-Q(N) FOR K(N) 
C     S4=S4+TW(K) 
C      IF(K .NE. 1)  GOTO  1080 
      IF(LIOM.EQ.2) GO TO 1200  
C******************************************************** 
C**ORDINARY B.F.
      C1=1.0/SQRT(.5*PI*RHO)
      C2=.25*PI 
      C3=.5*PI*RM 
      C4=.5*PI*RN 
C**EPSI(9)=350.0 IN ORIGINAL PROGRAM, CHANGED TO 85. FOR MINC 
      IF(ABS(CMD) .GT. EPSI(9))  GOTO 5040
      C5=C1*EXP(-CMD) 
      C6=C1*EXP(CMD)
C**EPSI(8)=1.0E+10
      IF(ABS(RED) .GT. EPSI(8))  GOTO 5050
C**+- ZR - THETA/2 - PI/4 - N*M/2 
      ALPH1=RED-ZETA-C2-C3
      ALPH2=RED+ZETA-C2-C3
      ALPH3=RED-ZETA-C2-C4
      ALPH4=RED+ZETA-C2-C4
C**H(N)(1)(Z) 
      FM1=C5*CMPLX(COS(ALPH1),SIN(ALPH1))*S1
C**H(N)(2)(Z) 
      FM2=C6*CMPLX(COS(ALPH2),-SIN(ALPH2))*S2 
C**H(N+1)(1)(Z) 
      FN1=C5*CMPLX(COS(ALPH3),SIN(ALPH3))*S3
C**H(N+1)(2)(Z) 
      FN2=C6*CMPLX(COS(ALPH4),-SIN(ALPH4))*S4 
      IF(ZR .LT. 0.0)  GOTO  1115 
C**Z LIES IN RIGHT HALF-PLANE 
C**J(M) 
      HM1=FM1 
C**Y(M) 
      HM2=FM2 
C**J(N) 
      HN1=FN1 
C**Y(N) 
      HN2=FN2 
      GOTO  1140
C**CORRECT HANKEL FUNCTIONS IF Z WAS GIVEN IN THE 
C**LEFT HALF-PLANE
C**M EVEN, N ODD
 1115 SGN1=1.0
C**M ODD, N EVEN
      IF(MOD(MORD,2) .NE. 0) SGN1=-1.0
      SGN2=-SGN1
      IF(ZI .LT. 0.0)  GOTO  1130 
C**IV --> II
      HM1=SGN2*FM2
      HM2=SGN1*(FM1+2.0*FM2)
      HN1=SGN1*FN2
      HN2=SGN2*(FN1+2.0*FN2)
      GOTO  1140
C**I --> III
 1130 HM1=SGN1*(2.0*FM1+FM2)
      HM2=SGN2*FM1
      HN1=SGN2*(2.0*FN1+FN2)
      HN2=SGN1*FN1
C**OUTPUT ARRAYS FOR ORDINARY B.F.
 1140 JIM=.5*(HM1+HM2)
      JIN=.5*(HN1+HN2)
      YKM=.5*I*(HM2-HM1)
      YKN=.5*I*(HN2-HN1)
C**RETURN 
      GOTO  2000
C******************************************************** 
C**MODIFIED B.F.
 1200 C1=1.0/SQRT(2.0*PI) 
      C2=1.0/SQRT(RHO)
      C3=SQRT(.5*PI)
C**EPSI(9)=350.0
      IF(ABS(RED) .GT. EPSI(9))  GOTO 5040
      C4=EXP(-RED)
      C5=C1*C2*EXP(RED) 
      C6=C1*C2*C4 
      C7=C3*C2*C4 
C**EPSI(8)=1.0E+10
      IF(ABS(CMD) .GT. EPSI(8)) GOTO 5050 
      ALPH1=CMD-ZETA
      A1=CMPLX(COS(ALPH1),SIN(ALPH1)) 
      SGN1=1.0
      IF(ZR .LE. 0.0) SGN1=-1.0 
      IF(ZI .LE. 0.0) SGN1=-SGN1
      ALPH2=-CMD-ZETA+SGN1*(RM+.5)*PI 
      A2=CMPLX(COS(ALPH2),SIN(ALPH2)) 
      ALPH3=-CMD-ZETA+SGN1*(RN+.5)*PI 
      A3=CMPLX(COS(ALPH3),SIN(ALPH3)) 
      ALPH4=-ZETA-CMD 
      A4=CMPLX(COS(ALPH4),SIN(ALPH4)) 
C**I(M) 
      FM1=C5*A1*S1+C6*A2*S2 
C**I(N) 
      FN1=C5*A1*S3+C6*A3*S4 
C**K(M) 
      FM2=C7*A4*S2
C**K(N) 
      FN2=C7*A4*S4
      IF(ZR .LT. 0.0)  GOTO  1220 
C**OUTPUT ARRAYS FOR Z IN RIGHT HALF-PLANE
      JIM=FM1 
      JIN=FN1 
      YKM=FM2 
      YKN=FN2 
C**RETURN 
      GOTO  2000
C**OUTPUT ARRAYS FOR Z IN LEFT HALF-PLANE 
C**M EVEN, N ODD
 1220 SGN1=1.0
C**M ODD, N EVEN
      IF(MOD(MORD,2) .NE. 0) SGN1=-1.0
      SGN2=-SGN1
C**QUAD II  
      T=I 
C**QUAD III 
      IF(ZI .LT. 0.0) T=-I
      JIM=SGN1*FM1
      JIN=SGN2*FN1
      YKM=SGN1*(FM2-T*PI*JIM) 
      YKN=SGN2*(FN2-T*PI*JIN) 
C**RETURN 
      GOTO  2000
C******************************************************** 
C 
C     ERROR PRINT OUTS
C 
 5000 JERR=1
      GOTO 5090 
 5001 WRITE(7,5002) 
 5002 FORMAT(// 49H CAN NOT COMPUTE THE BESSEL FUNCTION IF Z IS ZERO) 
      WRITE(7,5003) 
 5003 FORMAT(// 47H J 0 EQUAL 1  -  ALL OTHER ORDERS OF J ARE ZERO) 
      WRITE(7,5004) 
 5004 FORMAT(38H0ALL ORDERS OF Y ARE EQUAL TO INFINITY) 
      GO TO 5100
C************************************************************ 
 5010  JERR=2 
      GOTO 5090 
 5011 WRITE(7,5012) OPT2
 5012 FORMAT(/ 30H OPT2 IS NOT 1 OR 2     OPT2 =,E12.4) 
      GOTO 5100 
C************************************************************** 
 5020 JERR=3
      GOTO 5090 
 5021 WRITE(7,5022) RHO 
 5022 FORMAT(// 29H RHO IS OUTSIDE RANGE   RHO =,E12.4) 
      GO TO 5100
C*************************************************************
 5030 JERR=4
      GOTO 5090 
 5031 WRITE(7,5032) 
 5032 FORMAT(// 43H CONSTANT TERM OF FINITE SERIES WILL EXCEED, 
     123H NUMBER SIZE OF MACHINE) 
      WRITE(7,5033) OM,RN,CMR1,CMI1 
 5033 FORMAT(4HOM =,F12.4, 5H  N =,E12.4, 8H  CMR1 =,E12.4, 
     1 8H  CMI1 =, E12.4) 
      WRITE(7,5034) PHI,CHI 
 5034 FORMAT(6H0PHI =,E15.8, 7H  CHI =,E15.8) 
      GOTO 5100 
C***************************************************************
 5040 JERR=5
      GOTO 5090 
 5041 WRITE(7,5042) 
 5042 FORMAT(/ 52H ERROR IN ASYMPTOTIC SECTION - ARGUMENT FOR EXP TOO , 
     1 5HLARGE) 
      WRITE(7,5043) RED,CMD 
 5043 FORMAT(1H0,5HRED =,E15.8,4X,5HCMD =,E15.8)
      GOTO 5100 
C*****************************************************************( 
 5050 JERR=6
      GOTO 5090 
 5051 WRITE(7,5052) 
 5052 FORMAT(/ 52H ARGUMENT FOR SIN AND COS IN ASYMPTOTIC SECTION TOO , 
     1 5HLARGE) 
      WRITE(7,5053) ZETA
 5053 FORMAT(1H0,6HZETA =,E15.8)
      GOTO 5100 
C**************************************************************** 
 5060 JERR=7
      GOTO 5090 
 5061 WRITE(7,5062) 
 5062 FORMAT(55H SUBROUTINE CALCULATES INTEGRAL ORDER ONLY- CHECK INPUT)
      GOTO 5100 
C********************************************************************** 
 5070 JERR=8
      GOTO 5090 
 5071 WRITE(7,5072) 
 5072 FORMAT(33H OPT1 IS NOT 1 OR 2 - CHECK INPUT)  
      GOTO 5100 
C******************************************************************** 
 5090 WRITE(7,5091) 
 5091 FORMAT(// 40H RUN ERROR IN BESSEL FUNCTION SUBROUTINE)
      WRITE(7,5777) ' PHI',PHI,' CHI',CHI,' ORD',ORD,'OPT1',OPT1, 
     1  'OPT2',OPT2 
C 5092 FORMAT(1H0,5HPHI =,F15.8,4X,5HCHI =,F15.8,4X,5HORD =,F6.1,4X,
C     1 6HOPT1 =,F6.1,4X,6HOPT2 =,F6.1) 
 5777 FORMAT(1H0,5(A4,' =',E14.7,4X)) 
      GOTO (5001,5011,5021,5031,5041,5051,5061,5071), JERR
 5100 WRITE(7,5101) 
 5101 FORMAT(1H ) 
C 
 2000 LERR=JERR 
      GJIM=JIM
      GJIN=JIN
      GYKM=YKM
      GYKN=YKN
      RETURN
      END 
