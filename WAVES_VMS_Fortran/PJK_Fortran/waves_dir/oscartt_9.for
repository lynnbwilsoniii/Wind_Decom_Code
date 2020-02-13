      PROGRAM DISPER
C
C     IN MODE = 3,4,5 K MOVES ON A CIRCLE.
C	RKX IS INITIAL ANGLE, RKZ IS ANGLE INCREMENT,
C	XKX**2 + XKZ**2 GIVES MAGNITUDE OF K.
C     IN MODE = 6,7,8 PROGRAM FINDS WI AND MAG(K), GIVEN THETA AND WR
C	XKZ (NOT RKZ) THEN GIVES TRIAL INCREMENTING FACTOR FOR MAG(K)
C	RKX GIVES INITIAL ANGLE, IN DEGREES.
C     I WANT: NPR=0 IS SCREEN ONLY, NPR=1 IS ROOTS ON PRINTER
C	NPR=2 IS POLARIZATIONS AND FULL INFO
C
C	MAIN PROGRAM (DISPER) prints variables and results
C	It calls DISPES, which solves determinant by Newton's method
C	DISPES calls EPSILN to calculate dispersion matrix
C	EPSILN  calls POLARZ once for each plasma component
C	POLARZ mainly does sum over N = cyclotron harmonic number
C	see POLARZ for further description
C
      DIMENSION RATIOM(10),DENS(10),BETAZ(10),TPAR(10),TPERP(10)
      DIMENSION DIELR(10,3,3),DIELI(10,3,3),EPSR(3,3),EPSI(3,3)
      DIMENSION CAPKS(45)
      LOGICAL NAME(70)
      COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS  
      COMMON /DATABL/ XD(70)
      EQUIVALENCE (XD(1),WR),(XD(2),WI),(XD(3),WCYCL),(XD(4),XKX)
      EQUIVALENCE (XD(5),XKZ),(XD(7),RW),(XD(8),RKX),(XD(9),RKZ)
      EQUIVALENCE (XD(13),END1),(XD(14),END2)
      DATA NAME /'  WR','  WI',' WCE','  KX','  KZ',' KIT','  RW',
     1' RKX',' RKZ','MODE',' NPR','  NC','END1','END2',6*'    ',
     2 10*'RMAS',10*'DENS',10*'BETA',10*'TPAR',10*'TPRP'/
      DATA END1,END2 /16.,20./
      DATA NPR /0/
      DATA IHPR / 1 / 
C
	OPEN(UNIT=3,NAME='OSCARD.DAT',TYPE='OLD')
	READ (3,106) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
C	PRINT   106, WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
	WRITE(6,105) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
 105	FORMAT(8E12.5,3I3)
 106	FORMAT(5F10.5,3F5.3,3I2)
	READ(3,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
C	PRINT  101,(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
	WRITE(6,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
 101	FORMAT(5E10.4)
	CLOSE(UNIT = 3)
      XD(12) = NC
      XD(6) = KIT
      XD(10) = MODE
      XD(11) = NPR
      WRITE (6,4) 
      IF(IHPR.EQ.1) WRITE (6,5)  
      IHPR = 0  
    4 FORMAT (1H1)
    5 FORMAT (///,16X,41HTHIS PROGRAM CALCULATES ROOTS OF THE DIS-/,
     18X,49HPERSION EQUATION FOR A MULTICOMPONENT HOT PLASMA.//,8X, 
     221HTO CHANGE PARAMETERS:///,8X,27H1.      TYPE IN PARAMETER"S,
     316H NUMBER FROM THE /,16X,15HLIST BELOW.    //,8X,8H2.      , 
     439HENTER A COMMA AND THEN THE VALUE OF THE/,16X,10HPARAMETER ,
     517HON THE SAME LINE.//,16X,30HNOTE:  MORE THAN ONE PARAMETER ,
     67H CAN BE/,16X,35HCHANGED BY MAKING ALL BUT THE FINAL/,16X, 
     733HNUMBER OF THE PARAMETER NEGATIVE./ 
     $/,16X,29H1, WR       4, XKX      7, RW/,
     816X,30H2, WI       5, XKZ      8, RKX/,16X,15H3, WCYCL    6, ,
     915HKIT      9, RKZ/,15X,31H10, MODE    11, NPR     12, NC   /
     +,15X,31H13, END1    14, END2    15,      /
     +/,16X,21H*TO STOP, ENTER 0,0 *//,16X,
     $ 42H*FOR LISTING OF ALL PARAM"S, ENTER 999,0 *///)
      ROUND = 1.
  200 WRITE (7,6) ROUND  
      WRITE (6,150)
  100 READ(5,*) IWHICH,XPARM
    6 FORMAT ( ////,72(1H-),/,35X,F3.0,////)  
      IF (IWHICH.NE.999) GO TO 35 
      II =NC+1
      WRITE (7,130) 'WR  = ',WR,'WI  = ',WI,'WCYCL = ',WCYCL 
  130 FORMAT (/,2(8X,A6,F8.4),6X,A8,F8.4) 
      WRITE (7,135) 'XKX = ',XKX,'XKZ = ',XKZ,'RW = ',RW,
     1'RKX = ',RKX,'RKZ = ',RKZ 
      WRITE (6,135) 'XKX = ',XKX,'XKZ = ',XKZ,'RW = ',RW,
     1'RKX = ',RKX,'RKZ = ',RKZ 
  135 FORMAT (3(8X,A6,F8.4)/,3(8X,A6,F8.4)) 
      WRITE (7,140) 5HTPERP,(I,TPERP(I),I=1,II)
  140 FORMAT (/,8X,A6,5(/,16X,I2,2H, ,E16.9,8X,I2,2H, ,E16.9))
      WRITE (7,140) 4HTPAR,(I,TPAR(I),I=1,I,II)
      WRITE (7,140) 5HBETAZ,(I,BETAZ(I),I=1,II)
      WRITE (7,140) 6HRATIOM,(I,RATIOM(I),I=1,II)
      WRITE (7,140) 4HDENS,(I,DENS(I),I=1,II)
      WRITE (7,145) 'NC = ',NC,'KIT = ',KIT,'MODE = ',MODE,'NPR = ',NPR
      WRITE (6,145) 'NC = ',NC,'KIT = ',KIT,'MODE = ',MODE,'NPR = ',NPR
  145 FORMAT (/,4(8X,A7,I3))
      WRITE (6,150)
  150 FORMAT (///,8X,26H*NOW ENTER NEW PARAMETERS*//) 
      GO TO 200 
   35 IF (IWHICH.EQ.0) STOP 
      IW=IABS(IWHICH) 
      WRITE(7,1000) NAME(IW),XD(IW),XPARM
      WRITE(6,1000) NAME(IW),XD(IW),XPARM
      XD(IW) = XPARM
  110 IF (IWHICH.LT.0) GO TO 100
C   15 FORMAT (/,8X,7HCHANGE ,A5,6H FROM ,F13.9,4H TO ,F13.9,/)
C   25 FORMAT (/,8X,7HCHANGE ,A5,6H FROM ,I5    ,4H TO ,I5,/)  
 1000 FORMAT (/,8X,7HCHANGE ,A4,6H FROM ,F18.10    ,4H TO ,F18.10,/)  
      NC = XD(12) + .5
      KIT = XD(6) + .5
      MODE = XD(10) + .5
      NPR = XD(11) + .5
C	WRITE HEADER FOR PLOTTING FILE
	WRITE(8,105) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
	WRITE(8,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
      CALL DISPES (NC,KIT,MODE,NPR)
      ROUND = ROUND + 1.
      GO TO 200 
      END 
      SUBROUTINE DISPES (NC,KIT,MODE,NPR)
      DIMENSION RATIOM(10), DENS(10), BETAZ(10), TPAR(10), TPERP(10), DI
     1ELR(10,3,3), DIELI(10,3,3), EPSR(3,3), EPSI(3,3), CAPKS(45) 
      DIMENSION ROT(3,3), EPSKR(3,3), EPSKI(3,3)
      DIMENSION DT(45), DRV(45), VAR(45), DML(45)  
      DIMENSION XKS(45), NTRY(45) 
      DIMENSION E(6), B(6)
      COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS  
      COMMON /DATABL/ XD(70)
      EQUIVALENCE (XD(1),WR),(XD(2),WI),(XD(3),WCYCL),(XD(4),XKX)
      EQUIVALENCE (XD(5),XKZ),(XD(7),RW),(XD(8),RKX),(XD(9),RKZ)
      EQUIVALENCE (XD(13),END1),(XD(14),END2)
      MCIR=MODE/3 
      MODE=MOD(MODE,3)
      WRITE (7,280) 
      DO 90 I=1,NC
      IF(NPR.GT.0)  PRINT 270,I,RATIOM(I),DENS(I),BETAZ(I),TPAR(I),
     1 TPERP(I) 
   90 WRITE (7,270) I,RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I) 
      WRITE (7,282) 
      DO 100 I=1,NC 
        WP=SQRT(ABS(DENS(I)/RATIOM(I))) 
        WC=WCYCL/RATIOM(I)
        VZT=BETAZ(I)/SQRT(TPAR(I))
        TTE=TPAR(I)*RATIOM(I)/TPAR(1) 
  100   WRITE (7,270) I,WP,WC,VZT,TTE 
      LL=20*MODE
      IF (LL.EQ.0) LL=1
      NF=2  
      ANG=RKX 
      XKS(1)=XKX
      DKMAG=XKZ 
      DO 265 LZ=1,KIT 
      XK2 = XKX**2 + XKZ**2
      XK = SQRT(XK2)
        IF (MCIR-1) 105,105,150 
C
C		ENTRY FOR MODES 0 TO 5, I.E. FIND W GIVEN K
C
  105   VAR(1)=WR 
        VAR(2)=WI 
        VAR(3)=WR*RW  
        VAR(4)=WI*RW  
        NTRY(1)=1 
	XKS(1)=XK
        CAPKR=0.
        CAPKI=0.
       IF(LL.GT.3) GO TO 202
C
C	ENTRY FOR MODE 0 (OR 3 OR 6)
C
       IF(LZ.EQ.1) WRITE (7,351) 6HRE(W) ,6HIM(W) ,7HLN(DET),7HRE(DET),
     1  7HIM(DET),7H CAPKR ,7H CAPKI ,6HMAG(K),3HTRY 
C       IF(LZ.EQ.1)    PRINT 351, 6HRE(W) ,6HIM(W) ,7HLN(DET),7HRE(DET),
       IF(LZ.EQ.1) WRITE (6,351) 6HRE(W) ,6HIM(W) ,7HLN(DET),7HRE(DET),
     1  7HIM(DET),7H CAPKR ,7H CAPKI ,6HMAG(K),3HTRY 
       K = 1
       CALL EPSILN (VAR(K),VAR(K+1),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR
     1    ,DIELI) 
          CALL DETER (EPSR,EPSI,DT(K),DT(K+1),DMAG,DML(K)) 
          CAPKR=QR(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**
     1    2,EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K
     2    )**2-VAR(K+1)**2,2.*VAR(K)*VAR(K+1))
          CAPKI=QI(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**
     1    2,EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K
     2    )**2-VAR(K+1)**2,2.*VAR(K)*VAR(K+1))
        WRITE (7,360) VAR(1),VAR(2),DML(K),DT(1),DT(2),CAPKR,CAPKI,XK
        WRITE (6,360) VAR(1),VAR(2),DML(K),DT(1),DT(2),CAPKR,CAPKI,XK
	WRITE (8,361) VAR(1),VAR(2),DML(1),DT(1),DT(2),XKX,XKZ
C        PRINT    360, VAR(1),VAR(2),DML(K),DT(1),DT(2),CAPKR,CAPKI,XK
C        WR=WR*RW 
C	WI=WI*RW 
C	XKX=XKX*RKX 
C	XKZ=XKZ*RKZ
	NF = K+3
	NBEST = 1
	GO TO 225
  202   CONTINUE 
	NBEST = 1
        DO 145 K=1,LL,2 
          CALL EPSILN (VAR(K),VAR(K+1),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR
     1    ,DIELI) 
          CALL DETER (EPSR,EPSI,DT(K),DT(K+1),DMAG,DML(K)) 
          NTRY(K)=(K+1)/2 
          CAPKR=QR(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**
     1    2,EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K
     2    )**2-VAR(K+1)**2,2.*VAR(K)*VAR(K+1))
          CAPKI=QI(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**
     1    2,EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K
     2    )**2-VAR(K+1)**2,2.*VAR(K)*VAR(K+1))
          CAPKS(K)=CAPKR
          CAPKS(K+1)=CAPKI
      	  IF (K.EQ.1) GO TO 145
          XKS(K)=XKS(K-2) 
    	  IF (DML(K).LT.DML(K-2)) GO TO 120
C		MOVE BEST SOLUTION TO LAST POSITION
          DO 115 I=1,2
            J=K+I-1 
            TEMP=VAR(J)
            VAR(J)=VAR(J-2) 
            VAR(J-2)=TEMP  
            TEMP=DT(J)
            DT(J)=DT(J-2) 
            DT(J-2)=TEMP
            TEMP=CAPKS(J)
            CAPKS(J)=CAPKS(J-2) 
            CAPKS(J-2)=TEMP
            NTEMP=NTRY(J)  
            NTRY(J)=NTRY(J-2) 
  115       NTRY(J-2)=NTEMP
   	    TEMP = DML(K)
   	    DML(K) = DML(K-2)
   	    DML(K-2) = TEMP
  120     CONTINUE
	  NBEST = K
   	  FACT = EXP(DML(K-2) - DML(K))
   	  DTI = FACT * DT(K-1)
   	  DTR = FACT * DT(K-2)
C	WRITE(7,*) DMAG,DML(K),DTR,DTI,DT(K),DT(K+1)
          DRV(K)=QR(DT(K)-DTR,DT(K+1)-DTI,VAR(K)-VAR(K-2),VAR(K+
     1    1)-VAR(K-1))
          DRV(K+1)=QI(DT(K)-DTR,DT(K+1)-DTI,VAR(K)-VAR(K-2),VAR(
     1    K+1)-VAR(K-1))
 	  IF(ABS(DRV(K))+ABS(DRV(K+1)).LT.1.E-16) GO TO 215
          VAR(K+2)=VAR(K)-QR(DT(K),DT(K+1),DRV(K),DRV(K+1)) 
          VAR(K+3)=VAR(K+1)-QI(DT(K),DT(K+1),DRV(K),DRV(K+1)) 
          A=VAR(K)**2+VAR(K+1)**2 
          D=XKZ*XKZ 
          C=XKX*XKX
   	  IF ((VAR(K+2).EQ.VAR(K)).AND.(VAR(K+3).EQ.VAR(K+1))) 
     1	    GO TO 215
         CONTINUE
          DT(K+2)=0.  
          DT(K+3)=0.  
          DRV(K+2)=0.
          DRV(K+3)=0. 
          DML(K+2) = 0.
          XKS(K+2) = 0.
          NF=K+3
C
C		END TEST
C
          A=(C*C+D*D)*A 
      	  IF (DML(K).LT.(ALOG(A)-END1)) GOTO 215
      	  IF (DML(K).LT.(DMAG-END2)) GOTO 215
 145      CONTINUE
        GO TO 215 
C
C	ENTRY FOR MODES 6 AND UP, FIND IM(W) AND K, GIVEN RE(W) AND THETA
C
  150   VAR(1)=WR 
        VAR(2)=WI 
        VAR(3)=WR 
        VAR(4)=WI*RW  
	IF(WI.EQ.0.) VAR(4) = -WR*(RW-1.)
        VAR(6)=WI 
        VAR(5)=WR 
 	XKS(1) = XKX
        XKS(3)=XKS(1) 
        XKS(5)=XKS(1)*DKMAG 
        NTRY(1)=1 
        NTRY(3)=2 
        XKX=XKS(1)*SIN(ANG/57.295779518)  
        XKZ=XKS(1)*COS(ANG/57.295779518)  
        CALL EPSILN (VAR(1),VAR(2),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR,DIE
     1  LI) 
        CALL DETER (EPSR,EPSI,DT(1),DT(2),DMAG,DML(1)) 
	print*,'first call,wr,wi,xkx,xkz',var(1),var(2),xkx,xkz
	print*,'first call,dt1,dt2,dmag,dml1',dt(1),dt(2),dmag,dml(1)
        CALL EPSILN (VAR(3),VAR(4),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR,DIE
     1  LI) 
        CALL DETER (EPSR,EPSI,DT(3),DT(4),DMAG,DML(3)) 
	print*,'secon call,wr,wi,xkx,xkz',var(3),var(4),xkx,xkz
	print*,'secon call,dt3,dt4,dmag,dml3',dt(3),dt(4),dmag,dml(3)
        DO 205 K=5,LL,2 
          XKX=XKS(K)*SIN(ANG/57.295779518)
          XKZ=XKS(K)*COS(ANG/57.295779518)
	  XK = XKS(K)
          NTRY(K)=(K+1)/2 
          CALL EPSILN (VAR(K),VAR(K+1),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR
     1    ,DIELI) 
          CALL DETER (EPSR,EPSI,DT(K),DT(K+1),DMAG,DML(K)) 
	print*,'next call,wr,wi,xkx,xkz',var(k),var(k+1),xkx,xkz
	print*,'next call,dt1,dt1,dmag,dml1',dt(k),dt(k+1),dmag,dml(k)
C	PRINT*,'DMAG @ K=',K,DMAG,DML(K)
C	MOVE WORST SOLUTION TO K-4 POSITION
          DO 170 I=1,3,2
            KI=K+I-3  
            IF (DML(KI).LE.DML(K-4)) GO TO 170
            DO 160 J=1,2
              JI=J-1+KI 
              JK=K-5+J
              TEMP=VAR(JI)
              VAR(JI)=VAR(JK) 
              VAR(JK)=TEMP 
              TEMP=DT(JI)  
              DT(JI)=DT(JK) 
              DT(JK)=TEMP  
              TEMP=CAPKS(JI)  
              CAPKS(JI)=CAPKS(JK) 
              CAPKS(JK)=TEMP  
              TEMP=XKS(JI) 
              XKS(JI)=XKS(JK) 
  160         XKS(JK)=TEMP 
            NTEMP=NTRY(KI) 
            NTRY(KI)=NTRY(K-4) 
            NTRY(K-4)=NTEMP 
	    TEMP = DML(KI)
	    DML(KI) = DML(K-4)
	    DML(K-4) = TEMP
C	    IF(I.NE.3) GO TO 170
C             DMAG=0. 
C             XKX=XKS(K)*SIN(ANG/57.295779518)
C             XKZ=XKS(K)*COS(ANG/57.295779518)
C	     XK =XKS(K)
  170       CONTINUE
C	MOVE BEST SOLUTION TO K POSITION
            KI=K-2 
            IF (DML(KI).GT.DML(K)) GO TO 164
            DO 162 J=1,2
              JI=J-1+KI 
              JK=K-1+J
              TEMP=VAR(JI)
              VAR(JI)=VAR(JK) 
              VAR(JK)=TEMP 
              TEMP=DT(JI)  
              DT(JI)=DT(JK) 
  162         DT(JK)=TEMP  
            TEMP=XKS(KI) 
            XKS(KI)=XKS(K) 
            XKS(K)=TEMP 
            NTEMP=NTRY(KI) 
            NTRY(KI)=NTRY(K) 
            NTRY(K)=NTEMP 
	    TEMP = DML(KI)
	    DML(KI) = DML(K)
	    DML(K) = TEMP
  164       CONTINUE
	  NBEST = K
	  FACT  = EXP(DML(K-2) - DML(K))  
	  FACT2 = EXP(DML(K-4) - DML(K))  
	  DTR = FACT*DT(K-2)
	  DTI = FACT*DT(K-1)
	  DTR2 = FACT2*DT(K-4)
	  DTI2 = FACT2*DT(K-3)
          DK1=-XKS(K)+XKS(K-2)  
          DK2=-XKS(K)+XKS(K-4)  
          DW1=-VAR(K+1)+VAR(K-1)
          DW2=-VAR(K+1)+VAR(K-3)
          DEN=DK1*DW2-DK2*DW1 
          IF (DEN.EQ.0.) GO TO 175
C	ENTRY FOR NORMAL COMPUTATION OF DERIVATIVES, I.E.
C	DETERMINANT FOR DERIVATIVE CONPUTATION IS NONZERO
C	DRV(K),(K+1) ARE DERIV WRT XK
C	DRV(K-2),(K-1) ARE DERIV WRT WI
          DRV(K)=(DW2*DTR-DW1*DTR2+(DW1-DW2)*DT(K))/DEN
          DRV(K+1)=(DW2*DTI-DW1*DTI2+(DW1-DW2)*DT(K+1))/DEN
          DRV(K-2)=-(DK2*DTR-DK1*DTR2+(DK1-DK2)*DT(K))/DEN 
          DRV(K-1)=-(DK2*DTI-DK1*DTI2+(DK1-DK2)*DT(K+1))/DEN 
C	PRINT*,'NORMAL DERIV,K=',K,(DRV(L),L=K-2,K+1) 	
C	PRINT*,'DML',(DML(L),L=K-4,K,2)
          IF (ABS(DRV(K-2))+ABS(DRV(K-1))-1.0E-8*(ABS(DT(K))+ABS(DT(K+1
     1    )))) 175,175,190
  175     DW3=-VAR(K+1)/2.
C	PRINT*,'NO, ABNORMAL'
C	DETERMINANT OF DERIVATIVE COMPUTATION VANISHES, I.E. DW, DK LIE
C	ON A STRAIGHT LINE IN WI, XK SPACE, AND ONLY DERIVATIVE IN THAT
C	DIRECTION CAN BE COMPUTED.
          IF (ABS(DK1).GT.ABS(DK2)) GO TO 180 
          IF (DK2.EQ.0.) THEN
	    PRINT 285, K 
	    NF = K+1
            GO TO 215
	  ENDIF
          DRV(K)=(DTR2-DT(K))/DK2
          DRV(K+1)=(DTI2-DT(K+1))/DK2  
          GO TO 185 
  180     DRV(K)=(DTR-DT(K))/DK1
          DRV(K+1)=(DTI-DT(K+1))/DK1  
  185     DK3=-QR(DT(K),DT(K+1),DRV(K),DRV(K+1))
          GO TO 195 
  190     DEN=DRV(K-2)*DRV(K+1)-DRV(K-1)*DRV(K) 
C	IF THIS DETERMINANT VANISHES SO THAT THE SEPARATE
C	DERIVATIVES CANNOT BE CALCULATED, THE MOST LIKELY
C	CAUSES ARE (1) THAT DT IS PURE REAL AND IM(DRV) VANISHES.
C	THEN ASSUME THAT ONLY XK CHANGES.
C	(2) WI IS SO SMALL THAT CHANGES ARE LOST IN THE NOISE
	  IF(DEN.EQ.0.) GO TO 192
          DW3=(DT(K+1)*DRV(K)-DT(K)*DRV(K+1))/DEN 
          DK3=-(DT(K+1)*DRV(K-2)-DT(K)*DRV(K-1))/DEN
	  GO TO 195
  192		IF(DRV(K).EQ.0.) THEN
		  NF = K-1
		  PRINT*,'BOTH DERIVS VANISH,K=',K
		  GO TO 215
		ENDIF
		DW3 = -EXP(DML(K))*DT(K+1)/DRV(K)
		DK3 = -EXP(DML(K))*DT(K)/DRV(K)
  195     VAR(K+3)=VAR(K+1)+DW3 
          XKS(K+2)=XKS(K)+DK3 
          VAR(K+2)=VAR(K) 
          A=VAR(K)**2+VAR(K+1)**2 
          D=XKZ*XKZ 
          C=XKX*XKX 
          A=(C*C+D*D)*A 
	  A = AMAX1(A,5.E-34)
          NF=K+3
          DT(K+2)=0.  
          DT(K+3)=0.  
          DRV(K+2)=0. 
          DRV(K+3)=0. 
C	PRINT*,'END TRY',K,VAR(K+1),XKZ,XKS(K)
C
C	END TEST
C
C	print*,'end test, dml,A,lna,dmag',dml(k),A,alog(a),dmag
          IF (DML(K).LT.(ALOG(A)-END1)) GO TO 210
          IF (DML(K).LT.(DMAG-END2)) GO TO 210
  205     CONTINUE
	NF = NF-2
  210   K=NF-3

        CAPKR=QR(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**2,
     1  EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K)**2
     2  -VAR(K+1)**2,2.*VAR(K)*VAR(K+1))  
        CAPKI=QI(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**2,
     1  EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K)**2
     2  -VAR(K+1)**2,2.*VAR(K)*VAR(K+1))  
        CAPKS(K)=CAPKR
        CAPKS(K+1)=CAPKI
        K=NF-5
        CALL EPSILN (VAR(K),VAR(K+1),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR,D
     1  IELI) 
        CAPKR=QR(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**2,
     1  EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K)**2
     2  -VAR(K+1)**2,2.*VAR(K)*VAR(K+1))  
        CAPKI=QI(EPSR(1,1)*XKX**2+2.*EPSR(1,3)*XKX*XKZ+EPSR(3,3)*XKZ**2,
     1  EPSI(1,1)*XKX**2+2.*EPSI(1,3)*XKX*XKZ+EPSI(3,3)*XKZ**2,VAR(K)**2
     2  -VAR(K+1)**2,2.*VAR(K)*VAR(K+1))  
        CAPKS(K)=CAPKR
        CAPKS(K+1)=CAPKI
C
C		 PRINT OUT RESULTS AND CALCULATE NEW START
C
  215   WRITE (7,350) 
        WRITE (6,350) 
        WRITE (7,351) 6HRE(WR),6HIM(WR),7HLN(DET),7HRE(DET),7HIM(DET), 
     1   7HRE(DER),7HIM(DER),6HMAG(K),3HTRY 
C        PRINT 351,    6HRE(WR),6HIM(WR),7HLN(DET),7HRE(DET),7HIM(DET), 
        WRITE (6,351) 6HRE(WR),6HIM(WR),7HLN(DET),7HRE(DET),7HIM(DET), 
     1   7HRE(DER),7HIM(DER),6HMAG(K),3HTRY 
  351   FORMAT (//,6X,A6,6X,A6,5(1X,A7,1X),3X,A6,1X,A3,/)
        DO 220 K=1,NF,2 
          XNX=XKX/VAR(K)
          XNZ=XKZ/VAR(K)
      IF(NPR.GT.0)WRITE(6,360)VAR(K),VAR(K+1),DML(K),DT(K),DT(K+1),
     1    DRV(K),DRV(K+1),XKS(K),NTRY(K)
  220   WRITE (7,360) VAR(K),VAR(K+1),DML(K),DT(K),DT(K+1),DRV(K)
     1    ,DRV(K+1),XKS(K),NTRY(K)
        NF=IABS(NF-3)+3 
        IF (NPR.GT.1) WRITE (7,290) VAR(NF-3),VAR(NF-2),WCYCL,XKX,XKZ 
      WRITE(7, 291) XKX, XKZ
      WRITE(6, 291) XKX, XKZ
  291 FORMAT( ' KX=', E12.5, ' KZ =', E12.5)
	K = NBEST
	WRITE (8,361) VAR(K),VAR(K+1),DML(K),DT(K),DT(K+1),XKX,XKZ
        VPX=XKX*VAR(NF-3)/XK2 
        VPZ=XKZ*VAR(NF-3)/XK2 
        XN2=XK2/(VAR(NF-3)**2-VAR(NF-2)**2) 
        WIWR=VAR(NF-2)/VAR(NF-3)
        IF (NPR.EQ.2) WRITE (7,295) VPX,VPZ,XN2,WIWR
        IF (CAPKS(NF-3).EQ.CAPKS(NF-5)) GO TO 225 
        RESR=QR(VAR(NF-3)-VAR(NF-5),VAR(NF-2)-VAR(NF-4),CAPKS(NF-3)-CAPK
     1  S(NF-5),CAPKS(NF-2)-CAPKS(NF-4))  
        RESI=QI(VAR(NF-3)-VAR(NF-5),VAR(NF-2)-VAR(NF-4),CAPKS(NF-3)-CAPK
     1  S(NF-5),CAPKS(NF-2)-CAPKS(NF-4))
        RES1=RESI*XK2 
        RES2=XK2*RESR 
        IF (NPR.GT.1) WRITE (7,300) RESR,RESI,RES2,RES1
  225   IF (NPR.LT.3) GO TO 235 
        WRITE (7,305) 
        DO 230 N=1,NC 
          WRITE (7,350) 
          WRITE (7,355) ((DIELR(N,I,J),DIELI(N,I,J),I=1,3),J=1,3) 
          WRITE (6,350) 
  230     WRITE (6,355) ((DIELR(N,I,J),DIELI(N,I,J),I=1,3),J=1,3) 
        WRITE (6,310) 
        WRITE (6,355) ((EPSR(I,J),EPSI(I,J),I=1,3),J=1,3) 
        WRITE (7,310) 
        WRITE (7,355) ((EPSR(I,J),EPSI(I,J),I=1,3),J=1,3) 
  235   IF (NPR.LT.2) GO TO 250 
        CALL WAVPOL (EPSR,EPSI,E,B,VAR(NF-3),VAR(NF-2),XKX,XKZ) 
        WRITE (7,350) 
        WRITE(7,315) (E(I),I=1,6) 
        WRITE(7,320) (B(I),I=1,6) 
        DO 240 I=1,3  
          DO 240 J=1,3
          EPSKR(I,J)=0. 
          EPSKI(I,J)=0. 
  240     ROT(I,J)=0. 
        ROT(1,1)=XKZ/XK 
        ROT(3,3)=XKZ/XK 
        ROT(1,3)=XKX/XK 
        ROT(3,1)=-XKX/XK
        ROT(2,2)=1. 
        DO 245 I=1,3  
          DO 245 J=1,3
          DO 245 K=1,3
          DO 245 L=1,3
          EPSKR(I,J)=EPSKR(I,J)+ROT(L,I)*EPSR(L,K)*ROT(K,J) 
  245     EPSKI(I,J)=EPSKI(I,J)+ROT(L,I)*EPSI(L,K)*ROT(K,J) 
       WRITE(7,325) 
       WRITE(7,355) ((EPSKR(I,J),EPSKI(I,J),I=1,3),J=1,3) 
       WRITE(7,350) 
       WRITE(7,330) (E(I),I=1,6) 
       WRITE(7,335) (B(I),I=1,6) 
C       WRITE (6,340) 
C       WRITE (6,345) DT(NF-3),DT(NF-2),A,DMAG
C       WRITE (6,350) 
  250   CONTINUE
        WR=VAR(NF-1)  
        WI=VAR(NF)
        IF (MCIR) 260,260,255 
  255   ANG=RKX+FLOAT(LZ)*RKZ 
        XKX=XK*SIN(ANG/57.295779518)
        XKZ=XK*COS(ANG/57.295779518)
        XKS(1)=XK 
        GO TO 265 
  260   XKX=XKX*RKX 
        XKZ=XKZ*RKZ 
  265   CONTINUE
      RETURN
C 
  270 FORMAT (/,5X,I3,4X,5E11.4)
  280 FORMAT (//,2X,40H COMPONENT   MASS/ME    DENSITY     VZ/C , 
     1 23H       TPAR       TPERP)
  282 FORMAT (//,2X,43H COMPONENT  PLASM FRQ  CYCL FREQ  VZ/VTHERM, 
     110H  T/TE PAR)  
  285 FORMAT (28H VANISHING K INCREMENT, K=  ,I4) 
  290 FORMAT (//,8X,9H W/WPE = E11.5,4H +I E11.5/,  
     1 8X,10H WCE/WPE =, E11.5,10H KXC/WPE =,E11.5,/, 
     2 8X,10H KZC/WPE =,E11.5,//) 
  295 FORMAT (//,8X,23H PHASE VELOCITY  VPX = E11.5,6H VPZ =E11.5/, 
     1 8X,21H     REF. INDEX  N2 =,E11.5,8H WI/WR =,E10.4//)  
  300 FORMAT (//,8X,19H RESIDUE OF K2*KL =,E14.5,2H+I,E14.5/, 
     18X,8H OF KL =,E14.5,2H+I,E14.5,//)  
  305 FORMAT (16H POLARIZATIONS  )
  310 FORMAT (//25H DISPERSION DETERMINANT  //) 
  315 FORMAT (10H EX/EY/EZ=3(E16.6,3H +IE13.6)) 
  320 FORMAT (10H BX/BY/BZ=3(E16.6,3H +IE13.6)) 
  325 FORMAT (//46H DISPERSION DETERMINANT IN Y X K, Y, K SYSTEM //)
  330 FORMAT (10H E1/EY/EK=3(E16.6,3H +IE13.6)) 
  335 FORMAT (10H B1/BY/BK=3(E16.6,3H +IE13.6)) 
C  340 FORMAT (//35H VALUE AND ESTIMATE OF MAGNITUDE   //) 
C  345 FORMAT (E19.9,4H +I E19.9,15H W2*(KX4+KZ4)= E19.9,15H LARGEST TERM
C     1= E19.9)
  350 FORMAT (/)
  355 FORMAT (3(E20.10,3H +IE17.10))
  360 FORMAT (1X,E12.6 ',' E10.3,1X,F8.3,F8.3,',',F6.3,E10.2 ',' E9.2,
     1  1X,F9.3,I3)
  361 FORMAT (1X,E12.6 ',' E10.3,1X,F8.3,F8.3,',',F6.3,E12.4 ',' E11.4)
      END 
      SUBROUTINE EPSILN (WR,WI,WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR,DIELI) 
      DIMENSION RATIOM(10), DENS(10), BETAZ(10), TPAR(10), TPERP(10), DI
     1ELR(10,3,3), DIELI(10,3,3), EPSR(3,3), EPSI(3,3), POLR(3,3), POLI(
     2 3,3)  
      COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS  
C      COMMON /DATABL/ XD(70)
C
C	print*,'@eps, w,k=',wr,wi,xkx,xkz
      DO 100 I=1,3
        DO 100 J=1,3  
        EPSR(I,J)=0.  
        EPSI(I,J)=0.  
        DO 100 N=1,NC 
        DIELR(N,I,J)=0. 
  100   DIELI(N,I,J)=0. 
      EPSR(1,1)=WR*WR-WI*WI-XKZ*XKZ 
      EPSI(1,1)=2.*WR*WI
      EPSR(3,3)=WR*WR-WI*WI-XKX*XKX 
      EPSI(3,3)=2.*WR*WI
      EPSR(2,2)=WR*WR-WI*WI-XKX*XKX-XKZ*XKZ 
      EPSI(2,2)=2.*WR*WI
      EPSR(1,3)=XKZ*XKX 
      EPSI(1,3)=0.
      DO 105 N=1,NC 
        WC=WCYCL/RATIOM(N)
        WP2=DENS(N)/ABS(RATIOM(N))
        BZ=BETAZ(N) 
        CALL POLARZ (WR,WI,WP2,WC,XKX,XKZ,BZ,TPAR(N),TPERP(N),POLR,POLI)
        DO 101 I=1,3  
	  IJ=I
        DO 101 J=IJ,3  
C 	WRITE(7,7000) N,I,J,IJ,POLI(I,J)
C 7000	FORMAT(' INDICES'4I4,2E15.6)
        DIELR(N,I,J)=POLR(I,J)  
        EPSR(I,J)=EPSR(I,J)+POLR(I,J)  
 101	CONTINUE
	DO 102 I=1,3
	IJ=I
	DO 102 J=IJ,3
	DIELI(N,I,J)=POLI(I,J)
	EPSI(I,J)=EPSI(I,J)+POLI(I,J)
 102	CONTINUE
 105	CONTINUE
      EPSR(3,1)=EPSR(1,3) 
      EPSI(3,1)=EPSI(1,3) 
      EPSR(2,1)=-EPSR(1,2)
      EPSI(2,1)=-EPSI(1,2)
      EPSI(3,2)=-EPSI(2,3)
      EPSR(3,2)=-EPSR(2,3)
C 	write(9,1001) WR,EPSR(1,1),EPSI(1,1),EPSR(1,3),EPSI(1,3)
C 1001	format(6e12.4)
      RETURN
      END 
      SUBROUTINE POLARZ (WR,WI,WP2,WC,XKX,XKZ,BZ,TP,TPER,POLR,POLI) 
C
C	POLARZ has entry points:
C		100  kx Rl gt sqrt(30.)   (changed apr 89 to sqrt(1000.)
C		  105  and W gt 30.Wc
C		       then treat as if mag field = 0.
C		110  kx Rl lt sqrt(1000) or /W/ lt 30.Wc
C		       first sum over N = law harmonic number
C		       then (135-175) add harmonics around resonant
C		       harmonic (in alpha) if they havent yet been done
C
      DIMENSION TER(3,3), TEI(3,3), POLR(3,3), POLI(3,3), EI(99)
C
      IF (XKX*XKX*TPER-30.*WC*WC) 110,100,100 
  100 IF (WR*WR-900.*WC*WC) 110,105,105 
  105 XK2=XKX*XKX+XKZ*XKZ 
      XK=SQRT(XK2)
      EBZ=XKZ*BZ/XK 
      TE=(XKX*XKX*TPER+XKZ*XKZ*TP)/XK2
      VT=SQRT(2.*TE)  
      W=WR-XKZ*BZ 
      CALL F (WR,WI,TE,XK,EBZ,F0R,F0I,F1R,F1I,F2R,F2I)
      H0R=QR(F0R,F0I,W,WI)*XK*VT
      H0I=QI(F0R,F0I,W,WI)*XK*VT
      H1R=-XK*QR(EBZ*F0R-F1R,EBZ*F0I-F1I,W,WI)
      H1I=-XK*QI(EBZ*F0R-F1R,EBZ*F0I-F1I,W,WI)
      H2R=XK*QR(EBZ*EBZ*F0R-2.*EBZ*F1R+F2R,EBZ*EBZ*F0I-2.*EBZ*F1I+F2I,W,
     1WI)/VT
      H2I=XK*QI(EBZ*EBZ*F0R-2.*EBZ*F1R+F2R,EBZ*EBZ*F0I-2.*EBZ*F1I+F2I,W,
     1WI)/VT
      H3R=PR(W,WI,H2R,H2I)/XK/VT
      H3I=PI(W,WI,H2R,H2I)/XK/VT-.5 
      CR=WP2
      CI=0. 
      TD=1.-TP/TPER 
      TDD=TPER/TP-1.  
      TX=XKX*TPER/TE/XK 
      TX2=TX*TX 
      TZ=XKZ*TP/TE/XK 
      TZ2=TZ*TZ 
      T0R=PI(H0R,H0I,CR,CI) 
      T0I=-PR(H0R,H0I,CR,CI)
      T1R=PI(H1R,H1I,CR,CI) 
      T1I=-PR(H1R,H1I,CR,CI)
      T2R=PI(H2R,H2I,CR,CI) 
      T2I=-PR(H2R,H2I,CR,CI)
      T3R=PI(H3R,H3I,CR,CI) 
      T3I=-PR(H3R,H3I,CR,CI)
      POLR(2,2)=-CR-TPER/TE*T1R 
      POLI(2,2)=-CI-TPER/TE*T1I 
      POLR(3,3)=-CR-TX*TP*XKX/TE/XK*((1.+2.*XKZ**2*TD/XK2)*T1R+2.*XKZ*BZ
     1*TE/XK/VT*TD/TP*T0R)-2.*TZ2*T3R-4.*BZ/VT*TZ*T2R-BZ*BZ/VT/VT*T1R*2.
      POLI(3,3)=-CI-TX*TP*XKX/TE/XK*((1.+2.*XKZ**2*TD/XK2)*T1I+2.*XKZ*BZ
     1*TE/XK/VT*TD/TP*T0I)-2.*TZ2*T3I-4.*BZ/VT*TZ*T2I-BZ*BZ/VT/VT*T1I*2.
      POLR(1,3)=-2.*TX*TZ*T3R-2.*TX*BZ/VT*T2R-TX*TZ*(-1.+TDD*XKX*XKX/XK2
     1-TD*XKZ*XKZ/XK2)*T1R+XKZ*BZ/XK/VT*TX*TZ*TE/TP*T0R*TD
      POLI(1,3)=-2.*TX*TZ*T3I-2.*TX*BZ/VT*T2I-TX*TZ*(-1.+TDD*XKX*XKX/XK2
     1-TD*XKZ*XKZ/XK2)*T1I+XKZ*BZ/XK/VT*TX*TZ*TE/TP*T0I*TD
      POLR(1,1)=-CR-2.*TX2*T3R-TZ*TPER*XKZ/TE*(1.-2.*(XKX/XK)**2*TDD)*T1
     1R/XK  
      POLI(1,1)=-CI-2.*TX2*T3I-TZ*TPER*XKZ/TE*(1.-2.*(XKX/XK)**2*TDD)*T1
     1I/XK  
      POLR(2,3)=0.
      POLI(2,3)=0.
      POLR(1,2)=0.
      POLI(1,2)=0.
      RETURN
  110 XL=XKX*XKX*TPER/WC/WC 
      DENN=4.19-ALOG(XL+.00001)
      DENN=AMAX1(DENN,2.228)
      NMAX=108./DENN+5. 
      CALL BESSI (NMAX,XL,EI) 
      A=EI(1)-XL*EI(2)
      DO 9 I = 1,3  
	DO 9 J = 1,3  
	TER(I,J) = 0. 
    9 TEI(I,J) = 0. 
      CALL TERM (WR,WI,WP2,WC,XKX,XKZ,1.,BZ,TP,TPER,0,EI(1),A,POLR,POLI,
     1TER,TEI,SF) 
      POLR(2,2)=XL*POLR(2,2)
      POLI(2,2)=XL*POLI(2,2)
      DO 130 M=1,NMAX 
	L=M
        LAST=M
        IF (L-1) 115,115,120
  115   EIP=XL*(EI(2)-.5*EI(3))-.5*EI(1)  
        GO TO 125 
  120   EIP=XL*(EI(L+1)-.5*(EI(L)+EI(L+2))) 
  125   CALL TERM (WR,WI,WP2,WC,XKX,XKZ,XL,BZ,TP,TPER,L,EI(L+1),EIP,TER,
     1  TEI,POLR,POLI,SF) 
        FS=SF 
        CALL TERM (WR,WI,WP2,WC,XKX,XKZ,XL,BZ,TP,TPER,-L,EI(L+1),EIP,TER
     1  ,TEI,POLR,POLI,SF)
        FS=FS+SF
        IF (FS-1.) 135,130,130  
  130   CONTINUE
  135 HN=(WR-XKZ*BZ)/WC 
      NH=MIN1(ABS(HN),32000.)
      NS=SIGN(1.,HN)  
      IF (LAST-NH) 140,140,175  
  140 CALL SER (NH,XL,A,B)
      IF (LAST-NH) 145,160,175  
  145 NU=NH-LAST
      EL=A  
      EL1=B 
      XNH=NH
      EIP=XL*B+XNH*A  
      DO 155 I=1,NU 
        M=NS*(NH+1-I) 
        CALL TERM (WR,WI,WP2,WC,XKX,XKZ,XL,BZ,TP,TPER,M,EL,EIP,TER,TEI,P
     1  OLR,POLI,SF)  
        IF (SF-1.) 160,160,150  
  150   XN=(NH+1-I)*2 
        T=EL1+XN*EL/XL
        EIP=XL*(T+EL1)/2. 
        EL1=EL
        EL=T
  155   CONTINUE
  160 NL=NH+1 
      NU=2*NH 
      EL=A  
      EL1=B 
      XNH=NH+1
      EIP=-XNH*B+XL*A 
      DO 170 I=NL,NU  
        M=I*NS
        CALL TERM (WR,WI,WP2,WC,XKX,XKZ,XL,BZ,TP,TPER,M,EL1,EIP,TER,TEI,
     1  POLR,POLI,SF) 
        IF (SF-1.) 175,175,165  
  165   XN=I
        T=EL-XN*EL1/XL*2. 
        EIP=EL1*XL-(XN+1.)*T
        EL=EL1
        EL1=T 
  170   CONTINUE
  175 CONTINUE
C	write(9,1001) WR,polr(1,1),poli(1,1),polr(2,3),poli(2,3)
C 1001	format(6e12.4)
	RETURN
      END 
      SUBROUTINE TERM (WR,WI,WP2,WC,XKX,XKZ,XL,BZ,TP,TPER,N,EI,EIP,TER,T
     1EI,POLR,POLI,SF)
      DIMENSION TER(3,3), TEI(3,3), POLR(3,3), POLI(3,3)
      XN=N  
      W=WR+XN*WC
      AR=W-XKZ*BZ 
      CALL F (W,WI,TP,XKZ,BZ,AFOR,AFOI,AF1R,AF1I,AF2R,AF2I) 
      VR=BZ*AF1R-AF2R 
      VI=BZ*AF1I-AF2I 
      UR=BZ*AFOR-AF1R 
      UI=BZ*AFOI-AF1I 
      TR=-PR(VR,VI,W,WI)/TP-XN*WC*AF2R/TPER 
      TI=-PI(VR,VI,W,WI)/TP-XN*WC*AF2I/TPER 
      SR=QR(TR,TI,WR,WI)
      SI=QI(TR,TI,WR,WI)
      C=WP2*EI
      DR=QR(AR,WI,WR,WI)
      DI=QI(AR,WI,WR,WI)
      TER(3,3)=-C*QI(SR,SI,DR,DI)*XL
      TEI(3,3)=C*QR(SR,SI,DR,DI)*XL 
      TR=XKZ*(TPER*UR/TP+AF1R)  
      TI=XKZ*(TPER*UI/TP+AF1I)  
      SR=QR(TR,TI,WR,WI)-AFOR 
      SI=QI(TR,TI,WR,WI)-AFOI 
      THETAR=QR(SR,SI,DR,DI)
      THETAI=QI(SR,SI,DR,DI)
      TR=XKZ*(TPER*VR/TP+AF2R)  
      TI=XKZ*(TPER*VI/TP+AF2I)  
      SI=QI(TR,TI,WR,WI)-AF1I 
      SR=QR(TR,TI,WR,WI)-AF1R 
      VTHETI=QI(SR,SI,DR,DI) 
      VTHETR=QR(SR,SI,DR,DI) 
      TEI(1,1)=-C*XN*XN*THETAR  
      TER(1,1)=C*XN*XN*THETAI 
      CP=WP2*EIP
      TER(1,2)=CP*XN*THETAR 
      TEI(1,2)=CP*XN*THETAI 
      TER(1,3)=-C*XN*XKX*VTHETI/WC 
      TEI(1,3)=C*XN*XKX*VTHETR/WC
      TER(2,2)=TER(1,1)+2.*XL*CP*THETAI 
      TEI(2,2)=TEI(1,1)-2.*XL*CP*THETAR 
      TER(2,3)=CP*XKX*VTHETR/WC
      TEI(2,3)=CP*XKX*VTHETI/WC
      SF=0. 
      DO 110 I=1,3
	IJ=I
        DO 110 J=IJ,3  
        A=POLR(I,J)+TER(I,J)
        B=POLI(I,J)+TEI(I,J)
        IF (ABS(A)+ABS(B)-1.0E6*(ABS(TER(I,J))+ABS(TEI(I,J)))) 100,105,1
     1  05  
  100   SF=2. 
  105   POLR(I,J)=A 
  110   POLI(I,J)=B 
      RETURN
      END 
      SUBROUTINE DETER (AIN,BIN,DR,DI,DM,DML)
C	THIS ROUTINE RETURNS THE DETERMINANT OF AIN + I*BIN EXPRESSED AS
C	DR +I*DI = (DET)/NORM AND LN(NORM) = DML,  DM IS AN ESTIMATE OF
C	THE LOGARITHM OF THE LARGEST TERM
      DIMENSION A(3,3), B(3,3),AIN(3,3),BIN(3,3),RMAG(3)
      DO 1 I=1,3
        RMAG(I) = 0.
	print*,'a(i)',i,(ain(i,j),j=1,3)
	print*,'b(i)',i,(bin(i,j),j=1,3)
        DO 2 J=1,3
   	  A(I,J) = AIN(I,J)
   	  B(I,J) = BIN(I,J)
          AM = ABS(AIN(I,J)) + ABS(BIN(I,J))
 2        RMAG(I) = AMAX1(AM,RMAG(I))
        DO 3 J=1,3
   	  A(I,J) = A(I,J)/RMAG(I)
          B(I,J) = B(I,J)/RMAG(I)
C	WRITE(7,*) A(I,J),B(I,J),RMAG(I),AIN(I,J),BIN(I,J)
 3	CONTINUE
 1      CONTINUE
      DR=TPR(A(1,1),B(1,1),A(2,2),B(2,2),A(3,3),B(3,3)) 
      DI=TPI(A(1,1),B(1,1),A(2,2),B(2,2),A(3,3),B(3,3)) 
      TR=-TPR(A(3,1),B(3,1),A(2,2),B(2,2),A(1,3),B(1,3))
      TI=-TPI(A(3,1),B(3,1),A(2,2),B(2,2),A(1,3),B(1,3))
	print*,'dr',dr,di,tr,ti
	if(1) stop
      DR=DR+TR
      DI=DI+TI
      TR=TPR(A(1,2),B(1,2),A(2,3),B(2,3),A(3,1),B(3,1)) 
      TI=TPI(A(1,2),B(1,2),A(2,3),B(2,3),A(3,1),B(3,1)) 
      DR=DR+TR
      DI=DI+TI
      TR=TPR(A(1,3),B(1,3),A(3,2),B(3,2),A(2,1),B(2,1)) 
      TI=TPI(A(1,3),B(1,3),A(3,2),B(3,2),A(2,1),B(2,1)) 
      DR=DR+TR
      DI=DI+TI
      TR=-TPR(A(2,1),B(2,1),A(1,2),B(1,2),A(3,3),B(3,3))
      TI=-TPI(A(2,1),B(2,1),A(1,2),B(1,2),A(3,3),B(3,3))
      DR=DR+TR
      DI=DI+TI
      TR=-TPR(A(1,1),B(1,1),A(3,2),B(3,2),A(2,3),B(2,3))
      TI=-TPI(A(1,1),B(1,1),A(3,2),B(3,2),A(2,3),B(2,3))
      DR=DR+TR
      DI=DI+TI
      DM = ALOG(RMAG(1)) + ALOG(RMAG(2)) + ALOG(RMAG(3))
      DML = SQRT(DR**2 + DI**2)
	print*,'Dr,Di,sqrt',Dr,di,DML
      IF(DML.EQ.0.) DML = 1.E-15
      DR = DR/DML
      DI = DI/DML
      DML = DM + ALOG(DML) 
	print*,'DETER,DMAG,DML',DM,DML
      RETURN
      END 
      SUBROUTINE BESSI (NMAX,XL,EI) 
C
C	RETURNS EXP(-XL)*In(XL)/XL IN EI(n+1), except n = 0
C	contains EXP(-XL)*In(XL)
C
      DIMENSION EI(99)
      IF (XL-.0001) 100,100,110 
  100 EI(1)=EXP(-XL)  
      EI(2)=.5*EI(1)  
      DO 105 N=2,NMAX 
        XN=2*N
  105   EI(N+1)=XL*EI(N)/XN 
      RETURN
  110 EI(NMAX+2)=0. 
      EI(NMAX)=1.0E-30
      EI(NMAX+1)=0. 
      DO 125 J=2,NMAX 
        I=NMAX+1-J
        XN=2*I
  115   EI(I)=XN*EI(I+1)/XL+EI(I+2) 
        IF (EI(I).LT.1.E30) GO TO 125
        R=AMIN1(.001,XL) 
        DO 120 K=I,NMAX 
  120     EI(K)=EI(K)*R 
        GO TO 115 
  125   CONTINUE
      T=EI(1) 
      DO 130 J=2,NMAX 
  130   T=T+2.*EI(J)  
      EI(1)=EI(1)/T 
      T=T*XL
      DO 135 J=2,NMAX 
  135   EI(J)=EI(J)/T 
      RETURN
      END 
      SUBROUTINE SER (N,XL,EIN,EIN1)
C
C	RETURNS EIN = EXP(-XL)*In(XL)/XL AND EXP(-XL)*In+1(XL)/XL
C	EXCEPT N = 0, WHEN DENOM IS JUST 1.
C
      XN=N  
      XL2=XL*XL/4 
      IF (N-4) 100,100,125
  100 EIN=0.
      IF (XL.LT.35.) EIN=EXP(-XL)
      IF (N-1) 105,110,115
  105 EIN1=.5*EIN 
      GO TO 145 
  110 EIN=.5*EIN
      GO TO 140 
  115 EIN=.5*EIN
      DO 120 J=2,N
        XJ=J
  120   EIN=.5*EIN*XL/XJ
      GO TO 140 
  125 IF (XL-1.0E-5) 130,135,135
  130 EIN=0 
      EIN1=0
      RETURN
  135 T=-XL+XN*ALOG(.5*XL)-(XN+.5)*ALOG(XN)-.9189385332+XN
      T1=1./(12.*XN)  
      T2=T1/(30.*XN*XN) 
      T3=T2/(3.5*XN*XN) 
      TT=T-T1+T2-T3 
      TT=AMAX1(TT,-675.)
      EIN=EXP(TT) 
  140 EIN1=EIN*XL/2./(XN+1.)
  145 T3=EIN
      T4=EIN1 
      DO 150 J=1,30 
        XJ=J
        Q=J*(J+N) 
        T3=T3*XL2/Q 
        T4=T4*XL2/(Q+XJ)
        EIN=EIN+T3
        EIN1=EIN1+T4  
        IF (EIN-1.0E+6*T3) 150,150,155
  150   CONTINUE
  155 RETURN
      END 
      SUBROUTINE F(W,WI,TP,XKZ,BZ,F0R,F0I,F1R,F1I,F2R,F2I) 
      DIV=2.6 
      AR=W-XKZ*BZ 
      B=XKZ*SQRT(2.*TP) 
        IF (ABS(AR*AR-WI*WI)-34.*B*B) 100,100,105  
  100 ZR=AR/B 
      ZI=WI/B 
      TE=-ZI*ZI+ZR*ZR 
      U=0.  
      IF (TE.LT.34.) U=EXP(-TE)
      UI=-U*SIN(2.*ZR*ZI)*1.772453851 
      UR=U*COS(2.*ZR*ZI)*1.772453851
      U=PR(UR,UI,ZR,ZI) 
      UI=PI(UR,UI,ZR,ZI)
      UR=U  
      U2R=TPR(W,WI,W,WI,UR,UI)/XKZ/XKZ
      U2I=TPI(W,WI,W,WI,UR,UI)/XKZ/XKZ
      GO TO 115 
  105 IF ((AR*AR-WI*WI).LT.0..AND.WI.LT.0.) GO TO 110 
      UR=0. 
      UI=0. 
      U2R=0.
      U2I=0.
      GO TO 115 
  110 U=EXP(28.) 
      WRITE (7,180) AR,WI,W,TP,B
      UR=WI*U 
      UI=UR 
      U2R=UR
      U2I=UR
  115 IF (AR*AR+WI*WI-(DIV*B)**2) 120,120,135 
  120 Z2R=ZR*ZR-ZI*ZI 
      Z2I=2.*ZR*ZI
      TR=Z2R
      TI=Z2I
      SR=TR+SIGN(.5,XKZ)*UI 
      SI=TI-SIGN(.5,XKZ)*UR 
      DO 125 N=1,40 
        XN=N
        T=-(TR*Z2R-TI*Z2I)/(XN+.5)
        TI=-(TR*Z2I+TI*Z2R)/(XN+.5) 
        TR=T
        SR=SR+TR
        SI=SI+TI
        IF (ABS(SR)+ABS(SI)-1.0E+6*(ABS(TR)+ABS(TI))) 125,125,130 
  125   CONTINUE
  130 F0R=-2.*SI
      F0I=2.*SR 
      F1R=(PR(W,WI,F0R,F0I)+WI)/XKZ 
      F1I=(PI(W,WI,F0R,F0I)-AR)/XKZ 
      F2R=(PR(W,WI,F1R,F1I)+BZ*WI)/XKZ
      F2I=(PI(W,WI,F1R,F1I)-BZ*AR)/XKZ
C	WRITE(7,*) F0R,F0I,F1R,F1I,F2R,F2I
      print*, 'f130',F0R,F0I,F1R,F1I,F2R,F2I
      RETURN
  135 ZR=QR(B,0.,AR,WI) 
      ZI=QI(B,0.,AR,WI) 
      YR=ZR*ZR-ZI*ZI  
      YI=2.*ZR*ZI 
      SR=QR(W,WI,AR,WI) 
      SI=QI(W,WI,AR,WI) 
      TR=-PI(SR,SI,SR,SI)*TP
      TI=PR(SR,SI,SR,SI)*TP 
      IF (WI) 140,155,140 
  140 IF (XKZ*WI) 145,145,150 
  145 C=SIGN(2.,XKZ)  
      GO TO 160 
  150 C=0.  
      GO TO 160 
  155 C=SIGN(1.,XKZ)  
  160 SR=TR+C*U2R 
      SI=TI+C*U2I+BZ*BZ 
      DO 170 N=2,30 
        XN=N
        TTR=(XN-.5)*(TR*YR-TI*YI) 
        TTI=(XN-.5)*(TR*YI+TI*YR) 
        IF (ABS(TR)+ABS(TI)-ABS(TTR)-ABS(TTI)) 175,165,165
  165   TR=TTR
        TI=TTI
        SR=SR+TR
        SI=SI+TI
        IF (ABS(SR)+ABS(SI)-1.0E+6*(ABS(TR)+ABS(TI))) 170,175,175 
  170   CONTINUE
C      WRITE (7,180) SR,SI
  175 F2R=SR
      F2I=SI
      F1R=QR(XKZ*F2R-BZ*WI,XKZ*F2I+BZ*AR,W,WI)
      F1I=QI(XKZ*F2R-BZ*WI,XKZ*F2I+BZ*AR,W,WI)
      F0R=QR(XKZ*F1R-WI,XKZ*F1I+AR,W,WI)  
      F0I=QI(XKZ*F1R-WI,XKZ*F1I+AR,W,WI)  
C      WRITE(7,*) F0R,F0I,F1R,F1I,F2R,F2I
      print*, 'f175',F0R,F0I,F1R,F1I,F2R,F2I
	RETURN
C 
  180 FORMAT (1X,6E16.8) 
      END 
      FUNCTION PR (A,B,C,D) 
      PR=A*C-B*D
      RETURN
      END 
      FUNCTION PI (A,B,C,D) 
      PI=A*D+B*C
      RETURN
      END 
      FUNCTION QR (A,B,C,D)
      CD = AMAX1(ABS(C),ABS(D))
      AT = A/CD
      BT = B/CD
      CT = C/CD
      DT = D/CD
      QR = (AT*CT+BT*DT)/(CT*CT+DT*DT)
      RETURN
      END 
      FUNCTION QI (A,B,C,D) 
      CD = AMAX1(ABS(C),ABS(D))
      AT = A/CD
      BT = B/CD
      CT = C/CD
      DT = D/CD
      QI=(BT*CT-AT*DT)/(CT*CT+DT*DT)
      RETURN
      END 
      FUNCTION TPR (A,B,C,D,E,F)
      TPR=E*(A*C-B*D)-F*(B*C+A*D) 
      RETURN
      END 
      FUNCTION TPI (A,B,C,D,E,F)
      TPI=F*(A*C-B*D)+E*(B*C+A*D) 
      RETURN
      END 
      SUBROUTINE WAVPOL (A,B,E,F,WR,WI,XKX,XKZ) 
      DIMENSION A(3,3), B(3,3), E(6), F(6), DMR(3,3), DMI(3,3)
      DM=0. 
      DO 105 I=1,3
        DO 105 J=1,3  
        N1=MOD(I,3)+1 
        N2=MOD(I+1,3)+1 
        M1=MOD(J,3)+1 
        M2=MOD(J+1,3)+1 
        DMR(I,J)=PR(A(N1,M1),B(N1,M1),A(N2,M2),B(N2,M2))-PR(A(N1,M2),B(N
     1  1,M2),A(N2,M1),B(N2,M1))
        DMI(I,J)=PI(A(N1,M1),B(N1,M1),A(N2,M2),B(N2,M2))-PI(A(N1,M2),B(N
     1  1,M2),A(N2,M1),B(N2,M1))
        DMT=ABS(DMR(I,J))+ABS(DMI(I,J)) 
        IF (DM-DMT) 100,100,105 
  100   DM=DMT
        IMAX=I
        JMAX=J
  105   CONTINUE
      I=IMAX
      J=JMAX
      N1=MOD(I,3)+1 
      N2=MOD(I+1,3)+1 
      M1=MOD(J,3)+1 
      M2=MOD(J+1,3)+1 
      E(2*J-1)=1. 
      E(2*J)=0. 
      F(1)=PR(A(N2,M2),B(N2,M2),A(N1,J),B(N1,J))-PR(A(N1,M2),B(N1,M2),A(
     1N2,J),B(N2,J))  
      F(2)=PI(A(N2,M2),B(N2,M2),A(N1,J),B(N1,J))-PI(A(N1,M2),B(N1,M2),A(
     1N2,J),B(N2,J))  
      F(3)=PR(A(N1,M1),B(N1,M1),A(N2,J),B(N2,J))-PR(A(N2,M1),B(N2,M1),A(
     1N1,J),B(N1,J))  
      F(4)=PI(A(N1,M1),B(N1,M1),A(N2,J),B(N2,J))-PI(A(N2,M1),B(N2,M1),A(
     1N1,J),B(N1,J))  
      E(2*M1-1)=-QR(F(1),F(2),DMR(I,J),DMI(I,J))
      E(2*M1)=-QI(F(1),F(2),DMR(I,J),DMI(I,J))
      E(2*M2-1)=-QR(F(3),F(4),DMR(I,J),DMI(I,J))
      E(2*M2)=-QI(F(3),F(4),DMR(I,J),DMI(I,J))
      T=0.  
      DO 110 N=1,6
  110   T=T+E(N)**2 
      T=SQRT(T) 
      DO 115 N=1,6
  115   E(N)=E(N)/T 
      F(1)=-XKZ*QR(E(3),E(4),WR,WI) 
      F(2)=-XKZ*QI(E(3),E(4),WR,WI) 
      F(3)=XKZ*QR(E(1),E(2),WR,WI)-XKX*QR(E(5),E(6),WR,WI)
      F(4)=XKZ*QI(E(1),E(2),WR,WI)-XKX*QI(E(5),E(6),WR,WI)
      F(5)=XKX*QR(E(3),E(4),WR,WI)
      F(6)=XKX*QI(E(3),E(4),WR,WI)
      RETURN
      END 
