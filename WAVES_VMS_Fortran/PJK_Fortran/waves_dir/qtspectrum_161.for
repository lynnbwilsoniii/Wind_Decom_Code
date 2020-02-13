	PROGRAM QTSPECTRUM
C
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
        DIMENSION RATIOM(10),DENS(10),BETAZ(10),TPAR(10),TPERP(10)
        REAL*4 BETAPAR(10),BETAPERP(10)
        DIMENSION DIELR(10,3,3),DIELI(10,3,3),EPSR(3,3),EPSI(3,3)
C        DIMENSION CAPKS(45)
        LOGICAL NAME(70)
        COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS  
        COMMON /DATABL/ XD(75)
C	COMMON /INTBLK/ XKZ,W
	EXTERNAL ARGMENT,RKQS
      EQUIVALENCE (XD(1),WR),(XD(2),WI),(XD(3),WCYCL),(XD(4),XKX)
      EQUIVALENCE (XD(5),XKZ),(XD(7),RW),(XD(8),RKX),(XD(9),RKZ)
      EQUIVALENCE (XD(13),END1),(XD(14),END2),(XD(74),FP)
      DATA NAME /'  WR','  WI',' WCE','  KX','  KZ',' KIT','  RW',
     1' RKX',' RKZ','MODE',' NPR','  NC','END1','END2',6*'    ',
     2 10*'RMAS',10*'DENS',10*'BETA',10*'TPAR',10*'TPRP'/
        DATA TWOPI /6.2831853/
	DATA FP /25.E3/
	DATA CLIGHT /2.9979E8/
	DATA NPR /0/
C
	OPEN(UNIT=3,NAME='QTDATA.DAT',TYPE='OLD')
	READ (3,106) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
	MODE = 0
C	PRINT   106, WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
	WRITE(6,105) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
 105	FORMAT(8E12.5,3I3)
 106	FORMAT(5F10.5,3F5.3,3I2)
	READ(3,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
C	PRINT  101,(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
	WRITE(6,1011)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
 101	FORMAT(5E10.4)
 1011	FORMAT(5E12.4)
	CLOSE(UNIT = 3)
	KIT = 1
        XD(12) = NC + .5
        XD(6) = KIT + .5
        XD(10) = MODE + .5
        XD(11) = NPR + .5
	RW = 1.
	RKX = 1.
	RKZ = 1.
C
C	test rbar
c	do n = 1,100
c	  xkl = .1*n
c	  rtest = rbar(xkl)
c         write(88,*) n,xkl,rtest
c	enddo
C
C	CALCULATE XK0, ESTIMATED XKZ FOR MAX OF INTEGRAND
C
	XK0 = 1./SQRT(TPAR(2))
	XD(75) = XK0
C
C	THERE IS A PROBLEM, IN THAT THERE CAN BE ROOTS OF CAPKR = 0. IN A 
C	REGION WHERE CAPKI IS SO SMALL THAT IT IS SET TO ZERO.  SO I HAVE TO 
C	TREAT THOSE ROOTS OF CAPKR SEPARATELY.  
C
C	ALENGTH = 6.
C	wr = 1.02
	wi = 0.
	SAVE = 0.
	do n = 1,99
	  x = n/100.
	  call argment(x,pwr,arg)
C	  print*,'n,x,arg',n,x,arg
	  XK1 = XK0*X/(1.-X)
          CALL DISPES (NC,KIT,MODE,NPR,CAPKR,CAPKI)
	  CAPKR = CAPKR/XK1**2
	  CAPKI = CAPKI/XK1**2
	  IF(CAPKR*SAVE.LT.0.) THEN
	    PRINT*,'CAPKR ROOT,XK,OLD,R,I',XK1,SAVE,CAPKR,CAPKI
	  ENDIF
	  SAVE = CAPKR
CC	  K IN /METER FOR RBAR
	  XKM = XK1*TWOPI*FP/CLIGHT
c	  write(88,*) n,x,RBAR(XKM*ALENGTH),arg
	enddo
c	if(1) stop
c
	PRINT*,'XK0',XK0
c	PRINT*,'GOING TO ARGMNT,W,XKZ',W,XK
c	temp = argmnt(w,xkz)
c	PRINT*,'ARGMNT',TEMP
C
	X1 = .001
	X2 = .999
	H1 = .25*(X2-X1)
C	HMIN = 3.E-5
	HMIN = 0.
	EPS = 3.E-3
	DO NW = 1,103
	  W = .975 + .01*NW
	  W = -W
	  XD(1) = W
	  XD(2) = 0.
	  PWR = 0.
	  CALL ODEINT(PWR,1,X1,X2,EPS,H1,HMIN,NOK,NBAD,ARGMENT,RKQS)
	  WRITE(87,*) ABS(W),PWR
	  PRINT*,'W,PWR',W,PWR
C	  if(1) stop	
	ENDDO
	STOP
	END
	SUBROUTINE ARGMENT(X,PWR,ARGMNT)

        DIMENSION RATIOM(10),DENS(10),BETAZ(10),TPAR(10),TPERP(10)
        REAL*4 BETAPAR(10),BETAPERP(10)
        DIMENSION DIELR(10,3,3),DIELI(10,3,3),EPSR(3,3),EPSI(3,3)
        COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS
        COMMON /DATABL/ XD(75)
C	COMMON /INTBLK/ XKZ,W
        EQUIVALENCE (XD(1),WR),(XD(2),WI),(XD(3),WCYCL),(XD(4),XKX)
        EQUIVALENCE (XD(5),XKZ),(XD(7),RW),(XD(8),RKX),(XD(9),RKZ)
C	EQUIVALENCE (XD(6),KIT),(XD(11),NPR),(XD(12),NC)
        EQUIVALENCE (XD(13),END1),(XD(14),END2),(XD(74),FP)
	DATA CLIGHT /2.9979E8/
	DATA EM /9.11E-31/
	DATA SICONV /8.988E9/
	DATA ALENGTH /6./
C	DATA ALENGTH /50./
        DATA TWOPI /6.2831853/
C
	MODE = 0
	NC = XD(12)
	KIT = XD(6)
	NPR = XD(11)
	XK0 = XD(75)
	XK = X*XK0/(1.-X)
	XKZ = XK
	XKX = 0.
C	K IN /METER FOR RBAR
	XKM = XK*TWOPI*FP/CLIGHT
C	PRINT*,'GOING TO CALL DISPES,NC,XK=',NC,XK
        CALL DISPES (NC,KIT,MODE,NPR,CAPKR,CAPKI)
	CAPKR = CAPKR/XKZ**2
	CAPKI = CAPKI/XKZ**2
C	PRINT*,'INTEGRAND, CAPK',CAPKR,CAPKI
C	DO SPECIES DEPENDENT PARTS OF EQ 14, KELLOGG, PLASMA PHYSICS 23,735,1981
	SUM = 0.
	W = XD(1)
	DO N = 1,NC
	  EXPON = -.5*(W/XK - BETAZ(N))**2/TPAR(N)
	  SUM = SUM + ABS(RATIOM(N))*SQRT(TPAR(N))*(1.-XK*BETAZ(N)/W)*
     1		(DENS(N)/ABS(RATIOM(N))/TPAR(N))*EXP(EXPON)
c	print*,'n,xkz,sum',n,xkz,capkr,sum
	ENDDO
C	THIS INTEGRAND WAS IN CGS, BUT GOT TOO CLOSE TO 10^-37, SO NOW SI
C	THE THERMAL NOISE FORMULA COMPUTES AN ENERGY.  IT IS MOSTLY 
C	DIMENSIONLESS, BUT I PUT ELECTRON MASS AND VELOCITY OF LIGHT IN SI.
C	SO THEN THE CONVERSION FACTOR TO SI ELECTRIC FIELD IS JUST 
C	1/4 PI EPS0 
C	
	ARGMNT = SICONV*EM*CLIGHT*(4./SQRT(TWOPI))*SUM*RBAR(XKM*ALENGTH)/
     1		(CAPKR**2+CAPKI**2)/XK0/XK/X**2
C	print*,'arg,x,xkz,argmnt',X,XKZ,ARGMNT
C	  write(88,*) n,x,RBAR(XKM*ALENGTH),argmnt
C	  write(89,*) n,x,XK,CAPKR,CAPKI
C	  write(89,1089) x,XK,rbar(xkm*alength),CAPKR,CAPKI,sum,argmnt
 1089	  format(f7.4,6e12.3)
	RETURN
	END
	FUNCTION RBAR(XKL)
	  CALL CISI(XKL,CI1,SI1)
	  CALL CISI(2.*XKL,CI2,SI2)
	  RBAR = 0.
	  IF(XKL.LE.1.E-6) THEN
	    RBAR = (2./3.)*XKL**2
	  ELSE
	    RBAR = 8.*(-(1.-COS(XKL))**2/XKL + 2.*SI1 - SI2)/XKL
	  ENDIF
	RETURN
	END
C      subroutine DISPER
C
C	COPIED FROM OSCARS, 30 JUNE 2006
C
      SUBROUTINE DISPES (NC,KIT,MODE,NPR,CAPKR,CAPKI)
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
      DIMENSION RATIOM(10), DENS(10), BETAZ(10), TPAR(10), TPERP(10), DI
     1ELR(10,3,3), DIELI(10,3,3), EPSR(3,3), EPSI(3,3), CAPKS(45) 
      DIMENSION DEL_NR(10),DEL_NI(10)
      DIMENSION ROT(3,3), EPSKR(3,3), EPSKI(3,3)
      DIMENSION DT(45), DRV(45), VAR(45), DML(45)  
      DIMENSION XKS(45), NTRY(45) 
      DIMENSION E(6), B(6)
      COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS  
      COMMON /DATABL/ XD(75)
      EQUIVALENCE (XD(1),WR),(XD(2),WI),(XD(3),WCYCL),(XD(4),XKX)
      EQUIVALENCE (XD(5),XKZ),(XD(7),RW),(XD(8),RKX),(XD(9),RKZ)
C      EQUIVALENCE (XD(6),KIT),(XD(11),NPR),(XD(12),NC)
      EQUIVALENCE (XD(13),END1),(XD(14),END2)
      DATA TWOPI /6.2831853/
      DATA IPRN /0/
C
C      PRINT*,'STARTING DISPES,NC,WR,XKZ =',NC,WR,XKZ
      MCIR=MODE/3 
      MODE=MOD(MODE,3)
      IF(NPR.GT.0) THEN
	WRITE (7,280) 
        DO 90 I=1,NC
	 PRINT 270,I,RATIOM(I),DENS(I),BETAZ(I),TPAR(I),
     1   TPERP(I) 
   90   WRITE (7,270) I,RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I) 
        WRITE (7,282)
      ENDIF 
      DO 100 I=1,NC 
        WP=SQRT(ABS(DENS(I)/RATIOM(I))) 
        WC=WCYCL/RATIOM(I)
        VZT=BETAZ(I)/SQRT(TPAR(I))
        TTE=TPAR(I)*RATIOM(I)/TPAR(1) 
  100   IF(NPR.GT.0) WRITE (7,270) I,WP,WC,VZT,TTE 
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
       IF(LZ.EQ.1.AND.NPR.GT.0) WRITE (7,351) 6HRE(W) ,6HIM(W) ,
     1  7HLN(DET),7HRE(DET),7HIM(DET),7H CAPKR ,7H CAPKI ,6HMAG(K),3HTRY 
C       IF(LZ.EQ.1)    PRINT 351, 6HRE(W) ,6HIM(W) ,7HLN(DET),7HRE(DET),
       IF(LZ.EQ.1.AND.NPR.GT.0) WRITE (6,351) 6HRE(W) ,6HIM(W) ,
     1  7HLN(DET),7HRE(DET),7HIM(DET),7H CAPKR ,7H CAPKI ,6HMAG(K),3HTRY 
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
      IF(NPR.GT.0) THEN
        WRITE (7,360) VAR(1),VAR(2),DML(K),DT(1),DT(2),CAPKR,CAPKI,XK
        WRITE (6,360) VAR(1),VAR(2),DML(K),DT(1),DT(2),CAPKR,CAPKI,XK
	WRITE (8,361) VAR(1),VAR(2),DML(1),DT(1),DT(2),XKX,XKZ
C        PRINT    360, VAR(1),VAR(2),DML(K),DT(1),DT(2),CAPKR,CAPKI,XK
      ENDIF
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
	  print*,'after do 145, k,w=',k,var(k),var(k+1) 
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
	if(abs(dml(k-2)-dml(k)).gt.17.) print*,'fact',dml(k-2),dml(k)
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
	  IF(A.EQ.0.) THEN
	    PRINT*,'A EQ 0 IN DISPES,VAR,C,D=',VAR(K),VAR(K+1),C,D
	    GO TO 215
	  ENDIF
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
 	IF(LZ.EQ.1) THEN
		XKS(1) = XKX
	ELSE
		XKS(1) = XK
	ENDIF
        XKS(3)=XKS(1) 
        XKS(5)=XKS(1)*DKMAG 
        NTRY(1)=1 
        NTRY(3)=2 
        XKX=XKS(1)*SIN(ANG/57.295779518)  
        XKZ=XKS(1)*COS(ANG/57.295779518)  
        CALL EPSILN (VAR(1),VAR(2),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR,DIE
     1  LI) 
        CALL DETER (EPSR,EPSI,DT(1),DT(2),DMAG,DML(1)) 
        CALL EPSILN (VAR(3),VAR(4),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR,DIE
     1  LI) 
        CALL DETER (EPSR,EPSI,DT(3),DT(4),DMAG,DML(3)) 
        DO 205 K=5,LL,2 
          XKX=XKS(K)*SIN(ANG/57.295779518)
          XKZ=XKS(K)*COS(ANG/57.295779518)
	  XK = XKS(K)
          NTRY(K)=(K+1)/2 
          CALL EPSILN (VAR(K),VAR(K+1),WCYCL,XKX,XKZ,NC,EPSR,EPSI,DIELR
     1    ,DIELI) 
          CALL DETER (EPSR,EPSI,DT(K),DT(K+1),DMAG,DML(K)) 
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
	WRITE (18,*) ATAN2D(XKX,XKZ),-VAR(K+1)/VAR(K)
        VPX=XKX*VAR(NF-3)/XK2 
        VPZ=XKZ*VAR(NF-3)/XK2 
        XN2=XK2/(VAR(NF-3)**2-VAR(NF-2)**2) 
	WIWR = 0.
        IF(VAR(NF-3).NE.0.) WIWR=VAR(NF-2)/VAR(NF-3)
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
c
c	..STUART'S attempt to calculate density fluctuations from the continuity 
c		equation	11/23/92
c
c
c	..fixed up on 6/18/93
c
c
	emag = sqrt(e(1)**2. + e(2)**2. + e(3)**2. + e(4)**2. + e(5)**2.
	1		+ e(6)**2.) 
c
c	print*,'emag = ',emag		!this should be 1
c
c
c********************
c	ALL, N = 1,NC
c		density is derived from new Stix p 262  (1992)
c		- w sigma + k . j = 0 for each species
c			where sigma is charge density
c		then k . j = -i (w/fourpi) k . chi . E		eq 1-5 
c	so 	sigma = -i/fourpi   k . chi . E       = q del_n
c		and the dispersion relation is eps = 0 = 1 + sum (chi)
c
c	or new Stix eq 10.75, which is above, directly
c
c	then I want to normalize to del_n for isothermal electrons:
c	
c		del_n = n0 e phi/kte del phi
c
c		which means calculating -1 sign(q) (kc/wp) . chi . E
c			times kTe/me c**2
c
c	..first calculate the complex matrix product -i k . chi . E
c
	EDOTKR = (E(1)*XKX + E(5)*XKZ)/(XKX**2 + XKZ**2)
	EDOTKI = (E(2)*XKX + E(6)*XKZ)/(XKX**2 + XKZ**2)
	IF(EDOTKR.EQ.0..AND.EDOTKI.EQ.0.) THEN
C
	     PRINT*,'NO CHARGE SEPARATION'
	     WRITE(7,*) 'NO CHARGE SEPARATION'
	     DO N = 1,NC
	        DEL_NR(N) = 0.
		DEL_NI(N) = 0.
	     ENDDO
C
	ELSE
	 rhototr = 0.
	 rhototi = 0.
 	 DO N = 1,NC
	  del_nr(N) = 0.
	  del_ni(N) = 0.
	   do j=1,3
	        jj = 2*j-1
		del_nr(N) = del_nr(N) + 			!real part
     1		XKX*(dieli(N,1,j)*E(jj) + dielr(N,1,j)*E(jj+1)) 
     1		+ XKZ*(dieli(N,3,j)*E(jj)+ dielr(N,3,j)*E(jj+1))
c
		del_ni(N) = del_ni(N) -				!imaginary part 
     1		XKX*(dielr(N,1,j)*E(jj) - dieli(N,1,j)*E(jj+1))
     1		- XKZ*(dielr(N,3,j)*E(jj) - dieli(N,3,j)*E(jj+1))
c
	   end do
c
c	..then include the real scalars -- noting that my dielr is
c		w**2 times Stix's
c
c
	  WW2R = PR(VAR(NF-1),VAR(NF),VAR(NF-1),VAR(NF)) 
	  WW2I = PI(VAR(NF-1),VAR(NF),VAR(NF-1),VAR(NF)) 
C
	  TEMP      = QR(DEL_NR(N),DEL_NI(N),WW2R,WW2I)
	  DEL_NI(N) = QI(DEL_NR(N),DEL_NI(N),WW2R,WW2I)
	  DEL_NR(N) = TEMP
C
c	now normalize
c
c	  del_nr(N) = del_nr(N)*ratiom(N)*tpar(1)/abs(ratiom(N))
c	  del_ni(N) = del_ni(N)*ratiom(N)*tpar(1)/abs(ratiom(N))
c	  TEMP      = QR(del_nr(n),del_ni(n),EDOTKR,EDOTKI)
c	  del_ni(N) = QI(del_nr(n),del_ni(n),EDOTKR,EDOTKI)
c	  del_nr(n) = temp
Cc
C	THEN I DECIDED, ON 23 OCT 2004, TO CHANGE NORMALIZATION TO
C	DEL N / ( i K DOT E/4PI), I.E. THE VALUE OF DEL N WHICH WOULD GIVE
C	DIV E ALL BY ITSELF.  BUT HAVENT DONE IT YET.--  done 10 dec 2004
C	MULTIPLY BY -i/fourpi gives charge density, not number density
C
	  del_nr(N) = .5*del_ni(N)/twopi
	  del_ni(N) =-.5*del_nr(N)/twopi
	  rhototr = rhototr + del_nr(N)
	  rhototi = rhototi + del_ni(n)
	  PRINT*,'DEL_N',N,DEL_NR(N),DEL_NI(N)
	 ENDDO
	ENDIF
	RHOTOT = SQRT(RHOTOTR**2 + RHOTOTI**2)
	  NN = MIN0(NC,4)
	  print*,'rho',(del_nr(n),del_ni(n),n=1,2),rhotot
	  WRITE(27,727),ANG,VAR(NF-1),(DEL_NR(N),DEL_NI(N),N=1,NN),RHOTOT
 727	 FORMAT(F6.1,E12.3,4E12.3,F5.2)
c
c****************    END STUART'S ADDITION
C
	RATIO_N = 0.
	IF(DEL_NR(1).NE.0..OR.DEL_NI(1).NE.0.) THEN
	  RATIO_N = CABS(CMPLX(DEL_NR(2),DEL_NI(2)))/
     1		CABS(CMPLX(DEL_NR(1),DEL_NI(1)))
	ENDIF

C	PRINT E AND B IN THE X,Y,Z  (Z = B DIRECTION) SYSTEM
C
C********************
C		A SECTION TO WRITE PARAMETERS OF INTEREST IN STUDY
C			OF MODES OF MAG FLUCTUATIONS IN THE SOLAR WIND
C
C		ADD UP DENSITIES OF ELECTRONS , PROTONS, HEAVY IONS
C			CALLED EDENS,HDENS,HIDENS
C
	EDENSR = 0.
	HDENSRR = 0.
	HIDENSR = 0.
	EDENSI = 0.
	HDENSI = 0.
	HIDENSI = 0.
	DO N = 1,NC
	  IF(RATIOM(N).LT.0.) THEN
	    EDENSR = EDENSR + DEL_NR(N)
	    EDENSI = EDENSI + DEL_NI(N)
	  ELSEIF(RATIOM(N).GT.2700.) THEN
	    HIDENSR = HIDENSR + DEL_NR(N)
	    HIDENSI = HIDENSI + DEL_NI(N)
	  ELSE
	    HDENSR = HDENSR + DEL_NR(N)
	    HDENSI = HDENSI + DEL_NI(N)
	  ENDIF
	ENDDO
C
C	write	FOR015, wr,wi,/k/,ang,Bz,|B|,6 densities, ratio edens/Bz
C
	Bmag = sqrt(B(1)**2 + B(2)**2 + B(3)**2 + B(4)**2 + B(5)**2
     1		+ B(6)**2.) 
	IF(B(5).NE.0..OR.B(6).NE.0.) THEN
	  DBRATIOR = QR(EDENSR,EDENSI,B(5),B(6))
	  DBRATIOI = QI(EDENSR,EDENSI,B(5),B(6))
	ELSE	
	  DBRATIOR = 0.	
	  DBRATIOI = 0.
	ENDIF
C
c	IF(NPR.GE.3) 
c     1	WRITE(15,1015) VAR(NF-1),VAR(NF),XK,ANG,B(5),B(6),BMAG,EDENSR,
c     2	EDENSI,HDENSR,HDENSI,HIDENSR,HIDENSI,DBRATIOR,DBRATIOI
C
 1015	FORMAT(E11.3,E11.3,E10.3,F8.2,11F8.2)
C 
        WRITE(7,315) (E(I),I=1,6) 
        WRITE(7,320) (B(I),I=1,6) 
C
	IF(NPR.GE.3) THEN
	  WRITE(7,*) ' '
  	  DO N = 1,NC
	    WRITE(7,*) 'DEL_N',N,DEL_NR(N),DEL_NI(N)
	  ENDDO
	ENDIF
C
C	CALCULATE TRANSVERSE E AND B
C
	ETOT = E(1)**2 + E(2)**2 + E(3)**2 + E(4)**2 + E(5)**2 + E(6)**2
	ETOT = SQRT(ETOT)
	REEDOTK = E(1)*XKX + E(5)*XKZ
	CMEDOTK = E(2)*XKX + E(6)*XKZ
	ETRANS2 = REEDOTK**2 + CMEDOTK**2
	BTOT = B(1)**2 + B(2)**2 + B(3)**2 + B(4)**2 + B(5)**2 + B(6)**2
	BTOT = SQRT(BTOT)
	XKPR = SQRT(XKX**2 + XKZ**2)
	IF(NPR.GE.3) WRITE(13,*) XKPR,VAR(NF-3),ETOT,ETRANS,BTOT
C
C	CALCULATE E,B AND EPS IN THE Y X K, Y, K SYSTEM
C
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
       XK = SQRT(XKX**2+XKZ**2)
       CALL WAVPOL (EPSKR,EPSKI,E,B,VAR(NF-3),VAR(NF-2),0.,XK) 
       WRITE(7,330) (E(I),I=1,6) 
       WRITE(7,335) (B(I),I=1,6) 
C
C	CALCULATE THE AUGMENTED STOKES PARAMETERS, I,Q,U,V and EL,PHEL
C	IN THIS, THE TRANSVERSE INTENSITY, the first Stokes parameter,
C	is taken as unity.  The principle direction is in the X-B
c	plane, (Y X K direction) so that U = 0.  	
c	Then Q and V have the same information
C	so only Q is useful.  A fifth "Stokes" parameter is EL, the
C	ratio of the longitudinal E to total E (both squared in
C	field strength) and a sixth is PHEL, the phase of longitudinal E 
C	relative to the component in the X-B plane,
C
	ETRANS = E(1)**2 + E(2)**2 + E(3)**2 + E(4)**2
	BTRANS = B(1)**2 + B(2)**2 + B(3)**2 + B(4)**2
	TPH = 0.
C	Tph = relative phase of transverse components
	IF(E(1).NE.0..OR.E(2).NE.0.) THEN
	  TPHR = QR(E(3),E(4),E(1),E(2))
C	  EYXK = SQRT(E(1)**2 + E(2)**2)
C	  EY   = SQRT(E(3)**2 + E(4)**2)
	  TPHI = QI(E(3),E(4),E(1),E(2))
	  TPH = ATAN2D(TPHI,TPHR)
	ENDIF
	STOKESQ = (E(1)**2 + E(2)**2 - E(3)**2 - E(4)**2)
	STOKESBQ = (B(1)**2 + B(2)**2 - B(3)**2 - B(4)**2)
	IF(BTRANS.NE.0.) STOKESBQ = STOKESBQ/BTRANS
	IF(ETRANS.NE.0.) THEN
		STOKESQ = STOKESQ/ETRANS
		FLONG = (E(5)**2 + E(6)**2)/(ETRANS+E(5)**2 + E(6)**2)
	ENDIF
	RATIOEB = 0.
	IF(BTRANS.NE.0.) RATIOEB = SQRT((ETRANS+E(5)**2 + E(6)**2)/BTRANS)
	IF(E(1).NE.0..OR.E(2).NE.0.) THEN
	  PHR = QR(E(5),E(6),E(1),E(2))
	  PHI = QI(E(5),E(6),E(1),E(2))
	  PHEL = 0.
	  IF(FLONG.NE.0.) PHEL = ATAN2D(PHI,PHR)
	ELSE
	  PHEL = 0.
	ENDIF
	IF(MODE.LE.2) ANG = ATAN2D(XKX,XKZ)
	IF(IPRN.EQ.0) THEN
	  WRITE(9,401)
 401	  FORMAT(14X,'FREQ',14X,'XK',7X,'ANG',4X,'Q',3X,'Tph',2X
     1	'Ilong/Itot',2X,'EL ph', ' ni/ne')
	  IPRN = 1
	ENDIF
	WRITE(9,402) VAR(Nbest),VAR(Nbest+1),XK,ANG,STOKESQ,TPH,FLONG,
     1		PHEL,RATIO_N
C 402	FORMAT(E15.7,E11.3,E11.4,F7.1,F6.2,F5.0,E11.4,F7.1,F6.2)
C 402	FORMAT(E15.7,E11.3,E11.4,F7.1,F6.2,F6.0,F11.8,F7.1,F6.2)
 402	FORMAT(E15.7,E11.3,E11.4,F7.1,F6.2,F6.0,F11.8,F7.1,F6.3)
C
	IF(B(1).NE.0..OR.B(2).NE.0.) THEN
	  TPHR = QR(B(3),B(4),B(1),B(2))
C	  EYXK = SQRT(E(1)**2 + E(2)**2)
C	  EY   = SQRT(E(3)**2 + E(4)**2)
	  TPHI = QI(B(3),B(4),B(1),B(2))
	  TPHB = ATAN2D(TPHI,TPHR)
	ENDIF
	WRITE(14,414)  VAR(Nbest),VAR(Nbest+1),XK,ANG,STOKESBQ,TPHB,
     1		RATIOEB
 414	FORMAT(E14.5,E14.5,E13.4,F7.1,F8.3,F7.1,E10.3)
C
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
  290 FORMAT (//,8X,9H W/WPE = E11.5,4H +I E11.5,  
     1 8X,10H WCE/WPE =, E11.5,/10H KXC/WPE =,E11.5, 
     2 8X,10H KZC/WPE =,E11.5,//) 
  295 FORMAT (//,8X,23H PHASE VELOCITY  VPX = E11.5,6H VPZ =E11.5/, 
     1 8X,21H     REF. INDEX  N2 =,E11.5,8H WI/WR =,E10.4//)  
  300 FORMAT (//,8X,19H RESIDUE OF K2*KL =,E14.5,2H+I,E14.5/, 
     18X,8H OF KL =,E14.5,2H+I,E14.5,//)  
  305 FORMAT (16H POLARIZATIONS  )
  310 FORMAT (//25H DISPERSION DETERMINANT  //) 
  315 FORMAT (10H EX/EY/EZ=3(E11.3,3H +IE10.3)) 
  320 FORMAT (10H BX/BY/BZ=3(E11.3,3H +IE10.3)) 
  325 FORMAT (//46H DISPERSION DETERMINANT IN Y X K, Y, K SYSTEM //)
  330 FORMAT (10H E1/EY/EK=3(E11.3,3H +IE10.3)) 
  335 FORMAT (10H B1/BY/BK=3(E11.3,3H +IE10.3)) 
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
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
      DIMENSION RATIOM(10), DENS(10), BETAZ(10), TPAR(10), TPERP(10), DI
     1ELR(10,3,3), DIELI(10,3,3), EPSR(3,3), EPSI(3,3), POLR(3,3), POLI(
     2 3,3)  
      COMMON /DISBLK/ RATIOM,BETAZ,TPAR,TPERP,DENS  
C      COMMON /DATABL/ XD(75)
C
C	this routine returns w^2 times epsilon, 
C	as in D = epsilon dot E, for a plasma.  
C	It sums the contributions from each
C	plasma component, DIEL(N,I,J), together with the
C	vacuum part, 1. - ??.  The DIEL are calculated in
C	SUBROUTINE POLARZ
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
C 	  WRITE(7,7000) N,I,J,IJ,POLI(I,J)
C 7000	  FORMAT(' INDICES'4I4,2E15.6)
          DIELR(N,I,J)=POLR(I,J)  
          EPSR(I,J)=EPSR(I,J)+POLR(I,J)  
 101	CONTINUE
	DO 102 I=1,3
	  IJ=I
	  DO 102 J=IJ,3
	  DIELI(N,I,J)=POLI(I,J)
	  EPSI(I,J)=EPSI(I,J)+POLI(I,J)
 102	CONTINUE
 105  CONTINUE
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
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
C
C	this routine calculates POL, the contributions of a given
C	plasma component to w^2 epsilon
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
C	this entry treats the magnetic field as zero
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
C	call n = 0 separately, as EI(1) is not divided by lambda 
      CALL TERM (WR,WI,WP2,WC,XKX,XKZ,1.,BZ,TP,TPER,0,EI(1),A,POLR,POLI,
     1TER,TEI,SF) 
      POLR(2,2)=XL*POLR(2,2)
      POLI(2,2)=XL*POLI(2,2)
      DO 130 M=1,NMAX 
	L=M	! subscript of I, Bessel function, note n=0 not done here
        LAST=M
	XN = L	
C	on 16 May 2006, I changed to a formula which does not use Izero
C	EIP is (I - Iprime), not divided by lambda
c
c        IF (L-1) 115,115,120
c  115   EIP=XL*(EI(2)-.5*EI(3))-.5*EI(1)  
c        GO TO 125 
c  120   EIP=XL*(EI(L+1)-.5*(EI(L)+EI(L+2))) 
c
c  115   EIP=EI(1) - XL*EI(2)  		!! erroneous correction 16 may 2006
c        GO TO 125 
c  120   EIP=(XL-XN)*EI(L)-XL*EI(L+1) 
c
	EIP = (XL-XN)*EI(L+1) - XL*EI(L+2)
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
C	this routine adds one term, n, for one component, to the dispersion
C		matrix, 
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
      DIMENSION TER(3,3), TEI(3,3), POLR(3,3), POLI(3,3)
      XN=N  
      W=WR+XN*WC
      AR=W-XKZ*BZ   		 ! numerator of alpha
c	Fried&Conte Z(z) = iF0(z);  AFO is alpha F0(alpha)
      CALL F (W,WI,TP,XKZ,BZ,AFOR,AFOI,AF1R,AF1I,AF2R,AF2I) 
      VR=BZ*AF1R-AF2R 
      VI=BZ*AF1I-AF2I 
      UR=BZ*AFOR-AF1R 
      UI=BZ*AFOI-AF1I 
      TR=-PR(VR,VI,W,WI)/TP-XN*WC*AF2R/TPER 
      TI=-PI(VR,VI,W,WI)/TP-XN*WC*AF2I/TPER 
      SR=QR(TR,TI,WR,WI)
      SI=QI(TR,TI,WR,WI)
C	now S = ((Vz/c)*aF1 - aF2)/Tpar - n (wc/w) aF2/Tper
      C=WP2*EI
      DR=QR(AR,WI,WR,WI)
      DI=QI(AR,WI,WR,WI)
      TER(3,3)=-C*QI(SR,SI,DR,DI)*XL
      TEI(3,3)=C*QR(SR,SI,DR,DI)*XL 
      TR=XKZ*(TPER*UR/TP+AF1R)  
      TI=XKZ*(TPER*UI/TP+AF1I)  
      SR=QR(TR,TI,WR,WI)-AFOR 
      SI=QI(TR,TI,WR,WI)-AFOI 
C	theta is (kTperp/mc^2)*(w/kz c) times Stix (old) Eq 12 p 190
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
c******pjk
c	print*,'n,c,cp,ter(1,3),tei(2,3)',xn,c,cp,ter(1,3),tei(2,3)
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
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
C	THIS ROUTINE RETURNS THE DETERMINANT OF AIN + I*BIN EXPRESSED AS
C	DR +I*DI = (DET)/NORM AND LN(NORM) = DML,  DM IS AN ESTIMATE OF
C	THE LOGARITHM OF THE LARGEST TERM
      DIMENSION A(3,3), B(3,3),AIN(3,3),BIN(3,3),RMAG(3)
      DO 1 I=1,3
        RMAG(I) = 0.
        DO 2 J=1,3
   	  A(I,J) = AIN(I,J)
   	  B(I,J) = BIN(I,J)
          AM = ABS(AIN(I,J)) + ABS(BIN(I,J))
 2        RMAG(I) = AMAX1(AM,RMAG(I))
	IF(RMAG(I).NE.0.) THEN
          DO 3 J=1,3
		A(I,J) = A(I,J)/RMAG(I)
          	B(I,J) = B(I,J)/RMAG(I)
C	  WRITE(7,*) A(I,J),B(I,J),RMAG(I),AIN(I,J),BIN(I,J)
 3	  CONTINUE
	ELSE
		DO IJ = 1,3
		  PRINT*,(A(IJ,J),J=1,3)
		  PRINT*,(B(IJ,J),J=1,3)
		ENDDO
		DR = 0.
		DI = 0.
		DL = -60.
		RETURN
	ENDIF
 1      CONTINUE
      DR=TPR(A(1,1),B(1,1),A(2,2),B(2,2),A(3,3),B(3,3)) 
      DI=TPI(A(1,1),B(1,1),A(2,2),B(2,2),A(3,3),B(3,3)) 
      TR=-TPR(A(3,1),B(3,1),A(2,2),B(2,2),A(1,3),B(1,3))
      TI=-TPI(A(3,1),B(3,1),A(2,2),B(2,2),A(1,3),B(1,3))
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
      IF(DML.EQ.0.) DML = 1.E-15
      DR = DR/DML
      DI = DI/DML
      DML = DM + ALOG(DML) 
C	print*,'DETER,DMAG,DML',DM,DML
      RETURN
      END 
      SUBROUTINE BESSI (NMAX,XL,EI) 
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
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
c	write(44,*) 'b',xl,(ei(j),j=1,4)
      RETURN
      END 
      SUBROUTINE SER (N,XL,EIN,EIN1)
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
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
c	write(44,*) 's',xl,(ein,ein1)
  155 RETURN
      END 
      SUBROUTINE F (W,WI,TP,XKZ,BZ,F0R,F0I,F1R,F1I,F2R,F2I) 
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
C
C	THIS RETURNS ZETA*F0 ETC, FROM STIX FIRST BOOK, ZETA IS ARGUMENT
C	iF0 is Fried and Conte Z, zeta is zr,zi here
C	Note that, on call, W(here) is w + n wc (actually)
C	 
      DOUBLE PRECISION Z2R,Z2I,T,TR,TI,SR,SI,XN5,TTR,TTI,UR,UI,U,DTE,W2R,W2I
      PARAMETER(DIV=4.55)
C      PARAMETER(DIV=2.85)		! USED UNTIL 30 APRIL 2002
C 
      AR=W-XKZ*BZ 
      B=XKZ*SQRT(2.*TP) 
        IF (ABS(AR*AR-WI*WI)-80.*B*B) 100,100,105  
  100 ZR=AR/B 
      ZI=WI/B 
      DTE=-DPROD(ZI,ZI)+DPROD(ZR,ZR) 
      TE=DTE 
      U=0.D00  
      IF (ABS(TE).LT.80.) U=DEXP(-DTE)
C	print*,'f,100,tp,u',tp,u
      UI=-U*DSIN(DBLE(2.*ZR*ZI))*1.772453850905516D00 
      UR=U*DCOS(DBLE(2.*ZR*ZI))*1.772453850905516D00
C	print*,'init u,ur,ui',u,ur,ui
C      U=PR(UR,UI,ZR,ZI) 
C      UI=PI(UR,UI,ZR,ZI)
C      UR=U  
      U=UR*DBLE(ZR) - UI*DBLE(ZI)
      UI=UR*DBLE(ZI) + UI*DBLE(ZR)
      UR=U  
C      U2R=TPR(W,WI,W,WI,UR,UI)/XKZ/XKZ
C      U2I=TPI(W,WI,W,WI,UR,UI)/XKZ/XKZ
      W2R = (DPROD(W,W) - DPROD(WI,WI))/XKZ**2
      W2I = 2.D00*DPROD(W,WI)/XKZ**2
      U2R = W2R*UR - W2I*UI
      U2I = W2R*UI + W2I*UR
      GO TO 115 
  105 IF ((AR*AR-WI*WI).LT.0..AND.WI.LT.0.) GO TO 110 
      UR=0. 
      UI=0. 
      U2R=0.
      U2I=0.
C	print*,'f,105,tp,ur',tp,ur
      GO TO 115 
  110 U=DEXP(80.D00) 
      WRITE (7,180) AR,WI,W,TP,B
      WRITE (7,*) '****warning, subr F (dbl) too big', AR,WI,W
      print*, '****warning, subr F (dbl) too big', AR,WI,W
C      print*,'ar,wi,w,b', AR,WI,W,B
      UR=WI*U 
      UI=UR 
      U2R=UR
      U2I=UR
  115 IF (AR*AR+WI*WI-(DIV*B)**2) 120,120,135 
C
C	ENTRY FOR POWER SERIES
C
  120 Z2R=DPROD(ZR,ZR)-DPROD(ZI,ZI) 
      Z2I=2.D00*DPROD(ZR,ZI)
C	print*,'f,120,tp,z2r',tp,z2r
      TR=Z2R
      TI=Z2I
      SR=TR+DBLE(SIGN(.5,XKZ))*UI 
      SI=TI-DBLE(SIGN(.5,XKZ))*UR 
      DO 125 N=1,100 
        XN5=DFLOAT(N)+.5D00
        T=-(TR*Z2R-TI*Z2I)/XN5
        TI=-(TR*Z2I+TI*Z2R)/XN5 
        TR=T
        SR=SR+TR
        SI=SI+TI
C	PRINT*,'N,SUM,TERM',N,DABS(SR)+DABS(SI),DABS(TR)+DABS(TI)
C	PRINT*,'N,UR,UI',N,UR,UI
C	PRINT*,'SR,SI,TR,TI,',SR,SI,TR,TI
        IF (DABS(SR)+DABS(SI)-1.0D9*(DABS(TR)+DABS(TI))) 125,125,130 
  125   CONTINUE
  130 F0R=-2.*SI
      F0I=2.*SR 
      F1R=(PR(W,WI,F0R,F0I)+WI)/XKZ 
      F1I=(PI(W,WI,F0R,F0I)-AR)/XKZ 
      F2R=(PR(W,WI,F1R,F1I)+BZ*WI)/XKZ
      F2I=(PI(W,WI,F1R,F1I)-BZ*AR)/XKZ
C	for checking agaist fried and conte
c	write(44,*) 'p',zr,zi,qr(f0r,f0i,zr,zi),qi(f0r,f0i,zr,zi)
C	WRITE(7,*) F0R,F0I,F1R,F1I,F2R,F2I
      RETURN
C
C	ENTRY FOR ASYMPTOTIC SERIES
C
  135 ZR=QR(B,0.,AR,WI) 
      ZI=QI(B,0.,AR,WI) 
C	print*,'f,135,tp',tp
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
        XN5=DFLOAT(N)-.5D00
        TTR=XN5*(TR*YR-TI*YI) 
        TTI=XN5*(TR*YI+TI*YR) 
C		EXIT WHEN TERMS START TO GET LARGER 
       IF (DABS(TR)+DABS(TI)-DABS(TTR)-DABS(TTI)) 175,165,165
  165   TR=TTR
        TI=TTI
        SR=SR+TR
        SI=SI+TI
C		EXIT IF TERMS ARE SMALL ENOUGH
        IF (DABS(SR)+DABS(SI)-1.0D+10*(DABS(TR)+DABS(TI))) 170,175,175 
  170   CONTINUE
C      WRITE (7,180) SR,SI
  175 F2R=SR
      F2I=SI
      F1R=QR(XKZ*F2R-BZ*WI,XKZ*F2I+BZ*AR,W,WI)
      F1I=QI(XKZ*F2R-BZ*WI,XKZ*F2I+BZ*AR,W,WI)
      F0R=QR(XKZ*F1R-WI,XKZ*F1I+AR,W,WI)  
      F0I=QI(XKZ*F1R-WI,XKZ*F1I+AR,W,WI)  
C      WRITE(7,*) F0R,F0I,F1R,F1I,F2R,F2I
c	note here zr,zi are 1./(zetar + i zetai)
c	write(44,*) 'a',qr(1.,0.,zr,zi),qi(1.,0.,zr,zi),
c     1	pr(f0r,f0i,zr,zi),pi(f0r,f0i,zr,zi)
	RETURN
C 
  180 FORMAT (1X,6E16.8) 
      END 
      FUNCTION PR (A,B,C,D) 
	implicit real (a-h,o-z)
      PR=A*C-B*D
      RETURN
      END 
      FUNCTION PI (A,B,C,D) 
	implicit real (a-h,o-z)
      PI=A*D+B*C
      RETURN
      END 
      FUNCTION QR (A,B,C,D)
	implicit real (a-h,o-z)
      CD = AMAX1(ABS(C),ABS(D))
      AT = A/CD
      BT = B/CD
      CT = C/CD
      DT = D/CD
      QR = (AT*CT+BT*DT)/(CT*CT+DT*DT)
      RETURN
      END 
      FUNCTION QI (A,B,C,D) 
	implicit real (a-h,o-z)
      CD = AMAX1(ABS(C),ABS(D))
      AT = A/CD
      BT = B/CD
      CT = C/CD
      DT = D/CD
      QI=(BT*CT-AT*DT)/(CT*CT+DT*DT)
      RETURN
      END 
      FUNCTION TPR (A,B,C,D,E,F)
	implicit real (a-h,o-z)
      TPR=E*(A*C-B*D)-F*(B*C+A*D) 
      RETURN
      END 
      FUNCTION TPI (A,B,C,D,E,F)
	implicit real (a-h,o-z)
      TPI=F*(A*C-B*D)+E*(B*C+A*D) 
      RETURN
      END 
      SUBROUTINE WAVPOL (A,B,E,F,WR,WI,XKX,XKZ) 
c	calculate vector E from the dispersion determinant (A + iB)
c	normalized to unit length
c	and B (called F here) from del x E + (1/c)dB/dt = 0
c
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
      DIMENSION A(3,3), B(3,3), E(6), F(6), DMR(3,3), DMI(3,3)
c	find largest minor of the dispersion determinant
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
c	use Kramer;s rule to solve for relative E
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
c	find B using Maxwell equation.
      F(1)=-XKZ*QR(E(3),E(4),WR,WI) 
      F(2)=-XKZ*QI(E(3),E(4),WR,WI) 
      F(3)=XKZ*QR(E(1),E(2),WR,WI)-XKX*QR(E(5),E(6),WR,WI)
      F(4)=XKZ*QI(E(1),E(2),WR,WI)-XKX*QI(E(5),E(6),WR,WI)
      F(5)=XKX*QR(E(3),E(4),WR,WI)
      F(6)=XKX*QI(E(3),E(4),WR,WI)
      RETURN
      END 
