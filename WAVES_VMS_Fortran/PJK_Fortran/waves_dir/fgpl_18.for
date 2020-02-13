	PROGRAM ZTEST
	DO 10 I = 0,22
	ZTR = .4*(I-10)
	ZTI = .0
C	DO 10 J = 0,6
C	ZTI = J-3
	CALL Z(ZTR,ZTI,ZR,ZI,DZR,DZI)
	WRITE(7,100) ZTR,ZTI,ZR,ZI,DZR,DZI
  10	CONTINUE
	DO 20 I = 1,20
	ZTR = I
	CALL Z(ZTR,ZTI,ZR,ZI,DZR,DZI)
	WRITE(7,100) ZTR,ZTI,ZR,ZI,DZR,DZI
  20	CONTINUE
 100	FORMAT(6E15.7)
	STOP
	END
      SUBROUTINE Z(ZTR,ZTI,ZR,ZI,DZR,DZI)
C
C	THIS ROUTINE RETURNS Z(ZETA) AS DEFINED IN EQ.(1) OF
C	FRIED AND CONTE 'THE PLASMA DISPERSION FUNCTION'
C	AND ITS DERIVATIVE.
C
	DOUBLE PRECISION Z2R,Z2I,XN,SR,SI,T,TR,TI,TTR,TTI,U,UR,UI
C      DATA DIV,EXLIM /2.6,80./
      DATA DIV,EXLIM /3.3,80./
C
      TE = ZTR*ZTR - ZTI*ZTI
      IF (ABS(TE)-EXLIM) 100,100,105  
C	ENTRY FOR REASONABLE EXPONENT
  100 CONTINUE
      U=EXP(-TE)
      UR=U*SIN(2.*ZTR*ZTI)*1.772453851 
      UI=U*COS(2.*ZTR*ZTI)*1.772453851
      GO TO 115 
C	ENTRY FOR LARGE (MAGNITUDE) EXPONENT
  105 IF ((AR*AR-WI*WI).LT.0..AND.WI.LT.0.) GO TO 110 
C	EXPONENT IS LARGE AND NEGATIVE
      UR=0. 
      UI=0. 
      GO TO 115 
C	EXPONENT IS LARGE AND POSITIVE
  110 U=EXP(EXLIM) 
      WRITE (7,180) AR,WI,W,TP,B
      UR=U*SIN(2.*ZTR*ZTI)*1.772453851 
      UI=U*COS(2.*ZTR*ZTI)*1.772453851
  115 IF (ZTR*ZTR+ZTI*ZTI-(DIV)**2) 120,120,135 
C	POWER SERIES FOR SMALL ARGUMENT
  120 Z2R=ZTR*ZTR-ZTI*ZTI 
      Z2I=2.*ZTR*ZTI
      TR=-2.*ZTR
      TI=-2.*ZTI
      SR=TR+UR
      SI=TI+UI
      DO 125 N=1,40 
        XN=N+.5D00
        T=-(TR*Z2R-TI*Z2I)/XN
        TI=-(TR*Z2I+TI*Z2R)/XN
        TR=T
        SR=SR+TR
        SI=SI+TI
        IF (ABS(ZR)+ABS(ZI)-1.0E+6*(ABS(TR)+ABS(TI))) 125,125,130 
  125   CONTINUE
  130 ZR = SR
      ZI = SI
      DZR = -2.*(1.+SR*ZTR-SI*ZTI)
      DZI = -2.*(SR*ZTI+SI*ZTR)
      RETURN
C	ASYMPTOTIC SERIES FOR LARGE ARGUMENT
  135 SIG = 1.
      IF(ZTI.LT.0.) SIG  = 2.
      IF(ZTI.GT.0.) SIG  = 0.
      SR=QR(1.,0.,ZTR,ZTI) 
      SI=QI(1.,0.,ZTR,ZTI) 
      YR=SR*SR-SI*SI  
      YI=2.*SR*SI 
      TR=YR
      TI=YI
      DZR = -2.*SIG*(ZTR*UR - ZTI*UI) + YR
      DZI = -2.*SIG*(ZTR*UI + ZTI*UR) + YI
      DO 170 N=2,30 
        XN=N-.5D00
        TTR=XN*(TR*YR-TI*YI) 
        TTI=XN*(TR*YI+TI*YR) 
C	EXIT IF TERMS START TO INCREASE
        IF (ABS(TR)+ABS(TI)-ABS(TTR)-ABS(TTI)) 175,165,165
  165   TR=TTR
        TI=TTI
        DZR=DZR+TR
        DZI=DZI+TI
C	EXIT IF TERMS ARE SUFFICIENTLY SMALL
        IF (ABS(DZR)+ABS(DZI)-1.0E+6*(ABS(TR)+ABS(TI))) 170,175,175 
  170   CONTINUE
  175 ZR = -.5*((DZR+2.)*SR - DZI*SI)
      ZI = -.5*((DZR+2.)*SI + DZI*SR)
	RETURN
C 
  180 FORMAT (' EXPONENT TOO LARGE IN Z ',6E15.7) 
      END 
      FUNCTION QR (A,B,C,D)
      QR = (A*C+B*D)/(C*C+D*D)
      RETURN
      END 
      FUNCTION QI (A,B,C,D) 
      QI=(B*C-A*D)/(C*C+D*D)
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