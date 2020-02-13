      SUBROUTINE F (W,WI,TP,XKZ,BZ,F0R,F0I,F1R,F1I,F2R,F2I) 
C      DIV=2.6 
C
C	THIS ROUTINE RETURNS ZETA*F0, ETC., FROM STIX FIRST BOOK,
C	iF0 is Fried and Conte Z
C
c****      DOUBLE PRECISION Z2R,Z2I,T,TR,TI,SR,SI,XN5,TTR,TTI
      DOUBLE PRECISION Z2R,Z2I,T,TR,TI,SR,SI,XN5,TTR,TTI
      PARAMETER(DIV=4.33)
C 
      AR=W-XKZ*BZ 
      B=XKZ*SQRT(2.*TP) 
C****        IF (ABS(AR*AR-WI*WI)-34.*B*B) 100,100,105  
      IF (ABS(AR*AR-WI*WI)-80.*B*B) 100,100,105  
  100 ZR=AR/B 
      ZI=WI/B 
      TE=-ZI*ZI+ZR*ZR 
      U=0.  
      IF (TE.LT.80.) U=EXP(-TE)
C	print*,'f,100,tp,u',tp,u
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
C	print*,'f,105,tp,ur',tp,ur
      GO TO 115 
C*****  110 U=EXP(28.) 
  110 U=EXP(80.) 
C      WRITE (7,180) AR,WI,W,TP,B
C      print*,'ar,wi,w,b', AR,WI,W,B
      UR=WI*U 
      UI=UR 
      U2R=UR
      U2I=UR
  115 IF (AR*AR+WI*WI-(DIV*B)**2) 120,120,135 
C
C	ENTRY FOR POWER SERIES
C
  120 Z2R=ZR*ZR-ZI*ZI 
      Z2I=2.*ZR*ZI
C	print*,'f,120,tp,z2r',tp,z2r
      TR=Z2R
      TI=Z2I
      SR=TR+SIGN(.5,XKZ)*UI 
      SI=TI-SIGN(.5,XKZ)*UR 
      DO 125 N=1,100 
        XN5=DFLOAT(N)+.5D00
        T=-(TR*Z2R-TI*Z2I)/XN5
        TI=-(TR*Z2I+TI*Z2R)/XN5 
        TR=T
        SR=SR+TR
        SI=SI+TI
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
C	WRITE(7,*) 's',w,wi,tp,F0R,F0I,F1R,F1I,F2R,F2I
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
C	write(7,*) 'xkz,wi,c,u',xkz,wi,c,u
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
C	WRITE(7,*) 'a',w,wi,tp,F0R,F0I,F1R,F1I,F2R,F2I
C      WRITE(7,*) F0R,F0I,F1R,F1I,F2R,F2I
	RETURN
C 
  180 FORMAT (1X,6E16.8) 
      END 
      SUBROUTINE Fold(W,WI,TP,XKZ,BZ,F0R,F0I,F1R,F1I,F2R,F2I) 
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

