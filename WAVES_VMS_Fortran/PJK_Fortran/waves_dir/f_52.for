      SUBROUTINE F (W,WI,TP,XKZ,BZ,F0R,F0I,F1R,F1I,F2R,F2I) 
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
C
C	THIS RETURNS ZETA*F0 ETC, FROM STIX FIRST BOOK, ZETA IS ARGUMENT
C	iF0 is Fried and Conte Z
C	 
      DOUBLE PRECISION Z2R,Z2I,T,TR,TI,SR,SI,XN5,TTR,TTI,UR,UI,U,DTE,W2R,W2I
C      PARAMETER(DIV=4.11)
      PARAMETER(DIV=2.11)
C      PARAMETER(DIV=2.85)
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
	pmax = 0.
      DO 125 N=1,100 
        XN5=DFLOAT(N)+.5D00
        T=-(TR*Z2R-TI*Z2I)/XN5
        TI=-(TR*Z2I+TI*Z2R)/XN5 
        TR=T
        SR=SR+TR
        SI=SI+TI
	psize = abs(tr) + abs(ti)
	if(psize.gt.pmax) then
	  max = n
	  pmax = psize
	endif
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
	write(10,*) w,wi,max,1.e-16*pmax,sqrt(AR*AR+WI*WI)/B
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
	pmin = 1.e12
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
	psize = abs(tr) + abs(ti)
	if(psize.lt.pmin) then
	  min = n
	  pmin = psize
	endif
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
	write(11,*) w,wi,min,pmin,sqrt(AR*AR+WI*WI)/B
C      WRITE(7,*) F0R,F0I,F1R,F1I,F2R,F2I
	RETURN
C 
  180 FORMAT (1X,6E16.8) 
      END 

