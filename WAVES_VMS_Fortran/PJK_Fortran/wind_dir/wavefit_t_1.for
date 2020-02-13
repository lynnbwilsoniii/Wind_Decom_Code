	SUBROUTINE WAVEFIT_T(X,SUMSQ)
C
	COMMON /PARTBLK/ X4DATA(2050,4),XRE,YRE,ZRE,SUNCLOCK,SPINRATE
	COMMON /WAVFIT/ N1,N2
	REAL X(25),WTX(2048),WTY(2048),C(8,8),Y(8),W(8),V(8,8),YOUT(8)
	REAL U(8,8)
C	DATA XLEN,YLEN,ZLEN /41.1, 3.79, 2.17/
	DATA XLEN,YLEN,ZLEN /.0411, .00379, .00217/             ! KM, FOR mV
	DATA TWOPI /6.28318531/
C
	SUMSQ = 0.
C
	F1 = X(1)
	F2 = X(1) + X(2)
	EX1 = X(3)
	EY1 = X(4)
	EX2 = X(5)
	EY2 = X(6)
	PH1 = 0.
	PH2 = 0.
	TX1 = X(7)
	TY1 = X(8)
	TX2 = X(9)
	TY2 = X(10)
	DELT = 1./120.				! MSEC
C
	DO I = 1,8
	  Y(I)= 0.
	  DO J = 1,8
	    C(I,J) = 0.
	  ENDDO
	ENDDO
C	CONST = 0.
C
	DO N = N1,N2
	  T = (N-1024)*DELT	
	  S1 = SIN(TWOPI*F1*T + PH1)
	  S2 = SIN(TWOPI*F2*T + PH2)
	  C1 = COS(TWOPI*F1*T + PH1)
	  C2 = COS(TWOPI*F2*T + PH2)
	  CONST = CONST + (X4DATA(N,1)/XLEN)**2 + (X4DATA(N,2)/YLEN)**2
	  Y(1) = Y(1) + (X4DATA(N,1)/XLEN)*S1
	  Y(2) = Y(2) + (X4DATA(N,2)/YLEN)*S1
	  Y(3) = Y(3) + (X4DATA(N,1)/XLEN)*S2
	  Y(4) = Y(4) + (X4DATA(N,2)/YLEN)*S2
	  Y(5) = Y(5) + (X4DATA(N,1)/XLEN)*C1
	  Y(6) = Y(6) + (X4DATA(N,2)/YLEN)*C1
	  Y(7) = Y(7) + (X4DATA(N,1)/XLEN)*C2
	  Y(8) = Y(8) + (X4DATA(N,2)/YLEN)*C2
	  C(1,1) = C(1,1) + S1*S1
C	  C(1,2) = 0.				! COEFF OF EX1 IN 2ND EQ
	  C(1,3) = C(1,3) + S1*S2		! COEFF OF EX1 IN 3RD EQ
	  C(1,5) = C(1,5) + S1*C1		! COEFF OF EX1 IN 5TH EQ
	  C(1,7) = C(1,7) + S1*C2		! COEFF OF EX1 IN 7TH EQ
	  C(2,2) = C(2,2) + S1*S1		! COEFF OF EY1 IN 2ND EQ
	  C(2,4) = C(2,4) + S1*S2
	  C(2,6) = C(2,6) + S1*C1
	  C(2,8) = C(2,8) + S1*C2
	  C(3,3) = C(3,3) + S2*S2		! COEFF OF EX2 IN 3RD EQ
	  C(3,5) = C(3,5) + S2*C1		! COEFF OF EX2 IN 5TH EQ
	  C(3,7) = C(3,7) + S2*C2		! COEFF OF EX2 IN 7TH EQ
	  C(4,4) = C(4,4) + S2*S2
	  C(4,6) = C(4,6) + S2*C1
	  C(4,8) = C(4,8) + S2*C2
	  C(5,5) = C(5,5) + C1*C1
	  C(5,7) = C(5,7) + C1*C2
	  C(6,6) = C(6,6) + C1*C1
	  C(6,8) = C(6,8) + C1*C2
	  C(7,7) = C(7,7) + C2*C2
	  C(8,8) = C(8,8) + C2*C2
	ENDDO
C
	C(3,1) = C(1,3)
	C(5,1) = C(1,5)
	C(7,1) = C(1,7)
	C(4,2) = C(2,4)
	C(6,2) = C(2,6)
	C(8,2) = C(2,8)
	C(5,3) = C(3,5)
	C(7,3) = C(3,7)
	C(6,4) = C(4,6)
	C(8,4) = C(4,8)
	C(7,5) = C(5,7)
	C(8,6) = C(6,8)
C	CALL GAUSSJ(C,4,4,Y,1,1)
	DO I = 1,8
	  YOUT(I) = Y(I)
	  DO J = 1,8
	    U(I,J) = C(I,J)
	  ENDDO
	ENDDO
	CALL SVDCMP(U,8,8,8,8,W,V)
	CALL SVBKSB(U,W,V,8,8,8,8,Y,YOUT)
C
	EX1 = YOUT(1)
	EY1 = YOUT(2)
	EX2 = YOUT(3)
	EY2 = YOUT(4)
	TX1 = YOUT(5)
	TY1 = YOUT(6)
	TX2 = YOUT(7)
	TY2 = YOUT(8)
	SUMSQ = 0.
C
	DO N = N1,N2
	  T = (N-1024)*DELT	
	  EX = EX1*SIN(TWOPI*F1*T + PH1) + EX2*SIN(TWOPI*F2*T + PH2)
     1	   + TX1*COS(TWOPI*F1*T + PH1) + TX2*COS(TWOPI*F2*T + PH2)
	  EY = EY1*SIN(TWOPI*F1*T + PH1) + EY2*SIN(TWOPI*F2*T + PH2)
     1	   + TY1*COS(TWOPI*F1*T + PH1) + TY2*COS(TWOPI*F2*T + PH2)
	  SUMSQ = SUMSQ + (X4DATA(N,1)/XLEN - EX)**2
     1		+ (X4DATA(N,2)/YLEN - EY)**2
	ENDDO
	DO N = 1,4
	  X(N+2) = YOUT(N)
	ENDDO
	DO N = 5,8
	  X(N+4) = YOUT(N)
	ENDDO
C
	RETURN
	END	

