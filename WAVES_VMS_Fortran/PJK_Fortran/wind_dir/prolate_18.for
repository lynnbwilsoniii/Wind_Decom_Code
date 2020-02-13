	PROGRAM PROLATE
C
C	CALCULATES INDUCED FIELD AROUND A PROLATE CONDUCTING ELLIPSOID
C
	COMMON /SHAPE/A,B,C
	COMMON /COORDS/ XI, ETA, ZETA
	DIMENSION X(3)
	DATA SEMIL,R /.3175,12.7/      ! PLATE, 10 IN. DIA., .25 IN. THICK
C
C	PROLATE A = B > C     AND X(1) GOES WITH A
	A = R
	B = R
	C = SEMIL
C
C	CALCULATE PHI AT THE SURFACE
C
	X(1) = R
	X(2) = 0.
	X(3) = 0.
	PHIS = PHI(1,X)
	print*,'phi on x axis',phis
	X(1) = 0.
	X(2) = 0.
	X(3) = SEMIL
	PHIS = PHI(1,X)
	print*,'phi at edge of disk',phis
	STOP
	END
	FUNCTION PHI(N,X)
C
	COMMON /SHAPE/A,B,C
	COMMON /COORDS/ XI, ETA, ZETA
	COMPLEX Z1,Z2,Z3
	DIMENSION X(3)
C
C	CALCULATE XI,ETA,ZETA BY SOLVING THE CUBIC
C
	A2 = A**2 + B**2 + C**2 - X(1)**2 - X(2)**2 - X(3)**2
	A1 = (A*B)**2 + (A*C)**2 + (B*C)**2 - X(1)**2*(B**2+C**2)
     1    - X(2)**2*(A**2+C**2) - X(3)**2*(A**2+B**2)
	A0 = (A*B*C)**2 - (X(1)*B*C)**2 - (X(2)*A*C)**2 - (X(3)*A*B)**2
	print*,'a2,a1,a0',a2,a1,a0
C	do m = 1,100
C	  z = -m/100.
C	  y = z**3 + a2*z**2 + a1*z + a0
C	  print*,z,y
C	enddo
C	do m = 1,100
C	  z = -m
C	  y = z**3 + a2*z**2 + a1*z + a0
C	  print*,z,y
C	enddo
	CALL RCUBIC(A2,A1,A0,Z1,Z2,Z3)
	XI = Z1
	ETA = Z2
	ZETA = Z3
	IF(XI.LT.ETA) THEN
	  TEMP = ETA
	  ETA = XI
	  XI = TEMP
	ENDIF
	IF(XI.LT.ZETA) THEN
	  TEMP = ZETA
	  ZETA = XI
	  XI = TEMP
	ENDIF
	IF(ETA.LT.ZETA) THEN
	  TEMP = ETA
	  ETA = ZETA
	  ZETA = TEMP
	ENDIF
C
	PHI = 0.
	RETURN
	END
	SUBROUTINE FIELD(N,XS,B)
C
	DIMENSION X(3),XS(3),B(3)
C
	DO J = 1,3
	  X(J) = XS(J)
	ENDDO
	RETURN
	END
