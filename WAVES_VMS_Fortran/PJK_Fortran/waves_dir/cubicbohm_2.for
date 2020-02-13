	PROGRAM CUBICSOLV
C
C	SMALL EPSILON APPROX TO DISERSION RELATION, STERO-CASSINI NB P.84
C
	COMPLEX ROOT(3)
C
	DATA TPAR /.15E-4/
	DATA WRSAVE,XKSAVE /1.0014,7.13/
	DATA VGP0 /-1./
	DATA WCYCL /.0062/
	DATA ANG /11.1/
c	DATA ANG /0./
c
	XK = 7.
	A = -XK**2
	B = (XK**4 - WCYCL**2)/4.
	C = ((WCYCL*XK*SIND(ANG))**2)/8.
	  CALL CUBIC(A,B,C,ROOT) 
C	  PRINT*,'ROOTS',ROOT
	PRINT*,'XK,A,B,C',XK,A,B,C
	WRITE(26,1026) XK,ROOT
	XKSAVE = XK
	WRSAVE = 1.0014
C
	RATIO = 1.1
	DO I = 1,60
	  XK =XK/RATIO
C	  USE CUBIC, COLD PLASMA
	  A = -XK**2
	  B = (XK**4 - WCYCL**2)/4.
	  C = ((WCYCL*XK*SIND(ANG))**2)/8.
	  CALL CUBIC(A,B,C,ROOT) 
C	  PRINT*,'ROOTS',ROOT
	  PRINT*,'XK,A,B,C',XK,A,B,C
	  WRITE(26,1026) XK,ROOT
 1026	FORMAT(F6.3,6E12.3)
	ENDDO
C
	XK = 0.
	A = -XK**2
	B = (XK**4 - WCYCL**2)/4.
	C = ((WCYCL*XK*SIND(ANG))**2)/8.
	  CALL CUBIC(A,B,C,ROOT) 
C	  PRINT*,'ROOTS',ROOT
	PRINT*,'XK,A,B,C',XK,A,B,C
	WRITE(26,1026) XK,ROOT
	STOP
	END