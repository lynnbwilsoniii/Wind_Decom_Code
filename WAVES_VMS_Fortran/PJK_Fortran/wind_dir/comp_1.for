C
C	EFFECT OF INPUT (211) BOARD
C
	WCC = W*CCOMP
	Z2 = CMPLX(R2C,-1./WCC)	
	Y1 = 1./(R1C + Z2) + CMPLX(0.,W*C16)	
C*****************   effect of compensation
	CGAIN = CGAIN*(Z2/(R1C+Z2))*CGAIN/(1. + R33*Y1)
C*************

