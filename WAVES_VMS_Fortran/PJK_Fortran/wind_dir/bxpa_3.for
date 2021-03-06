	FUNCTION BXPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND X SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD. (COMPLEX)
C
C
	COMPLEX BXPA
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.47,.749,1650./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .952E-6, 49.9E3, 1.E6, 1.E-9/
	DATA TWOPI /6.2831853/
C
C
	BXPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
C	OUTPUT OF SEARCH COIL PLUS PREAMP, VOLTS/nT
	CGAIN = T0/CMPLX(1.,Q*(F/F0 - F0/F))
C
C	EFFECT OF INPUT ON 212 BOARD
C
	Y2 = CMPLX(1./R2,W*C1)
	Z1 = CMPLX(R1, -1./W/C10)
	CGAIN = CGAIN/(Z1*Y2)
C
	BXPA = CGAIN
	RETURN
	END

