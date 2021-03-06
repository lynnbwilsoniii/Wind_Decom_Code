	PROGRAM POLARPA
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
	COMPLEX CGAIN
C
	DO N = 1,10
	  F = N*2.E4
C	  PA(F)
	  PRINT*,F,PA(F),CGAIN,57.3*PHASE
	ENDDO
	STOP
	END
	FUNCTION PA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE POLAR PREAMP.  THE SIGNAL
C	IS ASSUMED TO BE INJECTED AT THE SHEATH-PLASMA INTERFACE.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,ZIN,VSIG,VSH,VSP,GNOPAMP
C	R AND C OF SHEATHS AROUND SPHERE (SP) AND STUB (ST)
	DATA RSP,CSP,RST,CST /1.E7,10.E-12,1.E6,100.E-12/
C	DC GAIN AND GAIN-BANDWIDTH PRODUCT OF OPAMP
	DATA GOPAMP,GBWOPAMP /1.E6,1.E6/
	DATA RIN /80.E6/
	DATA R9,C9,R10,CINP /5.E8, 23.6E-12, 5.E8, 4.37E-12/
	DATA R33,C16 /2.2E+03, 1.203E-9/
	DATA R1C,R2C,CCOMP / 0., 4.64E3, .947E-6/   
	DATA R5 /22.1E3/
	DATA TWOPI /6.2831853/
C
	W = TWOPI*F
	CGAIN = 1.
C	COMPLEX GAIN OF OP AMP
	GNOPAMP = GOPAMP/CMPLX(1., W*GOPAMP/GBWOPAMP)
C	GAIN WITH FEEDBACK
	GNOPAMP = GOPAMP/CMPLX((1. + GOPAMP), W*GOPAMP/GBWOPAMP)
C	SPHERE SHEATH IMPEDANCE
	Z1 = RSP/CMPLX(1., W*RSP*CSP)
	ZIN = CMPLX(RIN,0.)
	VSP = CGAIN*ZIN/(ZIN+Z1)
	CGAIN = VSP*GNOPAMP
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	PA = CABS(CGAIN)
	RETURN
	END

