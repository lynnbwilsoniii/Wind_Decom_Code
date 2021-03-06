	FUNCTION TDSCAL(CHANNEL,SPEED,TM)
C
	INCLUDE 	'TDS_CALDATA.FOR'
C
C	COMMON /TCALDATA/ A(6),B(6),POFFS(6),NOFFS(6)
C	INTEGER*4 CHANNEL,SPEED,TM,INDEX
C	REAL TDSCAL,A,B,POFFS,NOFFS
C	DATA B /.08316,.08151,.08099,.08212,.08293,.08293/
C	DATA A /3.19E-4,2.14E-4,3.89E-4,2.40E-4,1.75E-4,1.94E-4/
C	DATA POFFS /4.01,6.31,4*0./
C	DATA NOFFS /1.59,1.79,4*0./
C
	INDEX = MIN0(2*CHANNEL + MIN0(SPEED,1) - 1, 5)
	TMNB = TM - 128
	IF(INDEX.EQ.5.AND.ABS(TMNB).LT.78.) INDEX = 6
C
C	INDEX = 1 IS CHANNEL 1, FASTEST, 2 IS CHANNEL 1, SLOWER, 3 IS
C	CHANNEL 2 FASTEST, 4 IS CHANNEL 2 SLOWER, 5,6 ARE CHANNELS 3-6,
C	5 FOR TM GT. 78, 6 FOR TM LT 78
C
	IF(TMNB.GT.0.) THEN
	  TDSCAL = A(INDEX)*(EXP(B(INDEX)*TMNB) - POFFS(INDEX))
	ELSE
	  TDSCAL = -A(INDEX)*(EXP(-B(INDEX)*TMNB) - NOFFS(INDEX))
	ENDIF
	RETURN
	END
	FUNCTION EXDCPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E8, 23.6E-12, 5.E8, 4.37E-12/
	DATA R33,C16 /2.2E+03, 1.203E-9/
	DATA R1C,R2C,CCOMP / 0., 4.64E3, .947E-6/    ! as shown on drawing
C	DATA R1C,R2C,CCOMP / 0., 5.E3, .947E-6/
	DATA R5 /22.1E3/
	DATA TWOPI /6.2831853/
C
C	C16 = CVAR(1)
C	C9 = CVAR(2)
C	CINP = CVAR(3)
C
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C	EFFECT OF OUTPUT RC
	WC16 = 1./(W*C16)
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
C
C	EFFECT OF INPUT (211) BOARD
C
	WCC = W*CCOMP
	Z2 = CMPLX(R2C,-1./WCC)	
	Y1 = 1./R5 + 1./(R1C + Z2) + CMPLX(0.,W*C16)	
	CGAIN = CGAIN*(Z2/(R1C+Z2))/(1. + R33*Y1)
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EXDCPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EYDCPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C
C	DONE AND CHECKED
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E9,24.5E-12,5.E9,4.757E-12/
	DATA R33,C16 /2.2E+03,1.185E-9/
	DATA R1C,R2C,CCOMP /37.4E3,39.2E3,1.982E-6/
	DATA R19 /1.E5/
	DATA TWOPI /6.2831853/
C
C	C16 = 1.185E-9                           
C	C9  = 24.5E-12                           
C	CINP = 4.757E-12                         
C
C
C	C16 = CVAR(1)                             ! ADJUSTING EY
C	C9  = CVAR(2)                             ! ADJUSTING EY
C	CINP = CVAR(3)                            ! ADJUSTING EY
C
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C	EFFECT OF OUTPUT RC
	WC16 = 1./(W*C16)
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
C
C	EFFECT OF INPUT (211) BOARD
C
	WCC = W*CCOMP
	Z2 = CMPLX(R2C,-1./WCC)	
	Y2 = 1./Z2 + 1./R19
	Z1 = R1C + 1./Y2
	Y1 = 1./Z1 + CMPLX(0.,W*C16)	
	CGAIN = CGAIN/(1. + R33*Y1)/(1. + R1C*Y2)
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EYDCPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EZDCPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C	DONE AND TESTED
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /18.E6,84.E-12,2.E8,4.99E-12/
	DATA R33,C16 /2.2E+03,1.202E-9/
	DATA RCOMP,CCOMP /22.1E3,47.E-12/
	DATA R29 /22.1E3/
	DATA TWOPI /6.2831853/
C
C	C16 = 1.203E-9                             ! ADJUSTING EX
C	C9  = 23.6E-12                            ! ADJUSTING EX
C	CINP = 4.99E-12                          ! ADJUSTING EZ
C
C	C16 = CVAR(1)
C	C9 = CVAR(2)
C	CINP = CVAR(3)
C
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C	EFFECT OF OUTPUT RC
	WC16 = 1./(W*C16)
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
C
C	EFFECT OF INPUT (211) BOARD
C
	Y1 = 1./R29 + CMPLX(0.,W*C16)	
	CGAIN = CGAIN/(1. + R33*Y1)
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EZDCPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EXACPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN,ZMEU
	DATA R9,C9,R10,CINP /5.E8,23.6E-12,5.E8,4.37E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.49E-9,10.E3,86.9E-9/
	DATA C15,R32,C27,R11/ 0.,10.E3,470.E-12,1.5E3/
	DATA C25,R39,C30/829.E-9,330.,29.9E-9/ 
	DATA CMEU,RMEU /20.E-9,300./
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
C
C	C30 = 30.9E-9                     ! ADJUSTING EX
C	C13 = 3.7E-9                     ! ADJUSTING EX
C
C	C30 = CVAR(1)
C	C26 = CVAR(2)
C	C25 = CVAR(3)
C	C13 = CVAR(4)
C
	W = TWOPI*F
	CGAIN = 1.
C
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C
C	CGAIN = 3.2
C
C	EFFECT OF OUTPUT OP AMP
C	
C	FILTERING BEFORE INPUT
	Z1 = R28                 	! LAST LOAD BEFORE OP AMP
	WC12 = W*C12
	Z2 = CMPLX(R28,-1./WC12)        !  LAST RC
	Y1 = 1./CMPLX(R43,0.) + 1./Z2   !  LAST RC IN PARALLEL WITH R43
	WC29 = W*C29                    
	Z3 = 1./Y1 + CMPLX(0.,-1./WC29) !  LOAD ON FOLLOWER
	OPAMPIN = CGAIN*(Z1/Z2)*(1./Y1/Z3)
C
C	OP AMP GAIN
C
C	Z3 FROM NEG. INPUT TO GROUND
	WC13 = W*C13
	Z1 = CMPLX(R29,-1./WC13)
	WC26 = W*C26
	Z2 = CMPLX(R12,-1./WC26)
	Y1 = 1./Z1 + 1./Z2                  ! Y FROM NEG INPUT TO GROUND
	Z3 = 1./Y1                          ! Z FROM NEG INPUT TO GROUND
C	Z2 FROM OUTPUT TO NEG INPUT
	WC27 = W*C27
	Z1 = CMPLX(R11,-1./WC27)
	WC15 = W*C15
	Y1 = CMPLX(1./R32,WC15)
	Y2 = Y1 + 1./Z1 
	Z2 = 1./Y2
	CGAIN = OPAMPIN*(Z3 + Z2)/Z3
C
C	FILTERING AFTER OP AMP OUTPUT
C
	WC30 = W*C30
	RMINN = 22.1E3                ! input resistance on 210 board
	Y1 = CMPLX(1./RMINN,WC30)
	Z1 = R39 + 1./Y1
	WC25 = W*C25
C	RTNR = .42e5		      ! total TNR load
	ZMEU = 47. + CMPLX(RMEU,-1./W/CMEU)
	Y3 = 1./ZMEU + 1./Z1
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN/(1.+Y3*Z2)/Y1/Z1	
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EXACPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EYACPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN,ZMEU
	DATA R9,C9,R10,CINP /5.E9,24.5E-12,5.E9,4.757E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.437E-9,10.E3,98.23E-9/
	DATA C15,R32,C27,R11/ 0.,33.E3,150.E-12,4.7E3/
	DATA C25,R39,C30/1401.E-9,330.,30.61E-9/
 	DATA CMEU,RMEU /20.E-9,300./
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
C	DONE AND CHECKED
C
C	C30 = CVAR(1) = 30.61 E-9
C	C26 = CVAR(2) = 98.23 E-9
C	C25 = CVAR(3) = 1401. E-6
C	C13 = CVAR(4) = 3.437 E-9
C
	W = TWOPI*F
	CGAIN = 1.
C
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C
C	CGAIN = 3.2
C
C	EFFECT OF OUTPUT OP AMP
C	
C	FILTERING BEFORE INPUT
	Z1 = R28                 	! LAST LOAD BEFORE OP AMP
	WC12 = W*C12
	Z2 = CMPLX(R28,-1./WC12)        !  LAST RC
	Y1 = 1./CMPLX(R43,0.) + 1./Z2   !  LAST RC IN PARALLEL WITH R43
	WC29 = W*C29                    
	Z3 = 1./Y1 + CMPLX(0.,-1./WC29) !  LOAD ON FOLLOWER
	OPAMPIN = CGAIN*(Z1/Z2)*(1./Y1/Z3)
C
C	OP AMP GAIN
C
C	Z3 FROM NEG. INPUT TO GROUND
	WC13 = W*C13
	Z1 = CMPLX(R29,-1./WC13)
	WC26 = W*C26
	Z2 = CMPLX(R12,-1./WC26)
	Y1 = 1./Z1 + 1./Z2                  ! Y FROM NEG INPUT TO GROUND
	Z3 = 1./Y1                          ! Z FROM NEG INPUT TO GROUND
C	Z2 FROM OUTPUT TO NEG INPUT
	WC27 = W*C27
	Z1 = CMPLX(R11,-1./WC27)
	WC15 = W*C15
	Y1 = CMPLX(1./R32,WC15)
	Y2 = Y1 + 1./Z1 
	Z2 = 1./Y2
	CGAIN = OPAMPIN*(Z3 + Z2)/Z3
C
C	FILTERING AFTER OP AMP OUTPUT
C
	WC30 = W*C30
	RMINN = 22.1E3                ! input resistance on 210 board
	Y1 = CMPLX(1./RMINN,WC30)
	Z1 = R39 + 1./Y1
	WC25 = W*C25
C	RTNR = .42e5		      ! total TNR load
	ZMEU = 47. + CMPLX(RMEU,-1./W/CMEU)
	Y3 = 1./ZMEU + 1./Z1
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN/(1.+Y3*Z2)/Y1/Z1	
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EYACPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EZACPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C	COMMON /COMPBLK/ CVAR(5)
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN,ZMEU
	DATA R9,C9,R10,CINP /18.E6,84.E-12,2.E8,4.99E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.448E-9,10.E3,92.E-9/
	DATA C15,R32,C27,R11/ 0.,33.E3,150.E-12,4.7E3/
	DATA C25,R39,C30/703.E-9,330.,32.89E-9/ 
	DATA CMEU,RMEU /20.E-9,300./
	DATA TWOPI /6.2831853/
C
C	DONE AND CHECKED
C
C 	C30 = CVAR(1)
C	C26 = CVAR(2)
C	C25 = CVAR(3)
C	C13 = CVAR(4)
C
	W = TWOPI*F
	CGAIN = 1.
C
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C
C	CGAIN = 3.2
C
C	EFFECT OF OUTPUT OP AMP
C	
C	FILTERING BEFORE INPUT
	Z1 = R28                 	! LAST LOAD BEFORE OP AMP
	WC12 = W*C12
	Z2 = CMPLX(R28,-1./WC12)        !  LAST RC
	Y1 = 1./CMPLX(R43,0.) + 1./Z2   !  LAST RC IN PARALLEL WITH R43
	WC29 = W*C29                    
	Z3 = 1./Y1 + CMPLX(0.,-1./WC29) !  LOAD ON FOLLOWER
	OPAMPIN = CGAIN*(Z1/Z2)*(1./Y1/Z3)
C
C	OP AMP GAIN
C
C	Z3 FROM NEG. INPUT TO GROUND
	WC13 = W*C13
	Z1 = CMPLX(R29,-1./WC13)
	WC26 = W*C26
	Z2 = CMPLX(R12,-1./WC26)
	Y1 = 1./Z1 + 1./Z2                  ! Y FROM NEG INPUT TO GROUND
	Z3 = 1./Y1                          ! Z FROM NEG INPUT TO GROUND
C	Z2 FROM OUTPUT TO NEG INPUT
	WC27 = W*C27
	Z1 = CMPLX(R11,-1./WC27)
	WC15 = W*C15
	Y1 = CMPLX(1./R32,WC15)
	Y2 = Y1 + 1./Z1 
	Z2 = 1./Y2
	CGAIN = OPAMPIN*(Z3 + Z2)/Z3
C
C	FILTERING AFTER OP AMP OUTPUT
C
	WC30 = W*C30
	RMINN = 22.1E3                ! input resistance on 210 board
	Y1 = CMPLX(1./RMINN,WC30)
	Z1 = R39 + 1./Y1
	WC25 = W*C25
C	RTNR = .42e5		      ! total TNR load
	ZMEU = 47. + CMPLX(RMEU,-1./W/CMEU)
	Y3 = 1./ZMEU + 1./Z1
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN/(1.+Y3*Z2)/Y1/Z1	
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EZACPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION BXPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND X SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.47,.749,1650./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .952E-6, 49.9E3, 1.E6, 1.E-9/
	DATA TWOPI /6.2831853/
C
C
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
C	NORMALIZE TO E PREAMPS
C
c	CGAIN = .852*CGAIN
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	BXPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION BYPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND X SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.49,.759,1560./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .992E-6, 49.9E3, 1.E6, 1.002E-9/
	DATA TWOPI /6.2831853/
C
C
	W = TWOPI*F
C	OUTPUT OF SEARCH COIL PLUS PREAMP, VOLTS/nT
	CGAIN = T0/CMPLX(1.,Q*(F/F0 - F0/F))         ! Volts/nT
C
C	EFFECT OF INPUT ON 212 BOARD
C
	Y2 = CMPLX(1./R2,W*C1)
	Z1 = CMPLX(R1, -1./W/C10)
	CGAIN = CGAIN/(Z1*Y2)
C
C	NORMALIZE TO E PREAMPS
C
c	CGAIN = .852*CGAIN
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	BYPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION BZPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND X SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.43,.763,1700./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .959E-6, 49.9E3, 1.E6, 1.005E-9/
	DATA TWOPI /6.2831853/
C
C
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
C	NORMALIZE TO E PREAMPS
C
c	CGAIN = .852*CGAIN
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	BZPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION TDSDET(CHANNEL,F)
C
C	THIS IS THE GAIN OF A CIRCUIT JUST BEFORE THE TDS DETECTOR
C
	INTEGER*4 CHANNEL
	COMPLEX Z1,Y2,GAIN
	COMMON /GAINBLK/ PHASE,GAIN
	DATA R1,C1,R2,C2 /1.505E3,.33E-6,1.E5,.33E-6/
	DATA TWOPI /6.2831853/
C
	W = TWOPI*F
	IF(CHANNEL.GT.2) THEN
	  Z1 = CMPLX(R2,-1./W/C2)               ! 4.82 HZ
	  GAIN = R2/Z1
	ELSE
	  Z1 = CMPLX(R1,-1./W/C1)		! 320 HZ
	  GAIN = R1/Z1
	ENDIF
C
	GNIP = AIMAG(GAIN)
	GNRP = GAIN
	PHASE = ATAN2(GNIP,GNRP)
	TDSDET = CABS(GAIN)
	RETURN
	END
	FUNCTION FFTAMP(CHANNEL,F)
C
C	THIS IS THE GAIN OF A CIRCUIT JUST BEFORE THE FFT D/A CONVERTER
C
	INTEGER*4 CHANNEL
	COMPLEX Z1,Y2,GAIN
	REAL RSW(6),C2MUF(6),F
	COMMON /GAINBLK/ PHASE,GAIN
	DATA RSW /50.,400.,400.,0.,0.,0./
	DATA C2MUF /1.961,1.896,1.949,1.981,2.011,1.943/
	DATA C1,R2,C2 /2.E-6,1.E4,1.E-9/
	DATA TWOPI /6.2831853/
C
	GAIN = CMPLX(1.,0.)
	PHASE = 0.
	FFTAMP = 1.
	IF(CHANNEL.GT.6) RETURN
	W = TWOPI*F
	C1 = C2MUF(CHANNEL)*1.E-6
	RSWT = RSW(CHANNEL)
	Z1 = CMPLX(RSWT,-1./W/C1)
	Y2 = CMPLX(1./R2,W*C2)
	GAIN = 1./(1. + Y2*Z1)

C
	GNIP = AIMAG(GAIN)
	GNRP = GAIN
	PHASE = ATAN2(GNIP,GNRP)
	FFTAMP = CABS(GAIN)
	RETURN
	END
	FUNCTION PREAMP(ICHAN,IPA,F)
C
C	THIS FUNCTION CALLS THE APPROPRIATE PREAMP, PUTS THE
C	COMPLEX GAIN AND PHASE SHIFT IN CGAIN AND PHASE, AND RETURNS
C	THE MAGNITUDE OF THE PREAMP GAIN
C
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	INTEGER*4 IPACAL(6,4)
	DATA IPACAL /1,    1,     4,     7,     8,     9,
     1               4,    2,     5,     4,     5,     6,
     2               0,    3,     6,     0,     0,     0,
     3               0,    3,     6,     0,     0,     0/
C
	IRX = IPACAL(ICHAN,IPA+1)
	IF(IRX.EQ.1) PREAMP = EXACPA(F)
	IF(IRX.EQ.2) PREAMP = EYACPA(F)
	IF(IRX.EQ.3) PREAMP = EZACPA(F)
	IF(IRX.EQ.4) PREAMP = EXDCPA(F)
	IF(IRX.EQ.5) PREAMP = EYDCPA(F)
	IF(IRX.EQ.6) PREAMP = EZDCPA(F)
	IF(IRX.EQ.7) PREAMP = BXPA(F)
	IF(IRX.EQ.8) PREAMP = BYPA(F)
	IF(IRX.EQ.9) PREAMP = BZPA(F)
	RETURN
	END
