	PROGRAM GNTST
C
C	MAKES A PLOT OF PREAMP GAIN TO COMPARE WITH STEVE'S DATA
C
	COMMON /GAINBLK/ PHASE,GNRP,GNIP      	  ! PHASE IS IN RADIANS
	CHARACTER*50 CH(20)
	CHARACTER*10 TITLE(4)
	DIMENSION FRGN(500,2)
C	INDPA = 0 MEANS DC, = 1 MEANS AC
C	DATA INDPA /1/
	DATA INDPA /0/
	DATA TITLE /'EX DC P/A','EX AC P/A',' ',' '/
C
	FMIN = .1
	FMAX = 100.
	IF(INDPA.EQ.1) THEN
	  FMIN = 20.
	  FMAX = 5.E5
	ENDIF
	NPT = 500
	FRAT = ALOG(FMAX/FMIN)/(NPT-1)
	FRAT = EXP(FRAT)
	F = FMIN/FRAT
	DO N = 1,NPT
	  F = F*FRAT
C	  FREQ(N) = F
C	  GAIN(N) = XPADC(F)
C	  WRITE(33,*) FREQ(N),GAIN(N)
	  FRGN(N,1) = F
	  IF(INDPA.EQ.0) FRGN(N,2) = XPADC(F)
	  IF(INDPA.EQ.1) FRGN(N,2) = XPAAC(F)
	  PRINT*,F,GNRP,GNIP,PHASE
	ENDDO
	CH(1) = 'PRINTER 6'
	CH(2) = 'ERASE'
	CH(3) = 'TICKSIZE -1  0  .02 .1'
	IF(INDPA.EQ.1) CH(3) = 'TICKSIZE -1  0  .2  1.'
	CH(4) = 'XCOLUMN 1'
	CH(5) = 'XLOGARITHM 1'
	CH(6) = 'YCOLUMN 2'
	CH(7) = 'LIMITS -1. 3.301  0.  1.265'     ! DC P/A'S LOW END
	CH(13) = 'RELOCATE -.5 1.0'
	CH(14) = 'LABEL EX DC P/A'
	IF(INDPA.EQ.1) THEN
	  CH(7) = 'LIMITS 1.398 5.699  0.  6.35'    ! AC P/A
	  CH(13) = 'RELOCATE 1.5 5.5'
	  CH(14) = 'LABEL EX AC P/A'
	ENDIF
	CH(8) = 'CONNECT'
	CH(9) = 'GRID 0'
	CH(10) = 'BOX'
	CH(11) = 'XLABEL FREQUENCY'
	CH(12) = 'YLABEL P/A GAIN'
	CH(15) = 'ID'
	CH(16) = 'HARDCOPY'
	CH(17) = 'END'
	CALL MONGO(17,CH,500,2,FRGN)
C
	IF(INDPA.EQ.1) STOP
	FMIN = 50.
	FMAX = 1.E6
	NPT = 500
	FRAT = ALOG(FMAX/FMIN)/(NPT-1)
	FRAT = EXP(FRAT)
	F = FMIN/FRAT
	DO N = 1,NPT
	  F = F*FRAT
	  FRGN(N,1) = F
	  FRGN(N,2) = XPADC(F)
	ENDDO
	CH(7) = 'LIMITS 1.699 6.0  0.  1.265'     ! DC P/A'S LOW END
	CH(13) = 'RELOCATE 2. 1.0'
	CALL MONGO(17,CH,500,2,FRGN)
	STOP
	END
	FUNCTION XPADC(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E8,22.E-12,5.E8,4.51E-12/
	DATA R33,C16 /2.2E+03,1.E-9/
	DATA TWOPI /6.2831853/
C
	C16 = 1.2E-9                             ! ADJUSTING EX
	C9  = 24.E-12                            ! ADJUSTING EX
	CINP = 4.92E-12                          ! ADJUSTING EX
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
C	PRINT*,'W,Z1,WC16',W,Z1,WC16
C	PRINT*,'F,CGAIN,PHASE
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	XPADC = CABS(CGAIN)
	RETURN
	END
	FUNCTION XPAAC(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN
	DATA R9,C9,R10,CINP /5.E8,22.E-12,5.E8,4.51E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.3E-9,10.E3,100.E-9/
	DATA C15,R32,C27,R11/ 0.,10.E3,470.E-12,1.5E3/
	DATA C25,R39,C30/470.E-9,330.,33.E-9/ 
	DATA RCOMP,CCOMP /22.1E3,47.E-12/
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
	C30 = 30.E-9                     ! ADJUSTING EX
	C13 = 3.7E-9                     ! ADJUSTING EX
C
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
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
	Z1 = CMPLX(R39,-1./WC30)
	WC25 = W*C25
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN*CMPLX(0.,-1./WC30)/(Z2+Z1)	
C
C	EFFECT OF INPUT BOARD
C
	WCC = W*CCOMP
	Z1 = CMPLX(0.,-1./WCC)	
	Z2 = CMPLX(RCOMP,-1./WCC)	
	Z3 = Z1/(Z1+Z2)
C*****************
C	CGAIN = CGAIN*Z3*Z3
C*************
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	XPAAC = CABS(CGAIN)
	RETURN
	END

