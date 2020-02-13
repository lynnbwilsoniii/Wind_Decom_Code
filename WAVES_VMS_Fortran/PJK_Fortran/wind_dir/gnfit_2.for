	PROGRAM GNFIT
C
C	ADJUSTS NANOFARAD CAPACITORS TO FIT MEASURED GAIN AND 
C	MAKES A PLOT OF PREAMP GAIN TO COMPARE WITH STEVE'S DATA
C
C	CHANGED ON 20 JAN 1997 TO USE STEVE'S DATA OF APRIL 20, 1992
C	FLIGHT MODEL PREAMPS + 810 BOARD BUT MEUDON NOT CONNECTED AND
C	SO NOT LOADING PREAMP OUTPUT.  
C	STEVE'S DATA IS READ IN FROM [.WIND]STEVES.DAT, INSTEAD OF
C	USING DATA STATEMENTS, AS WAS DONE EARLIER.
C
C	TO CHANGE TO ANOTHER PREAMP:
C		IF CHANGING BETWEEN DC AND AC:
C			CHANGE INDPA 
C			CHANGE INPUT PARAMS  (X'S)
C		IF ANOTHER COMPONENT:
C			CHANGE FIRST CALL TO E.G. FITXAC
C			CHANGE MONGO CALLS
C			CHANGE CH(14) = LABEL
C		IN EITHER CASE:
C			CHANGE SUBROUTINE ARGUMENT OF FINMIN
C			COMMENT OUT ALL COMMON PTBLK IN "FIT" S.R. 
C				EXCEPT DESIRED PREAMP
C	flight data change, common ptblk does not have to be out
C
	COMMON /GAINBLK/ PHASE,GNRP,GNIP      	  ! PHASE IS IN RADIANS
	COMMON /PTBLK/ STEVEG(40),STEVEF(40),NDATA
	CHARACTER*50 CH(20)
	CHARACTER*10 TITLE(6)
	EXTERNAL FITXDC,FITXAC,FITYDC,FITYAC,FITZDC,FITZAC
	DIMENSION FRGN(500,2),FRGNST(40,2)
	DIMENSION X(25),DX(25),Y(25),STDATA(40,3)
C	INDPA = 0 MEANS DC, = 1 MEANS AC
C	DATA INDPA /0/
	DATA INDPA /1/
	DATA TITLE /'EZ DC P/A','EZ AC P/A','EY DC P/A','EY AC P/A',
     1   'EX DC P/A','EX AC P/A'/
C
C	READ IN STEVE'S DATA
C
	OPEN(UNIT=33,FILE='STEVES.DAT',STATUS='OLD',READONLY)
	NDATA = 1
	DO N = 1,100
	  READ(33,*,END=100) STEVEF(NDATA),PHX,PHY,PHZ,STDATA(NDATA,1)
     1		,STDATA(NDATA,2),STDATA(NDATA,3)
	  NDATA = NDATA + 1
	ENDDO
 100	NDATA = NDATA-1
C	DO NOT DO POINT AT 100 KHZ, NOR 70 KHZ
	NDATA = NDATA-2
	PRINT*,NDATA,' POINTS READ IN'
	PRINT*,'FIRST LINE',STEVEF(1),(STDATA(1,I),I=1,3)
	PRINT*,'PENULT LINE',STEVEF(NDATA-1),(STDATA(NDATA-1,I),I=1,3)
	PRINT*,'LAST LINE',STEVEF(NDATA),(STDATA(NDATA,I),I=1,3)
C
C
	X(1) = 1.E-9                              ! C16, DC
	X(2) = 24.E-12                            ! C9 , DC
C	X(2) = 84.E-12                            ! C9 , EZ DC
	X(3) = 4.62E-12                           ! CINP,DC
C
	X(1) = 33.E-9                            ! C30, AC ALL
	X(2) = 100.E-9                           ! C26, AC ALL
	X(3) = 470.E-9				 ! C25, AC ALL
	X(4) = 3.3E-9				 ! C13, AC ALL
	X(5) = 100.E-9				 ! C29, AC ALL
	X(6) = 15.E-9				 ! C12, AC ALL
c	X(7) = 5.E-12				 ! CINP, AC ALL
C
	DX(1) = .1*X(1)
	DX(2) = .1*X(2)
	DX(3) = .1*X(3)
	DX(4) = .1*X(4)
	DX(5) = .1*X(5)
	DX(6) = .1*X(6)
C
C	PUT FLIGHT MODEL DATA IN STEVEG
C
	DO N = 1,NDATA
C	  STEVEG(N) = STDATA(N,1)			! EX
	  STEVEG(N) = STDATA(N,2)			! EY
C	  STEVEG(N) = STDATA(N,3)			! EZ
	ENDDO
C
	NVAR = 3
C	NVAR = 2                                   ! for EZDC
	IF(INDPA.NE.0) NVAR = 4
CPA	IF(INDPA.NE.0) CALL FITZAC(X,SUMSQ)
	IF(INDPA.NE.0) CALL FITYAC(X,SUMSQ)
	IF(INDPA.EQ.0) CALL FITXDC(X,SUMSQ)
	PRINT*,'ST X(N),SQ',(X(I),I=1,NVAR),SUMSQ
	DO IT = 1,15
	  IF(INDPA.EQ.0) CALL FINMIN(NVAR,X,DX,Y,FITXDC,SUMSQ)
	  IF(INDPA.EQ.0) CALL FINMIN(NVAR,Y,DX,X,FITXDC,SUMSQ)
	  IF(INDPA.NE.0) CALL FINMIN(NVAR,X,DX,Y,FITYAC,SUMSQ)
	  IF(INDPA.NE.0) CALL FINMIN(NVAR,Y,DX,X,FITYAC,SUMSQ)
	PRINT*,'FIT X(N),SQ',(X(I),I=1,NVAR),SUMSQ
	ENDDO
C
	NVAR = 6
	DX(1) = .1*X(1)
	DX(2) = .1*X(2)
	DX(3) = .1*X(3)
	DX(4) = .1*X(4)
	DX(5) = .1*X(5)
	DX(6) = .1*X(6)
	DX(7) = .1*X(7)
c
	DO IT = 1,15
	  IF(INDPA.EQ.0) CALL FINMIN(NVAR,X,DX,Y,FITXDC,SUMSQ)
	  IF(INDPA.EQ.0) CALL FINMIN(NVAR,Y,DX,X,FITXDC,SUMSQ)
CPA	  IF(INDPA.NE.0) CALL FINMIN(NVAR,Y,DX,X,FITZAC,SUMSQ)
	  IF(INDPA.NE.0) CALL FINMIN(NVAR,X,DX,Y,FITYAC,SUMSQ)
	  IF(INDPA.NE.0) CALL FINMIN(NVAR,Y,DX,X,FITYAC,SUMSQ)
	PRINT*,'FIT X(N),SQ',(X(I),I=1,NVAR),SUMSQ
	ENDDO
C	PRINT*,'FIT X(N),SQ',(X(I),I=1,NVAR),SUMSQ
c
	PRINT*,'RANDOMIZE'
	iseed = 83017
	DO IT = 1,6
	  x(it) = x(it) + (ran(iseed) - .5)*x(it) 
	enddo
c
	DX(1) = .1*X(1)
	DX(2) = .1*X(2)
	DX(3) = .1*X(3)
	DX(4) = .1*X(4)
	DX(5) = .1*X(5)
	DX(6) = .1*X(6)
	DX(7) = .1*X(7)
c
	DO IT = 1,20
	  IF(INDPA.EQ.0) CALL FINMIN(NVAR,X,DX,Y,FITXDC,SUMSQ)
	  IF(INDPA.EQ.0) CALL FINMIN(NVAR,Y,DX,X,FITXDC,SUMSQ)
CPA	  IF(INDPA.NE.0) CALL FINMIN(NVAR,Y,DX,X,FITZAC,SUMSQ)
	  IF(INDPA.NE.0) CALL FINMIN(NVAR,X,DX,Y,FITYAC,SUMSQ)
	  IF(INDPA.NE.0) CALL FINMIN(NVAR,Y,DX,X,FITYAC,SUMSQ)
	PRINT*,'FIT X(N),SQ',(X(I),I=1,NVAR),SUMSQ
	ENDDO
C
c	STOP
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
C	  GAIN(N) = YPADC(F)
C	  WRITE(33,*) FREQ(N),GAIN(N)
	  FRGN(N,1) = F
C	  IF(INDPA.EQ.0) FRGN(N,2) = YPADC(F)
	  IF(INDPA.EQ.1) FRGN(N,2) = YPAAC(F)
	  IF(INDPA.EQ.0) FRGN(N,2) = YPADC(F)
C	  IF(INDPA.EQ.1) FRGN(N,2) = ZPAAC(F)
C	  WRITE(33,*) F,GNRP,GNIP,PHASE
c	  PRINT*,F,GNRP,GNIP,PHASE
	ENDDO
C*****
C	IF(1) STOP
C****
	CH(1) = 'PRINTER 2'
	CH(2) = 'ERASE'
	CH(3) = 'TICKSIZE -1  0  .02 .1'
	IF(INDPA.EQ.1) CH(3) = 'TICKSIZE -1  0  .2  1.'
	CH(4) = 'XCOLUMN 1'
	CH(5) = 'XLOGARITHM 1'
	CH(6) = 'YCOLUMN 2'
	CH(7) = 'LIMITS -1. 3.301  0.  1.265'     ! DC P/A'S LOW END
	CH(13) = 'RELOCATE -.5 1.0'
	CH(14) = 'LABEL EY DC P/A'
	IF(INDPA.EQ.1) THEN
CPA	  CH(7) = 'LIMITS 1.398 5.699  0.  3.0'    ! EX AC P/A         
	  CH(7) = 'LIMITS 1.398 5.699  0.  7.5 '    ! Y,Z AC P/A
C	  CH(13) = 'RELOCATE 2.1 2.5'
	  CH(13) = 'RELOCATE 2.1 5.5'
CPA	  CH(14) = 'LABEL EZ AC P/A'
	  CH(14) = 'LABEL EY AC P/A'
CPA	  CH(14) = 'LABEL EX AC P/A'
	  PRINT*,CH(14)
	ENDIF
	CH(8) = 'CONNECT'
	CH(9) = 'GRID 0'
	CH(10) = 'BOX'
	CH(11) = 'XLABEL FREQUENCY'
	CH(12) = 'YLABEL P/A GAIN'
	CALL MONGO(14,CH,500,2,FRGN)
C
	DO N = 1,NDATA
	  FRGNST(N,1) = STEVEF(N)
	  FRGNST(N,2) = STEVEG(N)
	ENDDO
	DO N = NDATA+1,40
	  FRGNST(N,1) = STEVEF(NDATA)
	  FRGNST(N,2) = STEVEG(NDATA)
	ENDDO
	CH(1) = 'XCOLUMN 1'
	CH(2) = 'XLOGARITHM 1'
	CH(3) = 'YCOLUMN 2'
	CH(4) = 'PTYPE 6 1'
	CH(5) = 'POINTS'
	CH(6) = 'ID'
	CH(7) = 'HARDCOPY'
	CH(8) = 'END'
	CALL MONGO(8,CH,40,2,FRGNST)
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
	  FRGN(N,2) = ZPADC(F)
	ENDDO
	CH(1) = 'PRINTER 6'
	CH(2) = 'ERASE'
	CH(3) = 'TICKSIZE -1  0  .02 .1'
	IF(INDPA.EQ.1) CH(3) = 'TICKSIZE -1  0  .2  1.'
	CH(4) = 'XCOLUMN 1'
	CH(5) = 'XLOGARITHM 1'
	CH(6) = 'YCOLUMN 2'
	CH(7) = 'LIMITS 1.699 6.0  0.  1.265'     ! DC P/A'S HIGH END
	CH(8) = 'CONNECT'
	CH(9) = 'GRID 0'
	CH(10) = 'BOX'
	CH(11) = 'XLABEL FREQUENCY'
	CH(12) = 'YLABEL P/A GAIN'
C	CH(13) = 'RELOCATE 2. 1.0'
	CALL MONGO(12,CH,500,2,FRGN)
C
	DO N = 1,NDATA
	  FRGNST(N,1) = STEVEF(N)
	  FRGNST(N,2) = STEVEG(N)
	ENDDO
	CH(1) = 'XCOLUMN 1'
	CH(2) = 'XLOGARITHM 1'
	CH(3) = 'YCOLUMN 2'
	CH(4) = 'PTYPE 6 1'
	CH(5) = 'POINTS'
	CH(6) = 'ID'
	CH(7) = 'HARDCOPY'
	CH(8) = 'END'
	CALL MONGO(8,CH,NDATA,2,FRGNST)
C
	STOP
	END
	FUNCTION XPADC(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E8,22.E-12,5.E8,4.51E-12/
	DATA R33,C16 /2.2E+03,1.E-9/
	DATA TWOPI /6.2831853/
C
	C16 = 1.203E-9                             ! ADJUSTING EX
	C9  = 23.6E-12                            ! ADJUSTING EX
	CINP = 4.37E-12                          ! ADJUSTING EX
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
C	PRINT*,'W,Z1,WC16',W,Z1,WC16
C	PRINT*,'F,CGAIN,PHASE
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	XPADC = CABS(CGAIN)
	RETURN
	END
	FUNCTION YPADC(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E9,22.E-12,5.E9,4.51E-12/
	DATA R33,C16 /2.2E+03,1.E-9/
	DATA TWOPI /6.2831853/
C
	C16 = CVAR(1)
	C9 = CVAR(2)
	CINP = CVAR(3)
C
	C16 = 1.185E-9                            ! ADJUSTING EY
	C9  = 24.5E-12                            ! ADJUSTING EY
	CINP = 4.757E-12                          ! ADJUSTING EY
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
	YPADC = CABS(CGAIN)
	RETURN
	END
	FUNCTION ZPADC(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /18.E6,82.E-12,2.E8,4.51E-12/
	DATA R33,C16 /2.2E+03,1.E-9/
	DATA TWOPI /6.2831853/
C
C	C16 = 1.203E-9                             ! ADJUSTING EX
C	C9  = 23.6E-12                            ! ADJUSTING EX
C	CINP = 4.37E-12                          ! ADJUSTING EX
C
	C16 = CVAR(1)
	C9 = CVAR(2)
	CINP = CVAR(3)
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
	ZPADC = CABS(CGAIN)
	RETURN
	END
	END
	FUNCTION YPAACold(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN
	DATA R9,C9,R10,CINP /5.E9,22.E-12,5.E9,4.51E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.3E-9,10.E3,100.E-9/
	DATA C15,R32,C27,R11/ 0.,33.E3,150.E-12,4.7E3/
	DATA C25,R39,C30/470.E-9,330.,33.E-9/ 
	DATA RCOMP,CCOMP /22.1E3,47.E-12/
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
	C16 = 1.185E-9                            ! ADJUSTING EY
	C9  = 24.5E-12                            ! ADJUSTING EY
C	CINP = 4.757E-12                          ! ADJUSTING EY
	CINP = 5.02 E-12                          ! MID F DC GAIN .83
C
	C30 = CVAR(1)
	C26 = CVAR(2)
	C25 = CVAR(3)
	C13 = CVAR(4)
	C29 = CVAR(5)
	C12 = CVAR(6)
C	PRINT*,'C12',C12
C
C
C	C30 = 30.61E-9				  ! ADJUST EY
C	C26 = 98.23E-9				  ! ADJUST EY
C	C25 = 1.401E-6				  ! ADJUST EY
C	C13 = 34.37E-9				  ! ADJUST EY
C
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C	THIS PART IS USED TO DETERMINE THE INPUT CAPACITANCE, BUT WHEN
C	THE EXPERIMENT IS INTEGRATED, THIS PART IS COMPENSATED BY A
C	CIRCUIT IN THE MAIN BOX, SO IS TO BE COMMENTED OUT, AND REPLACED
C	BY A FIXED GAIN.
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
	YPAAC = CABS(CGAIN)
	RETURN
	END
	FUNCTION ZPAACold(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN
	DATA R9,C9,R10,CINP /18.E6,84.E-12,2.E8,4.99E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.3E-9,10.E3,100.E-9/
	DATA C15,R32,C27,R11/ 0.,33.E3,150.E-12,4.7E3/
	DATA C25,R39,C30/470.E-9,330.,33.E-9/ 
	DATA RCOMP,CCOMP /22.1E3,47.E-12/
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
C	C30 = 30.9E-9                     ! ADJUSTING EX
C	C13 = 3.7E-9                     ! ADJUSTING EX
C
	C30 = CVAR(1)
	C26 = CVAR(2)
	C25 = CVAR(3)
	C13 = CVAR(4)
C
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C	THIS PART IS USED TO DETERMINE THE INPUT CAPACITANCE, BUT WHEN
C	THE EXPERIMENT IS INTEGRATED, THIS PART IS COMPENSATED BY A
C	CIRCUIT IN THE MAIN BOX, SO IS TO BE COMMENTED OUT, AND REPLACED
C	BY A FIXED GAIN.
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
	Z1 = CMPLX(R39,-1./WC30)
	WC25 = W*C25
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN*CMPLX(0.,-1./WC30)/(Z2+Z1)	
C
C	EFFECT OF INPUT BOARD AND COMPENSATION
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
	ZPAAC = CABS(CGAIN)
	RETURN
	END
	SUBROUTINE FITXAC(X,SUMSQ)
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
	COMMON /PTBLK/ STEVEG(40),STEVEF(40),NDATA
	DIMENSION X(25)
C	STEVEF(30),STEVEG(30)
	DATA STEVEF /20.,30.,40.,70.,100.,150.,200.,300.,400.,600.,         !X
     1   1000.,2000.,3000.,4000.,6000.,8000.,1.E4,1.5E4,2.E4,3.E4,
     2   4.E4,5.E4,7.E4, 1.E5,1.5E5,2.E5,3.E5,4.E5,5.E5,1.E6,10*1.E6/
	DATA STEVEG /.03,.08,.16,.4, .55, .92,1.12,1.36, 1.5,1.55,          !X
     1  1.6,  1.68, 1.71, 1.80, 1.90, 2.11, 2.27, 2.4,  2.4, 2.25,
     2  1.9, 1.61, 1.20, .8,  .5,   .35, .25, .15,  .1,0.,10*0./
C
	CVAR(1) = X(1)
	CVAR(2) = X(2)
	CVAR(3) = X(3)
	CVAR(4) = X(4)
	CVAR(5) = X(5)
	CVAR(6) = X(6)
	CVAR(7) = X(7)
C
	SUMSQ = 0.
cold	NPT = 29
	NPT = NDATA
	DO N = 1,NPT
	  XGAIN = XPAAC(STEVEF(N))
	  SUMSQ = SUMSQ + (XGAIN - STEVEG(N))**2
	ENDDO
	RETURN
	END
	SUBROUTINE FITXDC(X,SUMSQ)
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
	DIMENSION X(25),STEVEF(32),STEVEG(32)
	DATA STEVEF /.1, .3, .5, .7,  1., 2., 3., 5.,6.,  7.,10.,
     1  15.,20.,30.,40.,50.,
     1  100.,300.,1000.,3000.,1.E4,1.5E4,2.E4,3.E4,4.E4,6.E4,8.E4, 
     2  1.E5,1.5E5,3.E5,5.E5,.8E6/
	DATA STEVEG /.5,.5 ,.5 ,.5  ,.51,.51,.51,.52,.81,.82,.83,
     1   5*.83,
     1   .83,.83,  .83, .83,  .83, .83,   .81 ,.77, .70, .60,.495,
     2   .44, .31, .165,.1,.06/
C
	CVAR(1) = X(1)
	CVAR(2) = X(2)
	CVAR(3) = X(3)
	CVAR(4) = X(4)
C
	SUMSQ = 0.
COLD	NPT = 32                              ! ALL
	NPT = 33                              ! ALL
	DO N = 1,NPT
	  XGAIN = YPADC(STEVEF(N))
	  SUMSQ = SUMSQ + (XGAIN - STEVEG(N))**2
	ENDDO
	RETURN
	END
	SUBROUTINE FITYDC(X,SUMSQ)
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
	COMMON /PTBLK/ STEVEG(40),STEVEF(40),NDATA
	DIMENSION X(25)
C	DATA STEVEF /.1, .3, .5, .7,  1., 2., 3., 4., 5.,6.,  7.,10.,
C     1  15.,20.,30.,40.,50.,
C     1  100.,300.,1000.,3000.,1.E4,1.5E4,2.E4,3.E4,4.E4,6.E4,8.E4, 
C     2  1.E5,1.5E5,3.E5,5.E5,.8E6/
C	DATA STEVEG /.5,.51,.53,.545,.58,.67,.73,.77,.80,.81,.82,.83,
C     1   5*.83,
C     1   .83,.83,  .83, .83,  .83, .83,   .81 ,.77, .70, .60,.495,
C     2   .44, .31, .165,.1,.06/
C
	CVAR(1) = X(1)
	CVAR(2) = X(2)
	CVAR(3) = X(3)
	CVAR(4) = X(4)
C
	SUMSQ = 0.
COLD	NPT = 33                              ! ALL
	NPT = 33                             ! FLIGHT MODEL
	DO N = 1,NPT
	  XGAIN = YPADC(STEVEF(N))
	  SUMSQ = SUMSQ + (XGAIN - STEVEG(N))**2
	ENDDO
	RETURN
	END
	SUBROUTINE FITYAC(X,SUMSQ)
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
	COMMON /PTBLK/ STEVEG(40),STEVEF(40),NDATA
C	DIMENSION STEVEG(40),STEVEF(40)
	DIMENSION X(25)
	DATA STEVEF / 20.,30.,40.,60.,80.,100.,120.,140.,160.,200.,300.,500.,
     1  700.,1000.,1500.,2000.,3000.,5000.,.7E4,1.E4, 1.2E4,1.5E4,1.7E4,
     2  2.E4,2.2E4,2.5E4,3.E4,4.E4,5.E4,6.E4,7.E4,1.E5,1.5E5,2.E5,3.E5,
     3  4.E5,5.E5,6.E5,2*1.E6/     !Y
	DATA STEVEG /.07,.15,.28,.55,.87,1.20,1.50,1.75,2.08,2.35,2.90,3.30, !Y
     1  3.40,3.55, 3.70, 3.80, 4.0,  4.50, 5.2, 5.90, 6.30, 6.6  ,6.69 , 
     2  6.70, 6.65, 6.50,6.10,5.25,4.5,3.85,3.30,2.15,1.30,  .85, .55,  
     3  .35,  .25,.21,2*0./
C
	CVAR(1) = X(1)
	CVAR(2) = X(2)
	CVAR(3) = X(3)
	CVAR(4) = X(4)
	CVAR(5) = X(5)
	CVAR(6) = X(6)
	CVAR(7) = X(7)
C
	SUMSQ = 0.
COLD	NPT = 38                              ! ALL
	NPT = NDATA                              ! FLIGHT MODEL
	DO N = 1,NPT
	  GAIN = YPAAC(STEVEF(N))
	  SUMSQ = SUMSQ + (GAIN - STEVEG(N))**2
	ENDDO
	RETURN
	END
	SUBROUTINE FITZDC(X,SUMSQ)
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
	COMMON /PTBLK/ STEVEG(40),STEVEF(40),NDATA
COLD	DIMENSION STEVEG(40),STEVEF(40)
	DIMENSION X(25)
c	DATA STEVEF /.1, .3,  .7,  1., 2., 3., 7., 10.,30.,50.,
	DATA STEVEF /10.,30., 70.,100.,200.,300.,700.,1000.,
     1  3000.,5000.,1.E4,1.5E4,2.E4,3.E4,4.E4,5.E4,7.E4, 
     2  1.E5,1.5E5,2.E5,3.E5,5.E5,.8E6,17*1.E6/
	DATA STEVEG /.92,.925,.93, .93, .94, .94, .94,  .94,
     1   2*.94,     .93, .92, .90,  .85, .79,.72 ,.61,
     2   .48, .36, .275,.19,.11,.07,17*0./
C
	CVAR(1) = X(1)
	CVAR(2) = X(3)
	CVAR(3) = X(2)
	CVAR(4) = X(4)
C
	SUMSQ = 0.
COLD	NPT = 23                               ! ALL
	NPT = NDATA                               ! FLIGHT MODEL
	DO N = 1,NPT
	  GAIN = ZPADC(STEVEF(N))
	  SUMSQ = SUMSQ + (GAIN - STEVEG(N))**2
	ENDDO
	RETURN
	END
	SUBROUTINE FITZAC(X,SUMSQ)
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
	COMMON /PTBLK/ STEVEG(40),STEVEF(40),NDATA
C	DIMENSION STEVEG(40),STEVEF(40)
	DIMENSION X(25)
	DATA STEVEF / 20.,30.,40.,60.,80.,100.,120.,140.,160.,200.,300.,500.,
     1  700.,1000.,2000.,3000.,4000.,5000.,.6E4,.7E4,.8E4,  1.E4, 
     2  1.1E4, 1.3E4, 1.5E4, 1.7E4,
     3  2.E4, 2.5E4, 3.E4, 4.E4, 5.E4, 7.E4, 1.E5, 1.5E5,2.E5,3.E5,
     4  4.E5,5.E5,6.E5,1.E6/     !Z
	DATA STEVEG /.07,.15,.28,.57,.94,1.30,1.60,1.88,2.12,2.48,3.10,3.60, !Z
     1  3.90,4.00, 4.10, 4.36,  4.60, 4.9, 5.28, 5.60,6.08 ,6.50 , 
     2  6.60,   6.90,  7.10,  7.15,
     3  7.15,  7.00, 6.60, 5.45, 4.70, 3.50, 2.35, 1.35, .9,   .59,
     4  .38,  .27,.21,0./
C
	CVAR(1) = X(1)
	CVAR(2) = X(2)
	CVAR(3) = X(3)
	CVAR(4) = X(4)
	CVAR(5) = X(5)
	CVAR(6) = X(6)
	CVAR(7) = X(7)
C
	SUMSQ = 0.
COLD	NPT = 39                              ! ALL
	NPT = NDATA                              ! FLIGHT MODEL
	DO N = 1,NPT
	  GAIN = ZPAAC(STEVEF(N))
	  SUMSQ = SUMSQ + (GAIN - STEVEG(N))**2
	ENDDO
	RETURN
	END
	FUNCTION EXDCPA(F)
C
C						 included 22 Jan 1997
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
	CGAIN = 0.
	EXDCPA=0.
	IF(F.EQ.0.) RETURN
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
C						 included 22 Jan 1997
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
	CGAIN = 0.
	EYDCPA=0.
	IF(F.EQ.0.) RETURN
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
C						 included 22 Jan 1997
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
	CGAIN = 0.
	EZDCPA=0.
	IF(F.EQ.0.) RETURN
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
	FUNCTION XPAAC(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
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
	C16 = 1.185E-9                            ! ADJUSTING EY
C	CINP = 4.757E-12                          ! 
	CINP = 4.83 E-12                          ! GAIN .83
C
	C30 = 30.9E-9                     ! ADJUSTING EX
	C13 = 3.7E-9                     ! ADJUSTING EX
C
	C30 = CVAR(1)
	C26 = CVAR(2)
	C25 = CVAR(3)
	C13 = CVAR(4)
	C29 = CVAR(5)
	C12 = CVAR(6)
c	CINP = CVAR(7)
C
	CGAIN = 0.
	EXACPA=0.
	XPAAC=0.
	IF(F.EQ.0.) RETURN
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
	XPAAC = EXACPA 
	RETURN
	END
	FUNCTION YPAAC(F)
C	FUNCTION EYACPA(F)
C						 included 22 Jan 1997
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
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
	CINP = 5.02 E-12                          ! GAIN .83
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
	C30 = CVAR(1)
	C26 = CVAR(2)
	C25 = CVAR(3)
	C13 = CVAR(4)
	C29 = CVAR(5)
	C12 = CVAR(6)
c	CINP = CVAR(7)
C
	CGAIN = 0.
	EYACPA=0.
	YPAAC = EYACPA 
	IF(F.EQ.0.) RETURN
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
	YPAAC = EYACPA 
	RETURN
	END
	FUNCTION ZPAAC(F)
C	FUNCTION EZACPA(F)
C						 included 22 Jan 1997
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(10)
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
	C30 = CVAR(1)
	C26 = CVAR(2)
	C25 = CVAR(3)
	C13 = CVAR(4)
	C29 = CVAR(5)
	C12 = CVAR(6)
c	CINP = CVAR(7)
C
C
	CGAIN = 0.
	EZACPA=0.
	ZPAAC = EZACPA
	IF(F.EQ.0.) RETURN
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
	ZPAAC = EZACPA
	RETURN
	END

