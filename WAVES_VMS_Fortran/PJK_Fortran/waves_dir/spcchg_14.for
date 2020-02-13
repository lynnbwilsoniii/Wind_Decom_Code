	PROGRAM SPCCHG
C
C	COMPUTES POTENTIAL IN A PLASMA CHAMBER, ASSUMING SPACE
C	CHARGE LIMITED FLOW OUTWARD ACROSS THE MAGNETIC FIELD.
C
      COMMON /PPAK/ XMIN,XMAX,DELX,YMIN,YMAX,DELY,FILFAC
      DIMENSION RADIUS(700),POTL(700),SYM(700),EXTRA(700)
	DATA IPR /1/
C
	YMAX = 0.
	YMIN = 0.
	DELY = 5.
	IPR = 2
	DO 200 N = 1,20
	CALL WOMPOT(0.,IPR,POTE,POTI,POT,E)
 200	CONTINUE
	IF(IPR.EQ.1) STOP
	IPR = 0
	DO 100 N = 1,531
	R = -40. + 80.*(N-1)/530
	RADIUS(N) = R
	CALL WOMPOT(R,IPR,POTE,POTI,POT,E)
	YMAX = AMAX1(POT,YMAX)
	YMIN = AMIN1(POT,YMIN)
	SYM(N) = 1.
	POTL(N) = POT
	EXTRA(N) = E
	EMAX = AMAX1(EXTRA(N),EMAX)
	EMIN = AMIN1(EXTRA(N),EMIN)
  100	CONTINUE
	CALL BITPLTBGN(' ',' ')
	CALL THROW(358)
	XMAX = 50.
	XMIN = -50.
	DELX = 5.
	YMAX = 3.*YMAX
	YMIN = 3.*YMIN
	FILFAC = 1.
	CALL PRINTXX(4,' POT',1,400)
	CALL LGRAPH(531,RADIUS,POTL,SYM)
	YMAX = 3.*EMAX
	YMIN = 3.*EMIN
	DELY = 100.
	FILFAC = 1.
	CALL PRINTXX(4,' E',1,400)
	CALL LGRAPH(531,RADIUS,EXTRA,SYM)
	CALL BPLTERM
	STOP
	END
	SUBROUTINE WOMPOT(R,IPR,POTE,POTI,POT,E)
C
C	COMPUTES POTENTIAL IN WOMBAT ON ASSUMPTION OF SPACE
C	CHARGE LIMITED FLOW OF IONS ACROSS THE FIELD
C
	REAL MASSI,MEC2	
	DATA FCE /20.E6/	!ELECTRON CYCLOTRON FREQUENCY
	DATA PTORR /5.E-6/	!PRESSURE IN TORR
	DATA VBEAM /200./	!BEAM ENERGY IN ELECTRON VOLTS
	DATA SIGMAI /2.6E-16/	!IONIZATIONS CROSS SECTION IN CM**2
	DATA BEAMI /1.E-6/	!BEAM CURRENT IN AMPERES
	DATA MASSI /30./	!ATOMIC WT. OF IONS
	DATA PI /3.14159265/	
	DATA MEC2 /.51E6/	!M C**2 FOR ELECTRONS IN EV
	DATA GUNDIV /9./	!GUN DIVERGENCE ANGLE IN DEGREES
	DATA SECEMC /.5/	!SECONDARY EMISSION COEFFICIENT
	DATA ESEC /3./		!ENERGY OF END PLATE SECONDARIES IN EV
	DATA RC /40./		!RADIUS OF CONDUCTING WALL IN CM
	DATA AXL /100./		!LENGTH OF INTERACTION REGION
	DATA QE /4.807E-10/	!ELECTRON CHARGE IN ESU
	DATA C /3.E10/		!VELOCITY OF  LIGHT IN CGS
C
	IF(IPR.NE.2) GO TO 20
	PRINT*,'FCE',FCE,' PTORR',PTORR
	PRINT*,'VBEAM',VBEAM,' BEAMI',BEAMI
	PRINT*,'MASSI',MASSI,' AXLENGTH',AXL
	PRINT*,'GUNDIV',GUNDIV,' RC',RC
	PRINT*,'ESEC',ESEC,' SECEMC',SECEMC
	IPR = 1
  20	QECOUL = 10.*QE/C
	VESU = C*1.E-8			!300 = VOLTS TO ESU
	EPS0 = 1.E11/(4.*PI*C**2)
C	IONIZATION CROSS SECTION, COMPROMISE FOR O2 AND N2
	SIGMAI = 131.7E-16*(VBEAM-15.)/(1357. + VBEAM**1.73)
	PRINT*,'FCE',FCE,' SIGMAI',SIGMAI
C	ELECTRON BEAM LARMOR RADIUS
	VE = SQRT(2.*VBEAM/MEC2)*C
	RLBE = VE*SIN(GUNDIV/57.295)/(2.*PI*FCE)

C	ION BEAM LARMOR RADIUS AT kT = 26 meV
	VTI = SQRT(2.*.026/(MASSI*1836.*MEC2))*C
	RLI = VTI*1836.*MASSI/(2.*PI*FCE)
	IF(IPR.NE.0)PRINT*,'RLBE,RLI',RLBE,RLI

C	TOTAL NO. OF ELECTRON IN CHAMBER
C	BEAM ELECTRONS
	TOTNBE = (BEAMI/QECOUL)*(AXL/VE)
	VOLE = PI*AXL*(2.*RLBE)**2
	DENSE = TOTNBE/VOLE
C	SEC. EMISSION ELECTRONS FROM END PLATE
	VSE = SQRT(2.*ESEC/MEC2)*C
	SECFAC = SECEMC*VE/VSE
	DENSE = DENSE*(1. + SECFAC)
	IF(IPR.NE.0)PRINT*,'SECFAC,DENSE',SECFAC,DENSE

C	TOTAL NO. OF IONS IN CHAMBER
C	FIRST COMPUTE NEUTRAL DENSITY
	DENSNT = (6.022E23/23900.)*(PTORR/760.)
C	LIFETIME OF IONS
	TAUI = .5*AXL*1.4142/VTI
	DNIDT = (BEAMI/QECOUL)*AXL*DENSNT*SIGMAI
	TOTNI = DNIDT*TAUI
	VOLI = PI*AXL*(2.*RLI)**2
	DENSI = TOTNI/VOLI
	IF(IPR.NE.0)PRINT*,'NI,TAUI,DENSI',TOTNI,TAUI,DENSI
C
C	POTENTIAL AND ELECTRIC FIELD
C
	RA = ABS(R)
	IF(RA.GE.(2.*RLBE)) GO TO 10
C	R IS INSIDE THE BEAM
	POTE = -PI*DENSE*QE*(2.*RLBE)**2*
     1  (1. - (.5*R/RLBE)**2 + 2.*ALOG(.5*RC/RLBE))*VESU
	EEL = -2.*PI*DENSE*QE*R*VESU*100.
	GO TO 30
  10	CONTINUE
C	R IS OUTSIDE THE BEAM
	POTE = -PI*DENSE*QE*(2.*RLBE)**2*2.*ALOG(RC/RA)*VESU
	IF(R.NE.0.)
     1	EEL = -2.*PI*DENSE*QE*(2.*RLBE)**2*VESU*100./R
C
  30	IF(RA.GE.(2.*RLI)) GO TO 40
C	IONS,  R IS INSIDE THE BEAM
	POTI =  PI*DENSI*QE*(2.*RLI )**2*
     1  (1. - (.5*R/RLI)**2 + 2.*ALOG(.5*RC/RLI ))*VESU
	EI =  2.*PI*DENSI*QE*R*VESU*100.
	GO TO 50
  40	CONTINUE
C	IONS,  R IS OUTSIDE THE BEAM
	POTI =  PI*DENSI*QE*(2.*RLI )**2*2.*ALOG(RC/RA)*VESU
	IF(R.NE.0.)
     1	EI =  2.*PI*DENSI*QE*(2.*RLI)**2*VESU*100./R
  50	POT = POTE + POTI
	E = EEL + EI
  60	IF(IPR.NE.0) PRINT 1001,'R,POTE,POTI,POT,E',R,POTE,POTI,POT,E
C  60	IF(IPR.NE.0) PRINT 1001,'R,POTE,POTI,POT,E',R,POTE,POTI,POT,E
	FCE = FCE*1.2
	RETURN
 1001	FORMAT(1X,A17,F10.3,5F12.4)
	END

