	PROGRAM NOISE1
C
C	MODIFIED FROM [KELLOGG.STEREO]NOISE1.FOR, MODIFIED FROM NOISE.FOR TO 
C		USE THE SUBROUTINES PREAMP,TDSGAIN,LRSGAIN. 
C		APMGAIN IS A SEPARATE SUBROUTINE WHICH INCLUDES BOTH THE
C		PARTS OF THE CIRCUIT IN THE PREAMP BOX AND THE MAIN BOX
C
C	CALCULATES A FILE OF NOISE VALUES
C	modified for Stereo electric preamp, for circuit as changed in 
C		Jan 2002. (drawing dated 4 Feb.)  Now two files are produced:
C		the low rate science output and the mid freq output.
C		Gains for the two outputs, also.
C	NOW PLOTS (NOISEPA) SIGNAL AT PREAMP INPUT,  (NOISEL) NOISE AT
C		LOW RATE SCIENCE A/D, AND (NOT DONE YET) NOISE AT TDS
C		A/D CONVERTER.  (NOT DONE YET) ALSO CALCULATES RMS VOLTS
C		FOR THESE CASES
C
C	IN THE FOLLOWING:
C		RANT AND RBASE ARE THE ANTENNA AND BASE RESISTANCES, 
C		CALCULATED FROM KELLOGG AND BALE, JGR 106, 18721-7, (2001)
C		CANT AND CBASE ARE THE CORRESPONDING CAPACITANCES
C		R2, CIN ARE THE RESISTANCE FROM DEVICE + INPUT TO
C			CHASSIS AND CIN IS COUPLING CAPACITOR
C		R1 IS RESISTANCE FROM ANTENNA TO S/C GROUND
C	NEW PHILOSPHY IS THIS PROGRAM --  COMPUTE SIGNALS AT ANTENNA
C		IN MAIN PROGRAM, RELEGATE CIRCUIT OUTPUTS TO SUBROUTINES
C
	REAL CASFREQ(1025),CASSIG(1025)
	REAL EN(5),IN(5),FTABLE(5)
	REAL VTOT2OUT(30)
	REAL VN2SV(30)
C
	COMPLEX GAPMCPI,GTDSC,GLRSC,GAPMC,GMCPI,GMC,ZIN
	COMPLEX YTEMP,ZTEMP,ZTEMP2,ZINEQ,Z1,YC1,EFFLENC
C
	COMMON /LOWOUT/ V2ENOUT(50),V2INOUT(50)
	COMMON /BOTH/ NF,FLIST(50),VJOHN2(50),VSHOT2(50),VSCS2(50)
	COMMON /ANTENNZ/ RANTPL,RBASE,CANT,CBASE
C
	DATA R1,CIN /1.5E8,1.E-10/
	DATA CANT /53.E-12/		! from Bob Manning, for Stacer
	DATA CBASE /70.E-12/
	DATA TWOPI /6.28318531/
	DATA BOLTZK /1.3807E-23/
	DATA CLIGHT /2.9979E10/
	DATA EVME /.511E6/
	DATA QESI /1.603E-19/
	DATA FLIST /.033,.18, 1., 5.5, 30., 45*1./
	DATA T_PREAMP /273./		! 0. C
	DATA DENSE /7./
	DATA TEMPE /10./		! ELECTRON TEMP IN EV
	DATA ANTLEN,ANTDIA,BASEDIA/ 600.,2.4,3.2/  ! IN CM, DIA IS AVERAGE
	DATA SATAREA /18.6E4/			   ! IN CM^2
	DATA SATCAP /100.E-12/			   ! 100 pF for S/C
C
C	DEVICE DATA FOR 2N3821 (WIND)
C
c	DATA EN /13.E-9,4.5E-9,2.5E-9,1.8E-9,1.6E-9/
c	DATA IN /2*.4E-16,4.5E-16,6.E-16,1.5E-15/
C
C	DEVICE DATA FOR 2N4861 (MANNING PROPOSAL FOR STEREO)
C
	DATA EN /15.E-9,7.E-9,4.E-9,2.8E-9,2.E-9/
	DATA IN /3*.9E-15,1.E-15,2.E-15/
C
	IA = 2
	IB = 2
C
C	CALCULATE ANTENNA RESISTANCE FROM KELLOGG AND BALE
C
	PHOTAREA = ANTDIA*ANTLEN			 ! in cm^2
	PUPAREA = ANTDIA*ANTLEN			 ! in cm^2
	TPHI = 1.3*11600.
	FLUX = DENSE*SQRT(TEMPE/TWOPI/EVME)*CLIGHT       ! per cm^2
	RANT = BOLTZK*TPHI/(QESI**2)/FLUX/PUPAREA   ! in SI AND OHMS
	RA = RANT
C*********
C	RA = 100.
C*********
	RANTPL = RANT
	PRINT*,'PICKUP AREA,FLUX,RANT',PUPAREA,FLUX,RANTPL
C
C	CALCULATE BASE RESISTANCE FROM KELLOGG AND BALE
C
	BFLUX = DENSE*SQRT(TEMPE)
	RBASE = (.038/BASEDIA)*(110.E6 + 654.E6/BFLUX)
	RBASE = 2.*RBASE			! FUDGE FOR BIG ANTS
	RB = RBASE
	GANT = 1./RANT + 1./RBASE
	RAPAR = 1./GANT
	PRINT*,'PL,BASE,PAR',RANT,RBASE,RAPAR	 
C
	NF = 34
	FLIST(1) = .0003
	RATIO = 1.874
	DO IFR = 2,NF
	  FLIST(IFR) = FLIST(IFR-1)*RATIO
	ENDDO
	PRINT*,'NF,F RANGE',NF,FLIST(1),FLIST(NF)
C
	ANTAREA = .5*TWOPI*ANTDIA*ANTLEN
C	PRINT*,'ELECTRON PICKUP AREA',ANTAREA
	DO IFR = 1,NF
	  F = FLIST(IFR)
	  W = TWOPI*F	
C
	  CALL PREAMP(F,GMCPI,GMPI,PHASE,ZIN,VN2P)
C	  CALL TDSGAIN(F,IFIL,GMCPI,GMPI,PHASE,VN2T)
	  CALL LRSGAIN(F,GLRSC,GLRS,PHASE,VN2L)
	  CALL APMGAIN(F,GAPMCPI,GAPMPI,PHASE,VN2A)
C	  CALL CHECK
C	  IF(1) STOP
C	WRITE(60,*) F,VM,VAPM,GM,GAPM
C
C	  CALCULATE JOHNSON NOISE
C
C	  VJ02 = 4.*BOLTZK*T_PREAMP*R2*R3/(R2+R3)
C	  RPY1 = 1./RBASE + 1./RANT
C	  Z1 = 1./CMPLX(RPY1,W*CANT)
C	  Z1 = Z1 + CMPLX(R1,-1./W/CIN)
C	  I1 = SQRT(VJ02)/Z1 
C	  V1 = SQRT(VJ02) - I1*R1
C	  VJ2 = CABS(V1)**2
	  VJOHN2(IFR) = VN2P
C
C	  CALCULATE DEVICE NOISE
C
	  ZANT = 1./CMPLX(1./RANT,W*CANT)
	  YC1 = 1./ZANT + 1./RBASE            ! ANTENNA, BASE IN PARALLEL
	  Z1 = 1./YC1 + CMPLX(0.,-1./W/CIN)	 ! ADD CIN IN SERIES
	  YC1 = 1./Z1 + 1./R1			 ! PUT RIN IN PARALLEL
	  ZINEQ = 1./YC1
	  ZINEQ2 = CABS(ZINEQ)**2
C
	  IF(F.LT.10) THEN
C		ASSUME En**2 ~ 1/F BELOW 10 HZ, In CONSTANT
	    V2EN = (EN(1)**2)*(10./F)
	    V2IN = (IN(1)**2)*ZINEQ2
	  ELSEIF(F.LT.100000.) THEN
C		INTERPOLATE
	    CALL INTERP(FTABLE,EN,5,F,VEN,ERR)
	    V2EN = VEN**2
	    CALL INTERP(FTABLE,IN,5,F,VINT,ERR)
	    V2IN = VINT**2*ZINEQ2
	  ELSE
C		EXTRAPOLATE, En CONSTANT, In ~ F
	    V2EN = (EN(5)**2)
	    V2IN = (IN(5)**2)*ZINEQ2*(F/1.E5)**2
	  ENDIF
	  V2ENOUT(IFR) = V2EN
	  V2INOUT(IFR) = V2IN
C
C	  CALCULATE SHOT NOISE
C
	  VTHERM = SQRT(16.*TEMPE/EVME/TWOPI)*CLIGHT		!CGS
	  FLUX = .25*DENSE*VTHERM			! e's /cm^2-sec
	  TFLUX = FLUX*ANTAREA
	  YTEMP = 1./ZIN
	  GEQ = 1./RANT + 1./RBASE + REAL(YTEMP)
	  XEQ = IMAG(YTEMP) + W*(CANT + CBASE)
	  REQ = 1./GEQ
	  VS2 = 2.*QESI**2*TFLUX*REQ**2/(1. + (REQ*XEQ)**2)
C
C	  THAT IS THE SHOT NOISE ON THE ANTENNA
C
C	  PRINT*,'VTHERM,FLUX,TFLUX',VTHERM,FLUX,TFLUX
C	  PRINT*,F,VJ2,VS2,VTOT2
	  VSHOT2(IFR) = VS2
	  VTOT2OUT(IFR) = VS2 + VN2A
C	  VN2SV(IFR) = VN2
C
C	  CALCULATE SHOT NOISE ON THE SATELLITE BODY, AND THEN COUPLING
C		TO ANTENNA.  I ASSUME RBASE AND CBASE COUPLING TO
C		ANTENNA, THEN RANT AND CANT COUPLING TO PLASMA (GROUND)
C
	  TFLUX = FLUX*SATAREA
	  SATPAREA = 2.73E4
	  RSAT = BOLTZK*TPHI/(QESI**2)/FLUX/SATAREA   		! in SI
	  VSSAT2 = 2.*QESI**2*TFLUX*RSAT**2/(1. + (W*RSAT*SATCAP)**2)
	  ZTEMP = CMPLX(RANTPL,W*CANT)
	  ZTEMP2 = CMPLX(RBASE,W*CBASE)	
	  VSCS2(IFR) = VSSAT2*CABS(ZTEMP2/(ZTEMP+ZTEMP2))**2	 
	  print*,'sat',vssat2,ztemp,ztemp2
	ENDDO
c	WRITE(99,199) (FLIST(IFR),IFR=1,NF)
c	WRITE(99,199) (VSHOT2(IFR),IFR=1,NF)
c	WRITE(99,199) (VSCS2(IFR),IFR=1,NF)
c	WRITE(99,199) (VTOT2OUT(IFR),IFR=1,NF)
 199	FORMAT(5X,'1',5(E12.3,','))
C	DO IFR = 1,NF
C	  WRITE(98,198) FLIST(IFR),VJL2(IFR),VSL2(IFR),V2ENOUT(IFR),
C     1	   V2INOUT(IFR),VTOT2OUT(IFR)
C	ENDDO
C	DO IFR = 1,NF
C	  WRITE(99,198) FLIST(IFR),VJM2(IFR),VSM2(IFR),V2ENOUT(IFR),
C     1	   V2INOUT(IFR),VTOT2OUT(IFR)
C	ENDDO
 198	FORMAT( F10.5,6E11.3)
	ITERM = 3
	ITERM = -1
	CALL NOISEL(ITERM,SIGSQ)	! AT LOW RATE SCIENCE A/D
	PRINT*,'AT LRS OUTPUT, V RMS= ',SQRT(SIGSQ)
C	ITERM = 3
	CALL NOISEPA(ITERM)		! AT PREAMP INPUT
	IFILT = 0
	CALL NOISET(ITERM,IFILT,SIGSQ)	! AT TDS OUTPUT
	PRINT*,'AT TDS OUTPUT, V RMS= ',SQRT(SIGSQ)
	STOP
	END
	SUBROUTINE NOISEPA(ITERM)
C
C	PLOT SHOT NOISE, RESISTOR NOISE,DEVICE NOISE,AND A CASSINI SPECT.
C		AT ANTENNA OR PREAMP INPUT
C
	COMMON /ANTENNZ/ RANTPL,RBASE,CANT,CBASE
	COMMON /LOWOUT/ V2ENOUT(50),V2INOUT(50)
	COMMON /BOTH/ NF,FLIST(50),VJOHN2(50),VSHOT2(50),VSCS2(50)
	REAL CASFREQ(1025),CASSIG(1025),VJMC2(50)
	COMPLEX EFFLENC,GMCMPI,ZIN
C
	CHARACTER*12 TITLE(30)
	CHARACTER*120 STR
	CHARACTER*1 DISPOSE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV,DDS
C
	DIMENSION YY(2048),YYT(2048),XX(2048)
	DATA TWOPI /6.2831853/
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,1000.,2100.,2600.)
	ENDIF
	CALL MGOSETEXPAND(.8)
C
	CALL MGOSETLIM( -2.,  -15., 1.7 , -4.)
	CALL MGOTICKSIZE(-1.,-1.,-1.,-1)
	PLIMIT = 10.**Y1
C
C	GET JOHNSON NOISE AT ANTENNA  (VN2 OF PREAMP IS AT INPUT TO
C		FET)  (not done yet)
C
	DO N = 1,NF
	  CALL PREAMP(FLIST(N),GMCPI,GMPI,PHASE,ZIN,VN2P)
	  VJMC2(N) = AMAX1(PLIMIT,VN2P)
	ENDDO
C
C	PLOT ANTENNA SHOT NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VSHOT2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
C	LABL = NF/2 - 1
	LABL = 15
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(15,'-ANT SHOT NOISE')
C
C	PLOT SATELLITE SHOT NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VSCS2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
C	LABL = NF/2 +2
	LABL = 10
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(15,'-SAT SHOT NOISE')
C
C	PLOT RESISTOR NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VJMC2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = 15
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(14,'-JOHNSON NOISE')
C	
C	  CALL MGOSETEXPAND(.85)
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
C	  ELSE
C	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
C	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
C
C	PUT ON A SPECTRUM FROM CASSINI
C
	OPEN(UNIT=59,FILE='[KELLOGG.CASSINI.PAPERS]TSPECTAV.DAT;329'
     1		,STATUS='OLD',READONLY)
C
	READ(59,*) NUMBER
	PRINT*,'NUMBER OF SPECTRA AVERAGED',NUMBER
	NC = 0
  200   READ(59,*,END=210) T1,T2,T3,T4,T5,T6
C	T1 IS FREQ, T2 IS EX SPECTRUM V/M**2/HZ, T3 IS EZ, T456 ARE BXYZ
	IF(T1.EQ.0.) GO TO 200
 	NC = NC+1
 	CASFREQ(NC) = T1
	CALL ANTENNAL(T1,EFFLENC,EFFLEN)
	CASSIG(NC) = T3*EFFLEN**2     ! MULTIPLY BY EFFECTIVE LENGTH SQUARED
 	GO TO 200
  210	PRINT*,NC,' CASSINI DATA POINTS' 
	CLOSE(UNIT=59)
C	THIS IS PREAMP INPUT 
	DO N = 1,NC
C	  CALL PREAMP(CASFREQ(N),GMCPI,GMPI,PHASE,ZIN,VN2)
	  CALL ANTENNAL(CASFREQ(N),EFFLENC,EFFLEN)
	  XX(N) = ALOG10(CASFREQ(N))
	  YY(N) = ALOG10(CASSIG(N)*EFFLEN**2)
C	  WRITE(97,198) CASFREQ(N),GAPM**2*CASSIG(N),GM**2*CASSIG(N)
	ENDDO
	CALL MGOCONNECT(XX,YY,NC)
C
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(9,'FREQ (Hz)')
	CALL MGOYLABEL(19,'APM OUTPUT V\U2/Hz')
	CALL MGOPLOTID('[.STEREO]NOISE','APM')
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  READ(5,1023) DISPOSE
 1023	  FORMAT(A1)
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END
	SUBROUTINE NOISEL(ITERM,SIGSQ)
C
C	PLOT SHOT NOISE, RESISTOR NOISE,DEVICE NOISE,AND A CASSINI SPECT.
C		AT LOW RATE SCIENCE A/D
C
	COMMON /ANTENNZ/ RANTPL,RBASE,CANT,CBASE
	COMMON /LOWOUT/ V2ENOUT(50),V2INOUT(50)
	COMMON /BOTH/ NF,FLIST(50),VJOHN2(50),VSHOT2(50),VSCS2(50)
	REAL CASFREQ(1025),CASSIG(1025),VJMC2(50),VSMC2(50),VSSMC2(50)
	COMPLEX GMCPI,GLRSC,ZIN,EFFLENC
C
	CHARACTER*12 TITLE(30)
	CHARACTER*120 STR
	CHARACTER*1 DISPOSE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV,DDS
C
	DIMENSION YY(2048),YYT(2048),XX(2048)
	DATA TWOPI /6.2831853/
C
	print*,'noisel,nf=',nf,flist(1),flist(nf)
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,1000.,2100.,2600.)
	ENDIF
	CALL MGOSETEXPAND(.8)
C
	CALL MGOSETLIM( -2.,  -15., 2.3 , -4.)
	PLIMIT = 10.**Y1
	CALL MGOTICKSIZE(-1.,-1.,-1.,-1)
C
C	CORRECT STUFF FOR GAINS OF PREAMP AND LRS CIRCUIT
C
	DO N = 1,NF
	  CALL PREAMP(FLIST(N),GMCPI,GMPI,PHASE,ZIN,VN2P)
	  CALL LRSGAIN(FLIST(N),GLRSC,GLRS,PHASE,VN2L)
	  CORRF = (GLRS*GMPI)**2
C	  VJMC2(N) = CORRF*VJM2(N)
	  VJMC2(N) = AMAX1(PLIMIT,CORRF*VN2P)
	  VSMC2(N) = AMAX1(PLIMIT,CORRF*VSHOT2(N))
	  VSSMC2(N) = AMAX1(PLIMIT,CORRF*VSCS2(N))
C	write(23,*)'vsm2,vsmc2',n,vsm2(n),vsmc2(n),corrf
	ENDDO
C
C	PLOT ANTENNA SHOT NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VSMC2(N))
C	write(23,*)'vsm2,vsmc2',n,vsm2(n),vsmc2(n),xx(n),yy(n)
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = NF/2 + 1
	LABL = 15
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(15,'-ANT SHOT NOISE')
C	write(23,*) 'ant shot noise at',xx(labl),yy(labl)
C
C	PLOT SATELLITE SHOT NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VSSMC2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = NF/2 +2
	LABL = 14
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(15,'-SAT SHOT NOISE')
C
C	PLOT RESISTOR NOISE
C
	DO N = 1,NF
C	write(23,*)'vjm2,vsm2',n,flist(n),vjm2(n),vsm2(n)
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VJMC2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = 14
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(14,'-JOHNSON NOISE')
C	write(23,*) 'johnson noise at',xx(labl),yy(labl)
C	
C	  CALL MGOSETEXPAND(.85)
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
C	  ELSE
C	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
C	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
C
C	PUT ON A SPECTRUM FROM CASSINI
C
	OPEN(UNIT=59,FILE='[KELLOGG.CASSINI.PAPERS]TSPECTAV.DAT;329'
     1		,STATUS='OLD',READONLY)
C
	READ(59,*) NUMBER
	PRINT*,'NUMBER OF SPECTRA AVERAGED',NUMBER
	NC = 0
  200   READ(59,*,END=210) T1,T2,T3,T4,T5,T6
	IF(T1.EQ.0.) GO TO 200
 	NC = NC+1
 	CASFREQ(NC) = T1
	CALL ANTENNAL(T1,EFFLENC,EFFLEN)
	CASSIG(NC) = T3*EFFLEN**2     ! MULTIPLY BY EFFECTIVE LENGTH SQUARED
 	GO TO 200
  210	PRINT*,NC,' CASSINI DATA POINTS, RMS V= ',SQRT(SIGSQ),' VOLTS' 
	CLOSE(UNIT=59)
	SIGSQ = 0.
	DO N = 1,NC
	  CALL PREAMP(CASFREQ(N),GMCPI,GMPI,PHASE,ZIN,VN2P)
	  CALL LRSGAIN(CASFREQ(N),GLRSC,GLRS,PHASE,VN2L)
C	  CALL PREAMP(CASFREQ(N),VM,VAPM,GM,GAPM,GMPI,GAPMPI)
	  XX(N) = ALOG10(CASFREQ(N))
	  YY(N) = ALOG10((GLRS*GMPI)**2*CASSIG(N))
C	  ASSUME UNIFORM FREQUENCY SPACING AT CASFREQ(1)
	  SIGSQ = SIGSQ + CASFREQ(1)*(GLRS*GMPI)**2*CASSIG(N)
C	  write(23,*) n,casfreq(n),cassig(n),sigsq
	ENDDO
	CALL MGOCONNECT(XX,YY,NC)
C
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(9,'FREQ (Hz)')
	CALL MGOYLABEL(18,'LRS OUTPUT V\U2/Hz')
	CALL MGOPLOTID('[.STEREO]NOISE','LRS')
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  READ(5,1023) DISPOSE
 1023	  FORMAT(A1)
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END
	SUBROUTINE CHECK
C
C	MAKES A TABLE TO CHECK AGAINST STEVE'S MEASUREMENTS
C
	REAL FLIST(10)
	COMPLEX GMCPI,ZIN
	DATA FLIST /1.,2.,5.,7.,6*10./
C
	FACTOR = .1
	DO N = 1,8
	  DO J = 1,4
	    F = FACTOR*FLIST(J)	
	    CALL PREAMP(F,GMCPI,GMPI,PHASE,ZIN,VN2)
C	    CALL PREAMP(F,VM,VAPM,GM,GAPM,GMPI,GAPMPI)
C	    WRITE(67,*) F,GAPMPI,GMPI
	  ENDDO
	  FACTOR = 10.*FACTOR
	ENDDO
	RETURN
	END
	SUBROUTINE ANTENNAL(F,EFFLENC,EFFLEN)
C
	COMMON /ANTENNZ/ RANTPL,RBASE,CANT,CBASE
	COMPLEX YC1,Z1,YC2,Z2,ZIN,GMCPI,EFFLENC
	DATA BARELEN /3./	! EFFECTIVE LENGTH W/O INPUT DIVISION
	DATA TWOPI /6.28318531/
C	
	W = TWOPI*F
	CALL PREAMP(F,GMCPI,GMPI,PHASE,ZIN,VN2)
	YC1 = CMPLX(1./RBASE,W*CBASE) + 1./ZIN
	Z1 = 1./YC1
	YC2 = CMPLX(1./RANTPL,W*CANT)
	Z2 = 1./YC2
	EFFLENC = BARELEN*Z1/(Z1+Z2)
	EFFLEN = CABS(EFFLENC)
C
	RETURN
	END
	SUBROUTINE NOISET(ITERM,IFILT,SIGSQ)
C
C	PLOT SHOT NOISE, RESISTOR NOISE,DEVICE NOISE,AND A CASSINI SPECT.
C		AT TDS A/D
C
	COMMON /ANTENNZ/ RANTPL,RBASE,CANT,CBASE
	COMMON /LOWOUT/ V2ENOUT(50),V2INOUT(50)
	COMMON /BOTH/ NF,FLIST(50),VJOHN2(50),VSHOT2(50),VSCS2(50)
	REAL CASFREQ(1025),CASSIG(1025),VJMC2(50),VSMC2(50),VSSMC2(50)
	COMPLEX GMCPI,GTDSC,ZIN,EFFLENC
C
	CHARACTER*12 TITLE(30)
	CHARACTER*120 STR
	CHARACTER*1 DISPOSE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV,DDS
C
	DIMENSION YY(2048),YYT(2048),XX(2048)
	DATA TWOPI /6.2831853/
C
	print*,'noisel,nf=',nf,flist(1),flist(nf)
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,1000.,2100.,2600.)
	ENDIF
	CALL MGOSETEXPAND(.8)
C
	CALL MGOSETLIM( 0.,  -15., 5.5 , -4.)
	PLIMIT = 10.**Y1
	CALL MGOTICKSIZE(-1.,-1.,-1.,-1)
C
C	CORRECT STUFF FOR GAINS OF PREAMP AND TDS CIRCUIT
C
	SIGSQ = 0.
C
	DO N = 1,NF
	  CALL PREAMP(FLIST(N),GMCPI,GMPI,PHASE,ZIN,VN2P)
	  CALL TDSGAIN(FLIST(N),GTDSC,GTDS,PHASE,VN2T)
	  CORRF = (GTDS*GMPI)**2
C	  VJMC2(N) = CORRF*VJM2(N)
	  VJMC2(N) = AMAX1(PLIMIT,CORRF*VN2P)
	  VSMC2(N) = AMAX1(PLIMIT,CORRF*VSHOT2(N))
	  VSSMC2(N) = AMAX1(PLIMIT,CORRF*VSCS2(N))
	  IF(N.GT.1) SIGSQ = SIGSQ 
     1	    + .5*(FLIST(N)-FLIST(N-1))*(VSMC2(N)+VSSMC2(N)+VJMC2(N)
     2	       +VSMC2(N-1)+VSSMC2(N-1)+VJMC2(N-1))
C	write(23,*)'vsm2,vsmc2',n,vsm2(n),vsmc2(n),corrf
	ENDDO
	PRINT*,'TDS RMS SHOT + JOHNSON',SQRT(SIGSQ)
C
c	RATIO = 1.3
c	DO N = NF+1,50
c	  FLIST(N) = RATIO*FLIST(N-1)
c	  CALL PREAMP(FLIST(N),GMCPI,GMPI,PHASE,ZIN,VN2P)
c	  CALL TDSGAIN(FLIST(N),GTDSC,GTDS,PHASE,VN2T)
c	  CORRF = (GTDS*GMPI)**2
c	  VJMC2(N) = AMAX1(PLIMIT,CORRF*VN2P)
c	  NFT = N
c	ENDDO
C	write(23,*) 'max freq in noiset ',flist(50)
C
C
C	PLOT ANTENNA SHOT NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VSMC2(N))
C	write(23,*)'vsm2,vsmc2',n,vsm2(n),vsmc2(n),xx(n),yy(n)
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = NF/2 + 1
	LABL = 15
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(15,'-ANT SHOT NOISE')
C	write(23,*) 'ant shot noise at',xx(labl),yy(labl)
C
C	PLOT SATELLITE SHOT NOISE
C
	DO N = 1,NF
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VSSMC2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = NF/2 +2
	LABL = 14
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(15,'-SAT SHOT NOISE')
C
C	PLOT RESISTOR NOISE
C
	DO N = 1,NF
C	write(23,*)'vjm2,vsm2',n,flist(n),vjm2(n),vsm2(n)
	  XX(N) = ALOG10(FLIST(N))
	  YY(N) = ALOG10(VJMC2(N))
	ENDDO
	CALL MGOCONNECT(XX,YY,NF)
	LABL = NF
	CALL MGORELOCATE(XX(LABL),YY(LABL))
	CALL MGOLABEL(14,'-JOHNSON NOISE')
C	write(23,*) 'johnson noise at',xx(labl),yy(labl)
C	
C	  CALL MGOSETEXPAND(.85)
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
C	  ELSE
C	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
C	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
C
C	PUT ON A SPECTRUM FROM CASSINI
C
	OPEN(UNIT=59,FILE='[KELLOGG.CASSINI.PAPERS]TSPECTAV.DAT;329'
     1		,STATUS='OLD',READONLY)
C
	READ(59,*) NUMBER
	PRINT*,'NUMBER OF SPECTRA AVERAGED',NUMBER
	NC = 0
  200   READ(59,*,END=210) T1,T2,T3,T4,T5,T6
	IF(T1.EQ.0.) GO TO 200
 	NC = NC+1
 	CASFREQ(NC) = T1
	CALL ANTENNAL(T1,EFFLENC,EFFLEN)
	CASSIG(NC) = T3*EFFLEN**2     ! MULTIPLY BY EFFECTIVE LENGTH SQUARED
 	GO TO 200
  210	PRINT*,NC,' CASSINI DATA POINTS' 
	CLOSE(UNIT=59)
	SIGSQCAS = 0.
	DO N = 1,NC
	  CALL PREAMP(CASFREQ(N),GMCPI,GMPI,PHASE,ZIN,VN2P)
	  CALL LRSGAIN(CASFREQ(N),GLRSC,GLRS,PHASE,VN2L)
C	  CALL PREAMP(CASFREQ(N),VM,VAPM,GM,GAPM,GMPI,GAPMPI)
	  XX(N) = ALOG10(CASFREQ(N))
	  YY(N) = ALOG10((GLRS*GMPI)**2*CASSIG(N))
C	  ASSUME UNIFORM FREQUENCY SPACING AT CASFREQ(1)
	  SIGSQCAS = SIGSQCAS + CASFREQ(1)*(GLRS*GMPI)**2*CASSIG(N)
C	  write(23,*) n,casfreq(n),cassig(n),sigsq
	ENDDO
  	PRINT*,' CASSINI, RMS V= ',SQRT(SIGSQCAS),' VOLTS' 
C
	CALL MGOCONNECT(XX,YY,NC)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(9,'FREQ (Hz)')
	CALL MGOYLABEL(18,'TDS OUTPUT V\U2/Hz')
	CALL MGOPLOTID('[.STEREO]NOISET','TDS')
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  READ(5,1023) DISPOSE
 1023	  FORMAT(A1)
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END