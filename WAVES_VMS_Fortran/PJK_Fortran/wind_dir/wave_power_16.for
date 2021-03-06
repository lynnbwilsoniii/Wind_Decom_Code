	PROGRAM WAVE_POWER
C
C	MAKES A PLOT OF WAVE POWER AND FREQ, OBTAINED FROM FIT_WAVE AND
C		TDSEXEY
C
	CHARACTER*120 	JUNK
	REAL	ANGMAG,SUNCLOCK,PWR1(50),PWR2(50),F1P(50),F2P(50),TMS(50)
	REAL    EMAG1(50),EMAG2(50)
	CHARACTER*3 MONTH(12)
	DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1    'SEP','OCT','NOV','DEC'/
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	DATA ITERM /-1/
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500., 850., 2300., 2650.)
	ELSE
	  CALL MGOSETLOC(200.,870.,80.,750.)
	ENDIF
C
C	  CALL MGOSETEXPAND(.85)
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
C	  ELSE
C	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
C	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)

C
C	ISUBR = 1		! 1 IS 8 VARIABLE FIT, LONGITUDINAL WAVES
C				  2 IS 12 VARIABLE FIT, WITH CIRCULAR POL.
C
	READ(5,*,ERR=10) ISUBR,N1ST,NLAST
	PRINT*,'READIN ISUBR,N1ST,NLAST=',ISUBR,N1ST,NLAST
	GO TO 20
 10	PRINT*,'ERROR IN WAVES_DIRECTIONS READIN'
	ISUBR = 1		! DEFAULT
C
 20	OPEN(UNIT=44,FILE='FITWAVE.RESULTS',STATUS='OLD',READONLY)
	OPEN(UNIT=45,FILE='FITWAVE12.RESULTS',STATUS='OLD',READONLY)
	READ(44,22) JUNK
	READ(44,22) JUNK
	READ(45,22) JUNK
	READ(45,22) JUNK
 22	FORMAT(A)
	N = 0
	NP = 0
	PMAX = 0.
	EMAX = 0.
	FMAX = 0.
	FMIN = 100.
 100	CONTINUE
	N = N+1
	IF(ISUBR.EQ.1) THEN
	    READ(44,*,END=200,ERR=200)  NDATE,NEVENT,F1,DELF,EX1,EY1,
     1		EX2,EY2,PH1,PH2,RMS,N1,N2,ANGMAG,SUNCLOCK
 144	    FORMAT(2I10,F7.3,F7.3,4F6.2,2F8.2,F9.4)
 	    print*,'sunclock,angmag',sunclock,angmag
	    IF(N.LT.N1ST) GO TO 100
	    IF(N.GT.NLAST) GO TO 200
 150        print*,'after read',n,ndate,nevent,rms
	    NP = NP+1
	    TMS(NP) = .5*(N1+N2)/120.
 	    PWR1(NP) = EX1**2 + EY1**2
	    EMAG1(NP) = SQRT(EX1**2 + EY1**2)
	    PWR2(NP) = EX2**2 + EY2**2
	    EMAG2(NP) = SQRT(EX2**2 + EY2**2)
	    EMAX = AMAX1(EMAG1(NP),EMAG2(NP),EMAX)
	    PMAX = AMAX1(PMAX,PWR1(NP),PWR2(NP))
	    IF(DELF.GT.0.) THEN
		F1P(NP) = F1
		F2P(NP) = F1 + DELF
	    ELSE
		F2P(NP) = F1
		F1P(NP) = F1 + DELF
		TEMP = EMAG2(NP)
		EMAG2(NP) = EMAG1(NP)
		EMAG1(NP) = TEMP
		TEMP = PWR2(NP)
		PWR2(NP) = PWR1(NP)
		PWR1(NP) = TEMP
	    ENDIF
	    FMAX = AMAX1(FMAX,F1P(NP),F2P(NP))
	    FMIN = AMIN1(FMIN,F1P(NP),F2P(NP))
	    GO TO 100
	ELSE
	    READ(45,*,END=200,ERR=200)  NDATE,NEVENT,F1,DELF,EX1,EY1,
     1		EX2,EY2,TX1,TY1,TX2,TY2,RMS,N1,N2,ANGMAG,SUNCLOCK
	    IF(N.LT.N1ST) GO TO 100
	    IF(N.GT.NLAST) GO TO 200
 160	    CONTINUE
	    IF(NP.EQ.0) PRINT*,'1ST LINE,F1,RMS,N1,N2',F1,RMS,N1,N2
	    NP = NP+1
	    TMS(NP) = .5*(N1+N2)/120.
 	    PWR1(NP) = EX1**2 + EY1**2 + TX1**2 + TY1**2
	    EMAG1(NP) = SQRT(PWR1(NP))
	    PWR2(NP) = EX2**2 + EY2**2 + TX2**2 + TY2**2
	    EMAG2(NP) = SQRT(PWR2(NP))
	    EMAX = AMAX1(EMAG1(NP),EMAG2(NP),EMAX)
	    PMAX = AMAX1(PMAX,PWR1(NP),PWR2(NP))
	    IF(DELF.GT.0.) THEN
		F1P(NP) = F1
		F2P(NP) = F1 + DELF
	    ELSE
		F2P(NP) = F1
		F1P(NP) = F1 + DELF
		TEMP = EMAG2(NP)
		EMAG2(NP) = EMAG1(NP)
		EMAG1(NP) = TEMP
		TEMP = PWR2(NP)
		PWR2(NP) = PWR1(NP)
		PWR1(NP) = TEMP
	    ENDIF
 145	    FORMAT(2I10,F7.3,F7.3,4F6.2,2F8.2,F9.4)
	    FMAX = AMAX1(FMAX,F1P(NP),F2P(NP))
	    FMIN = AMIN1(FMIN,F1P(NP),F2P(NP))
	    WRITE(43,*) NP,TMS(NP),PWR1(NP),PWR2(NP),F1P(NP),F2P(NP)
	    GO TO 100
	ENDIF
C
 200	CONTINUE
C
C		START GRAPH	
C
	PRINT*,'LAST LINE,F1,RMS,N1,N2',F1,RMS,N1,N2
C
	TRANGE = TMS(NP) - TMS(1)
	TSTART = TMS(1) - .05*TRANGE
	TEND = TMS(NP) + .05*TRANGE
	FRANGE = FMAX - FMIN
C	PRANGE = PMAX 
	PRANGE = EMAX 
	CALL MGOWINDOW(1,2,1)
C	CALL MGOSETLIM(TSTART,0.,TEND,PMAX+.1*PRANGE)
	CALL MGOSETLIM(TSTART,0.,TEND,EMAX+.1*PRANGE)
C	CALL MGOCONNECT(TMS,PWR1,NP)
C	CALL MGOCONNECT(TMS,PWR2,NP)
	CALL MGOSETLWEIGHT(3)
	CALL MGOCONNECT(TMS,EMAG1,NP)
	CALL MGOSETLWEIGHT(1)
	CALL MGOSETLTYPE(2)
	CALL MGOCONNECT(TMS,EMAG2,NP)
	CALL MGOSETLTYPE(0)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(4,'msec')
	CALL MGOYLABEL(14,'signal (mV/m)')
C
	CALL MGOWINDOW(1,2,2)
	CALL MGOSETLIM(TSTART,FMIN-.1*FRANGE,TEND,FMAX+.1*FRANGE)
	CALL MGOSETLWEIGHT(3)
	CALL MGOCONNECT(TMS,F1P,NP)
	CALL MGOSETLWEIGHT(1)
	CALL MGOSETLTYPE(2)
	CALL MGOCONNECT(TMS,F2P,NP)
	CALL MGOSETLTYPE(0)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(4,'msec')
	CALL MGOYLABEL(9,'freq, kHz')
C
	    CALL MGORELOCATE(TSTART,FMAX + .3*FRANGE)
	    NYEAR = NDATE
	    NDAY = MOD(NDATE,100)
	    NYEAR = NYEAR/100
	    NM = MOD(NYEAR,100)
	    NYEAR = NYEAR/100	    
	    WRITE(JUNK,235) NYEAR,MONTH(NM),NDAY
 235	    FORMAT(I5,'-',A3,'-',I2)
	    CALL MGOPUTLABEL(12,JUNK,6)
	    WRITE(JUNK,234) NEVENT
 234	    FORMAT(I10) 	
	    CALL MGOPUTLABEL(11,'  EVENT NO.',6)
	    CALL MGOPUTLABEL(10,JUNK,6)
	    WRITE(JUNK,238) NP
 238	    FORMAT(I6,' SECTIONS') 	
	    CALL MGOPUTLABEL(15,JUNK,6)
c	    CALL MGORELOCATE(TSTART,1.15*FMAX)
c	    CALL MGOSETEXPAND(.8)
c	    WRITE(JUNK,236) N1,N2
c 236	    FORMAT('SAMPLES',I5,' TO',I5)
c	    CALL MGOPUTLABEL(20,JUNK,6)
c	    WRITE(JUNK,237) RMS
c 237	    FORMAT(F6.2) 	
c	    CALL MGOPUTLABEL(6,'   RMS',6)
c	    CALL MGOPUTLABEL(6,JUNK,6)
c	    CALL MGOPUTLABEL(5,' mV/m',6)
	    CALL MGOSETEXPAND(1.)
C
C
C
	 PRINT*,'IN READ',NEVENT,F1,DELF,EX1
	 PRINT*,'IN READ',EX2,EY1,EY2,RMS
	 CALL MGOSETEXPAND(.5)
	 CALL MGOPLOTID(' ',' ')
	 CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	CLOSE(UNIT=44)
	CLOSE(UNIT=45)
	PAUSE
C
	STOP
 300	PRINT*,'READ ERROR AT N,NDATE,NEVENT =',N,NDATE,NEVENT
	STOP
	END
