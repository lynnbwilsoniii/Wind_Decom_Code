	PROGRAM rad2
C
C	PLOTS rad2 when fixed tuned, for radar periods, etc.  
C
C	SOME CHANGES ARE MARKED C 4 MAR OR !4 MAR
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok,ok2
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*80	stream
	character*4	event
	character*4	pa(6,4)
	parameter	size=2048
	integer*4	return_size,FLIST_SIZE,rad2_size
	integer*4	rad2_spectrum(512)
	integer*4	temp_waves
	integer*4	count(512),NUMSAMP
	character*32	s
	character*5	sumsep(2)
	integer*4	s_scet(2)
	real*8		scet,scetrad2,scetsrt,SSCET(512),ZSCET(512)
	real*8		scetend,timelist(512),smsec,zmsec,tdiff
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday
	integer*4	hhL,mmL,ssL
	integer*4	NREM,NHRMNSS,IHRMN,yyyy,mon,dd,hh,mm,ss
	integer*4	channel(1024),SUNCLOCK,status,separate
	integer*4	status_now
	real 		signal(5000),WATT_S(512),volt6_2(512)
	real 		pwrplot(4096,2),freqs(512)
	REAL		BINSIG(361,8),BINCOUNT(361,8)
c	REAL 		one_spectrum(256,1500),PTIME(256,1500)
c	REAL 		one_list(16,24000),LISTIME(16,24000)
	REAL		S_DB(512),Z_DB(512),SANGLE(512),ZANGLE(512)
	REAL		FREQ_LIST(512)
c	EQUIVALENCE (ONE_LIST(1,1),ONE_SPECTRUM(1,1))
c	EQUIVALENCE (PTIME(1,1),LISTIME(1,1))
!
	common /nrblk/ nrem,NHRMNSS

C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CH,TMCH
	COMMON /PLTBLK/ COUNT,PTIME
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
	DATA TWOPI /6.2831853/
	DATA NPLOTS /0/
	DATA STATUS /0/
	DATA SUMSEP /'  SUM','  SEP'/
	DATA BINSIG  /2888*0./
	DATA BINCOUNT /2888*0./
C
	ITERM = -1
C	ITERM = 3
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = ' RAD 2'
	PTITLE(3) = 'RCVR'
	PTITLE(4) = 'EVENT NO.'
	PRINT*,' '
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
C
C	Rad2 in linear sweep mode has 256 frequencies from 1.075
C	to 13.825 Mhz, spacing 50 kHz.  On 1995 sep 10, spectra were
C	separated by 16 sec (high bit rate?), so 5400 spectra per day
C	In linear sweep, one sample is made at each frequency, lowest
C	to highest
C
C	In list mode, the number of frequencies, etc. can be larger
C	than 256.  288, 384 are numbers that I have found.  Consecutive
C	samples are at different frequencies, unlike RAD1
C
C	Frequency in MHz is given by 1.075 + (channel_number)*.05
C
C	GET STARTED
C
	     NPT = 0
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
C
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min,sec to start, e.g. 041200'
	  read(5,3,err=10) iq, NHRMNSS,LASTH
	  type*,NHRMNSS,LASTH
	  HH = NHRMNSS/10000
	  MM = MOD(NHRMNSS,10000)
	  SS = MOD(NHRMNSS,100)
	  MM = MM/100
C
	  HHL = LASTH/10000
	  MML = MOD(LASTH,10000)
	  SSL = MOD(LASTH,100)
	  MML = MML/100
c		nrem = 225 for one hour of data
	  write(6,4)
	  read(5,3,err=10,end=220) iq, NREM
	  type*,nrem
	endif
c
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,2i10)
c
	ok = w_channel_open(TMCH,stream)
	if (.not. ok) stop 'Cannot open channel'
	scet = 0.
	call w_channel_position(TMCH,scet)
	print*,'file starts at scet',scet
	dd = scet
	call w_ur8_to_ydoy(scet,yyyy,idoy,msec)
	scetsrt = float(dd) + hh/24. + mm/1440. + ss/86400.
	scetEND = float(dd) + hhl/24. + mml/1440. + ssl/86400.
	print*,'check end',scetend,hhl,mml,ssl
	print*,'set channel position to',scetsrt
	call w_channel_position(TMCH,scetsrt)
	print*,'channel position set to',scetsrt
C
 20	CONTINUE
C
       ok = w_event(TMCH,'rad2')
	if(ok.eq.82) then
		stop 'end of file on CH'
	endif
	if (.not. ok) stop 'cannot get event'
	item = 'EVENT_SCET_R8'
	  ok = w_item_r8(TMCH, item, scet, 1, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(TMCH, item, s_scet, 2, return_size)
	print*,'initial CH time',s_scet
c	  scetfill = s_scet(2)
c
	ok = w_channel_filename(TMCH,file)
	print*,file
        item = 'EVENT_STATE'
	ok = w_item_i4(TMCH, item, status, 1, return_size)
	TYPE*,'STATUS, 0=ERR0R, 1=FIX TUNE, 2=LINEAR SWEEP, 3=LIST :',STATUS
        item = 'FREQUENCIES_HZ_R4'
	ok = w_item_r4(TMCH, item, freqs, 512, return_size)
	TYPE*,'SIZE,FIRST 4 FREQS',RETURN_SIZE,(freqs(i),i=1,4)
	item = 'CHANNEL_NUMBERS'
	ok = w_item_I4(TMCH,item,channel,1024,FLIST_SIZE)
	TYPE*,'CHANNEL_NUMBERS, RETURN SIZE',FLIST_SIZE
	TYPE*,'first 8 ch nos.',(channel(J),J=1,8)
c
	get_tm_stream = 1
C
	IF(STATUS.EQ.2.OR.STATUS.EQ.0) GO TO 20	
C
	DO N = 2,FLIST_SIZE
	  NCHANNS = N-1
	  IF(CHANNEL(N).EQ.CHANNEL(1)) GO TO 40
	ENDDO
 40	CONTINUE
	NOBS = NREM*FLIST_SIZE/NCHANNS
	NAVR = NOBS/1000 + 1
	PRINT*,'NCHANNS,NAVR',NCHANNS,NAVR
	GO TO 100
C
 110    continue
C
	! this is the main program loop
C
C	GET NEXT EVENT
C
C	type*,'going to get next event'

	event = 'rad2'

	ok = w_event(TMCH,event)
	   if(ok.eq.82) then
c		IF(STATUS.EQ.3) CALL SHPLOT
		stop 'end of file on CH'
	   endif
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	   end if
C
 100	   CONTINUE
	     item = 'EVENT_STATE'
	     ok = w_item_i4(TMCH, item, status_now, 1, return_size)
	     item = 'SUM_FLAG'
	     ok = w_item_i4(TMCH, item, separate, 1, return_size)
	     item = 'EVENT_SCET'
	     ok = w_item_i4(TMCH, item, s_scet, 2, return_size)
	     ss = mod(s_scet(2),100)
C	     type*,'s_scet',s_scet
		mm = s_scet(2)/100
		mm = mod(mm,100)
		hh = s_scet(2)/10000
		scett = float(dd) + hh/24. + mm/1440. + ss/86400.
	   HRDAY = HH + MM/60. + SS/3600.
C
	     item = 'S_PRIME_SCET_R8'
	     ok = w_item_R8(TMCH, item, Sscet, 512, return_size)
	     item = 'S_SCET_R8'
	     ok = w_item_R8(TMCH, item, Sscet, 512, return_size)
	     dd = sscet(1)
	     smsec = (sscet(1) - dd)*86400.d03	     
	     item = 'Z_SCET_R8'
	     ok = w_item_R8(TMCH, item, Zscet, 512, return_size)
	     zmsec = (zscet(1) - dd)*86400.d03	     
	     item = 'S_DBVOLTS_R4'
	     ok = w_item_R4(TMCH, item, S_DB, 512, nsamples)
	     item = 'Z_DBVOLTS_R4'
	     ok = w_item_R4(TMCH, item, Z_DB, 512, return_size)
	     item = 'SUN_ANGLE'
	     ok = w_item_I4(TMCH, item, SUNCLOCK, 1, return_size)
 	     TYPE*,'SUN ANGLE',SUNCLOCK
	     item = 'WIND_SPIN_RATE_R4'
	     ok = w_item_R4(TMCH, item, SPINRATE, 1, return_size)
c	     type*,'ok,return_size,spinrate',ok,return_size,spinrate
	     item = 'START_OFFSET_ZS_R4'
	     ok = w_item_R4(TMCH, item, Z_OFFSET, 1, return_size)
	     item = 'R_EARTH_R4'
	     ok = w_item_R4(TMCH, item, RE, 1, return_size)
	     item = 'WIND_ORBIT_X(GSE)_R8'
	     ok = w_item_R8(TMCH, item, XKM, 1, return_size)
	     item = 'WIND_ORBIT_Y(GSE)_R8'
	     ok = w_item_R8(TMCH, item, YKM, 1, return_size)
	     item = 'WIND_ORBIT_Z(GSE)_R8'
	     ok = w_item_R8(TMCH, item, ZKM, 1, return_size)
	     XRE = 1.D03*XKM/RE
	     YRE = 1.D03*YKM/RE
C
C	BEGINNING CALCULATING SOURCE POSITION IN GSE
C
	PHI_EAXIS = -(IDOY-172)*(360./365.25)       ! PHI = 0 ON JUNE 21
	THETA_EAXIS = 23.5
	GSEAX_X = SIND(THETA_EAXIS)*COSD(PHI_EAXIS)
	GSEAX_Y = SIND(THETA_EAXIS)*SIND(PHI_EAXIS)
	GSEAX_Z = COSD(THETA_EAXIS)
	print*,'doy,phi earth axis',idoy,phi_eaxis
C
C	THIS LOADS ARRAYS FOR TPLOT
C
	IF(NAVR.EQ.0) NAVR = 1
	CALL TLOAD(NCHANNS,NSAMPLES,NAVR,CHANNEL,S_DB,Z_DB,SSCET)
C
	     WIND_ANGLE = ATAN2D(YRE,XRE)		! EARTH TO WIND
	print*,'wind_angle',wind_angle
	     EARTH_ANGLE = WIND_ANGLE + 180.           ! WIND TO SOURCE
C
	     ANGLE =  -360.*SUNCLOCK/4096. - 45.     ! ANGLE SUN TO +EX AT START
	     PRINT*,'SUN TO EX, ANGLE',ANGLE
	     DANG = SPINRATE*360./TWOPI		     ! CHANGE PER SECOND
c	     ANGLE =  EARTH_ANGLE - ANGLE  	     ! ANGLE +EX TO SOURCE
	     ANGLE =  ANGLE - EARTH_ANGLE  	     ! ANGLE +EX TO SOURCE
	     PRINT*,'XRE,YRE,ANGLE,EARTH_ANGLE',XRE,YRE,ANGLE,EARTH_ANGLE
C
C	     Change to direction from Ey
C
	     ANGLE = ANGLE - 90.
C
C	The angle from +EX to the direction the signal is coming from
C	decreases with time.  
C
	     DO N = 1,nsamples				! 4 MAR 1998
		TDIFF = SSCET(N) - SSCET(1)
		SANGLE(N) = ANGLE - DANG*86400.*TDIFF
		ZANGLE(N) = SANGLE(N) - DANG*Z_OFFSET
	     ENDDO
C
	     DO N = 1,nsamples				! 4 MAR 1998
		NPT = MIN0(NPT+1,4096)
		spwr = 10.**(.1*s_db(n))
		samp = 10.**(.05*s_db(n))
	        spwrx = spwr*cosd(sangle(n))
	        spwry = spwr*sind(sangle(n))
c	ratio to reduce scintillation
		rpwr = 10.**(.1*(s_db(n)-z_db(n)))
		ramp = 10.**(.05*(s_db(n)-z_db(n)))
	        rpwrx = rpwr*cosd(sangle(n))
	        rpwry = rpwr*sind(sangle(n))
	        rampx = ramp*cosd(sangle(n))
	        rampy = ramp*sind(sangle(n))
c	        IF(N.LE.10) PRINT*, .001*smsec,sangle(n),s_db(n),
c     1		zangle(n),z_db(n),rpwrx,rpwry
	        smsec = (sscet(n) - dd)*86400.d03	     
c	        WRITE(69,169) .001*smsec,sangle(n),s_db(n),zangle(n),
c     1			z_db(n),rpwrx,rpwry
c	        WRITE(69,169) .001*smsec,sangle(n),s_db(n),zangle(n),
c     1			z_db(n),rampx,rampy
		pwrplot(npt,1) = rampx 
C		pwrplot(npt,1) = rpwrx 
C 4 MAR		pwrplot(npt,1) = spwrx 
C 4 MAR		pwrplot(npt,1) = (s_db(n)+145.)*cosd(sangle(n)) 
		pwrplot(npt,2) = rampy
C		pwrplot(npt,2) = rpwry
C 4 MAR		pwrplot(npt,2) = spwry
C 4 MAR		pwrplot(npt,2) = (s_db(n)+145.)*sind(sangle(n)) 
c	WRITE(67,167) N,SANGLE(N),PWRPLOT(N,1),PWRPLOT(N,2),SPWR,S_DB(N)

	     ENDDO
C
 167	FORMAT(I5,F8.1,3E12.4,F8.1)
 169	format(F12.3,f8.1,e12.4,F8.1,e12.4,2e12.4)
C
C	THIS LOADS ARRAYS FOR BINPLOT
C
	CALL BINLOAD(TMCH,NCHANNS,NSAMPLES,CHANNEL,SANGLE,ZANGLE,
     1	S_DB,Z_DB,BINSIG,BINCOUNT)
C
C*****		TESTING
	     n = 1
	print 76, status_now,sumsep(separate+1),smsec,s_db(n),zmsec,z_db(n)
C	WRITE(67,77) status_now,smsec,s_db(n),zmsec,z_db(n)
 76	format(i6,a5,F12.0,e12.4,F12.0,e12.4)
 77	format(i6,F12.0,e12.4,F12.0,e12.4)
	     nrem = nrem - 1
	print*,'endtest',sscet(1),scetend,nrem
	     if(sscet(1).LT.scetend.and.nrem.gt.0) go to 110
	     ITERM = -1
c	     call plotpwr(iterm,NPT,pwrplot) 
	     ITERM = -2
	     CALL TPLOT(TMCH,ITERM,NCHANNS,CHANNEL)
	     ITERM = -1
	     CALL ANTPLOT(TMCH,ITERM,NCHANNS,CHANNEL,BINSIG,BINCOUNT)
	PRINT*,'IT SHOULD STOP HERE'
	     if(NREM.LE.0.or.scet.ge.scetend)stop
c	

	   ihrmn = 100*hh+mm
	   WRITE(PTITLE(17),1017) s_scet(1)
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1017	   FORMAT(I9)
C 1017	   FORMAT(I10.6)
c 1017	   FORMAT('doy ',I3,I5.4)
 1018	   FORMAT(I6.4,I3.2)
c	TYPE*,'GOING TO GET FREQ COV'
c	   item = 'FREQUENCY_COVERAGE_HZ_R4'
c	   ok = w_item_r4(TMch,item,FREQ_LIST,512,FLIST_SIZE)
c	   TYPE*,'FREQ_COVERAGE, RETURN SIZE',FLIST_SIZE
c	TYPE*,(FREQ_LIST(J),J=1,FLIST_SIZE)
	   item = 'S_SCET_R8'
	   ok = w_item_r8(TMch,item,timeLIST,512,FLIST_SIZE)
C	   TYPE*,'timelist, RETURN SIZE',FLIST_SIZE
C	TYPE*,(timeLIST(J),J=1,FLIST_SIZE)
c	   item = 'CHANNEL_COVERAGE'
c	   ok = w_item_I4(TMch,item,rad2_SPECTRUM,1024,FLIST_SIZE)
c	   TYPE*,'CHANNEL_COVERAGE, RETURN SIZE',FLIST_SIZE
c	TYPE*,(rad2_SPECTRUM(J),J=1,FLIST_SIZE)
c	   item = 'CHANNEL_NUMBERS'
c	   ok = w_item_I4(TMch,item,channel,1024,FLIST_SIZE)
c	   TYPE*,'CHANNEL_NUMBERS, RETURN SIZE',FLIST_SIZE
c	TYPE*,'first 4',(channel(J),J=1,FLIST_SIZE)
c	TYPE*,'first 8',(channel(J),J=1,8)
C	   item = 'POINTER_LIST'
C	   ok = w_item_I4(TMch,item,rad2_SPECTRUM,1024,FLIST_SIZE)
C	   TYPE*,'POINTER_LIST, RETURN SIZE',FLIST_SIZE
C	TYPE*,(rad2_SPECTRUM(J),J=1,FLIST_SIZE)
	   item = 'FREQUENCIES_HZ_R4'
	   ok = w_item_r4(TMch,item,FREQ_LIST,512,FLIST_SIZE)
c	   TYPE*,'FREQ_LIST, RETURN SIZE',FLIST_SIZE
c	TYPE*,(FREQ_LIST(J),J=1,FLIST_SIZE)
c	   item = 'S_WATTS_R4'
C	   ok = w_item_r4(TMch,item,watt_S,512,NUMSAMP)
c	   TYPE*,'WATT_S, RETURN SIZE',NUMSAMP
c	TYPE*,(WATT_S(J),J=1,FLIST_SIZE)
	   NPT = RETURN_SIZE
	   item = 'S_MICROVOLTS_R4'
	   ok2 = w_item_r4(TMch,item,volt6_2,512,NUMSAMP)
	   TYPE*,'MUVOLT_2, RETURN SIZE',NUMSAMP
C	   DHR = 2./3600.
c	HRDAY = HRDAY + DHR
C
C
c	write(65,1019) HRDAY,FLIST_SIZE,FREQ_LIST(1),
c     1      (SIGNAL(jj),jj=1,3)
	   if(ok) go to 200
 1019	   FORMAT(F12.4,2I6,4e12.3)
	   if( .not. ok) then
		nerror = nerror + 1
		if(nerror.lt.11) go to 110
	   else
	   	nerror = 0
	   endif
 200	   continue
c	   item = 'BAND_THIS_SPEC'
c	   ok = wind_tm_get_item(TMch, item, itemp, 1, return_size)
c	   type*,'BAND_THIS_SPEC',itemp
C 	   item = 'EVENT_BAND'
C	   ok = wind_tm_get_item(TMch, item, itemp, 1, return_size)
c	   type*,'EVENT_BAND',itemp
C
C	SEPARATE SPECTRA
C
	  DO N = 1,nsamples
	    NFR = CHANNEL(N)+1
	    NSPCTR = COUNT(NFR) + 1
	    IF(NSPCTR.GE.NREM) THEN
c		the 2nd arg. is freq no., 0 = 1.075 MHz, 255 = 
C		CALL TPLOT(ITERM,0)
c		CALL TPLOT(ITERM,8)
C		CALL TPLOT(ITERM,248)
C		CALL TPLOT(ITERM,176)
		STOP
	    ENDIF
C	    ONE_SPECTRUM(NFR,NSPCTR) = WATT_S(N)
c	    ONE_SPECTRUM(NFR,NSPCTR) = VOLT6_2(N)
c	    PTIME(NFR,NSPCTR) = TIMELIST(N) - DD
	    COUNT(NFR) = NSPCTR
	  ENDDO
	  GO TO 110
C	if(nspctr.gt.0) stop
C	    if(1) stop
C
 1004	   FORMAT(F7.2,' MHz')
 1008	   FORMAT(F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

C
 220	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	
C	IF(NPLOTS.LT.1) CALL TPLOT(TMCH,-2,1)
	NPLOTS=NPLOTS+1
	IF(NPLOTS.LT.NREM) GO TO 110
	ITERM = 2
	CALL TPLOT(TMCH,ITERM,NCHANNS,CHANNEL)
	PRINT*,'REACHED SECOND SHPLOT'
c	IF(STATUS.EQ.3) CALL SHPLOT
	STOP
	END
	SUBROUTINE PLOTPWROLD(ITERM,N,PWRPLOT)
C
	REAL		PWRPLOT(4096,2)
	CHARACTER*50 	COM(20)
	CHARACTER*32 	S
	INTEGER*4 S_SCET(2)
	CHARACTER*4 EVENT
	CHARACTER*12 PTITLE(20)
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
C
	IF(ITERM.LT.0) GO TO 123
C
C	PLOT ON SCREEN
C

	PRINT*,'PWRPLOT CALLED, SCREEN'
	PWRMAX = -1000.
	DO NN = 1,N
	  PWRMAX = AMAX1(PWRMAX,PWRPLOT(NN,1))
	  PWRMAX = AMAX1(PWRMAX,PWRPLOT(NN,2))
	ENDDO
	PWRMAX = 1.2*PWRMAX
	PRINT*,'PWRMAX',PWRMAX
C
	NC = 1
	COM(NC) = 'TERMINAL 3'
	NC = NC+1
	COM(NC) = 'ERASE'
	NC = NC+1
	COM(NC) = 'LOCATION 100. 750. 100. 750.'
	NC = NC+1
	WRITE(COM(NC),100) N
	NC = NC+1
	COM(NC) = 'XCOLUMN 1'
	NC = NC+1
	COM(NC) = 'YCOLUMN 2'
	NC = NC+1
	WRITE(COM(NC),1101) -PWRMAX,PWRMAX,-PWRMAX,PWRMAX
 1101	FORMAT('LIMITS',4F8.1)
C	COM(NC) = 'LIMITS'
	NC = NC+1
	COM(NC) = 'CONNECT'
	NC = NC+1
	COM(NC) = 'BOX'
	NC = NC+1
	COM(NC) = 'END'
	CALL MONGO(NC,COM,N,2,PWRPLOT)
 100	FORMAT('LINES 1',I5)
C
 123	CONTINUE
C
C	HARDCOPY
C
	PRINT*,'PWRPLOT CALLED, PRINTER'
	PWRMAX = -1000.
	DO NN = 1,N
	  PWRMAX = AMAX1(PWRMAX,PWRPLOT(NN,1))
	  PWRMAX = AMAX1(PWRMAX,PWRPLOT(NN,2))
	ENDDO
	PWRMAX = 1.2*PWRMAX
	PRINT*,'PWRMAX',PWRMAX
	NC = 1
	COM(NC) = 'PRINTER 2'
	NC = NC+1
	COM(NC) = 'ERASE'
	NC = NC+1
	COM(NC) = 'LOCATION 700. 2500. 400. 2200.'
	NC = NC+1
	WRITE(COM(NC),100) N
	print*,'com',nc,com(nc)
	NC = NC+1
	COM(NC) = 'XCOLUMN 1'
	NC = NC+1
	COM(NC) = 'YCOLUMN 2'
	NC = NC+1
	WRITE(COM(NC),1101) -PWRMAX,PWRMAX,-PWRMAX,PWRMAX
C	COM(NC) = 'LIMITS'
	NC = NC+1
	COM(NC) = 'CONNECT'
	NC = NC+1
	COM(NC) = 'BOX'
C        write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	print*,s
	NC = NC+1
	WRITE(COM(NC),1102) S_SCET
 1102	FORMAT('XLABEL ',I8.8,I8.6)
c	COM(NC) = /'XLABEL  '//s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
c     1	s(9:10)//':'//s(11:12)//':'//s(13:14)
	NC = NC+1
	COM(NC) = 'ID'
	NC = NC+1
	COM(NC) = 'HARDCOPY'
	NC = NC+1
	COM(NC) = 'END'
	CALL MONGO(NC,COM,N,2,PWRPLOT)
	RETURN
	END
	SUBROUTINE PLOTPWR(ITERM,N,PWRPLOT)
C
	REAL		PWRPLOT(4096,2),XX(4096),YY(4096)
	CHARACTER*50 	COM(20)
	CHARACTER*32 	S
	INTEGER*4 S_SCET(2)
	CHARACTER*4 EVENT
	CHARACTER*12 PTITLE(20)
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
C
C	COMMON /MONGOPAR/
C     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
C     1  GX,GY,CX,CY,
C     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
C     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
C     1  TERMOUT,XYSWAPPED,NUMDEV,
C     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
C	EQUIVALENCE (XX(1),PWRPLOT(1,1))
C	EQUIVALENCE (YY(1),PWRPLOT(1,2))
C
	PRINT*,'PWRPLOT2 CALLED, PRINTER'
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC( 500., 700., 2300., 2500.)
	ENDIF
C
	PWRMAX = -1000.
	DO NN = 1,N
	  PWRMAX = AMAX1(PWRMAX,PWRPLOT(NN,1))
	  PWRMAX = AMAX1(PWRMAX,PWRPLOT(NN,2))
	  XX(NN) = PWRPLOT(NN,1)
	  YY(NN) = PWRPLOT(NN,2)
	ENDDO
	PWRMAX = 1.2*ABS(PWRMAX)
	PRINT*,'PWRMAX',PWRMAX
	CALL MGOSETLIM( -PWRMAX, -PWRMAX, PWRMAX, PWRMAX)
	CALL MGOCONNECT(XX,YY,N)
	CALL MGOBOX(1,2)
C
C        write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	print*,s
	NC = 1
	WRITE(COM(NC),1102) S_SCET
 1102	FORMAT(I8.8,I8.6)
c	COM(NC) = /'XLABEL  '//s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
c     1	s(9:10)//':'//s(11:12)//':'//s(13:14)

	CALL MGOXLABEL(16,COM(NC))
	CALL MGOPLOTID('RAD2FIXED',' ')
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END
	SUBROUTINE TLOAD(NCHANNS,NSAMPLES,NAVR,CHANNEL,S_DB,Z_DB,SSCET)
C
	REAL		S_DB(512),Z_DB(512)
	REAL		PLOTDATA(1000,8),PKDATA(1000,8),PLOTTIME(1000,8)
	REAL		SSUM(8),ZSUM(8),TSUM(8),SPK(8),ZPK(8)
	INTEGER*4	CHANNEL(512),COUNT(8),NPTS(8),DD
	REAL*8		SSCET(512)
C
	COMMON /PLOTBLK/ NPTS,PLOTDATA,PKDATA,PLOTTIME
C
	DATA SSUM /8*0./
	DATA ZSUM /8*0./
	DATA TSUM /8*0./
	DATA SPK /8*-1000./
	DATA ZPK /8*-1000./
	DATA COUNT /8*0/
	DATA NPTS /8*0/
	DATA NCALL /0/
C
C	THIS SUBROUTINE LOADS ARRAYS FOR TPLOT
C
	IF(NCALL.EQ.0) THEN
	  NCALL = NCALL + 1
	  DD = SSCET(1)
	ENDIF
C
	DO N = 1,NCHANNS
c	print*,'timecheck,sscet,dd',sscet(1),dd,dble(dd)
 	  DO I = 1,nsamples
	    IF(CHANNEL(I).EQ.CHANNEL(N)) THEN
	      SSUM(N) = SSUM(N) + S_DB(I)
	      ZSUM(N) = ZSUM(N) + Z_DB(I)
	      TSUM(N) = TSUM(N) + (SSCET(I)-DBLE(DD))
	      COUNT(N) = COUNT(N) + 1
	    ENDIF
	    IF(COUNT(N).GE.NAVR) THEN
	      NN = NPTS(2*N-1) + 1
	      PLOTDATA(NN,2*N-1) = SSUM(N)/COUNT(N)
	      PLOTTIME(NN,2*N-1) = 24.*TSUM(N)/COUNT(N)
	      NPTS(2*N-1) = NN
	      NN = NPTS(2*N) + 1
	      PLOTDATA(NN,2*N) = ZSUM(N)/COUNT(N)
	      PLOTTIME(NN,2*N) = 24.*TSUM(N)/COUNT(N)
	      NPTS(2*N) = NN
c	print*,'time',nn,n,plottime(nn,2*n),tsum(n)
	      SSUM(N) = 0.
	      ZSUM(N) = 0.
	      TSUM(N) = 0.
	      COUNT(N) = 0
	    ENDIF
	  ENDDO
	ENDDO
	RETURN
	END
	SUBROUTINE TPLOT(TMCH,ITERM,NCHANNS,CHANNEL)
C
C	PLOT 4 WINDOWS ON EACH PAGE, FREQ CHANNELS STARTING WITH NCHANS
C	THE FIRST, LOWEST, WINDOW IS S ANTENNA, THE SECOND IS Z ANTENNA
C	BOTH FOR THE LOWEST FREQUENCY.  3RD WINDOW IS S ANTENNA, 2ND FREQ.
C	ETC.
C
C	Frequency in MHz is given by 1.075 + (channel_number)*.05
C
	REAL		PLOTDATA(1000,8),PKDATA(1000,8),PLOTTIME(1000,8)
	INTEGER*4	TMCH,CHANNEL(512),NPTS(8)
C
	COMMON /PLOTBLK/ NPTS,PLOTDATA,PKDATA,PLOTTIME
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*32 S,SCET
	CHARACTER*4 EVENT
	INTEGER*4 NDATA(1024),SCETI4(2),IDOY
	COMMON /HEADBL/ PTITLE,EVENT,SCETI4,IDOY
	CHARACTER*12 PTITLE(20)
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
	DIMENSION YY(1000),PP(1000)
C
C
	NN = NPTS(1)
	PRINT*,'IN TPLOT,TSTART,TEND',PLOTTIME(1,1),PLOTTIME(NN,1)
	PRINT*,'IN TPLOT,NPTS',NPTS
C
	NWX = 1
	NWY = 4
	NPAGES = (NCHANNS+1)/2
	IF(NPAGES.EQ.0) THEN
	  NPAGES = 1
	  NWY = 2
	ENDIF
	DO IPAGE = 1,NPAGES
	  PRINT*,'PAGE,ITERM',IPAGE,ITERM
    	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
 1003	  FORMAT(I9)
	  WRITE(STR,1003) SCETI4(1)
	  CALL MGOGRELOCATE(GX1,GY1-.08*(GY2-GY1))
	  CALL MGOLABEL(9,STR)
	  DO IWY = 1,NWY
	    INDEX = (IPAGE-1)*4 + IWY
	    CALL MGOWINDOW(1,NWY,IWY)
	    NN = NPTS(INDEX)
	    IF(NN.EQ.0) GO TO 1002 
	    YMAX = -1000.
	    YMIN = 1000.
	    TSTART = PLOTTIME(1,INDEX)
	    TEND = PLOTTIME(NN,INDEX)
	print*,'tstart,tend',tstart,tend
	    DO N = 1,NN
	      PP(N) = PLOTTIME(N,INDEX)
	      YY(N) = PLOTDATA(N,INDEX)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOSETLIM(TSTART,YMIN,TEND,YMAX)
	    CALL MGOCONNECT(PP,YY,NN)
	    CALL MGOBOX(1,2)
	    ICH = (IWY+1)/2 + 2*(IPAGE-1)
	    FREQKHZ = 1075. + 50.*CHANNEL(ICH)
	    IRX = IWY.AND.1
	    IF(IRX.EQ.0) THEN
	      WRITE(STR,1000) FREQKHZ
 1000	      FORMAT('Z_DB ',F6.0)
	    ELSE
	      WRITE(STR,1001) FREQKHZ
 1001	      FORMAT('S_DB ',F6.0)
	    ENDIF
 1002	    CALL MGOYLABEL(11,STR)
	    IF(IWY.EQ.1) CALL MGOXLABEL(16 ,'HOURS OF THE DAY')
	    IF(IWY.EQ.NWY) CALL MGOPLOTID('[KELLOGG.WIND','RAD2FIXED')
	  ENDDO  				! END OF A WINDOW
 	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	ENDDO		! end of a page
	RETURN
	END
	SUBROUTINE BINLOAD(TMCH,NCHANNS,NSAMPLES,CHANNEL,SANGLE,ZANGLE,
     1	S_DB,Z_DB,BINSIG,BINCOUNT)
C
C	LOADS ANGLE BINS, FOR AVERAGING AND PLOTTING BY ANTPLOT
C
	REAL		S_DB(512),Z_DB(512)
	REAL		BINSIG(361,8),BINCOUNT(361,8)
	REAL		SANGLE(1),ZANGLE(1)
	INTEGER*4	CHANNEL(1),DD
C	REAL*8		SSCET(512)
C
	DATA NCALL /0/
C
	IF(NCALL.EQ.0) THEN
	  NCALL = NCALL + 1
C	  DD = SSCET(1)
	ENDIF
C
c	print*,'binload called,ncall=',ncall
	DO N = 1,NCHANNS
 	  DO I = 1,nsamples
c	print*,'channel check',n,nchanns,channel(i),channel(n),index
	    IF(CHANNEL(I).EQ.CHANNEL(N)) THEN
	      INDEX = 2*N-1
	      BANG = SANGLE(I) + 7200.
	      BANG = AMOD(BANG,360.)
	      NBIN = BANG + 1.5
	      IF(NBIN.GT.360) NBIN=1
	      BINSIG(NBIN,INDEX) = BINSIG(NBIN,INDEX) + S_DB(I)
	      BINCOUNT(NBIN,INDEX) = BINCOUNT(NBIN,INDEX) + 1.
c
              INDEX = 2*N
	      BANG = ZANGLE(I) + 7200.
	      BANG = AMOD(BANG,360.)
	      NBIN = BANG + 1.5
	      IF(NBIN.GT.360) NBIN=1
	      BINSIG(NBIN,INDEX) = BINSIG(NBIN,INDEX) + Z_DB(I)
	      BINCOUNT(NBIN,INDEX) = BINCOUNT(NBIN,INDEX) + 1.
	    ENDIF
	  ENDDO
	ENDDO
	RETURN
	END
	SUBROUTINE ANTPLOT(TMCH,ITERM,NCHANNS,CHANNEL,BINSIG,BINCOUNT)
C
C	PLOT 4 WINDOWS ON EACH PAGE, FREQ CHANNELS STARTING WITH NCHANS
C	IN BINSIG AND BINCOUNT(*.INDEX):
C		INDEX = 1 IS S ANTENNA,  2 IS Z ANTENNA, FIRST FREQ 
C		INDEX = 3 IS S ANTENNA,  4 IS Z ANTENNA, SECOND FREQ 
C		ETC.
C	Frequency in MHz is given by 1.075 + (channel_number)*.05
C
	REAL		BINSIG(361,8),BINCOUNT(361,8)
	INTEGER*4	TMCH,CHANNEL(512),NPTS(8)
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*32 S,SCET
	CHARACTER*4 EVENT
	INTEGER*4 NDATA(1024),SCETI4(2),IDOY
	COMMON /HEADBL/ PTITLE,EVENT,SCETI4,IDOY
	CHARACTER*12 PTITLE(20)
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
	DIMENSION YY(400),AG(400)
C
	PRINT*,'IN ANTPLOT,'
C
	NWX = 1
	NWY = 4
	NPAGES = (NCHANNS+1)/2
	IF(NPAGES.EQ.0) THEN
	  NPAGES = 1
	  NWY = 2
	ENDIF
	DO IPAGE = 1,NPAGES
	  PRINT*,'PAGE,ITERM',IPAGE,ITERM
    	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
 1003	  FORMAT(I9)
	  WRITE(STR,1003) SCETI4(1)
	  CALL MGOGRELOCATE(GX1,GY1-.07*(GY2-GY1))
	  CALL MGOLABEL(9,STR)
	  DO IWY = 1,NWY
	    INDEX = (IPAGE-1)*4 + IWY
	    CALL MGOWINDOW(1,NWY,IWY)
	    YMAX = -1000.
	    YMIN = 1000.
	    NN = 0
	    ZCOUNT = 0.
	    DO N = 1,360
	      IF(BINCOUNT(N,INDEX).NE.0.) THEN
	 	NN = NN+1
	        YY(NN) = BINSIG(N,INDEX)/BINCOUNT(N,INDEX)
	        AG(NN) = N-1
	        YMAX = AMAX1(YY(NN),YMAX)
	        YMIN = AMIN1(YY(NN),YMIN)
	      ELSE
C		PRINT*,'BINCOUNT ZERO',N,INDEX,BINSIG(N,INDEX)
	        ZCOUNT = ZCOUNT+1.
	      ENDIF
	    ENDDO
	    PRINT*,'INDEX,ZERO COUNT',INDEX,ZCOUNT
	    CALL MGOSETLIM(0.,YMIN,360.,YMAX)
	    IF(NN.NE.0) CALL MGOCONNECT(AG,YY,NN)
	    CALL MGOBOX(1,2)
	    ICH = (IWY+1)/2 + 2*(IPAGE-1)
	    FREQKHZ = 1075. + 50.*CHANNEL(ICH)
	    IRX = IWY.AND.1
	    IF(IRX.EQ.0) THEN
	      WRITE(STR,1000) FREQKHZ
 1000	      FORMAT('Z_DB ',F6.0)
	    ELSE
	      WRITE(STR,1001) FREQKHZ
 1001	      FORMAT('S_DB ',F6.0)
	    ENDIF
	    CALL MGOYLABEL(11,STR)
	    IF(IWY.EQ.1) CALL MGOXLABEL(16 ,'ANGLE, ANTENNA TO EARTH')
	    IF(IWY.EQ.NWY) CALL MGOPLOTID('[KELLOGG.WIND','RAD2FIXED')
	  ENDDO  				! END OF A WINDOW
 	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	ENDDO		! end of a page
	RETURN
	END
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	common /nrblk/ nrem,NHRMN
	integer*4	iq,NREM,NHRMN

10	  write(6,6)
	  read(5,5,err=10,end=20) iq, stream

 	if (iq .lt. 1) then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else
	   ! assume the user entered the name of an offline file
	end if

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end
	SUBROUTINE SHPLOT
C
	character*32 s,SCET
	integer*4	s_scet(2),IDOY
	integer*2 npwrl
	integer*4 rad2_spectrum(1024)
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
	CHARACTER*12 PTITLE(20)
c	COMMON /PLTBLK/ COUNT,PTIME,one_spectrum
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
	CHARACTER*10    TITLE(4)
	CHARACTER*120   STR
c	REAL 		one_spectrum(256,1500),PTIME(256,1500)
	INTEGER*4	COUNT(512)
C	DIMENSION       PWRL(256000)
	DATA NSPCTR,HSTART,HEND /0,0.,1./	
C
C
	ITERM = -2		! LANDSCAPE
	XSTART = 400.
	XEND = 3000.
	IW = 1
c	NSPCTR = COUNT(1)
	PRINT*,' IN SHPLOT,NSPCTR',nspctr
c	PRINT*,' IN SHPLOT,TIMES',PTIME(1,1),PTIME(256,NSPCTR),NSPCTR
C	PRINT*,'PTIME',PTIME(1),PTIME(2),PTIME(3),PTIME(5)
C	CALCULATE BOX SIZE
	PIXSIZ = .1
c	IF(NSPCTR.GT.1) 
c     1		PIXSIZ = (PTIME(256,NSPCTR) - PTIME(1,1))/(NSPCTR-1)
c	HSTART = PTIME(1,1) - .5*PIXSIZ
c	HEND =   PTIME(256,NSPCTR) + .5*PIXSIZ
	PRINT*,' IN SHPLOT',HSTART,HEND,NSPCTR
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,280.,XEND,2300.)
	ENDIF
C	
	  YMIN = 1.E6
	  YMAX = -YMIN
	  SMIN = 1.E-18
	  SMAX = 5.E-16
	  CALL HISTOG(2,TJUNK,256,SMIN,SMAX,.01,TOTAL,RET)     ! CLEAR AND INIT
	  DO N = 1,NSPCTR
	  DO M = 1,256
	    NM = N + (M-1)*NSPCTR
c	    PWRL(NM) = ONE_SPECTRUM(M,N)
C	    CALL HISTOG(1,PWRL(NM),256,SMIN,SMAX,.5,TOTAL,RET)   !LOAD ARRAY
C
C	    YMIN = AMIN1(YMIN,PWRL(NM))
C	    YMAX = AMAX1(YMAX,PWRL(NM))
	  ENDDO
	  ENDDO
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C
	  CALL HISTOG(0,TJUNK,256,SMIN,SMAX,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE
	  CALL HISTOG(0,TJUNK,256,SMIN,SMAX,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER
C	  LOWERING YMAX MAKES THE SIGNAL DARKER

	  YAMIN = YHMIN
	  YAMAX = YHMAX
	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX
c	  arguments are: array(m,n),m,n,white,black,linearity
c	  m is the x direction
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C	  CALL MGOHALFTONE(PWRL,NSPCTR,256,YAMIN,YAMAX,1.E7)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOSETLIM(HSTART,1.05,HEND,13.1)
	    CALL MGOYLABEL(14,'FREQUENCY, MHz')	
	  CALL MGOBOX(1,2)
	  CALL MGOGRELOCATE(GX1,GY2)
	  CALL MGOPUTLABEL(10,TITLE(IW),9)
	print*,'title in shplot  ',title(iw)
C
	  WRITE(STR,704) YAMIN,YAMAX
 704	  FORMAT('WHITE,BLACK ',2E10.2,' W/M2/Hz ')
	  CALL MGOSETANGLE(90.)
	  CALL MGOSETEXPAND(.5)
	  XPR = GX2 + .005*(GX2-GX1)
	  YPR = .5*(GY1+GY2)
	  CALL MGOGRELOCATE(XPR,YPR)
	  CALL MGOPUTLABEL(38,STR,2)                  
	  CALL MGOSETANGLE(0.)
	  CALL MGOSETEXPAND(1.)
	  IF(IW.EQ.1) THEN
	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
	   scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
            WRITE(STR,1001) SCET(1:10),IDOY
 1001	    FORMAT('HOURS OF ',A10,' DOY ',I3)
c            WRITE(STR,1001) s_SCET(1)
c 1001	    FORMAT('HOURS OF ',i10)
	    CALL MGOSETEXPAND(.8)
            CALL MGOXLABEL(28,STR)	
	    CALL MGOSETEXPAND(.5)
C	    IF(ITMCORR.EQ.0) THEN
C		CALL MGOPUTLABEL(23,'    consecutive spectra',6)
C	    ELSE
C		CALL MGOPUTLABEL(18,'    corrected time',6)
C	    ENDIF
	    CALL MGOSETEXPAND(1.)
	  ENDIF
C
	    CALL MGOSETEXPAND(.8)
	    CALL MGOPLOTID('WIND-WAVES-rad2','SHPLOT')
	    CALL MGOSETEXPAND(1.)
C
	  CALL MGOSETEXPAND(1.)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
	END
	SUBROUTINE HISTOG(ILOAD,X,NBIN,XMIN,XMAX,PCTILE,TOTAL,RET)
C
C	THIS IS A SPECIAL VERSION TO CALCULATE LEVEL AT A CERTAIN PERCENTILE
C
C	INTEGER*4 NARR
	DIMENSION NARR(256)
	DATA NARR /256*0/
C
	IF(ILOAD.GT.1) THEN
	  DO N = 1,256
	    NARR(N) = 0
	  ENDDO
	  TOTAL = 0.
	  RETURN
	ENDIF
C
	IF(ILOAD.EQ.1) THEN
	  XT = AMAX1(X,XMIN)
	  XT = AMIN1(XT,XMAX)
	  IBIN = (NBIN-1)*(XT-XMIN)/(XMAX-XMIN) + 1
	  NARR(IBIN) = NARR(IBIN)+1
	  TOTAL = TOTAL + 1.
	  RETURN
	ENDIF
C
	IF(PCTILE.GT..5) THEN
C	  PRINT OUT RESULTS
    	  PRINT*,' TOTAL NUMBER, NBIN',TOTAL,NBIN
	  PRINT*,'XMIN,XMAX',XMIN,XMAX
	  PRINT*,'HISTOGRAM'
	  PRINT 1111, (NARR(J),J=1,NBIN)
 1111	  FORMAT(10I7)
	ENDIF
C
C	NOW CALCULATE VALUE OF X CORRESPONDING TO PCTILE PERCENTILE
C
C	PCTILE = .2
	SET = PCTILE*TOTAL
	SUM = 0.
	N = 0
  100	N = N+1
	  SUM = SUM + NARR(N)
	  IF(SUM.LT.SET) GO TO 100
C
C	NOW THE DESIRED VALUE LIES IN BIN N
C
	SUMLOW = SUM - NARR(N)
	XN = N
	RETLOW = XMIN + ((XN-1.)/(NBIN-1.))*(XMAX-XMIN)
	RETHI = XMIN +       (XN/(NBIN-1.))*(XMAX-XMIN)
	RET = RETLOW + (SET-SUMLOW)*(RETHI-RETLOW)/NARR(N)
C	PRINT*,'N,SET,SUMLOW,LOW,RET',N,SET,SUMLOW,RET
	RETURN
	END
