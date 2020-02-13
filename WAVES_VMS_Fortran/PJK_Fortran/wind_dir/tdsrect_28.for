 	PROGRAM TDSREC
C
C	PLOTS TDS MAXIMA AND FREQUENCIES
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok,statok
	integer*4	i,j,k,n,itemp,EVNO
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6)
	character*80	stream
	character*4	pa(6,4),event
	parameter	size=2048
	integer*4	return_size
	integer*4	tds_channel,ifilf,ifils,ifsps,issps,isps
	integer*4	temp_waves
	integer*4	ndata(2048)
	character*32	s
	integer*4	s_scet(2)
	real		zint(1024),zcross(1024)
	real*8		scet,scetstr
	integer*4	major, minor,ihist,nhist(256,6),npos(6),neg(6)
	integer*4	dpuhi(2000),dpulo(2000),dpuclk,deldpu
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss
	real 		ffilter(4),sfilter(4),fsps(4),ssps(4),sps
!
	common /nrblk/ nrem,NHRMN
	COMMON /HEADBL/ PTITLE,PTITLE2,FILE
	COMMON /STATUS/ ifilf,ifils,ifsps,issps,ichpa,ifilf0,ifils0
	COMMON /OFFBLK/ ITCH,NHIST
C	common /headblk/ major,minor,tds_channel,s_scet,file
C
C	TO PRINT A FILE OF TIMES AND MAXIMA, UNCOMMENT APPROPRIATE
C	LINES IN PLOTMAX   FOR095.DAT IS FOR TDSF, FOR098.DAT FOR TDSS
C
	CHARACTER*12 PTITLE(20),PTITLE2(20)
	INTEGER*4 TDSCH
	COMMON /PLTBLK/ TDSHI(2000),TDSLO(2000),TDSB(2000),FRQHI(2000),
     1          FRQLO(2000),FRQB(2000),HRHI(2000),HRLO(2000),HRB(2000),
     1		SPSHI(2000),SPSLO(2000),NPTH,NPTL,NPTB,NSIM,HRSIM(200),
     1		EVNO(2000,3)
	COMMON /PLOTDT/ SPS,YYYY,DOY
	DATA TWOPI /6.2831853/
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.875/
	DATA SSPS /7500.,1875.,468.75,117.2/
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
	DATA IFILS0,IFILF0 /-1,-1/
C
	PTITLE(1) = 'SAMP.RATE'
	PTITLE(7) = 'SAMP.RATE'
	PTITLE(3) = 'kSPS'
	PTITLE(9) = '  SPS'
	PTITLE(4) = 'L.P.FIL.'
	PTITLE(10) = 'L.P.FIL.'
	PTITLE(6) = '  HZ'
	PTITLE(12) = '  HZ'
	PRINT*,' '
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
C	GET STARTED
C
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
C
C        if(stream.eq.'offline') then
C 10	  write(6,*)  'type hr,min to start, e.g. 0412'
C	  read(5,3,err=10) iq, NHRMN
C	  type*,NHRMN
C	  HH = NHRMN/100
C	  MM = MOD(NHRMN,100)
C	endif
c
  5	format(q,a)
  3	format(q,i10)
c

	ok = wind_tm_open_channel(TDSCH,stream)
	if (.not. ok) stop 'Cannot open channel'
	ok = w_channel_filename(TDSCH,file)
	print*,file
	WRITE(96,*) FILE
	write(96,*) 'LIST OF POSSIBLE SIMULTANEOUS EVENTS'
	write(96,*) 'HI evt no.   hi hrs   lo evt no. lo hrs'
     1  ,' hi t/m  lo t/m'
C
	DO 200 LOCATE = 1,3
C
	NEVENT=0
	event = 'FILL'
	if(locate.eq.2) event='TDSF'
	if(locate.eq.3) event='TDSS'
	SCET = 0.
	call w_channel_position(TDSCH,SCET)
	print*,' file starts at scet',SCET
	scetstr = scet
	dd = SCET
C	SCET = float(dd) + hh/24. + mm/1440.
C	print*,'set channel position to',SCET
C	call w_channel_position(TDSCH,SCET)
C	print*,'channel position set to',SCET

	ok = wind_tm_get_mfmf(TDSCH,major,minor)
	if (.not. ok) stop 'cannot get Mfmf stream position'
	print*,' after mfmf, at Mfmf=',major,minor
c
	get_tm_stream = 1
c
        ok = w_event(TDSCH,EVENT)
	print*,'for first event,OK=',ok
c
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
	   IF(TDS_CHANNEL.LE.2) THEN
	      ISPS = IFSPS
	      WRITE(PTITLE(2),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(5),1008) FFILTER(IFILF+1)
	   ELSE
	      ISPS = ISSPS
	      WRITE(PTITLE(8),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	   ENDIF
	   call w_ur8_to_ydoy(scet,yyyy,doy,msday)
 1012	   FORMAT(I10)
C
C	   ipa = ichpa(tds_channel)
C	   WRITE(PTITLE(7),1007) pa(tds_channel,ipa+1)
 1004	   FORMAT(F7.2)
 1008	   FORMAT(F7.0)
C
	type*,'going to get first desired event'
	go to 110
c
C
C	GET NEXT EVENT
C
 100    continue

!	 this is the main program loop

C	if(wind_tm_eof(TDSCH,major,minor)) then
C	  type*,'call pltmax at eof, just after 100'
C	  call pltmax(-2)
C	  stop
C	endif
C
C	IF(NEVENT.GT.20) GO TO 200

	ok = w_event(TDSCH,EVENT)
	NEVENT = NEVENT+1
	   if (ok.ne.1) then
	      if(ok.eq.82) go to 200
	      ok = wind_tm_get_mfmf(TDSCH,major,minor)
	      type *, char(7), '***** not OK missing packet in event ****'
c	      type *, 'Cannot get event at MF.mf: ', major, minor
C	      if (wind_tm_realtime(ch)) then
C	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
C	         type *, '-reset to latest position: ', major, minor
C	      else
C	         call wind_tm_increment_packet(major,minor)
C	         type *, '-incrementing packet...'
C	      end if
	   end if
	  ok = wind_tm_get_mfmf(TDSCH,major,minor)
	  if (.not. ok) stop 'cannot get Mfmf stream position'
C
 110	  CONTINUE


C
	ITEM = 'DATA'
	OK = WIND_TM_GET_ITEM(TDSCH,ITEM,NDATA,SIZE,RETURN_SIZE)
	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
C	PRINT*,'CHANNEL',TDS_CHANNEL
C	  PRINT*,'DATA, SIZE=',RETURN_SIZE
C	  PRINT 222, (NDATA(J),J=1,2048)
 222	FORMAT(10I6)
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(TDSCH,ITEM,SCET,1,RETURN_SIZE)
	item = 'DPU_CLOCK'
	OK = W_ITEM_I4(TDSCH,ITEM,DPUCLK,1,RETURN_SIZE)
c******		temporary fix for bad first event
	if(dd.eq.0) dd = scet
c
	   IF(TDS_CHANNEL.LE.2) THEN
	      ISPS = IFSPS
	      WRITE(PTITLE(2),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(5),1008) FFILTER(IFILF+1)
	   ELSE
	      ISPS = ISSPS
	      WRITE(PTITLE(8),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	   ENDIF
C
	DO IK = 1,2048
	  NDATA(IK) = NDATA(IK)-128
	ENDDO
C
C	FIND ZERO CROSSINGS AND MAXIMUM, AND DO HISTOGRAM
C
	IZCNT = 0
	MAXTM = IABS(NDATA(2048))
	MAXTM = MAX0(MAXTM,IABS(NDATA(1)))
C	IF(NDATA(1).EQ.0) PRINT*,'ZERO DATA',' 1',NDATA(1)
	DO IL = 2,2047
	  MAXTM = MAX0(MAXTM,IABS(NDATA(IL)))

	  IZ = IL
C	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL-1),
C     1   NDATA(IL),NDATA(IL+1)
C		COUNT ALL CROSSINGS, POS TO NEG AND NEG TO POS
C	  IF(NDATA(IL)*NDATA(IL+1).LE.0) THEN
C	        IZCNT = IZCNT+1
C		S1 = NDATA(IL)
C		S2 = NDATA(IL+1)
C		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
C	  ENDIF
C		COUNT ONLY POS TO NEG
	  IF(NDATA(IL).GT.0.AND.NDATA(IL+1).LE.0) THEN
	        IZCNT = IZCNT+1
		S1 = NDATA(IL)
		S2 = NDATA(IL+1)
		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
	  ENDIF
	ENDDO
	DO N = 1,IZCNT-1
	  ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
	  IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
	  IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
	ENDDO
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
C
C	DO HISTOGRAM
C
	IF(MAXTM.LE.51) THEN			! DO ONLY WEAK EVENTS
C	IF(MAXTM.GT.51) THEN
c
C	print*,'chann,pa,speed,maxtm',tds_channel,ichpa(tds_channel)
C     1   ,isps,maxtm
c	  INDEX = 2*TDS_CHANNEL + MIN0(ISPS,1) - 1
c	  ITCH = MIN0(INDEX,5)
	  ITCH = TDS_CHANNEL
	  IF(ITCH.LE.0) THEN
		PRINT*,'ITCH OUT OF RANGE'
		GO TO 20
	  ENDIF
	  DO N = 1,2048
	    IHIST = NDATA(N) + 128      !  IHIST = 128 IS NDATA = 0
C	    IHIST = MIN0(IHIST,100)
C	    IHIST = MAX0(IHIST,1)
	    IF(IHIST.GT.256.OR.IHIST.LE.0) THEN
		PRINT*,'N,DATA,IHIST,ITCH',N,NDATA(N),IHIST,ITCH
		IHIST = 1
	    ENDIF
	    NHIST(IHIST,ITCH) = NHIST(IHIST,ITCH) + 1
	    IF(NDATA(N).GT.0) NPOS(ITCH) = NPOS(ITCH)+1
	    IF(NDATA(N).LT.0) NEG(ITCH) = NEG(ITCH)+1
	  ENDDO
	ENDIF

 20	CONTINUE




C*********
	     item = 'EVENT_SCET'
	     ok = w_item_i4(TDSCH, item,s_scet, 2, return_size)
C	     type*,'s_scet',s_scet 
	     SS = MOD(S_SCET(2),100)
	     MM = S_SCET(2)/100
	     MM = MOD(MM,100)
	     HH = S_SCET(2)/10000
C	     SCET = DFLOAT(DD) + HH/24. + MM/1440. + SS/86400.
C	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
C	     call w_ur8_to_string(scet,PTITLE(16),PTITLE(18))
C	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
C	     call w_ur8_to_ydoy(scet,yyyy,doy,msday)
C	     ihrmn = 100*hh+mm
C	     TYPE 1005,itemp,YYYY,MON,DD,IHRMN
C		write(26,*) itemp,YYYY,MON,DD,IHRMN
c 1016	   format(I5,'/',I2,'/',I2)
C
C	print*,'loc,ch,ichpa',locate,tds_channel,ichpa(tds_channel)
	IF(LOCATE.EQ.1) THEN			!FILL
	  IF(TDS_CHANNEL.EQ.1.AND.ICHPA(1).EQ.0) THEN      ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
c            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.2.AND.ICHPA(2).EQ.0) THEN       ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
c            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.3.AND.ICHPA(3).EQ.0) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
c	TYPE*,'PT NO, TIME',NPTL,HRLO(NPTL) 
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.1) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	  ENDIF
	  IF(TDS_CHANNEL.EQ.5.AND.ICHPA(5).EQ.0) THEN       !  BY
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.0) THEN       !  BX
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	ENDIF	
C
	IF(LOCATE.EQ.2) THEN			! TDSF
	  IF(TDS_CHANNEL.EQ.1.AND.ICHPA(1).EQ.0) THEN      ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
C            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.2.AND.ICHPA(2).EQ.0) THEN       ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
 	    EVNO(NPTH,1) = ITEMP
C            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	ENDIF
C
	IF(LOCATE.EQ.3) THEN			! TDSS
	  IF(TDS_CHANNEL.EQ.3.AND.ICHPA(3).EQ.0) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
 	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.1) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	  ENDIF
	  IF(TDS_CHANNEL.EQ.5.AND.ICHPA(5).EQ.0) THEN       !  BY
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.0) THEN       !  BX
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	ENDIF	
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	if(npth.lt.1900) go to 100
	if(LOCATE.EQ.3.AND.nptL.lt.1900) go to 100
C
C	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
C
	PRINT*,NEVENT,' ',EVENT,' EVENTS FOUND'
 200	CONTINUE          ! END OF ILOCAT LOOP
C
C	LOOK FOR SIMULTANEOUS EVENTS IN HIGH, LOW, AND MAG
C
C	write(95,*) (evno(nh,1),hrhi(nh),nh=1,npth)
c	write(95,*) (evno(nl,2),hrlo(nl),nl=1,nptl)
c	write(95,*) (evno(nh,3),hrb(nh),nh=1,nptb)
	NSIM = 0
	DO NH = 1,NPTH
	  DO NL = 1,NPTL
	    DELDPU = DPULO(NL)-DPUHI(NH)
	    LOC =2048./SPSLO(NL)/320.E-6  	!DPU COUNTS, LO EVENT
	    NHC =-2048./SPSHI(NH)/320.E-3	!DPU COUNTS, HI EVENT
	    IF(DELDPU.LT.LOC.AND.DELDPU.GT.NHC) THEN
 	      IF(ABS(HRHI(NH)-HRLO(NL)).LT..001) THEN
		WRITE(96,1096) EVNO(NH,1),HRHI(NH),EVNO(NL,2),
     1		HRLO(NL),TDSHI(NH),TDSLO(NL),DELDPU
 1096		FORMAT(I10,F10.5,I12,F10.5,2F6.0,I6)
		NSIM = MIN0(NSIM+1,200)
		HRSIM(NSIM) = HRHI(NH)
	      ENDIF
	    ENDIF
	  ENDDO
	ENDDO
C
C	PLOT RESULTS
C
	type*,'call pltmax at npth limit, npth=',npth
	CALL PLTMAX(-2)
c	CALL PLTMAX(3)
	DO N = 1,6
	  WRITE(43,1043) (NHIST(J,N),J=1,256)
	ENDDO
	ITCH = 5
	CALL OFFIT(X,SUMSQ)
 1043	FORMAT(10I8)
	WRITE(43,*) FILE
	WRITE(43,*) NPOS
	WRITE(43,*) NEG
	CALL OFFCHK(X,SUMSQ)
	STOP
	END
	SUBROUTINE PLTMAX(ITERM)
C
C	PLOT TDS MAXIMA FOR 3 CHANNELS
C
	CHARACTER*12 TITLE1(20),TITLE2(20)
	CHARACTER*120 STR
	character*4	pa(6,4)
	character*80	file
	COMMON /HEADBL/ TITLE1,TITLE2,FILE
	COMMON /PLTBLK/ TDSHI(2000),TDSLO(2000),TDSB(2000),FRQHI(2000),
     1          FRQLO(2000),FRQB(2000),HRHI(2000),HRLO(2000),HRB(2000),
     1		SPSHI(2000),SPSLO(2000),NPTH,NPTL,NPTB,NSIM,HRSIM(200),
     1		EVNO(2000,3)
	COMMON /PLOTDT/ SPS,IYEAR,IDOY
	REAL FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /STATUS/ ifilf,ifils,ifsps,issps,ichpa,ifilf0,ifils0
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
	INTEGER*4 ITERM,ICHPA(6),EVNO
	DIMENSION YY(2048)
	DATA NCH /1/
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.875/
	DATA SSPS /7500.,1875.,468.75,117.2/
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
C
	PRINT*,'NPT IN PLTMAX',NPTH,NPTL,NPTB
	NPTH1 = AMAX0(NPTH,1)
	NPTL1 = AMAX0(NPTL,1)
	NPTB1 = AMAX0(NPTB,1)
	PRINT*,'TIMES BEFORE SORT',HRHI(NPTH1),HRLO(NPTL1),HRB(NPTB1)
C
	PRINT*,'FRQHI',FRQHI(1),FRQHI(2),FRQHI(3),FRQHI(NPTH1)
	PRINT*,'TDSLO',TDSLO(1),TDSLO(2),TDSLO(3),TDSLO(NPTL1)
	PRINT*,'FRQLO',FRQLO(1),FRQLO(2),FRQLO(3),FRQLO(NPTL1)
	PRINT*,'SPSHI',SPSHI(1),SPSHI(2),SPSHI(3),SPSHI(NPTH1)
	PRINT*,'SPSLO',SPSLO(1),SPSLO(2),SPSLO(3),SPSLO(NPTL1)


C
	CALL SORTN(HRHI,TDSHI,FRQHI,SPSHI,EVNO(1,1),NPTH)
	CALL SORTN(HRLO,TDSLO,FRQLO,SPSLO,EVNO(1,2),NPTL)
	CALL SORTN(HRB,TDSB,FRQB,YY,EVNO(1,3),NPTB)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PLOT TDS DATA
C
	YTOP = 2230.
	YBOT = 200.
	PRANGE = (YTOP-YBOT)/6.
	TSTART = AMIN1(HRHI(1),HRLO(1),HRB(1))
	HRHI1 = HRHI(1)
	HRLO1 = HRLO(1)
	HRB1  = HRB(1)
	TRENORM = AMIN1(HRHI1,HRLO1,HRB1)
	TEND = AMAX1(HRHI(NPTH1),HRLO(NPTL1),HRB(NPTB1))
	PRINT*,'PLTMAX CALLED, TIMES',TSTART,TEND
	NEARLYHI = 0
	NEARLYLO = 0
	NEARLYB  = 0
	DO N=1,NPTH1
	  IF(HRHI(N).LT.0.) THEN
		HRHI(N) = HRHI(N)/ABS(TRENORM)
		NEARLYHI = NEARLYHI + 1
	  ENDIF
	ENDDO
	DO N=1,NPTL1
	  IF(HRLO(N).LT.0.) THEN
		HRLO(N) = HRLO(N)/ABS(TRENORM)
		NEARLYLO = NEARLYLO + 1
	  ENDIF
	ENDDO
	DO N=1,NPTB1
	  IF(HRB(N).LT.0.) THEN
  		HRB(N) = HRB(N)/ABS(TRENORM)
		NEARLYB = NEARLYB + 1
	  ENDIF
	ENDDO

	XEND = 3000.

C
C	  START AT THE BOTTOM
C
C	  PLOT B FREQ CHANNEL
C
	  YLOW = YBOT 
	  YHI = YLOW + .9*PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ENDIF
C
	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	    ENDIF
	   WRITE(STR,1017) IYEAR,IDOY
 1017	   FORMAT(I5,' DOY ',I3)
	   CALL MGOLABEL(14,STR)
	   CALL MGOSETEXPAND(1.)
C
C
	    DO N = 1,NPTB
	      FRQB(N) = 1000.*FRQB(N)
	    ENDDO
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C  	  CALL MGOSETLIM(TSTART,0.,TEND,1300.)
  	  CALL MGOSETLIM(-1.1,0.,24.,1300.)
c	  CALL MGOGRID(0)
	  CALL MGOCONNECT(HRB,FRQB,NPTB)
	  CALL MGOSETEXPAND(.5)
	print*,'before',gx,gy
	  CALL MGOXLABEL(16,'HOURS OF THE DAY')
	print*,'after',gx,gy
	  GYS = GY
	  CALL MGORELOCATE(-1.,0.)
	  GXS = GX
	  CALL MGOGRELOCATE(GXS,GYS)
	  WRITE(STR,1014) TRENORM,NEARLYHI,NEARLYLO,NEARLYB
1014	  FORMAT('1st data',f7.2,' hours, no.evts,hi,lo,b',3i5)
	  CALL MGOLABEL(54,STR)
	  CALL MGOBOX(1,2)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOYLABEL(2,'Hz')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .12*TRANGE
C	  XTITLE = GX2 +.02*(GX2-GX1)
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2 
	  CALL MGOSETEXPAND(.5)
 1015	  FORMAT(F7.2)
 1011	  FORMAT(F6.0)
 1001	  FORMAT(I6)
	      YTITLE = YTITLE - 3.*TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(3,'  B')
	  XTITLE = 1.02*GX2
	  YTITLE = GY2 - 5.*TINC 
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(TITLE1(13),1001) NPTB
	  CALL MGOLABEL(6,TITLE1(13))
	  CALL MGOGRELOCATE(XTITLE,YTITLE-TINC)
	  CALL MGOLABEL(6,'EVENTS')
C
C	  PLOT B CHANNEL
	  YLOW = YHI
	  YHI = YLOW + PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ENDIF
	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	    ENDIF
	    CALL MGOSETEXPAND(1.)
C
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C  	  CALL MGOSETLIM(TSTART,0.,TEND,130.)
  	  CALL MGOSETLIM(-1.1,0.,24.,130.)
c	  CALL MGOGRID(0)
	  CALL MGOCONNECT(HRB,TDSB,NPTB)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOBOX(0,2)
	  CALL MGOYLABEL(7,'T/M NO.')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .12*TRANGE
C	  XTITLE = GX2 +.02*(GX2-GX1)
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2
	  CALL MGOSETEXPAND(.5)
	      YTITLE = YTITLE - 3.*TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(3,'  B')
C
C	  PLOT LO CHANNEL FREQUENCY
C
	  YLOW = YHI + .1*PRANGE
	  YHI = YLOW + PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ENDIF
C
	  DO N = 1,NPTL
	    FRQLO(N) = 1000.*FRQLO(N)
	  ENDDO

	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	    ENDIF
	    CALL MGOSETEXPAND(1.)
C
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C  	  CALL MGOSETLIM(TSTART,0.,TEND,1600.)
  	  CALL MGOSETLIM(-1.1,0.,24.,1600.)
c	  CALL MGOGRID(0)
	  CALL MGOCONNECT(HRLO,FRQLO,NPTL)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOBOX(0,2)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOYLABEL(2,'Hz')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .12*TRANGE
C	  XTITLE = GX2 +.02*(GX2-GX1)
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2
	  CALL MGOSETEXPAND(.5)
C	  WRITE(TITLE1(8),1015) SSPS(ISSPS+1)
	  WRITE(TITLE1(8),1015) SPSLO(1)
	  TITLE1(9) = '  SPS'
	  WRITE(TITLE1(11),1011) SFILTER(IFILS0+1)
	  TITLE1(12) = '  HZ'
	      YTITLE = YTITLE - 3.*TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(4,'EXDC')
	  XTITLE = 1.02*GX2
	  YTITLE = GY2 - 5.*TINC 
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(TITLE1(13),1001) NPTL
	  CALL MGOLABEL(6,TITLE1(13))
	  CALL MGOGRELOCATE(XTITLE,YTITLE-TINC)
	  CALL MGOLABEL(6,'EVENTS')
C
C	  PLOT LO CHANNEL MAXIMA
C
	  YLOW = YHI
	  YHI = YLOW + PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ENDIF
C
	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	    ENDIF
	    CALL MGOSETEXPAND(1.)
C
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C  	  CALL MGOSETLIM(TSTART,0.,TEND,130.)
  	  CALL MGOSETLIM(-1.1,0.,24.,130.)
c	  CALL MGOGRID(0)
	  CALL MGOCONNECT(HRLO,TDSLO,NPTL)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOBOX(0,2)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOYLABEL(7,'T/M NO.')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .12*TRANGE
C	  XTITLE = GX2 +.02*(GX2-GX1)
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2
	  CALL MGOSETEXPAND(.5)
	  YTITLE = YTITLE - 3.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(4,'EXDC')
	  YTITLE = YTITLE - TINC
	    DO N = 7,12
	      YTITLE = YTITLE - TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(10,TITLE1(N))
	    ENDDO
	  XTITLE = 1.02*GX2
	  YTITLE = GY2 - 4.*TINC
	  WRITE(TITLE1(11),1011) SFILTER(IFILS+1)
	  WRITE(TITLE1(8),1015) SPSLO(NPTL)
	    DO N = 7,12
	      YTITLE = YTITLE - TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(10,TITLE1(N))
	    ENDDO
	    CALL MGOSETEXPAND(1.)
C
C
C	  PLOT HI CHANNEL FREQUENCY
C
	  YLOW = YHI + .1*PRANGE
	  YHI = YLOW + .9*PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ENDIF
C
	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	    ENDIF
	    CALL MGOSETEXPAND(1.)
C
C	  CALL MGOTICKSIZE(0.,0.,5.6,28.)  
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C  	  CALL MGOSETLIM(TSTART,0.,TEND,40.)
  	  CALL MGOSETLIM(-1.1,0.,24.,40.)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(HRHI,FRQHI,NPTH)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOBOX(0,2)
	  CALL MGOYLABEL(3,'kHZ')
	  TRANGE = GY2-GY1
	  TINC = .1*TRANGE
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2 - 3.*TINC
	  CALL MGOSETEXPAND(.5)
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(4,'EXAC')
	  CALL MGOSETEXPAND(.8)
	  XTITLE = 1.02*GX2
	  YTITLE = GY2 - 5.*TINC 
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOSETEXPAND(.5)
	  WRITE(TITLE1(13),1001) NPTH
	  CALL MGOLABEL(6,TITLE1(13))
	  CALL MGOGRELOCATE(XTITLE,YTITLE-TINC)
	  CALL MGOLABEL(6,'EVENTS')
C
C	  CHANNEL 1 GOES AT THE TOP
C
C	  PLOT HI FREQUENCY CHANNEL
C
	  YLOW = YHI
	  YHI = YLOW + PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ENDIF
C
	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	    ENDIF
	    CALL MGOSETEXPAND(1.)
C
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
C  	  CALL MGOSETLIM(TSTART,0.,TEND,130.)
  	  CALL MGOSETLIM(-1.1,0.,24.,130.)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(HRHI,TDSHI,NPTH)
	    do i = 1,npth
	      ihivpm = tdshi(i)+128
	      write(95,*) hrhi(i),evno(i,1),tdshi(i)
     1		,1000.*tdscal(1,1,ihivpm)/41.1
	    enddo
	    do i = 1,nptl
	      ilovpm = tdslo(i)+128
	      write(98,*) evno(i,2),hrlo(i),tdslo(i),
     1		1000.*tdscal(5,1,ilovpm)/41.1
	    enddo
	  CALL MGOSETEXPAND(.5)
	  CALL MGOBOX(0,2)
	  CALL MGOYLABEL(10,'T/M NO.')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .1*TRANGE
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2
	  CALL MGOSETEXPAND(.5)
C	  INPUT = ICHPA(NCH)
C	  TITLE1(6) = PA(NCH,INPUT+1)
	  YTITLE = YTITLE - 3.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(4,'EXAC')
	  YTITLE = YTITLE - TINC
	  WRITE(TITLE1(2),1015) SPSHI(1)
	  WRITE(TITLE1(5),1011) FFILTER(IFILF0+1)
	  DO N = 1,6
	      YTITLE = YTITLE - TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(12,TITLE1(N))
	  ENDDO
	  IF(NSIM.GT.0) THEN
	    CALL MGOSETEXPAND(2.)
	    DO N = 1,NSIM
	      CALL MGORELOCATE(HRSIM(N),10.)
	      CALL MGOPOINT(3,2)
	    ENDDO
	    CALL MGOSETEXPAND(.5)
	  ENDIF
	  XTITLE = 1.02*GX2
	  YTITLE = GY2 - 4.*TINC
	  WRITE(TITLE1(2),1015) SPSHI(NPTH)
	  WRITE(TITLE1(5),1011) FFILTER(IFILF+1)
	    DO N = 1,6
	      YTITLE = YTITLE - TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(12,TITLE1(N))
	    ENDDO
	  CALL MGOSETEXPAND(1.)
	  CALL MGOSETEXPAND(.8)
C
	  CALL MGOPLOTID('TDS MAX','[KELLOGG.WIND]TDSRECT')
	  CALL MGOSETEXPAND(1.)
C	 ENDDO
	  CALL MGOSETEXPAND(.6)
	 CALL MGOGRELOCATE(GX1,GY2)
	 CALL MGOPUTLABEL(50,FILE,9)
	  CALL MGOSETEXPAND(1.)
C
	PRINT*,'TERMINAL',ITERM
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
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
	SUBROUTINE GETSTATUS(CH,MAJOR,MINOR,STATOK)
C
	CHARACTER*32 ITEM
	INTEGER*4 ICHPA(6),CH,MAJOR,MINOR,OK,STATOK
	integer*4	tds_channel,ifilf,ifils,ifsps,issps
	real 		ffilter(4),sfilter(4),fsps(4),ssps(4),sps
	COMMON /STATUS/ ifilf,ifils,ifsps,issps,ichpa,ifilf0,ifils0
C	
	   item = 'SLOW_RX_SPEED'
	   ok = wind_tm_get_item(CH, item, issps, 1, return_size)
C	   type*,'slow rx speed',issps
	   item = 'FAST_RX_SPEED'
	   ok = wind_tm_get_item(CH, item, ifsps, 1, return_size)
C	   type*,'fast rx speed',ifsps
	   item = 'SLOW_RX_FILTER'
	   ok = wind_tm_get_item(CH, item, ifils, 1, return_size)
C	   type*,'slow rx filter',ifils
	   item = 'FAST_RX_FILTER'
	   ok = wind_tm_get_item(CH, item, ifilf, 1, return_size)
C	   type*,'fast rx filter',ifilf
	   item = 'SOURCE_CHAN_1'
	   ok = wind_tm_get_item(CH, item, ichpa(1), 1, return_size)
	   item = 'SOURCE_CHAN_2'
	   ok = wind_tm_get_item(CH, item, ichpa(2), 1, return_size)
C          IF( .NOT. OK) TYPE*,'CANT GET SOURCE'
C	   type*,'CHAN 2 SOURCE',ICHPA(2)
	   item = 'SOURCE_CHAN_3'
	   ok = wind_tm_get_item(CH, item, ichpa(3), 1, return_size)
	   item = 'SOURCE_CHAN_4'
	   ok = wind_tm_get_item(CH, item, ichpa(4), 1, return_size)
	   item = 'SOURCE_CHAN_5'
	   ok = wind_tm_get_item(CH, item, ichpa(5), 1, return_size)
	   item = 'SOURCE_CHAN_6'
	   ok = wind_tm_get_item(CH, item, ichpa(6), 1, return_size)
	RETURN
	END
      SUBROUTINE SORTN(XS,FOLLOW1,FOLLOW2,FOLLOW3,IFOL4,NS)
C
      REAL FOLLOW1(1),XS(1),FOLLOW2(1),FOLLOW3(1)
      INTEGER*4 IFOL4(1)
C
C     ORDER X COORDS IN INCREASING ORDER
C
      DO 11 N = 1,NS-1
      N1 = N + 1
      DO 12 M = N1,NS
      IF((XS(M)-XS(N)).GT.0.) GO TO 12
C     EXCHANGE
      XT = XS(N)
      XS(N) = XS(M)
      XS(M) = XT
      XT = FOLLOW1(N)
      FOLLOW1(N) = FOLLOW1(M)
      FOLLOW1(M) = XT
      XT = FOLLOW2(N)
      FOLLOW2(N) = FOLLOW2(M)
      FOLLOW2(M) = XT
      XT = FOLLOW3(N)
      FOLLOW3(N) = FOLLOW3(M)
      FOLLOW3(M) = XT
      IFT = IFOL4(N)
      IFOL4(N) = IFOL4(M)
      IFOL4(M) = IFT
   12 CONTINUE
   11 CONTINUE
      RETURN
      END
	SUBROUTINE OFFIT(X,SUMSQ)
C
	INCLUDE  'TDS_CALDATA.FOR'
	COMMON /OFFBLK/ ITCH,NHIST(256,6)
	REAL X(25),VBOUND(129),NVHIST(128)
C
	INDEX=5
	DO N = 1,129
	  VBOUND(N) = A(INDEX)*EXP(B(INDEX)*(N-.5))
	ENDDO
C
C	DO NEGATIVE SIDE
C
C	COUNT ZEROS, AND MAXIMUM NO OF OCCURRENCES, FOR LOW T/M NO.
	NZEROS = 0
	MAXNO = 0
	NPMAX = 49
	DO N = 1,10
	  NP = 128-N
	  IF(NHIST(NP,ITCH).EQ.0) NZEROS = NZEROS + 1
	  IF(NHIST(NP,ITCH).GT.MAXNO) THEN
		MAXNO = NHIST(NP,ITCH)
		NPMAX = NP
	  ENDIF
	ENDDO
	PRINT*,'NEG SIDE, NZEROS,NPMAX,MAXNO=',NZEROS,NPMAX,MAXNO
	DO N = 1,50
	  XN = 128-N
	  VCALC = A(INDEX)*(EXP(B(INDEX)*XN) - NOFFS(INDEX))
	  DO M = 1,128
	     IF(VCALC.LT.VBOUND(M+1).AND.VCALC.GE.VBOUND(M)) THEN
		NVHIST(M) = NVHIST(M) + NHIST(N,ITCH)
		GO TO 20
	     ENDIF
	  ENDDO
 20	  CONTINUE
	ENDDO
C
C	DO POSITIVE SIDE
C
C	COUNT ZEROS, AND MAXIMUM NO OF OCCURRENCES, FOR LOW T/M NO.
	NZEROS = 0
	MAXNO = 0
	NPMAX = 50
	DO N = 1,10
	  NP = 127+N
	  IF(NHIST(NP,ITCH).EQ.0) NZEROS = NZEROS + 1
	  IF(NHIST(NP,ITCH).GT.MAXNO) THEN
		MAXNO = NHIST(NP,ITCH)
		NPMAX = NP
	  ENDIF
	ENDDO
	PRINT*,'POS SIDE, NZEROS,NPMAX,MAXNO=',NZEROS,NPMAX,MAXNO
	DO N = 1,50
	  XN = 127+N
	  VCALC = A(INDEX)*(EXP(B(INDEX)*XN) - NOFFS(INDEX))
	  DO M = 1,128
	     IF(VCALC.LT.VBOUND(M+1).AND.VCALC.GE.VBOUND(M)) THEN
		NVHIST(M) = NVHIST(M) + NHIST(N,ITCH)
		GO TO 40
	     ENDIF
	  ENDDO
 40	  CONTINUE
	ENDDO
	RETURN
	END
	SUBROUTINE OFFCHK(X,SUMSQ)
C
C	TRIES TO FIT A GAUSSIAN TO CALIBRATED HISTOGRAM DATA
C		NOT DONE YET
C
	INCLUDE  'TDS_CALDATA.FOR'
	COMMON /OFFBLK/ ITCH,NHIST(256,6)
	REAL X(25),VBOUND(129),NVHIST(128)
	INTEGER*4 MHIST(256,6)
	DATA MHIST /1536*0/
C
	INDEX=5
	DO N = 1,129
	  VBOUND(N) = A(INDEX)*EXP(B(INDEX)*(N-.5))
	ENDDO
C
	DO NCH = 1,6
	  ISPEED = 3			! DOESNT MATTER, SO LONG AS IT'S > 1
	  IF(NCH.LE.2) ISPEED = 0
	  DO NTM = 1,256
	    IF(NTM.NE.127) THEN
	      SIG = TDSCALT(NCH,ISPEED,NTM-1)
	      IHIST = 20000.*SIG + 128
	      IHIST = MAX0(IHIST,1)
	      IHIST = MIN0(IHIST,256)
	      MHIST(IHIST,NCH) = MHIST(IHIST,NCH) + NHIST(NTM,NCH)
C	      IF NTM.GT.120) THEN
C		PRINT*,'CHK',NTM,NCH,NHIST(NTM,NCH),SIG
C	        IF(NTM.GE.135) STOP
C	      ENDIF
	    ENDIF
	  ENDDO
	ENDDO
	DO NSIG = 1,256
	  WRITE(44,1044) (NSIG-128)/20.,(MHIST(NSIG,NCH),NCH=1,6)
	ENDDO
	RETURN
 1044	FORMAT(E12.4,6I9)
	END
	FUNCTION TDSCALT(CHANNEL,SPEED,TM)
C
C	this function converts the telemetry number TM from the TDS of
C	Wind-Waves to volts--voltage output from the preamplifiers.
C	If voltage input, i.e. voltage on the antenna, is desired, some
C	further calculation to correct for preamp gain as a function of
C	frequency is required.  
C		As there are 5 different A/D converters,
C	it is necessary to specify the channel  (1 to 6, ITEM = 'CHANNEL')  
C	For channels 1 and 2, the fast channels, it is also necessary to 
C	specify the speed, i.e. the sample rate (ITEM = "RX_SPEED")
C
C	INCLUDE 	'TDS_CALDATA.FOR'
C
	REAL A(6),B(6),POFFS(6),NOFFS(6),EFFLEN(4)
	INTEGER*4 CHANNEL,SPEED,TM,INDEX
	DATA B /.08316,.08151,.08099,.08212,.0834,.0834/
	DATA A /3.19E-4,2.14E-4,3.89E-4,2.40E-4,1.79E-4,2.03E-4/
C	DATA POFFS /4.01,6.31, 0., 0., 0., 3.5/
C	DATA NOFFS /1.59,1.79, 0., 0., 0., -6.6/
	DATA POFFS /4.01,6.31, 0., 0., 0., 4.5/
	DATA NOFFS /1.59,1.79, 0., 0., 0., -7.6/
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
	  TDSCALT = A(INDEX)*(EXP(B(INDEX)*TMNB) - POFFS(INDEX))
	ELSE
	  TDSCALT = -A(INDEX)*(EXP(-B(INDEX)*TMNB) - NOFFS(INDEX))
	ENDIF
	RETURN
	END

