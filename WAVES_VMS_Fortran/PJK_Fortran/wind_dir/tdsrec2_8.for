 	PROGRAM TDSREC
C
C	PLOTS TDS MAXIMA AND FREQUENCIES
C	MODIFIED FROM TDSREC TO (1) PRINT EVENT NO. AND TIME IN A
C	STANDARD FORMAT FOR READING BY OTHER PROGRAMS,  (2)  ADD
C	FILL OR TDSF, ADD PEAK FREQ AND BANDWIDTH TO RESULT FILE
C	NOT FINISHED, AND I GAVE UP
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
	integer*4	major, minor,ihist,nhist(256,5),npos(5),neg(5)
	integer*4	dpuhi(2000),dpulo(2000),dpuclk,deldpu
	character*80	file
	character*80	results(1000)
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
	WRITE(95,*) FILE
	WRITE(98,*) FILE
CT	WRITE(96,*) FILE
CT	write(96,*) 'LIST OF POSSIBLE SIMULTANEOUS EVENTS'
CT	write(96,*) 'HI evt no.   hi hrs    lo evt no. lo hrs'
CT     1  ,'  hi t/m  lo t/m  del DPU'
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
C
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
C	ITEM = 'DATA'
C	OK = WIND_TM_GET_ITEM(TDSCH,ITEM,NDATA,SIZE,RETURN_SIZE)
C	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
	IPROCESS=4
	CALL TDS_PHYS(TDSCH,IPROCESS,NDATA,DATA,SPECT)
	INC = 1
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	CALL FINDPEAK(1,2048,INC,SPECT,NPK,FPK)
	CALL FANDBW2(FREQ,DBX,DBY,NPK,FAVR,FBW,SKEW)

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
CT	NSIM = 0
CT	DO NL = 1,NPTL
CT	  DO NH = 1,NPTH
CT	    DELDPU = DPULO(NL)-DPUHI(NH)
CT	    LOC =2048./SPSLO(NL)/320.E-6  	!DPU COUNTS, LO EVENT
CT	    NHC =-2048./SPSHI(NH)/320.E-3	!DPU COUNTS, HI EVENT
CT	    IF(DELDPU.LT.LOC.AND.DELDPU.GT.NHC) THEN
CT	      IF(ABS(HRHI(NH)-HRLO(NL)).LT..001) THEN
CT		WRITE(96,1096) EVNO(NH,1),HRHI(NH),EVNO(NL,2),
CT     1		HRLO(NL),TDSHI(NH),TDSLO(NL),DELDPU
CT 1096		FORMAT(I10,F10.5,I12,F10.5,2F6.0,I6)
CT		NSIM = MIN0(NSIM+1,200)
CT		HRSIM(NSIM) = HRHI(NH)
CT	      ENDIF
CT	    ENDIF
CT	  ENDDO
CT	ENDDO
C
C	PLOT RESULTS
C
CT	type*,'call pltmax at npth limit, npth=',npth
C	type*,'call pltmax commented out, npth=',npth
C
	CALL SORTN(HRHI,TDSHI,FRQHI,SPSHI,EVNO(1,1),NPTH)
	CALL SORTN(HRLO,TDSLO,FRQLO,SPSLO,EVNO(1,2),NPTL)
	CALL SORTN(HRB,TDSB,FRQB,YY,EVNO(1,3),NPTB)
	CALL SORTCH(HRHI,TDSHI,FRQHI,SPSHI,EVNO(1,1),NPTH)
	CALL SORTCH(HRLO,TDSLO,FRQLO,SPSLO,EVNO(1,2),NPTL)
	CALL SORTCH(HRB,TDSB,FRQB,YY,EVNO(1,3),NPTB)
C
	    do i = 1,npth
	      ihivpm = tdshi(i)+128
	WRITE(56,1111) SCETI4,NO_EVT,EVENT(1:1),ISPS,IZCNT,FREQHZ,FAVR(1),
     1	 FREQ(NFMAX),ZCBW,FRBW,FP_3DP,XMAX,XRE,YRE,ZRE
 1111	FORMAT(I10,I8,I10,A2,I2,I4,3F6.0,2F6.2,F7.0,F6.1,3F7.1)
C
	      write(95,1095) hrhi(i),evno(i,1),tdshi(i)
     1		,1000.*tdscal(1,1,ihivpm)/41.1
	    enddo
	    do i = 1,nptl
	      ilovpm = tdslo(i)+128
	      write(98,1098) evno(i,2),hrlo(i),tdslo(i),
     1		1000.*tdscal(5,1,ilovpm)/41.1
	    enddo
 1095	FORMAT()
 1098	FORMAT()
CT	CALL PLTMAX(-2)
C	CALL PLTMAX(3)
CT	DO N = 1,5
CT	  WRITE(43,1043) (NHIST(J,N),J=1,256)
CT	ENDDO
CT	ITCH = 5
CT	CALL OFFIT(X,SUMSQ)
 1043	FORMAT(10I8)
CT	WRITE(43,*) FILE
CT	WRITE(43,*) NPOS
CT	WRITE(43,*) NEG
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
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PLOT TDS DATA
C
	IF(ITERM.LT.0) THEN
 	  YTOP = 2230.
	  YBOT = 200.
 	  XEND = 3000.
	ELSE
	  YTOP = GY2
	  YBOT = GY1
 	  XEND = .9*GX2
	ENDIF
C
	PRANGE = (YTOP-YBOT)/6.
	TSTART = AMIN1(HRHI(1),HRLO(1),HRB(1))
	HRHI1 = HRHI(1)
	HRLO1 = HRLO(1)
	HRB1  = HRB(1)
	TRENORM = AMIN1(HRHI1,HRLO1,HRB1)
	TEND = AMAX1(HRHI(NPTH1),HRLO(NPTL1),HRB(NPTB1))
	IF(ITERM.LT.0) PRINT*,'PLTMAX CALLED, TIMES',TSTART,TEND
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
C
C	  START AT THE BOTTOM
C
C	  PLOT B FREQ CHANNEL
C
	  YLOW = YBOT 
	  YHI = YLOW + .9*PRANGE	  
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(400.,YLOW,XEND,YHI)
	  ELSE
	    CALL MGOSETLOC(100.,YLOW,XEND,YHI)
	  ENDIF
C
	    CALL MGOSETEXPAND(.85)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(100.,20.)                      ! maxch, crt
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
C	print*,'before',gx,gy
	  CALL MGOXLABEL(16,'HOURS OF THE DAY')
C	print*,'after',gx,gy
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
	  ELSE
	    CALL MGOSETLOC(100.,YLOW,XEND,YHI)
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
	  ELSE
	    CALL MGOSETLOC(100.,YLOW,XEND,YHI)
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
	  ELSE
	    CALL MGOSETLOC(100.,YLOW,XEND,YHI)
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
	  ELSE
	    CALL MGOSETLOC(100.,YLOW,XEND,YHI)
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
	  ELSE
	    CALL MGOSETLOC(100.,YLOW,XEND,YHI)
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
	  CALL MGOPLOTID('TDS MAX','[KELLOGG.WIND]TDSREC')
	  CALL MGOSETEXPAND(1.)
C	 ENDDO
	  CALL MGOSETEXPAND(.6)
	 CALL MGOGRELOCATE(GX1,GY2)
	 CALL MGOPUTLABEL(50,FILE,9)
	  CALL MGOSETEXPAND(1.)
C
	IF(ITERM.LT.0) PRINT*,'TERMINAL',ITERM
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
C	  CALL MGOTCLOSE
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
      SUBROUTINE SORTCH(XS,CHLINE,NS)
C
      REAL XS(1)
      CHARACTER*80 CHLINE(1),CHT
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
      CHT = CHLINE(N)
      CHLINE(N) = CHLINE(M)
      CHLINE(M) = CHT
   12 CONTINUE
   11 CONTINUE
      RETURN
      END
	SUBROUTINE OFFIT(X,SUMSQ)
C
	INCLUDE  'TDS_CALDATA.FOR'
	COMMON /OFFBLK/ ITCH,NHIST(256,5)
	REAL X(25),VBOUND(129),NVHIST(128)
	DATA ITERM /-1/
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
	IF(ITERM.LT.0) 
     1      PRINT*,'NEG SIDE, NZEROS,NPMAX,MAXNO=',NZEROS,NPMAX,MAXNO
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
	IF(ITERM.LT.0) THEN
	  PRINT*,'POS SIDE, NZEROS,NPMAX,MAXNO=',NZEROS,NPMAX,MAXNO
	ENDIF
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
	SUBROUTINE FINDPEAK(N1,N2,INC,DBSUM,NPK,FPK)
C
	REAL DBX(1025),DBY(1025),DBSUM(1025)
	REAL F(5),BW(5),AMP(5)
	REAL X(25)
	INTEGER HPEAK
	DATA SPS /120000./
C
C	FINDS FREQUENCY OF HIGHEST PEAK BETWEEN N1 AND N2
C
C	  FIND MAXIMUM IN SPECTRUM
C
	SMAX = DBSUM(N1)
	NPK = N1
	DO N = N1+INC,N2,INC
	    IF(DBSUM(N).GT.SMAX) THEN
C	      CHECK THAT IT IS A PEAK
	      IF(DBSUM(N-1).LT.DBSUM(N).AND.DBSUM(N+1).LT.DBSUM(N))THEN
	        SMAX = DBSUM(N)
	        NPK = N
	      ENDIF
	    ENDIF
	ENDDO
	PRINT*,'in findpeak MAX:N,F,SMAX',NPK,(NPK-1)*SPS/2048.,SMAX
	FPK = (NPK-1)*SPS/2048.
C
	RETURN
	END
	SUBROUTINE FANDBW2(FREQ,DBX,DBY,NPEAK,FAVR,FBW,SKEW)
C
	DATA SPS /120000./
	REAL AMP(2),DBX(1025),DBY(1025),FREQ(1025)
	REAL FCOUNT(2),FAVR(2),FSTD(2),F3MOM(2),FBW(2),SKEW(2)
	DATA NFLLIMP,NFUPLIMP /4, 1024/
C
	CONST1 = (1./41.1)**2		! DO IN (VOLTS/M)**2
	CONST2 = (1./3.79)**2
C
C	CALCULATE BANDWIDTH, ETC.
	NFLLIM = NPEAK/2
	NFLLIM = MAX0(NFLLIM,NFLLIMP+1)
	NFUPLIM = (3*NPEAK)/2
	NFUPLIM = MIN0(NFUPLIM,NFUPLIMP)
C*********
C	THIS CHOICE DID NOT WORK WELL.  IT GAVE NARROW BANDWIDTHS
C	FOR SOME CASES WHEN THE BANDWIDTH WAS WIDE.  FOR EXAMPLE, IT
C	GAVE A BANDWIDTH OF 63.7 HZ FOR 19960421 # 13042040
C	NFLLIM = NPEAK-2
C	NFUPLIM = NPEAK+2
C*********
	AMP(1) = DBX(NPEAK)
	AMP(2) = DBY(NPEAK)
C
	DO NC = 1,2
	  FCOUNT(NC) = 1.E-20
	  FAVR(NC) = 0.
	  FSTD(NC) = 0.
	  F3MOM(NC) = 0.
	  DO N = NFLLIM,NFUPLIM
	    IF(NC.EQ.1) THEN
		VLT = CONST1*10.**(.1*DBX(N))
	    ELSE
		VLT = CONST2*10.**(.1*DBY(N))
	    ENDIF
	    FCOUNT(NC) = FCOUNT(NC) + VLT
	    FAVR(NC) = FAVR(NC) + VLT*FREQ(N)
	    FSTD(NC) = FSTD(NC) + VLT*FREQ(N)**2
	    F3MOM(NC) = F3MOM(NC) + VLT*FREQ(N)**3
	  ENDDO
	  FAVR(NC) = FAVR(NC)/FCOUNT(NC)
	  FSTD(NC) = FSTD(NC)/FCOUNT(NC) - FAVR(NC)**2
	  FBW(NC) = SQRT(AMAX1(FSTD(NC),0.))
	  SKEW(NC) = F3MOM(NC)/FCOUNT(NC) - 3.*FAVR(NC)*FSTD(NC)
     1		 - FAVR(NC)**3
	ENDDO
c	PRINT*,'FANDBW2,F,BW',FAVR(1),FBW(1),FAVR(2),FBW(2)
C
	RETURN
	END

