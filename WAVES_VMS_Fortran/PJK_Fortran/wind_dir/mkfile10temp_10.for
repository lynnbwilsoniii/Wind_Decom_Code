C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok,statok
	integer*4	i,j,k,n,N_EVT
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
	real*8		scet,scetstr
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
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
C	GET STARTED
C
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
	get_tm_stream = 1
C
	ok = wind_tm_open_channel(TDSCH,stream)
	if (.not. ok) stop 'Cannot open channel'
	ok = w_channel_filename(TDSCH,file)
	print*,file
	WRITE(91,*) FILE
	write(91,*) 'LIST OF POSSIBLE SIMULTANEOUS EVENTS'
	write(91,*) 'HI evt no.   hi hrs    lo evt no. lo hrs'
     1  ,'  hi t/m  lo t/m  del DPU'
C
	DO 200 LOCATE = 1,2
C
	NEVENT=0
	event = 'FILL'
	if(locate.eq.2) event='TDSF'
	SCET = 0.
	call w_channel_position(TDSCH,SCET)
	print*,' file starts at scet',SCET
	scetstr = scet
	dd = SCET

	ok = wind_tm_get_mfmf(TDSCH,major,minor)
	if (.not. ok) stop 'cannot get Mfmf stream position'
	print*,' after mfmf, at Mfmf=',major,minor
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
	ok = w_item_I4(tdsch, item, N_NEVT, 1, return_size)
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
C
C	IF(NEVENT.GT.20) GO TO 200

	ok = w_event(TDSCH,EVENT)
	NEVENT = NEVENT+1
	   if (ok.ne.1) then
	      if(ok.eq.82) go to 200
	      ok = wind_tm_get_mfmf(TDSCH,major,minor)
	      type *, char(7), '***** not OK missing packet in event ****'
	   end if
	  ok = wind_tm_get_mfmf(TDSCH,major,minor)
	  if (.not. ok) stop 'cannot get Mfmf stream position'
C
 110	  CONTINUE


C
C	ITEM = 'DATA'
C	OK = WIND_TM_GET_ITEM(TDSCH,ITEM,NDATA,SIZE,RETURN_SIZE)
C	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
C
	  IPROCESS = 4
	  CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
C
	  DO N = 1,2048
	      X4DATA(N,ITDSCH) = DATA(N)
	  ENDDO
C
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(TDSCH,ITEM,SCET,1,RETURN_SIZE)
	item = 'DPU_CLOCK'
	OK = W_ITEM_I4(TDSCH,ITEM,DPUCLK,1,RETURN_SIZE)
c******		temporary fix for bad first event
	if(dd.eq.0) dd = scet
c
	   IF(TDS_CHANNEL.LE.2) THEN
	      ISPS = IFSPS
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
C	IF(MAXTM.LT.51) THEN
	IF(MAXTM.GT.51) THEN
c
C	print*,'chann,pa,speed,maxtm',tds_channel,ichpa(tds_channel)
C     1   ,isps,maxtm
	  INDEX = 2*TDS_CHANNEL + MIN0(ISPS,1) - 1
	  ITCH = MIN0(INDEX,5)
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
C	LOOK FOR EVENTS IN WHICH ARE LESS THAN 3 SEC APART
C
	NSIM = 0
	DO NL = 1,NPTL
	  DO NH = 1,NPTH
	    DELDPU = DPULO(NL)-DPUHI(NH)
	    LOC =2048./SPSLO(NL)/320.E-6  	!DPU COUNTS, LO EVENT
	    NHC =-2048./SPSHI(NH)/320.E-3	!DPU COUNTS, HI EVENT
	    IF(DELDPU.LT.LOC.AND.DELDPU.GT.NHC) THEN
 	      IF(ABS(HRHI(NH)-HRLO(NL)).LT..001) THEN
		WRITE(91,1096) EVNO(NH,1),HRHI(NH),EVNO(NL,2),
     1		HRLO(NL),TDSHI(NH),TDSLO(NL),DELDPU
 1096		FORMAT(I10,F10.5,I12,F10.5,2F6.0,I6)
		NSIM = MIN0(NSIM+1,200)
		HRSIM(NSIM) = HRHI(NH)
	      ENDIF
	    ENDIF
	  ENDDO
	ENDDO
	STOP
	END
C
	CALL SORTN(HRHI,TDSHI,FRQHI,SPSHI,EVNO(1,1),NPTH)
	CALL SORTN(HRLO,TDSLO,FRQLO,SPSLO,EVNO(1,2),NPTL)
	CALL SORTN(HRB,TDSB,FRQB,YY,EVNO(1,3),NPTB)
C
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
	SUBROUTINE FANDBW(SPS,ICHANN,DBSPECT,NPEAK)
C
	REAL DBSPECT(1025,2)
	INTEGER HPEAK
C
C	FINDS FREQUENCY OF HIGHEST PEAK AND ITS BANDWIDTH
C
C	FIRST FIND THE MAXIMUM IN THE SPECTRUM. 
C	THEN TRACE EACH SIDE DOWN TO:
C		(1) HALFWAY TO BACKG
C		(2) SIGNAL STARTS TO GO UP AGAIN (REQUIRE 3 UP POINTS)
C	THEN FIT A/((W-W0)**2 + DW**2) TO THESE POINTS IF THERE
C		ARE AT LEAST 3 POINTS
C	THEN LOOK FOR POSSIBLE SECOND PEAK
C
C	  FIND MAXIMUM IN SPECTRUM
C
	SMAX = -1000.
	DO N = 10,1022
	    IF(DBSPECT(N,1).GT.SMAX) THEN
	      SMAX = DBSPECT(N,1)
	      NSPMAX1 = N
	    ENDIF
	ENDDO
	PRINT*,'X SPEC. MAX:N,F,SMAX',NSPMAX1,(NSPMAX1-1)*SPS/2048.,SMAX
	FREQMAX1 = (NSPMAX1-1)*SPS/2048.
C
C	GO DOWN LOW SIDE
C
	SLIM = .5*(SMAX - 80.)
	LCOUNT = 0
	SSAVE = SMAX
	LPEAK = 0
	DO N = NSPMAX1-1,10,-1
	    NSVL = N
	    IF(DBSPECT(N,1).GT.SSAVE) THEN
	      LCOUNT = LCOUNT+1
	      IF(LCOUNT.GE.3) THEN
		LPEAK = 1
		GO TO 20
	      ENDIF
	    ENDIF
	    IF(DBSPECT(N,1).LT.SLIM) GO TO 20
	    SSAVE = DBSPECT(N,1)
	ENDDO
C
C	GO DOWN HIGH SIDE
C
 20	CONTINUE
	LCOUNT = 0
	SSAVE = SMAX
	HPEAK = 0
	DO N = NSPMAX1+1,1024
	    NSVH = N
	    IF(DBSPECT(N,1).GT.SSAVE) THEN
	      LCOUNT = LCOUNT+1
	      IF(LCOUNT.GE.3) THEN
		HPEAK = 1
		GO TO 30
	      ENDIF
	    ENDIF
	    IF(DBSPECT(N,1).LT.SLIM) GO TO 30
	    SSAVE = DBSPECT(N,1)
	ENDDO
C
 30	CONTINUE
C
C	DETERMINE SECOND AND MAYBE THIRD PEAK
C
	IF(LPEAK.NE.0) THEN
	  SMAX = -1000.
	  DO N = NSVL+2,10,-1
	    IF(DBSPECT(N,1).GT.SMAX) THEN
	      SMAX = DBSPECT(N,1)
	      NSPMAX3 = N
	    ENDIF
	  ENDDO
	  FREQMAX3 = (NSPMAX3-1)*SPS/2048.
	  PRINT*,'2ND PK. MAX:N,F,SMAX',NSPMAX3,FREQMAX3,SMAX
	ENDIF
C
	IF(HPEAK.NE.0) THEN
	  SMAX = -1000.
	  DO N = NSVH-2,1024
	    IF(DBSPECT(N,1).GT.SMAX) THEN
	      SMAX = DBSPECT(N,1)
	      NSPMAX4 = N
	    ENDIF
	  ENDDO
	  FREQMAX4 = (NSPMAX4-1)*SPS/2048.
	  PRINT*,'2ND PK. MAX:N,F,SMAX',NSPMAX4,FREQMAX4,SMAX
	ENDIF
C
	RETURN
	END
