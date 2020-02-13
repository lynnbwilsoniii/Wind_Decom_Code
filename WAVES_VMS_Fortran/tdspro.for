
	PROGRAM TDSPRO
C
C	PLOTS TDS AND FREQUENCY
C	SOME ERRORS FOUND IN THIS AND IN TDS_PHYS ON 13 JULY 1995
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok,okt
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6),ipacal(6,4)
	character*80	stream
	character*4	event
	character*4	pa(6,4),parx(9)
	parameter	size=2048
	integer*4	return_size
	integer*4	tds_channel,ifilf,ifils,ifil,ifsps,issps,isps
	integer*4	temp_waves,iend,n2
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scettds,scetfill,beginevt,endevt
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
	real 		FILTER,ffilter,sfilter,fsps,ssps,sps
	REAL 		S1,S2,dpuclk,sunclock
!
	common /nrblk/ nrem,NHRMN,IFASTSLOW
C	common /headblk/ major,minor,tds_channel,s_scet,ch
	common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds

C
	CHARACTER*12 PTITLE(30)
	INTEGER*4 TDSCH,hkch,fillch,ch,NUMBER_EVENT
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /HEADBL/ PTITLE,EVENT,NUMBER_EVENT,CH
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /FRLIMITS/ FFTMIN,FFTMAX
	DATA TWOPI /6.2831853/
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.875/
	DATA SSPS /7500.,1875.,468.75,117.2/
	DATA PARX /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1    ' BX ',' BY ',' BZ '/
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
	DATA IPACAL /1,    1,     4,     7,     8,     9,
     1               4,    2,     5,     4,     5,     6,
     2               0,    3,     6,     0,     0,     0,
     3               0,    3,     6,     0,     0,     0/
	DATA IFASTSLOW /0/		! 0 IS FAST, 1 IS SLOW
	DATA ICHEOF /0/			! 1 IS EOF ON FILL, 2 ON TDS
C	NOTE FFTMIN IS SET LATER IN PROGRAM
	DATA FFTMIN,FFTMAX /150.,60000./
C	DATA FFTMIN,FFTMAX /20.E3,60000./
C
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = 'TIME DOMAIN'
	PTITLE(3) = '  SAMPLER'
	PTITLE(4) = 'EVENT NO.'
	PTITLE(8) = 'SAMPLE RATE'
	PTITLE(10) = ' L.P.FILTER'
C	PTITLE(12) = 'TRIGGERS'
	PTITLE(13) = 'DPU CLOCK'
	PTITLE(15) = 'SCET'
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
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
	  write(6,4)
	  read(5,*,err=10,end=20)  iend,n2
	  type*,iend,n2
	    if(iend.eq.1) then
		nrem = n2
	    endif
	    if(iend.eq.2) then
		nrem = 2
			if(ifastslow.eq.1) nrem = 4
		nevent = n2
	    endif
	endif
	type*,'type desired process level,0=raw,1=raw volts,2=fft,fft-1'
	read(5,3) iq,iprocess
	type*,' '
	if(iprocess.eq.0) type*,'ok, plot data in tm numbers - 128'
	if(iprocess.eq.1) type*,'ok, plot in volts, unity freq. response.'
	if(iprocess.ge.2) type*,'ok, plot volts, corrected for freq. response.'
	if(iprocess.eq.3) type*,'        at the frequency of the spectral peak'
	if(iprocess.eq.4) type*,'            and corrected for bad TDS data'
	type*,' '
c
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)
c
	ok = w_channel_open(tdsch,stream)
	if (.not. ok) stop 'Cannot open tds channel'
	scettds = 0.
	call w_channel_position(tdsch,scettds)
	print*,'tds file starts at scettds',scettds
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set tds channel position to',scettds
	call w_channel_position(tdsch,scettds)
	print*,'tds channel position set to',scettds

	ok = w_channel_open(fillch,stream)           
	if (.not. ok) stop 'Cannot open fill channel'      
	scetfill = 0.
	call w_channel_position(fillch,scetfill)
	print*,'fill channel starts at',scetfill
	dds = scetfill
	scetfill = float(dds) + hh/24. + mm/1440.
	print*,'set fill channel position to',scetfill
	call w_channel_position(fillch,scetfill)
	print*,'fill channel position set to',scetfill
c
	call w_ur8_to_ymd(scetfill,yyyy,mon,dd,hh,mm,ss,ms)
	call w_ur8_to_ydoy(scetfill,yyyy,doy,msday)
C
        IF(IFASTSLOW.EQ.0) THEN
	  ok = w_event(tdsch,'TDSF')
	  if (ok.ne.1) type*, 'cannot get TDSF event,ok=',ok
	ELSE
          ok = w_event(tdsch,'TDSS')
	  if (ok.ne.1) type*, 'cannot get TDSS event,ok=',ok
	ENDIF
	if(ok.eq.82) then
	   scettds = 10000.       	! artificially large
	else
	  item = 'EVENT_SCET'
	  ok = w_item_i4(tdsch, item, s_scet, 2, return_size)
	  print*,'initial tdsch time',s_scet
	endif
c
        ok = w_event(fillch,'FILL')
	if(ok.eq.82) stop 'end of file on fillch'
	if (.not. ok) stop 'cannot get fill event'
	item = 'EVENT_SCET_R8'
	ok = w_item_r8(fillch, item, scetfill, 1, return_size)
c	item = 'EVENT_SCET'
c	ok = w_item_i4(fillch, item, s_scet, 2, return_size)
	print*,'initial fillch time',s_scet
	  
c
	ok = w_channel_filename(tdsch,file)
c	write(87,*) file
	print*,file
c
	get_tm_stream = 1
C
C	now two channels are open, fillch and tdsch, and positioned at
c	the first event of each kind after the requested start time. 
c
	type*,'start at tds,fill times',scettds,scetfill
	go to 100
C
C	GET NEXT EVENT
C
 110    continue
C
	! this is the main program loop

	type*,'going to get next event,tds,fill times',scettds,scetfill
c
c	now get the next event of the kind just done.  if it is earlier
c	than the other kind, process it
c
	ok = w_event(ch,event)
	if (ok.ne.1) then
              if( ok.eq.82) then
		type*,'end of file on ',event
	        if(ch.eq.tdsch) then
		  if(icheof.eq.1) stop 'end of both files'
	          icheof = 2
		  event = 'FILL'
	  	  ch = fillch
		  go to 110
	        else
		  if(icheof.eq.2) stop 'end of both files'
	          icheof = 1
		  event = 'TDSF'
	  	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  	  ch = tdsch
		  go to 110
	        endif
	      else
	        type *, char(7), '******** missing packet in event ********'
	      endif
	endif
c
	item = 'EVENT_SCET_R8'
	ok = w_item_R8(ch, item, scett, 1, return_size)
c
	if(ch.eq.tdsch) then
	          scettds = scett
	else
	          scetfill = scett
	endif
c
 100	continue
c
c	now process the earliest one
c
	if(scettds.lt.scetfill.and.icheof.ne.2) then
	  event = 'TDSF'
	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  ch = tdsch
	elseif(icheof.ne.1) then
	  event = 'FILL'
	  ch = fillch
	else
	  print*,'ends?',icheof
	endif
c
c	unless end of file on this channel
c
c	if(icheof.eq.1) then
c	  event = 'TDSF'
c	  IF(IFASTSLOW.NE.0)event = 'TDSS'
c	  ch = tdsch
c	elseif(icheof.eq.2) then
c	  event = 'FILL'
c	  ch = fillch
C	else
c	endif
c
c	type*,'compare scettds,scetfill,nxt evt',scettds,scetfill,event
c
	       item = 'EVENT_SCET'
	       ok = w_item_i4(ch, item, s_scet, 2, return_size)
	       ss = mod(s_scet(2),100)
	       mm = s_scet(2)/100
	       mm = mod(mm,100)
	       hh = s_scet(2)/10000
	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
c*********
	       item = 'EVENT_NUMBER'
	       ok = w_item_i4(ch, item, itemp, 1, return_size)
	       type*,'event number',itemp,'  ',event
	       if(iend.eq.2.and.itemp.ne.nevent) go to 110
	       item = 'DPU_CLOCK'
	       ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
	       WRITE(PTITLE(14),1014) DPUCLK
 1014		FORMAT(F12.3)
	       item = 'SUN_ANGLE'
	       ok = w_item_R4(ch, item, sunclock, 1, return_size)
	       item = 'SUN_ANGLE_R4'
	       ok = w_item_R4(ch, item, sunANG, 1, return_size)
c		PRINT*,'#,SUN_ANGLE, SUN_ANGLE_R4',ITEMP,SUNCLOCK,SUNANG
	       item = 'source'
	       ok = w_item_i4(ch, item,isource, 1, return_size)
C
	       IF(ISOURCE.LE.3) THEN
		 FFTMIN = 150.
	       ELSEIF(ISOURCE.LT.7) THEN
		 FFTMIN = 50.      		! A GUESS, NOT WELL FOUNDED
C                 FFTMIN = 10.                   ! A 2nd GUESS, NOT WELL FOUNDED  
	       ELSE
		 FFTMIN = 3.3
	       ENDIF
c	       if(isource.ne.9) go to 110
C
	       item = 'event_boe_r8'
	       ok = w_item_R8(ch, item, beginevt, 1, return_size)
	       item = 'event_eoe_r8'
	       ok = w_item_R8(ch, item, endevt, 1, return_size)
c	
	   CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
C	if(irx.eq.8) then
CC	  do n = 1,1025
C	        FREQ = SPS*(IK-1)/4096.
C	    write(84,*) n,sps*(n-1)/4096.,spect(n)
C	  enddo
C	  stop
C	endif
C
c	   item = 'CHANNEL'
c	   ok = w_item_I4(ch, item, tds_channel, 1, return_size)
c
c	do n = 1,2048
C	do n = 769,1280			! central 512 samples
c	do n = 917,1103			! 
C		write(56,*) n,TDS_CHANNEL,itemp,ndata(n),data(n)
c		write(56,*) n,N/120.,TDS_CHANNEL,itemp,data(n)
c	enddo
C

C	     item = 'EVENT_SCET_R8'
c	     ok = w_item_r8(tdsch, item, scet, 1, return_size)
c	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c	     call w_ur8_to_string(scet,PTITLE(16),PTITLE(18))
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	     call w_ur8_to_ydoy(scetT,yyyy,doy,msday)
	     ihrmn = 100*hh+mm
	     TYPE *,s_scet
	     TYPE *,'scett,doy',scett,doy
C		write(26,*) itemp,s_scet
	   ihrmn = 100*hh+mm

	
	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)



	   WRITE(PTITLE(16),1016) s(1:4),S(5:6),S(7:8)
	   WRITE(PTITLE(17),1017) DOY
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1017	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
	   TYPE*,'CHANNEL',tds_channel
	   WRITE(PTITLE(6),1019) TDS_CHANNEL
 1019	   FORMAT('CHANNEL',I2)
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
	   WRITE(PTITLE(5),1012) ITEMP
 1012	   FORMAT(I10)
C
	   item = 'ERT_MAJOR_FRAME'
	   ok = W_ITEM_I4(ch, item, MAJOR, 1, return_size)
	   item = 'ERT_MINOR_FRAME'
	   ok = W_ITEM_I4(ch, item, MINOR, 1, return_size)
C
c	   item = 'SLOW_RX_SPEED'
c	   ok = wind_tm_get_item(ch, item, issps, 1, return_size)
c	   type*,'slow rx speed',issps
c	   item = 'FAST_RX_SPEED'
c	   ok = wind_tm_get_item(ch, item, ifsps, 1, return_size)
c	   type*,'fast rx speed',ifsps
c	   item = 'SLOW_RX_FILTER'
c	   ok = wind_tm_get_item(ch, item, ifils, 1, return_size)
c	   type*,'slow rx filter',ifils
c	   item = 'FAST_RX_FILTER'
c	   ok = wind_tm_get_item(ch, item, ifilf, 1, return_size)
c	   type*,'fast rx filter',ifilf
c	   item = 'SOURCE_CHAN_1'
c	   ok = wind_tm_get_item(ch, item, ichpa(1), 1, return_size)
c	   item = 'SOURCE_CHAN_2'
c	   ok = wind_tm_get_item(ch, item, ichpa(2), 1, return_size)
c	   item = 'SOURCE_CHAN_3'
c	   ok = wind_tm_get_item(ch, item, ichpa(3), 1, return_size)
c	   item = 'SOURCE_CHAN_4'
c	   ok = wind_tm_get_item(ch, item, ichpa(4), 1, return_size)
c	   item = 'SOURCE_CHAN_5'
c	   ok = wind_tm_get_item(ch, item, ichpa(5), 1, return_size)
c	   item = 'SOURCE_CHAN_6'
c	   ok = wind_tm_get_item(ch, item, ichpa(6), 1, return_size)
C
	   ipa = ichpa(tds_channel)
C	   WRITE(PTITLE(7),1007) pa(tds_channel,ipa+1)
	   WRITE(PTITLE(7),1007) PARX(IRX)
 1007	   FORMAT('P/A ',A4)
c	   IRX = IPACAL(TDS_CHANNEL,IPA+1)
C	   IF(TDS_CHANNEL.LE.2) THEN
c	      WRITE(PTITLE(9),1004) FSPS(ISSPS+1)
c	      SPS = 1000.*FSPS(IFSPS+1)
C	      WRITE(PTITLE(11),1008) FFILTER(IFILF+1)
C	      IFIL = IFILF
C	   ELSE
C	      WRITE(PTITLE(9),1008) SSPS(ISSPS+1)
C	      SPS = SSPS(ISSPS+1)
C	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
C	      IFIL = IFILS
C	   ENDIF
	   IF(TDS_CHANNEL.LE.2) THEN
	      WRITE(PTITLE(9),1004) .001*SPS
	   ELSE
	      WRITE(PTITLE(9),1008) SPS
	   ENDIF
	      WRITE(PTITLE(11),1008) FILTER
 1004	   FORMAT(F7.2,' kHZ')
 1008	   FORMAT(F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

C	   if (ok) type *, 'waves2 temperature', temp_waves2
CW		write(16,*) 'temp in t/m counts',temp_waves
C		write(26,*) 'temp in t/m counts',temp_waves
c	   ok = wind_tm_xlate_item(ch, 'HK', item, temp_waves, title)
c		type*,'temp in degrees?',title
c	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c
C
C	ITEM = 'DATA'
C	OK = WIND_TM_GET_ITEM(CH,ITEM,NDATA,SIZE,RETURN_SIZE)
C	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
C	  PRINT*,'DATA, SIZE=',RETURN_SIZE
C	  PRINT 222, (NDATA(J),J=1,2048)
 222	FORMAT(10I6)
	MAXDATA = 0
	IF(IPROCESS.EQ.0) THEN
	  DO IK = 1,2048
	    NDATA(IK) = NDATA(IK)-128
	    MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
	  ENDDO
	ELSE
	  DO IK = 1,2048
C	    DATA(IK) = TDSCAL(TDS_CHANNEL,ISPS,NDATA(IK))
	    NDATA(IK) = NDATA(IK)-128
	    MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
	  ENDDO
	ENDIF
**********************
c	ISUNCL = AMOD(SUNCLOCK,2048.)
c	IF(ISUNCL.LT.1750) GO TO 110
c	write(77,*) s_scet,itemp,sunclock
c	IF(ISUNCL.LT.5050) GO TO 110
	IF(IFASTSLOW.EQ.0.AND.TDS_CHANNEL.GT.2) GO TO 110
c	write(87,7737) itemp,event,s_scet(2),tds_channel,
c     1	parx(irx),maxdata,
c     2	tdscal(tds_channel,isps,maxdata+128)
 7737	format(i10,2x,a4,i10,i5,2x,a4,i5,e12.3)
	print*,'iend,tds_channel,maxdata,nrem,nplots',iend,
     1		tds_channel,maxdata,nrem,nplots
C	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.85) GO TO 110
c	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.65) GO TO 110
C	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.2.AND.MAXDATA.LE.75) GO TO 110
C	IF(IEND.EQ.1.AND.MAXDATA.LE.78) GO TO 110
C	IF(IEND.EQ.1.AND.TDS_CHANNEL.GE.2) GO TO 110
C	IF(IRX.NE.9) GO TO 110
C	IF(IEND.EQ.1.AND.TDS_CHANNEL.NE.3) GO TO 110
c	to pick out a specific event  (NOW MOVED EARLIER)
	if(iend.eq.2.and.itemp.ne.nevent) go to 110
C*******************
C
C	FIND ZERO CROSSING
C
	IZCNT = 0
	IL = 1
	IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL),NDATA(IL+1)
	DO IL = 2,2047
	  IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL-1),
     1   NDATA(IL),NDATA(IL+1)
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
		IF(IPROCESS.EQ.0) THEN
		  S1 = NDATA(IL)
		  S2 = NDATA(IL+1)
		ELSE
		  S1 = DATA(IL)
		  S2 = DATA(IL+1)
		ENDIF
	        DELZ = .5
		IF((S1-S2).NE.0.) DELZ = S1/(S1 - S2)
		IF(DELZ.LE.1.AND.DELZ.GE.0.) THEN
			ZCROSS(IZCNT) = IL + DELZ
		ELSE
			ZCROSS(IZCNT) = IL + .5
		ENDIF
	  ENDIF
	ENDDO
	DO N = 1,IZCNT-1
	  ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
	  IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
	  IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
	ENDDO
	print*,'zero crossings found',izcnt
	print*,'first 5',(zcross(kk),kk=1,5)
	IF(IZCNT.GT.1) AVRINT = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
C
C	REMOVE CROSSINGS WHICH ARE TOO CLOSE TOGETHER AND ARE PROBABLY 
C		SPURIOUS
C
	NREMOVE = 0
	DO N = 1,IZCNT-1
	  IF(ZINT(N).LT.2..AND.AVRINT.GT.5.) THEN
	    DO NN = N,IZCNT-1
	      ZINT(NN) = ZINT(NN+1)
	      NREMOVE = NREMOVE+1
	    ENDDO
	  ENDIF
	ENDDO
	IZCNT = IZCNT-NREMOVE
C
	IF(IPROCESS.EQ.2) THEN
c	TYPE*,'BEFORE'
c	TYPE*,(DATA(I),I=1,10)
c	TYPE*,(DATA(I),I=2040,2048)
C		REALFT IS FROM NUMERICAL RECIPES, CAMBRIDGE U PRESS
C	     CALL REALFT(DATA,1024,1)
C	     CALL SPPLOT(-2)
c	TYPE*,'FFT'
c	TYPE*,(DATA(I),I=1,10)
c	TYPE*,(DATA(I),I=2040,2048)
C
C	CORRECT FOURIER ANALYSED SIGNAL FOR FREQUENCY RESPONSE
C	THIS PART IS NOW IN TDS_PHYS
C	     FREQMAX = .5*SPS
C             EJUNK = PREAMP(IRX,FREQMAX)
C	     PAMAX = CABS(CGAIN)
C	     IF(TDS_CHANNEL.LE.2) THEN
C		IF(IPA.EQ.0) PAMAX = 2.4  		       ! EXAC
C		IF(TDS_CHANNEL.EQ.2.AND.IPA.EQ.1) PAMAX = 6.6  ! EYAC
C		IF(TDS_CHANNEL.EQ.2.AND.IPA.GT.1) PAMAX = 7.   ! EZAC
C	     ENDIF
C	     DO IK = 3,2048,2
C		FCOEF = CMPLX(DATA(IK),DATA(IK+1))
C	        FREQ = SPS*(IK-1)/4096.
C	        EJUNK = PREAMP(IRX,FREQ)
C		FCOEFT = FCOEF/CONJG(CGAIN)
C		CCORR = CGAIN
C		EJUNK = TDSDET(TDS_CHANNEL,FREQ)
C		FCOEFT = FCOEFT/CONJG(CGAIN)
C		CCORR = CGAIN*CCORR
C		EJUNK = TDS_FILTER(TDS_CHANNEL,IFIL+1,FREQ)
C		EJUNK = TDS_FILTER(TDS_CHANNEL,FILTER+1,FREQ)
C		FCOEFT = FCOEFT/CGAIN
C		CCORR = CGAIN*CCORR
C		DON'T DO CORRECTION IF CORRECTION IS LARGE, I.E. 
C			IF GAIN IS SMALL
C		IF(CABS(CCORR).GT..1*PAMAX) FCOEF = FCOEFT       ! 20 DB
C		IF(CABS(CCORR).GT..0316*PAMAX) FCOEF = FCOEFT    ! 30 DB
C		IF(CABS(CCORR).GT..01*PAMAX) FCOEF = FCOEFT      ! 40 DB
C		DATA(IK) = FCOEF
C		DATA(IK+1) = AIMAG(FCOEF)
C	     ENDDO
c	TYPE*,'FFT CORRECTED'
c	TYPE*,(DATA(I),I=1,10)
c	TYPE*,(DATA(I),I=2040,2049)
C	FCORR = 20.*ALOG10(1024.)
C	DO N = 3,2049,2
C	  NP = (N-1)/2
C	  SPECT(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2) - FCORR
C	ENDDO
C	     CALL SPPLOT(-2)
C
C	     CALL REALFT(DATA,1024,-1)
C	     DO IK = 1,2049
C	       DATA(IK) = DATA(IK)/1024.
C	     ENDDO
c	TYPE*,'AFTER'
c	TYPE*,(DATA(I),I=1,10)
c	TYPE*,(DATA(I),I=2040,2049)
	ENDIF
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	number_event = itemp
C	CALL PLTDSFR(-2)
	CALL COMBO(-3)			! save
C	CALL COMBO(-1)
C	CALL PARTSPEC(852,512,1,DATA)
c	CALL PARTSPEC(1,512,1,DATA)
c	CALL PARTSPEC(1535,512,1,DATA)
C	CALL PARTSPEC(240,1024,1,DATA)
C	CALL PUBCOMBO(-1)
C	CALL PUBCOMBO2(-1)
c	CALL PUBCOMBO2(3)
C	CALL PUBCOMBO2(-3)		! save
c	IF(IPROCESS.LE.1) CALL DETAIL(-2,900,1100)
C	CALL DETAIL(-2,924,1124)
C	CALL DETAIL(-2,1,200)
c	CALL DETAIL(-2,1847,2047)
C	CALL DETAIL(-2,900,1100)
	NPLOTS=NPLOTS+1
	print*,'got to nplots.lt.nrem;',nplots,nrem
	IF(NPLOTS.LT.NREM) GO TO 110
	STOP
	END
	SUBROUTINE PLTDSFR(ITERM)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 TITLE(30)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 NUMBER_EVENT,CH
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
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
	DIMENSION YY(2048),PP(2048)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PLOT TDS DATA
C
	XEND = 2750.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(300.,1100.,XEND,2230.)
	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	IF(IPROCESS.EQ.0) THEN
	  DO N = 1,2048
  	    PP(N) = N
	    YY(N) = NDATA(N)
	  ENDDO
	ELSE
	  YMAX = 0.
	  DO N = 1,2048
  	    PP(N) = N
	    YY(N) = DATA(N)
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
	ENDIF
C
	PRINT*,'MAX VOLTS',YMAX
	IF(IPROCESS.EQ.0) THEN
		CALL MGOTICKSIZE(0.,0.,5.6,28.)  
		CALL MGOSETLIM(-2.,-130.,2050.,130.)
	ELSE
		CALL MGOTICKSIZE(0.,0.,0.,0.)  
		CALL MGOSETLIM(-2.,-YMAX,2050.,YMAX)
	ENDIF
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,2048)
	CALL MGOSETEXPAND(.8)
	CALL MGOBOX(0,2)
	IF(IPROCESS.EQ.0) THEN
		CALL MGOYLABEL(14,'T/M NUMBER-128')
	ELSE
		CALL MGOYLABEL(14,'VOLTS or nT')
	ENDIF
	CALL MGOSETEXPAND(1.)
	TRANGE = GY2-GY1
C	TINC = .08*TRANGE
	TINC = .07*TRANGE
	XTITLE = GX2 +.005*(GX2-GX1)
	YTITLE = GY2
	CALL MGOSETEXPAND(.8)
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = 'AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')

C	  TITLE(16) = 'FFT BANDPASS'
C	  WRITE(TITLE(17),1019) FFTMIN,FFTMAX
C 1019	  FORMAT(F5.1,'-',F5.0)


	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO')
	CALL MGOSETEXPAND(1.)
C
C	PLOT FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C		OR 1./INT FOR POS TO NEG
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(300.,200.,XEND,1050.)
	ENDIF
	SPSKHZ = .001*SPS
	YMIN = .5*SPSKHZ
	YMAX = 0.
	PRINT*,'IZCNT',IZCNT
	DO N = 1,IZCNT-1
	IF(ZINT(N).EQ.0.) PRINT*,'IN PLOT, ZINT=0 AT',N
C	  YY(N) = .5*SPSKHZ/ZINT(N)
	  YY(N) = SPSKHZ/ZINT(N)
	  PP(N) = .5*(ZCROSS(N)+ZCROSS(N+1))
	  YMIN = AMIN1(YY(N),YMIN)
	  YMAX = AMAX1(YY(N),YMAX)
	ENDDO
  	YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
	PRINT*,'YMIN,YMAX',YMIN,YMAX
	CALL MGOSETEXPAND(.8)
	CALL MGOSETLIM(-2.,YMIN,2050.,YMAX)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOCONNECT(PP,YY,IZCNT-1)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(10,'SAMPLE NO.')
	CALL MGOYLABEL(10,'FREQ (kHZ)')
	CALL MGOSETEXPAND(1.)
C
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
	SUBROUTINE COMBO(ITERM)
C
C	THE FIRST (TOP) PANEL IS THE DATA IN T/M UNITS,
C	THE SECOND IS IN PHYSICAL UNITS, THE THIRD IS THE FREQ  FROM 
C	ZERO CROSSINGS, AND THE FOURTH IS THE FOURIER TRANSFORM
C
	CHARACTER*12 TITLE(30)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	character*32	item
	REAL*8 BEGINevt,endevt
	REAL ANG(2048)
	INTEGER*4 TDS_CHANNEL,S_SCET(2),ERT(2)
	INTEGER*4 NUMBER_EVENT,CH,nhr,min
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
	COMMON /FIXUPBLK/ NBAD3,NBAD2,NBAD1,IFXB
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /FRLIMITS/ FFTMIN,FFTMAX
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
	DIMENSION YY(2048),YYT(2048),PP(2048)
	DATA TWOPI /6.2831853/
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PUT LABELS ON RIGHT HAND SIDE
C
	     item = 'R_EARTH_R4'
	     ok = w_item_R4(ch, item, RE, 1, return_size)
	     item = 'WIND_ORBIT_X(GSE)_R8'
	     ok = w_item_R8(ch, item, XR8, 1, return_size)
	     XRE = 1000.*XR8/RE
	     item = 'WIND_ORBIT_Y(GSE)_R8'
	     ok = w_item_R8(ch, item, YR8, 1, return_size)
	     YRE = 1000.*YR8/RE
	     item = 'WIND_ORBIT_Z(GSE)_R8'
	     ok = w_item_R8(ch, item, ZR8, 1, return_size)
	     ZRE = 1000.*ZR8/RE
	     TITLE(23) = 'WIND ORBIT'
	     WRITE(TITLE(24),1034) XRE
 1034	FORMAT('Xgse',F6.1)
	     WRITE(TITLE(25),1035) YRE
 1035	FORMAT('Ygse',F6.1)
	     WRITE(TITLE(26),1036) ZRE
 1036	FORMAT('Zgse',F6.1)
	     item = 'WIND_MFI_BZ(GSE)_R4'
	     ok = w_item_R4(ch, item, BZ, 1, return_size)
	     item = 'WIND_MFI_BY(GSE)_R4'
	     ok = w_item_R4(ch, item, BY, 1, return_size)
	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BX, 1, return_size)
	     CALL DIFFFK(XRE,YRE,ZRE,BX,BY,BZ,DFK,XT,YT,ZT,XI,YI,ZI,IN)
	print*,'diff=',dfk
	PRINT*,'B,X,Y,Z',BX,BY,BZ
c	     IF(ABS(DFK).LT.999.) THEN
	       WRITE(TITLE(27),1037) DFK
c	     ELSE
c	       TITLE(27) = '      '
c	     ENDIF
 1037	FORMAT('Diff',F7.1)
	
C
	XSTART = 350.
	XEND = 2000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,400.,XEND,3100.)
	ENDIF
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = ' AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
	  TITLE(21) = 'FFT BANDPASS'
	  WRITE(TITLE(22),1019) FFTMIN,FFTMAX
 1019	  FORMAT(F5.1,'-',F6.0)
C
C	XTITLE = GX2 +.006*(GX2-GX1)
	XTITLE = GX2 +.02*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
C
	DO N = 1,5
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  IF(N.EQ.19) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
C
	TINC = .023*TRANGE
	CALL MGOSETEXPAND(.6)
	DO N = 6,27
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  IF(N.EQ.19) YTITLE = YTITLE - TINC
	  IF(N.EQ.23) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
C
C	PLOT TDS DATA IN TELEMETRY UNITS
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,2425.,XEND,3100.)
	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
C	     item = 'SLOW_RX_SPEED_R4'
C	     ok = w_item_R4(ch, item, SPS, 1, return_size)
	     item = 'WIND_SPIN_RATE_R4'
	     ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
C
c	on 16 nov 1999 it was found that the sunclock is read 1024
c	samples after the end of the event, plus about 10.6 msec or
c	about 14 spin clock counts for message passing time
c
	print*,'spinrate,sunclock,sps',spinrate,sunclock,sps
	END_ANGLE =  -360.*(SUNCLOCK-14.)/4096. - 45. ! ANGLE SUN TO +EX AT END
	IF(END_ANGLE.LT.-180.) END_ANGLE = END_ANGLE + 360.
	IF(END_ANGLE.GT.180.)  END_ANGLE = END_ANGLE - 360.
	DANG = SPINRATE*360./SPS/TWOPI
	ST_ANGLE = END_ANGLE + 3072.*DANG  ! ANGLE SUN TO +EX AT START 16nov99
	END_ANGLE = END_ANGLE + 1024.*DANG
c	ST_ANGLE = END_ANGLE + 2048.*DANG	  ! ANGLE SUN TO +EX AT START
	print*,'start angle, end angle, dang',st_angle,end_angle,dang
C
	  DO N = 1,2048
  	    PP(N) = 1000.*(N-1)/SPS
	    YY(N) = NDATA(N)
	    ANG(N) = ST_ANGLE - (N-1)*DANG
	if(yy(n).gt.ymax) then
	  ymax = yy(n)
	  angmax = ang(n)
	endif
	  ENDDO
	print*,'maxy,ang',ymax,angmax
C
	CALL MGOSETLIM(ANG(1),-128.,ANG(2048),128.)
	CALL MGOSETEXPAND(.8)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOCONNECT(ANG,YY,2048)
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(16,'ANGLE, SUN TO EX')
C	CALL MGOXLABEL(5,'mSEC.')
	CALL MGOYLABEL(14,'T/M NUMBER-128')
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO,COMBO')
	CALL MGOSETEXPAND(1.)
C
C	PLOT TDS DATA IN PHYSICAL UNITS
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1605.,XEND,2275.)
	ENDIF
	  YMAX = 0.
	  EFFLEN = 45.				  ! X ANTENNA  18-JUL-95
	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 6.    ! Y ANTENNA   "
	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 4.    ! Z ANTENNA   "
	  EFFLEN = 41.1				  ! X ANTENNA   9-NOV-96
	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 3.79  ! Y ANTENNA   "
	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 2.17  ! Z ANTENNA   "
	  ACORR = 1000.
	  IF(IRX.GE.7) THEN			! SEARCH COILS
		ACORR=1.
		EFFLEN = 1.
	  ENDIF
	print*,'effective length',efflen
	RMSSIG = 0.
C	CHANGE TO mV/meter
	  DO N = 1,2048
C 	    PP(N) = N
	    YY(N) = ACORR*DATA(N)/EFFLEN
c	write(88,*) n,irx,ndata(n),yy(n)
	write(88,*) n,irx,pp(n),yy(n)
	    RMSSIG = RMSSIG + YY(N)**2
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
	RMSSIG = SQRT(RMSSIG/2048.)
C
C
C
c	OPEN(UNIT=83,FILE='SIGNAL.DATA',STATUS='OLD',ACCESS='APPEND')
c	WRITE(83,200)S_SCET,TDS_CHANNEL,number_event,RMSSIG,YMAX
c 200	FORMAT(I12,I10,I5,I12,F9.3,F10.2)
c	CLOSE(UNIT=83)
C*********************************
c	IF(1) RETURN
C
	PRINT*,'MAX mV/m',YMAX
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOSETLIM(PP(1),-YMAX,PP(2048),YMAX)
C
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
C****
C	WRITE(66,*) 'PA',IRX
C	DO N = 1,500
C	  WRITE(66,*) PP(N),YY(N)
C	ENDDO
	CALL MGOCONNECT(PP,YY,2048)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(5,'mSEC.')
	IF(IRX.LE.6) THEN
	  CALL MGOYLABEL(4,'mV/m')
	ELSE
	  CALL MGOYLABEL(2,'nT')
	ENDIF
	CALL MGOSETEXPAND(1.)
C
C	PLOT FREQUENCY FROM ZERO CROSSINGS, ALSO SMOOTH FREQUENCY
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1120.,XEND,1455.)
	ENDIF

C
	SPSKHZ = .001*SPS
	YMIN = .5*SPSKHZ
	YMAX = 0.
	STD = 0.
	PRINT*,'IZCNT',IZCNT
	IF(IZCNT.GT.1) THEN
	  DO N = 1,IZCNT-1
	    IF(ZINT(N).EQ.0.) PRINT*,'IN PLOT, ZINT=0 AT',N
	    YY(N) = SPSKHZ/ZINT(N)
C 	      PP(N) = 1000.*(N-1)/SPS
	    PP(N) = 500.*(ZCROSS(N)+ZCROSS(N+1))/SPS
	    YMIN = AMIN1(YY(N),YMIN)
	    YMAX = AMAX1(YY(N),YMAX)
	    STD = STD + ZINT(N)**2
	  ENDDO
	  RMS = SQRT(STD/(IZCNT-1) - AVRPER**2)
	  IF(IZCNT.NE.0) RMS = SPSKHZ*(1./AVRPER - 1./(AVRPER+RMS))
C	
C	  SMOOTH FREQUENCY
C
	  YMIN = 0.
	  YMAX = 0.
	  DO N = 2,IZCNT-2
	    YYT(N) = .4*YY(N) + .3*(YY(N+1)+YY(N-1))
	    YMIN = AMIN1(YYT(N),YMIN)
	    YMAX = AMAX1(YYT(N),YMAX)
c*******
c	   if(pp(n).lt.32.) yyt(n) = 0.
c	   if(pp(n).gt.35.) yyt(n) = 0.
	  ENDDO
	  YYT(1) = YY(1)
	  YYT(IZCNT-1) = YY(IZCNT-1)
	ENDIF
C
  	YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
	PRINT*,'YMIN,YMAX',YMIN,YMAX
	PRINT*,'AVRFREQ,RMS',AVRFREQ,RMS
C	YMIN =  .9*AVRFREQ
C	YMAX = 1.1*AVRFREQ
c	ymin = 8.
c	ymax = 17.
C	YMIN = AVRFREQ - 3.*RMS
C	YMAX = AVRFREQ + 3.*RMS
	PRINT*,'SET TO   ',YMIN,YMAX
	CALL MGOSETEXPAND(.8)
	CALL MGOSETLIM(0.,YMIN,PP(2048),YMAX)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOCONNECT(PP,YYT,IZCNT-1)
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(5,'mSEC')
	CALL MGOYLABEL(10,'FREQ (kHZ)')
	CALL MGOSETEXPAND(1.)
C
C	PLOT FOURIER SPECTRUM
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,300.,XEND,970.)
	ENDIF
C
C
	N1 = 3
	N2 = 2048
	YMAX = -500.
	YMIN =  500.
	XMAX = 0.
	SPSUSE = SPS
	IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	DO N = N1,N2,2
	  NP = (N-1)/2
	  PP(NP) = NP*SPSUSE/2048.
C	  YY(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2)
	  YY(NP) = SPECT(NP+1)
c******
c	write(67,*) pp(np),spect(np),spect(np)-20.*alog10(efflen)
c******
	  XMAX = AMAX1(XMAX,PP(NP))
	  IF(YY(NP).GT.YMAX) THEN
	    NPSV = NP
	    YMAX = YY(NP)
	  ENDIF
	  YMIN = AMIN1(YMIN,YY(NP))
	ENDDO
C
C	CALCULATE 10 DB BANDWIDTH
C
	MAXCOUNT = 0
	LOWCOUNT = 0
	DO N=NPSV,1024
	   IF(SPECT(N).GT.(YMAX-10.)) THEN
		MAXCOUNT = MAXCOUNT+LOWCOUNT+1
		LOWCOUNT = 0
	   ELSE
		LOWCOUNT = LOWCOUNT+1
	   ENDIF
	   IF(LOWCOUNT.GT.3) GO TO 410
	ENDDO
C 410	MAXCOUNT = MAXCOUNT + LOWCOUNT
 410	CONTINUE
	LOWCOUNT = 0
	DO N=NPSV-1,1,-1
	   IF(SPECT(N).GT.(YMAX-10.)) THEN
		MAXCOUNT = MAXCOUNT+LOWCOUNT+1
		LOWCOUNT = 0
	   ELSE
		LOWCOUNT = LOWCOUNT+1
	   ENDIF
	   IF(LOWCOUNT.GT.3) GO TO 420
	ENDDO
 420	PRINT*,'DB,YMAX,YMIN',YMAX,YMIN
	BW = MAXCOUNT*SPSUSE/2048.
	PRINT*,'MAXCOUNT, BANDWIDTH',MAXCOUNT,BW
	XN1 = N1-2
	XN2 = N2+2
	YRANGE = YMAX-YMIN
	YMAX = YMAX + .02*YRANGE
	YMIN = YMIN - .05*YRANGE
	IF(IRX.LE.6) THEN
C	  YMIN = AMAX1(YMIN,YMAX-70.)
C	  YMIN = AMAX1(YMIN,-120.)
	  YMIN = -120.
	  IF(IRX.EQ.2) YMIN = -125.
	ELSE
	  YMIN = AMAX1(YMIN,-100.)
	ENDIF
	CALL MGOSETLIM(0.,YMIN,XMAX,YMAX)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NP)
C	CALL MGOPOINTS(60.,1,PP,YY,NP)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	IF(IRX.LE.6) THEN
	  CALL MGOYLABEL(17,'POWER, DB V\u2/HZ')
	ELSE
	  CALL MGOYLABEL(18,'POWER, DB nT\u2/HZ')
	ENDIF
	IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .1*TRANGE
	YTITLE = YTITLE-.3*TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,'   10 dB  ')
	YTITLE = YTITLE-TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,' BANDWIDTH')
	YTITLE = YTITLE-TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,'  OF PEAK ')
	IF(TDS_CHANNEL.GT.2) WRITE(STR,1021) .001*BW
	IF(TDS_CHANNEL.LE.2) WRITE(STR,1022) BW
 1021	FORMAT(F7.2,' HZ')
 1022	FORMAT(F6.3,' kHZ')
	YTITLE = YTITLE - TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,STR)
	CALL MGOSETEXPAND(1.)
C
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOSETEXPAND(.5)
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,' TRANSMITTED @')
C
	  YTITLE = YTITLE-.6*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  ITEM = 'EVENT_TM_SCET_I4'
	  ok = w_item_I4(ch, item, ERT, 2, return_size)
	  WRITE(STR,1029) ERT(1)
C 1029	   format(I4,'/',I2,'/',I2)
 1029	   format(I10)
	  CALL MGOLABEL(10,STR)
C
	  FDAY = BEGINEVT - DFLOAT(DDS)
          NHR = FDAY*24.
	  MIN = FDAY*1440. - (NHR*60)
	  SEC = FDAY*86400. - 3600.*NHR - 60.*MIN	  
C	     ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
	  WRITE(STR,1028) NHR,MIN,SEC
	print*,'beginevt',beginevt
	print*,'dds,fday,nhr,min',dds,fday,nhr,min
 1028	  FORMAT(1X,I2.2,I2.2,1X,F6.2)
	  YTITLE = YTITLE-.6*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,STR)
C
	  YTITLE = YTITLE-TINC
	  CALL MGOSETEXPAND(.5)
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(13,'S/C SUN CLOCK')
	  YTITLE = YTITLE-.6*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(STR,1027) SUNCLOCK*360./4096.
 1027	  FORMAT(F6.1' DEG.')
	  CALL MGOLABEL(10,STR)
C
	YTITLE = YTITLE-2.*TINC
	CALL MGOSETEXPAND(.5)
	IF(IPROCESS.GE.4) THEN
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(STR,1026) IFXB
 1026	  FORMAT(' BAD TDS',I2)
	  CALL MGOLABEL(10,STR)
	  YTITLE = YTITLE-.7*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(9,'CORRECTED')
	  YTITLE = YTITLE-.7*TINC
	ENDIF
C
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(STR,1024) IPROCESS
 1024	  FORMAT(' LEVEL',I2)
	  CALL MGOLABEL(8,STR)
C
	IF(IPROCESS.GE.4) THEN
	  WRITE(STR,1025) NBAD1 + NBAD2 + NBAD3
 1025	  FORMAT(I5,' PTS')
	  YTITLE = YTITLE-.6*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(9,STR)
	ELSE
	  YTITLE = YTITLE-5.*TINC
	ENDIF
	  CALL MGOSETEXPAND(.8)
C
	  CALL MGOSETEXPAND(.5)
	  YTITLE = YTITLE-1.5*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(STR,1030) MAJOR,MINOR
 1030	  FORMAT('MF.mf',I8,'.',I3.3)
	  CALL MGOLABEL(17,STR)
C
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
	SUBROUTINE PUBCOMBO(ITERM)
C
C	SAME AS COMBO EXCEPT NO RAW DATA
C	THE FIRST (TOP) PANEL IS THE DATA IN PHYSICAL UNITS, 
C	THE SECOND IS THE FREQ  FROM 
C	ZERO CROSSINGS, AND THE THIRD IS THE FOURIER TRANSFORM
C
	CHARACTER*12 TITLE(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 TDS_CHANNEL,S_SCET(2)
	INTEGER*4 NUMBER_EVENT,CH
	REAL*8 BEGINevt,endevt
	integer*4 dds
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PUB/ HDATA(4100)
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
	DIMENSION YY(4096),YYT(4096),PP(4096)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PUT LABELS ON RIGHT HAND SIDE
C
	XSTART = 350.
	XEND = 2000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,400.,XEND,2700.)
	ENDIF
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = ' AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
C	XTITLE = GX2 +.006*(GX2-GX1)
	XTITLE = GX2 +.02*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  IF(N.EQ.12) YTITLE = YTITLE + 2.*TINC
	  IF(N.EQ.19) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  IF(N.NE.13.AND.N.NE.14) CALL MGOLABEL(12,TITLE(N))
	ENDDO
C
C	IF(IPROCESS.EQ.3) THEN
C	  CALL MGOSETEXPAND(.6)
C	  YTITLE = YTITLE-2.*TINC
C	  CALL MGOGRELOCATE(XTITLE,YTITLE)
C	  CALL MGOLABEL(8,' BAD A/D')
C	  YTITLE = YTITLE-2.*TINC
C	  CALL MGOGRELOCATE(XTITLE,YTITLE)
C	  CALL MGOLABEL(9,'CORRECTED')
C	  CALL MGOSETEXPAND(.8)
C	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	  CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
c	CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO,PUBCOMBO')
	CALL MGOSETEXPAND(1.)
C
C	PLOT TDS DATA IN PHYSICAL UNITS
C
	     DO NN = 1,6
		HDATA(NN) = 0.
	     ENDDO	    
	     CALL REALFT(HDATA,2048,-1)
	     DO IK = 1,4096
	       HDATA(IK) = HDATA(IK)/1024.
C	       VDATA(IK) = DATA(IK)
	if(tds_channel.eq.2) write(66,*) ik,hdata(ik)
	     ENDDO
c
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1800.,XEND,2700.)
	ENDIF
	  YMAX = 0.
C	  EFFLEN = 45.				  ! X ANTENNA  18-JUL-95
C	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 6.    ! Y ANTENNA   "
C	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 4.    ! Z ANTENNA   "
	  EFFLEN = 41.1				  ! X ANTENNA   9-NOV-96
	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 3.79  ! Y ANTENNA   "
	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 2.17  ! Z ANTENNA   "
	  ACORR = 1000.
	  IF(IRX.GE.7) THEN			! SEARCH COILS
		ACORR=1.
		EFFLEN = 1.
	  ENDIF
C	CHANGE TO mV/meter
	  DO N = 1,4096
 	    PP(N) = 500.*(N-1)/SPS
C 	    PP(N) = N
	    YY(N) = ACORR*HDATA(N)/EFFLEN
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
C
c********
c	do n = 1,4096
c	  write(97,*) pp(n),yy(n)
c	enddo
c*****
	PRINT*,'MAX mV/m',YMAX
C	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOSETLIM(PP(1),-YMAX,PP(4096),YMAX)
C
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,4096)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(5,'mSEC.')
	IF(IRX.GE.7) THEN			! SEARCH COILS
	  CALL MGOYLABEL(2,'nT')
	ELSE
	  CALL MGOYLABEL(4,'mV/m')
	ENDIF
	CALL MGOSETEXPAND(1.)
C
C	PLOT FREQUENCY FROM ZERO CROSSINGS, ALSO SMOOTH FREQUENCY
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1250.,XEND,1595.)
	ENDIF

C
	SPSKHZ = .001*SPS
	YMIN = .5*SPSKHZ
	YMAX = 0.
	PRINT*,'IZCNT',IZCNT
	DO N = 1,IZCNT-1
	  IF(ZINT(N).EQ.0.) PRINT*,'IN PLOT, ZINT=0 AT',N
	  YY(N) = SPSKHZ/ZINT(N)
	  PP(N) = 500.*(ZCROSS(N)+ZCROSS(N+1))/SPS
	  YMIN = AMIN1(YY(N),YMIN)
	  YMAX = AMAX1(YY(N),YMAX)
	ENDDO
C	
C	SMOOTH FREQUENCY
C
	DO N = 2,IZCNT-2
	  YYT(N) = .4*YY(N) + .3*(YY(N+1)+YY(N-1))
	ENDDO
	YYT(1) = YY(1)
	IF(IZCNT.GT.1) YYT(IZCNT-1) = YY(IZCNT-1)
C
  	YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
	PRINT*,'YMIN,YMAX',YMIN,YMAX
	YMIN =  .93*AVRFREQ
	YMAX = 1.07*AVRFREQ
	YMIN =  .85*AVRFREQ
	YMAX = 1.15*AVRFREQ
C**********
C	YMIN =  0.
C	YMAX =  3.
C	YMIN =  18.
C	YMAX =  26.
C	YMIN =  12.
C	YMAX =  17.
	PRINT*,'SET TO   ',YMIN,YMAX
	CALL MGOSETEXPAND(.8)
	CALL MGOSETLIM(0.,YMIN,PP(4096),YMAX)
C	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOCONNECT(PP,YYT,IZCNT-1)
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(5,'mSEC.')
	CALL MGOYLABEL(10,'FREQ (kHZ)')
	CALL MGOSETEXPAND(1.)
C
C	PLOT FOURIER SPECTRUM
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,300.,XEND,1020.)
	ENDIF
C
C
	N1 = 3
	N2 = 2048
	YMAX = -500.
	YMIN =  500.
	XMAX = 0.
	SPSUSE = SPS
	IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	DO N = N1,N2,2
	  NP = (N-1)/2
	  PP(NP) = NP*SPSUSE/2048.
C	  YY(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2)
	  YY(NP) = SPECT(NP+1)
	  XMAX = AMAX1(XMAX,PP(NP))
	  IF(YY(NP).GT.YMAX) THEN
	    NPSV = NP
	    YMAX = YY(NP)
	  ENDIF
	  YMIN = AMIN1(YMIN,YY(NP))
	ENDDO
C
C	CALCULATE 10 DB BANDWIDTH
C
	MAXCOUNT = 0
	LOWCOUNT = 0
	DO N=NPSV,1024
	   IF(SPECT(N).GT.(YMAX-10.)) THEN
		MAXCOUNT = MAXCOUNT+LOWCOUNT+1
		LOWCOUNT = 0
	   ELSE
		LOWCOUNT = LOWCOUNT+1
	   ENDIF
	   IF(LOWCOUNT.GT.3) GO TO 410
	ENDDO
C 410	MAXCOUNT = MAXCOUNT + LOWCOUNT
 410	CONTINUE
	LOWCOUNT = 0
	DO N=NPSV-1,1,-1
	   IF(SPECT(N).GT.(YMAX-10.)) THEN
		MAXCOUNT = MAXCOUNT+LOWCOUNT+1
		LOWCOUNT = 0
	   ELSE
		LOWCOUNT = LOWCOUNT+1
	   ENDIF
	   IF(LOWCOUNT.GT.3) GO TO 420
	ENDDO
 420	PRINT*,'DB,YMAX,YMIN',YMAX,YMIN
	BW = MAXCOUNT*SPSUSE/2048.
	PRINT*,'MAXCOUNT, BANDWIDTH',MAXCOUNT,BW
	XN1 = N1-2
	XN2 = N2+2
	YRANGE = YMAX-YMIN
	YMAX = YMAX + .05*YRANGE
	YMIN = YMIN - .05*YRANGE
	YMIN = AMAX1(YMIN,YMAX-70.)
	CALL MGOSETLIM(-.017*XMAX,YMIN,XMAX,YMAX)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NP)
C	CALL MGOPOINTS(60.,1,PP,YY,NP)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	CALL MGOYLABEL(17,'POWER, DB V\u2/HZ')
	IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .1*TRANGE
	CALL MGOSETEXPAND(1.)
C
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
	SUBROUTINE PUBCOMBO2(ITERM)
C
C	SAME AS COMBO EXCEPT NO RAW DATA, AND NO FREQ FROM ZERO CROSSINGS
C	THE FIRST (TOP) PANEL IS THE DATA IN PHYSICAL UNITS, 
C	THE SECOND IS THE FOURIER TRANSFORM
C
	CHARACTER*12 TITLE(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 TDS_CHANNEL,S_SCET(2)
	INTEGER*4 NUMBER_EVENT,CH
	REAL*8 BEGINevt,endevt
	integer*4 dds
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PUB/ HDATA(4100)
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
	DIMENSION YY(4096),YYT(4096),PP(4096)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	CALL MGOSETLWEIGHT(2)
C
C	PUT LABELS ON RIGHT HAND SIDE
C
	XSTART = 350.
	XEND = 2000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,400.,XEND,2700.)
	ENDIF
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = ' AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
C	XTITLE = GX2 +.006*(GX2-GX1)
	XTITLE = GX2 +.02*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  IF(N.EQ.12) YTITLE = YTITLE + 2.*TINC
	  IF(N.EQ.19) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  IF(N.NE.13.AND.N.NE.14) CALL MGOLABEL(12,TITLE(N))
	ENDDO
C
C	CHANGE THIS TO FFT LIMITS
C	IF(IPROCESS.EQ.3) THEN
C	  CALL MGOSETEXPAND(.6)
C	  YTITLE = YTITLE-2.*TINC
C	  CALL MGOGRELOCATE(XTITLE,YTITLE)
C	  CALL MGOLABEL(8,' BAD A/D')
C	  YTITLE = YTITLE-2.*TINC
C	  CALL MGOGRELOCATE(XTITLE,YTITLE)
C	  CALL MGOLABEL(9,'CORRECTED')
C	  CALL MGOSETEXPAND(.8)
C	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	  CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
c	CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO,PUBCOMBO2')
	CALL MGOSETEXPAND(1.)
C
C	PLOT TDS DATA IN PHYSICAL UNITS
C
	     DO NN = 1,6
		HDATA(NN) = 0.
	     ENDDO	    
	     CALL REALFT(HDATA,2048,-1)
	     DO IK = 1,4096
	       HDATA(IK) = HDATA(IK)/1024.
C	       VDATA(IK) = DATA(IK)
	if(tds_channel.eq.2) write(66,*) ik,hdata(ik)
	     ENDDO
c
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1850.,XEND,2650.)
	ENDIF
	  YMAX = 0.
C	  EFFLEN = 45.				  ! X ANTENNA  18-JUL-95
C	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 6.    ! Y ANTENNA   "
C	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 4.    ! Z ANTENNA   "
	  EFFLEN = 41.1				  ! X ANTENNA   9-NOV-96
	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 3.79  ! Y ANTENNA   "
	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 2.17  ! Z ANTENNA   "
	  ACORR = 1000.
	  IF(IRX.GE.7) THEN			! SEARCH COILS
		ACORR=1.
		EFFLEN = 1.
	  ENDIF
C	CHANGE TO mV/meter
	  DO N = 1,4096
 	    PP(N) = 500.*(N-1)/SPS
C 	    PP(N) = N
	    YY(N) = ACORR*HDATA(N)/EFFLEN
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
C
	PRINT*,'MAX mV/m',YMAX
C	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOSETLIM(PP(1),-YMAX,PP(4096),YMAX)
C
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,4096)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(5,'mSEC.')
	IF(IRX.GE.7) THEN			! SEARCH COILS
	  CALL MGOYLABEL(2,'nT')
	ELSE
	  CALL MGOYLABEL(4,'mV/m')
	ENDIF
	CALL MGOSETEXPAND(1.)
C
C	PLOT FOURIER SPECTRUM
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1100.,XEND,1700.)
	ENDIF
C
C
	N1 = 3
	N2 = 2048
	YMAX = -500.
	YMIN =  500.
	XMAX = 0.
	SPSUSE = SPS
	IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	DO N = N1,N2,2
	  NP = (N-1)/2
	  PP(NP) = NP*SPSUSE/2048.
C	  YY(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2)
	  YY(NP) = SPECT(NP+1)
	  XMAX = AMAX1(XMAX,PP(NP))
	  IF(YY(NP).GT.YMAX) THEN
	    NPSV = NP
	    YMAX = YY(NP)
	  ENDIF
	  YMIN = AMIN1(YMIN,YY(NP))
	ENDDO
C
	XN1 = N1-2
	XN2 = N2+2
	YRANGE = YMAX-YMIN
	YMAX = YMAX + .05*YRANGE
	YMIN = YMIN - .05*YRANGE
	YMIN = AMAX1(YMIN,YMAX-70.)
c*******
C	CALL MGOSETLIM(-.017*XMAX,YMIN,XMAX,YMAX)
	CALL MGOSETLIM(-.017*XMAX,YMIN,15.,YMAX)
c**********
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NP)
C	CALL MGOPOINTS(60.,1,PP,YY,NP)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	CALL MGOYLABEL(17,'POWER, DB V\u2/HZ')
	IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .1*TRANGE
	CALL MGOSETEXPAND(1.)
C
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
	SUBROUTINE DETAIL(ITERM,N1,N2)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 TITLE(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 NUMBER_EVENT,CH
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
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
	DIMENSION YY(2048),PP(2048)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PLOT TDS DATA
C
	XEND = 2750.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(300.,400.,XEND,2230.)
	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
c	  WRITE(STR,704) SAA()
c 704	  FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c	  CALL MGOPUTLABEL(55,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	DO N = N1,N2
	  NP = N
	  PP(N) = N
	  YY(N) = NDATA(N)
	ENDDO
C
	CALL MGOTICKSIZE(0.,0.,5.6,28.)  
	XN1 = N1-2
	XN2 = N2+2
	CALL MGOSETLIM(XN1,-130.,XN2,130.)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NP)
	CALL MGOPOINTS(60.,1,PP,YY,NP)
	CALL MGOSETEXPAND(.8)
	  CALL MGOBOX(1,2)
	  CALL MGOYLABEL(14,'T/M NUMBER-128')
	  CALL MGOSETEXPAND(1.)
	TRANGE = GY2-GY1
	TINC = .04*TRANGE
	XTITLE = GX2 +.005*(GX2-GX1)
	YTITLE = GY2
	CALL MGOSETEXPAND(.8)
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = 'AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO')
	CALL MGOSETEXPAND(1.)
C
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
	SUBROUTINE PARTSPEC(NSTART,NVAR,NSPECT,DATA)
C
C	THIS ROUTINE DOES FOURIER ANALYSIS ON  OF AN EVENT.  
C	NSTART IS THE SAMPLE NUMBER OF THE FIRST SAMPLE, NVAR IS THE
C	NUMBER OF SAMPLES TO BE FOURIER ANALYSED.  NVAR MUST BE A POWER
C	OF TWO.  DATA IS THE DATA FROM THE EVENT, IN VOLTS BUT UNCORRECTED 
C	FOR FREQUENCY RESPONSE, ETC. 
C
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	INTEGER*4 NUMBER_EVENT,CH
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR,PREAMP
	character*32	item
	REAL DATA(2050),SPECT(2050),PDATA(2050)
	CHARACTER*12 TITLE(25)
	CHARACTER*20 STR
	DIMENSION YY(2048),PP(2048)
C
	ITERM = -1
C	ITERM = 3
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	IF(ITERM.LT.0) THEN
c	  CALL MGOSETLOC(500.,400.,2000.,3100.)
	  CALL MGOSETLOC(350.,400.,2000.,2800.)  	! TO FIT PUBCOMBO
	ELSE
	  CALL MGOSETLOC(75.,150.,900.,700.)
	ENDIF
	TRANGE = GY2-GY1
	TINC = .04*TRANGE
	XTITLE = GX2 +.03*(GX2-GX1)
	YTITLE = GY2
	CALL MGOSETEXPAND(.8)
	AVRFREQ=0.
C	IF(IZCNT.GT.1) THEN
C	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
C	  AVRFREQ = .001*SPS/AVRPER
C	ENDIF
	TITLE(19) = 'AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	YTITLE = GY1 + .3*(GY2-GY1)
	CALL MGOGRELOCATE(GX1-.12*GX2,YTITLE)
	CALL MGOSETANGLE(90.)
	CALL MGOLABEL(17,'POWER, DB V\u2/HZ')
	CALL MGOSETANGLE(0.)
C
	IF(ITERM.LT.0) THEN
	  IF(NSPECT.EQ.1) CALL MGOSETLOC(500.,1300.,2000.,2200.)
	  IF(NSPECT.EQ.2) CALL MGOSETLOC(500.,850.,2000.,2650.)
	  IF(NSPECT.GT.2) CALL MGOSETLOC(500.,400.,2000.,3100.)
	  CALL MGOSETLOC(350.,400.,2000.,1120.)  	! TO FIT PUBCOMBO
	ELSE
	  CALL MGOSETLOC(75.,150.,900.,700.)
	ENDIF
	NHALF = NVAR/2
	HALF = NHALF
	DO NS = 1,NSPECT
	  NSTT = NSTART + (NS-1)*NVAR
	  NP = 0
	  DO N = NSTT, NSTT+NVAR-1
	    NP = NP+1
	    PDATA(NP) = DATA(N)
  	  ENDDO
	  CALL REALFT(PDATA,NHALF,1)
C
C	  CORRECT FOURIER ANALYSED SIGNAL FOR FREQUENCY RESPONSE
C
	  IF(TDS_CHANNEL.GT.2) THEN
	   item = 'SLOW_RX_FILTER'
	  ELSE
	   item = 'FAST_RX_FILTER'
	  ENDIF
	   ok = W_ITEM_i4(ch, item, ifil, 1, return_size)
C
	     DO IK = 3,NVAR,2
		FCOEF = CMPLX(PDATA(IK),PDATA(IK+1))
	        FREQ = SPS*(IK-1)/4096.
	        CGAIN = PREAMP(IRX,FREQ)
	if(ik.eq.5) print*,'ik,irx,freq,cgain',ik,irx,freq,cgain
		FCOEFT = CMPLX(0.,0.)
		FCOEFT = FCOEF/CONJG(CGAIN)
		CCORR = CGAIN
		CGAIN = TDSDET(TDS_CHANNEL,FREQ)
		FCOEFT = FCOEFT/CONJG(CGAIN)
		CCORR = CGAIN*CCORR
		CGAIN = TDS_FILTER(TDS_CHANNEL,IFIL+1,FREQ)
		FCOEFT = FCOEFT/CONJG(CGAIN)
		CCORR = CGAIN*CCORR
		FCOEF = FCOEFT
		PDATA(IK) = FCOEF
		PDATA(IK+1) = AIMAG(FCOEF)
	     ENDDO
C
C	  NOW PDATA CONTAINS FOURIER COEFFS CORRECTED FOR FREQUENCY RESPONSE 
C
C	  CALCULATE SPECTRUM, IN DB WRT 1 V**2/HZ
C
	  SNORM = 20.*ALOG10(HALF)
	  IK = 1
	  ISP = 1
	  SPECT(ISP) = -SNORM
	  IF(PDATA(IK).NE.0.)SPECT(ISP) = 20.*ALOG10(ABS(PDATA(IK)))-SNORM
	  DO IK = 3,NVAR,2
	      ISP = ISP+1
	      SPECT(ISP) = 10.*ALOG10(PDATA(IK)**2 + PDATA(IK+1)**2)-SNORM
	  ENDDO
C
C
C	  PLOT FOURIER SPECTRUM
C
	print*,'call mgowindow, 1,',nspect,ns
	
	  IF(NS.GT.1)  THEN
	    CALL MGOWINDOW(1,NSPECT,NS)
	  ELSE
	    CALL MGOSETLOC(500.,1300.,2000.,2200.)
	  CALL MGOSETLOC(350.,400.,2000.,1120.)  	! TO FIT PUBCOMBO
	  ENDIF
C
C
	  N1 = 3
	  N2 = NVAR
	  YMAX = -500.
	  YMIN =  500.
	  XMAX = 0.
	  SPSUSE = SPS
	  IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	  DO N = N1,N2,2
	    NP = (N-1)/2
	    PP(NP) = NP*SPSUSE/NVAR
	    YY(NP) = SPECT(NP+1) 
	    XMAX = AMAX1(XMAX,PP(NP))
	    IF(YY(NP).GT.YMAX) THEN
	      NPSV = NP
	      YMAX = YY(NP)
	    ENDIF
	    YMIN = AMIN1(YMIN,YY(NP))
	  ENDDO
C
	  XN1 = N1-2
	  XN2 = N2+2
	  YRANGE = YMAX-YMIN
	  YMAX = YMAX + .05*YRANGE
	  YMIN = YMIN - .05*YRANGE
	  YMIN = AMAX1(YMIN,YMAX-80.)
	  PRINT*,'SET YLIMITS',YMIN,YMAX
	  CALL MGOSETLIM(0.,YMIN,XMAX,YMAX)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
C	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(PP,YY,NP)
C	  CALL MGOPOINTS(60.,1,PP,YY,NP)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOBOX(1,2)
	print 1017, nstt,nstt+nvar-1
	WRITE(STR,1017)  NSTT, NSTT+NVAR-1
 1017	  FORMAT('SAMPLES',I5,' TO',I5)
	  CALL MGOYLABEL(20,STR)
	  IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	  IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
C	  YTITLE = GY2
C	  TRANGE = GY2-GY1
C	  TINC = .1*TRANGE
C	  YTITLE = YTITLE-TINC
C	  CALL MGOGRELOCATE(XTITLE,YTITLE)
C	  CALL MGOLABEL(10,'   10 dB  ')
C
	ENDDO
C
	CALL MGOPLOTID('PARTSPEC','[.WIND]TDSPRO')
	CALL MGOSETEXPAND(1.)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	  STOP
	ENDIF
C
	RETURN
	END
	SUBROUTINE SPPLOT(ITERM)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 TITLE(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 NUMBER_EVENT
	REAL*8 BEGINevt,endevt
	integer*4 dds,CH,S_SCET(2)
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT,CH
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
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
	DIMENSION YY(2048),PP(2048)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PLOT TDS DATA
C
	XEND = 2750.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(300.,400.,XEND,2230.)
	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C
	  CALL MGOSETEXPAND(1.)
C
	N1 = 3
	N2 = 2048
	YMAX = -500.
	YMIN =  500.
	XMAX = 0.
	SPSUSE = SPS
	IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	DO N = N1,N2,2
	  NP = (N-1)/2
	  PP(NP) = NP*SPSUSE/2048.
	  YY(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2)
	  XMAX = AMAX1(XMAX,PP(NP))
	  YMAX = AMAX1(YMAX,YY(NP))
	  YMIN = AMIN1(YMIN,YY(NP))
	ENDDO
C
	PRINT*,'YMAX,YMIN',YMAX,YMIN
C	CALL MGOTICKSIZE(10.,10.,5.6,28.)  
	XN1 = N1-2
	XN2 = N2+2
	YRANGE = YMAX-YMIN
	YMAX = YMAX + .05*YRANGE
	YMIN = YMIN - .05*YRANGE
	CALL MGOSETLIM(0.,YMIN,XMAX,YMAX)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NP)
	CALL MGOPOINTS(60.,1,PP,YY,NP)
	CALL MGOSETEXPAND(.8)
	CALL MGOBOX(1,2)
	CALL MGOYLABEL(14,'POWER, DB')
	IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
	CALL MGOSETEXPAND(1.)
	TRANGE = GY2-GY1
	TINC = .04*TRANGE
	XTITLE = GX2 +.005*(GX2-GX1)
	YTITLE = GY2
	CALL MGOSETEXPAND(.8)
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = 'AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO,SPPLOT')
	CALL MGOSETEXPAND(1.)
C
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
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	integer*4	iq,NREM,NHRMN,IFASTSLOW

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
