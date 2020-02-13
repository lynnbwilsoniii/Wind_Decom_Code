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
	integer*4	major, minor,ihist,nhist(256,5),npos(5),neg(5)
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
C	LINES IN PLOTMAX   
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
	WRITE(96,*) FILE
	write(96,*) 'LIST OF POSSIBLE SIMULTANEOUS EVENTS'
	write(96,*) 'HI evt no.   hi hrs    lo evt no. lo hrs'
     1  ,'  hi t/m  lo t/m  del DPU'
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
	STOP
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
