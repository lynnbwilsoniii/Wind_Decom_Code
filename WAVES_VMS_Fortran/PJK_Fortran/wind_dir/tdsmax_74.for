 	PROGRAM TDSMAX
C
C	PLOTS TDS MAXIMA, I.E. ITEMS "TDS_INT_MAX_CH1" TO ... _CH6
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok,statok
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6)
	character*80	stream
	character*4	pa(6,4)
	parameter	size=2048
	integer*4	return_size
	integer*4	tds_channel,ifilf,ifils,ifsps,issps
	integer*4	temp_waves
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scethk
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss
	real 		ffilter(4),sfilter(4),fsps(4),ssps(4),sps
!
	common /nrblk/ nrem,NHRMN
	COMMON /HEADBL/ PTITLE,PTITLE2,FILE
	COMMON /STATUS/ ifilf,ifils,ifsps,issps,ichpa
C	common /headblk/ major,minor,tds_channel,s_scet,file

C
	CHARACTER*12 PTITLE(20),PTITLE2(20)
	INTEGER*4 HKCH
	COMMON /PLTBLK/ TDSHI(2000,2),TDSLO(2000,4),HRHI(2000),HRLO(2000)
     1		,NPTH,NPTL
	COMMON /PLOTDT/ SPS
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
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = 'TIME DOMAIN'
	PTITLE(3) = 'SAMPLER'
	PTITLE(4) = 'INT_MAX'
	PTITLE(7) = 'SAMP.RATE'
	PTITLE(10) = 'L.P.FIL.'
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
        if(stream.eq.'offline') then
 10	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
	endif
c
  5	format(q,a)
  3	format(q,i10)
c

	ok = wind_tm_open_channel(HKCH,stream)
	if (.not. ok) stop 'Cannot open HK channel'
	scethk = 0.
	call w_channel_position(HKCH,scethk)
	print*,' file starts at scet',scethk
	dd = scethk
	scethk = float(dd) + hh/24. + mm/1440.
	print*,'set channel position to',scethk
	call w_channel_position(HKCH,scethk)
	print*,'channel position set to',scethk

	ok = w_channel_filename(HKCH,file)
	print*,file

	ok = wind_tm_get_mfmf(HKCH,major,minor)
	if (.not. ok) stop 'cannot get Mfmf stream position'
	print*,' after mfmf, at Mfmf=',major,minor
c
	get_tm_stream = 1
c
	CALL GETSTATUS(HKCH,MAJOR,MINOR,STATOK)
c
           ok = wind_tm_get_next_event(HKCH,major,minor,'HK')
c	   item = 'TEMP_WAVES2'
c	   ok = wind_tm_get_item(HKCH, item, temp_waves, 1, return_size)
C
C	IF OFFLINE, FIND DESIRED TIME
C
c        if(stream.eq.'offline') then
c	   dowhile(ihrmn.lt.nhrmn)
c            ok = wind_tm_get_next_event(HKCH,major,minor,'TDSF')
c	     item = 'EVENT_SCET_R8'
c	     ok = w_item_r8(HKCH, item, scet, 2, return_size)
c	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
c	     ihrmn = 100*hh+mm
c	     item = 'EVENT_NUMBER'
c	     ok = wind_tm_get_item(HKCH, item, itemp, 1, return_size)
c	     TYPE 1005,itemp,YYYY,MON,DD,IHRMN
C		write(26,*) itemp,YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)
c	  enddo
c	endif

	type*,'going to get first desired event'
	go to 100
c
C
C	GET NEXT EVENT
C
 110    continue

!	 this is the main program loop

	if(wind_tm_eof(hkch,major,minor)) then
	  type*,'call pltmax at eof, just after 110'
	  call pltmax(-2)
	  stop
	endif

	ok = wind_tm_get_next_event(HKCH,major,minor,'HK')
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
c	      type *, 'Cannot get event at MF.mf: ', major, minor
C	      if (wind_tm_realtime(ch)) then
C	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
C	         type *, '-reset to latest position: ', major, minor
C	      else
C	         call wind_tm_increment_packet(major,minor)
C	         type *, '-incrementing packet...'
C	      end if
	   end if
C
 100	  CONTINUE
	     SCET =SCETHK
C*********
C	     item = 'EVENT_SCET_R8'
C	     ok = w_item_r8(HKCH, item, scet, 2, return_size)
	     item = 'EVENT_SCET'
	     ok = w_item_i4(HKCH, item,s_scet, 2, return_size)
C	     type*,'s_scet',s_scet 
	     SS = MOD(S_SCET(2),100)
	     MM = S_SCET(2)/100
	     MM = MOD(MM,100)
	     HH = S_SCET(2)/10000
	     SCET = DFLOAT(DD) + HH/24. + MM/1440. + SS/86400.
C	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
C	     call w_ur8_to_string(scet,PTITLE(16),PTITLE(18))
C	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
C	     call w_ur8_to_ydoy(scet,yyyy,doy,msday)
C	     ihrmn = 100*hh+mm
C	     TYPE 1005,itemp,YYYY,MON,DD,IHRMN
C		write(26,*) itemp,YYYY,MON,DD,IHRMN
c 1016	   format(I5,'/',I2,'/',I2)
	   WRITE(PTITLE(17),1017) doy,ihrmn
 1017	   FORMAT('doy ',I3,I5.4)
 1019	   FORMAT('CHANNEL',I2)
 1012	   FORMAT(I10)
C
C	   WRITE(PTITLE(2),1009)  TDS_CHANNEL
C	   ipa = ichpa(tds_channel)
C	   WRITE(PTITLE(7),1007) pa(tds_channel,ipa+1)
 1007	   FORMAT('P/A ',A4)
	   IF(TDS_CHANNEL.LE.2) THEN
	      WRITE(PTITLE(9),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(11),1008) FFILTER(IFILF+1)
	   ELSE
	      WRITE(PTITLE(9),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	   ENDIF
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
	ITEM = 'TDS_INT_MAX_CH1'
	OK = WIND_TM_GET_ITEM(HKCH,ITEM,ITEMP,1,RETURN_SIZE)
	IF(OK) THEN
	  NPTH = NPTH+1
C******
 	HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
	  TDSHI(NPTH,1) = IABS(ITEMP-128)
C	print*,item,s_scet(2),itemp,tdshi(npth,1)
	  ITEM = 'TDS_INT_MAX_CH2'
	  OK = WIND_TM_GET_ITEM(HKCH,ITEM,ITEMP,1,RETURN_SIZE)
	  TDSHI(NPTH,2) = IABS(ITEMP-128)
	ENDIF
	ITEM = 'TDS_INT_MAX_CH3'
	OK = WIND_TM_GET_ITEM(HKCH,ITEM,ITEMP,1,RETURN_SIZE)
	IF(OK) THEN
	  NPTL = NPTL+1
C******
 	HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
	  TDSLO(NPTL,1) = IABS(ITEMP-128)
	  ITEM = 'TDS_INT_MAX_CH4'
	  OK = WIND_TM_GET_ITEM(HKCH,ITEM,ITEMP,1,RETURN_SIZE)
	  TDSLO(NPTL,2) = IABS(ITEMP-128)
	  ITEM = 'TDS_INT_MAX_CH5'
	  OK = WIND_TM_GET_ITEM(HKCH,ITEM,ITEMP,1,RETURN_SIZE)
	  TDSLO(NPTL,3) = IABS(ITEMP-128)
	  ITEM = 'TDS_INT_MAX_CH6'
	  OK = WIND_TM_GET_ITEM(HKCH,ITEM,ITEMP,1,RETURN_SIZE)
	  TDSLO(NPTL,4) = IABS(ITEMP-128)
	ENDIF
	
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	if(npth.lt.1990) go to 110
c	if(npth.lt.200) go to 110
C
	CALL GETSTATUS(HKCH,MAJOR,MINOR,STATOK)
	type*,'call pltmax at npth limit, npth=',npth
	CALL PLTMAX(-2)
	STOP
	END
	SUBROUTINE PLTMAX(ITERM)
C
C	PLOT TDS INTERVAL MAXIMA FOR 6 CHANNELS
C
	CHARACTER*12 TITLE1(20),TITLE2(20)
	CHARACTER*120 STR
	character*4	pa(6,4)
	character*80	file
	COMMON /HEADBL/ TITLE1,TITLE2,FILE
	COMMON /PLTBLK/ TDSHI(2000,2),TDSLO(2000,4),HRHI(2000),HRLO(2000)
     1		,NPTH,NPTL
	COMMON /PLOTDT/ SPS
	REAL FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /STATUS/ ifilf,ifils,ifsps,issps,ichpa
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
	INTEGER*4 ITERM,ICHPA(6)
	DIMENSION YY(2048)
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.875/
	DATA SSPS /7500.,1875.,468.75,117.2/
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
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
	TSTART = AMIN1(HRHI(1),HRLO(1))
	TEND = AMAX1(HRHI(NPTH),HRLO(NPTL))
	PRINT*,'PLTMAX CALLED, TIMES',TSTART,TEND

	XEND = 3000.

	DO NW = 1,4
C
C	  CHANNEL 6 GOES AT THE BOTTOM
C
	  NCH = 7-NW
C
C	  PLOT LOW FREQUENCY CHANNELS
C
	  YLOW = YBOT + (NW-1)*PRANGE
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
C	    CALL MGOPUTLABEL(53,STR,9)
c	    WRITE(STR,704) SAA()
c 704	    FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c	    CALL MGOPUTLABEL(55,STR,9)
	    CALL MGOSETEXPAND(1.)
C
	  DO N = 1,NPTL
	    YY(N) = TDSLO(N,NCH-2)
	  ENDDO
C
C	  CALL MGOTICKSIZE(0.,0.,5.6,28.)  
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
  	  CALL MGOSETLIM(TSTART,0.,TEND,130.)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(HRLO,YY,NPTL)
	  CALL MGOSETEXPAND(.5)
	  IF(NW.EQ.1) THEN
	     CALL MGOXLABEL(16,'HOURS OF THE DAY')
	     CALL MGOBOX(1,2)
	  ELSE
	     CALL MGOBOX(0,2)
	  ENDIF
	  CALL MGOSETEXPAND(.5)
	  CALL MGOYLABEL(7,'T/M NO.')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .12*TRANGE
C	  XTITLE = GX2 +.02*(GX2-GX1)
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2
	  CALL MGOSETEXPAND(.5)
	  INPUT = ICHPA(NCH)
	  TITLE1(6) = PA(NCH,INPUT+1)
	  WRITE(TITLE1(8),1015) SSPS(ISSPS+1)
 1015	  FORMAT(F7.2)
	  TITLE1(9) = '  SPS'
	  WRITE(TITLE1(11),1011) SFILTER(IFILS+1)
 1011	  FORMAT(F6.0)
	  TITLE1(12) = '  HZ'
	  IF(NW.EQ.4) THEN
	    DO N = 6,12
	      YTITLE = YTITLE - TINC
	      IF(N.EQ.7) YTITLE = YTITLE - TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(10,TITLE1(N))
	    ENDDO
	    CALL MGOSETEXPAND(1.)
	  ELSE
	      YTITLE = YTITLE - 3.*TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(10,TITLE1(6))
	  ENDIF
	 ENDDO
C

	DO NW = 1,2
C
C	  CHANNEL 1 GOES AT THE TOP
C
	  NCH = 3-NW
C
C	  PLOT HI FREQUENCY CHANNELS
C
	  YLOW = YBOT + (NW+3)*PRANGE
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
	  DO N = 1,NPTL
	    YY(N) = TDSHI(N,NCH)
	  ENDDO
C
C	  CALL MGOTICKSIZE(0.,0.,5.6,28.)  
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
  	  CALL MGOSETLIM(TSTART,0.,TEND,130.)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(HRLO,YY,NPTL)
	  CALL MGOSETEXPAND(.5)
	  CALL MGOBOX(0,2)
	  CALL MGOYLABEL(10,'T/M NO.')
	  CALL MGOSETEXPAND(1.)
	  TRANGE = GY2-GY1
	  TINC = .1*TRANGE
c	  XTITLE = GX2 +.005*(GX2-GX1)
	  XTITLE = .02*(GX2-GX1)
	  YTITLE = GY2
	  CALL MGOSETEXPAND(.5)
	  INPUT = ICHPA(NCH)
	  TITLE1(6) = PA(NCH,INPUT+1)
	  WRITE(TITLE1(8),1015) FSPS(IFSPS+1)
	  TITLE1(9) = '  kSPS'
	  TITLE1(10) = 'L.P.FIL.'
	  WRITE(TITLE1(11),1011) FFILTER(IFILF+1)
	  TITLE1(12) = '  HZ'
	  IF(NW.EQ.2) THEN
	    DO N = 6,12
	      YTITLE = YTITLE - TINC
	      IF(N.EQ.7) YTITLE = YTITLE - TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(12,TITLE1(N))
	    ENDDO
	  ELSE
	      YTITLE = YTITLE - 3.*TINC
	      CALL MGOGRELOCATE(XTITLE,YTITLE)
	      CALL MGOLABEL(12,TITLE1(6))
	  ENDIF
	  CALL MGOSETEXPAND(1.)
	  CALL MGOSETEXPAND(.8)
	  IF(NW.EQ.2) CALL MGOPLOTID(TITLE1(4),'[KELLOGG.WIND]TDSMAX')
	  CALL MGOSETEXPAND(1.)
	 ENDDO
	  CALL MGOSETEXPAND(.6)
	 CALL MGOGRELOCATE(GX1,GY2)
	 CALL MGOPUTLABEL(50,FILE,9)
	  CALL MGOSETEXPAND(1.)
C
	PRINT*,'TERMINAL',ITERM
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  NPLOTS = NPLOTS + 1
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
	COMMON /STATUS/ ifilf,ifils,ifsps,issps,ichpa
C	
           ok = wind_tm_get_next_event(CH,major,minor,'TDSF')
	   statok = ok
	   item = 'SLOW_RX_SPEED'
	   ok = wind_tm_get_item(CH, item, issps, 1, return_size)
	   type*,'slow rx speed',issps
	   item = 'FAST_RX_SPEED'
	   ok = wind_tm_get_item(CH, item, ifsps, 1, return_size)
	   type*,'fast rx speed',ifsps
	   item = 'SLOW_RX_FILTER'
	   ok = wind_tm_get_item(CH, item, ifils, 1, return_size)
	   type*,'slow rx filter',ifils
	   item = 'FAST_RX_FILTER'
	   ok = wind_tm_get_item(CH, item, ifilf, 1, return_size)
	   type*,'fast rx filter',ifilf
	   item = 'SOURCE_CHAN_1'
	   ok = wind_tm_get_item(CH, item, ichpa(1), 1, return_size)
	   item = 'SOURCE_CHAN_2'
	   ok = wind_tm_get_item(CH, item, ichpa(2), 1, return_size)
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
