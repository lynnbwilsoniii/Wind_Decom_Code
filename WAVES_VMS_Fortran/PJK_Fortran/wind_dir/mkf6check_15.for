	PROGRAM mkf6check
C
C	PLOTS TDS AND FREQUENCY ON SCREEN, USING INPUT FROM MAKEFILE6
C		RESULTS
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6),ipacal(6,4)
	character*80	stream
	character*4	event
	character*4	pa(6,4),parx(9)
	character*1	DISPOSE
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
	common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds

C
	CHARACTER*12 PTITLE(25)
	INTEGER*4 TDSCH,hkch,fillch,ch,NUMBER_EVENT
	INTEGER*4 YYYYMMDD,NHRMNSS,NFDAY
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /HEADBL/ PTITLE,EVENT,NUMBER_EVENT
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
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
	DATA IREADIN /0/
C
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = 'TIME DOMAIN'
	PTITLE(3) = 'SAMPLER'
	PTITLE(4) = 'EVENT NO.'
	PTITLE(8) = 'SAMPLE RATE'
	PTITLE(10) = 'L.P.FILTER'
C	PTITLE(12) = 'TRIGGERS'
	PTITLE(14) = 'DPU CLOCK'
	PTITLE(16) = 'SCET'
	PRINT*,' '
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
C	GET STARTED
C
	OPEN(UNIT=90,FILE='MAKEFILE6.FOR090',STATUS='OLD',READONLY)
C
	IF(IREADIN.EQ.0) THEN
	  DO II = 1,10
	    READ(90,5) JUNK
	  ENDDO
	    PRINT*,JUNK
	  IREADIN = 1
	ENDIF
C
10	READ(90,1011) YYYYMMDD,NHRMNSS,NFDAY,JUNK,NUMBER_EVENT
 1011   format(I10,I7,I3,A2,I10,2E11.3,F6.3,F6.3,F7.3,I5,F6.1,F6.1,
     1    F7.1,F6.1,F6.3,F7.3)
	   DD = MOD(YYYYMMDD,100)
	   NHRMN = NHRMNSS/100
	   PRINT*,YYYYMMDD
	   nevent = number_event
	   WRITE(STREAM,30) YYYYMMDD/100,NFDAY
c 30	   FORMAT('wi_lz_wav_',A8,'_v*.dat')
 30	   FORMAT('wi_lz_wav_',I6.6,I2.2,'_v*.dat')
	print*,'in main, initial stream=  ',stream
C
C	ok = get_stream_name(stream)
C	if (.not. ok) stop 'no file supplied.'
C
C	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  IF(NFDAY.EQ.DD) THEN
	    HH = NHRMN/100
	    MM = MOD(NHRMN,100)
	  ELSE
	    HH = 0
	    MM = 0
	  ENDIF
c	  write(6,4)
	  ifastslow = 1
c	  read(5,*,err=10,end=20)  ifastslow
c	  read(5,*,err=10,end=20)  iend,n2
c	  type*,iend,n2
	iend = 2
c	    if(iend.eq.1) then
c		nrem = n2
c		nrem = 2000
c	    endif
c	    if(iend.eq.2) then
		nrem = 2
		if(ifastslow.eq.1) nrem = 4
c		nevent = n2
c	    endif
c	endif
C	type*,'type desired process level,0=raw,1=raw volts,2=fft,fft inv'
C	type*,' or 3,4 fft,fft inv and correct bad tds numbers'
	iprocess = 4
c	read(5,3) iq,iprocess
	type*,' '
	if(iprocess.eq.0) type*,'ok, plot data in tm numbers - 128'
	if(iprocess.eq.1) type*,'ok, plot in volts, unity freq. response.'
	if(iprocess.ge.2) type*,'ok, plot volts, corrected for freq. response.'
	if(iprocess.eq.3) type*,'ok, plot volts, corrected for bad TDS data'
	type*,' '
c
  6	format(1x,'for a specific event,type 2, event number')
  7	format(1x,'type 1,anything, to do all events after start time')
  5	format(q,a)
  4	format(1x,'type 0 for TDSF (fast), 1 FOR TDSS (slow)')
  3	format(q,i10)
c
	ok = w_channel_open(tdsch,stream)
	PRINT*,'STREAM AFTER OPEN',STREAM
	if (.not. ok) stop 'Cannot open tds channel'
	scettds = 0.
	call w_channel_position(tdsch,scettds)
	print*,'tds file starts at scettds',scettds
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set tds channel position to',scettds
	call w_channel_position(tdsch,scettds)
	print*,'tds channel position set to',scettds

	ok = w_channel_filename(tdsch,file)
	stream = file
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
	if(ok.eq.82) stop 'end of file on tdsch'
	if (.not. ok) stop 'cannot get fill event'
	item = 'EVENT_SCET'
	  ok = w_item_i4(fillch, item, s_scet, 2, return_size)
	print*,'initial fillch time',s_scet
	  
c
	ok = w_channel_filename(tdsch,file)
	write(87,*) file
	print*,file
c
	get_tm_stream = 1
c
c           ok = wind_tm_get_next_event(tdsch,major,minor,'HK')
c	   item = 'TEMP_WAVES2'
c	   ok = wind_tm_get_item(tdsch, item, temp_waves, 1, return_size)
c
C
C	GET NEXT EVENT
C
 110    continue
C
	! this is the main program loop
c 
	type*,'going to get next event,tds,fill times',scettds,scetfill
c
	if(scettds.lt.scetfill) then
	  event = 'TDSF'
	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  ch = tdsch
	else
	  event = 'FILL'
	  ch = fillch
	endif
	if(icheof.eq.1) then
	  event = 'TDSF'
	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  ch = tdsch
	elseif(icheof.eq.2) then
	  event = 'FILL'
	  ch = fillch
	else
	endif
C	type*,'compare scettds,scetfill,evt',scettds,scetfill,event

	ok = w_event(ch,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
              if( ok.eq.82) type*,'end of file on ',event
              if( ok.eq.82) stop 'end of file on above channel '
	   else
	       item = 'EVENT_SCET'
	       ok = w_item_i4(ch, item, s_scet, 2, return_size)
	       ss = mod(s_scet(2),100)
	       mm = s_scet(2)/100
	       mm = mod(mm,100)
	       hh = s_scet(2)/10000
	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
c
	       item = 'EVENT_SCET_R8'
	       ok = w_item_R8(ch, item, scett, 1, return_size)
	       if(ch.eq.tdsch) then
	          scettds = scett
	       else
	          scetfill = scett
	       endif
	       item = 'EVENT_NUMBER'
	       ok = w_item_i4(ch, item, itemp, 1, return_size)
	       type*,'event number',itemp,'  ',event
	       if(iend.eq.2.and.itemp.ne.nevent) go to 110
	       item = 'DPU_CLOCK'
	       ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
	       WRITE(PTITLE(15),1014) DPUCLK
 1014		FORMAT(F12.3)
	       item = 'SUN_ANGLE'
	       ok = w_item_R8(ch, item, sunclock, 1, return_size)
	       item = 'event_boe_r8'
	       ok = w_item_R8(ch, item, beginevt, 1, return_size)
	       item = 'event_eoe_r8'
	       ok = w_item_R8(ch, item, endevt, 1, return_size)
	       CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
	   end if
c	do n = 1,2048
c		write(56,*) n,ndata(n),data(n)
c	enddo
C

C	     item = 'EVENT_SCET_R8'
c	     ok = w_item_r8(tdsch, item, scet, 1, return_size)
c	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c	     call w_ur8_to_string(scet,PTITLE(16),PTITLE(18))
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
c	     call w_ur8_to_ydoy(scetT,yyyy,doy,msday)
	     ihrmn = 100*hh+mm
c	     TYPE *,s_scet
c	     TYPE *,'scett,doy',scett,doy
C		write(26,*) itemp,s_scet
	   ihrmn = 100*hh+mm

	
	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)



	   WRITE(PTITLE(17),1016) s(1:4),S(5:6),S(7:8)
	   WRITE(PTITLE(18),1017) DOY
	   WRITE(PTITLE(19),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1017	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
c	   item = 'CHANNEL'
c	   ok = wind_tm_get_item(ch, item, tds_channel, 1, return_size)
c	   TYPE*,'CHANNEL',tds_channel
	   WRITE(PTITLE(6),1019) TDS_CHANNEL
 1019	   FORMAT('CHANNEL',I2)
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
c	   type*,'event number',itemp
	   WRITE(PTITLE(5),1012) ITEMP
 1012	   FORMAT(I10)
C
	   item = 'ERT_MAJOR_FRAME'
	   ok = W_ITEM_I4(ch, item, MAJOR, 1, return_size)
	   item = 'ERT_MINOR_FRAME'
	   ok = W_ITEM_I4(ch, item, MINOR, 1, return_size)
C
	   ipa = ichpa(tds_channel)
	   WRITE(PTITLE(7),1007) PARX(IRX)
 1007	   FORMAT('P/A ',A4)
c
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
	MAXDATA = 0
	IF(IPROCESS.EQ.0) THEN
	  DO IK = 1,2048
	    NDATA(IK) = NDATA(IK)-128
	    MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
	  ENDDO
	ELSE
	  DO IK = 1,2048
	    NDATA(IK) = NDATA(IK)-128
	    MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
	  ENDDO
	ENDIF
**********************
	IF(IFASTSLOW.EQ.0.AND.TDS_CHANNEL.GT.2) GO TO 110
	IF(IFASTSLOW.EQ.1.AND.TDS_CHANNEL.LE.2) GO TO 110
	write(87,7737) itemp,event,s_scet(2),tds_channel,
     1	parx(irx),maxdata,tdscal(tds_channel,isps,maxdata+128)
 7737	format(i10,2x,a4,i10,i5,2x,a4,i5,e12.3)
c	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.85) GO TO 110
c	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.65) GO TO 110
c	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.2.AND.MAXDATA.LE.75) GO TO 110
c	IF(IEND.EQ.1.AND.TDS_CHANNEL.GE.2) GO TO 110
c	IF(IEND.EQ.1.AND.TDS_CHANNEL.NE.3) GO TO 110
c	to pick out a specific event  (NOW MOVED EARLIER)
	if(iend.eq.2.and.itemp.ne.nevent) go to 110
C*******************
C
C	FIND ZERO CROSSING
C
	IZCNT = 0
	IL = 1
	IZ = IL
c	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL),NDATA(IL+1)
	DO IL = 2,2047
	  IZ = IL
C
C		COUNT ONLY POS TO NEG
C
	  IF(NDATA(IL).GT.0.AND.NDATA(IL+1).LE.0) THEN
	        IZCNT = IZCNT+1
		IF(IPROCESS.EQ.0) THEN
		  S1 = NDATA(IL)
		  S2 = NDATA(IL+1)
		ELSE
		  S1 = DATA(IL)
		  S2 = DATA(IL+1)
		ENDIF
		DELZ = S1/(S1 - S2)
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
C	print*,'zero crossings found',izcnt
C	print*,'first 5',(zcross(kk),kk=1,5)
C
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	number_event = itemp
	CALL PUBCOMBO(3)
C	READ(5,*) DISPOSE
	NPLOTS=NPLOTS+1
C	print*,'got to nplots.lt.nrem;',nplots,nrem
	IF(NPLOTS.LT.NREM) GO TO 110
	nplots = 0
	ok = w_channel_close(tdsch)
	ok = w_channel_close(fillch)
	GO TO 10
C	STOP
	END
	SUBROUTINE PUBCOMBO(ITERM)
C
C	SAME AS COMBO EXCEPT NO RAW DATA
C	THE FIRST (TOP) PANEL IS THE DATA IN PHYSICAL UNITS, 
C	THE SECOND IS THE FREQ  FROM 
C	ZERO CROSSINGS, AND THE THIRD IS THE FOURIER TRANSFORM
C	THIS VERSION DOES NOT HAVE INTERPOLATED PHYSICAL DATA
C
	CHARACTER*12 TITLE(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*1 DISPOSE
	INTEGER*4 TDS_CHANNEL,S_SCET(2)
	INTEGER*4 NUMBER_EVENT
	REAL*8 BEGINevt,endevt
	integer*4 dds
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT
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
	IF(ITERM.LT.0) THEN
	  XSTART = 350.
	  XEND = 2000.
	  CALL MGOSETLOC(XSTART,400.,XEND,2900.)
	ELSE
	  XSTART = 100.
	  XEND = 880.
	  CALL MGOSETLOC(XSTART,80.,XEND,750.)
	ENDIF
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(20) = ' AVR.FREQ.'
	WRITE(TITLE(21),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
	XTITLE = GX2 +.02*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
	DO N = 2,21
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  IF(N.EQ.12) YTITLE = YTITLE + TINC
	  IF(N.EQ.20) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	  YTITLE = YTITLE - TINC
	ENDDO
C
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOSETEXPAND(.7)
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,' TRANSMITTED @')
	  YTITLE = YTITLE-.8*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  FDAY = BEGINEVT - DFLOAT(DDS)
          NHR = FDAY*24.
	  MIN = FDAY*1440. - (NHR*60)
	  SEC = FDAY*86400. - 3600.*NHR - 60.*MIN	  
	  WRITE(STR,1028) NHR,MIN,SEC
 1028	  FORMAT(1X,I2.2,I2.2,1X,F6.2)
	  CALL MGOLABEL(12,STR)
C
	  YTITLE = YTITLE-TINC
	  CALL MGOSETEXPAND(.7)
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(10,' SUN ANGLE')
	  YTITLE = YTITLE-.8*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(STR,1027) SUNCLOCK*360./4096.
 1027	  FORMAT(F6.1' DEG.')
	  CALL MGOLABEL(10,STR)

	IF(IPROCESS.GE.3) THEN
	  CALL MGOSETEXPAND(.7)
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(8,' BAD A/D')
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(9,'CORRECTED')
	  CALL MGOSETEXPAND(.8)
	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	  CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSVIS,PUBCOMBO')
	CALL MGOSETEXPAND(1.)
C
C	PLOT TDS DATA IN PHYSICAL UNITS
C
C	     CALL REALFT(HDATA,2048,-1)		! USE FFT TO INTERPOLATE
C	     DO IK = 1,4096
C	       HDATA(IK) = HDATA(IK)/1024.
C	       VDATA(IK) = DATA(IK)
C	     ENDDO
c
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1800.,XEND,2900.)
	ELSE
	  CALL MGOSETLOC(XSTART,465.,XEND,750.)
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
	  DO N = 1,2048
 	    PP(N) = 500.*(N-1)/SPS
C 	    PP(N) = N
C	    YY(N) = ACORR*HDATA(N)/EFFLEN		! INTERPOLATED DATA
	    YY(N) = ACORR*DATA(N)/EFFLEN
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
C
c********
c	do n = 1,4096
c	  write(97,*) pp(n),yy(n)
c	enddo
c*****
C	PRINT*,'MAX mV/m',YMAX
C	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOSETLIM(PP(1),-YMAX,PP(2048),YMAX)
C
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,2048)
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
	ELSE
	  CALL MGOSETLOC(XSTART,310.,XEND,413.)
	ENDIF

C
	SPSKHZ = .001*SPS
	YMIN = .5*SPSKHZ
	YMAX = 0.
C	PRINT*,'IZCNT',IZCNT
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
C	PRINT*,'YMIN,YMAX',YMIN,YMAX
	YMIN =  .5*AVRFREQ
	YMAX = 1.25*AVRFREQ
C**********
C	YMIN =  0.
C	YMAX =  3.
C	YMIN =  18.
C	YMAX =  26.
C	YMIN =  12.
C	YMAX =  17.
C	PRINT*,'SET TO   ',YMIN,YMAX
	CALL MGOSETEXPAND(.8)
	CALL MGOSETLIM(0.,YMIN,PP(2048),YMAX)
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
	ELSE
	  CALL MGOSETLOC(XSTART,50.,XEND,264.)
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
	  YY(NP) = SPECT(NP)
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
 420	CONTINUE
C	PRINT*,'DB,YMAX,YMIN',YMAX,YMIN
	BW = MAXCOUNT*SPSUSE/2048.
C	PRINT*,'MAXCOUNT, BANDWIDTH',MAXCOUNT,BW
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
	YTITLE = YTITLE-TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
C	CALL MGOLABEL(10,'   10 dB  ')
C	YTITLE = YTITLE-TINC
C	CALL MGOGRELOCATE(XTITLE,YTITLE)
C	CALL MGOLABEL(10,' BANDWIDTH')
C	YTITLE = YTITLE-TINC
C	CALL MGOGRELOCATE(XTITLE,YTITLE)
C	CALL MGOLABEL(10,'  OF PEAK ')
	IF(TDS_CHANNEL.GT.2) WRITE(STR,1021) .001*BW
	IF(TDS_CHANNEL.LE.2) WRITE(STR,1022) BW
 1021	FORMAT(F7.2,' HZ')
 1022	FORMAT(F6.3,' kHZ')
	YTITLE = YTITLE - TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
C	CALL MGOLABEL(10,STR)
	CALL MGOSETEXPAND(1.)
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
C
	END	
	SUBROUTINE DETAIL(ITERM,N1,N2)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 NUMBER_EVENT
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT
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
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	CHARACTER*80	YYYYMMDD
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	integer*4	iq,NREM,NHRMN,IFASTSLOW

10	  write(6,6)
	  write(6,7)
	  read(5,5,err=10,end=20) iq, stream
	print*,'in get_, initial stream=  ',stream
 	if (iq .lt. 1) then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else
	   ! assume the user entered the TIME of an offline file
	   YYYYMMDD = STREAM(1:8)
	   PRINT*,YYYYMMDD
	   WRITE(STREAM,30) YYYYMMDD
 30	   FORMAT('wi_lz_wav_',A8,'_v*.dat')
	   PRINT*,'STREAM IN GET',STREAM
	end if

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
  7	format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
	end
