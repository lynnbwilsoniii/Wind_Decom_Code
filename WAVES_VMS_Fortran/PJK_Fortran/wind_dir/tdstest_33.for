
	PROGRAM TDSPRO
C
C	PLOTS TDS AND FREQUENCY
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	i,j,k,n,itemp,il,ilp,iz
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6),ipacal(6,4)
	character*80	stream
	character*4	event
	character*4	pa(6,4)
	parameter	size=2048
	integer*4	return_size
	integer*4	tds_channel,ifilf,ifils,ifil,ifsps,issps,isps
	integer*4	temp_waves
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scettds,scetfill
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
	real 		ffilter,sfilter,fsps,ssps,sps
	REAL 		S1,S2,SPECT(1025)
!
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	common /headblk/ major,minor,tds_channel,s_scet

C
	CHARACTER*16 PTITLE(20)
	INTEGER*4 TDSCH,hkch,fillch,ch
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
c	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
c     1   NDATA(2060),DATA(2050)
	integer*4   NDATA(2050)
	real DATA(2050)
	COMMON /HEADBL/ PTITLE,EVENT
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /PLOTDT/ SPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	DATA TWOPI /6.2831853/
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.875/
	DATA SSPS /7500.,1875.,468.75,117.2/
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
	DATA IPACAL /1,    1,     4,     7,     8,     9,
     1               4,    2,     5,     4,     5,     6,
     2               0,    3,     6,     0,     0,     0,
     3               0,    3,     6,     0,     0,     0/
	DATA IFASTSLOW /0/
C
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = 'TIME DOMAIN'
	PTITLE(3) = 'SAMPLER'
	PTITLE(4) = 'EVENT NO.'
	PTITLE(8) = 'SAMPLE RATE'
	PTITLE(10) = 'L.P.FILTER'
	PTITLE(12) = 'TRIGGERS'
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
	  read(5,3,err=10,end=20) iq, NREM
	  type*,nrem
	endif
	type*,'type desired process level,0=raw,1=raw volts,2=fft,fft-1'
	read(5,3) iq,iprocess
	type*,' '
	if(iprocess.eq.0) type*,'ok, plot data in tm numbers - 128'
	if(iprocess.eq.1) type*,'ok, plot in volts, unity freq. response.'
	if(iprocess.eq.2) type*,'ok, plot volts, corrected for freq. response.'
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
	dd = scettds
	scettds = float(dd) + hh/24. + mm/1440.
	print*,'set tds channel position to',scettds
	call w_channel_position(tdsch,scettds)
	print*,'tds channel position set to',scettds

	ok = w_channel_open(fillch,stream)           
	if (.not. ok) stop 'Cannot open fill channel'      
	scetfill = 0.
	call w_channel_position(fillch,scetfill)
	print*,'scetfill',scetfill
	dd = scetfill
	scetfill = float(dd) + hh/24. + mm/1440.
	print*,'set channel position to',scetfill
	call w_channel_position(fillch,scetfill)
	print*,'fill channel position set to',scetfill

        ok = w_event(tdsch,'TDSF')
        if(IFASTSLOW.NE.0) ok = w_event(tdsch,'TDSS')
	if(ok.eq.82) stop 'end of file on tdsch'
	if (.not. ok) stop 'cannot get tds event'
c	item = 'EVENT_SCET_R8'
c	  ok = w_item_r8(tdsch, item, scet, 2, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(tdsch, item, s_scet, 2, return_size)
	print*,'initial tdsch time',s_scet
c	  scetfill = s_scet(2)
c
        ok = w_event(fillch,'FILL')
	if(ok.eq.82) stop 'end of file on tdsch'
	if (.not. ok) stop 'cannot get fill event'
c	item = 'EVENT_SCET_R8'
c	  ok = w_item_r8(fillch, item, scet, 2, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(fillch, item, s_scet, 2, return_size)
	print*,'initial fillch time',s_scet
	  
c
	ok = w_channel_filename(tdsch,file)
c	print*,file
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

        if( wind_tm_eof(tdsch,major,minor)) stop 'end of file on tdsch'
        if( wind_tm_eof(fillch,major,minor)) stop 'end of file, fillch'

	type*,'going to get next tds event'

	if(scettds.lt.scetfill) then
	  event = 'TDSF'
	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  ch = tdsch
	else
	  event = 'FILL'
	  ch = fillch
	endif

	ok = w_event(ch,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	   end if
C
	     item = 'EVENT_SCET'
	     ok = w_item_i4(ch, item, s_scet, 2, return_size)
		ss = mod(s_scet(2),100)
		mm = s_scet(2)/100
		mm = mod(mm,100)
		hh = s_scet(2)/10000
		scett = float(dd) + hh/24. + mm/1440. + ss/86400.
	     if(scettds.lt.scetfill) then
	       scettds = scett
	     else
	       scetfill = scett
	     endif


	     item = 'EVENT_NUMBER'
	     ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
	     TYPE *,itemp,YYYY,MON,DD,IHRMN
		write(26,*) itemp,YYYY,MON,DD,IHRMN
c 1016	   format(I5,'/',I2,'/',I2)
	   ihrmn = 100*hh+mm
	   WRITE(PTITLE(17),1017) s_scet(1)
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
c	   WRITE(PTITLE(17),1017) ihrmm,ss
 1017	   FORMAT('YMD',I9)
C 1017	   FORMAT(I10.6)
c 1017	   FORMAT('doy ',I3,I5.4)
 1018	   FORMAT(I6.4,I3.2)
	   item = 'CHANNEL'
	   ok = wind_tm_get_item(ch, item, tds_channel, 1, return_size)
	TYPE*,'CHANNEL',tds_channel
	   WRITE(PTITLE(6),1019) TDS_CHANNEL
 1019	   FORMAT('CHANNEL',I2)
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
	   type*,'event number',itemp
	   WRITE(PTITLE(5),1012) ITEMP
 1012	   FORMAT(I10)
c
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
	   WRITE(PTITLE(2),1009)  TDS_CHANNEL
	   ipa = ichpa(tds_channel)
	   WRITE(PTITLE(7),1007) pa(tds_channel,ipa+1)
 1007	   FORMAT('P/A ',A4)
	   IRX = IPACAL(TDS_CHANNEL,IPA+1)
	   IF(TDS_CHANNEL.LE.2) THEN
	      WRITE(PTITLE(9),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(11),1008) FFILTER(IFILF+1)
	      IFIL = IFILF
	   ELSE
	      WRITE(PTITLE(9),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	      IFIL = IFILS
	   ENDIF
 1004	   FORMAT(F7.2,' kHZ')
 1008	   FORMAT(F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

C
	print*,' call tds_phys,iprocess=',iprocess
	call tds_phys(ch,iprocess,data,spect)
C
c	ITEM = 'DATA'
c	OK = WIND_TM_GET_ITEM(CH,ITEM,NDATA,SIZE,RETURN_SIZE)
c	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
c	  PRINT*,'DATA, SIZE=',RETURN_SIZE
C	  PRINT 222, (NDATA(J),J=1,2048)
c 222	FORMAT(10I6)
c	IF(IPROCESS.EQ.0) THEN
c	  DO IK = 1,2048
c	    NDATA(IK) = NDATA(IK)-128
c	  ENDDO
c	ELSE
c	  DO IK = 1,2048
c	    DATA(IK) = TDSCAL(TDS_CHANNEL,ISPS,NDATA(IK))
c	    NDATA(IK) = NDATA(IK)-128
c	  ENDDO
c	ENDIF
C
C	FIND ZERO CROSSING
C
ce means emergency to try to get the damn thng to run
	IZCNT = 0
ce	DO IL = 1,2047
ce	  IZ = IL
ce	print*,' il',iz
ce	  ilp = max0(il,2)
ce	  IF(NDATA(ILp).EQ.0) PRINT*,'ZERO DATA',ILp,
ce     1   NDATA(ILp-1), NDATA(ILp),NDATA(ILp+1)
C		COUNT ALL CROSSINGS, POS TO NEG AND NEG TO POS
C	  IF(NDATA(IL)*NDATA(IL+1).LE.0) THEN
C	        IZCNT = IZCNT+1
C		S1 = NDATA(IL)
C		S2 = NDATA(IL+1)
C		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
C	  ENDIF
C		COUNT ONLY POS TO NEG
ce	  IF(DATA(IL).GT.0.AND.DATA(IL+1).LE.0) THEN
ce	        IZCNT = IZCNT+1
ce		  S1 = DATA(IL)
ce		  S2 = DATA(IL+1)
ce		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
ce	  ENDIF
ce	ENDDO
ce	DO N = 1,IZCNT-1
ce	  ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
ce	  IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
ce	  IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
ce	ENDDO
ce	print*,'zero crossings found',izcnt
ce	print*,'first 5',(zcross(kk),kk=1,5)
C
	IF(IPROCESS.EQ.2) THEN
	TYPE*,'TDSTEST'
	TYPE*,(DATA(I),I=1,10)
	TYPE*,(DATA(I),I=2040,2049)
	     CALL SPPLOT(-2,SPECT)
	ENDIF
C
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C

	CALL PLTDSFR(-2)
	IF(IPROCESS.LE.1) CALL DETAIL(-2,900,1100)
	CALL DETAIL(-2,900,1100)
	NPLOTS=NPLOTS+1
	IF(NPLOTS.LT.NREM) GO TO 110
	STOP
	END
	SUBROUTINE PLTDSFR(ITERM)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2060),DATA(2050)
	COMMON /PLOTDT/ SPS
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
c	  WRITE(STR,704) SAA()
c 704	  FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c	  CALL MGOPUTLABEL(55,STR,9)
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
	TINC = .08*TRANGE
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
	  NPLOTS = NPLOTS + 1
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
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2060),DATA(2050)
	COMMON /PLOTDT/ SPS
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
	  NPLOTS = NPLOTS + 1
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END
	SUBROUTINE SPPLOT(ITERM,SPECT)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2060),DATA(2050)
	COMMON /PLOTDT/ SPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	common /headblk/ major,minor,tds_channel,s_scet
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
	DIMENSION YY(2048),PP(2048),SPECT(1500)
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
	IF(FFT_CHANNEL.LE.2) SPSUSE = .001*SPS
	DO NP = 1,1024
C	  NP = (N-1)/2
	  PP(NP) = (NP-1)*SPSUSE/2048.
	  YY(NP) = SPECT(NP)
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
	YMAX = YMAX + .05*RANGE
	YMIN = YMIN - .05*RANGE
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
	IF(FFT_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	IF(FFT_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
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
