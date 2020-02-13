 	PROGRAM MOONSWAKE
C
C	PLOTS CDF DATA,  WIND'S POSITION RELATIVE TO MOON AND SOLAR
C		ARRAY CURRENT 
C	altered 11 Oct 1996 to plot 3dp,swe, and waves densities
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*80	stream
	character*4	event
	parameter	size=2048
	integer*4	return_size
	integer*4	temp_waves,iend,n2
	character*32	s
	real*8		SCET_SWE(2000)
	real*8		scet_MOON(2000),XM(2000),YM(2000),ZM(2000)
	real*8		scet_wind(2000),windx(2000),windy(2000),windz(2000)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	real*8		SCET_WAV(2000),scet_3dp(2000)
	real*8		scet,scettds,scetfill
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
!
	common /pltblk2/ nswe,SCET_SWE,vx,vy,vz,vmag,densswe
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
	common /pltblk4/ ntnr,SCET_WAV,denstnr
	common /pltblk5/ nMOON,RMOON,SCET_MOON,XM,YM,ZM
	common /pltblk6/ nWIND ,SCET_WIND,WINDX,WINDY,WINDZ
	common /pltblk7/ nsa ,samax(2000),samin(2000)

C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CDHFCH,hkch,fillch,ch
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	DATA TWOPI /6.2831853/
C
C	GET STARTED
C
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
C
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,*,err=10) NHRMN,LAST
	  type*,NHRMN,LAST
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
	endif
c
  5	format(q,a)
  3	format(q,i10)
c

	ok = w_channel_open(CDHFCH,stream)
	if (.not. ok) stop 'Cannot open cdhf channel'
	scettds = 0.
	call w_channel_position(CDHFCH,scettds)
	print*,'cdhf file starts at scettds',scettds
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set cdhf channel position to',scettds
	call w_channel_position(CDHFCH,scettds)
	print*,'cdhf channel position set to',scettds
c
	ok = w_channel_filename(CDHFCH,file)
C	write(87,*) file
	print*,file
c
	get_tm_stream = 1
c
	call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	call w_ur8_to_ydoy(scetfill,yyyy,doy,msday)

	  ok = w_event(CDHFCH,'CDF')
	if(ok.eq.82) then
	   scettds = 10000.       	! artificially large
	   print*,'end of file'
	   if (.not. ok) stop 'cannot get tds event'
	else
	  item = 'WIND_SA_MAX_AMPS_R4'
	  ok = w_item_r4(CDHFCH, item, SAMAX, 2000, return_size)
	  nsa = return_size
	  PRINT*,'N SOLAR CURRET',NSA
	  item = 'WIND_SA_MIN_AMPS_R4'
	  ok = w_item_r4(CDHFCH, item, SAMIN, 2000, return_size)
	  item = 'WIND_SWE_SCET_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_SWE, 2000, return_size)
	  print*,'initial swe time',SCET_SWE(1),' size',return_size
	  print*,'swe dens(1)',densswe(1),'size',return_size
c
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(CDHFCH, item, dens3dp, 2000, return_size)
	  n3dp = return_size
	  print*,'3dp dens(1)',dens3dp(1),'size',return_size
	  item = 'WIND_3DP_ION_VX(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VX3, 2000, return_size)
	  item = 'WIND_3DP_ION_VY(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VY3, 2000, return_size)
	  item = 'WIND_3DP_ION_VZ(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VZ3, 2000, return_size)
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_3DP, 2000, return_size)
c
	  item = 'WIND_wav_ne_R4'
	  ok = w_item_r4(CDHFCH, item, denstnr, 2000, return_size)
	  item = 'WIND_wav_scet_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_WAV, 2000, return_size)
	  print*,'tnr dens(1)',denstnr(1),'size',return_size
	  ntnr = return_size
c
	  item = 'WIND_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, WINDX, 2000, return_size)
	  nWIND = return_size
	  item = 'WIND_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, WINDY, 2000, return_size)
	  item = 'WIND_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, WINDZ, 2000, return_size)
	  item = 'WIND_ORBIT_scet_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_WIND, 2000, return_size)
	  print*,'WIND size',return_size
C
	  item = 'MOON_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, XM, 2000, return_size)
	  nMOON = return_size
	  print*,'MOON FILE size',NMOON
	  item = 'MOON_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, YM, 2000, return_size)
	  item = 'MOON_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, ZM, 2000, return_size)
	  item = 'MOON_ORBIT_scet_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_MOON, 2000, return_size)
	  ITEM = 'R_MOON_R4'
	  ok = w_item_r4(CDHFCH, item, RMOON, 1, return_size)
	  RMOON = .001*RMOON                	! CHANGE TO KM
	endif
c
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	   ihrmn = 100*hh+mm

	
c	   write(s,'(i8.8,i6.6)',iostat=ios) mfi_scet(1), mfi_scet(2)
C	   mfi_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
C	CALL ORBPLOT(3)
	CALL ORBPLOT(-1)
C	CALL BPLOT(-1)
C	NPLOTS=NPLOTS+1
	STOP
	END
	SUBROUTINE ORBPLOT(ITERM)
C
C	PLOT WIND V.S. WIND AND  V.S. TIME
C
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk4/ ntnr,SCET_WAV,denstnr
	common /pltblk5/ nMOON,RMOON,SCET_MOON,XM,YM,ZM
	common /pltblk6/ nWIND ,SCET_WIND,WINDX,WINDY,WINDZ
	common /pltblk7/ nsa ,samax(2000),samin(2000)
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
	real*8		scet_MOON(2000),xM(2000),yM(2000),zM(2000)
	real*4		xINT(2000),yINT(2000),zINT(2000)
	real*8		scet_wind(2000),windx(2000),windy(2000),windz(2000)
	real*8		sday,SWETIME,SCET_WAV(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		TGT(2048),DENSTNR(2000)
	integer*4 	nWIND,nGTAIL
	DIMENSION 	YY(2048),XX(2048)
	DATA RE /6378./
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	HH = LAST/100
	MM = MOD(LAST,100)
	FDAYL = HH/24. + MM/1440.
C
	XEND = 2000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(600.,400.,XEND,3100.)
	ENDIF
C
	NDDS = SCET_WIND(1)
	SDAY = NDDS
	TFACT = 24.
	    XMAX = 0.
	    XMIN = 24.
	    N = 0
	    dowhile(n.lt.nmoon)
	       N = N+1
	       fday = scet_MOON(n) - SDAY
  	       TGT(N) = fday*tfact
	    enddo
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NWIND) GO TO 202
	       IF(N.GT.NMOON) GO TO 202
	       fday = scet_WIND(n) - SDAY
	       WTIME = (SCET_WIND(N) - SDAY)*TFACT
	       NPPNT = N
  	       XX(N) = WTIME
	       XMAX = AMAX1(XX(N),XMAX)
	       XMIN = AMIN1(XX(N),XMIN)
C		CALCULATE MOON'S POSITION AT WIND TIMES
	       CALL DINTERP(TGT,XM,NMOON,WTIME,XXTT,ERR)
	       XINT(N) = XXTT
	       CALL DINTERP(TGT,YM,NMOON,WTIME,XXTT,ERR)
	       YINT(N) = XXTT
	       CALL DINTERP(TGT,ZM,NMOON,WTIME,XXTT,ERR)
	       ZINT(N) = XXTT
	    ENDDO
 202	 XLST = XX(NPPNT)
	 XRANGE = XX(NPPNT) - XX(1)
	 XLST = XLST + .003*XRANGE
	 XST = XX(1) - .003*XRANGE
	PRINT*,'NPPNT,TIME',NPPNT,XX(NPPNT)
C	PRINT*,'TGT',TGT(1),TGT(2),TGT(3),TGT(4)
C	PRINT*,'MOONXYZ',XM(1),YM(1),ZM(1)
C	PRINT*,'MOONXYZ',XM(2),YM(2),ZM(2)
C	PRINT*,'MOONXYZ',XM(3),YM(3),ZM(3)
C	PRINT*,'MOONXYZ',XM(4),YM(4),ZM(4)
C	PRINT*,'INTXYZ',XINT(1),YINT(1),ZINT(1)
C	PRINT*,'INTXYZ',XINT(2),YINT(2),ZINT(2)
C	PRINT*,'WTIME',XX(1),XX(2),XX(3),XX(4)
C	PRINT*,'WNXYZ',WINDX(1),WINDY(1),WINDZ(1)
C	PRINT*,'WNXYZ',WINDX(2),WINDY(2),WINDZ(2)
	
C
	NW = 4
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
C
	  IF(JW.EQ.1) THEN
	print*,'jw=1,,nppnt',nppnt
C
C		PLOT DEL Z V.S. TIME
C		X,Y,ZINT ARE MOONS POSITION AT WIND TIME
C
	    DO N = 1,NPPNT
	      YY(N) = (ZINT(N) - WINDZ(N))/RMOON
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(15,'DEL Z (GSE, Rm)')
	    CALL MGOXLABEL(18,'HOURS')
	  ENDIF
C
	  IF(JW.EQ.2) THEN
	print*,'jw=2,,nppnt',nppnt
	    DO N = 1,NPPNT
	      YY(N) = (YINT(N) - WINDY(N))/RMOON
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(15,'DEL Y (GSE, Rm)')
	    CALL MGOXLABEL(18,'HOURS')
	  ENDIF
C
	  IF(JW.EQ.3) THEN
	print*,'jw=3,,nppnt',nppnt
	    DO N = 1,NPPNT
	      YY(N) = (XINT(N) - WINDX(N))/RMOON
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOXLABEL(12,' HOURS')
	    CALL MGOYLABEL(15,'DEL X (GSE, Rm)')
	  ENDIF
C
	  IF(JW.EQ.4) THEN
	    N = 0
	    XXTT = 0.
	    DOWHILE(XXTT.LT.FDAYL)
	      N = N+1
	      YY(N) = samax(n)
C	      XX(N) = (N-1)/20.
	      XXTT = (SCET_WAV(N) - SDAY)
	      XX(N) = XXTT*TFACT
	      MPPNT = N
	      IF(YY(N).LT.50.) YMAX = AMAX1(YY(N),YMAX)
	    ENDDO
	print*,'jw=4,,mppnt',mppnt
	    YMAX = 1.1*YMAX
	    YMIN = 0.
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOCONNECT(XX,YY,MPPNT)
	    DO N = 1,MPPNT
	      YY(N) = samin(n)
	    ENDDO
	    CALL MGOXLABEL(5,'HOURS')
	    CALL MGOYLABEL(7,'SA AMPS')
	    NPPNT=MPPNT
	  ENDIF
C
C
	  YRANGE = YMAX - YMIN
	  PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	  YMAX = YMAX + .02*YRANGE
	  YMIN = YMIN - .02*YRANGE
	  CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(XX,YY,NPPNT)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOBOX(1,2)
	ENDDO
C
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(FILE(31:42),'[.WIND]ORBPLOT')
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
	SUBROUTINE BPLOT(ITERM)
C
C	PLOT MAG DATA V.S. TIME
C
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
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
	real*8		mfi_scet(2000)
	real*8		SCET_SWE(2000),SCET_WAV(2000),SCET_3DP(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	integer*4 	nmfi,nswe,N3DP,NTNR
	DIMENSION YY(2048),PP(2048),TMFI(2048)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	HH = LAST/100
	MM = MOD(LAST,100)
	FDAYL = HH/24. + MM/1440.
C
	XEND = 2250.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,XEND,3100.)
	ENDIF
C
	NDDS = MFI_SCET(1)
	SDAY = NDDS
	TFACT = 24.
	NW = 4
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
	  IF(JW.EQ.1) THEN     	! 	PLOT MAGNETIC FIELD
C
C		PLOT MFI DATA
C
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	      N = N+1
	       IF(N.GT.NMFI) GO TO 201
	      fday = mfi_scet(n) - SDAY
  	      TMFI(N) = fday*tfact
  	      PP(N) = fday*tfact
	      NPPNT = N
	      YY(N) = BMAG(N)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 201	    XLST = PP(NPPNT)
	    XRANGE = PP(NPPNT) - PP(1)
	    CALL MGOYLABEL(5,'B mag')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.2) THEN		! 	PLOT BX
	    DO N = 1,NPPNT
	      YY(N) = BX(N)
	      IF(YY(N).LT.1500.)  YMAX = AMAX1(YY(N),YMAX)
	      IF(YY(N).GT.-1500.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(7,'BX(GSE)')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.3) THEN			! PLOT BY
	    DO N = 1,NPPNT
	       YY(N) = BY(N)
	       IF(YY(N).LT.1000.)YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.-1000.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    CALL MGOYLABEL(7,'BY(GSE)')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.4) THEN		!	PLOT SPEEDS
	    DO N = 1,NPPNT
	         YY(N) = BZ(N)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(7,'BZ(GSE)')
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
 200	  XST = PP(1) - .003*XRANGE
	  XLST = XLST + .003*XRANGE
	  YRANGE = YMAX - YMIN
	  PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	  YMAX = YMAX + .02*YRANGE
	  YMIN = YMIN - .02*YRANGE
	  CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  DO N = 1,NPPNT
	    YY(N) = AMIN1(YY(N),1.E10)
	    YY(N) = AMAX1(YY(N),-1.E10)
	  ENDDO
	  CALL MGOCONNECT(PP,YY,NPPNT)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOBOX(1,2)
	  CALL MGOSETEXPAND(1.)
	  CALL MGOSETEXPAND(.8)
	  CALL MGOSETEXPAND(1.)
 207	   CONTINUE
	  IF(JW.EQ.4)CALL MGOPLOTID(FILE(31:42),'[.WIND]CDFPLOT')
	ENDDO			!END JW=1,4 DO LOOP
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

	print*,' get_stream_name got',stream

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end
	SUBROUTINE DINTERPTEST(XX,YY,NN,X,Y,ERR)
C
C	INTERPOLATE ON X TO FIND Y, GIVEN TABLES XX AND YY, CONTAINING
C	NN ELEMENTS, TABLE XX MUST BE IN INCREASING ORDER
C
	DIMENSION  XX(1)
	REAL*8	   YY(1)
C
	print*,'dinterp called,x=',x
	Y = YY(1)
	ERR = -1.
	IF(X.LT.XX(1)) print*,'first return,x,xx(1)=',x,xx(1)
	IF(X.LT.XX(1)) RETURN
	Y = YY(NN)
	ERR = 1.
	IF(X.gT.XX(nn)) print*,'last return,x,xx(1)=',x,xx(nn)
	IF(X.GT.XX(NN)) RETURN
	ERR = 0.
	NNX = (NN+1)/2
	NDEL = (NNX+1)/2
  25	IF(NDEL.LE.1) GO TO 29
	IF(X.GE.XX(NNX)) GO TO 20
	NNX = NNX - NDEL
	NDEL = (NDEL+1)/2
	GO TO 25
  20	NNX = NNX + NDEL
	NDEL = (NDEL+1)/2
	GO TO 25
  29	LLIM = MAX0(NNX-1,2)
	DO 10 I = LLIM,NN
	IF((X-XX(I)).GT.0.) GO TO 10
	Y = YY(I-1) + (YY(I)-YY(I-1))*(X-XX(I-1))/(XX(I)-XX(I-1))
	print*,'dint20',i,xx(i-1),xx(i),yy(i-1),yy(i),y
	RETURN
  10	CONTINUE
	print*,'dint10',i,xx(i-1),xx(i),yy(i-1),yy(i),y
	RETURN
	END

