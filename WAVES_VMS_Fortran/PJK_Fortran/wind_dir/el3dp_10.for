    	PROGRAM EL3DP
C
C	PLOTS GRAY SCALE PLOT OF 3DP ELECTRONS, AND DIFF
C		TPLOT 3DP     FLUXES IN MANY ENERGY CHANNELS
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*120	JUNK
	character*80	stream
	character*4	event
	parameter	size=2048
	integer*4	return_size
	integer*4	temp_waves,iend,n2,S_SCET(2)
	character*32	s
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),ef250(2000),ef1(2000)
	real*4		ptemp(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		dens3dp(2000)
	real*8		scet_3dp(2000)
	real*8		scet,scettds,scetfill
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
!
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,PTEMP
C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CDHFCH,hkch,fillch,ch
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	DATA TWOPI /6.2831853/
	DATA RE /6378./
C
C	GET STARTED
C
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
C
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min to start and end, e.g. 0412,1201'
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
	  item = 'EVENT_SCET'
	  ok = w_item_I4(CDHFCH, item, S_scet, 2, return_size)
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
	  item = 'WIND_3DP_ION_TEMP_R4'
	  ok = w_item_r4(CDHFCH, item, PTEMP, 2000, return_size)
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(CDHFCH, item, ETEMP, 2000, return_size)
	  item = 'WIND_3DP_E_QDOTB_R4'
	  ok = w_item_r4(CDHFCH, item, EFQ, 2000, return_size)
	  item = 'WIND_3DP_EF_250EV_R4'
	  ok = w_item_r4(CDHFCH, item, EF250, 2000, return_size)
	  item = 'WIND_3DP_EF_1KEV_R4'
	  ok = w_item_r4(CDHFCH, item, EF1, 2000, return_size)
	  item = 'WIND_3DP_EF_5KEV_R4'
	  ok = w_item_r4(CDHFCH, item, EF5, 2000, return_size)
	  item = 'WIND_3DP_EF_20KEV_R4'
	  ok = w_item_r4(CDHFCH, item, EF20, 2000, return_size)
	  item = 'WIND_3DP_EF_34KEV_R4'
	  ok = w_item_r4(CDHFCH, item, EF34, 2000, return_size)
	  item = 'WIND_3DP_EF_90KEV_R4'
	  ok = w_item_r4(CDHFCH, item, EF90, 2000, return_size)
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_3DP, 2000, return_size)
c
C	print*,'3dp,time,vx,vy,vz',scet_3dp(1),vx3(1),vy3(1),vz3(1)
C	if(1) stop
c
	endif
c
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	   ihrmn = 100*hh+mm

	
c
C	CALL TPLOT3DP(3)
	CALL TPLOT3DP(-1)
	DELAY = SECNDS(0.)
33	DELTA = SECNDS(DELAY)
	IF(DELTA.LT.2.) GO TO 33
	DELAY = SECNDS(0.)
34	DELTA = SECNDS(DELAY)
	IF(DELTA.LT.2.) GO TO 34
C	NPLOTS=NPLOTS+1
	STOP
	END
	SUBROUTINE TPLOT3DP(ITERM)
C
C	PLOT 3DP DATA V.S. TIME
C
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,PTEMP(2000)
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
	real*8		SCET_3DP(2000)
	real*8		sday
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	REAL*4		TORB(200)
	real*8		scet_wind(200),XGSE(200),YGSE(200),ZGSE(200)
	integer*4 	N3DP,NTNR,NWIND
	DIMENSION YY(2048),PP(2048)
	DATA RE 	/6.378E3/
C
	IF(N3DP.LE.0) THEN
	  PRINT*,'NO 3DP DATA'
	  RETURN
	ENDIF
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	HH = NHRMN/100
	MM = MOD(NHRMN,100)
	FDAYS = HH/24. + MM/1440.
	HH = LAST/100
	MM = MOD(LAST,100)
	FDAYL = HH/24. + MM/1440.
	print*,'last fraction of day',fdayl
C
	TFACT = 24.
	XST = TFACT*FDAYS
	XLST = TFACT*FDAYL
	XRANGE = XLST-XST
 	XST = XST - .005*XRANGE
 	XLST = XLST + .005*XRANGE
	PRINT*,'IN TPLOT3DP,XST,XLST=',NHRMN,LAST,XST,XLST
C
	XEND = 2250.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(550.,400.,XEND,3000.)
	ENDIF
C
	NDDS = SCET_3DP(1)
	SDAY = NDDS
	NW = 9
C
C	JW=1 IS	DIFF   		=2 IS E_TEMP		=3 IS 250EV
C	JW=4 IS 1 KEV		=5 IS 5 KEV		=6 IS 20 KEV
C	JW=7 IS 34 KEV		=8 IS 90 KEV	
C
	    DO N = 1,NWIND
	      fday = scet_WIND(n) - SDAY
  	      TORB(N) = fday*tfact
	    ENDDO
C
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
	PRINT*,'AFTER WINDOW JW',JW,GX1,GY1,GX2,GY2
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
C
	  IF(JW.EQ.1) THEN		! 	PLOT DIFF
C
C	    PLOT DIFF, TIME IS MFI TIME
C
	    YMAX = -1000.
	    YMIN = 1000.
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	      N = N+1
	      IF(N.GT.NMFI) GO TO 202
	      fday = MFI_scet(n) - sday
  	      PP(N) = fday*tfact
	      NPPNT = N
C		interpolate to get orbit at mfi times
C	      CALL DINTERP(TORB,XGSE,NWIND,PP(N),X0,ERR) 
C	      CALL DINTERP(TORB,YGSE,NWIND,PP(N),Y0,ERR)
C	      CALL DINTERP(TORB,ZGSE,NWIND,PP(N),Z0,ERR)
	      XRE = X0/RE
	      YRE = Y0/RE
	      ZRE = Z0/RE
C	      CALL DIFFFK(XRE,YRE,ZRE,BX(N),BY(N),BZ(N),DDF,XT,YT,ZT
C     1		XI,YI,ZI,INSIDE)
C	      YY(N) = AMIN1(DDF,20.)
C	      YY(N) = AMAX1(YY(N),-20.)
c	      YMAX = AMAX1(YY(N),YMAX)
c	      YMIN = AMIN1(YY(N),YMIN) 
	      YMAX = 20.
	      YMIN = -20.
	    ENDDO
 202	    XRANGE = PP(NPPNT) - PP(1)
	    CALL MGOYLABEL(4,'DIFF')
	    call w_ur8_to_ydoy(mfi_scet(2),yyyy,doy,ms)
	    call w_ur8_to_ymd(mfi_scet(2),yyyy,mon,dd,hh,mm,ss,ms)
	    write(STR,1202,iostat=ios) YYYY,MON,DD,DOY
 1202	    format(' HOURS OF ',I4.4,'/',I2.2,'/',I2.2,'  DOY',I5)
	    EXP_SAVE = EXPAND
	    CALL MGOSETEXPAND(.8)
	    XTITLE = .5*(GX1+GX2)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(XTITLE,75.)                      ! hardcopy
	    ENDIF
	    CALL MGOPUTLABEL(30,STR,8)
	    CALL MGOSETEXPAND(EXP_SAVE)
	    WRITE(STR,1203) XGSE(1)/RE,YGSE(1)/RE,ZGSE(1)/RE
 1203	    FORMAT('X,Y,Z(GSE)=',3F6.1)
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(GX1,200.)                      ! hardcopy
	    ENDIF
	    CALL MGOPUTLABEL(29,STR,9)
	    WRITE(STR,1203) XGSE(NWIND)/RE,YGSE(NWIND)/RE,ZGSE(NWIND)/RE
	    IF(ITERM.GT.0) THEN
	      CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	    ELSE
	      CALL MGOGRELOCATE(GX2,200.)                      ! hardcopy
	    ENDIF
	    CALL MGOPUTLABEL(29,STR,7)
	  ENDIF
C
	  IF(JW.EQ.2) THEN     	! 	PLOT ELECTRON TEMPERATURE
C
C		PLOT ETEMP
C
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	      N = N+1
	      IF(N.GT.N3DP) GO TO 201
	      fday = scet_3DP(n) - SDAY
C  	      TD3DP(N) = fday*tfact
  	      PP(N) = fday*tfact
	      NPPNT = N
	      YY(N) = ETEMP(N)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 201	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(6,'e TEMP')
	    EXP_SAVE = EXPAND
C	    CALL MGOSETEXPAND(.8)
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
C	    CALL MGOSETEXPAND(EXP_SAVE)
	  ENDIF
C
	  IF(JW.EQ.3) THEN			! PLOT 250 EV FLUX
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.N3DP) GO TO 203
 	       NPPNT = N
	       fday = scet_3DP(n) - SDAY
  	       PP(N) = fday*tfact
	       YY(N) = AMAX1(EF250(N),0.)
	       YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GT.0.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 203	    CONTINUE
	    YRANGE = YMAX - YMIN
	    CALL MGOYLABEL(8,'J,250 eV')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.4) THEN		!	PLOT 1 KEV FLUX
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.N3DP) GO TO 204
	       fday = scet_3DP(n) - SDAY
  	       PP(N) = fday*tfact
	         NPPNT = N
	         YY(N) = EF1(N)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 204	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(7,'J,1 KEV')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.5) THEN		!	PLOT 5 KEV FLUX
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N = N+1
	         IF(N.GT.N3DP) GO TO 205
	         NPPNT = N
	         fday = scet_3DP(n) - SDAY
	         YY(N) = EF5(N)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 205	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(7,'J,5 KEV')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.6) THEN		!	PLOT 20 KEV FLUX
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N = N+1
	         IF(N.GT.N3DP) GO TO 206
	         NPPNT = N
	         fday = scet_3DP(n) - SDAY
	         YY(N) = EF20(N)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 206	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(8,'J,20 KEV')
	  ENDIF
C
	  IF(JW.EQ.7) THEN		!	PLOT 34 KEV FLUX
	    N = 0
	    N7 = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       N7 = N7+1
	       IF(N7.GT.N3DP) GO TO 207
	       IF(EF34(N7).GT.0.) THEN
	         N = N+1
	         NPPNT = N
	         fday = scet_3DP(n) - SDAY
	         YY(N) = EF34(N7)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 207	    CONTINUE
	    CALL MGOYLABEL(8,'J,34 KEV')
	  ENDIF
C
	  IF(JW.EQ.8) THEN		!	PLOT 90 KEV FLUX
	    N = 0
	    FDAY = 0.
	    N8 = 0    
	    DOWHILE(FDAY.LE.FDAYL)
	       N8 = N8+1
	       IF(N8.GT.N3DP) GO TO 208
	       IF(EF90(N8).GE.0.) THEN
	         N = N+1
	         NPPNT = N
 	         fday = scet_3DP(n) - SDAY
	         YY(N) = EF90(N8)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 208	    CONTINUE
	    CALL MGOYLABEL(7,'J,90 KEV')
	  ENDIF
C
	  IF(JW.EQ.9) THEN		!	PLOT ION TEMPERTURE
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N9 = N9+1
	         IF(N9.GE.N3DP) GO TO 209
C	print*,'ptemp,fday,fdayl,n,n3dp',ptemp(N9),fday,n,n9
		 IF(PTEMP(N9).GT.0.) THEN
	           N = N+1
	           NPPNT = N
 	           fday = scet_3DP(n9) - SDAY
 	 	   PP(N) = fday*tfact
	           YY(N) = PTEMP(N9)
	           YMAX = AMAX1(YY(N),YMAX)
	           YMIN = AMIN1(YY(N),YMIN)
		 ENDIF
	    ENDDO
 209	    CONTINUE
	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(11,'ION TEMP EV')
	  ENDIF
C
 200	  CONTINUE
	  YRANGE = YMAX - YMIN
	  PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	PRINT*,'JW,GX1,GY1,GY2 WINDOW',JW,GX1,GY1,GY2
	  YMAX = YMAX + .02*YRANGE
	  YMIN = YMIN - .02*YRANGE
	  GYRANGE = GY2-GY1
	  GY2SAV = GY2
	  GY2NEW = GY2 + .2*GYRANGE
	  GXRANGE = GX2 - GX1
	  GX1SAV = GX1
	  GX1NEW = GX1 + .04*GXRANGE
C	  CALL MGOTICKSIZE(6.,1.,0.,0.)
C	  IF(ITERM.LT.0) THEN
C	PRINT*,'range',JW,GXRANGE,GYRANGE
	PRINT*,'BEFORE SETLOC',GX1,GY1,GX2,GY2
	    CALL MGOSETLOC(GX1NEW,GY1,GX2,GY2NEW)
	PRINT*,'AFTER SETLOC',GX1,GY1,GX2,GY2
C	  ENDIF
	  CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
C  	  DO N = 1,NPPNT
C	    YY(N) = AMIN1(YY(N),1.E10)
C	    YY(N) = AMAX1(YY(N),-1.E10)
C	  ENDDO
	  CALL MGOCONNECT(PP,YY,NPPNT)
C	  CALL MGOSETEXPAND(.7)
C	  CALL MGOBOX(1,0)
	  CALL MGOSETEXPAND(.5)
C	  CALL MGOBOX(0,2)
	  CALL MGOBOX(1,2)
	  CALL MGOSETEXPAND(1.)
C
C		PRINT*,'AFTER RESTORE',GX1,GY1,GX2,GY2
	  IF(JW.EQ.NW) THEN
	        CALL MGOSETEXPAND(.8)
		CALL MGOPLOTID(FILE(31:42),'[.WIND]CDFPLOT2')
C	        CALL MGOPLOTID(FILE(52:63),'[.WIND]CDFPLOT2')
	  ENDIF
	  CALL MGOSETLOC(GX1SAV,GY1,GX2,GY2SAV)
C
	ENDDO			!END JW=1,8 DO LOOP
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

