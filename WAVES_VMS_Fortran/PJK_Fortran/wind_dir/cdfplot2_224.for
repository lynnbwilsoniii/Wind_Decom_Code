    	PROGRAM CDFPLOT2
C
C	PLOTS CDF DATA, for examplt WIND_MFI_BX(GSE)_R4 or 
C		WIND_SWE_DENSITY_R4
C	altered 11 Oct 1996 to plot 3dp,swe, and waves densities
C	CAN PRODUCE SEVERAL SHEETS:  SUBROUTINES ARE:
C		TPLOT1,  BMAG,V dot B, DENSITY, SW VEL,VX
C		TPLOT 3DP     FLUXES IN MANY ENERGY CHANNELS
C		ORBPLOT	      GEOTAIL V.S. WIND AND WIND V.S. TIME
C		BPLOT	      3 COMPONENTS OF B AND BMAG	
C
C		YYYYMMDD MUST BE ENTERED MANUALLY IN TPLOT2
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
	character*20	MFI_FILE
	character*4	event
	parameter	size=2048
	integer*4	return_size
	integer*4	temp_waves,iend,n2,S_SCET(2)
	character*32	s
	real*8		mfi_scet(2000)
	real*8		SCET_SWE(2000)
	real*8		scet_gtail(200),gtx(200),gty(200),gtz(200),xyz(3)
	real*8		scet_wind(200),windx(200),windy(200),windz(200)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		bphi(2000),bth(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ptemp(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		ef176(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	real*8		SCET_TNR(2000),scet_3dp(2000)
	real*8		scet,scettds,scetfill
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
!
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,SCET_SWE,vx,vy,vz,vmag,densswe
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,ef176,PTEMP
	common /pltblk4/ ntnr,SCET_TNR,denstnr,dds
	common /pltblk5/ nGTAIL,SCET_GTAIL,GTX,GTY,GTZ
	common /pltblk6/ nWIND ,SCET_WIND,WINDX,WINDY,WINDZ

C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CDFCH,hkch,fillch,ch
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

	ok = w_channel_open(CDFCH,stream)
	if (.not. ok) stop 'Cannot open cdf channel'
	scettds = 0.
	call w_channel_position(CDFCH,scettds)
	print*,'cdf file starts at scettds',scettds
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set cdf channel position to',scettds
	call w_channel_position(CDFCH,scettds)
	print*,'cdf channel position set to',scettds
c
	ok = w_channel_filename(CDFCH,file)
C	write(87,*) file
	print*,file
c
	get_tm_stream = 1
c
	call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	call w_ur8_to_ydoy(scetfill,yyyy,doy,msday)

	  ok = w_event(CDFCH,'CDF')
	if(ok.eq.82) then
	   scettds = 10000.       	! artificially large
	   print*,'end of file'
	   if (.not. ok) stop 'cannot get tds event'
	else
	  item = 'EVENT_SCET'
	  ok = w_item_I4(CDFCH, item, S_scet, 2, return_size)
	  item = 'WIND_MFI_SCET_R8'
	  ok = w_item_r8(CDFCH, item, mfi_scet, 2000, return_size)
	  nmfi = return_size
	  print*,'initial mfi time',mfi_scet(1),' size',return_size
	  item = 'WIND_MFI_BX(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, BX, 2000, return_size)
	  item = 'WIND_MFI_BY(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, BY, 2000, return_size)
	  item = 'WIND_MFI_BZ(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, BZ, 2000, return_size)
	  item = 'WIND_MFI_BMAG_R4'
	  ok = w_item_r4(CDFCH, item, BMAG, 2000, return_size)
	  item = 'WIND_MFI_BPHI(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, BPHI, 2000, return_size)
	  item = 'WIND_MFI_BTHETA(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, BTH, 2000, return_size)
C
	  item = 'MOON_ORBIT_XYZ(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, XYZ, 3, return_size)
	  print*,'moon',xyz
C	  if(1) stop 'moon'
C
	  IF(NMFI.NE.0) THEN
C		print*,'last mfi time',mfi_scet(return_size)
C		do nn = 1,nmfi
C		  write(77,77) mfi_scet(nn),bx(nn),by(nn),bz(nn),bmag(nn)
C     1			,bphi(nn),bth(nn)
C  77		format(f18.12,4f9.3,2F8.1)
C		enddo
C	  elseif(nmfi.eq.0) then
	  ELSE
		PRINT*,'NO MFI DATA'
		NMFI = 0
 		  WRITE(MFI_FILE,765),S_SCET(1)
 765		  FORMAT('[.STUFF]',I8,'.MFI')
		  PRINT*,'OPEN MFI FILE:  ',MFI_FILE
		OPEN(UNIT=99,FILE=MFI_FILE,TYPE='OLD',READONLY)
		READ(99,1097) JUNK
		READ(99,1097) JUNK
 1097		FORMAT(A120)
 1099		FORMAT(A10,I3,1X,I2,1X,F6.3,4F14.4)
 1098		READ(99,1099,END=1199) JUNK,NHR,MIN,SEC,BMAGT,BXT,BYT,BZT
		MFI_SCET(NMFI+1) = DFLOAT(DDS) + NHR/24.D00
     1		 + MIN/1440.D00 + SEC/86400.D00
		IF(MFI_SCET(NMFI+1).LT.SCETTDS) GO TO 1098
		NMFI = NMFI+1
		BX(NMFI) = BXT
		BY(NMFI) = BYT
		BZ(NMFI) = BZT
		BMAG(NMFI) = BMAGT
		GO TO 1098
 1199		CLOSE(UNIT=99)
		print*,'last mfi and time',NMFI,mfi_scet(NMFI)
		print*,'values',BMAG(NMFI),BX(NMFI),BY(NMFI),BZ(NMFI)
	  ENDIF
	  item = 'WIND_swe_VX(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, VX, 2000, return_size)
	  item = 'WIND_swe_VY(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, VY, 2000, return_size)
	  item = 'WIND_swe_VZ(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, VZ, 2000, return_size)
	  item = 'WIND_swe_VMAG_R4'
	  ok = w_item_r4(CDFCH, item, VMAG, 2000, return_size)
	  item = 'WIND_swe_density_R4'
	  ok = w_item_r4(CDFCH, item, densswe, 2000, return_size)
	  nswe = return_size
	  item = 'WIND_SWE_SCET_R8'
	  ok = w_item_r8(CDFCH, item, SCET_SWE, 2000, return_size)
	  print*,'initial swe time',SCET_SWE(1),' size',return_size
	  print*,'swe dens(1)',densswe(1),'size',return_size
c
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(CDFCH, item, dens3dp, 2000, return_size)
	  n3dp = return_size
	  print*,'3dp dens(1)',dens3dp(1),'size',return_size
	  item = 'WIND_3DP_ION_VX(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, VX3, 2000, return_size)
	  item = 'WIND_3DP_ION_VY(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, VY3, 2000, return_size)
	  item = 'WIND_3DP_ION_VZ(GSE)_R4'
	  ok = w_item_r4(CDFCH, item, VZ3, 2000, return_size)
	  item = 'WIND_3DP_ION_TEMP_R4'
	  ok = w_item_r4(CDFCH, item, PTEMP, 2000, return_size)
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(CDFCH, item, ETEMP, 2000, return_size)
	  item = 'WIND_3DP_E_QDOTB_R4'
	  ok = w_item_r4(CDFCH, item, EFQ, 2000, return_size)
	  item = 'WIND_3DP_EF_250EV_R4'
	  ok = w_item_r4(CDFCH, item, EF250, 2000, return_size)
	  item = 'WIND_3DP_EF_1KEV_R4'
	  ok = w_item_r4(CDFCH, item, EF1, 2000, return_size)
	  item = 'WIND_3DP_EF_5KEV_R4'
	  ok = w_item_r4(CDFCH, item, EF5, 2000, return_size)
	  item = 'WIND_3DP_EF_20KEV_R4'
	  ok = w_item_r4(CDFCH, item, EF20, 2000, return_size)
	  item = 'WIND_3DP_EF_34KEV_R4'
	  ok = w_item_r4(CDFCH, item, EF34, 2000, return_size)
	  item = 'WIND_3DP_EF_90KEV_R4'
	  ok = w_item_r4(CDFCH, item, EF90, 2000, return_size)
	  item = 'WIND_3DP_EF_176KEV_R4'
	  ok = w_item_r4(CDFCH, item, EF176, 2000, return_size)
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(CDFCH, item, SCET_3DP, 2000, return_size)
c
C	print*,'3dp,time,vx,vy,vz',scet_3dp(1),vx3(1),vy3(1),vz3(1)
C	if(1) stop
c
	  item = 'WIND_wav_ne_R4'
	  ok = w_item_r4(CDFCH, item, denstnr, 2000, return_size)
	  item = 'WIND_wav_scet_R8'
	  ok = w_item_r8(CDFCH, item, SCET_TNR, 2000, return_size)
	  print*,'tnr dens(1)',denstnr(1),'size',return_size
	  ntnr = return_size
c
	  item = 'WIND_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, WINDX, 200, return_size)
	  NWIND = return_size
	  item = 'WIND_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, WINDY, 200, return_size)
	  item = 'WIND_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, WINDZ, 200, return_size)
	  item = 'WIND_ORBIT_scet_R8'
	  ok = w_item_r8(CDFCH, item, SCET_WIND, 200, return_size)
	  print*,'WIND size',return_size
	  print*,'at start of day,x,y,z',windx(1)/re,windy(1)/re,
     1		windz(1)/re
	  print*,'at end of day,  x,y,z',windx(nwind)/re,
     1    windy(nwind)/re,windz(nwind)/re
C
	  item = 'GEOTAIL_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, GTX, 200, return_size)
	  nWIND = return_size
	  item = 'GEOTAIL_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, GTY, 200, return_size)
	  item = 'GEOTAIL_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, GTZ, 200, return_size)
	  item = 'GEOTAIL_ORBIT_scet_R8'
	  ok = w_item_r8(CDFCH, item, SCET_GTAIL, 200, return_size)
	  print*,'GEOTAIL size',return_size
	  ngtail=return_size
	endif
c
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	   ihrmn = 100*hh+mm

	
c	   write(s,'(i8.8,i6.6)',iostat=ios) mfi_scet(1), mfi_scet(2)
C	   mfi_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
C	CALL TPLOT3DP(3)
	CALL TPLOT3DP(-1)
	DELAY = SECNDS(0.)
33	DELTA = SECNDS(DELAY)
	IF(DELTA.LT.2.) GO TO 33
C	CALL ORBPLOT(-1)
	IF(NMFI.NE.0) CALL BPLOT(-3)
	DELAY = SECNDS(0.)
34	DELTA = SECNDS(DELAY)
	IF(DELTA.LT.2.) GO TO 34
c	CALL TPLOT1(-1)
	CALL TPLOT1(-3)			! NOPRINT
C	CALL TPLOT1(3)
	CALL TPLOT2(-3)
C	NPLOTS=NPLOTS+1
	STOP
	END
	SUBROUTINE TPLOT1(ITERM)
C
C	PLOT DATA V.S. TIME
C
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	CHARACTER*1 DISPOSE
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,SCET_SWE,vx,vy,vz,vmag,densswe
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,ef176,PTEMP
	common /pltblk4/ ntnr,SCET_TNR,denstnr,ndds
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
	real*8		SCET_SWE(2000),SCET_TNR(2000),SCET_3DP(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		ef176(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	integer*4 	nmfi,nswe,N3DP,NTNR
	DIMENSION YY(2048),PP(2048),TMFI(2048)
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
C
	XEND = 2250.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,XEND,3100.)
	ENDIF
C
	NDDS = MFI_SCET(1)
	SDAY = NDDS
	TFACT = 24.
	XST = TFACT*FDAYS
	XLST = TFACT*FDAYL
	XRANGE = XLST-XST
 	XST = XST - .005*XRANGE
 	XLST = XLST + .005*XRANGE
C
	NW = 5
	DO JW = 1,NW
	  print*,'start jw =',jw
	  CALL MGOWINDOW(1,NW,JW) 
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
	  IF(JW.EQ.1.AND.NMFI.NE.0) THEN     	! 	PLOT MAGNETIC FIELD
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
 201	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(5,'B mag')
	    WRITE(STR,1001) FILE(31:34),FILE(35:36),FILE(37:38)
 1001	    FORMAT('HOURS OF ',A4,'/',A2,'/',A2)
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOXLABEL(19,STR)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOBOX(1,2)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.2.AND.NMFI.NE.0) THEN		! 	PLOT V.B / B
	    YMIN = 1500.
	    YMAX = -1500.
	    N = 0
	    NPPNT = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE) GO TO 202
C		B measurements are more frequent, so interpolate
c		B at SWE time
	      SWETIME = SCET_SWE(N)
	      CALL INTERP(TMFI,BX,NMFI,SWETIME,BXINT,ERR)
	      CALL INTERP(TMFI,BY,NMFI,SWETIME,BYINT,ERR)
	      CALL INTERP(TMFI,BZ,NMFI,SWETIME,BZINT,ERR)
	      CALL INTERP(TMFI,BMAG,NMFI,SWETIME,BMINT,ERR)
	      YYT = (VX(N)*BXINT + VY(N)*BYINT + VZ(N)*BZINT)/BMINT
	      IF(YYT.LT.1500..AND.YYT.GT.-1500.) THEN
	       NPPNT = NPPNT+1
	       fday = SWETIME - SDAY
  	       PP(NPPNT) = fday*tfact
	       YY(NPPNT) = YYT
	       YMAX = AMAX1(YYT,YMAX)
	       YMIN = AMIN1(YYT,YMIN)
	      ENDIF
	    ENDDO
 202	    CONTINUE
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(8,'V.B/Bmag')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOBOX(1,2)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.3) THEN			! PLOT DENSITIES
	    N = 0
	    NT = 0
	    NPPNT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.N3DP) GO TO 204
	      YYT = DENS3DP(NT)
	      IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	       N = N+1
	       fday = SCET_3DP(NT) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENS3DP(NT)
	       YMAX = AMAX1(YY(N),YMAX)
	       YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 204	CONTINUE
C
C	SET PLOT LIMITS, LABELS, ETC.
C
	  IF(NPPNT.NE.0) THEN
C
C	    DO N = 1,NPPNT
C	      YY(N) = AMIN1(YY(N),1000.)
C	      YY(N) = AMAX1(YY(N),0.)
C	    ENDDO	    
	    YMIN = AMAX1(YMIN,0.)
	    YMIN = AMIN1(YMIN,200.)
	    YMAX = AMIN1(YMAX,500.)
	    YMAX = AMAX1(YMAX,0.)
	    YRANGE = YMAX - YMIN
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    YMAX = YMAX + .02*YRANGE
	    YMIN = YMIN - .02*YRANGE
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(7,'DENSITY')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
C
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOBOX(1,2)
	    YLOC = .1*YMIN + .9*YMAX
	    CALL MGORELOCATE(XST+.2*XRANGE,YLOC)
	    CALL MGODRAW(XST+.25*XRANGE,YLOC)
	    CALL MGOLABEL(4,' 3DP')
	    CALL MGOSETEXPAND(.7)
	  ENDIF
C
	    N = 0
	    NT = 0
	    NPPNT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      YYT = DENSSWE(NT)
	      IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	       N = N+1
	       IF(N.GT.NSWE) GO TO 203
	       fday = SCET_SWE(NT) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENSSWE(NT)
	       IF(YY(N).LT.100.)YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 203	    CONTINUE
	  IF(NPPNT.NE.0) THEN
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGORELOCATE(XST+.4*XRANGE,YLOC)
	    CALL MGODRAW(XST+.45*XRANGE,YLOC)
	    CALL MGOLABEL(4,' SWE')
	    CALL MGOSETEXPAND(.7)
	  ENDIF
C
	    N = 0
	    NT = 0
	    NPPNT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.NTNR) GO TO 205
	       YYT = DENSTNR(NT)
	       IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	         N = N+1
	         fday = SCET_TNR(NT) - sday
  	         PP(N) = FDAY*TFACT
	         NPPNT = N
	         YY(N) = DENSTNR(NT)
	         YMAX = AMAX1(YY(N),YMAX)
	         IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 205	    CALL MGOSETLTYPE(2)
	  IF(NPPNT.NE.0) THEN
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGORELOCATE(XST+.6*XRANGE,YLOC)
	    CALL MGODRAW(XST+.65*XRANGE,YLOC)
	    CALL MGOLABEL(4,' TNR')
	    CALL MGOSETEXPAND(.7)
	   ENDIF
	   CALL MGOSETLTYPE(0)
	   GO TO 207
	  ENDIF				!  END IF(JW.EQ.3)
C
	  IF(JW.EQ.4) THEN		!	PLOT SPEEDS
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.N3DP) GO TO 208
	       YYT = VX3(NT)
 	       IF(ABS(YYT).LT.1500.) THEN
	         N = N+1
	         fday = SCET_3DP(NT) - sday
  	         PP(N) = fday*tfact
	         NPPNT = N
	         YY(N) = SQRT(VX3(NT)**2 + VY3(NT)**2 + VZ3(NT)**2)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 208	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(9,'S.W.SPEED')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(1.)
C
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.NSWE) GO TO 206
	      YYT = VMAG(NT)
	      IF(YYT.GT.0.AND.YYT.LT.1500.) THEN
	       N = N+1
	       fday = SCET_SWE(NT) - sday
  	       PP(N) = fday*tfact
	       NPPNT = N
	       YY(N) = VMAG(NT)
	       YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 206	    YMIN = AMAX1(YMIN,0.)
	print*,'jw.eq.4 got past 206'
	   IF(NPPNT.NE.0) THEN
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
c	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	   ENDIF
           GO TO 207
	  ENDIF
C
	  IF(JW.EQ.5) THEN		!	PLOT VX
	    YMAX = 0.
	    YMIN = 0.
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.N3DP) GO TO 209
	       YYT = VX3(NT)
 	       IF(ABS(YYT).LT.1500.) THEN
	         N = N+1
	         fday = SCET_3DP(NT) - sday
  	         PP(N) = fday*tfact
	         NPPNT = N
	         YY(N) = YYT
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
C
 209	    CONTINUE
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    YMIN = AMAX1(YMIN,-1500.)
	    YMAX = AMIN1(YMAX, 1500.)
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(3,' VX')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(1.)
C
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.NSWE) GO TO 210
	      YYT = VX(NT)
	      IF(YYT.GT.-1500..AND.YYT.LT.1500.) THEN
	       N = N+1
	       fday = SCET_SWE(NT) - sday
  	       PP(N) = fday*tfact
	       NPPNT = N
	       YY(N) = YYT
	       YMAX = AMAX1(YY(N),YMAX)
	       YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 210	    CONTINUE
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    CALL MGOYLABEL(3,' VX')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(1.)
	  ENDIF
C
 207	   CONTINUE
	print*,'got to 207, jw=',jw
C
	  IF(JW.EQ.5)CALL MGOPLOTID(FILE(31:42),'[.WIND]CDFPLOT2')
C	  IF(JW.EQ.5)CALL MGOPLOTID(FILE(52:63),'[.WIND]CDFPLOT2')
C
	ENDDO			!END JW=1,5 DO LOOP
C
 	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  READ(5,1234) DISPOSE
 1234	  FORMAT(A)
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END	
	SUBROUTINE TPLOT2(ITERM)
C
C	PLOT DATA V.S. TIME, DATA PLOTTED ARE BMAG,BX,DENSITY,SWSPEED,VX,
C	TDSAMP,TDSFREQ, = 7 WINDOWS
C		IT HAS BEEN MODIFIED FOR TERMINAL PRINTING
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	CHARACTER*1 TorF
	CHARACTER*1 DISPOSE
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,SCET_SWE,vx,vy,vz,vmag,densswe
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,ef176,PTEMP(2000)
	common /pltblk4/ ntnr,SCET_TNR,denstnr,ndds
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
	real*8		SCET_SWE(2000),SCET_TNR(2000),SCET_3DP(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	real*4		tdsfreq(1000),TDSTIME(1000)
	integer*4 	nmfi,nswe,N3DP,NTNR,SCETI4(2),HH
	DIMENSION YY(2048),PP(2048),TMFI(2048)
C
	NSCET = 20010221
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
C
	XEND = 2250.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,XEND,3100.)
	ELSE
	  GX1CALL=90.
	  GX2CALL = 950.
	  GY1CALL = 40.
	  GY2CALL = 750.
	  CALL MGOSETLOC(GX1CALL,GY1CALL,GX2CALL,GY2CALL)
	ENDIF
C
	NDDS = MFI_SCET(1)
	SDAY = NDDS
	TFACT = 24.
	XST = TFACT*FDAYS
	XLST = TFACT*FDAYL
	XRANGE = XLST-XST
 	XST = XST - .005*XRANGE
 	XLST = XLST + .005*XRANGE
C
	NW = 7
	DO JW = 1,NW
c	  print*,'start jw =',jw
	  GYSAVE = GY2
	  IF(ITERM.LT.0) THEN
	    CALL MGOWINDOW(1,NW,JW) 
	  ELSE
  	  GX1 = GX1CALL
	  GX2 = GX2CALL
	  GY1N = GY1CALL + (GY2CALL-GY1CALL)*(JW-1)/NW
	  GY2N = GY1CALL + (GY2CALL-GY1CALL)*(JW-.2)/NW
c	write(77,*) 'jwG',jw,gx1,gx2,gy1,gy2
	  CALL MGOSETLOC(GX1CALL,GY1N,GX2CALL,GY2N)
c	write(77,*) 'jwN',jw,gx1,gx2,gy1,gy2
	  ENDIF
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
	  IF(JW.EQ.1.AND.NMFI.NE.0) THEN     	! 	PLOT MAGNETIC FIELD
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
 201	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOYLABEL(5,'B mag')
	    WRITE(STR,1001) FILE(31:34),FILE(35:36),FILE(37:38)
 1001	    FORMAT('HOURS OF ',A4,'/',A2,'/',A2)
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOXLABEL(19,STR)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOBOX(1,2)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.2.AND.NMFI.NE.0) THEN		! 	PLOT BX
	    YMIN = 1500.
	    YMAX = -1500.
	    N = 0
	    NPPNT = 0
	    DOWHILE(FDAY.LE.FDAYL)
	      N = N+1
	      IF(N.GT.NMFI) GO TO 202
	      fday = mfi_scet(n) - SDAY
  	      TMFI(N) = fday*tfact
  	      PP(N) = fday*tfact
	      NPPNT = N
	      YY(N) = BX(N)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
C	      ENDIF
	    ENDDO
 202	    CONTINUE
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(2,'BX')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOBOX(1,2)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.3) THEN			! PLOT DENSITIES
	    N = 0
	    NT = 0
	    NPPNT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.N3DP) GO TO 204
	      YYT = DENS3DP(NT)
	      IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	       N = N+1
	       fday = SCET_3DP(NT) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENS3DP(NT)
	       YMAX = AMAX1(YY(N),YMAX)
	       YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 204	CONTINUE
C
C	SET PLOT LIMITS, LABELS, ETC.
C
	  IF(NPPNT.NE.0) THEN
C
	    YMIN = AMAX1(YMIN,0.)
	    YMIN = AMIN1(YMIN,200.)
	    YMAX = AMIN1(YMAX,200.)
	    YMAX = AMAX1(YMAX,0.)
	    YRANGE = YMAX - YMIN
C	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    YMAX = YMAX + .02*YRANGE
	    YMIN = YMIN - .02*YRANGE
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(7,'DENSITY')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
C
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOBOX(1,2)
	    YLOC = .1*YMIN + .9*YMAX
	    CALL MGORELOCATE(XST+.2*XRANGE,YLOC)
	    CALL MGODRAW(XST+.25*XRANGE,YLOC)
	    CALL MGOLABEL(4,' 3DP')
	    CALL MGOSETEXPAND(.7)
	  ENDIF
C
	    N = 0
	    NT = 0
	    NPPNT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      YYT = DENSSWE(NT)
	      IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	       N = N+1
	       IF(N.GT.NSWE) GO TO 203
	       fday = SCET_SWE(NT) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENSSWE(NT)
	       IF(YY(N).LT.100.)YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 203	    CONTINUE
	  IF(NPPNT.NE.0) THEN
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGORELOCATE(XST+.4*XRANGE,YLOC)
	    CALL MGODRAW(XST+.45*XRANGE,YLOC)
	    CALL MGOLABEL(4,' SWE')
	    CALL MGOSETEXPAND(.7)
	  ENDIF
C
	    N = 0
	    NT = 0
	    NPPNT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.NTNR) GO TO 205
	       YYT = DENSTNR(NT)
	       IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	         N = N+1
	         fday = SCET_TNR(NT) - sday
  	         PP(N) = FDAY*TFACT
	         NPPNT = N
	         YY(N) = DENSTNR(NT)
	         YMAX = AMAX1(YY(N),YMAX)
	         IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 205	    CALL MGOSETLTYPE(2)
	  IF(NPPNT.NE.0) THEN
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGORELOCATE(XST+.6*XRANGE,YLOC)
	    CALL MGODRAW(XST+.65*XRANGE,YLOC)
	    CALL MGOLABEL(4,' TNR')
	    CALL MGOSETEXPAND(.7)
	   ENDIF
	   CALL MGOSETLTYPE(0)
	   GO TO 207
	  ENDIF				!  END IF(JW.EQ.3)
C
	  IF(JW.EQ.4) THEN		!	PLOT SW SPEED
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.N3DP) GO TO 208
	       YYT = VX3(NT)
 	       IF(ABS(YYT).LT.1500.) THEN
	         N = N+1
	         fday = SCET_3DP(NT) - sday
  	         PP(N) = fday*tfact
	         NPPNT = N
	         YY(N) = SQRT(VX3(NT)**2 + VY3(NT)**2 + VZ3(NT)**2)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 208	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(9,'S.W.SPEED')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(1.)
C
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.NSWE) GO TO 206
	      YYT = VMAG(NT)
	      IF(YYT.GT.0.AND.YYT.LT.1500.) THEN
	       N = N+1
	       fday = SCET_SWE(NT) - sday
  	       PP(N) = fday*tfact
	       NPPNT = N
	       YY(N) = VMAG(NT)
	       YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 206	    YMIN = AMAX1(YMIN,0.)
C	print*,'jw.eq.4 got past 206'
	   IF(NPPNT.NE.0) THEN
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
c	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
C	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	   ENDIF
           GO TO 207
	  ENDIF
C
	  IF(JW.EQ.5) THEN		!	PLOT VX
	    YMAX = 0.
	    YMIN = 0.
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.N3DP) GO TO 209
	       YYT = VX3(NT)
 	       IF(ABS(YYT).LT.1500.) THEN
	         N = N+1
	         fday = SCET_3DP(NT) - sday
  	         PP(N) = fday*tfact
	         NPPNT = N
	         YY(N) = YYT
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
C
 209	    CONTINUE
C	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    YMIN = AMAX1(YMIN,-1500.)
	    YMAX = AMIN1(YMAX, 1500.)
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOYLABEL(2,'VX')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(0)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(1.)
C
	    N = 0
	    NT = 0
	    NPPNT=0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.NSWE) GO TO 210
	      YYT = VX(NT)
	      IF(YYT.GT.-1500..AND.YYT.LT.1500.) THEN
	       N = N+1
	       fday = SCET_SWE(NT) - sday
  	       PP(N) = fday*tfact
	       NPPNT = N
	       YY(N) = YYT
	       YMAX = AMAX1(YY(N),YMAX)
	       YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 210	    CONTINUE
C	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
C	    CALL MGOYLABEL(3,' VX')
C	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(1.)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.6) THEN		!	PLOT TDS AMPS
	    OPEN(UNIT=56,FILE='MAKEFILE18.RESULTS',STATUS='OLD',READONLY)
	    N = 0
	    NT = 0
	    NPPNT=0
	    YMIN = 0.
	    YMAX = 50.
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
C
 1113	    CONTINUE
	    READ(56,1111,END=1112) SCETI4,NO_EVT,TorF,ISPS,IZCNT,FREQHZ,
     1	    FAVR,FREQN,ZCBW,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE
C 1111	    FORMAT(I10,I8,I10,A2,I2,I4,3F6.0,2F6.2,F7.0,F6.1,3F7.1)
C	1111 IS NOW 114 CHARACTERS
 1111	FORMAT(I10,I8,I10,A2,I2,I4,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1		,F6.3)
C
C	    CALCULATE AND TEST TIME	   
	    IF(SCETI4(1).NE.NSCET) GO TO 1113
	    HH = SCETI4(2)/100
	    MIN = MOD(HH,100)
	    HH = HH/100
	    TDST = HH/24. + MIN/1440.
	    IF(TDST.LT.FDAYS.OR.TDST.GT.FDAYL) GO TO 1113
	    TDST = TDST*TFACT
	write(92,*)'t2,no_evt,favr,emax,xre',no_evt,favr,emax,xre
	    CALL MGOPOINTS(60.,1,TDST,EMAX,1)
C
	    N = N+1
	    NT = NT+1
	    NPPNT=NPPNT+1
	    TDSFREQ(NPPNT) = .001*FAVR
	    TDSTIME(NPPNT) = TDST
	    GO TO 1113
C
 1112	    CONTINUE
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOYLABEL(8,'AMP mV/m')
	    CLOSE(UNIT=56)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.7) THEN		!	PLOT TDS FREQS
	    YMAX = 11.
	    YMIN = 0.
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOYLABEL(5,'f kHz')
	    CALL MGOPOINTS(60.,1,TDSTIME,TDSFREQ,NPPNT)
	    CALL MGOBOX(1,2)
	  ENDIF
C
 207	   CONTINUE
C	   print*,'got to 207, jw=',jw
C
	  CALL MGOSETEXPAND(.6)
	  IF(JW.EQ.NW)CALL MGOPLOTID(FILE(31:39),'[.WIND]CDFPLOT2')
C	  IF(JW.EQ.NW)CALL MGOPLOTID(FILE(52:63),'[.WIND]CDFPLOT2')
C
	ENDDO			!END JW=1,NW DO LOOP
C
 	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  READ(5,1234) DISPOSE
 1234	  FORMAT(A)
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
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
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,SCET_SWE,vx,vy,vz,vmag,densswe
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,ef176,PTEMP(2000)
	common /pltblk4/ ntnr,SCET_TNR,denstnr,ndds
	common /pltblk6/ nWIND ,SCET_WIND,XGSE,YGSE,ZGSE
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
	real*8		SCET_SWE(2000),SCET_TNR(2000),SCET_3DP(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		ef176(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	REAL*4		TORB(200)
	real*8		scet_wind(200),XGSE(200),YGSE(200),ZGSE(200)
	integer*4 	nmfi,nswe,N3DP,NTNR,NWIND
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
	NW = 10
C
C	JW=1 IS	DIFF   		=2 IS E_TEMP		=3 IS 250EV
C	JW=4 IS 1 KEV		=5 IS 5 KEV		=6 IS 20 KEV
C	JW=7 IS 34 KEV		=8 IS 90 KEV		=9 is 176 KeV
c	JW=10 IS ION TEMP
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
	      CALL DINTERP(TORB,XGSE,NWIND,PP(N),X0,ERR) 
	      CALL DINTERP(TORB,YGSE,NWIND,PP(N),Y0,ERR)
	      CALL DINTERP(TORB,ZGSE,NWIND,PP(N),Z0,ERR)
	      XRE = X0/RE
	      YRE = Y0/RE
	      ZRE = Z0/RE
	      CALL DIFFFK(XRE,YRE,ZRE,BX(N),BY(N),BZ(N),DDF,XT,YT,ZT,
     1		XI,YI,ZI,IN)
	      YY(N) = AMIN1(DDF,20.)
	      YY(N) = AMAX1(YY(N),-20.)
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
	    YMAX = 0.
	    YMIN = 0.
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
 204	    CONTINUE
	    print*,'1kev, n max,min',n,ymin,ymax
	    YMIN = AMAX1(YMIN,0.)
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
	  IF(JW.EQ.9) THEN		!	PLOT 176 KEV
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N9 = N9+1
	         IF(N9.GE.N3DP) GO TO 209
C	print*,'ptemp,fday,fdayl,n,n3dp',ptemp(N9),fday,n,n9
		 IF(EF176(N9).GT.0.) THEN
	           N = N+1
	           NPPNT = N
 	           fday = scet_3DP(n9) - SDAY
 	 	   PP(N) = fday*tfact
	           YY(N) = EF176(N9)
	           YMAX = AMAX1(YY(N),YMAX)
	           YMIN = AMIN1(YY(N),YMIN)
		 ENDIF
	    ENDDO
 209	    CONTINUE
	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(9,'176 keV')
	  ENDIF
C
	  IF(JW.EQ.10) THEN		!	PLOT ION TEMPERTURE
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N10 = N10+1
	         IF(N10.GE.N3DP) GO TO 210
C	print*,'ptemp,fday,fdayl,n,n3dp',ptemp(N9),fday,n,n9
		 IF(PTEMP(N10).GT.0.) THEN
	           N = N+1
	           NPPNT = N
 	           fday = scet_3DP(n10) - SDAY
 	 	   PP(N) = fday*tfact
	           YY(N) = PTEMP(N10)
	           YMAX = AMAX1(YY(N),YMAX)
	           YMIN = AMIN1(YY(N),YMIN)
		 ENDIF
	    ENDDO
 210	    CONTINUE
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
  	  DO N = 1,NPPNT
	    YY(N) = AMIN1(YY(N),1.E10)
	    YY(N) = AMAX1(YY(N),-1.E10)
	  ENDDO
	print*,'plot nppnt',nppnt
	print*,'connect for jw.nw',jw,nw
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
		CALL MGOPLOTID(FILE(31:42),'[.WIND]CDFPLOT2 TPLOT3DP')
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
	SUBROUTINE ORBPLOT(ITERM)
C
C	PLOT GEOTAIL V.S. WIND AND WIND V.S. TIME
C
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk5/ nGTAIL,SCET_GTAIL,GTX,GTY,GTZ
	common /pltblk6/ nWIND ,SCET_WIND,WINDX,WINDY,WINDZ
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
	real*8		scet_gtail(200),gtx(200),gty(200),gtz(200)
	real*8		gtxINT(200),gtyINT(200),gtzINT(200)
	real*8		scet_wind(200),windx(200),windy(200),windz(200)
	real*8		sday,SWETIME
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	integer*4 	nWIND,nGTAIL
	DIMENSION 	YY(2048),XX(2048),TGT(2048)
	DATA RE /6378./
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	HH = LAST/100
	MM = MOD(LAST,100)
	FDAYL = HH/24. + MM/1440.
	EXAV = 0.
	EYAV = 0.
	EZAV = 0.
C
	XEND = 2000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(600.,400.,XEND,3100.)
	ENDIF
C
	NDDS = SCET_WIND(1)
	SDAY = NDDS
	TFACT = 24.
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NGTAIL) GO TO 202
	       fday = scet_GTAIL(n) - SDAY
  	       TGT(N) = fday*tfact
C	       fday = scet_GTAIL(n) - SDAY
  	       XX(N) = fday*tfact
	       NPPNT = N
	      SWETIME = SCET_WIND(N)
	      CALL DINTERP(TGT,GTX,NGTAIL,SWETIME,GTXINT(N),ERR)
	      CALL DINTERP(TGT,GTY,NGTAIL,SWETIME,GTYINT(N),ERR)
	      CALL DINTERP(TGT,GTZ,NGTAIL,SWETIME,GTZINT(N),ERR)
	    ENDDO
 202	 CONTINUE
	PRINT*,'GTXYZ',GTX(1),GTY(1),GTZ(1)
	PRINT*,'GTINTXYZ',GTXINT(1),GTYINT(1),GTZINT(1)
	PRINT*,'WNXYZ',WINDX(1),WINDY(1),WINDZ(1)
	
C
	NW = 3
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
C
	  IF(JW.EQ.1) THEN
C
C		PLOT Y V.S. X
C
	    DO N = 1,NPPNT
	      YY(N) = (GTYINT(N) - WINDY(N))/RE
	      XX(N) = (GTXINT(N) - WINDX(N))/RE
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	      XMAX = AMAX1(XX(N),XMAX)
	      XMIN = AMIN1(XX(N),XMIN)
	    ENDDO
	    CALL MGOYLABEL(10,'Y (GSE, Re)')
	    CALL MGOXLABEL(18,'X(Re) GT from WIND')
	  ENDIF
C
	  IF(JW.EQ.2) THEN
	    DO N = 1,NPPNT
	      YY(N) = (GTZINT(N) - WINDZ(N))/RE
	      XX(N) = (GTXINT(N) - WINDX(N))/RE
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	      XMAX = AMAX1(XX(N),XMAX)
	      XMIN = AMIN1(XX(N),XMIN)
	    ENDDO
	    CALL MGOYLABEL(10,'Z (GSE, Re)')
	    CALL MGOXLABEL(18,'X(Re) GT from WIND')
	  ENDIF
C
	  IF(JW.EQ.3) THEN
	    DO N = 1,NPPNT
	      YY(N) = WINDY(N)/RE
	      XX(N) = WINDX(N)/RE
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	      XMAX = AMAX1(XX(N),XMAX)
	      XMIN = AMIN1(XX(N),XMIN)
	    ENDDO
	    CALL MGOXLABEL(12,' X WIND (GSE)')
	    CALL MGOYLABEL(12,' Y WIND (GSE)')
	  ENDIF
C
C
 200	  CONTINUE
	  YRANGE = YMAX - YMIN
C	  PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
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
C	CALL MGOPLOTID(FILE(52:63),'[.WIND]CDFPLOT2')
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
	real*8		SCET_SWE(2000),SCET_TNR(2000),SCET_3DP(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	integer*4 	nmfi,nswe,N3DP,NTNR
	DIMENSION YY(2048),PP(2048),TMFI(2048)
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
C
	XEND = 2250.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,XEND,3100.)
	ENDIF
C
	NDDS = MFI_SCET(1)
	SDAY = NDDS
	TFACT = 24.
	XST = TFACT*FDAYS
	XLST = TFACT*FDAYL
	XRANGE = XLST-XST
 	XST = XST - .005*XRANGE
 	XLST = XLST + .005*XRANGE
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
 201	    CONTINUE
C	    XRANGE = PP(NPPNT) - PP(1)
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
 200	  CONTINUE
	  YRANGE = YMAX - YMIN
	  PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	  YMAX = YMAX + .02*YRANGE
	  YMIN = YMIN - .02*YRANGE
c*******
C	  if(jw.eq.1) then
c		ymin = 0.
c		ymax = 35
c	  endif
c	  if(jw.eq.2) then
c		ymin = -10.
c		ymax = 15.
c	  endif
c	  if(jw.eq.3) then
c		ymin = -25.
c		ymax = 16.
c	  endif
c	  if(jw.eq.4) then
c		ymin = -25.
c		ymax = 30.
c	  endif
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
	  IF(JW.EQ.4)CALL MGOPLOTID(FILE(31:42),'[.WIND]CDFPLOT2')
	ENDDO					!END JW=1,4 DO LOOP
C
 	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  IF(1) STOP
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

