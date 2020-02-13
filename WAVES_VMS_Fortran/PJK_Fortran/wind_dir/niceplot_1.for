   	PROGRAM CDFPLOT2
C
C	PLOTS CDF DATA, for examplt WIND_MFI_BX(GSE)_R4 or 
C		WIND_SWE_DENSITY_R4
C	altered 11 Oct 1996 to plot 3dp,swe, and waves densities
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
	real*8		scet_gtail(2000),gtx(2000),gty(2000),gtz(2000)
	real*8		scet_wind(2000),windx(2000),windy(2000),windz(2000)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		bphi(2000),bth(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ptemp(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
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
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,PTEMP
	common /pltblk4/ ntnr,SCET_TNR,denstnr
	common /pltblk5/ nGTAIL,SCET_GTAIL,GTX,GTY,GTZ
	common /pltblk6/ nWIND ,SCET_WIND,WINDX,WINDY,WINDZ

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
	  item = 'EVENT_SCET'
	  ok = w_item_I4(CDHFCH, item, S_scet, 2, return_size)
	  item = 'WIND_MFI_SCET_R8'
	  ok = w_item_r8(CDHFCH, item, mfi_scet, 2000, return_size)
	  nmfi = return_size
	  print*,'initial mfi time',mfi_scet(1),' size',return_size
	  item = 'WIND_MFI_BX(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, BX, 2000, return_size)
	  item = 'WIND_MFI_BY(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, BY, 2000, return_size)
	  item = 'WIND_MFI_BZ(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, BZ, 2000, return_size)
	  item = 'WIND_MFI_BMAG_R4'
	  ok = w_item_r4(CDHFCH, item, BMAG, 2000, return_size)
	  item = 'WIND_MFI_BPHI(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, BPHI, 2000, return_size)
	  item = 'WIND_MFI_BTHETA(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, BTH, 2000, return_size)
	  IF(NMFI.NE.0) THEN
		print*,'last mfi time',mfi_scet(return_size)
		do nn = 1,nmfi
		  write(77,77) mfi_scet(nn),bx(nn),by(nn),bz(nn),bmag(nn)
     1			,bphi(nn),bth(nn)
  77		format(f18.12,4f9.3,2F8.1)
		enddo
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
	  ok = w_item_r4(CDHFCH, item, VX, 2000, return_size)
	  item = 'WIND_swe_VY(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VY, 2000, return_size)
	  item = 'WIND_swe_VZ(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VZ, 2000, return_size)
	  item = 'WIND_swe_VMAG_R4'
	  ok = w_item_r4(CDHFCH, item, VMAG, 2000, return_size)
	  item = 'WIND_swe_density_R4'
	  ok = w_item_r4(CDHFCH, item, densswe, 2000, return_size)
	  nswe = return_size
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
	  item = 'WIND_wav_ne_R4'
	  ok = w_item_r4(CDHFCH, item, denstnr, 2000, return_size)
	  item = 'WIND_wav_scet_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_TNR, 2000, return_size)
	  print*,'tnr dens(1)',denstnr(1),'size',return_size
	  ntnr = return_size
c
	  item = 'WIND_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, WINDX, 2000, return_size)
	  NWIND = return_size
	  item = 'WIND_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, WINDY, 2000, return_size)
	  item = 'WIND_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, WINDZ, 2000, return_size)
	  item = 'WIND_ORBIT_scet_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_WIND, 2000, return_size)
	  print*,'WIND size',return_size
	  print*,'at start of day,x,y,z',windx(1)/re,windy(1)/re,
     1		windz(1)/re
	  print*,'at end of day,  x,y,z',windx(nwind)/re,
     1    windy(nwind)/re,windz(nwind)/re
C
	  item = 'GEOTAIL_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, GTX, 2000, return_size)
	  nWIND = return_size
	  item = 'GEOTAIL_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, GTY, 2000, return_size)
	  item = 'GEOTAIL_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDHFCH, item, GTZ, 2000, return_size)
	  item = 'GEOTAIL_ORBIT_scet_R8'
	  ok = w_item_r8(CDHFCH, item, SCET_GTAIL, 2000, return_size)
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
	IF(NMFI.NE.0) CALL BPLOT(-1)
	DELAY = SECNDS(0.)
34	DELTA = SECNDS(DELAY)
	IF(DELTA.LT.2.) GO TO 34
	CALL TPLOT1(-1)
C	CALL TPLOT1(3)
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
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,SCET_SWE,vx,vy,vz,vmag,densswe
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,PTEMP(2000)
	common /pltblk4/ ntnr,SCET_TNR,denstnr
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
	    YMAX = AMIN1(YMAX,200.)
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
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90,PTEMP(2000)
	common /pltblk4/ ntnr,SCET_TNR,denstnr
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
	integer*4 	nmfi,nswe,N3DP,NTNR
	DIMENSION YY(2048),PP(2048)
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
C	JW=1 IS E_TEMP		=2 IS HEAT FLUX		=3 IS 250EV
C	JW=4 IS 1 KEV		=5 IS 5 KEV		=6 IS 20 KEV
C	JW=7 IS 34 KEV		=8 IS 90 KEV	
C
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
	PRINT*,'AFTER WINDOW JW',JW,GX1,GY1,GX2,GY2
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
	  IF(JW.EQ.1) THEN     	! 	PLOT ELECTRON TEMPERATURE
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
	    CALL MGOSETEXPAND(.8)
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETEXPAND(EXP_SAVE)
	  ENDIF
C
	  IF(JW.EQ.2) THEN		! 	PLOT HEAT FLUX
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.N3DP) GO TO 202
	       NPPNT = N
	       fday = scet_3DP(n) - SDAY
c	       mm = mfi_scet(2)/100
C	       mm = mod(mm,100)
c	       hh = mfi_scet(2)/10000
C	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
C		B measurements are more frequent, so interpolate
c		B at SWE time
C	      SWETIME = SCET_SWE(N)
C	      CALL INTERP(TMFI,BX,N3DP,SWETIME,BXINT,ERR)
C	      CALL INTERP(TMFI,BY,N3DP,SWETIME,BYINT,ERR)
C	      CALL INTERP(TMFI,BZ,N3DP,SWETIME,BZINT,ERR)
C	      CALL INTERP(TMFI,BMAG,N3DP,SWETIME,BMINT,ERR)
C	      YY(N) = (VX(N)*BXINT + VY(N)*BYINT + VZ(N)*BZINT)/BMINT
	      YY(N) = AMIN1(EFQ(N),1.E10)
	      YY(N) = AMAX1(YY(N),-1.E10)
	if(yy(n).gt.-1.e-10) print*,'q.b',n,efq(n)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 202	    XRANGE = PP(NPPNT) - PP(1)
	    CALL MGOYLABEL(8,'Q.B')
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
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N = N+1
	         IF(N.GT.N3DP) GO TO 207
	         NPPNT = N
	         fday = scet_3DP(n) - SDAY
	         YY(N) = EF34(N)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 207	    CONTINUE
	    CALL MGOYLABEL(8,'J,34 KEV')
	  ENDIF
C
	  IF(JW.EQ.8) THEN		!	PLOT 90 KEV FLUX
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N = N+1
	         IF(N.GT.N3DP) GO TO 208
	         NPPNT = N
 	         fday = scet_3DP(n) - SDAY
	         YY(N) = EF90(N)
	         YMAX = AMAX1(YY(N),YMAX)
	         YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(7,'J,90 KEV')
 208	    CONTINUE
	  ENDIF
C
	  IF(JW.EQ.9) THEN		!	PLOT 90 KEV FLUX
	    N = 0
	    N9 = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	         N9 = N9+1
	         IF(N9.GE.N3DP) GO TO 209
C	print*,'ptemp,fday,fdayl,n,n3dp',ptemp(N9+1),fday,fdayl,n,n3dp
		 IF(ABS(PTEMP(N9+1)).LT.1000.) THEN
	           N = N+1
	           NPPNT = N
 	           fday = scet_3DP(n) - SDAY
	           YY(N) = PTEMP(N)
	           YMAX = AMAX1(YY(N),YMAX)
	           YMIN = AMIN1(YY(N),YMIN)
		 ENDIF
	    ENDDO
 209	    CONTINUE
	    CALL MGOYLABEL(7,'ION TEMP EV')
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
	real*8		scet_gtail(2000),gtx(2000),gty(2000),gtz(2000)
	real*8		gtxINT(2000),gtyINT(2000),gtzINT(2000)
	real*8		scet_wind(2000),windx(2000),windy(2000),windz(2000)
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

