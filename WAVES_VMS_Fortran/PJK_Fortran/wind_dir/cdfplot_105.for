 	PROGRAM CDFPLOT
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
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
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
	common /pltblk4/ ntnr,SCET_TNR,denstnr
	common /pltblk5/ nGTAIL,SCET_GTAIL,GTX,GTY,GTZ
	common /pltblk6/ nWIND ,SCET_WIND,scetWINDX,WINDY,WINDZ

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

	ok = w_channel_open(CDFCH,stream)
	if (.not. ok) stop 'Cannot open CDF channel'
	scettds = 0.
	call w_channel_position(CDFCH,scettds)
	print*,'CDF file starts at scettds',scettds
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set CDF channel position to',scettds
	call w_channel_position(CDFCH,scettds)
	print*,'CDF channel position set to',scettds
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
	  IF(NMFI.NE.0) THEN
		print*,'last mfi time',mfi_scet(return_size)
	  elseif(nmfi.eq.0) then
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
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(CDFCH, item, SCET_3DP, 2000, return_size)
c
	  item = 'WIND_wav_ne_R4'
	  ok = w_item_r4(CDFCH, item, denstnr, 2000, return_size)
	  item = 'WIND_wav_scet_R8'
	  ok = w_item_r8(CDFCH, item, SCET_TNR, 2000, return_size)
	  print*,'tnr dens(1)',denstnr(1),'size',return_size
	  ntnr = return_size
c
	  item = 'WIND_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, WINDX, 2000, return_size)
	  nWIND = return_size
	  item = 'WIND_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, WINDY, 2000, return_size)
	  item = 'WIND_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, WINDZ, 2000, return_size)
	  item = 'WIND_ORBIT_scet_R8'
	  ok = w_item_r8(CDFCH, item, SCET_WIND, 2000, return_size)
	  print*,'WIND size',return_size
c
c	write(66,1066) (scet_wind(i),windx(i)/RE,windy(i)/RE,windz(i)/RE
c     1      ,i=1,40)
c 1066	FORMAT(F12.5,3F12.4)
c	if(1) stop
C
	  item = 'GEOTAIL_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, GTX, 2000, return_size)
	  nWIND = return_size
	  item = 'GEOTAIL_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, GTY, 2000, return_size)
	  item = 'GEOTAIL_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(CDFCH, item, GTZ, 2000, return_size)
	  item = 'GEOTAIL_ORBIT_scet_R8'
	  ok = w_item_r8(CDFCH, item, SCET_GTAIL, 2000, return_size)
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
c	CALL PLTDSFR(3)
	CALL ORBPLOT(-1)
	print*,'got to bplot, nmfi=',nmfi
	IF(NMFI.NE.0) CALL BPLOT(-1)
	CALL PLTDSFR(-1)
C	NPLOTS=NPLOTS+1
	STOP
	END
	SUBROUTINE PLTDSFR(ITERM)
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
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
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
	    XLST = PP(NPPNT)
	    XRANGE = PP(NPPNT) - PP(1)
	    CALL MGOYLABEL(5,'B mag')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.2.AND.NMFI.NE.0) THEN		! 	PLOT V.B / B
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE) GO TO 202
	       fday = SCET_SWE(n) - SDAY
  	       PP(N) = fday*tfact
	       NPPNT = N
c	       mm = mfi_scet(2)/100
C	       mm = mod(mm,100)
c	       hh = mfi_scet(2)/10000
C	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
C		B measurements are more frequent, so interpolate
c		B at SWE time
	      SWETIME = SCET_SWE(N)
	      CALL INTERP(TMFI,BX,NMFI,SWETIME,BXINT,ERR)
	      CALL INTERP(TMFI,BY,NMFI,SWETIME,BYINT,ERR)
	      CALL INTERP(TMFI,BZ,NMFI,SWETIME,BZINT,ERR)
	      CALL INTERP(TMFI,BMAG,NMFI,SWETIME,BMINT,ERR)
	      YY(N) = (VX(N)*BXINT + VY(N)*BYINT + VZ(N)*BZINT)/BMINT
	      IF(YY(N).LT.1500.)  YMAX = AMAX1(YY(N),YMAX)
	      IF(YY(N).GT.-1500.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 202	    XRANGE = PP(NPPNT) - PP(1)
c	    XLST = PP(NPPNT)
	    CALL MGOYLABEL(8,'V.B/Bmag')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.3) THEN			! PLOT DENSITIES
	    N = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE) GO TO 203
	       fday = SCET_SWE(n) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENSSWE(N)
	       IF(YY(N).LT.100.)YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 203	    CONTINUE
	    DO N = 1,NPPNT
	      YY(N) = AMIN1(YY(N),1000.)
	      YY(N) = AMAX1(YY(N),0.)
	    ENDDO
	    YMIN = AMAX1(YMIN,0.)
	    XRANGE = PP(NPPNT) - PP(1)
c	    XLST = PP(NPPNT)
 	    XST = PP(1) - .003*XRANGE
  	    XLST = XLST + .003*XRANGE
	    YRANGE = YMAX - YMIN
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    YMAX = YMAX + .02*YRANGE
	    YMIN = YMIN - .02*YRANGE
	    CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
	    CALL MGOYLABEL(7,'DENSITY')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(1.)
	    N = 0
	    NT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	      NT = NT+1
	      IF(NT.GT.N3DP) GO TO 204
	      YYT = DENS3DP(NT)
	      IF(YYT.GE.0..AND.YYT.LE.10000.) THEN
	       N = N+1
	       fday = SCET_3DP(nT) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENS3DP(NT)
	       YMAX = AMAX1(YY(N),YMAX)
	       YMIN = AMIN1(YY(N),YMIN)
	      ENDIF
	    ENDDO
 204	CONTINUE
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    N = 0
	    NT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.NTNR) GO TO 205
	       IF(DENSTNR(NT).GT.0.) THEN
	         N = N+1
	         fday = SCET_TNR(NT) - sday
  	         PP(N) = FDAY*TFACT
	         NPPNT = N
	         YY(N) = DENSTNR(NT)
	         YMAX = AMAX1(YY(N),YMAX)
	         IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	       ENDIF
	    ENDDO
 205	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
 	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    GO TO 207
	  ENDIF
C
	  IF(JW.EQ.4) THEN		!	PLOT SPEEDS
	    N = 0
	    NT = 0
	    FDAY = 0.
	    DOWHILE(FDAY.LE.FDAYL)
	       NT = NT+1
	       IF(NT.GT.N3DP) GO TO 208
	       YYT = VX3(NT)
 	       IF(ABS(YYT).LT.1500.) THEN
	         N = N+1
	         fday = SCET_3DP(n) - sday
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
	    CALL MGOSETLTYPE(1)
	    CALL MGOSETEXPAND(.6)
c	    CALL MGOCONNECT(PP,YY,NPPNT)
	    CALL MGOSETEXPAND(.7)
	    PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	    GO TO 207
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
C
	  IF(JW.EQ.4)CALL MGOPLOTID(FILE(31:42),'[.WIND]CDFPLOT')
C
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
 202	 XLST = XX(NPPNT)
	 XRANGE = XX(NPPNT) - XX(1)
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
 200	  XRANGE = XMAX - XMIN
	  XST = XMIN - .003*XRANGE
	  XLST = XMAX + .003*XRANGE
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
