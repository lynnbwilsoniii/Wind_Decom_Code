	PROGRAM PLOT3DP
C
C	PLOTS CDHF DATA, for examplt WIND_MFI_BX(GSE)_R4 or 
C		WIND_3DP_ION_DENSITY_R4
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
	character*32	s,MFI_FILE
	real*8		mfi_scet(2000)
	real*8		swe_scet(2000)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*8		scet,scettds,scetfill
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds,S_SCET(2)
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
!
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,swe_scet,vx,vy,vz,vmag,dens

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
	  item = 'EVENT_SCET'
	  ok = w_item_i4(CDHFCH, item, s_scet, 2, return_size)
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
	  IF(NMFI.NE.0) THEN
		print*,'last mfi time',mfi_scet(nmfi)
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
C
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(CDHFCH, item, swe_scet, 2000, return_size)
	  nswe = return_size
	  print*,'initial swe time',swe_scet(1),' size',return_size
	  item = 'WIND_3DP_ION_VX(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VX, 2000, return_size)
	  item = 'WIND_3DP_ION_VY(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VY, 2000, return_size)
	  item = 'WIND_3DP_ION_VZ(GSE)_R4'
	  ok = w_item_r4(CDHFCH, item, VZ, 2000, return_size)
C	  item = 'WIND_swe_VMAG_R4'
C	  ok = w_item_r4(CDHFCH, item, VMAG, 2000, return_size)
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(CDHFCH, item, dens, 2000, return_size)
	  print*,'swe dens(1)',dens(1),'size',return_size
	endif
c
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	   ihrmn = 100*hh+mm

	
c	   write(s,'(i8.8,i6.6)',iostat=ios) mfi_scet(1), mfi_scet(2)
C	   mfi_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
c	CALL PLTDSFR(3)
	CALL PLTDSFR(-1)
	CALL VXBPLOT(-2)
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
	common /pltblk2/ nswe,swe_scet,vx,vy,vz,vmag,dens
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
	real*8		swe_scet(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	integer*4 	nmfi,nswe
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
	  IF(JW.EQ.1) THEN
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
	  IF(JW.EQ.2) THEN
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE) GO TO 202
	       fday = swe_scet(n) - SDAY
  	       PP(N) = fday*tfact
	       NPPNT = N
c	       mm = mfi_scet(2)/100
C	       mm = mod(mm,100)
c	       hh = mfi_scet(2)/10000
C	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
C		B measurements are more frequent, so interpolate
c		B at SWE time
	      SWETIME = SWE_SCET(N)
	      CALL INTERP(TMFI,BX,NMFI,SWETIME,BXINT,ERR)
	      CALL INTERP(TMFI,BY,NMFI,SWETIME,BYINT,ERR)
	      CALL INTERP(TMFI,BZ,NMFI,SWETIME,BZINT,ERR)
	      CALL INTERP(TMFI,BMAG,NMFI,SWETIME,BMINT,ERR)
	      YY(N) = (VX(N)*BXINT + VY(N)*BYINT + VZ(N)*BZINT)/BMINT
	      IF(YY(N).LT.1500.)  YMAX = AMAX1(YY(N),YMAX)
	      IF(YY(N).GT.-1500.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 202	    XRANGE = PP(NPPNT) - PP(1)
	    XLST = PP(NPPNT)
	    CALL MGOYLABEL(8,'V.B/Bmag')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.3) THEN
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE) GO TO 203
	       fday = swe_scet(n) - sday
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = DENS(N)
	       YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 203	    YMIN = AMAX1(YMIN,0.)
	    XRANGE = PP(NPPNT) - PP(1)
	    XLST = PP(NPPNT)
	    CALL MGOYLABEL(7,'DENSITY')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.4) THEN
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE) GO TO 204
	       fday = swe_scet(n) - sday
  	       PP(N) = fday*tfact
	       NPPNT = N
c	       ss = mod(swe_scet(2),100)
c	       mm = mfi_scet(2)/100
C	       mm = mod(mm,100)
c	       hh = mfi_scet(2)/10000
C	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
C	      YY(N) = VMAG(N)
	      IF(ABS(VX(N)).LT.10000.) THEN
	        YY(N) = SQRT(VX(N)**2 + VY(N)**2 + VZ(N)**2)
	      ELSEIF(N.GT.1) THEN
	        YY(N) = YY(N-1)
	      ELSE
		YY(N) = YY(N+1)
	      ENDIF
	      YMAX = AMAX1(YY(N),YMAX)
	      IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
C	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 204	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(4,'VMAG')
	    XRANGE = PP(NPPNT) - PP(1)
	    XLST = PP(NPPNT)
	  ENDIF
C
 200	XST = PP(1) - .003*XRANGE
	XLST = XLST + .003*XRANGE
	YRANGE = YMAX - YMIN
	PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	YMAX = YMAX + .02*YRANGE
	YMIN = YMIN - .02*YRANGE
	CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
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
	  CALL MGOPLOTID(FILE(31:42),'[.WIND]3DPPLOT')
	  CALL MGOSETEXPAND(1.)
	ENDDO
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
	SUBROUTINE VXBPLOT(ITERM)
C
C	PLOT -(VxB) V.S. TIME
C
	CHARACTER*12 PTITLE(20)
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,swe_scet,vx,vy,vz,vmag,dens
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
	real*8		swe_scet(2000)
	real*8		sday
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	integer*4 	nmfi,nswe
	DIMENSION YY(2048),PP(2048),TMFI(2048)
	REAL*4		EX(2000),EY(2000),EZ(2000)
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
	XEND = 3100.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,XEND,2250.)
	ENDIF
C
	NDDS = MFI_SCET(1)
	SDAY = NDDS
	TFACT = 24.
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NSWE.OR.N.GT.NMFI) GO TO 202
	       fday = mfi_scet(n) - SDAY
  	       TMFI(N) = fday*tfact
	       fday = swe_scet(n) - SDAY
  	       PP(N) = fday*tfact
	       NPPNT = N
C		B measurements are more frequent, so interpolate
c		B at SWE time
	      SWETIME = SWE_SCET(N)
	      CALL INTERP(TMFI,BX,NMFI,SWETIME,BXINT,ERR)
	      CALL INTERP(TMFI,BY,NMFI,SWETIME,BYINT,ERR)
	      CALL INTERP(TMFI,BZ,NMFI,SWETIME,BZINT,ERR)
	      CALL INTERP(TMFI,BMAG,NMFI,SWETIME,BMINT,ERR)
	      EX(N) = -.001*(VY(N)*BZINT - VZ(N)*BYINT)
	      EY(N) = -.001*(VZ(N)*BXINT - VX(N)*BZINT)
	      EZ(N) = -.001*(VX(N)*BYINT - VY(N)*BXINT)
	      EXAV = EXAV + EX(N)
	      EYAV = EYAV + EY(N)
	      EZAV = EZAV + EZ(N)
	    ENDDO
 202	 XLST = PP(NPPNT)
	 XRANGE = PP(NPPNT) - PP(1)
	 EXAV = EXAV/NPPNT
	 EYAV = EYAV/NPPNT
	 EZAV = EZAV/NPPNT
	 OPEN(UNIT=112,FILE='EAV.DATA',STATUS='OLD',ACCESS='APPEND')
	WRITE(112,1112) FILE(31:42),NPPNT,EXAV,EYAV,EZAV
	TYPE 1112, FILE(31:42),NPPNT,EXAV,EYAV,EZAV
 1112	FORMAT(1X,A12,I6,3E12.3)
	 CLOSE(UNIT=112)
	
C
	NW = 3
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
	  IF(JW.EQ.1) THEN
C
C		PLOT EX
C
	    DO N = 1,NPPNT
	      YY(N) = EX(N)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(12,'-(VxB)x mV/m')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.2) THEN
	    DO N = 1,NPPNT
	      YY(N) = EY(N)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	    CALL MGOYLABEL(12,'-(VxB)y mV/m')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  ENDIF
C
	  IF(JW.EQ.3) THEN
	    DO N = 1,NPPNT
	      YY(N) = EZ(N)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
	  ENDIF
	    CALL MGOYLABEL(12,'-(VxB)z mV/m')
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
C
C
 200	XST = PP(1) - .003*XRANGE
	XLST = XLST + .003*XRANGE
	YRANGE = YMAX - YMIN
	PRINT*,'JW,MIN,MAX Y',JW,YMIN,YMAX
	YMAX = YMAX + .02*YRANGE
	YMIN = YMIN - .02*YRANGE
	CALL MGOSETLIM(XST,YMIN,XLST,YMAX)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NPPNT)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(FILE(31:42),'[.WIND]VXBPLOT')
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

	print*,' get_stream_name got',stream

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end
