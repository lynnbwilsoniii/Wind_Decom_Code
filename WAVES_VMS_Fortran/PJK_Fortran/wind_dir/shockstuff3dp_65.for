	PROGRAM SHOCKSTUFF
C
C	PLOTS 3DP CDHF DATA TO DETERMINE SOME SHOCK PARAMETERS
C	UNTIL 1710, 17 SEP 1996, THERE WERE ERRORS IN SOUND SPEED,
C		AND EXRATIO
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
	integer*4	temp_waves,iend,n2,s_scet(2)
	character*32	s
	character*20	MFI_FILE
	real*8		mfi_scet(2000)
	real*8		swe_scet(2000)
	real*8		dp3_scet(2000)
	real*8		orb_scet(200)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		dp3eden(2000),dp3iden(2000),tempe(2000),tempi(2000)
	real*8		xgse(200),ygse(200),zgse(200)
	real*8		scet,scetend
	real*4		vthp(2000)
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
	INTEGER*4 CDHFCH,hkch,CH
	CHARACTER*11 YYMMDD
!
	COMMON /MODEL/ RTA,RTB,C 
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,swe_scet,vx,vy,vz,vmag,dens
	common /pltblk3/ norb,orb_scet,xgse,ygse,zgse
	common /pltblk4/ n3dp,dp3_scet,dp3eden,dp3iden,tempi,tempe
	COMMON /HEADBL/ NHRMN,LAST,EVENT,FILE
C
	DIMENSION PLDATA(20,2)
	DATA TWOPI /6.2831853/
	DATA RTA,RTB,C /  25.6, 25.6, 14.6/
	DATA ALPHA,BETA,GAMMA /0.,1.,0./
	DATA X0,Y0,Z0 /14.6,1.,0./
	DATA RE /6.378E3/
C
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
	scet = 0.
	call w_channel_position(CDHFCH,scet)
	print*,'cdhf file starts at scet ',scet
	dds = scet
	scet = float(dds) + hh/24. + mm/1440.
	print*,'set cdhf channel position to',scet
	call w_channel_position(CDHFCH,scet)
	print*,'cdhf channel position set to',scet
	hhnd = last/100
	mmnd = mod(last,100)
	scetend = float(dds) + hhnd/24. + mmnd/1440.
c
	ok = w_channel_filename(CDHFCH,file)
C	write(87,*) file
	print*,file
c
	get_tm_stream = 1
c
c	call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
C
C	DOWHILE(SCET.LT.SCETEND)
	  ok = w_event(CDHFCH,'CDF')
	  if(ok.eq.82) then
	     scet = 10000.       	! artificially large
	     print*,'end of file'
	     if (.not. ok) stop 'cannot get tds event'
	  else
	    item = 'EVENT_SCET'
	    ok = w_item_I4(CDHFCH, item, S_scet, 2, return_size)
	    item = 'EVENT_SCET_R8'
	    ok = w_item_R8(CDHFCH, item, scet, 1, return_size)
	    item = 'WIND_ORBIT_SCET_R8'
	    ok = w_item_r8(CDHFCH, item, orb_scet, 200, return_size)
	    norb = return_size
	    print*,'initial orb time',orb_scet(1),' size',return_size
	    item = 'WIND_ORBIT_X(GSE)_R8'
	    ok = w_item_r8(CDHFCH, item, XGSE, 200, return_size)
	    item = 'WIND_ORBIT_Y(GSE)_R8'
	    ok = w_item_r8(CDHFCH, item, YGSE, 200, return_size)
	    item = 'WIND_ORBIT_Z(GSE)_R8'
	    ok = w_item_r8(CDHFCH, item, ZGSE, 200, return_size)
	    print*,'initial orb x,y,z',xgse(1),ygse(1),zgse(1)
C	    print*,'orb x',(xgse(NN),NN=1,RETURN_SIZE)
	    print*,'orbit file size',return_size
C
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
	  ENDIF
C
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(CDHFCH, item, DP3_SCET, 2000, return_size)
	  N3DP = return_size
	  print*,'initial 3dp time',DP3_SCET(1),' size',return_size
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
	  print*,' dens(1)',dens(1),'size',return_size
	  item = 'WIND_3DP_ION_TEMP_R4'
	  ok = w_item_r4(CDHFCH, item, vthp, 2000, return_size)
C
C	CLEAN OUT MISSING 3DP DATA
C
	  N3DPT = N3DP
	  DO N = 1,N3DP
		N1 = N
 100		IF(DENS(N).GT.1.E30.OR.DENS(N).LT.-1.E30.OR
     1 			.VX(N).GT.1.E30.OR.VX(N).LT.-1.E30) THEN
		  N3DPT = MIN0(N3DPT,2000) - 1
		print*,'remove n,dens',n,dens(n),' to',n3dpt
		  IF(N3DPT.LE.N) GO TO 101
		  DO NN = N1,N3DPT
		    DP3_SCET(NN) = DP3_SCET(NN+1)
		    DENS(NN) = DENS(NN+1)
		    VX(NN) = VX(NN+1)
		    VY(NN) = VY(NN+1)
		    VZ(NN) = VZ(NN+1)
C		    VMAG(NN) = VMAG(NN+1)
		    VTHP(NN) = VTHP(NN+1)
		  ENDDO
		GO TO 100
		ENDIF
	  ENDDO
 101	  PRINT*,N3DP-N3DPT,'  3DP DATA POINTS REMOVED'
	  N3DP = N3DPT
C
	  DO N = 1,N3DP
		VMAG(N) = SQRT(VX(N)**2 + VY(N)**2 + VZ(N)**2)
	  ENDDO
C
C	  print*,'initial 3dp time',dp3_scet(1),' size',return_size
	  item = 'WIND_3dp_ion_temp_R4'
	  ok = w_item_r4(CDHFCH, item, tempi, 2000, return_size)
	  item = 'WIND_3dp_e_temp_R4'
	  ok = w_item_r4(CDHFCH, item, tempe, 2000, return_size)
C	endif
c
C	do i = 1,40
C	  type*,'swe',swe_scet(i),dens(i),vthp(i)
C	enddo
C	do i = 1,40
C	  type*,'3dp',dp3_scet(i),tempe(i),tempi(i)
C	enddo

C	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
C	   ihrmn = 100*hh+mm

	
c	   write(s,'(i8.8,i6.6)',iostat=ios) mfi_scet(1), mfi_scet(2)
C	   mfi_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)


C	CALL PLTSTUFF(3)
	CALL PLTSTUFF(-1)
	STOP
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

	SUBROUTINE PLTSTUFF(ITERM)
C
C	PLOT DATA V.S. TIME  1st PANEL B.N = SHOCK NORMAL ANGLE, 2nd
C	= DISTANCE RELATIVE TO SHOCK  (1. = AT AVERAGE SHOCK)  3rd = 
C	SOLAR WIND PRESSURE, 4th = ALFVEN MACH NO., 5th = DIFF 
C	DISTANCE RELATIVE TO MAGNETOPAUSE?  BETA?  
C	ALFVEN MACH NO.,SONIC MACH NO, SUPERCRITICAL V.S. SUBCRITICAL?  
C
	CHARACTER*120 STR
	CHARACTER*80 FILE
	CHARACTER*4 EVENT
	COMMON /HEADBL/ NHRMN,LAST,EVENT,FILE
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag
	common /pltblk2/ nswe,swe_scet,vx,vy,vz,vmag,dens
	common /pltblk3/ norb,orb_scet,xgse,ygse,zgse
	common /pltblk4/ n3dp,dp3_scet,dp3eden,dp3iden,tempi,tempe
	COMMON /MODEL/ RTA,RTB,RTC 
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
	real*8		orb_scet(200)
	real*8		dp3_scet(2000)
	real*8		sday,X0,Y0,Z0
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		dp3eden(2000),dp3iden(2000),tempe(2000),tempi(2000)
	real*8		xgse(200),ygse(200),zgse(200)
C	REAL*4		DF(2000)
	integer*4	yyyy,mon,dd,doy,hh,mm,ss,ms
	DIMENSION YY(2048),PP(2048),TMFI(2048),RR(2048),TORB(200),T3DP(2000)
	DATA RE /6.378E3/    			! km
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	HH = LAST/100
	MM = MOD(LAST,100)
	FDAYL = HH/24. + MM/1440.
C
	SWPAVR = 7.*(450.**2)
	XEND = 2250.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,XEND,3100.)
	ENDIF
	call w_ur8_to_ydoy(mfi_scet(2),yyyy,doy,ms)
	call w_ur8_to_ymd(mfi_scet(2),yyyy,mon,dd,hh,mm,ss,ms)
	   write(STR,1202,iostat=ios) YYYY,MON,DD,DOY
 1202	format(' HOURS OF ',I4.4,'/',I2.2,'/',I2.2,'  DOY',I5)
C	   mfi_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
C	  CALL MGOSETEXPAND(.75)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(500.,50.)                      ! hardcopy
	  ENDIF
	  CALL MGOPUTLABEL(53,STR,9)
C
	  CALL MGOSETEXPAND(1.)
C
	NDDS = MFI_SCET(1)
	SDAY = NDDS
	TFACT = 24.
	NW = 5
	DO N = 1,NMFI
	      fday = MFI_scet(n) - SDAY
  	      TMFI(N) = fday*tfact
	ENDDO
	DO N = 1,NORB
	      fday = ORB_scet(n) - SDAY
  	      TORB(N) = fday*tfact
	ENDDO
	DO N = 1,N3DP
	      fday = DP3_scet(n) - SDAY
  	      T3DP(N) = fday*tfact
	ENDDO
C	PRINT*,'NS',NMFI,NORB,N3DP
C	PRINT*,'TLST',TMFI(NMFI),TORB(NORB),T3DP(N3DP)
	XST = AMIN1(TMFI(1),TORB(1),T3DP(1))
C	XLST = AMAX1(TMFI(NMFI),TORB(NORB),T3DP(N3DP))
	XLST = (LAST/100) + MOD(LAST,100)/60.
	XRANGE = XLST - XST
C
	DO JW = 1,NW
	  CALL MGOWINDOW(1,NW,JW) 
c	  CALL MGOTICKSIZE(0.,0.,0.,0.)
	  YMAX = -1.E10
	  YMIN =  1.E10
	  FDAY = 0.
C
	  IF(JW.EQ.1) THEN
C
C		PLOT ANGLE BETWEEN B AND SHOCK NORMAL, TIME IS MFI TIME
C
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	      N = N+1
	      IF(N.GT.NMFI) GO TO 201
	      fday = MFI_scet(n) - SDAY
	      ORBTIME = FDAY*tfact
C		interpolate to get orbit at mfi times
C		D2INTERP accepts real*4 xtable and x, real*8 ytable and y
	      CALL D2INTERP(TORB,XGSE,NORB,ORBTIME,X0,ERR) 
	      CALL D2INTERP(TORB,YGSE,NORB,ORBTIME,Y0,ERR)
	      CALL D2INTERP(TORB,ZGSE,NORB,ORBTIME,Z0,ERR)
C	      BMAG0 = SQRT(BX0**2 + BY0**2 + BZ0**2)
	      R02 = Y0**2 + Z0**2
	      EXRATIO = (.5/RTC)*(X0/RE + SQRT((X0/RE)**2 + 
     1		4.*(RTC/RTA)**2*R02/RE**2))
C	 	print*,'orb',ORBTIME,x0,y0,z0,exratio
C		print*,(torb(jj),jj=1,norb)
	      RR(N) = EXRATIO
C
C	CALCULATE MODEL SHOCK NORMAL
C
	      SNX = 1./RTC
	      SNY = 2.*Y0/EXRATIO/RTB**2
	      SNZ = 2.*Z0/EXRATIO/RTA**2
	      SNORM = SQRT(SNX**2 + SNY**2 + SNZ**2)
C		PRINT*,N,orbtime,SNORM,bMAG(N),BX(N)
	      BDOTN = (BX(N)*SNX + BY(N)*SNY + BZ(N)*SNZ)/SNORM/BMAG(N)
C		PRINT*,N,RR(N),snx/SNORM,sny/SNORM,snz/SNORM
C		PRINT*,N,orbtime,bdotn,bx(n),by(n),bz(n)
  	      PP(N) = ORBTIME
	      NPPNT = N
	      YY(N) = ABS(BDOTN)
	      YMAX = AMAX1(YY(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 201	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(5,'BdotN')
C	    CALL MGOSETEXPAND(.8)
	    CALL MGOXLABEL(16,'HOURS OF THE DAY')
C	    CALL MGOSETEXPAND(1.)
	  ENDIF
C
	  IF(JW.EQ.3) THEN
c	    CALL MGOTICKSIZE(0.,0.,-1.,-1.)
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.N3DP) GO TO 202
	       fday = DP3_SCET(n) - SDAY
  	       PP(N) = fday*tfact
	       NPPNT = N
c	       mm = mfi_scet(2)/100
C	       mm = mod(mm,100)
c	       hh = mfi_scet(2)/10000
C	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
C		B measurements are more frequent, so interpolate
c		B at 3DP time
	      YY(N) = AMAX1(DENS(N),0.)*AMAX1(VMAG(N),0.)**2/SWPAVR
	      IF(YY(N).LT.5.E7)  YMAX = AMAX1(YY(N),YMAX)
	      IF(YY(N).GT.-50000.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 202	    CALL MGOYLABEL(12,'S.W.PRESSURE')
	  ENDIF
C
	  IF(JW.EQ.2) THEN
C
C	PLOT RATIO TO AVERAGE SHOCK POSITION
C
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.NMFI) GO TO 203
	       fday = MFI_scet(n) - SDAY
  	       PP(N) = FDAY*TFACT
	       NPPNT = N
	       YY(N) = RR(N)
	       YMAX = AMAX1(YY(N),YMAX)
	       IF(YY(N).GE.0.) YMIN = AMIN1(YY(N),YMIN)
	    ENDDO
 203	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(11,'R/(SHOCK R)')
	  ENDIF
C
C
	  IF(JW.EQ.4) THEN
C
C	PLOT MACH NUMBERS, ALFVEN AND SONIC
C
	    N = 0
	    DOWHILE(FDAY.LE.FDAYL)
	       N = N+1
	       IF(N.GT.N3DP) GO TO 204
	       fday = DP3_scet(n) - sday
  	       PP(N) = fday*tfact
	       NPPNT = N
c	       ss = mod(swe_scet(2),100)
c	       mm = mfi_scet(2)/100
C	       mm = mod(mm,100)
c	       hh = mfi_scet(2)/10000
C	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
C
C	CALCULATE MODEL SHOCK NORMAL
C
	      CALL D2INTERP(TORB,XGSE,NORB,PP(N),X0,ERR) 
	      CALL D2INTERP(TORB,YGSE,NORB,PP(N),Y0,ERR)
	      CALL D2INTERP(TORB,ZGSE,NORB,PP(N),Z0,ERR)
	      CALL INTERP(TMFI,BMAG,NMFI,PP(N),BMAG0,ERR)
	      SNX = 1.
	      SNY = 2.*Y0/RE/RTB
	      SNZ = 2.*Z0/RE/RTA
	      SNORM = SQRT(SNX**2 + SNY**2 + SNZ**2)
C
C	CALCULATE ALFVEN SPEED IN KM/SEC
C
	      IF(DENS(N).GT.0.)
     1		VALF = 21.9*BMAG0/SQRT(DENS(N))    ! Bmag in nT, dens in cu.cm.
C
C	CALCULATE ION SOUND SPEED IN KM/SEC
C
	      TE = TEMPE(N)
	      TI = TEMPI(N)
C		print*,'mach',t3dp(n),tempe(n),tempi(n),te
	      VSOUND = 3.E5*SQRT(AMAX1((3.*TI + TE),0.)/.935E9)      ! T in eV 
C
	      IF(DENS(N).GT.0.)
     1		VALF = 21.9*BMAG0/SQRT(DENS(N))    ! Bmag in nT, dens in cu.cm.
C
	      IF(VMAG(N).GT.0.)
     1	      VDOTN = (VX(N)*SNX + VY(N)*SNY + VZ(N)*SNZ)/SNORM
C		print*,'vdotn,valf',n,vdotn,valf
	      IF(VALF.NE.0.) YY(N) = ABS(VDOTN/VALF)
	      IF(VSOUND.NE.0.) RR(N) = ABS(VDOTN/VSOUND)
	      IF(YY(N).LE. 30.) YMAX = AMAX1(YY(N),YMAX)
	      IF(RR(N).LE. 30.) YMAX = AMAX1(RR(N),YMAX)
	      YMIN = AMIN1(YY(N),YMIN)
c		PRINT*,N,PP(N),YMIN,YMAX,yy(n),VALF
	    ENDDO
 204	    YMIN = AMAX1(YMIN,0.)
	    CALL MGOYLABEL(15,'NORMAL MACH NO.')
	  ENDIF
C
	IF(JW.EQ.5) THEN
C
C	PLOT DIFF, TIME IS MFI TIME
C
	  YMAX = -1000.
	  YMIN = 1000.
	  N = 0
	  DOWHILE(FDAY.LE.FDAYL)
	    N = N+1
	    IF(N.GT.NMFI) GO TO 200
	    fday = MFI_scet(n) - sday
  	    PP(N) = fday*tfact
	    NPPNT = N
C		interpolate to get orbit at mfi times
	    CALL DINTERP(TORB,XGSE,NORB,PP(N),X0,ERR) 
	    CALL DINTERP(TORB,YGSE,NORB,PP(N),Y0,ERR)
	    CALL DINTERP(TORB,ZGSE,NORB,PP(N),Z0,ERR)
	    XRE = X0/RE
	    YRE = Y0/RE
	    ZRE = Z0/RE
C	    BMAG0 = SQRT(BX0**2 + BY0**2 + BZ0**2)
	    CALL DIFFFK(XRE,YRE,ZRE,BX(N),BY(N),BZ(N),DDF,XT,YT,ZT)
	    YY(N) = AMIN1(DDF,20.)
	    YY(N) = AMAX1(YY(N),-20.)
	    YMAX = AMAX1(YY(N),YMAX)
	    YMIN = AMIN1(YY(N),YMIN) 
	  ENDDO
	  CALL MGOYLABEL(4,'DIFF')
	ENDIF
C
 200	CONTINUE
C
C	THIS PLOTS ONE PANEL OF YY VS PP.  FOR JW = 4, RR IS PLOTTED ALSO
C
	XST = PP(1) - .003*XRANGE
	XLST = XLST + .003*XRANGE
	YRANGE = ABS(YMAX - YMIN)
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
	IF(JW.EQ.4) THEN
	  CALL MGOSETLTYPE(1)
	  CALL MGOCONNECT(PP,RR,NPPNT)
	  CALL MGOSETLTYPE(0)
	ENDIF
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
C	PRINT*,'IN PLOT, FILE=',FILE
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	IF(JW.EQ.NW) CALL MGOPLOTID(FILE(31:42),'[.WIND]SHOCKSTUFF3DP')
	CALL MGOSETEXPAND(1.)
C
	ENDDO
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
