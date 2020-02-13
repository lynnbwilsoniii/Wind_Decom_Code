	PROGRAM TDSXYZ
C
C	PLOTS TDS X AND Y AND FREQUENCY SPECTRUM DIFFERENCE
C	a serious error was corrected 30-dec-1996 1600  CONJG(CGAIN)
C
C	implicit	ALL
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
	parameter	size=2048
	integer*4	return_size,nmfi
	integer*4	tds_channel,ifilf,ifils,ifil,ifsps,issps,isps
	integer*4	temp_waves,iend,n2,sunclock
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scett,scettds,scetfill,MFI_SCET(2000)
	real*8		XKM,YKM,ZKM
	real*4		RE
	integer*4	major, minor,isrc,nsrc
	character*80	file
	character*32	item
	character*20	mfi_file
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
	real 		FILTER,ffilter,sfilter,fsps,ssps,sps
	REAL 		S1,S2,PP(2050),BX,BY,BZ
	REAL		BXS(2000),BYS(2000),BZS(2000),BTS(2000)
	REAL 		XDATA,XSPECT(1025,4)
C	FOR PARTSPEC:
	REAL		GNIP,GNRP
	INTEGER*4 	NHALF,NVAR,NSPECT,NSTART
	REAL		PDATA(2050)
	REAL 		XPHASE(1025,4),DPHASE(1025,4)
	REAL 		EFFLEN(4)
!
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	common /headblk/ major,minor,s_scet,nsys
	COMMON /XFER/ SPHASE(1025)
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
C
	CHARACTER*12 PTITLE(25)
	INTEGER*4 TDSCH,hkch,fillch,ch
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /HEADBL/ PTITLE,EVENT,ITEMP
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	DATA BX,BY,BZ /0.,0.,0./
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
	DATA IFASTSLOW /1/		! 0 IS FAST, 1 IS SLOW
	DATA ICHEOF /0/			! 1 IS EOF ON FILL, 2 ON TDS
	DATA EFFLEN /41.1,3.79,2.17,1./
C	LEVEL = 0 IS JUST PRINT, 1 IS WRITE FILES, 2 IS DO PLOTS
	DATA LEVEL /2/
C
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = 'TIME DOMAIN'
	PTITLE(3) = 'SAMPLER'
	PTITLE(4) = 'EVENT NO.'
	PTITLE(8) = 'SAMPLE RATE'
	PTITLE(10) = 'L.P.FILTER'
C	PTITLE(12) = 'TRIGGERS'
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
	  read(5,*,err=10,end=20)  iend,n2
	  type*,iend,n2
	    if(iend.eq.1) then
		nrem = n2
	    endif
	    if(iend.eq.2) then
		nrem = 2
			if(ifastslow.eq.1) nrem = 4
		nevent = n2
	    endif
	endif
	type*,'type desired process level,0=raw,1=raw volts,2=fft,fft-1'
	read(5,3) iq,iprocess
	type*,' '
	if(iprocess.eq.0) type*,'ok, plot data in tm numbers - 128'
	if(iprocess.eq.1) type*,'ok, plot in volts, unity freq. response.'
	if(iprocess.ge.2) type*,'ok, plot volts, corrected for freq. response.'
	if(iprocess.eq.3) type*,'ok, plot volts, corrected for bad TDS data'
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
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set tds channel position to',scettds
	call w_channel_position(tdsch,scettds)
	print*,'tds channel position set to',scettds

	ok = w_channel_open(fillch,stream)           
	if (.not. ok) stop 'Cannot open fill channel'      
	scetfill = 0.
	call w_channel_position(fillch,scetfill)
	print*,'scetfill',scetfill
	dds = scetfill
	scetfill = float(dds) + hh/24. + mm/1440.
	print*,'set channel position to',scetfill
	call w_channel_position(fillch,scetfill)
	print*,'fill channel position set to',scetfill
c
	call w_ur8_to_ydoy(scetfill,yyyy,doy,msday)
	PRINT*,'RETURN FROM YDOY,SCETFILL,YYYY',SCETFILL,YYYY

	PRINT*,'GO W_EVENT',TDSCH
        IF(IFASTSLOW.EQ.0) THEN
	  ok = w_event(tdsch,'TDSF')
	ELSE
          ok = w_event(tdsch,'TDSS')
	ENDIF
	PRINT*,'RETURN FROM W_EVENT',TDSCH
	if(ok.eq.82) then
	   scettds = 10000.       	! artificially large
	else
	  item = 'EVENT_SCET'
	  ok = w_item_i4(tdsch, item, s_scet, 2, return_size)
	  print*,'initial tdsch time',s_scet
	endif
c
        ok = w_event(fillch,'FILL')
	if(ok.eq.82) then
		icheof = 1
		stop 'end of file on tdsch'
	endif
	if (.not. ok) stop 'cannot get fill event'
c	item = 'EVENT_SCET_R8'
c	  ok = w_item_r8(fillch, item, scet, 2, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(fillch, item, s_scet, 2, return_size)
	print*,'initial fillch time',s_scet
	  
c
	ok = w_channel_filename(tdsch,file)
	IF(LEVEL.GE.1) write(87,*) file
	print*,file
c
	get_tm_stream = 1
c
C	GET NEXT EVENT
C
 110    continue
C
	! this is the main program loop

c        if( wind_tm_eof(fillch,major,minor)) stop 'end of file, fillch'

	if(scettds.lt.scetfill) then
	  event = 'TDSF'
	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  ch = tdsch
	else
	  event = 'FILL'
	  ch = fillch
	endif
C
	type*,'going to get next ',EVENT,' event'
c
	if(icheof.eq.1) then
	  event = 'TDSF'
	  IF(IFASTSLOW.NE.0)event = 'TDSS'
	  ch = tdsch
	elseif(icheof.eq.2) then
	  event = 'FILL'
	  ch = fillch
	else
	endif
c
	type*,'compare scettds,scetfill,evt',scettds,scetfill,event

	ok = w_event(ch,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
              if( ok.eq.82) then
		 type*,'end of file on ',event
		 if(event.eq.'fill') then
			icheof = 1
		 else
			icheof = 2
		 endif
c                if( ok.eq.82) stop 'end of file on above channel '
	      endif
	    end if
C
C

	      item = 'EVENT_NUMBER'
	      ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
		PRINT*,'EVENT NO.',ITEMP
	      item = 'EVENT_SCET_R8'
	      ok = w_ITEM_R8(ch, item, SCETT, 1, return_size)
C
	     if(ch.eq.tdsch) then
	       scettds = scett
	     else
	       scetfill = scett
	     endif
C
	      if(iend.eq.2.and.itemp.ne.nevent) go to 110
	      CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
c
	     item = 'WIND_SPIN_RATE_R4'
	     ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
	     item = 'FAST_RX_SPEED_R4'
	     ok = w_item_R4(ch, item, SPSS, 1, return_size)
	     PRINT*,'FAST TDS SPEED',SPSS
	     WRITE(79,*) 'FAST TDS SPEED',SPSS
	     item = 'SLOW_RX_SPEED_R4'
	     ok = w_item_R4(ch, item, SPSS, 1, return_size)
	     item = 'SLOW_RX_SPEED'
	     ok = w_item_i4(ch, item, ISPS, 1, return_size)
c	     item = 'WIND_MFI_BX(GSE)_R4'DS
	print*,'spin rate, speed',spinrate,spss
c	     ok = w_item_R4(ch, item, BX, 1, return_size)
c	     if(nmfi.eq.0) nmfi = return_size
c	     item = 'WIND_MFI_BY(GSE)_R4'
c	     ok = w_item_R4(ch, item, BY, 1, return_size)
c	     item = 'WIND_MFI_BZ(GSE)_R4'
c	     ok = w_item_R4(ch, item, BZ, 1, return_size)
c	     IF(NMFI.EQ.0) THEN
c		WRITE(79,*) 'NO WIND/LIB MFI DATA'
c	        IF(NMFI.NE.0) THEN
c		  print*,'last mfi time',mfi_scet(return_size)
c	        ELSE
c		  PRINT*,'NO WIND/LIB MFI DATA'
c		  NMFI = 0
c		  WRITE(MFI_FILE,765),S_SCET(1)
c 765		  FORMAT('[.STUFF]',I8,'.MFI')
c		  PRINT*,'OPEN MFI FILE:  ',MFI_FILE
c		  OPEN(UNIT=99,FILE='[.STUFF]19961116.MFI',
c     1			TYPE='OLD',READONLY)
c 		  READ(99,1097) JUNK
c		  READ(99,1097) JUNK
c 1097		  FORMAT(A120)
c 1099		  FORMAT(A10,I3,1X,I2,1X,F6.3,4F14.4)
c 1098		  READ(99,1099,END=1199) JUNK,NHR,MIN,SEC,BMAG,BX,BY,BZ
c		  NMFI = NMFI+1
c		  MFI_SCET(NMFI) = DFLOAT(DDS) + NHR/24.D00
c     1		   + MIN/1440.D00 + SEC/86400.D00
c		  BXS(NMFI) = BX
c		  BYS(NMFI) = BY
c		  BZS(NMFI) = BZ
c		  BTS(NMFI) = BMAG
c		  IF(MFI_SCET(NMFI).GT.SCETT.AND.MFI_SCET(NMFI-1).
c     1			LE.SCETT) WRITE(79,*) BX,BY,BZ,BMAG
c		  GO TO 1098
c 1199		  CLOSE(UNIT=99)
C		  print*,'last mfi and time',NMFI,mfi_scet(NMFI)
C		  print*,'values',BMAG(NMFI),BX(NMFI),BY(NMFI),BZ(NMFI)
c	        ENDIF
c	     ENDIF
c	     CALL DINTERP(MFI_SCET,BXS,NMFI,SCETT,BX,ERR)
c	     CALL DINTERP(MFI_SCET,BYS,NMFI,SCETT,BY,ERR)
c	     CALL DINTERP(MFI_SCET,BZS,NMFI,SCETT,BZ,ERR)
c	     CALL DINTERP(MFI_SCET,BTS,NMFI,SCETT,BMAG,ERR)
c	     WRITE(79,*) 'SCET,B X,Y,Z',S_SCET(2),BX,BY,BZ
	     IF(NMFI.NE.0) THEN
	       BANGLE = ATAN2D(BY,BX)
     	       PRINT*,'B',BX,BY,BANGLE
	       write(79,*) 'B',BX,BY,BZ,BANGLE
	     ENDIF
C
	     item = 'SUN_ANGLE'
	     ok = w_item_i4(ch, item, SUNCLOCK, 1, return_size)
		PRINT*,'SUNCLOCK',SUNCLOCK
C
	     item = 'WIND_ORBIT_X(GSE)_R8'
	     ok = w_item_R8(ch, item, XKM, 1, return_size)
	     item = 'WIND_ORBIT_Y(GSE)_R8'
	     ok = w_item_R8(ch, item, YKM, 1, return_size)
	     item = 'WIND_ORBIT_Z(GSE)_R8'
	     ok = w_item_R8(ch, item, ZKM, 1, return_size)
	     item = 'EVENT_SCET'
	     ok = w_item_i4(ch, item, s_scet, 2, return_size)
		ss = mod(s_scet(2),100)
		mm = s_scet(2)/100
		mm = mod(mm,100)
		hh = s_scet(2)/10000
		scett = float(dds) + hh/24. + mm/1440. + ss/86400.
c
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
	     ihrmn = 100*hh+mm
	     TYPE *,s_scet
	     TYPE *,'scett,doy',scett,doy
	   ihrmn = 100*hh+mm

	
	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)



	   WRITE(PTITLE(16),1016) s(1:4),S(5:6),S(7:8)
	   WRITE(PTITLE(17),1017) DOY
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1017	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
	   TYPE*,'CHANNEL',tds_channel,'   EVENT = ',EVENT
	   WRITE(PTITLE(6),1019) TDS_CHANNEL
 1019	   FORMAT('CHANNEL',I2)
	   item = 'EVENT_NUMBER'
	   ok = w_item_I4(ch, item, itemp, 1, return_size)
	   type*,'event number',itemp
	   WRITE(PTITLE(5),1012) ITEMP
 1012	   FORMAT(I10)
	   item = 'SOURCE'
	   ok = wind_tm_get_item(ch, item, isrc, 1, return_size)
C
	   ipa = ichpa(tds_channel)
	   WRITE(PTITLE(7),1007) PARX(IRX)
 1007	   FORMAT('P/A ',A4)
	   IF(TDS_CHANNEL.LE.2) THEN
	      WRITE(PTITLE(9),1004) .001*SPS
	   ELSE
	      WRITE(PTITLE(9),1008) SPS
	   ENDIF
	      WRITE(PTITLE(11),1008) FILTER
 1004	   FORMAT(F7.2,' kHZ')
 1008	   FORMAT(F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

 222	FORMAT(10I6)
		IF(IPROCESS.EQ.0) THEN
	  DO IK = 1,2048
	    NDATA(IK) = NDATA(IK)-128
	    MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
	  ENDDO
	ELSE
	  DO IK = 1,2048
C	    DATA(IK) = TDSCAL(TDS_CHANNEL,ISPS,NDATA(IK))
	    NDATA(IK) = NDATA(IK)-128
	    MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
	  ENDDO
	ENDIF
**********************
	IF(IFASTSLOW.EQ.0.AND.TDS_CHANNEL.GT.2) GO TO 110
C	write(87,7737) itemp,event,s_scet(2),tds_channel,
C     1	parx(irx),maxdata,
C     2	tdscal(tds_channel,isps,maxdata+128)
 7737	format(i10,2x,a4,i10,i5,2x,a4,i5,e12.3)
	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.95) GO TO 110
	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.2.AND.MAXDATA.LE.100) GO TO 110
	NPLOTS=NPLOTS+1
	NSRC = ISRC-3
	IF(NSRC.LT.1) NSRC = ISRC
	IF(NSRC.GT.4) NSRC = 4
	  DO N = 1,2050
	    XDATA(N,NSRC) = DATA(N)/EFFLEN(NSRC)
	  ENDDO
C	  DO N = 1,1025
C	    XSPECT(N,NSRC) = SPECT(N)
C	    XPHASE(N,NSRC) = SPHASE(N)
C	  ENDDO
C********THE FOLLOWING IS FOR A SPECIAL TEST OF CALIBRATIONS,
C	AND I HAD TO USE PARTSPEC BECAUSE I CAN'T FIND A FULL ONE.
C**********
c*******
c	correction for "normal" case, only EX Ch4 shifted
c	if(nsrc.eq.1) then
c	print*,'advance data for channel',tds_channel
c   	  CHECK OF X DELAY
c	  do n = 1,2047
c	    ndata(n+1) = ndata(n)
c	  enddo
c	endif
c*******end check
	  NSTART = 1020
	  NVAR = 1024
 c	  CHECK OF X DELAY
	  if(nsrc.eq.1) then
  	    print*,'advance data for channel',tds_channel
	    nstart = nstart+1
	  endif
	  NSPECT = 1
	  NHALF = NVAR/2
	  CALL PARTSPEC(CH,NSTART,NVAR,NDATA,NSPECT,DATA,PDATA,SPECT)
	  DO N = 1,NVAR
	    XDATA(N,NSRC) = PDATA(N)
	  ENDDO
C	  DO N = 3,NVAR+1,2
C		GNIP = PDATA(N+1)
C		GNRP = PDATA(N)
C		SPHASE(N/2) = ATAN2(GNIP,GNRP)
C	  ENDDO
	  DO N = 1,NHALF+1
	    XSPECT(N,NSRC) = SPECT(N)
	    GNRP = PDATA(2*N-1)
	    GNIP = PDATA(2*N)
c	    XPHASE(N,NSRC) = ATAN2D(GNIP,GNRP)
	  ENDDO
C*****END OF CAL SECTION
C
	  IF(TDS_CHANNEL.LT.6) GO TO 110
C	ELSE
	IF(LEVEL.GE.1) THEN
c	  open(unit=71,file='xyzdata.dat',status='old',access='append')
	  open(unit=71,file='xyzdata.dat',status='new')
	  write(71,*) file,itemp
	ENDIF
Cd
	PRINT*,'XDATA',XDATA(1,1),XDATA(1,2),XDATA(2,1)
	  DO N = 1,NVAR,2
		WRITE(71,1171) N,(XDATA(N,NN),XDATA(N+1,NN),NN=1,4)
	  ENDDO
 1171	  FORMAT(I6,8E12.3)
	  DO N = 1,NHALF+1
		PHXY = XPHASE(N,1) - XPHASE(N,2)
		IF(PHXY.GT.180.) PHXY = PHXY-360.
		IF(PHXY.LE.180.) PHXY = PHXY+360.
		PHYZ = XPHASE(N,2) - XPHASE(N,3)
		IF(PHYZ.GT.180.) PHYZ = PHYZ-360.
		IF(PHYZ.LE.180.) PHYZ = PHYZ+360.
		PHZX = XPHASE(N,3) - XPHASE(N,1)
		IF(PHZX.GT.180.) PHZX = PHZX-360.
		IF(PHZX.LE.180.) PHZX = PHZX+360.
C		WRITE(71,1071) N,XSPECT(N,1)-XSPECT(N,2),
C     1		XSPECT(N,2)-XSPECT(N,3),XSPECT(N,3)-XSPECT(N,1),
C     2		PHXY,PHYZ,PHZX
 1071	        FORMAT(I6,3E12.3,3F7.1)
	  ENDDO
	  DO N = 1,1025
	    PP(N) = N*SPSUSE/2048.
C            DPHASE(N) = AMOD(XPHASE(N)-YPHASE(N)+540.,360.) - 180.
	    IF(LEVEL.GE.1) THEN
C	      WRITE(68,*) PP(N),(XSPECT(N,NN),NN=1,4)
C     1        ,DPHASE(N)
CXY	      write(71,*) n,xspect(n),yspect(n),dphase(n)
C	      write(72,*) n,xspect(n),yspect(n),dphase(n)
	    ENDIF
	  ENDDO
c	ENDIF
CXYZ	close(unit=71)
	close(unit=71)
c	if(CH.ne.TDSCH) go to 110
C*******************
C
C	FIND ZERO CROSSING
C
	IZCNT = 0
	IL = 1
	IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL),NDATA(IL+1)
	DO IL = 2,2047
	  IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL-1),
     1   NDATA(IL),NDATA(IL+1)
c
C		COUNT ONLY POS TO NEG
	  IF(NDATA(IL).GT.0.AND.NDATA(IL+1).LE.0) THEN
	        IZCNT = IZCNT+1
		IF(IPROCESS.EQ.0) THEN
		  S1 = NDATA(IL)
		  S2 = NDATA(IL+1)
		ELSE
		  S1 = DATA(IL)
		  S2 = DATA(IL+1)
		ENDIF
		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
	  ENDIF
	ENDDO
	DO N = 1,IZCNT-1
	  ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
	  IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
	  IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
	ENDDO
	print*,'zero crossings found',izcnt
	print*,'first 5',(zcross(kk),kk=1,5)
C
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	IF(LEVEL.GE.2) THEN
	type*,'going to combo'
  	  CALL COMBO(-1)
	type*,'return from combo'
C
C	XFGSE TRANSFORMS E FROM S/C ANTENNA SYSTEM TO GSE SYSTEM
C
	  CALL XFGSE			! TRANSFORM E FROM S/C (ANT) TO GSE
C	  CALL XFORMSN			! TRANSFORM E TO SHOCK NORM,CURRENT
C
C	  plotpart plots short sections of the data from XFDATA
C		IN CASE XFORMVM IS NOT CALLED YET:
	  DO N = 1,2050
	    DO J = 1,4
	      XFDATA(N,J) = XGSE(N,J)
	    ENDDO
	  ENDDO
C
C  	  CALL PLOTPART(-1)
	  CALL XFORMVM(XGSE)			! TRANSFORM E TO VARIANCE EIGEN SYS.
C	print*,'after xformvm,xfdata(1,n) =',(xfdata(1,n),n=1,4)
C  	  CALL PLOTSEL(-1,2,2047) 	! 
c  	  CALL PLOTSEL(-1,2,2047) 	! 2334515
c  	  CALL PLOTSEL(-1,2,1024) 	! 2334850
c  	  CALL PLOTSEL(-1,512,1536) 	! 1916222	nov 13
C  	  CALL PLOTSEL(-1,945,1065) 	! 2340537
C  	  CALL PLOTSEL(-1,1373,1650) 	! 2340537
C  	  CALL PLOTSEL(-1,1350,1725) 	! 2343404
C  	  CALL PLOTSEL(-1,1725,1763) 	! 2343404
C  	  CALL PLOTSEL(-1,1763,2047) 	! 2343404
c  	  CALL PLOTSEL(-1,2,1024) 	! 2334850
C  	  CALL PLOTSEL(-1,2,450) 	! 2351754
C  	  CALL PLOTSEL(-1,563,750) 	! 2351754
C  	  CALL PLOTSEL(-1,900,1275) 	! 2351754
C  	  CALL PLOTSEL(-1,1275,1763) 	! 2351754
c  	  CALL PLOTSEL(-1,840,1260) 	! 2351697
c  	  CALL PLOTSEL(-1,1,300) 	! 2352052
c  	  CALL PLOTSEL(-1,525,712) 	! 2352052
c  	  CALL PLOTSEL(-1,1638,1775) 	! 2352052
c  	  CALL PLOTSEL(-1,1911,2048) 	! 2352052
c  	  CALL PLOTSEL(-1,1,273) 	! 2352027
c  	  CALL PLOTSEL(-1,412,675) 	! 2352027
c  	  CALL PLOTSEL(-1,1,375) 	! 2351327
C  	  CALL PLOTSEL(-1,750,1125)     ! 2351327
C  	  CALL PLOTSEL(-1,1875,1987)    ! 2351327
C  	  CALL PLOTSEL(-1,795,1177)     ! 2351837
C  	  CALL PLOTSEL(-1,1392,1605)    ! 2351837
c  	  CALL PLOTSEL(-1,202,525)      ! 2351763
c  	  CALL PLOTSEL(-1,900,1237)     ! 2351763
c  	  CALL PLOTSEL(-1,1463,1725)     ! 2351763
c  	  CALL PLOTSEL(-1,1853,2047)     ! 2351763
c  	  CALL PLOTSEL(-1,60,270)       ! 2351978
c  	  CALL PLOTSEL(-1,998,1200)     ! 2351978
c  	  CALL PLOTSEL(-1,1455,1800)    ! 2351978

c	  IF(IPROCESS.LE.1) CALL DETAIL(-2,900,1100)
	ENDIF
	IF(NPLOTS.LT.NREM) GO TO 110
	STOP
	END
	SUBROUTINE COMBO(ITERM)
C
C	THE FIRST (TOP) PANEL IS THE DATA IN T/M UNITS,
C	THE SECOND IS IN PHYSICAL UNITS, THE THIRD IS THE FREQ  FROM 
C	ZERO CROSSINGS, AND THE FOURTH IS THE FOURIER TRANSFORM
C
	CHARACTER*12 title(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	common /headblk/ major,minor,s_scet,nsys
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
	DIMENSION YY(2048),YYT(2048),PP(2048)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PUT LABELS ON RIGHT HAND SIDE
C
	XSTART = 350.
	XEND = 2000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,400.,XEND,3100.)
	ENDIF
	AVRFREQ=0.
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(19) = ' AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
C	XTITLE = GX2 +.006*(GX2-GX1)
	XTITLE = GX2 +.02*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  IF(N.EQ.19) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
C
C	PLOT TDS DATA IN TELEMETRY UNITS
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,2425.,XEND,3100.)
	ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	  DO N = 1,2048
  	    PP(N) = 1000.*(N-1)/SPS
	    YY(N) = NDATA(N)
	  ENDDO
	CALL MGOSETLIM(0.,-128.,PP(2048),128.)
	CALL MGOSETEXPAND(.8)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOCONNECT(PP,YY,2048)
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(5,'mSEC.')
	CALL MGOYLABEL(14,'T/M NUMBER-128')
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSXYZ,COMBO')
	CALL MGOSETEXPAND(1.)
C
C	PLOT TDS DATA IN PHYSICAL UNITS
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1605.,XEND,2275.)
	ENDIF
	  YMAX = 0.
c	  EFFLEN = 45.				  ! X ANTENNA  18-JUL-95
c	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 6.    ! Y ANTENNA   "
c	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 4.    ! Z ANTENNA   "
	  EFFLEN = 41.1				  ! X ANTENNA  23-SEP-96
	  IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 3.79  ! Y ANTENNA   "
	  IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 2.17  ! Z ANTENNA   "
	  ACORR = 1000.
	  IF(IRX.GE.7) THEN			! SEARCH COILS
		ACORR=1.
		EFFLEN = 1.
	  ENDIF
C	CHANGE TO mV/meter
	  DO N = 1,2048
C 	    PP(N) = N
	    YY(N) = ACORR*DATA(N)/EFFLEN
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
C
	PRINT*,'MAX mV/m',YMAX
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
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
	CALL MGOYLABEL(10,'mV/m or nT')
	CALL MGOSETEXPAND(1.)
C
C	PLOT FREQUENCY FROM ZERO CROSSINGS, ALSO SMOOTH FREQUENCY
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1120.,XEND,1455.)
	ENDIF

C
	SPSKHZ = .001*SPS
	YMIN = .5*SPSKHZ
	YMAX = 0.
	PRINT*,'IZCNT',IZCNT
	IF(IZCNT.GT.1) THEN
	  DO N = 1,IZCNT-1
	    IF(ZINT(N).EQ.0.) PRINT*,'IN PLOT, ZINT=0 AT',N
	    YY(N) = SPSKHZ/ZINT(N)
C 	      PP(N) = 1000.*(N-1)/SPS
	    PP(N) = 500.*(ZCROSS(N)+ZCROSS(N+1))/SPS
	    YMIN = AMIN1(YY(N),YMIN)
	    YMAX = AMAX1(YY(N),YMAX)
	  ENDDO
C	
C	  SMOOTH FREQUENCY
C
	  DO N = 2,IZCNT-2
	    YYT(N) = .4*YY(N) + .3*(YY(N+1)+YY(N-1))
c*******
c	   if(pp(n).lt.32.) yyt(n) = 0.
c	   if(pp(n).gt.35.) yyt(n) = 0.
	  ENDDO
	  YYT(1) = YY(1)
	  YYT(IZCNT-1) = YY(IZCNT-1)
	ENDIF
C
  	YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
	PRINT*,'YMIN,YMAX',YMIN,YMAX
	YMIN =  .9*AVRFREQ
	YMAX = 1.1*AVRFREQ
c	ymin = 8.
c	ymax = 17.
C	YMIN =  .6*AVRFREQ
C	YMAX = 1.6*AVRFREQ
	PRINT*,'SET TO   ',YMIN,YMAX
	CALL MGOSETEXPAND(.8)
	CALL MGOSETLIM(0.,YMIN,PP(2048),YMAX)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
c	CALL MGOCONNECT(PP,YYT,IZCNT-1)
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
	  CALL MGOSETLOC(XSTART,300.,XEND,970.)
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
 420	PRINT*,'DB,YMAX,YMIN',YMAX,YMIN
	BW = MAXCOUNT*SPSUSE/2048.
	PRINT*,'MAXCOUNT, BANDWIDTH',MAXCOUNT,BW
	XN1 = N1-2
	XN2 = N2+2
	YRANGE = YMAX-YMIN
	YMAX = YMAX + .05*YRANGE
	YMIN = YMIN - .05*YRANGE
	YMIN = AMAX1(YMIN,YMAX-70.)
	CALL MGOSETLIM(0.,YMIN,XMAX,YMAX)
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
	CALL MGOLABEL(10,'   10 dB  ')
	YTITLE = YTITLE-TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,' BANDWIDTH')
	YTITLE = YTITLE-TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,'  OF PEAK ')
	IF(TDS_CHANNEL.GT.2) WRITE(STR,1021) .001*BW
	IF(TDS_CHANNEL.LE.2) WRITE(STR,1022) BW
 1021	FORMAT(F7.2,' HZ')
 1022	FORMAT(F6.3,' kHZ')
	YTITLE = YTITLE - TINC
	CALL MGOGRELOCATE(XTITLE,YTITLE)
	CALL MGOLABEL(10,STR)
	CALL MGOSETEXPAND(1.)
C
C
	IF(IPROCESS.GE.3) THEN
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOSETEXPAND(.5)
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(8,' BAD TDS')
	  YTITLE = YTITLE-TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(9,'CORRECTED')
	  YTITLE = YTITLE-TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(STR,1024) IPROCESS
 1024	  FORMAT(' LEVEL',I2)
	  CALL MGOLABEL(8,STR)
	  WRITE(STR,1025) ISTART
 1025	  FORMAT(I5,' PTS')
	  YTITLE = YTITLE-TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(9,STR)
	  CALL MGOSETEXPAND(.8)
	ENDIF
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
	SUBROUTINE PLOTPART(ITERM)
C
C	PLOTS ONE COMPONENT AGAINST ANOTHER FOR A FRACTION OF AN
C		EVENT TO LOOK FOR POLARIZATION CHANGES
C
	CHARACTER*12 title(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*4 LABEL(4)
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	common /headblk/ major,minor,s_scet,nsys
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
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
	INTEGER*4 SUNCLOCK
	DIMENSION YY(2048),XX(2048),PP(2048)
	DATA LABEL /'EX','EY','EZ','B'/
C
	print*,'plotpart called'
	DO NN = 1,3
	  N1 = NN
	  N2 = N1+1
	  IF(NN.EQ.3) THEN
		N1 = 1
		N2 = 3
	  ENDIF
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,450.,2000.,3050.)
  	  ENDIF
C
c	  XTITLE = GX2 +.05*(GX2-GX1)
	  XTITLE = GX2 +.1*(GX2-GX1)              ! 3 dec 1996
	  YTITLE = GY2
	  TRANGE = GY2-GY1
	  TINC = .03*TRANGE
	  CALL MGOSETEXPAND(.6)
	  TITLE(6) = ' '
	  TITLE(7) = ' '
	  DO N = 1,18
	    YTITLE = YTITLE - TINC
	    IF(N.EQ.4) YTITLE = YTITLE - TINC
	    IF(N.EQ.6) YTITLE = YTITLE - TINC
	    IF(N.EQ.19) YTITLE = YTITLE - TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(12,TITLE(N))
	  ENDDO
C
C	  PLOT TDS DATA 
C
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	  IF(N2.EQ.4) N2 = 1
	  NXW = 3
	  NYW = 5
	  NW = NXW*NYW
	  DO IW = 1,NW
	    CALL MGOWINDOW(NXW,NYW,IW)
	    NPTST = (IW-1)*2048/NW + 1
	    NPTND = (IW)*2048/NW
	    NPT = 0	
	    XMAX = 0.
	    YMAX = 0.
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
C  	      PP(NPT) = 1000.*(N-1)/SPS
C	      XX(NPT) = XDATA(N,N1)
C	      YY(NPT) = XDATA(N,N2)
	      XX(NPT) = 1000.*XFDATA(N,N1)
	      YY(NPT) = 1000.*XFDATA(N,N2)
	      XMAX = AMAX1(XMAX,ABS(XX(NPT)))
	      YMAX = AMAX1(YMAX,ABS(YY(NPT)))
	    ENDDO
C	    XMAX = 1.1*XMAX
C	    YMAX = 1.1*YMAX
	    XMAX = 1.1*AMAX1(XMAX,YMAX)
	    YMAX = XMAX
	    CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(2,LABEL(N1))
	    CALL MGOYLABEL(2,LABEL(N2))
	    CALL MGOSETEXPAND(.8)
	    CALL MGOSETEXPAND(1.)
C
C
C		SPSKHZ = .001*SPS
C
	  ENDDO
C
	  IF(IPROCESS.GE.3) THEN
	    YTITLE = YTITLE-2.*TINC
	    CALL MGOSETEXPAND(.5)
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(8,' BAD TDS')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(9,'CORRECTED')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    WRITE(STR,1024) IPROCESS
 1024	    FORMAT(' LEVEL',I2)
	    CALL MGOLABEL(8,STR)
	    WRITE(STR,1025) ISTART
 1025	    FORMAT(I5,' PTS')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(9,STR)
	    CALL MGOSETEXPAND(.8)
	  ENDIF
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
	  CALL MGOSETEXPAND(.6)
	print*,'nsys',nsys
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,PLOTPART')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('VAR MX','[.WIND]TDSXYZ,PLOTPART')
	  CALL MGOSETEXPAND(.8)
	ENDDO
C
	RETURN
C
	END	
	SUBROUTINE PLOTSEL(ITERM,NPTST,NPTND)
C
C	PLOTS ONE COMPONENT AGAINST ANOTHER FOR A SELECTED FRACTION
C		 OF AN EVENT TO LOOK FOR POLARIZATION CHANGES
C
	CHARACTER*12 title(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*4 LABEL(4),LABEL1(4)
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	common /headblk/ major,minor,s_scet,nsys
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
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
	INTEGER*4 SUNCLOCK
	DIMENSION YY(2048),XX(2048),PP(2048)
	DATA LABEL /'EX','EY','EZ','B'/
	DATA LABEL1 /'E1','E2','E3','B'/
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	print*,'plotsel called'
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(600.,450.,1900.,3050.)
  	  ENDIF
C
c	  XTITLE = GX2 +.05*(GX2-GX1)
	  XTITLE = GX2 +.1*(GX2-GX1)              ! 3 dec 1996
	  YTITLE = GY2
	  TRANGE = GY2-GY1
	  TINC = .03*TRANGE
	  CALL MGOSETEXPAND(.6)
	  TITLE(6) = 'SAMPLES'
	  WRITE(STR,1007) NPTST,NPTND
 1007	  FORMAT(I4,' TO',I5)	
	  TITLE(7) = STR(1:12)
	  DO N = 1,18
	    YTITLE = YTITLE - TINC
	    IF(N.EQ.4) YTITLE = YTITLE - TINC
	    IF(N.EQ.6) YTITLE = YTITLE - TINC
	    IF(N.EQ.19) YTITLE = YTITLE - TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(12,TITLE(N))
	  ENDDO
C
C	  PLOT TDS DATA 
C
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	  XMAX = 0.
	  YMAX = 0.
c	PRINT*,'IN PLOTSEL, XFDATA=',(XFDATA(1,N),N=1,4)
    	  DO N = NPTST,NPTND
	     IF(ABS(XFDATA(N,1)).GT.XMAX) THEN
		XMAX = ABS(XFDATA(N,1))
		NXMAX = N
	     ENDIF
	     IF(ABS(XFDATA(N,2)).GT.YMAX.OR.XFDATA(N,3).GT.YMAX) THEN
	        YMAX = AMAX1(ABS(XFDATA(N,3)),ABS(XFDATA(N,2)))
		NYMAX = N
	     ENDIF
	  ENDDO
	  IF(1.855*YMAX.GT.XMAX) THEN
		NMAX = NYMAX
	  ELSE
		NMAX = NXMAX
	  ENDIF
	PRINT*,'PLOTSEL,NMAX,NXMAX,NYMAX',NMAX,NXMAX,NYMAX
	  XMAX = 1000.*XMAX
	  YMAX = 1000.*YMAX
	  XMAX = 1.1*AMAX1(XMAX,1.855*YMAX)
	  YMAX = XMAX/1.855
C
	  NXW = 1
	  NYW = 3
	  NW = NXW*NYW
	  DO IW = 1,NW
	    CALL MGOWINDOW(NXW,NYW,IW)
	    N1 = IW
	    N2 = N1+1
	    IF(IW.EQ.3) THEN
		N1 = 1
		N2 = 3
	    ENDIF
C
	    NPT = 0	
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
C  	      PP(NPT) = 1000.*(N-1)/SPS
	      XX(NPT) = 1000.*XFDATA(N,N1)
	      YY(NPT) = 1000.*XFDATA(N,N2)
	    ENDDO
	    CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(2,LABEL(N1))
	    CALL MGOYLABEL(2,LABEL(N2))
	    CALL MGOSETEXPAND(.8)
	    CALL MGOSETEXPAND(1.)
C	    PUT ON ARROW
	    NPTMAX = NMAX - NPTST + 1
	    SIZE = .01*SQRT(XMAX**2 + YMAX**2)
	    DX = XX(NPTMAX+1) - XX(NPTMAX)
	    DY = YY(NPTMAX+1) - YY(NPTMAX)
	    CALL ARROW(XX(NPTMAX),YY(NPTMAX),DX,DY,SIZE)
	if(n2.eq.2) then
	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
	  print*,nptmax,xx(nptmax),yy(nptmax)
	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
	endif
C
	  ENDDO
C
	  IF(IPROCESS.GE.3) THEN
	    YTITLE = YTITLE-2.*TINC
	    CALL MGOSETEXPAND(.5)
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(8,' BAD TDS')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(9,'CORRECTED')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    WRITE(STR,1024) IPROCESS
 1024	    FORMAT(' LEVEL',I2)
	    CALL MGOLABEL(8,STR)
	    WRITE(STR,1025) ISTART
 1025	    FORMAT(I5,' PTS')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(9,STR)
	    CALL MGOSETEXPAND(.8)
	  ENDIF
C
	  CALL MGOSETEXPAND(.6)
	print*,'nsys',nsys
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,PLOTSEL')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('VAR MX','[.WIND]TDSXYZ,PLOTSEL')
	  CALL MGOSETEXPAND(.8)
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
	SUBROUTINE SPPLOT(ITERM)
C
C	PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
	CHARACTER*12 title(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	common /headblk/ major,minor,s_scet,nsys
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
	print*,'spplot called'
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
	IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	DO N = N1,N2,2
	  NP = (N-1)/2
	  PP(NP) = NP*SPSUSE/2048.
	  YY(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2)
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
	YMAX = YMAX + .05*YRANGE
	YMIN = YMIN - .05*YRANGE
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
	IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
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
	CALL MGOPLOTID(EVENT,'[.WIND]TDSXYZ,SPPLOT')
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

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end
	SUBROUTINE FITSLOPE(X,Y,N,SLOPE,SUMSQ)
C
	DIMENSION X(1),Y(1),WT(2048)
C
	SUMSQ = 0.
	SXX = 0.
	SXY = 0.
	SYY = 0.	
	DO J = 1,N
	  WT(J) = .06 + SQRT(ABS(X(J)*Y(J)))
	  IF(WT(J).NE.0.) THEN
	    SXX =SXX + X(J)**2/WT(J)
	    SXY =SXY + X(J)*Y(J)/WT(J)
	    SYY =SYY + Y(J)**2/WT(J)
	  ENDIF
	ENDDO
C	SLOPE IS Y/X
	IF(SXX.NE.0.) THEN
		SLOPE = SXY/SXX
	ELSE
		PRINT*,'IN FITSLOPE, N,SXX,SYY=',N,SXX,SYY
		SLOPE=0.
	ENDIF
	SUMSQ = SXX*SLOPE**2 - 2.*SLOPE*SXY + SYY
	RETURN
	END
	SUBROUTINE XFORMSN
C
	REAL BEFORE(3),BAFTER(3),SN(3),CL(3),V3(3)
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	INTEGER*4 SUNCLOCK
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
	common /headblk/ major,minor,s_scet,nsys
	DATA AMOD,BMOD,CMOD /  25.6, 25.6, 14.6/
	DATA BEFORE /-2.366,.720,2.828/			! 19961116 1936:00
	DATA BAFTER /-1.462,1.360,9.976/		! 19961116 1939:04
C
C	CALCULATE SN = OUTWARD SHOCK NORMAL UNIT VECTOR, CL = CURRENT
C	LAYER UNIT VECTOR, V3 = THIRD DIRECTION
C
	print*,'xformsn called'

	NSYS = 1
C	SHOCK NORMAL
	      R02 = YRE**2 + ZRE**2
	      EXRATIO = (.5/CMOD)*(XRE + SQRT(XRE**2 + 4.*CMOD*R02/AMOD))
	      print*,'x,y,z,exratio', XRE,YRE,ZRE,EXRATIO
	      RTC = CMOD*EXRATIO
	      RTB = BMOD*EXRATIO
	      RTA = AMOD*EXRATIO
C
C	CALCULATE MODEL SHOCK NORMAL
C
	      SNX = 1./RTC
	      SNY = 2.*YRE/RTB**2
	      SNZ = 2.*ZRE/RTA**2
	      SNORM = SQRT(SNX**2 + SNY**2 + SNZ**2)
	      SN(1) = SNX/SNORM
	      SN(2) = SNY/SNORM
	      SN(3) = SNZ/SNORM
C
C	      CALCULATE CURRENT LAYER UNIT VECTOR
C
	  CLDOTSN = 0.
	  DO N = 1,3
	      CL(N) = BEFORE(N) - BAFTER(N)
	      CLDOTSN = CLDOTSN + CL(N)*SN(N)
	  ENDDO
	  DO N = 1,3
	      CL(N) = CL(N) - CLDOTSN*SN(N)
          ENDDO
          CLNORM = SQRT(CL(1)**2 + CL(2)**2 + CL(3)**2)
	  DO N = 1,3
		CL(N) = CL(N)/CLNORM
          ENDDO
C
C	CALCULATE V3 = AXIS NORMAL TO SN AND CL
C
	V3(1) = SN(2)*CL(3) - SN(3)*CL(2)
	V3(2) = SN(3)*CL(1) - SN(1)*CL(3)
	V3(3) = SN(1)*CL(2) - SN(2)*CL(1)
          V3NORM = SQRT(V3(1)**2 + V3(2)**2 + V3(3)**2)
	  DO N = 1,3
		V3(N) = V3(N)/V3NORM
          ENDDO
	  print*,'sn',sn
	  print*,'cl',cl
	  print*,'v3',v3
C
C	  SN,CL AND V3 ARE NOW IN THE GSE SYSTEM
C	  TRANSFORM THEM TO THE ANTENNA SYSTEM, EX, EY, EZ, BY ROTATING
C	  X AND Y.  Z gse is parallel to Z ant already
C
	  ANGLE =  -360.*SUNCLOCK/4096. + 45.       ! ANGLE SUN TO +EX
	  SINANG = SIND(ANGLE)
	  COSANG = COSD(ANGLE)
	  DO N = 1,3
		XT   =  SN(1)*COSANG + SN(2)*SINANG 
		SN(2) = SN(2)*COSANG - SN(1)*SINANG 
		SN(1) = XT
		XT   =  CL(1)*COSANG + CL(2)*SINANG 
		CL(2) = CL(2)*COSANG - CL(1)*SINANG 
		CL(1) = XT
		XT   =  V3(1)*COSANG + V3(2)*SINANG 
		V3(2) = V3(2)*COSANG - V3(1)*SINANG 
		V3(1) = XT
	  ENDDO
C
C	NOW SN,CL AND V3 ARE IN THE SPACECRAFT ANTENNA SYSTEM
C	TRANSFORM ELECTRIC FIELDS TO SN,CL,V3 SYSTEM
	DO N = 1,2048
	  XFDATA(N,1) = XDATA(N,1)*SN(1) + XDATA(N,2)*SN(2) + 
     1		XDATA(N,3)*SN(3)
	  XFDATA(N,2) = XDATA(N,1)*CL(1) + XDATA(N,2)*CL(2) + 
     1		XDATA(N,3)*CL(3)
	  XFDATA(N,3) = XDATA(N,1)*V3(1) + XDATA(N,2)*V3(2) + 
     1		XDATA(N,3)*V3(3)
	ENDDO
	DO N1 = 1,3
	  N2 = N1 + 1
	  spr = 0.
	  xsq = 0.
	  ysq = 0.
C	  IF(N1.EQ.1) THEN
C	    XLEN = 41.1				  ! X ANTENNA  23-SEP-96
C	    YLEN = 3.79  			  ! Y ANTENNA   "
C	  ENDIF
C	  IF(N1.EQ.2) THEN
C	    XLEN = 3.79  			  ! Y ANTENNA   "
C	    YLEN = 2.17				  ! Z ANTENNA   "
C	  ENDIF
C	  IF(N1.EQ.3) THEN
C	    XLEN = 2.17				  ! Z ANTENNA   "
C	    YLEN = 41.1				  ! X ANTENNA  23-SEP-96
C	  ENDIF
	  IF(N2.EQ.4) N2 = 1
C
C	XDATA IS ALREADY ELECTRIC FIELD, I.E. V DIVIDED BY EFF. LENGTH
C
	  DO N = 1,2048
	    xsq = xsq + xFdata(n,N1)**2
	    ysq = ysq + xFdata(n,n2)**2
	    spr = spr + xFdata(n,n1)*xFdata(n,n2)
C	    IF(LEVEL.GE.1) THEN
C		WRITE(67,*) N,(XDATA(N,NN),NN=1,4)
C	    ENDIF
	  ENDDO

C	  slope is y/x
C	  CHANGED TO EY/EX ON 15 NOV 1996
C
	  CALL FITSLOPE(XFDATA(1,N1),XFDATA(1,N2),2048,SLOPE,SUMSQ)
	  IF(XSQ*YSQ.NE.0.) print*,'corr',N1,N2,spr/sqrt(xsq*ysq)
C	  ANGLE = ATAND(SLOPE) + 360.*SUNCLOCK/4096. + 45.
C	  ANGLE = -AMOD(ANGLE,180.)

	  IF(XSQ*YSQ.NE.0.) write(79,*)' '
	  IF(XSQ*YSQ.NE.0.) write(79,*)'IN SN,CL,V3 SYSTEM '
	  IF(XSQ*YSQ.NE.0.) write(79,*)'corr',N1,N2,spr/sqrt(xsq*ysq)
	  IF(XSQ.NE.0.) print*,'E FIELD slope, SQERR',SLOPE, SUMSQ
C	  IF(XSQ.NE.0.) WRITE(79,*) ' SLOPE(EY/EX),SUNCLOCK',
C     1		SLOPE,SUNCLOCK
C	  IF(XSQ.NE.0..AND.N1.EQ.1) WRITE(79,*) 'ANGLE SUN TO E,GSE',ANGLE
C	WRITE(79,*) 'xlen,ylen',xlen,ylen
	ENDDO
C
	CALL VMATRIX(XDATA,1,2048)
C
	RETURN
	END
	SUBROUTINE XFORMVM(XINP)
C
C	THIS TRANSFORMS THE ELECTRIC FIELD, XINP,INTO THE VARIANCE MATRIX
C	EIGENSYSTEM, 1 = LARGEST EIGENVALUE.  XFDATA ARE THE TRANS-
C	FORMED FIELDS
C
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
	common /headblk/ major,minor,s_scet,nsys
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	INTEGER*4 SUNCLOCK
	REAL XINP(2050,4)
C
C	EVECT(I,J) IS THE Ith COMPONENT OF THE Jth EIGENVECTOR
C
C	VMATRIX CALCULATES THE VARIANCE MATRIX OF XGSE, AND RETURNS
C	EIGENVALUES IN COMMON /VARMATX/EVAL, AND EIGENVECTORS IN EVECT
C
	CALL VMATRIX(XGSE,1,2048)
	NSYS = 2
C
C	NOW EVECT ARE EIGENVECTORS IN THE XINP SYSTEM
C	XFDATA(N,3) CORRESPONDS TO LARGEST EIGENVECTOR
C
	DO N = 1,2048
	  XFDATA(N,1) = XINP(N,1)*EVECT(1,1) + XINP(N,2)*EVECT(2,1) + 
     1		XINP(N,3)*EVECT(3,1)
	  XFDATA(N,2) = XINP(N,1)*EVECT(1,2) + XINP(N,2)*EVECT(2,2) + 
     1		XINP(N,3)*EVECT(3,2)
	  XFDATA(N,3) = XINP(N,1)*EVECT(1,3) + XINP(N,2)*EVECT(2,3) + 
     1		XINP(N,3)*EVECT(3,3)
	ENDDO
C
	WRITE(79,*) 'IN THE VARIANCE MATRIX SYSTEM, EIGENVALUES'
	WRITE(79,*)  EVAL
	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
	WRITE(79,*)(EVECT(1,I),I=1,3)
	WRITE(79,*)(EVECT(2,I),I=1,3)
	WRITE(79,*)(EVECT(3,I),I=1,3)
C
	DO N1 = 1,3
	  N2 = N1 + 1
	  spr = 0.
	  xsq = 0.
	  ysq = 0.
	  DO N = 1,2048
	    xsq = xsq + xFdata(n,N1)**2
	    ysq = ysq + xFdata(n,n2)**2
	    spr = spr + xFdata(n,n1)*xFdata(n,n2)
C	    IF(LEVEL.GE.1) THEN
C		WRITE(67,*) N,(XDATA(N,NN),NN=1,4)
C	    ENDIF
	  ENDDO

C	  slope is y/x
C	  CHANGED TO EY/EX ON 15 NOV 1996
C
	  CALL FITSLOPE(XFDATA(1,N1),XFDATA(1,N2),2048,SLOPE,SUMSQ)
	  IF(XSQ*YSQ.NE.0.) print*,'corr',N1,N2,spr/sqrt(xsq*ysq)
C	  ANGLE = ATAND(SLOPE) + 360.*SUNCLOCK/4096. + 45.
C	  ANGLE = -AMOD(ANGLE,180.)

	  IF(XSQ*YSQ.NE.0.) write(79,*)' '
	  IF(XSQ*YSQ.NE.0.) write(79,*)'IN VAR MATRIX SYSTEM '
	  IF(XSQ*YSQ.NE.0.) write(79,*)'corr',N1,N2,spr/sqrt(xsq*ysq)
	  IF(XSQ.NE.0.) print*,'E FIELD slope, SQERR',SLOPE, SUMSQ
C	  IF(XSQ.NE.0.) WRITE(79,*) ' SLOPE(EY/EX),SUNCLOCK',
C     1		SLOPE,SUNCLOCK
C	  IF(XSQ.NE.0..AND.N1.EQ.1) WRITE(79,*) 'ANGLE SUN TO E,GSE',ANGLE
C	WRITE(79,*) 'xlen,ylen',xlen,ylen
	ENDDO
c
	RETURN
	END
	SUBROUTINE VMATRIX(DATA,N1,N2)
C
C	CALCULATES VARIANCE MATRIX AND EIGENVALUES
C
C	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
C     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	REAL DATA(2050,4),DATAS(2050,3),VMAT(3,3)
	REAL SN(3),CL(3),V3(3),AV(3),STD(3)
	INTEGER*4 SUNCLOCK
C
C	subtract averages
C
	WRITE(79,*) ' '	
	WRITE(79,*) 'VARIANCE MATRIX FOR SAMPLES',N1,' TO',N2	
	DO M = 1,3
	  AV(M) = 0.
	  DO N = N1,N2
		AV(M) = AV(M) + DATA(N,M)
	  ENDDO
	  AV(M) = AV(M)/(N2-N1+1)
	ENDDO
	DO M = 1,3	
	  DO N = N1,N2
		DATAS(N,M) = DATA(N,M) - AV(M)
	  ENDDO
	ENDDO
	M1 = 1
	M2 = M1+1
	XSQ = 0.
	YSQ = 0.
	SPR = 0.
	  DO N = 1,2048
	    xsq = xsq + datas(n,M1)**2
	    ysq = ysq + datas(n,M2)**2
	    spr = spr + datas(n,M1)*datas(n,M2)
	  ENDDO
	PRINT*,'SN RMS',SQRT(XSQ/2048.),'  CL RMS',SQRT(YSQ/2048.),
     1 '  CORR',spr/sqrt(xsq*ysq)
	WRITE(79,*)'SN RMS',SQRT(XSQ/2048.),'  CL RMS',SQRT(YSQ/2048.),
     1	 '  CORR', spr/sqrt(xsq*ysq)
C
	PRINT*,' WITH AVERAGES REMOVED'
C
C	FORM VARIANCE MATRIX
C	
	DO K = 1,3
	  DO M = 1,3
		VMAT(K,M) = 0.
	  ENDDO
	ENDDO
C	
	DO N = N1,N2
	  DO K = 1,3
	    DO M = K,3
		VMAT(K,M) = VMAT(K,M) + DATAS(N,K)*DATAS(N,M)
	    ENDDO
	  ENDDO
	ENDDO	
C
C	CALCULATE EIGENVALUES AND MATRIX OF EIGENVECTORS
C
	CALL JACOBI(VMAT,3,3,EVAL,EVECT,NROT)
C	PRINT*,'EIGENVALUES',EVAL
C	PRINT*,'EIGENVECTOR'
C	PRINT*,(EVECT(1,I),I=1,3)
C	PRINT*,(EVECT(2,I),I=1,3)
C	PRINT*,(EVECT(3,I),I=1,3)
C
C	SORT IN ASCENDING ORDER OF EIGENVALUES
	CALL EIGSRT(EVAL,EVECT,3,3)
C
	PRINT*,'IN THE S/C ANTENNA SYSTEM, EIGENVALUES'
	PRINT*,EVAL
	PRINT*,'EIGENVECTORS'
	PRINT*,(EVECT(1,I),I=1,3)
	PRINT*,(EVECT(2,I),I=1,3)
	PRINT*,(EVECT(3,I),I=1,3)
	WRITE(79,*) 'IN THE S/C ANTENNA SYSTEM, EIGENVALUES'
	WRITE(79,*)  EVAL
	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
	WRITE(79,*)(EVECT(1,I),I=1,3)
	WRITE(79,*)(EVECT(2,I),I=1,3)
	WRITE(79,*)(EVECT(3,I),I=1,3)
	RETURN
	END	
	SUBROUTINE XFGSE
C
C	THIS TRANSFORMS THE ELECTRIC FIELD INTO THE GSE SYSTEM
C	THE INPUT IS THE ELECTRIC FIELD IN THE SPACECRAFT ANTENNA
C	SYSTEM IN XDATA, THE OUTPUT IS THE ELECTRIC FIELD IN THE
C	GSE SYSTEM, IN XGSE
C
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	INTEGER*4 SUNCLOCK
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
	common /headblk/ major,minor,s_scet,nsys
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	DATA TWOPI /6.2831853/
C
C	  THE FIELDS ARE IN THE ANTENNA SYSTEM
C	  TRANSFORM THEM TO THE GSE SYSTEM, EX, EY, EZ, BY ROTATING
C	  X AND Y, and INVERTING  Z gse is parallel to Z ant already,
C	   but opposite
C
	ANGLE =  -360.*SUNCLOCK/4096. - 45.       ! ANGLE SUN TO +EX AT END
	DANG = SPINRATE*360./SPSS/TWOPI
	ANGLE = ANGLE + 2048.*DANG		  ! ANGLE SUN TO +EX AT START
	DO N = 1,2048
		ANGLE = ANGLE - DANG
	        SINANG = SIND(ANGLE)
	        COSANG = COSD(ANGLE)
		XGSE(N,1) = XDATA(N,1)*COSANG + XDATA(N,2)*SINANG 
		XGSE(N,2) =-XDATA(N,2)*COSANG + XDATA(N,1)*SINANG 
		XGSE(N,3) = -XDATA(N,3)
	ENDDO
C
C	NOW XGSE ARE IN THE GSE SYSTEM
C
	RETURN
	END
	SUBROUTINE ARROW(X,Y,DX,DY,SIZE)
C
C	makes an arrowhead, near x,y in user coords, size as fraction
c	of graph size, direction dx,dy  from  x,y
C	MONGO MUST BE OPENED AND WORKING
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
C	CALCULATE DXP,DYP PERPENDICULAR TO DX,DY
C
	print*,'arrow',x,y,dx,dy,size
	DXL = DX*SIZE
	DYL = DY*SIZE
	DXP = .5*DYL
	DYP = -.5*DXL
	CALL MGORELOCATE(X+DX,Y+DY)
	CALL MGODRAW(X+DXP,Y+DYP)
	CALL MGORELOCATE(X+DX,Y+DY)
	CALL MGODRAW(X-DXP,Y-DYP)
C
	RETURN
	END
	SUBROUTINE PARTSPEC(CH,NSTART,NVAR,NDATA,NSPECT,DATA,PDATA,SPECT)
C
C	THIS ROUTINE DOES FOURIER ANALYSIS ON PART OF AN EVENT.  
C	CH IS THE TELEMETRY CHANNEL OPENED IN THE MAIN SECTION
C	NSTART IS THE SAMPLE NUMBER OF THE FIRST SAMPLE, NVAR IS THE
C	NUMBER OF SAMPLES TO BE FOURIER ANALYSED.  NVAR MUST BE A POWER
C	OF TWO.  NDATA IS THE RAW DATA FROM THE EVENT, NSPECT IS THE
C	NUMBER OF DIFFERENT SPECTRA TO BE DONE
C
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	INTEGER*4 NUMBER_EVENT,TDS_CHANNEL
	CHARACTER*4 EVENT
	COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	character*32	item
	REAL DATA(2050),SPECT(2050),PDATA(2050)
	INTEGER CH,NDATA(2048)
	CHARACTER*12 TITLE(20)
	CHARACTER*20 STR
	DIMENSION YY(2048),PP(2048)
C
	ITERM = -1
C	ITERM = 3
	NHALF = NVAR/2
	HALF = NHALF
	print*,'in partspec, chann,isps,sps=',tds_channel,isps,sps
	DO NS = 1,NSPECT
	  NSTT = NSTART + (NS-1)*NVAR
	  NP = 0
	  DO N = NSTT, NSTT+NVAR-1
	    NP = NP+1
	    DATA(N) = TDSCAL(TDS_CHANNEL,ISPS,NDATA(N)+128)
	    PDATA(NP) = DATA(N)
C	    NDATA(IK) = NDATA(IK)-128
  	  ENDDO
C
C***************FOR CALS, THERE IS A LARGE DC COMPONENT, WHICH APPEARS
C		AS A DECLINING EXPONENTIAL DUE TO THE CAPACITIVE
C		COUPLING IN THE TDSDET.  TAKE IT OUT
C		ALSO THE SIGN IS CHANGED BY DRIVING EITHER THE + OR -
C		PREAMP.  CHANGE TO STANDARD ALL PLUS.
C
C******THIS IS IN SECTIONS
C	N1 = NVAR/16
C	DO NSEC = 1,16
C	  AVR = 0.	
C	  COUNT = 1.E-8
C	  DO NPT = 1,N1
C	    NP = (NSEC-1)*N1 + NPT
C	    AVR = AVR + PDATA(NP)
C	    COUNT = COUNT + 1.
C	  ENDDO
C	  WRITE(77,*) NUMBER_EVENT,TDS_CHANNEL,NSEC,AVR/COUNT
C	ENDDO
C	  		
C	MAKE A LEAST SQ FIT TO EXP(-T/RC)
	DELT = 1./SPS
	RC = .033
	PRINT*,'TIME STEP',DELT
	  SUM1 = 0.	
	  SUM2 = 0.
	  SUM3 = 0.	  
	  COUNT = 1.E-8
	  DO NP = 1,NVAR
	    SUM3 = SUM3 + PDATA(NP)**2
	    THEORY = EXP(-(NP-1)*DELT/RC)
	    SUM2 = SUM2 - 2.*THEORY*PDATA(NP)
	    SUM1 = SUM1 + THEORY**2
	    COUNT = COUNT + 1.
	  ENDDO
	  ECOEF = -.5*SUM2/SUM1
	  WRITE(77,*) NUMBER_EVENT,TDS_CHANNEL,ECOEF
C	  IF(ECOEF.GT.0.) THEN
		DO NP = 1,NVAR
		  THEORY = ECOEF*EXP(-(NP-1)*DELT/RC)
		  PDATA(NP) = PDATA(NP) - THEORY
		ENDDO
C	  ELSE
C		DO NP = 1,NVAR
C		  THEORY = ECOEF*EXP(-(NP-1)*DELT/RC)
C		  PDATA(NP) =-(PDATA(NP) - THEORY)
C		ENDDO
C	  ENDIF
C************
C
	  CALL REALFT(PDATA,NHALF,1)
C
C	  CORRECT FOURIER ANALYSED SIGNAL FOR FREQUENCY RESPONSE
C
	  IF(TDS_CHANNEL.GT.2) THEN
	   item = 'SLOW_RX_FILTER'
	  ELSE
	   item = 'FAST_RX_FILTER'
	  ENDIF
	   ok = W_ITEM_i4(ch, item, ifil, 1, return_size)
C
	  ICORR = 1
	  IF(ICORR.EQ.1) THEN
		PRINT*,'CORECTED FOR GAINS'
	     DO IK = 3,NVAR,2
		FCOEF = CMPLX(PDATA(IK),PDATA(IK+1))
	        FREQ = SPS*(IK-1)/(2.*NVAR)
	        EJUNK = PREAMP(IRX,FREQ)
		FCOEFT = FCOEF/CONJG(CGAIN)
		CCORR = CGAIN
		EJUNK = TDSDET(TDS_CHANNEL,FREQ)
		FCOEFT = FCOEFT/CONJG(CGAIN)
		CCORR = CGAIN*CCORR
		EJUNK = TDS_FILTER(TDS_CHANNEL,IFIL+1,FREQ)
		FCOEFT = FCOEFT/CONJG(CGAIN)
		CCORR = CGAIN*CCORR
		FCOEF = FCOEFT
		PDATA(IK) = FCOEF
		PDATA(IK+1) = AIMAG(FCOEF)
	     ENDDO
	  ENDIF
C
C	  NOW PDATA CONTAINS FOURIER COEFFS CORRECTED FOR FREQUENCY RESPONSE 
C
C	  CALCULATE SPECTRUM, IN DB WRT 1 V**2/HZ
C
	  SNORM = 20.*ALOG10(HALF)
	  IK = 1
	  ISP = 1
	  SPECT(ISP) = -SNORM
	  IF(PDATA(IK).NE.0.)SPECT(ISP) = 20.*ALOG10(ABS(PDATA(IK)))-SNORM
	  DO IK = 3,NVAR,2
	      ISP = ISP+1
	      SPECT(ISP) = 10.*ALOG10(PDATA(IK)**2 + PDATA(IK+1)**2)-SNORM
	  ENDDO
C
C	  CALL REALFT(PDATA,NHALF,-1)
C
C	  PLOT FOURIER SPECTRUM
C
	  IF(1) return
	print*,'call mgowindow, 1,',nspect,ns
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	IF(ITERM.LT.0) THEN
c	  CALL MGOSETLOC(500.,400.,2000.,3100.)
	  CALL MGOSETLOC(350.,400.,2000.,2800.)  	! TO FIT PUBCOMBO
	ELSE
	  CALL MGOSETLOC(75.,150.,900.,700.)
	ENDIF
	TRANGE = GY2-GY1
	TINC = .04*TRANGE
	XTITLE = GX2 +.03*(GX2-GX1)
	YTITLE = GY2
	CALL MGOSETEXPAND(.8)
	AVRFREQ=0.
C	IF(IZCNT.GT.1) THEN
C	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
C	  AVRFREQ = .001*SPS/AVRPER
C	ENDIF
	TITLE(19) = 'AVR.FREQ.'
	WRITE(TITLE(20),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	YTITLE = GY1 + .3*(GY2-GY1)
	CALL MGOGRELOCATE(GX1-.12*GX2,YTITLE)
	CALL MGOSETANGLE(90.)
	CALL MGOLABEL(17,'POWER, DB V\u2/HZ')
	CALL MGOSETANGLE(0.)
C
	IF(ITERM.LT.0) THEN
	  IF(NSPECT.EQ.1) CALL MGOSETLOC(500.,1300.,2000.,2200.)
	  IF(NSPECT.EQ.2) CALL MGOSETLOC(500.,850.,2000.,2650.)
	  IF(NSPECT.GT.2) CALL MGOSETLOC(500.,400.,2000.,3100.)
	  CALL MGOSETLOC(350.,400.,2000.,1120.)  	! TO FIT PUBCOMBO
	ELSE
	  CALL MGOSETLOC(75.,150.,900.,700.)
	ENDIF
	
	  IF(NS.GT.1)  THEN
	    CALL MGOWINDOW(1,NSPECT,NS)
	  ELSE
	    CALL MGOSETLOC(500.,1300.,2000.,2200.)
	  CALL MGOSETLOC(350.,400.,2000.,1120.)  	! TO FIT PUBCOMBO
	  ENDIF
C
C
	  N1 = 3
	  N2 = NVAR
	  YMAX = -500.
	  YMIN =  500.
	  XMAX = 0.
	  SPSUSE = SPS
	  IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
	  DO N = N1,N2,2
	    NP = (N-1)/2
	    PP(NP) = NP*SPSUSE/NVAR
	    YY(NP) = SPECT(NP) 
	    XMAX = AMAX1(XMAX,PP(NP))
	    IF(YY(NP).GT.YMAX) THEN
	      NPSV = NP
	      YMAX = YY(NP)
	    ENDIF
	    YMIN = AMIN1(YMIN,YY(NP))
	  ENDDO
C
	  XN1 = N1-2
	  XN2 = N2+2
	  YRANGE = YMAX-YMIN
	  YMAX = YMAX + .05*YRANGE
	  YMIN = YMIN - .05*YRANGE
	  YMIN = AMAX1(YMIN,YMAX-80.)
	  PRINT*,'SET YLIMITS',YMIN,YMAX
	  CALL MGOSETLIM(0.,YMIN,XMAX,YMAX)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
	  CALL MGOSETLTYPE(0)
C	  CALL MGOSETEXPAND(.6)
	  CALL MGOCONNECT(PP,YY,NP)
C	  CALL MGOPOINTS(60.,1,PP,YY,NP)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOBOX(1,2)
	print 1017, nstt,nstt+nvar-1
	WRITE(STR,1017)  NSTT, NSTT+NVAR-1
 1017	  FORMAT('SAMPLES',I5,' TO',I5)
	  CALL MGOYLABEL(20,STR)
	  IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
	  IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(9,'FREQ, kHZ')
C	  YTITLE = GY2
C	  TRANGE = GY2-GY1
C	  TINC = .1*TRANGE
C	  YTITLE = YTITLE-TINC
C	  CALL MGOGRELOCATE(XTITLE,YTITLE)
C	  CALL MGOLABEL(10,'   10 dB  ')
C
	ENDDO
C
	CALL MGOPLOTID('PARTSPEC','[.WIND]TDSPRO')
	CALL MGOSETEXPAND(1.)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	  STOP
	ENDIF
C
	RETURN
	END
