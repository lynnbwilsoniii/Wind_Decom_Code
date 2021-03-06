	PROGRAM TDSXYZ
C
C	PLOTS TDS X, Y AND Z AND FREQUENCY SPECTRUM DIFFERENCE
C	a serious error was corrected 30-dec-1996 1600  CONJG(CGAIN)
C		SOME FIXES ON 30-OCT-2008 TO MAKE IT WORK FOR BOTH 3E AND 3B
C		I ALSO DISCOVERED A LOT OF TDSF STUFF THAT IS TO BE TAKEN 
C		OUT.  IT SHOULDNT WORK UNLESS THERE ARE ALL THREE COMPONENTS
C		NOTE THAT IFASTSLOW MUST STILL BE SET TO DECIDE 3B OR 3E
C
C	implicit	ALL
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ipacal(6,4)
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
	integer*4	ifull(6)
	character*80	file
	character*32	item
	character*20	mfi_file
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
	REAL		AngEVtoB(3)
	real 		FILTER,ffilter,sfilter,fsps,ssps,sps
	REAL 		S1,S2,PP(2050),BX,BY,BZ
	REAL		BXS(2000),BYS(2000),BZS(2000),BTS(2000)
	REAL*8		BTIMS(2000)
	REAL 		XDATA
C	REAL 		XSPECT(1025,4)
C	FOR PARTSPEC:
	REAL		GNIP,GNRP
	INTEGER*4 	NHALF,NVAR,NSPECT,NSTART
	INTEGER*4	NEVTSV(4),IANTSV(4)
	REAL		PDATA(2050)
C	REAL 		XPHASE(1025,4),DPHASE(1025,4)
	REAL 		EFFLEN(9)
!
	common /nrblk/ nrem,NHRMN,IFASTSLOW,ANGEVTOB
	common /headblk/ major,minor,s_scet,nsys
	COMMON /XFER/ SPHASE(1025)
	COMMON /PROCBLK/ XPHASE(1025,4),DPHASE(1025,4),XSPECT(1025,4)
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4), AVRFREQ
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	COMMON /FRLIMITS/ FREQMIN,TOPFREQ
	COMMON /EXTRA/ NBXBY,MDATA(2048,4),XSPEC(1025,4),AVRB,STDB
	COMMON /ZFREQ/ ZCFREQ(1024,4)
C
C	IN THIS PROGRAM, THE PRIMARY DATA--E FIELD IN S/C ANTENNA
C	SYSTEM-- ARE KEPT IN XDATA(N,J), J=1 IS EX, J=4 IS B,  THE
C	DATA IN THE GSE SYSTEM ARE KEPT IN XGSE, AND DATA IN
C	WHATEVER IS THE LATEST RESULT OF TRANSFORMATIONS ARE IN
C	XFDATA.  NSYS KEEPS TRACK OF WHAT SYSTEM XFDATA ARE IN. 
C	(SEE NSYS BELOW)
C
C	NSYS = 0 IS S/C ANTENNA SYSTEM, = 1 IS SHOCK NORMAL AND
C	CURRENT LAYER SYSTEM (SN), = 2 IS VARIANCE MATRIX PRINCIPAL
C	AXIS SYSTEM, = 3 IS GSE
C
	CHARACTER*12 PTITLE(25)
	INTEGER*4 TDSCH,hkch,fillch,ch
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025),END_ANGLE,DANG,TPAUSE
	COMMON /HEADBL/ PTITLE,EVENT
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	REAL AVRB(1025,4),STDB(1025,4)
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
C	IFULL(I) = 0 MEANS XDATA(N,I) IS EMPTY, = 1 MEANS FULL
	DATA IFULL /6*0/
	DATA IFASTSLOW /2/		! 1 IS 3 E's,SLOW, 2 IS 3 B's,SLOW
	DATA ICHEOF /0/			! 1 IS EOF ON FILL, 2 ON TDS
	DATA EFFLEN /41.1,3.79,2.17,41.1,3.79,2.17,3*1./
	DATA FFTMIN /3.3/
	DATA FFTMAX /800./
C	LEVEL = 0 IS JUST PRINT, 1 IS WRITE FILES, 2 IS DO PLOTS
	DATA LEVEL /2/
	DATA IFOUND /0/
	DATA NSYS /0/
	DATA INREAD /0/
	DATA  TPAUSE /10./
	DATA NBXBY /1/
C
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = 'TIME DOMAIN'
	PTITLE(3) = ' SAMPLER'
	PTITLE(4) = ' EVENT NO.'
	PTITLE(8) = 'SAMPLE RATE'
	PTITLE(10) = 'L.P.FILTER'
C	PTITLE(12) = 'TRIGGERS'
	PTITLE(15) = 'SCET'
	PRINT*,' '
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
	IF(IFASTSLOW.LT.1) THEN
	  PRINT*,'THIS PROGRAM ONLY WORKS ON TDSS, CHECK IFASTSLOW'
	  STOP
	ENDIF
C
	IF(INREAD.EQ.0) THEN
	  OPEN(UNIT=49,FILE='[KELLOGG.WIND]BBACK2.DAT',STATUS='OLD',
     1		 READONLY)
	  READ(49,1002) JUNK
	  DO N = 1,4
	    DO NS = 1,1025
	      READ(49,*) FREQT,NNT,COUNTT,AVRT,STDT
	      STDB(NS,NNT) = STDT
	      AVRB(NS,NNT) = AVRT
	    ENDDO
C	    PRINT*,'LAST READIN',FREQT,NNT,COUNTT,AVRT,STDT
	    READ(49,1002) JUNK
	    PRINT*,JUNK
	  ENDDO
	  PRINT*,'NOISE DATA READ IN'
	  INREAD = 1
	  CLOSE(UNIT=49)
	ENDIF
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
	  write(6,6)
	  write(6,7)
	  read(5,*,err=10,end=20)  iend,n2
	  type*,iend,n2
	    if(iend.eq.1) then
		nrem = n2
	    endif
	    if(iend.eq.2) then
		nrem = 4
		nevent = n2
	    endif
	    if(iend.eq.3) then
		nrem = 4
		nhhmmss = n2
	    endif
	endif
	type*,'type desired process level,0=raw,1=raw volts,2=fft,fft-1'
	type*,'3 = gain corrected to peak freq, 4 = fft and fix bad a/d'
	read(5,3) iq,iprocess
	type*,' '
	if(iprocess.eq.0) type*,'ok, plot data in tm numbers - 128'
	if(iprocess.eq.1) type*,'ok, plot in volts, unity freq. response.'
	if(iprocess.ge.2) type*,'ok, plot volts, corrected for freq. response.'
	if(iprocess.eq.4) type*,'            and corrected for bad TDS data'
	type*,' '
c
  5	format(q,a)
  4	format(1x,'enter 1,number of events to find and process')
  6	format(1x,'   or 2, event number')
  7	format(1x,'   or 3, time of event, HHMMSS')
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
        ok = w_event(tdsch,'TDSS')
	PRINT*,'RETURN FROM W_EVENT',TDSCH
	if(ok.eq.82) then
	   scettds = 10000.       	! artificially large
	   if(icheof.eq.1) stop
	   icheof = 2
	else
	  item = 'EVENT_SCET'
	  ok = w_item_i4(tdsch, item, s_scet, 2, return_size)
	  print*,'initial tdsch time',s_scet
	endif
c
        ok = w_event(fillch,'FILL')
	if(ok.eq.82) then
		if(icheof.eq.2) stop 'end of file on tdsch'
		icheof = 1
	endif
	if (.not. ok) stop 'cannot get fill event'
c	item = 'EVENT_SCET_R8'
c	  ok = w_item_r8(fillch, item, scet, 2, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(fillch, item, s_scet, 2, return_size)
	print*,'initial fillch time',s_scet
	  
c
	ok = w_channel_filename(tdsch,file)
C	IF(LEVEL.GE.1) write(87,*) file
	print*,file
c
	get_tm_stream = 1
	IEVENTCOUNT = 0
c
C	GET NEXT EVENT
C
 110    continue
C
	! this is the main program loop

	IF(IFOUND.EQ.0) THEN			! DONT CHANGE CHANNEL WHEN
	  if(scettds.lt.scetfill) then		! EVENT HAS BEEN FOUND
	    event = 'TDSF'
	    IF(IFASTSLOW.NE.0)event = 'TDSS'
	    ch = tdsch
	  else
	    event = 'FILL'
	    ch = fillch
	  endif
	ENDIF
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
	      IEVENTCOUNT = IEVENTCOUNT+1
	      item = 'EVENT_NUMBER'
	      ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
	      PRINT*,'EVENT NO.',ITEMP
	      NEVTSV(IEVENTCOUNT) = ITEMP
	      item = 'EVENT_SCET_R8'
	      ok = w_ITEM_R8(ch, item, SCETT, 1, return_size)
	      call w_ur8_to_ydoy(scett,yyyy,doy,msday)
	      item = 'EVENT_SCET'
	      ok = w_ITEM_I4(ch, item, S_SCET, 2, return_size)
C
	     if(ch.eq.tdsch) then
	       scettds = scett
	     else
	       scetfill = scett
	     endif
C
C	CHECK THAT ALL EVENTS ARE THE SAME
C
	  if(iend.eq.2.and.itemp.ne.nevent) go to 110
	  if(iend.eq.3.and.S_SCET(2).ne.nhhmmss) go to 110
	  IFOUND = 1
	  write(79,*) file
	  write(79,*) 'EVENT NUMBER',ITEMP
	item = 'SOURCE'
	ok = wind_tm_get_item(ch, item, isrc, 1, return_size)
	IF(IFASTSLOW.EQ.1) NSRC = ISRC-3	
	IF(IFASTSLOW.EQ.2) NSRC = ISRC-6
	IF(IFASTSLOW.EQ.2.AND.ISRC.LT.7) NSRC = 4
	IF(IFASTSLOW.EQ.1.AND.ISRC.GT.6) NSRC = 4
	print*,'nsrc,isrc',nsrc,isrc
c
C**********  in order to test changing of phase.  this fixes cases where
c		only channel 4 is -180 deg wrt 3,6
	ITEM = 'DATA'
	OK = W_ITEM_I4(CH,ITEM,NDATA,2048,RETURN_SIZE)
c
	ISHIFT = 1			! SHIFT EX
	ISHIFT = 0			! NO SHIFT
	  IF(NSRC.EQ.ISHIFT) THEN
	    write(79,*) '**********'
	    write(79,*) 'SHIFT CHANNEL',TDS_CHANNEL,', SOURCE ', 
     1		PARX(NSRC)
	    write(79,*) '**********'
	    PRINT*,'SHIFT CHANNEL, NSRC=',TDS_CHANNEL, NSRC
c	right for x shifted
	    DO N = 2048,2,-1
	      NDATA(N) = NDATA(N-1)
c	wrong
c	    DO N = 1,2047
c	      NDATA(N) = NDATA(N+1)
	    ENDDO
	  ENDIF
c
C	DONT DO LOWEST FREQ
C
C	FREQMIN = 1.5*SPS/2048.
	FREQMIN = 4.5*SPS/2048.
C	FREQMIN = 7.5*SPS/2048.
	TOPFREQ = .49*SPS
C
	CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
c	write(79,*)'in tdsxyz,read in event no.,source',itemp,isrc
c	write(79,*) 'ndata,1,2,3',ndata(1),ndata(2),ndata(3)
c	write(79,*) 'vdata,1 ',data(1),data(2),data(3)


C
	     item = 'WIND_SPIN_RATE_R4'
	     ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
	     item = 'SLOW_RX_SPEED_R4'
	     ok = w_item_R4(ch, item, SPSS, 1, return_size)
	     item = 'SLOW_RX_SPEED'
	     ok = w_item_i4(ch, item, ISPS, 1, return_size)
	print*,'spin rate, speed',spinrate,isps,spss
C
	     item = 'SUN_ANGLE'
	     ok = w_item_i4(ch, item, SUNCLOCK, 1, return_size)
		PRINT*,'SUNCLOCK',SUNCLOCK
	 END_ANGLE = -360.*(SUNCLOCK-14)/4096.-45.   ! ANGLE SUN TO +EX AT END
	     DANG = SPINRATE*360./SPSS/TWOPI
	     item = 'EVENT_SCET'
	     ok = w_item_i4(ch, item, s_scet, 2, return_size)
		ss = mod(s_scet(2),100)
		mm = s_scet(2)/100
		mm = mod(mm,100)
		hh = s_scet(2)/10000
		scett = float(dds) + hh/24. + mm/1440. + ss/86400.
	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
	   WRITE(PTITLE(16),1016) s(1:4),S(5:6),S(7:8)
	   WRITE(PTITLE(17),1017) DOY
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1017	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
	   TYPE*,'CHANNEL',tds_channel,'   EVENT = ',EVENT
	   WRITE(PTITLE(6),1019) TDS_CHANNEL
 1019	   FORMAT('CHANNEL',I2)
C	   item = 'EVENT_NUMBER'
C	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
	   type*,'event number',itemp
	   WRITE(PTITLE(5),1012) ITEMP
 1012	   FORMAT(I10)
	   item = 'SOURCE'
	   ok = wind_tm_get_item(ch, item, isrc, 1, return_size)
	   IANTSV(IEVENTCOUNT) = ISRC
C
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
C
c
c	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	     call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
c	     call w_ur8_to_ydoy(scetT,yyyy,doy,msday)
	     ihrmn = 100*hh+mm
	     TYPE *,s_scet
	     TYPE *,'scett,doy',scett,doy
	   ihrmn = 100*hh+mm

	
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)




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
C	IF(IEND.EQ.1.AND.MAXDATA.LE.75) GO TO 110
	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.95) GO TO 110
	IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.2.AND.MAXDATA.LE.100) GO TO 110
c
	NPLOTS=NPLOTS+1
C	I WANT TO STORE XYZ IN NSRC = 1,3  WHERE XYZ ARE EITHER E OR B 
C	DEPENDING ON WHETHER IT'S 3B'S OR 3E'S.  IFASTSLOW IS SUPPOSED TO
C	TAKE CARE OF THAT DECISION, BUT IT CAN'T
C
C	IF(IFASTSLOW.EQ.1) NSRC = ISRC-3
C	IF(IFASTSLOW.EQ.2) NSRC = ISRC-6
C	IF(IFASTSLOW.EQ.2.AND.ISRC.LE.6) NSRC = 4
C	IF(IFASTSLOW.EQ.1.AND.ISRC.GT.6) NSRC = 4
	IF(ISRC.LE.6) I3E = I3E + 1
	IF(ISRC.GT.6) I3B = I3B + 1
	IF(I3E.GE.2.AND.IFASTSLOW.EQ.2) STOP 'NOT 3 B s'
	IF(I3B.GE.2.AND.IFASTSLOW.EQ.1) STOP 'NOT 3 E s'
	IANT(NSRC) = ISRC
	print*,'AT IANT, store,ant =',NSRC,isrc,Iant(Nsrc)
	IF(IFULL(NSRC).NE.0) THEN
	  DO NS = 1,3
	    IF(IFULL(NS).EQ.0) THEN
		NSRC = NS
		GO TO 120
	    ENDIF
	  ENDDO
	ENDIF
 120	PRINT*,'ISRC,NSRC=',ISRC,NSRC
	  DO N = 1,2048
	    XDATA(N,NSRC) = DATA(N)/EFFLEN(ISRC)
	    MDATA(N,NSRC) = NDATA(N)
	  ENDDO
	  DO N = 1,1025
	    XSPEC(N,NSRC) = SPECT(N)
	    XSPECT(N,NSRC) = SPECT(N)
	    XPHASE(N,NSRC) = SPHASE(N)
	  ENDDO
	  IFULL(NSRC) = 1
C
C	THIS WILL NOT WORK RIGHT NOW, AS NSRC HAS BEEN CHANGED
C	ISHIFT = 0
c	ISHIFT = 1
C	  IF(NSRC.EQ.ISHIFT) THEN
c	    PRINT*,'SHIFT CHANNEL, NSRC=',TDS_CHANNEL, NSRC
c	print*,'at shift,data=',Xdata(1,NSRC),Xdata(2,NSRC),Xdata(3,NSRC)
C*******
C	    DO N = 2,2048
C	      XDATA(N,NSRC) = XDATA(N-1,NSRC)
C	    ENDDO
C	    DO N = 1,1024
C	      XPHASE(N,NSRC) = XPHASE(N,NSRC) + 180.*(N-1)/1024.
C	    ENDDO
C	  ENDIF
C
c*******
C
c  	  CALL COMBO(-1)
c	  CALL COMBO(7)
	  IF(TDS_CHANNEL.LT.6) GO TO 110
	write(79,*)'tdsxyz read in ',itemp,ptitle(5)
	write(79,*) 'xdata,1',xdata(1,1),xdata(1,2),xdata(1,3)
	write(79,*) 'xdata,2',xdata(2,1),xdata(2,2),xdata(2,3)

C
	nmfi = 0
	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BX, 1, return_size)
	print*,' return size',return_size
	     if(nmfi.eq.0) nmfi = return_size
	     item = 'WIND_MFI_BY(GSE)_R4'
	     ok = w_item_R4(ch, item, BY, 1, return_size)
	     item = 'WIND_MFI_BZ(GSE)_R4'
	     ok = w_item_R4(ch, item, BZ, 1, return_size)
	     item = 'WIND_MFI_BMAG_R4'
	     ok = w_item_R4(ch, item, BMAG, 1, return_size)
	     item = 'WIND_MFI_SCET_R8'
	     NMFI = NMFI+1
	     ok = w_item_R8(ch, item, MFI_SCET(NMFI), 1, return_size)
	     HRMFI = 24.D00*DMOD(MFI_SCET(NMFI),1.D00)
	     MFIHR = HRMFI
	     MFIMIN = 60.*AMOD(HRMFI,1.)
	type*,'hr,min,bx,by,bz',mfihr,mfimin,bx,by,bz
	WRITE(79,*) 'hr,min,bx,by,bz',mfihr,mfimin,bx,by,bz
C	     MFI_SCET(NMFI) = DFLOAT(DDS) + NHR/24.D00
C     1	         + MIN/1440.D00 + SEC/86400.D00
C	     BXS(NMFI) = BX
C	     BYS(NMFI) = BY
C	     BZS(NMFI) = BZ
	     BMAG = SQRT(BX**2 + BY**2 + BZ**2)
C**********INTERPOLATE FOR BC	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BXS, 3, return_size)
	     item = 'WIND_MFI_BY(GSE)_R4'
	     ok = w_item_R4(ch, item, BYS, 3, return_size)
	     item = 'WIND_MFI_BZ(GSE)_R4'
	     ok = w_item_R4(ch, item, BZS, 3, return_size)
	     item = 'WIND_MFI_SCET_R8'
	     ok = w_item_R8(ch, item, BTIMS, 3, return_size)
C    	     PRINT*,'BXS',BXS(1),BXS(2),BXS(3)
C	     PRINT*,'TIM',BTIMS(1),BTIMS(2),BTIMS(3)
C		BX = BXS(1) + 
C     1	(BXS(2)-BXS(1))*(SCETT-BTIMS(1))/(BTIMS(2)-BTIMS(1))
C		BY = BYS(1) + 
C     1	(BYS(2)-BYS(1))*(SCETT-BTIMS(1))/(BTIMS(2)-BTIMS(1))
C		BZ = BZS(1) + 
C     1	(BZS(2)-BZS(1))*(SCETT-BTIMS(1))/(BTIMS(2)-BTIMS(1))
C    	     PRINT*,'B',BX,BY,BZ
C	     	BX = 150.
C		BY = 250.
		BZ = 300.
C		STOP
C***********  END OF INTERPOLATE
	     BTS(NMFI) = BMAG
		  IF(MFI_SCET(NMFI).GT.SCETT.AND.MFI_SCET(NMFI-1).
     1			LE.SCETT) WRITE(79,*) BX,BY,BZ,BMAG
	     IF(NMFI.lt.1) THEN
		WRITE(79,*) 'NO WIND/LIB MFI DATA'
	        IF(NMFI.NE.0) THEN
		  print*,'last mfi time',mfi_scet(return_size)
	        ELSE
		  PRINT*,'NO WIND/LIB MFI DATA'
		  NMFI = 0
 		  WRITE(MFI_FILE,765),S_SCET(1)
 765		  FORMAT('[.STUFF]',I8,'.MFI')
		  PRINT*,'OPEN MFI FILE:  ',MFI_FILE
c		  OPEN(UNIT=99,FILE='[.STUFF]19961116.MFI',
c     1			TYPE='OLD',READONLY)
		  OPEN(UNIT=99,FILE=MFI_FILE,TYPE='OLD',READONLY)
 		  READ(99,1097) JUNK
		  READ(99,1097) JUNK
 1097		  FORMAT(A120)
 1099		  FORMAT(A10,I3,1X,I2,1X,F6.3,4F14.4)
 1098		  READ(99,1099,END=1199) JUNK,NHR,MIN,SEC,BMAG,BX,BY,BZ
		  NMFI = NMFI+1
		  MFI_SCET(NMFI) = DFLOAT(DDS) + NHR/24.D00
     1		   + MIN/1440.D00 + SEC/86400.D00
		  BXS(NMFI) = BX
		  BYS(NMFI) = BY
		  BZS(NMFI) = BZ
		  BTS(NMFI) = BMAG
		  IF(MFI_SCET(NMFI).GT.SCETT.AND.MFI_SCET(NMFI-1).
     1			LE.SCETT) WRITE(79,*) BX,BY,BZ,BMAG
		  GO TO 1098
 1199		  CLOSE(UNIT=99)
		  print*,'last mfi and time',NMFI,mfi_scet(NMFI)
		  print*,'values',BTS(NMFI),BXS(NMFI),BYS(NMFI),BZS(NMFI)
	        ENDIF
	     ENDIF
C
	     item = 'WIND_ORBIT_X(GSE)_R8'
	     ok = w_item_R8(ch, item, XKM, 1, return_size)
	     item = 'WIND_ORBIT_Y(GSE)_R8'
	     ok = w_item_R8(ch, item, YKM, 1, return_size)
	     item = 'WIND_ORBIT_Z(GSE)_R8'
	     ok = w_item_R8(ch, item, ZKM, 1, return_size)
C
	WRITE(79,*) 'EVENT NO.',ITEMP
	WRITE(79,*) 'DATE,TIME',S_SCET
	item = 'R_EARTH_R4'
	ok = w_item_R4(ch, item, RE, 1, return_size)
	RE = .001*RE				! CHANGE TO KM
	XRE= XKM/RE
	YRE= YKM/RE
	ZRE= ZKM/RE
	write(79,*) 'ORBIT X,Y,Z',XRE,YRE,ZRE
C
	  SPSUSE = .001*SPS
	  DO N = 1,1025
	    PP(N) = N*SPSUSE/2048.
C            DPHASE(N) = AMOD(XPHASE(N)-YPHASE(N)+540.,360.) - 180.
	  ENDDO
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
		IF(S1-S2.NE.0.) THEN
	  	   ZCROSS(IZCNT) = IL + S1/(S1 - S2)
	  	ELSE
		   ZCROSS(IZCNT) = IL 
		ENDIF	
	  ENDIF
	ENDDO
	DO N = 1,IZCNT-1
	  ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
	  IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
	  IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
	ENDDO
	print*,'zero crossings found',izcnt
	print*,'first 5',(zcross(kk),kk=1,5)
	AVRFREQ=0.
	if(izcnt.ne.0)print*,'izcnt,zcross',izcnt,zcross(izcnt),zcross(1)
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
C
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	NSYS = 0
	IF(LEVEL.GE.2) THEN
C  	  CALL COMBO(3)
C  	  CALL COMBO(-1)
C
C	XFGSE TRANSFORMS E FROM S/C ANTENNA SYSTEM TO GSE SYSTEM
C
	print*,'before xfgse,xdata=',xdata(1,1),xdata(2,1),xdata(3,1)
	  CALL XFGSE			! TRANSFORM E FROM S/C (ANT) TO GSE
	print*,'after xfgse,nsys=',NSYS
	print*,'after xfgse,xdata=',xgse(1,1),xgse(2,1),xgse(3,1)
	write(79,*)'after xfgse,level=',level
C	write(79,*) 'xdata,1',xdata(1,1),xdata(1,2),xdata(1,3)
C	write(79,*) 'xdata,2',xdata(2,1),xdata(2,2),xdata(2,3)
C	write(79,*) 'xgse,1 ',xgse(1,1),xgse(1,2),xgse(1,3)
C	write(79,*) 'xgse,2 ',xgse(2,1),xgse(2,2),xgse(2,3)

C
C	  plotpart plots short sections of the data from XFDATA
C		CHANGED
C		IN CASE XFORMVM IS NOT CALLED YET:
ct	print*,'before plotp,xfdata=',xfdata(1,1),xfdata(2,1),xfdata(3,1)
	print*,'xformvm for plotpart commented out'
ct	  DO N = 1,2050
ct	    DO J = 1,4
ct	      XFDATA(N,J) = XGSE(N,J)
ct	    ENDDO
ct	  ENDDO
C
C	  CALL XFORMSN			! TRANSFORM E TO SHOCK NORM,CURRENT
C
C
C	NS2 = 1024
C	NS2 = 2048
	NS1 = 1
	NS2 = 2048
	  CALL XFORMVM(NS1,NS2,XGSE)		! TRANSFORM E TO VARIANCE EIGEN SYS.
	write(79,*)'after xformvm,itemp',itemp,ptitle(5)
	write(79,*) 'xgse,1 ',xgse(1,1),xgse(1,2),xgse(1,3)
	write(79,*) 'xgse,2 ',xgse(2,1),xgse(2,2),xgse(2,3)
	write(79,*) 'xfdata,1',xfdata(1,1),xfdata(1,2),xfdata(1,3)
	write(79,*) 'xfdata,2',xfdata(2,1),xfdata(2,2),xfdata(2,3)
C
C		CALC DOT PRODUCTS WITH B
C
	  DO IV = 1,3
	    EDOTB =  (EVECT(1,IV)*BX + EVECT(2,IV)*BY
     1		+ EVECT(3,IV)*BZ)/BMAG
	    WRITE(79,*) 'B DOT WITH EIGENV',IV, EDOTB
	    ANGEVTOB(IV) = ACOSD(EDOTB)
	  ENDDO
C
c***
C	  CALL SUMMPLOT(7,CH,1,2048)
	  CALL SUMMPLOT(-2,CH,1,2048)
C	  IPLOTCH IS THE INDEX IN XDATA, NOT A CHANNEL NUMBER
	  IPLOTCH = 4
	  CALL WHISTLER(-1,CH,IPLOTCH)
C	  CALL WHISTLER(-3,CH,IPLOTCH,IZCNT)
c***
C 	  CALL PLOTPART(-1)
C 	  CALL PLOTPART(3)
C 	  CALL PLOTSEL(-1,1,2048)
C 	  CALL PLOTSEL(-1,900,1150) 
C 	  CALL PLOTSEL(-1,NS1,NS2) 
C
c	  IF(IPROCESS.LE.1) CALL DETAIL(-2,900,1100)
	ENDIF
	print*,'at end,nplots,nrem=',nplots,nrem
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
	REAL ANG(2048)
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	common /headblk/ major,minor,s_scet,nsys
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025),END_ANGLE,DANG,TPAUSE
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /EXTRA/ NBXBY,MDATA(2048,4),DBSPEC(1025,4),AVRB,STDB
	COMMON /FRLIMITS/ FREQMIN,TOPFREQ
	COMMON /ZFREQ/ ZCFREQ(1024,4)
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
	REAL AVRB(1025,4),STDB(1025,4)
	DIMENSION YY(2048),YYT(2048),PP(2048)
C
	write(79,*),'combo called'
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
C	PUT LABELS ON RIGHT HAND SIDE
C
	IF(ITERM.LT.0) THEN
	  XSTART = 350.
	  XEND = 2000.
	  BOTTOM = 400.
	  TOP = 3100.
	  CALL MGOSETLOC(XSTART,BOTTOM,XEND,TOP)
	ELSE
	  XSTART = 100.
	  XEND = 870.
	  BOTTOM = 50.
	  TOP = 750.
	  CALL MGOSETLOC(XSTART,BOTTOM,XEND,TOP)
	ENDIF
	AVRFREQ=0.
	if(izcnt.ne.0)print*,'izcnt,zcross',izcnt,zcross(izcnt),zcross(1)
	IF(IZCNT.GT.1) THEN
	  AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  AVRFREQ = .001*SPS/AVRPER
	ENDIF
	TITLE(24) = ' AVR.FREQ.'
	WRITE(TITLE(25),1020) AVRFREQ
 1020	FORMAT(F8.2,' kHZ')
C
	  TITLE(21) = 'FFT BANDPASS'
	  WRITE(TITLE(22),1019) FREQMIN,TOPFREQ
 1019	  FORMAT(F4.1,'-',F6.1)
C	XTITLE = GX2 +.006*(GX2-GX1)
	XTITLE = GX2 +.02*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
	DO N = 1,22
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
	ELSE
	  YBOT = BOTTOM + .669*(TOP-BOTTOM)
	  CALL MGOSETLOC(XSTART,YBOT,XEND,TOP)
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
	ST_ANGLE = END_ANGLE + 2048.*DANG	  ! ANGLE SUN TO +EX AT START
C
	  DO N = 1,2048
  	    PP(N) = 1000.*(N-1)/SPS
	    YY(N) = NDATA(N)
	    ANG(N) = ST_ANGLE - (N-1)*DANG
	  ENDDO
C
	CALL MGOSETLIM(ANG(1),-128.,ANG(2048),128.)
	CALL MGOSETEXPAND(.8)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOCONNECT(ANG,YY,2048)
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(17,'ANGLE, SUN TO +EX')
	CALL MGOYLABEL(14,'T/M NUMBER-128')
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TDSXYZ,COMBO')
	CALL MGOSETEXPAND(1.)
C
C	PLOT TDS DATA IN PHYSICAL UNITS
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,1605.,XEND,2275.)
	ELSE
	  YBOT = BOTTOM + .446*(TOP-BOTTOM)
	  YTOP = BOTTOM + .694*(TOP-BOTTOM)
	  CALL MGOSETLOC(XSTART,YBOT,XEND,YTOP)
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
	ELSE
	  YBOT = BOTTOM + .267*(TOP-BOTTOM)
	  YTOP = BOTTOM + .391*(TOP-BOTTOM)
	  CALL MGOSETLOC(XSTART,YBOT,XEND,YTOP)
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
	  ENDDO
	  YYT(1) = YY(1)
	  YYT(IZCNT-1) = YY(IZCNT-1)
	ENDIF
C
  	YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
	PRINT*,'YMIN,YMAX',YMIN,YMAX
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
	ELSE
	  YTOP = BOTTOM + .174*(TOP-BOTTOM)
C	  YBOT = BOTTOM - .037*(TOP-BOTTOM)
	  YBOT = BOTTOM 
	  CALL MGOSETLOC(XSTART,YBOT,XEND,YTOP)
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
C
C	PLOT BACKGROUND FOR B AND EX IF SAMPLE RATE = 1875.
C
	IF(ISPS.EQ.1) THEN
	  LD = IRX - 6
	  IF(IRX.EQ.4) LD = 4
	  IF(LD.GT.0.AND.LD.LE.4) THEN
	    DO N = N1,N2,2
	      NP = (N-1)/2
	      YY(NP) = AVRB(NP,LD)
	    ENDDO
	    CALL MGOCONNECT(PP,YY,NP)
	  ENDIF
	ENDIF
C
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
	  PRINT*,'FIRST PLOT'
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
C
 	  START = SECNDS(0.)
	  DOWHILE(SEC.LT.TPAUSE)
	    SEC = SECNDS(START)
	  ENDDO
C	  READ(5,*) JUNK
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END	
	SUBROUTINE PLOTPART(ITERM)
C
C	PLOTS ONE COMPONENT AGAINST ANOTHER FOR 15 SECTIONS OF AN
C		EVENT TO LOOK FOR POLARIZATION CHANGES
C	I MODIFIED THIS FROM TDSF PROGRAMS, TO PLOT HODOGRAMS IN THE
C		PLANE OF THE EIGENVECTORS OF THE TWO LARGEST EIGENVALUES
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
     1   NDATA(2048),DATA(2050),SPECT(1025),END_ANGLE,DANG,TPAUSE
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
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
	write(79,*),'plotpart called'
c	DO NN = 1,3
c	  N1 = NN
c	  N2 = N1+1
c	  IF(NN.EQ.3) THEN
c		N1 = 1
c		N2 = 3
c	  ENDIF
C
	N1 = 1
	N2 = 2
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,450.,2000.,3050.)
	  ELSE
	    CALL MGOSETLOC(300.,80.,800.,750.)
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
C	  PLOT TDS HODOGRAMS IN PLANE OF TWO LARGEST EIGENVECTORS 
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
	  NXW = 3
	  NYW = 5
	  NW = NXW*NYW
	  DO IW = 1,NW
	    CALL MGOWINDOW(NXW,NYW,IW)
	    NPTST = (IW-1)*2048/NW + 1
	    NPTND = (IW)*2048/NW
	    CALL XFORMVM(NPTST,NPTND,XDATA)
	    NPT = 0	
	    XMAX = 0.
	    YMAX = 0.
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
C  	      PP(NPT) = 1000.*(N-1)/SPS
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
	    CALL MGOXLABEL(6,'EV MAX')
	    CALL MGOYLABEL(6,'EV MID')
	    CALL MGORELOCATE(-.8*XMAX,-.8*YMAX)
	    CALL MGOSETEXPAND(.4)
	    WRITE(STR,1001) EVAL(3)/EVAL(1)
	    CALL MGOLABEL(23,STR)
 1001	    FORMAT('3RD EV RATIO',E11.2)
	    WRITE(STR,1007) NPTST,NPTND
 1007	    FORMAT('SAMPLES ',I4,' TO',I5)		
	    CALL MGORELOCATE(-.8*XMAX, .8*YMAX)
	    CALL MGOLABEL(20,STR)
	    CALL MGOSETEXPAND(1.)
C	    PUT ON ARROW
	    NPTMAX = NPT-1
c	    SIZE = .00025*SQRT(XMAX**2 + YMAX**2)
	    SIZE = .0025*SQRT(XMAX**2 + YMAX**2)
	    XARR = .5*(XX(NPTMAX+1) + XX(NPTMAX))
	    YARR = .5*(YY(NPTMAX+1) + YY(NPTMAX))
	print*,'plot,ntp',nptmax,xx(nptmax),xx(nptmax+1),xmax,ymax
	    DX = XX(NPTMAX+1) - XX(NPTMAX)
	    DY = YY(NPTMAX+1) - YY(NPTMAX)
	    CALL ARROW(XARR,YARR,DX,DY,SIZE)
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
	    PRINT*,'SECOND PLOT'
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    READ(5,*) JUNK
	    CALL MGOTCLOSE
	  ENDIF
	  CALL MGOSETEXPAND(.6)
	print*,'nsys',nsys
 	  IF(NSYS.EQ.0)CALL MGOPLOTID('S/C ANT','[.WIND]TDSXYZ,PLOTPART')
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,PLOTPART')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('MIN VAR','[.WIND]TDSXYZ,PLOTPART')
 	  IF(NSYS.EQ.3)CALL MGOPLOTID('GSE','[.WIND]TDSXYZ,PLOTPART')
 	  IF(NSYS.GT.3)CALL MGOPLOTID(' ??? ','[.WIND]TDSXYZ,PLOTPART')
	  CALL MGOSETEXPAND(.8)
C	ENDDO
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
     1   NDATA(2048),DATA(2050),SPECT(1025),END_ANGLE,DANG,TPAUSE
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
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
	write(79,*),'plotsel called'
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	
C*************
C	DO N = NPTST,NPTND
C	  WRITE(67,*) N,XFDATA(N,1),XFDATA(N,2),XFDATA(N,3)
C	ENDDO
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(600.,450.,1900.,3050.)
	  ELSE
	    CALL MGOSETLOC(339.,50.,661.,750.)
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
	      XX(NPT) = XFDATA(N,N1)
	      YY(NPT) = XFDATA(N,N2)
	    ENDDO
	    CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(2,LABEL1(N1))
	    CALL MGOYLABEL(2,LABEL1(N2))
	    CALL MGOSETEXPAND(.8)
	    CALL MGOSETEXPAND(1.)
C	    PUT ON ARROW
	    NPTMAX = NMAX - NPTST + 1
	    SIZE = .0025*SQRT(XMAX**2 + YMAX**2)
	    IF(ITERM.GT.0) SIZE = 2.*SIZE
	    XARR = .5*(XX(NPTMAX+1) + XX(NPTMAX))
	    YARR = .5*(YY(NPTMAX+1) + YY(NPTMAX))
	    DX = XX(NPTMAX+1) - XX(NPTMAX)
	    DY = YY(NPTMAX+1) - YY(NPTMAX)
c	if(n2.eq.2) then
	  print*,'arrow,size=',size
	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
	  print*,nptmax,xx(nptmax),yy(nptmax)
	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
c	endif
	    CALL ARROW(XARR,YARR,DX,DY,SIZE)
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
	print*,'in plotsel,nsys=',nsys
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,PLOTSEL')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('VAR MX','[.WIND]TDSXYZ,PLOTSEL')
 	  IF(NSYS.EQ.3)CALL MGOPLOTID('GSE','[.WIND]TDSXYZ,PLOTSEL')
	  CALL MGOSETEXPAND(.8)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,'THIRD PLOT'
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
     1   NDATA(2048),DATA(2050),SPECT(1025),END_ANGLE,DANG,TPAUSE
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
	write(79,*),'spplot called'
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
	  PRINT*,'FOURTH PLOT'
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
	common /nrblk/ nrem,NHRMN,IFASTSLOW,ANGEVTOB(3)
	integer*4	iq,NREM,NHRMN,IFASTSLOW
	real		AngEVtoB
10	  write(6,6)
	  write(6,7)
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
  7	format(1x,'if offline interactive, type wi_lz_wav_yyyymmdd_v*.dat',$)
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
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	common /headblk/ major,minor,s_scet,nsys
	DATA AMOD,BMOD,CMOD /  25.6, 25.6, 14.6/
	DATA BEFORE /-2.366,.720,2.828/			! 19961116 1936:00
	DATA BAFTER /-1.462,1.360,9.976/		! 19961116 1939:04
C
C	CALCULATE SN = OUTWARD SHOCK NORMAL UNIT VECTOR, CL = CURRENT
C	LAYER UNIT VECTOR, V3 = THIRD DIRECTION
C
	NSYS = 1
	WRITE(79,*) ' '
	WRITE(79,*) 'XFORMSN CALLED--E DATA PUT INTO SN SYSTEM, NSYS=1'

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
	  ANGLE =  -360.*(SUNCLOCK-14)/4096. - 45.       ! ANGLE SUN TO +EX
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
	SUBROUTINE XFORMVM(NS1,NS2,XINP)
C
C	THIS TRANSFORMS THE ELECTRIC FIELD, XINP,INTO THE VARIANCE MATRIX
C	EIGENSYSTEM, 1 = LARGEST EIGENVALUE.  XFDATA ARE THE TRANS-
C	FORMED FIELDS
C
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	common /headblk/ major,minor,s_scet,nsys
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	INTEGER*4 SUNCLOCK
	REAL XINP(2050,4), XTEMP(2050,4)
C
C	EVECT(I,J) IS THE Ith COMPONENT OF THE Jth EIGENVECTOR
C
C	VMATRIX CALCULATES THE VARIANCE MATRIX OF XGSE, AND RETURNS
C	EIGENVALUES IN COMMON /VARMATX/EVAL, AND EIGENVECTORS IN EVECT
C
C	MAKE A COPY OF THE INPUT MATRIX TO WORK ON
C
	DO J = 1,4
 	  DO N = 1,2050
		XTEMP(N,J) = XINP(N,J)
	  ENDDO
	ENDDO	
C
	CALL VMATRIX(XTEMP,NS1,NS2)
	WRITE(79,*) ' '
	WRITE(79,*) 'XFORMVM CALLED--PUT E DATA INTO VAR MX SYSTEM, NSYS=2'
C
C	NOW EVECT ARE EIGENVECTORS IN THE XINP SYSTEM
C	XFDATA(N,1) CORRESPONDS TO LARGEST EIGENVALUE,N=COMPONENT
C
	DO N = 1,2048
	  XFDATA(N,1) = XTEMP(N,1)*EVECT(1,1) + XTEMP(N,2)*EVECT(2,1) + 
     1		XTEMP(N,3)*EVECT(3,1)
	  XFDATA(N,2) = XTEMP(N,1)*EVECT(1,2) + XTEMP(N,2)*EVECT(2,2) + 
     1		XTEMP(N,3)*EVECT(3,2)
	  XFDATA(N,3) = XTEMP(N,1)*EVECT(1,3) + XTEMP(N,2)*EVECT(2,3) + 
     1		XTEMP(N,3)*EVECT(3,3)
	ENDDO
C
C	CHECK TRANSFORMATION.  IN XTEMP, COMPONENT IS 2ND INDEX, IN
C	EVECT THE FIRST INDEX IS COMPONENT.  T1,T2,T3 ARE THE
C	COMPONENTS IN THE EIGENVECTOR SYSTEM, N IS EIGENVECTOR NO. 
C
C	DO N = 1,3
C	  T1 = EVECT(1,N)*EVECT(1,1) + EVECT(2,N)*EVECT(2,1) + 
C     1		EVECT(3,N)*EVECT(3,1)
C	  T2 = EVECT(1,N)*EVECT(1,2) + EVECT(2,N)*EVECT(2,2) + 
C     1		EVECT(3,N)*EVECT(3,2)
C	  T3 = EVECT(1,N)*EVECT(1,3) + EVECT(2,N)*EVECT(2,3) + 
C     1		EVECT(3,N)*EVECT(3,3)
C	  PRINT*,'CHECK',N,T1,T2,T3
C	ENDDO
C
C	WRITE(79,*) 'IN SYSTEM,',NSYS,' EIGENVALUES:'
C	WRITE(79,*)  EVAL
C	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
C	WRITE(79,*)(EVECT(1,I),I=1,3)
C	WRITE(79,*)(EVECT(2,I),I=1,3)
C	WRITE(79,*)(EVECT(3,I),I=1,3)
C
	NSYS = 2
C
C	IT SEEMS TO ME THAT THE FOLLOWING IS ONLY FOR CH 1 AND 2
C
c	DO N1 = 1,3
c	  N2 = N1 + 1
c	  spr = 0.
c	  xsq = 0.
c	  ysq = 0.
c	  DO N = 1,2048
c	    xsq = xsq + xFdata(n,N1)**2
c	    ysq = ysq + xFdata(n,n2)**2
c	    spr = spr + xFdata(n,n1)*xFdata(n,n2)
C	    IF(LEVEL.GE.1) THEN
C		WRITE(67,*) N,(XDATA(N,NN),NN=1,4)
C	    ENDIF
c	  ENDDO

C	  slope is y/x
C	  CHANGED TO EY/EX ON 15 NOV 1996
C
c	  CALL FITSLOPE(XFDATA(1,N1),XFDATA(1,N2),2048,SLOPE,SUMSQ)
c	  IF(XSQ*YSQ.NE.0.) print*,'corr',N1,N2,spr/sqrt(xsq*ysq)
C	  ANGLE = ATAND(SLOPE) + 360.*SUNCLOCK/4096. - 45.
C	  ANGLE = -AMOD(ANGLE,180.)

c	  IF(XSQ*YSQ.NE.0.) write(79,*)' '
c	  IF(XSQ*YSQ.NE.0.) write(79,*)'IN VAR MATRIX SYSTEM '
c	  IF(XSQ*YSQ.NE.0.) write(79,*)'corr',N1,N2,spr/sqrt(xsq*ysq)
c	  IF(XSQ.NE.0.) print*,'E FIELD slope, SQERR',SLOPE, SUMSQ
C	  IF(XSQ.NE.0.) WRITE(79,*) ' SLOPE(EY/EX),SUNCLOCK',
C     1		SLOPE,SUNCLOCK
C	  IF(XSQ.NE.0..AND.N1.EQ.1) WRITE(79,*) 'ANGLE SUN TO E,GSE',ANGLE
C	WRITE(79,*) 'xlen,ylen',xlen,ylen
c	ENDDO
c
	RETURN
	END
	SUBROUTINE VMATRIX(DATA,N1,N2)
C
C	CALCULATES VARIANCE MATRIX AND EIGENVALUES
C
C	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
C     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	common /headblk/ major,minor,s_scet,nsys
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	REAL DATA(2050,4),DATAS(2050,3),VMAT(3,3)
	REAL SN(3),CL(3),V3(3),AV(3),STD(3)
	integer*4	s_scet(2)
	INTEGER*4 SUNCLOCK
C
C	subtract averages
C
	WRITE(79,*) ' '	
	WRITE(79,*) 'VARIANCE MATRIX CALLED FOR SAMPLES',N1,' TO',N2	
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
c	M1 = 1
c	M2 = M1+1
c	XSQ = 0.
c	YSQ = 0.
c	SPR = 0.
c	  DO N = 1,2048
c	    xsq = xsq + datas(n,M1)**2
c	    ysq = ysq + datas(n,M2)**2
c	    spr = spr + datas(n,M1)*datas(n,M2)
c	  ENDDO
c	PRINT*,'E1 RMS',SQRT(XSQ/2048.),'  E2 RMS',SQRT(YSQ/2048.),
c     1 '  CORR',spr/sqrt(xsq*ysq)
c	WRITE(79,*)'E1 RMS',SQRT(XSQ/2048.),'  E2 RMS',SQRT(YSQ/2048.),
c     1	 '  CORR', spr/sqrt(xsq*ysq)
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
	    DO M = 1,3
		VMAT(K,M) = VMAT(K,M) + DATAS(N,K)*DATAS(N,M)
	    ENDDO
	  ENDDO
	ENDDO	
C
C	CONVERT TO MEAN SQUARE SIGNAL
C
	DO K = 1,3
	  DO M = 1,3
		VMAT(K,M) = VMAT(K,M)/(N2-N1+1)
	  ENDDO
	ENDDO
	print*,'diag elems',vmat(1,1),vmat(2,2),vmat(3,3),
     1	' trace',vmat(1,1)+vmat(2,2)+vmat(3,3)
C
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
C	SORT IN DESCENDING ORDER OF EIGENVALUES
C
	CALL EIGSRT(EVAL,EVECT,3,3)
C
C
C 	ENFORCE A RIGHT HANDED SYSTEM BY REVERSING THE SMALLEST
C	EIGENVECTOR IF NECESSARY
C
	ADOTBXC = EVECT(1,1)*EVECT(2,2)*EVECT(3,3)
     1	 - EVECT(1,1)*EVECT(3,2)*EVECT(2,3)
     1	 + EVECT(2,1)*EVECT(1,3)*EVECT(3,2)
     1	 - EVECT(2,1)*EVECT(1,2)*EVECT(3,3)
     1	 + EVECT(3,1)*EVECT(1,2)*EVECT(2,3)
     1	 - EVECT(3,1)*EVECT(2,2)*EVECT(1,3)
	PRINT*,'DETERMINANT OF EIGENVECTORS',ADOTBXC
	IF(ADOTBXC.LT.0.) THEN
	  EVECT(1,3) = -EVECT(1,3)
	  EVECT(2,3) = -EVECT(2,3)
	  EVECT(3,3) = -EVECT(3,3)
	ENDIF
C
	PRINT*,'IN SYSTEM',NSYS,'  EIGENVALUES:'
	PRINT*,EVAL
	PRINT*,'EIGENVECTORS'
	PRINT*,(EVECT(1,I),I=1,3)
	PRINT*,(EVECT(2,I),I=1,3)
	PRINT*,(EVECT(3,I),I=1,3)
	WRITE(79,*) 'IN SYSTEM',NSYS,'  EIGENVALUES:'
	WRITE(79,*)  EVAL,'  sum',eval(1)+eval(2)+eval(3)
	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
	WRITE(79,*)(EVECT(1,I),I=1,3)
	WRITE(79,*)(EVECT(2,I),I=1,3)
	WRITE(79,*)(EVECT(3,I),I=1,3)
	NSYS = 2
	RETURN
	END	
	SUBROUTINE XFGSE
C
C	THIS TRANSFORMS THE ELECTRIC FIELD INTO THE GSE SYSTEM
C	THE INPUT IS THE ELECTRIC FIELD IN THE SPACECRAFT ANTENNA
C	SYSTEM IN XDATA, THE OUTPUT, IN XGSE, IS THE ELECTRIC FIELD IN 
C	THE GSE SYSTEM.  NOTE THAT THE S/C IS UPSIDE DOWN, SO THAT IF
C	+EX POINTS AT THE SUN (ANG=0) THEN +EY POINTS IN THE - Y (GSE)
C	DIRECTION
C
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	INTEGER*4 SUNCLOCK
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	common /headblk/ major,minor,s_scet,nsys
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	DATA TWOPI /6.2831853/
C
C	  THE FIELDS ARE IN THE ANTENNA SYSTEM
C	  TRANSFORM THEM TO THE GSE SYSTEM, EX, EY, EZ, BY ROTATING
C	  X AND Y, and INVERTING  Z gse is parallel to Z ant already,
C	   but opposite
C
	NSYS = 3
	WRITE(79,*) ' '
	WRITE(79,*) 'XFGSE CALLED--E DATA PUT INTO GSE SYSTEM, NSYS=3'
C
	ANGLE =  -360.*(SUNCLOCK-14)/4096. - 45.       ! ANGLE SUN TO +EX AT END
	DANG = SPINRATE*360./SPSS/TWOPI
c	ANGLE = ANGLE + 2048.*DANG		  ! ANGLE SUN TO +EX AT START
	ANGLE = ANGLE + 3072.*DANG		  ! ANGLE SUN TO +EX AT START
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
	NSYS = 3
C
	RETURN
	END
	SUBROUTINE ARROWOLD(X,Y,DX,DY,SIZE)
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
	DXN = SQRT(DX**2 + DY**2)
	DXL = DX*SIZE/DXN
	DYL = DY*SIZE/DXN
	DXP = .3*DYL
	DYP = -.3*DXL
	CALL MGORELOCATE(X+2.*DXL,Y+2.*DYL)
	CALL MGODRAW(X+DXP,Y+DYP)
	CALL MGORELOCATE(X+2.*DXL,Y+2.*DYL)
	CALL MGODRAW(X-DXP,Y-DYP)
C
	RETURN
	END
	SUBROUTINE PARTSPEC(CH,NSTT,NVAR,NDATA,NSPECT,DATA,PDATA,SPECT)
C
C	THIS ROUTINE DOES FOURIER ANALYSIS ON PART OF AN EVENT.  
C	NSTART IS THE SAMPLE NUMBER OF THE FIRST SAMPLE, NVAR IS THE
C	NUMBER OF SAMPLES TO BE FOURIER ANALYSED.  NVAR MUST BE A POWER
C	OF TWO.  NDATA IS THE RAW DATA FROM THE EVENT, NSPECT IS THE
C	NUMBER OF DIFFERENT SPECTRA TO BE DONE
C
C	THIS PARTSPEC HAS BEEN MODIFIED FROM THE ORIGINAL TO WORK WITH
C		SHORTER SAMPLES, AND WITH PROCESSED DATA.
C	NO, I JUST COPIED REALFT TO WHISTLER
C
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
	INTEGER*4 NUMBER_EVENT,TDS_CHANNEL
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
	DATA NOPLOT /1/			 ! NOPLOT = 1 IS NOT TO PLOT
C
	WRITE(79,*),'PARTSPEC CALLED'
	PRINT*,'IN PARTSPEC,IRX,ISPS,SPS=',IRX,ISPS,SPS
	  NHALF = NVAR/2
	  HALF = NHALF
	  DO N = NSTT, NSTT+NVAR-1
	    NP = NP+1
	    DATA(NP) = TDSCAL(TDS_CHANNEL,ISPS,NDATA(NP)+128)
	    PDATA(NP) = DATA(NP)
C	    NDATA(IK) = NDATA(IK)-128
  	  ENDDO
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
	     DO IK = 3,NVAR,2
		FCOEF = CMPLX(PDATA(IK),PDATA(IK+1))
	        FREQ = SPS*(IK-1)/4096.
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
	IF(NOPLOT.EQ.1) RETURN
C
C	  PLOT FOURIER SPECTRUM
C
	ITERM = -3
C	ITERM = 3
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
	NHALF = NVAR/2
	HALF = NHALF
	DO NS = 1,NSPECT
	  NSTT = NSTART + (NS-1)*NVAR
	  NP = 0
	print*,'in partspec',tds_channel
	print*,'in partspec',isps
	print*,'in partspec',ndata(5)+128,ndata(6)+128,ndata(7)+128
	print*,'call mgowindow, 1,',nspect,ns
C	
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
	  PRINT*,'PARTSPEC PLOT'
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	  STOP
	ENDIF
C
	RETURN
	END
	SUBROUTINE PROCXYZDATA(FILE,EVENT_NO)
C
C	READS IN A BUNCH OF CALS AND AVERAGES GAIN AND PHASE
C
	CHARACTER*80 FILE
	CHARACTER*50 CH(20)
	CHARACTER*10 STR
	INTEGER*4 EVENT_NO
	COMMON /PROCBLK/ PH(1025,4),PHDIFF(1025,4),SPEC(1025,4)
	DIMENSION COUNT(1025)
	DIMENSION PHASE(1025),WT(1025),SPDIFF(1025,4)
	DIMENSION XDATA(1025,8)
	DIMENSION OUTDATA(1025,10)
	REAL SIGMAX(8),PHIAVR(8)
	REAL AVR(105,8),AVRC(105,8),RMS(105,8),WTA(1025,8),FREQ(105)
	REAL AVRCINV(105,8)
	DATA COUNT /1025*1.E-8/
C
	FLOW = 7.5E3/2048.
C
C	AS THERE IS AMBIGUITY WHEN THE PHASE DIFF IS +- 180,
C	CALCULATE AVERAGE PHASES
C
	DO J = 1,3
	  PHIAVR(J) = 0.
	  COUNT(J) = 1.E-8
	  DO NN = 2,1024
	    PHIAVR(J) = PHIAVR(J) + PH(NN,J)
	    COUNT(J) = COUNT(J) + 1.
	  ENDDO
	  PHIAVR(J) = PHIAVR(J)/COUNT(J)
	ENDDO
	WRITE(79,*),'PHIAVR',(PHIAVR(J),J=1,3)
C
 200	CONTINUE
	DO NN = 1,1025
	  PHDIFF(NN,1) = PH(NN,1) - PH(NN,2)
C	  IF(PHDIFF(NN,1).GT.180.)  PHDIFF(NN,1) = PHDIFF(NN,1)-360.
C	  IF(PHDIFF(NN,1).LE.-180.) PHDIFF(NN,1) = PHDIFF(NN,1)+360.
	  IF(PHDIFF(NN,1).GT.240.)  PHDIFF(NN,1) = PHDIFF(NN,1)-360.
	  IF(PHDIFF(NN,1).LE.-240.) PHDIFF(NN,1) = PHDIFF(NN,1)+360.
	  PHDIFF(NN,2) = PH(NN,2) - PH(NN,3)
C	  IF(PHDIFF(NN,2).GT.180.)  PHDIFF(NN,2) = PHDIFF(NN,2)-360.
C	  IF(PHDIFF(NN,2).LE.-180.) PHDIFF(NN,2) = PHDIFF(NN,2)+360.
	  IF(PHDIFF(NN,2).GT.240.)  PHDIFF(NN,2) = PHDIFF(NN,2)-360.
	  IF(PHDIFF(NN,2).LE.-240.) PHDIFF(NN,2) = PHDIFF(NN,2)+360.
	  PHDIFF(NN,3) = PH(NN,3) - PH(NN,1)
C	  IF(PHDIFF(NN,3).GT.180.)  PHDIFF(NN,3) = PHDIFF(NN,3)-360.
C	  IF(PHDIFF(NN,3).LE.-180.) PHDIFF(NN,3) = PHDIFF(NN,3)+360.
	  IF(PHDIFF(NN,3).GT.240.)  PHDIFF(NN,3) = PHDIFF(NN,3)-360.
	  IF(PHDIFF(NN,3).LE.-240.) PHDIFF(NN,3) = PHDIFF(NN,3)+360.
C	  SPEC(NN,1) = 10.*ALOG10(XDATA(NN,1)**2 + XDATA(NN,2)**2)
C	  SPEC(NN,2) = 10.*ALOG10(XDATA(NN,3)**2 + XDATA(NN,4)**2)
C	  SPEC(NN,3) = 10.*ALOG10(XDATA(NN,5)**2 + XDATA(NN,6)**2)
C	  SPEC(NN,4) = 10.*ALOG10(XDATA(NN,7)**2 + XDATA(NN,8)**2)
	  SPDIFF(NN,1) = SPEC(NN,1) - SPEC(NN,2)
	  SPDIFF(NN,2) = SPEC(NN,2) - SPEC(NN,3)
	  SPDIFF(NN,3) = SPEC(NN,3) - SPEC(NN,1)
	  SPDIFF(NN,4) = SPEC(NN,4) 
c	  WRITE(69,1002) NN,FLOW*NN,SPDIFF(NN,1),SPDIFF(NN,2),
c     1		SPDIFF(NN,3),PHDIFF(NN,1),PHDIFF(NN,2),PHDIFF(NN,3)
C	  WRITE(69,1004) NN,FLOW*NN,SPEC(NN,1),SPEC(NN,2),
C     1	  SPEC(NN,3),SPEC(NN,4),PHDIFF(NN,1),PHDIFF(NN,2),PHDIFF(NN,3)
	  OUTDATA(NN,1) = NN
	  OUTDATA(NN,2) = FLOW*NN
	  OUTDATA(NN,3) = SPDIFF(NN,1)
	  OUTDATA(NN,4) = SPDIFF(NN,2)
	  OUTDATA(NN,5) = SPDIFF(NN,3)
	  OUTDATA(NN,6) = PHDIFF(NN,1)
	  OUTDATA(NN,7) = PHDIFF(NN,2)
	  OUTDATA(NN,8) = PHDIFF(NN,3)
 1002	  FORMAT(I6,F10.3,3E12.4,3F7.1)
	ENDDO
C	IF(1) STOP
C
	WRITE(CH(6),1001) EVENT_NO
 1001	FORMAT('LABEL',I10)
	WRITE(CH(5),1003) FILE(31:39)
 1003	FORMAT('LABEL  ',A8)
	CH(1) = 'PRINTER 2'
C	CH(1) = 'terminal 3'
	CH(2) = 'ERASE'
	CH(3) = 'INPUT TDSXYZ.MGO'
	CH(4) = 'RELOCATE 0. 205.'
C	CH(5) = 'LABEL FILE'
C	CH(6) = 'LABEL STR'
	CH(7) = 'HARDCOPY'
	CH(8) = 'END'
C	CALL MONGO(8,CH,1025,10,OUTDATA)
C
C	AVERAGE DATA IN SECTIONS TO SMOOTH, AND MAKE FIT
C
	NSUM = 50			! NUMBER OF POINTS IN AN AVERAGE
	NSTART = 0			! N-1ST POINT, I.E. IGNORE SOME
	NSECT = (1025-NSTART)/NSUM
	DO N = 1,NSECT
	  FREQ(N) = 0.
	  DO J = 1,8
	    AVR(N,J) = 0.
	    RMS(N,J) = 0.
	    COUNT(J) = 0.
	  ENDDO
	  DO M = 1,NSUM
	      NN = NSTART + M + NSUM*(N-1)
	      FREQ(N) = FREQ(N) + NN*FLOW
	      DO J = 1,4 			! SIGNAL AMPLITUE
		WTA(NN,J) = 1.
		AVR(N,J) = AVR(N,J) + SPEC(NN,J)
		RMS(N,J) = RMS(N,J) + SPEC(NN,J)**2
		COUNT(J) =  COUNT(J) + WTA(NN,J)
	      ENDDO
	      DO J = 5,7 			! PHASE DIFF
		WTA(NN,J) = 1.
		AVR(N,J) = AVR(N,J) + PHDIFF(NN,J-4)
		RMS(N,J) = RMS(N,J) + PHDIFF(NN,J-4)**2
		COUNT(J) =  COUNT(J) + WTA(NN,J)
	      ENDDO
	  ENDDO
	  DO J = 1,7
	      AVR(N,J) = AVR(N,J)/COUNT(J)
	      RMS(N,J) = SQRT(AMAX1(RMS(N,J)/COUNT(J) - AVR(N,J)**2,0.))
	  ENDDO
	  FREQ(N) = FREQ(N)/NSUM
	  WRITE(67,1004) N,FREQ(N),(AVR(N,J),J=1,7)
	  WRITE(67,1004) N,FREQ(N),(RMS(N,J),J=1,7)
 1004	  FORMAT(I5,F10.2,8E12.3)
	ENDDO	
C	IF(1) STOP
C
C	NOW AVERAGES AND STANDARD DEVIATIONS HAVE BEEN DONE, ELIMINATE
C		OUTLIERS
C
	DO N = 1,NSECT
	  DO J = 1,8
	    COUNT(J) = 0.
	  ENDDO
	  DO M = 1,NSUM
	      NN = NSTART + M + NSUM*(N-1)
	        J = 1	 			! SIGNAL AMPLITUDE
		IF(ABS(SPEC(NN,J)-AVR(N,J)).GT.3.*RMS(N,J)) THEN
	print*,'remove,sect,col,nn',n,j,nn
		  WTA(NN,J)=0.
	 	  WTA(NN,7)=0.
	 	  WTA(NN,5)=0.
		  COUNT(J) =  COUNT(J) + 1.
		ENDIF
	        J = 2	 			! SIGNAL AMPLITUDE
		IF(ABS(SPEC(NN,J)-AVR(N,J)).GT.3.*RMS(N,J)) THEN
	print*,'remove,sect,col,nn',n,j,nn
		  WTA(NN,J)=0.
	 	  WTA(NN,5)=0.
	 	  WTA(NN,6)=0.
		  COUNT(J) =  COUNT(J) + 1.
		ENDIF
	        J = 3	 			! SIGNAL AMPLITUDE
		IF(ABS(SPEC(NN,J)-AVR(N,J)).GT.3.*RMS(N,J)) THEN
	print*,'remove,sect,col,nn',n,j,nn
		  WTA(NN,J)=0.
	 	  WTA(NN,6)=0.
	 	  WTA(NN,7)=0.
		  COUNT(J) =  COUNT(J) + 1.
		ENDIF
	      DO J = 5,7 			! PHASE DIFF
		IF(ABS(PHDIFF(NN,J-4)-AVR(N,J)).GT.3.*RMS(N,J)) WTA(NN,J)=0.
C		AVRC(N,J) = AVRC(N,J) + WTA(NN,J)*PHDIFF(NN,J-4)
C		RMS(N,J) = RMS(N,J) + PHDIFF(NN,J-4)**2
		COUNT(J) =  COUNT(J) + (1.-WTA(NN,J))
	      ENDDO
	  ENDDO
	print*,'sect, amp. pts removed',n,(count(i),i=1,3)
	print*,'sect, phs. pts removed',n,(count(i),i=5,7)
	ENDDO	

C
C	REDO AVERAGES
C
	DO J = 1,8
	    SIGMAX(J) = -1000.
	ENDDO
	DO N = 1,NSECT
	  DO J = 1,8
	    AVRC(N,J) = 0.
	    COUNT(J) = 0.
	  ENDDO
	  DO M = 1,NSUM
	      NN = NSTART + M + NSUM*(N-1)
	      DO J = 1,4 			! SIGNAL AMPLITUDE
C		IF(ABS(SPEC(NN,J)-AVR(N,J)).GT.3.*RMS(N,J)) WTA(NN,J)=0.
		AVRC(N,J) = AVRC(N,J) + WTA(NN,J)*SPEC(NN,J)
C		RMS(N,J) = RMS(N,J) + SPEC(NN,J)**2
		COUNT(J) =  COUNT(J) + WTA(NN,J)
	      ENDDO
	      DO J = 5,7 			! PHASE DIFF
C		IF(ABS(PHDIFF(NN,J-4)-AVR(N,J)).GT.3.*RMS(N,J)) WTA(NN,J)=0.
		AVRC(N,J) = AVRC(N,J) + WTA(NN,J)*PHDIFF(NN,J-4)
C		RMS(N,J) = RMS(N,J) + PHDIFF(NN,J-4)**2
		COUNT(J) =  COUNT(J) + WTA(NN,J)
	      ENDDO
	  ENDDO
	  DO J = 1,7
	      AVRC(N,J) = AVRC(N,J)/COUNT(J)
C	      RMS(N,J) = SQRT(AMAX1(RMS(N,J)/COUNT(J) - AVR(N,J)**2,0.))
	      AVRCINV(N,1) = FREQ(N)
	      AVRCINV(N,J+1) = AVRC(N,J)
	      IF(N.GT.1) SIGMAX(J) = AMAX1(AVRC(N,J),SIGMAX(J))
	  ENDDO
	  WRITE(69,1004) N,FREQ(N),(AVRC(N,J),J=1,7)
	ENDDO	
C
	WRITE(CH(8),1001) EVENT_NO
	WRITE(CH(7),1003) FILE(31:39)
	WRITE(CH(3),1005) NSECT
 1005	FORMAT('LINES 1',I5)
	CH(1) = 'PRINTER 2'
C	CH(1) = 'terminal 3'
	CH(2) = 'ERASE'
C	CH(3) = 'LINES 1 NSECT'
	CH(4) = 'INPUT AVRXYZ.MGO'
	CH(5) = 'LIMITS 0. 1. 0. 1.'
	CH(6) = 'RELOCATE 0. 1.05'
C	CH(7) = 'LABEL FILE'
C	CH(8) = 'LABEL STR'
	CH(9) = 'HARDCOPY'
	CH(10) = 'END'
C	CALL MONGO(10,CH,105,8,AVRCINV)
C
C	FIT A STRAIGHT LINE TO PHASE DATA
C
	TOPFREQ = 1023.*FLOW
	PRINT*,'TOP FREQ',TOPFREQ
	J = 5				! X-Y PHASE
	  DO N = 1,NSECT
	    WT(N) = 1.
	  ENDDO
	  WT(1) = 0.
	  SUM = 0.
	  DO N = 1,NSECT
	    IF(AVRC(N,1).LT.(SIGMAX(1)-15.)) WT(N) = 0.
	    IF(AVRC(N,2).LT.(SIGMAX(2)-15.)) WT(N) = 0.
	    SUM = SUM + WT(N)
	  ENDDO
	  IF(SUM.GT.3) CALL LSFIT(AVRC(1,J),FREQ,WT,NSECT,A,B,RMSLSF)
	  PRINT*,'LSFIT XY',SUM,A*TOPFREQ,B,RMSLSF
	  WRITE(79,*)'LSFIT XY',SUM,A*TOPFREQ,B,RMSLSF
	J = 6				! Y-Z PHASE
	  DO N = 1,NSECT
	    WT(N) = 1.
	  ENDDO
	  WT(1) = 0.
	  SUM = 0.
	  DO N = 1,NSECT
	    IF(AVRC(N,3).LT.(SIGMAX(3)-15.)) WT(N) = 0.
	    IF(AVRC(N,2).LT.(SIGMAX(2)-15.)) WT(N) = 0.
	    SUM = SUM + WT(N)
	  ENDDO
	  IF(SUM.GT.3) CALL LSFIT(AVRC(1,J),FREQ,WT,NSECT,A,B,RMSLSF)
	  PRINT*,'LSFIT YZ',SUM,A*TOPFREQ,B,RMSLSF
	  WRITE(79,*)'LSFIT YZ',SUM,A*TOPFREQ,B,RMSLSF
	J = 7				! Z-X PHASE
	  DO N = 1,NSECT
	    WT(N) = 1.
	  ENDDO
	  WT(1) = 0.
	  SUM = 0.
	  DO N = 1,NSECT
	    IF(AVRC(N,1).LT.(SIGMAX(1)-15.)) WT(N) = 0.
	    IF(AVRC(N,3).LT.(SIGMAX(3)-15.)) WT(N) = 0.
	    SUM = SUM + WT(N)
	  ENDDO
	  IF(SUM.GT.3) CALL LSFIT(AVRC(1,J),FREQ,WT,NSECT,A,B,RMSLSF)
	  PRINT*,'LSFIT ZX',SUM,A*TOPFREQ,B,RMSLSF
	  WRITE(79,*)'LSFIT ZX',SUM,A*TOPFREQ,B,RMSLSF
C
C	IF(1) STOP
	RETURN
	END
	SUBROUTINE SUMMPLOT(ITERM,CH,NPTST,NPTND)
C
C	LEFT HAND SECTION PLOTS MEASURED FIELDS, AND BZ RAW, RIGHT
C	PLOTS ONE COMPONENT AGAINST ANOTHER FOR A SELECTED FRACTION
C		 OF AN EVENT
C
	CHARACTER*12 title(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*6 LABELY(10)
	CHARACTER*7 LABEL1(4)
	CHARACTER*12 LABELT
	character*32 ITEM
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS,DOY,ERT(2),OK
	INTEGER*4 SUNCLOCK,CH
	REAL*8 SCET8
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	common /nrblk/ nrem,NHRMN,IFASTSLOW,ANGEVTOB(3)
	common /headblk/ major,minor,s_scet,nsys
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	COMMON /EXTRA/ NBXBY,NDATA(2048,4),DBSPEC(1025,4),AVRB,STDB
	COMMON /FRLIMITS/ FREQMIN,TOPFREQ
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
	DIMENSION YY(2048),XX(2048),PP(2048)
	REAL AVRB(1025,4),STDB(1025,4)
	DATA TWOPI /6.2831853/
C	REAL		AngEVtoB(3)
C	DATA LABELB /'BX(nT)','BY(nT)','BZ(nT)','EX(V)','BZ raw'/
	DATA LABELY /'EX(V)','EY(V)','EZ(V)','EX(V)','EY(V)','EZ(V)',
     1	'BX(nT)','BY(nT)','BZ(nT)','raw'/
	DATA LABEL1 /'MAX VAR','INT VAR','MIN VAR','B'/
	DATA DOY /0/
C
	print*,'starting summplot'
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C	  NSYS = 0			! POSSIBLY TEMPORARY
	print*,'at summ, iant=',iant
C*************
C	DO N = NPTST,NPTND
C	  WRITE(67,*) N,XFDATA(N,1),XFDATA(N,2),XFDATA(N,3)
C	ENDDO
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
C 1012	  FORMAT(I10)
 1009	  FORMAT(F8.1)
	  TITLE(12) = 'SCET'
c	  ITEM = 'EVENT_SCET'
c	  ok = W_ITEM_I4(ch, item, S_SCET, 2, ret_size)
	  WRITE(STR,1013) s_scet(1)
 1013	  FORMAT(I8)
	   WRITE(TITLE(13),1016) str(1:4),str(5:6),str(7:8)
	   ITEM = 'EVENT_SCET_R8'
	   ok = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	   call w_ur8_to_ydoy(scet8,yyyy,doy,msday)
	   WRITE(TITLE(14),1014) DOY
	   WRITE(TITLE(15),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1014	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
	  WRITE(title(18),*) 'RECEIVED AT' 
	  ITEM = 'EVENT_TM_SCET_I4'
	  ok = w_item_I4(ch, item, ERT, 2, return_size)
	  WRITE(STR,1013) ERT(1)
	  WRITE(TITLE(19),1016) str(1:4),str(5:6),str(7:8)
	  WRITE(title(20),1018) ERT(2)/100,mod(ERT(2),100)
 1017	  format(I10)
	  WRITE(title(21),1027),'WIND ORBIT'
 1027	  FORMAT(A10)
	  WRITE(title(22),1021) '  Xre',XRE
	  WRITE(title(23),1021) '  Yre',YRE
	  WRITE(title(24),1021) '  Zre',ZRE
 1021	  format(a5,f5.1)
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,320.,3050.,2170.)
	  ELSE
	    CALL MGOSETLOC(339.,50.,661.,750.)
 	  ENDIF
C
	  XTITLE = GX2 -.1*(GX2-GX1)              ! 3 dec 1996
	  YTITLE = GY2
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  TRANGE = GY2-GY1
	  TINC = .03*TRANGE
	  CALL MGOSETEXPAND(.6)
	  TITLE(6) = 'SAMPLES'
	  WRITE(STR,1007) NPTST,NPTND
 1007	  FORMAT(I4,' TO',I5)	
	  TITLE(7) = STR(1:12)
	  TITLE(16) = 'FFT BANDPASS'
	  WRITE(TITLE(17),1019) FREQMIN,TOPFREQ
 1019	  FORMAT(F5.1,'-',F5.0)
	  YTITLE = YTITLE + TINC
	  DO N = 4,24
	    YTITLE = YTITLE - TINC
	    IF(N.EQ.6) YTITLE = YTITLE - TINC
	    IF(N.EQ.16) YTITLE = YTITLE - TINC
	    IF(N.EQ.18) YTITLE = YTITLE - TINC
	    IF(N.EQ.21) YTITLE = YTITLE - TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(12,TITLE(N))
	  ENDDO
C
C	NEW STUFF IN  SPACE
C
   	  XTITLE = GX2 -.3*(GX2-GX1)         
	  YTITLE = GY2
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(4,'TDSS')
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  TITLE(24) = ' AVR.FREQ.'
	  WRITE(TITLE(25),1025) AVRFREQ
 1025	  FORMAT(F8.2,' kHZ')
	  CALL MGOLABEL(12,TITLE(24))
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(25))	
C
	  CALL VMATRIX(XGSE,1,2048)
C
	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BX, 1, return_size)
	     item = 'WIND_MFI_BY(GSE)_R4'
	     ok = w_item_R4(ch, item, BY, 1, return_size)
	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BX, 1, return_size)
	     item = 'WIND_MFI_BY(GSE)_R4'
	     ok = w_item_R4(ch, item, BY, 1, return_size)
	     item = 'WIND_MFI_BZ(GSE)_R4'
	     ok = w_item_R4(ch, item, BZ, 1, return_size)
	     item = 'WIND_MFI_BZ(GSE)_R4'
	     ok = w_item_R4(ch, item, BZ, 1, return_size)
	  item = 'WIND_MFI_BMAG_R4'
	  ok = w_item_R4(ch, item, BMAG, 1, return_size)
C***********
C	     	BX = 150.
C		BY = 250.
C		BZ = 300.
C		BMAG = SQRT(BX**2+BY**2+BZ**2)
	  FCE = BMAG*28.
C
	  DO IV = 1,3
	    EDOTB =  (EVECT(1,IV)*BX + EVECT(2,IV)*BY
     1		+ EVECT(3,IV)*BZ)/BMAG
	    WRITE(79,*) 'B DOT WITH EIGENV',IV, EDOTB
	    ANGEVTOB(IV) = ACOSD(EDOTB)
	  ENDDO
C
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(15,'ANGLE, SMALLEST')	
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,'EIGENVECT TO B')
	  YTITLE = YTITLE - TINC  
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  IF(ANGEVTOB(3).GT.90.) ANGEVTOB(3)=180.-ANGEVTOB(3)
C************
	  WRITE(39,*) TITLE(13),TITLE(15),TITLE(5),ANGEVTOB(3)
C**********
	  WRITE(LABELT,1022) ANGEVTOB(3)
 1022	  FORMAT(F5.0,' deg')
	  CALL MGOLABEL(10,LABELT)
C
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(15,'ANGLE, LARGEST')	
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,'EIGENVECT TO B')
	  YTITLE = YTITLE - TINC  
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  IF(ANGEVTOB(1).GT.90.) ANGEVTOB(3) = 180. - ANGEVTOB(1)
	  WRITE(LABELT,1022) ANGEVTOB(1)
	  CALL MGOLABEL(10,LABELT)
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(5,' fce ')
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(LABELT,1008) fce/1000.
 1008	  FORMAT(F7.3,' kHz')
	  CALL MGOLABEL(12,LABELT)
C
C	  PLOT TDS DATA 
C
C
C	  IF(ITERM.LT.0) THEN
C	    CALL MGOSETLOC(500.,350.,2700.,2200.)
C	  ELSE
C	    CALL MGOSETLOC(339.,50.,661.,750.)
C 	  ENDIF
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
	EANGLE =  -360.*SUNCLOCK/4096. - 45.       ! ANGLE SUN TO +EX AT END
	DANG = SPINRATE*360./SPS/TWOPI		  ! CHANGE PER SAMPLE
	THANT = EANGLE + 2049.*DANG + 360.	  ! ANGLE SUN TO +EX AT MIDDLE??
c
c	PRINT*,'IN PLOTSEL, XFDATA=',(XFDATA(1,N),N=1,4)
C
C	PLOT MEASURED FIELDS ON LEFT SIDE
C
	  NXW = 3
	  NYW = 5
	  NW = 15
C	the plots on the left side have nxw = 1, nyw = 1-5, iw = 1,4,7,10,13
C	       and n2 = 1,2,3,4,5.  n2 = 1,2,3 are X,Y,Z, of whichever field
C	       is main field, 4 is the other, and 5 is raw data	
	  DO IW = 1,NW,3
	    CALL MGOWINDOW(NXW,NYW,IW)
	    N2 = (IW+2)/3
C
	    NPT = 0	
	    YMAX = -1000.
	    YMIN = 1000.
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
  	      PP(NPT) = (N-1)/SPSS
	      IF(N2.GT.4) THEN				! BZ RAW
	        YY(NPT) = NDATA(N,3)
  	        PP(NPT) = THANT - N*DANG 
	      ELSE
	        YY(NPT) = XDATA(N,N2)
	      ENDIF
	      YMAX = AMAX1(YY(NPT),YMAX)
	      YMIN = AMIN1(YY(NPT),YMIN)
	    ENDDO
C	print*,'y',iw,n2,ymin,ymax,iant(n2)
	    CALL MGOSETLIM(PP(1),YMIN,PP(NPT),YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(PP,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    IF(IW.EQ.1) CALL MGOXLABEL(3,'SEC')
	    IF(N2.GT.4) CALL MGOXLABEL(17,'ANGLE,SUN TO +EX')
	    IF(N2.LE.4) NLAB = IANT(N2)
	    IF(N2.GT.4) NLAB = 10
	    PRINT*,'IW,N2,NLAB',IW,N2,NLAB
	    PRINT*,'YLABEL CHECK ',NLAB,N2,LABELY(NLAB)
	    CALL MGOYLABEL(6,LABELY(NLAB))
	    CALL MGOSETEXPAND(1.)
C	if(n2.eq.2) then
c	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
C	  print*,nptmax,xx(nptmax),yy(nptmax)
c	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
C	endif
C
	  ENDDO
C
c	  IF(IPROCESS.GE.3) THEN
c	    YTITLE = YTITLE-2.*TINC
c	    CALL MGOSETEXPAND(.5)
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    CALL MGOLABEL(8,' BAD TDS')
c	    YTITLE = YTITLE-TINC
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    CALL MGOLABEL(9,'CORRECTED')
c	    YTITLE = YTITLE-TINC
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    WRITE(STR,1024) IPROCESS
 1024	    FORMAT(' LEVEL',I2)
c	    CALL MGOLABEL(8,STR)
c	    WRITE(STR,1026) ISTART
 1026	    FORMAT(I5,' PTS')
c	    YTITLE = YTITLE-TINC
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    CALL MGOLABEL(9,STR)
c	    CALL MGOSETEXPAND(.8)
c	  ENDIF
C
	  CALL MGOSETEXPAND(.6)
C
C	PLOT MINIMUM VARIANCE FIELDS ON RIGHT SIDE.  SECOND ARGUMENT OF 
C	XDATA, 1 IS LARGEST, 3 IS SMALLEST
C
	  XMAX = -1000.
	  YMAX = -1000.
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
	  IF(1.364*YMAX.GT.XMAX) THEN
		NMAX = NYMAX
	  ELSE
		NMAX = NXMAX
	  ENDIF
	PRINT*,'SUMMPLOT,iw,XMAX,YMAX',IW,XMAX,YMAX
	PRINT*,'SUMMPLOT,NMAX,NXMAX,NYMAX',NMAX,NXMAX,NYMAX
C	  XMAX = 1000.*XMAX
C	  YMAX = 1000.*YMAX
	  XMAX = 1.1*AMAX1(XMAX,1.364*YMAX)
	  YMAX = XMAX/1.364
C
	  NXW = 3
	  NYW = 3
	  NW = 9
C             plot IW = 2,5,8, N1 is X axis, N2 is Y axis 
	  DO IW = 2,NW,3
	    IW1 = IW
	    IF(IW.EQ.8) IW1 = 3
	    CALL MGOWINDOW(NXW,NYW,IW1)
	    N1 = IW/2
	    N2 = N1+1
	    IF(IW.EQ.5) THEN
	      N1 = 1
	      N2 = 3
	    ENDIF
	    IF(IW.EQ.8) THEN
		N2 = 2
		N1 = 3
	    ENDIF
C
	    NPT = 0	
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
	      XX(NPT) = XFDATA(N,N1)
	      YY(NPT) = XFDATA(N,N2)
	    ENDDO
	    CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(9,LABEL1(N1))
	    CALL MGOYLABEL(9,LABEL1(N2))
	    CALL MGOSETEXPAND(.8)
	    CALL MGOSETEXPAND(1.)
C	    PUT ON ARROW
	    NPTMAX = NMAX - NPTST + 1
	    SIZE = .01*SQRT(XMAX**2 + YMAX**2)
	    IF(ITERM.GT.0) SIZE = 2.*SIZE
	    XARR = .5*(XX(NPTMAX+1) + XX(NPTMAX))
	    YARR = .5*(YY(NPTMAX+1) + YY(NPTMAX))
	    DX = XX(NPTMAX+1) - XX(NPTMAX)
	    DY = YY(NPTMAX+1) - YY(NPTMAX)
	print*,'summp',iw,nptst,nptnd
	print*,'nptmax',nptmax,xx(nptmax+1),xx(nptmax)
	print*,'summp',dx,dy,xarr,yarr,xmax
	IF(DX**2+DY**2.NE.0.) CALL ARROW(XARR,YARR,DX,DY,SIZE)
C	if(n2.eq.2) then
c	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
C	  print*,nptmax,xx(nptmax),yy(nptmax)
c	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
C	endif
C
	  ENDDO
C
	  CALL MGOSETEXPAND(.8)
C
C	PLOT A SPECTRUM
C
	  NXW = 3
	  NYW = 3
	  IW = 8
	  YMIN = 1000.
	  YMAX = -1000.
	  CALL MGOWINDOW(NXW,NYW,IW)
	    NPT = 0	
    	    DO N = 2,1024
	      NPT = NPT+1
	      XX(NPT) = ALOG10(SPS*(N-1)/2048.)
	      YY(NPT) = DBSPEC(N,NBXBY)
	      YMAX = AMAX1(YY(NPT),YMAX)
	      YMIN = AMIN1(YY(NPT),YMIN)
	    ENDDO
	    XMAX = ALOG10(SPS/2.)
	    XMIN = ALOG10(SPS/2048.)
	    CALL MGOTICKSIZE(-1.,-1.,0.,0.)  
	    CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOCONNECT(XX,YY,NPT)
	    NPT = 0
	    IF(NBXBY.EQ.3) NBXBY = 2
    	    DO N = 2,1024
	      NPT = NPT+1
	      YY(NPT) = AVRB(N,NBXBY)
	    ENDDO
	    CALL MGOSETLTYPE(1)
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(9,'FREQ (Hz)')
	    IF(NBXBY.EQ.1) CALL MGOYLABEL(14,'BX dB nT\u2/Hz')
	    IF(NBXBY.EQ.2) CALL MGOYLABEL(14,'BY dB nT\u2/Hz')
C
C	  CALL MGOSETEXPAND(.6)
C	print*,'in SUMMPLOT,nsys=',nsys
 	  IF(NSYS.EQ.0)CALL MGOPLOTID('S/C','[.WIND]TDSXYZ,SUMMPLOT')
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,SUMMPLOT')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('VAR MX','[.WIND]TDSXYZ,SUMMPLOT')
 	  IF(NSYS.EQ.3)CALL MGOPLOTID('GSE','[.WIND]TDSXYZ,SUMMPLOT')
	    CALL MGOSETEXPAND(.8)
 
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED IN SUMMPLOT',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
C
	END	
	SUBROUTINE WHISTLER(ITERM,CH,IPCH)
C
C	TO DIVIDE AN EVENT INTO SECTIONS AND (1) PLOT SPECTRA FOR EACH 
C	SECTION AND (2) DETERMINE FREQUENCY, ELLIPTICITY, ANGLES TO B
C	AND PLOT
C
	CHARACTER*12 title(30),PTITLE(30)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*6 LABELY(10)
	CHARACTER*4 LABEL1(4),PARX(9)
	CHARACTER*12 LABELT
	character*32 ITEM
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS,DOY,ERT(2),OK
	INTEGER*4 SUNCLOCK,CH
	REAL*8 SCET8
c	REAL*4 ZCROSS(2048),ZINT(2048),YYT(1024)
	REAL*4 YYT(1024)
	REAL*4 SPECT(2050),PDATA(2050),SPECTSM(2050)
	REAL*4 SECDATA(1024),ANGKTOB(256),ANGETOB(256),ECC(256)
	REAL*4 PPSEC(256)
	COMMON /HEADBL/ PTITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	common /nrblk/ nrem,NHRMN,IFASTSLOW,ANGEVTOB(3)
	common /headblk/ major,minor,s_scet,nsys
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NOTDATA(2048),XNOTDATA(2050),XNSPECT(1025),END_ANGLE,DANG,TPAUSE
	COMMON /EXTRA/ NBXBY,NDATA(2048,4),DBSPEC(1025,4),AVRB,STDB
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	COMMON /FRLIMITS/ FREQMIN,TOPFREQ
	COMMON /ZFREQ/ ZCFREQ(1024,4)
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
	DIMENSION YY(2048),XX(2048),PP(2048)
	REAL AVRB(1025,4),STDB(1025,4),FREQ(1024)
	REAL PLOTFREQ(128),ECCMAX(128),ECCMIN(128)
	REAL TFREQ(128),TECC(128),PPF(1024)
	DATA TWOPI /6.2831853/
	DATA NSTART,NPTND /1,2048/
C	DATA LABELB /'BX(nT)','BY(nT)','BZ(nT)','EX(V)','BZ raw'/
	DATA PARX /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1    ' BX ',' BY ',' BZ '/
	DATA LABELY /'EX(V)','EY(V)','EZ(V)','EX(V)','EY(V)','EZ(V)',
     1	'BX(nT)','BY(nT)','BZ(nT)','raw'/
	DATA LABEL1 /'E1','E2','E3','B'/
	DATA DOY /0/
C
C	  PLOT FOURIER SPECTRA FOR NSPECT SPECTRA
C
	DO N = 1,10
	  TITLE(N) = PTITLE(N)
	  PRINT*,N,TITLE(N)
	ENDDO
	DO N = 11,28
	  TITLE(N+2) = PTITLE(N)
	  PRINT*,N+2,TITLE(N+2)
	ENDDO
C
	NSPECT = 8
	NVAR = 2048/NSPECT
	NHALF = NVAR/2
	IPROCESS = 4
	SEP = 40.
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	CALL MGOSETLWEIGHT(2)
	IF(ITERM.LT.0) THEN
c	  CALL MGOSETLOC(500.,400.,2000.,3100.)
	  CALL MGOSETLOC(500.,500.,2000.,2800.)  	!
	ELSE
	  CALL MGOSETLOC(75.,150.,900.,700.)
	ENDIF

C	CALL MGOSETEXPAND(.8)
	TRANGE = GY2-GY1
	TINC = .04*TRANGE
	XTITLE = GX2 +.03*(GX2-GX1)
	YTITLE = GY2
C	CALL MGOSETEXPAND(.8)
C
	FUNDFR = SPS/NVAR
	PRINT*,'IN WHISTLER, FUNDFR=',FUNDFR
	DO NC = 1,NHALF
	  FREQ(NC) = (NC-1)*FUNDFR
	ENDDO
C

	CALL MGOTICKSIZE(0.,0.,0.,0.)
	YMIN = -80
	YMAX = -20. + NSPECT*SEP  
	CALL MGOSETLIM(FREQ(1),-80.,FREQ(NHALF),YMAX)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(9,'FREQ, HZ')
	CALL MGOYLABEL(20,'DB V\u2/HZ + OFFSET')
	XTITLE = .15*FREQ(NHALF)
	YTITLE = .95*YMAX + .05*YMIN
	CALL MGOSETEXPAND(.8)
	CALL MGORELOCATE(XTITLE,YTITLE)
        CALL MGOLABEL(11,TITLE(13))
	CALL MGOLABEL(10,TITLE(15))
	CALL MGOLABEL(10,' EVT NO.')
	CALL MGOLABEL(10, TITLE(5))
	CALL MGOSETEXPAND(1.)
C
	SNORM = 20.*ALOG10(FLOAT(NHALF)) + 10.*ALOG10(FUNDFR)
	IK = 1
	ISP = 1
	SPECT(ISP) = -SNORM
C
	DO NS = 1,NSPECT
	  NSTT = NSTART + (NS-1)*NVAR
	  NP = 0
	  ISP = 1
C		LOAD PDATA
	  DO NC = NSTT,NSTT+NVAR-1
	    NP = NP+1
       	    PDATA(NP) = XDATA(NC,IPCH)
	  ENDDO
	  CALL REALFT(PDATA,NHALF,1)
C
C	  CALCULATE SPECTRUM, IN DB WRT 1 V**2/HZ
C
	  DO IK = 3,NVAR,2
	    ISP = ISP+1
	    TPWR = PDATA(IK)**2 + PDATA(IK+1)**2
	    SPECT(ISP) = -50.
	    IF(TPWR.NE.0.) THEN
	      SPECT(ISP) = 10.*ALOG10(TPWR)-SNORM + NS*SEP
C	      SPHASE(ISP) = 57.296*ATAN2(PDATA(IK+1),PDATA(IK))
	    ELSE
		print*,'WHISTLER ERROR',Pdata(ik),Pdata(ik+1),irx
	    ENDIF
	  ENDDO
C	print*,'ns,spect',nS,spect(1),spect(2),spect(nhalf)
C
	  SPECT(1) = SPECT(2)
C	SMOOTH SPECTRA
	  SPECTSM(1) = SPECT(1)
	  SPECTSM(NHALF) = SPECT(NHALF)
	  DO NT = 2,NHALF-1
	    SPECTSM(NT) = .25*SPECT(NT-1) + .5*SPECT(NT) + .25*SPECT(NT+1)
	  ENDDO
	  CALL MGOCONNECT(FREQ,SPECTSM,NHALF)
C
	ENDDO
C
	CALL MGOSETEXPAND(.7)
	CALL MGOPLOTID('WHISTLER (SPECT)','[.WIND]TDSXYZ')
	CALL MGOSETEXPAND(1.)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
C	  STOP
	ENDIF
C
C	NOW DO THE SECOND PLOT, OF FREQ, ANGLE TO B, ECCENTRICITY, ETC
C		TOP PANEL = DATA, (2)= FREQ, (3) ANGLE TO B (4) ANGLE TO E
C		(5) ECCENTRICITY
C
	  TPAUSE = 2.
 	  START = SECNDS(0.)
	  DOWHILE(SEC.LT.TPAUSE)
	    SEC = SECNDS(START)
	  ENDDO
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C	  NSYS = 0			! POSSIBLY TEMPORARY
C
	XTITLE = GX2 -.15*(GX2-GX1)
	YTITLE = GY2
	TRANGE = GY2-GY1
	TINC = .03*TRANGE
	CALL MGOSETEXPAND(.8)
C
	   WRITE(TITLE(6),1006) IPCH
 1006	   FORMAT('CHANNEL',I2)
	   IF(IFASTSLOW.EQ.1) IRX = IPCH+3
	   IF(IFASTSLOW.EQ.2.AND.IPCH.NE.4) IRX = IPCH+6
	   IF(IFASTSLOW.EQ.2.AND.IPCH.EQ.4) IRX = 4
	   WRITE(TITLE(7),1007) PARX(IRX)
 1007	   FORMAT('P/A ',A4)
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	YTITLE = GY1 + .3*(GY2-GY1)
	CALL MGOGRELOCATE(GX1-.12*GX2,YTITLE)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,400.,2000.,3000.)  	! TO FIT PUBCOMBO
	ELSE
	  CALL MGOSETLOC(75.,150.,900.,700.)
	ENDIF
C
C	  PUT MORE LABELS ON RIGHT HAND SIDE
C
C 1012	  FORMAT(I10)
 1009	  FORMAT(F8.1)
	  TITLE(12) = 'SCET'
c	  ITEM = 'EVENT_SCET'
c	  ok = W_ITEM_I4(ch, item, S_SCET, 2, ret_size)
	  WRITE(STR,1013) s_scet(1)
 1013	  FORMAT(I8)
	   WRITE(TITLE(13),1016) str(1:4),str(5:6),str(7:8)
	   ITEM = 'EVENT_SCET_R8'
	   ok = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	   call w_ur8_to_ydoy(scet8,yyyy,doy,msday)
	   WRITE(TITLE(14),1014) DOY
	   WRITE(TITLE(15),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1014	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
	  WRITE(title(18),*) 'RECEIVED AT' 
	  ITEM = 'EVENT_TM_SCET_I4'
	  ok = w_item_I4(ch, item, ERT, 2, return_size)
	  WRITE(STR,1013) ERT(1)
	  WRITE(TITLE(19),1016) str(1:4),str(5:6),str(7:8)
	  WRITE(title(20),1018) ERT(2)/100,mod(ERT(2),100)
	  WRITE(title(22),1021) '  Xre',XRE
	  WRITE(title(23),1021) '  Yre',YRE
	  WRITE(title(24),1021) '  Zre',ZRE
 1021	  format(a5,f5.1)
C
	DO N = 21,27
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO

c	IF(1) RETURN
C
	if(1) go to 20
c
	  XTITLE = GX2 -.1*(GX2-GX1)              ! 3 dec 1996
	  YTITLE = GY2
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  TRANGE = GY2-GY1
	  TINC = .03*TRANGE
	  CALL MGOSETEXPAND(.6)
	  TITLE(6) = 'SAMPLES'
	  TITLE(7) = STR(1:12)
	  TITLE(16) = 'FFT BANDPASS'
	  WRITE(TITLE(17),1019) FREQMIN,TOPFREQ
 1019	  FORMAT(F5.1,'-',F5.0)
	  YTITLE = YTITLE + TINC
	  DO N = 4,23
	    YTITLE = YTITLE - TINC
	    IF(N.EQ.6) YTITLE = YTITLE - TINC
	    IF(N.EQ.16) YTITLE = YTITLE - TINC
	    IF(N.EQ.18) YTITLE = YTITLE - TINC
	    IF(N.EQ.21) YTITLE = YTITLE - TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(12,TITLE(N))
	  ENDDO

C
C	NEW STUFF IN  SPACE
C
   	  XTITLE = GX2 -.3*(GX2-GX1)         
	  YTITLE = GY2
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(4,'TDSS')
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  TITLE(24) = ' AVR.FREQ.'
	  WRITE(TITLE(25),1025) AVRFREQ
 1025	  FORMAT(F8.2,' kHZ')
	  CALL MGOLABEL(12,TITLE(24))
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(25))	
C
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(15,'ANGLE, SMALLEST')	
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,'EIGENVECT TO B')
	  YTITLE = YTITLE - TINC  
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(LABELT,1022) ANGEVTOB(3)
 1022	  FORMAT(F5.0,' deg')
	  CALL MGOLABEL(10,LABELT)
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(15,'ANGLE, LARGEST')	
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,'EIGENVECT TO B')
	  YTITLE = YTITLE - TINC  
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(LABELT,1022) ANGEVTOB(1)
	  CALL MGOLABEL(10,LABELT)
	  WRITE(LABELT,1028) fce
 1028	  FORMAT('fce= ',f8.3)
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(13,LABELT)
C
 20	continue
C
	  DO N = 1,2048
  	    PP(N) = 1000.*(N-1)/SPS		! msec
	  ENDDO
C
	XSTART = 400.
	XEND   = 1900.
	YBOT = 400
	YTOP = 3000.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,YBOT,XEND,YTOP)
	ELSE
c	  YBOT = BOTTOM + .446*(TOP-BOTTOM)
c	  YTOP = BOTTOM + .694*(TOP-BOTTOM)
	  CALL MGOSETLOC(XSTART,YBOT,XEND,YTOP)
	ENDIF
C
C
C	PLOT TDS DATA IN PHYSICAL UNITS
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
	IPCHX = IPCH
C	CHANGE TO mV/meter
	  DO N = 1,2048
	    YY(N) = ACORR*XDATA(N,IPCHX)/EFFLEN
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	  ENDDO
C
	PRINT*,'MAX mV/m',YMAX
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOSETLIM(PP(1),-YMAX,PP(2048),YMAX)
C
	CALL MGOWINDOW(1,4,4)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,2048)
	CALL MGOSETEXPAND(.7)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(5,'mSEC.')
	IF(IPCHX.LE.6) CALL MGOYLABEL(4,'mV/m')
	IF(IPCHX.GT.6) CALL MGOYLABEL(2,'nT')
	CALL MGOSETEXPAND(1.)
 	  IF(NSYS.EQ.0)CALL MGOPLOTID('S/C','[.WIND]TDSXYZ,WHISTLER')
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,WHISTLER')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('VAR MX','[.WIND]TDSXYZ,WHISTLER')
 	  IF(NSYS.EQ.3)CALL MGOPLOTID('GSE','[.WIND]TDSXYZ,WHISTLER')
	    CALL MGOSETEXPAND(.8)
C
C	PLOT FREQUENCY FROM ZERO CROSSINGS, ALSO SMOOTH FREQUENCY
C
c	  BOTTOM = 400.
c	  TOP = 3100.
C
c	IF(ITERM.LT.0) THEN
c	  CALL MGOSETLOC(XSTART,1120.,XEND,1455.)
c	ELSE
c	  YBOT = BOTTOM + .267*(TOP-BOTTOM)
c	  YTOP = BOTTOM + .391*(TOP-BOTTOM)
c	  CALL MGOSETLOC(XSTART,YBOT,XEND,YTOP)
c	ENDIF
C
	SPSKHZ = .001*SPS
	YMIN = .5*SPSKHZ
	YMAX = 0.
	PRINT*,'IZCNT',IZCNT
	print*,'first few zint',zint(1),zint(2),zint(3)
	IF(IZCNT.GT.1) THEN
	  DO N = 1,IZCNT-1
	    IF(ZINT(N).EQ.0.) PRINT*,'IN PLOT, ZINT=0 AT',N
	    YY(N) = SPSKHZ/ZINT(N)
C 	      PP(N) = 1000.*(N-1)/SPS
	    PPF(N) = 500.*(ZCROSS(N)+ZCROSS(N+1))/SPS
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
	CALL MGOWINDOW(1,4,3)
  	YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
	YMIN = AMAX1(YMIN,0.)
	PRINT*,'YMIN,YMAX',YMIN,YMAX
C	YMIN =  .9*AVRFREQ
C	YMAX = 1.1*AVRFREQ
C	YMIN =  .6*AVRFREQ
C	YMAX = 1.6*AVRFREQ
	PRINT*,'SET TO   ',YMIN,YMAX
	CALL MGOSETEXPAND(.8)
	CALL MGOSETLIM(0.,YMIN,PP(2048),YMAX)
	CALL MGOTICKSIZE(0.,0.,0.,0.)  
c	CALL MGOCONNECT(PP,YYT,IZCNT-1)
	CALL MGOCONNECT(PPF,YYT,IZCNT-1)
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(5,'mSEC.')
	CALL MGOYLABEL(10,'FREQ (kHZ)')
	CALL MGOSETEXPAND(1.)
C
C	  CALL MGOSETEXPAND(.6)
C	print*,'in WHISTLER,nsys=',nsys
C
	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BX, 1, return_size)
C	     if(nmfi.eq.0) nmfi = return_size
	     item = 'WIND_MFI_BY(GSE)_R4'
	     ok = w_item_R4(ch, item, BY, 1, return_size)
	     item = 'WIND_MFI_BZ(GSE)_R4'
	     ok = w_item_R4(ch, item, BZ, 1, return_size)
	     item = 'WIND_MFI_BMAG_R4'
	     ok = w_item_R4(ch, item, BMAG, 1, return_size)
	     FCE = 28.*BMAG
C	SAVE VALUE OF B ANGLE FROM WHOLE SAMPLE

	IPCHS = IPCH
	NSEC = 32
	NSECSAMP = 2048/NSEC
	YMINE = 1000.
	YMAXE = 0.
	YMINB = 1000.
	YMAXB = 0.
	DO N = 1,NSEC
	  NS1 = (N-1)*NSECSAMP + 1
	  NS2 = NS1 + NSECSAMP - 1
	  PPSEC(N) = .5*(PP(NS1)+PP(NS2))
	  NSST = 1
	  DO NPC = 1,3
	    DO NSS = NS1,NS2
	      SECDATA(NSST) = XGSE(NSS,NPC)
	      NSST = NSST+1
	    ENDDO
	  ENDDO
	  CALL VMATRIX(XGSE,NS1,NS2)
C
	  DO IV = 1,3
	    EDOTB =  (EVECT(1,IV)*BX + EVECT(2,IV)*BY
     1		+ EVECT(3,IV)*BZ)/BMAG
	    WRITE(79,*) 'B DOT WITH EIGENV',IV, EDOTB
	    IF(IV.EQ.1) EANGLE = ACOSD(EDOTB)
	    IF(IV.EQ.3) BANGLE = ACOSD(EDOTB)
	  ENDDO
	  IF(BANGLE.GT.90.) BANGLE=180.-BANGLE
	  ANGKTOB(N) = BANGLE
	print*,'ang',n,bangle,angktob(n)
	  YMINB = AMIN1(YMINB,BANGLE)
	  YMAXB = AMAX1(YMAXB,BANGLE)
	  IF(EANGLE.GT.90.) EANGLE = 180.-EANGLE
	  ANGETOB(N) = EANGLE
	  YMINE = AMIN1(YMINE,EANGLE)
	  YMAXE = AMAX1(YMAXE,EANGLE)
	  ECC(N) = SQRT(EVAL(2)/EVAL(1))
	ENDDO
C
	CALL MGOWINDOW(1,4,1)
	CALL MGOSETLIM(0.,0.,PP(2048),1.)
	CALL MGOCONNECT(PPSEC,ECC,NSEC)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(4,'MSEC')
	CALL MGOYLABEL(12,'ECCENTRICITY')
C
	IF(IFASTSLOW.EQ.1) THEN
 	  CALL MGOWINDOW(1,4,2)
	  CALL MGOSETLIM(0.,YMINE,PP(2048),YMAXE)
	  CALL MGOCONNECT(PPSEC,ANGETOB,NSEC)
	  CALL MGOYLABEL(24,'ANGLE LARGEST EV TO B\d0')
	  CALL MGOBOX(1,2)
	ENDIF
C
	IF(IFASTSLOW.EQ.2) THEN
	  CALL MGOWINDOW(1,4,2)
	  CALL MGOSETLIM(0.,YMINB,PP(2048),YMAXB)
	  CALL MGOCONNECT(PPSEC,ANGKTOB,NSEC)
	  CALL MGOYLABEL(15,'ANGLE K TO B\d0')
	  CALL MGOBOX(1,2)
	ENDIF 

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
