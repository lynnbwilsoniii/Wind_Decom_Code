! wind/waves fft data collection and dc fit program

	program		fft_process
	implicit	integer*4 (a-z)

	ok = 1
	if (ok) ok = get_set_tm_stream()
	TYPE*,'GOING TO COLLECT EVENT DATA'
	if (ok) ok = wait_for_events()


	end
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_set_tm_stream()
c	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	character*4	event,pa(9)
	integer*4	iq,i,j,k,n,iterm,nchgp,npt,np,nday,isrc
	integer*4	iw
	REAL		X(25)
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	NMODEL(1100)
	real		MODELAV(1100),MODELSQ(1100)
	character*80	stream
	parameter	size=1024
	integer*4	return_size
	integer*4	raw(size)
	integer*4	mantissa(size)
	integer*4	gain(size)
	integer*4	pwrdb(size)    ! actually, .5 db steps
	integer*4	fft_channel
	integer*4	temp_waves
	character*32	s
	real*8		scet8,scetlast,scetspin,tdiff
	character*32	s_scet
	integer*4	scet(2)
	integer*4	ch, chhk, major, minor,iraw
	character*80	file
	character*10	title(4)
	character*32	item
	integer*4	ios,yyyy,idoy,msec
	integer*4	NHRMN,IPNT,LAST,NPTMX,N1,N2,NFILL,NPPL
	REAL		PTIME,DELT,DELTS,DPUTIME,PHASE,SPINRATE
	REAL		tstart,eltime,evtime,tend,tintv
	REAL		FFTSPACE(4),COUNT(4),SPACE
	real		nhist(100,10),vdata(1024),dbspec(1024)
	real		ddata(1024)
	integer*2	pwrl
	integer*2	pwrlt(1000,511)
	integer*4 	ncount
!

        COMMON /SCFITBLK/ FDATA(1024),WT(1024),ERR(1024),Y(3),NFITPT
	COMMON /TBLK/ NPT,PTIME(1000),EDATA(1000,5)
	common /fftblk/ gain,mantissa,pwrdb
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),IDOY
	common /headblk/ major,minor,fft_channel,s_scet,title,scet8
!
	data ncount /0/
	DATA IPNT /4*0/
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1		'BX','BY','BZ'/
	DATA JP /0/
	DATA IRAW /0/
	DATA RAWCOUNT,FFTCOUNT /0,0/
	DATA TWOPI /6.2831853/
	DATA NPTMX /998/
	DATA NTPLOTS /0/
C
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'

	ok = w_channel_open(ch,stream)
	if (.not. ok) stop 'cannot open tm channel'

	write(6,*) 'type hr,min to start,end, e.g. 0414,0800'
	read(5,*) nhrmn,last
	type*,nhrmn,last
	tstart = (nhrmn/100) + mod(nhrmn,100)/60.
	tlast = (last/100) + mod(last,100)/60.
	nchgp = 3
 3	format(q,i10)
 5	format(q,a)
	do n = 1,4
	  title(n) = '          '
	  fftspace(n) = 0.
	  count(n) = 1.E-8
	enddo
 
	ok = w_channel_filename(ch,file)
	print*,'file',file

	scet8 = 0.
	call w_channel_position(ch,scet8)
	nday = scet8
	type*,'first channel position ',scet8,' ch',ch
	scet8 = dfloat(nday) + ((nhrmn/100) + mod(nhrmn,100)/60.)/24. 
	type*,'set channel position to',scet8,' ch',ch
	scetlast= dfloat(nday) + ((last/100) + mod(last,100)/60.)/24. 
C**********  TEMPORARY CODE TO GET PAST START
	OK = WIND_TM_GET_MFMF(CH,MAJOR,MINOR)
	MAJOR = MAJOR+1
	OK = WIND_TM_GET_WORD(CH,MAJOR,MINOR,24,I)
C*********** END OF TEMPORARY CODE
	call w_channel_position(ch,scet8)
	type*,'channel position set to',scet8
	

	get_set_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events()
c
	  event = 'FFT'
c	  if(nchgp.eq.1) event = 'FFTH'
c	  if(nchgp.eq.2) event = 'FFTM'
	  if(nchgp.eq.3) event = 'FFTL'
	  ok = w_event(ch,event)
c************test of spin phase
C	phsav = 0.
C	do ix = 1,100
C	   call w_channel_position(ch,scetlast)
	   item = 'WIND_SPIN_PHASE_R4'
	   ok = w_item_r4(ch, item, PHASE, 1, return_size)
	   item = 'WIND_SPIN_scet_R8'
	   ok = w_item_r8(ch, item, scetspin, 1, return_size)
	   item = 'wind_spin_rate_r4'
	   ok = w_item_r4(ch, item, spinrate, 1, return_size)
c	write(88,*) 'phase,scet,rate',phase,scetspin,spinrate
c
c	calculate spin phase at start of event
c
C	   tdiff = 864.D02*(scetlast - scetspin)
C	   phdiff = tdiff*spinrate
C	   ph1 = phase
C	   if(phdiff.lt.0.) then
C		ph1 = ph1 + phdiff
C	        dowhile (ph1.lt.0.)
C		  ph1 = ph1+twopi
C		enddo
C		ph1 = amod(ph1,twopi)
C	   else
C		ph1 = amod(ph1 + phdiff,twopi)
C	   endif
c
C
C
C	print*,'position,phase,time,del',scetlast,phase,scetspin
C     1    ,phdiff,ph1-phsav
C	   phsav = ph1
C	   scetlast = scetlast + .5d00/864.d02
C	enddo
C	scetlast= dfloat(nday) + ((last/100) + mod(last,100)/60.)/24. 
C	if(1) stop
c*******
	  if(ok.eq.82) then
		PRINT*,'END OF FILE'
		TYPE*,'GO TO PLOT AT ',S_SCET,' previous'
		call tplot(ch,isrc)
c		CALL MODELADD(VDATA,SPINRATE,SUNANG,1)
		print*,'rawcount',rawcount
		print*,'fftcount',fftcount
		stop
	  endif

	  NEWOLD = 0
	  IF(NCHGP.EQ.0) THEN
		NCHGP = 3
	        NEWOLD = 1
	  ENDIF
	  print*,'collect events: ',event

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, scet, 2, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	   type*,'first event at',scet,'  evtime',evtime

C
	   item = 'EVENT_SCET_R8'
	   ok = w_item_r8(ch, item, scet8, 1, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
c
	  go to 110

	! this is the main program loop

 100	   continue

           ok = w_event(ch,event)
	  if(ok.eq.82) then
		PRINT*,'END OF FILE'
		TYPE*,'GO TO PLOT AT ',S_SCET,' previous'
		call tplot(ch,isrc)
c		CALL MODELADD(VDATA,SPINRATE,SUNANG,1)
		print*,'rawcount',rawcount
		print*,'fftcount',fftcount
		stop
	  endif
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
	      err_count = err_count + 1
	      if (err_count .lt. 2) goto 100
	      if (err_count .ge. 2) goto 200
	   end if

 110	  CONTINUE	  
C
	   ! now get the items
C
	   item = 'channel_number'
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
C
	   item = 'SOURCE'
	   ok = w_item_i4(ch, item, ISRC, 1, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   TYPE*,'CHANNEL,SOURCE',FFT_CHANNEL,ISRC,PA(ISRC)
	   if(isrc.ne.4) go to 100			! only exdc
C
	   item = 'WIND_SPIN_PHASE_R4'
	   ok = w_item_r4(ch, item, PHASE, 1, return_size)
C
	   item = 'WIND_SPIN_scet_R8'
	   ok = w_item_r8(ch, item, scetspin, 1, return_size)
C
	   item = 'wind_spin_rate_r4'
	   ok = w_item_r4(ch, item, spinrate, 1, return_size)
C
	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, scet, 2, return_size)
c
	   item = 'EVENT_SCET_frctn'
	   ok = w_item_i4(ch, item, msec, 1, return_size)
c
	   item = 'EVENT_SCET_R8'
	   ok = w_item_R8(ch, item, SCET8, 1, return_size)
	   if(scet8.gt.scetlast) print*,'scet8,scetlast',scet8,scetlast
	   if(scet8.gt.scetlast) call tplot(ch,isrc)
	   if(scet8.gt.scetlast) stop 'at event_scet'
c
	   item = 'DPU_MAJOR_FRAME'
	   ok = w_item_I4(ch, item, MAJOR, 1, return_size)
	   item = 'DPU_MINOR_FRAME'
	   ok = w_item_I4(ch, item, MINOR, 1, return_size)
	
	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
c	calculate spin phase at start of event
c
c	     if(newold.eq.1.and.fft_channel.lt.7) go to 100
C	     if(fft_channel.NE.3) go to 100
	     iw = fft_channel-6
C	     if(nchgp.eq.1) iw = fft_channel
C	     if(nchgp.eq.2) iw = fft_channel-2
	     if(nchgp.eq.3) iw = fft_channel-6
	     iw = max0(iw,1)
	     iw = min0(iw,4)
c	   IF(TITLE(iw).EQ.'          ') THEN
c	     WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(ISRC)
c	   ENDIF
	   WRITE(TITLE(1),1001) PA(ISRC)
 1001	   FORMAT('  ',A4)

c
	   print*,' items',scet,msec,fft_channel,phase
	   print*,' more items  ',isrc,pa(isrc),spinrate
C	   write(88,*) ' items',scet,msec,fft_channel,phase
C	   write(88,*)' more items  ',isrc,pa(isrc),spinrate
C
	TYPE*,'CALL FFT_PHYS'
	call fft_phys(ch,iraw,vdata,dbspec)
	IF(IRAW.EQ.1) THEN
	  RAWCOUNT = RAWCOUNT + 1.
	ELSE
	  FFTCOUNT = FFTCOUNT + 1.
	ENDIF
C	PRINT*,' CHANNEL',FFT_CHANNEL,' IRAW',IRAW,' AND FIRST VDATA'
	IF(IRAW.NE.1) GO TO 100
C	PRINT*,(VDATA(I),I=1,10)
C	PRINT*,(VDATA(I),I=1021,1024)
C	IF(FFT_CHANNEL.EQ.8) THEN
C	  open(unit=67,name='lowEZ.data',status='new')
C 	  write(67,*) fft_channel,scet
C	  DO N = 1,1024	
C		DDATA(N) = 1000.*VDATA(N)
C	  ENDDO
C 	  write(67,4455) DDATA
C	  close(unit=67)
 4455	format(12(1x,f6.3))
C	  IF(1) STOP
C	ENDIF
c
C	TYPE*,'GET ITEMS SPECTRUM_DB AND VOLTS/METER'
C	   item = 'SPECTRUM_DB'
C	   ok = w_item_R4(ch, item, DBSPEC, 1024, return_size)
c	   item = 'E_VOLTS/METER'
c	   ok = w_item_R4(ch, item, VDATA, 1024, return_size)
C
C	CALCULATE CYCLES PER SAMPLE FOR SCFIT
C
	X(1) = (SPINRATE/TWOPI)/.33356/1024.
	PRINT*,'CYCLES PER SAMPLE FOR SCFIT',X(1)
	NPT = NPT + 1
	PTIME(NPT) = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	NFITPT=1024
	DO N = 1,NFITPT
	  FDATA(N) = VDATA(N)
	ENDDO
	CALL FFTLOANG2(CH,FDATA,MAXS,MINS,SUNANG,IERR)
	DO I = 1,1024
	  WT(I) = 1.
	ENDDO
	MXS = MAX0(MAXS-10,1)
	MNS = MIN0(MAXS+50,1024)
	DO I = MNS,MXS
		WT(I) = 0.
	ENDDO
	MXS = MAX0(MINS-10,1)
	MNS = MIN0(MINS+50,1024)
	DO I = MNS,MXS
		WT(I) = 0.
	ENDDO
	CALL SCFIT(X,SUMSQ)
	PRINT*,'1st HARM Y',Y
C	EDATA(NPT,1) = Y(2)
C	EDATA(NPT,2) = Y(3)
C
C	CALCULATE SUN ANGLE AT BEGINNING OF EVENT
	SUNANG = SUNANG + (360./TWOPI)*SPINRATE/.33356 
c******** not checked
	SUNANGX = SUNANG + 45.
	SUNANGX = AMOD(SUNANGX,360.)
C	ELECTRIC FIELD IS OPPOSITE TO VOLTAGE DIFFERENCE
	EDATA(NPT,1) = -Y(2)*COSD(SUNANGX) - Y(3)*SIND(SUNANGX)
	EDATA(NPT,2) = -Y(3)*COSD(SUNANGX) + Y(2)*SIND(SUNANGX)
	EDATA(NPT,3) = SQRT(Y(2)**2 + Y(3)**2)
c	WRITE(66,*) PTIME(NPT),MAXS,MINS,SUNANG,Y(2),Y(3)
	DO N = 1,NFITPT
	  FDATA(N) = VDATA(N)
	ENDDO
	X(1) = 2.*X(1)
	CALL SCFIT(X,SUMSQ)
	PRINT*,'2nd HARM Y',Y
	EDATA(NPT,4) = Y(2)
	EDATA(NPT,5) = Y(3)
C
C	THE MODEL HAS 1098 POINTS PER REVOLUTION AND STARTS WHEN
C	THE S/C X AXIS POINTS AT THE SUN
C
	IWRITE = 0
c	CALL MODELADD(VDATA,SPINRATE,SUNANG,IWRITE)
c****
c	IF(ISRC.LT.7) type*,'tplot called'
c	IF(ISRC.LT.7) call tplot(ch,isrc)
C	NTPLOTS = NTPLOTS+1
C	IF(NTPLOTS.LE.8) THEN
C  	  type*,'tplot called'
C	  if(iraw.eq.1) call tplot(ch,isrc)
C	ENDIF
C

c*************** Cathie check*********
C	ncount = ncount+1
C	write(77,*) ncount,fft_channel,isrc,(dbspec(n),n=1,513)
C	if(ncount.ge.100) stop
C
C	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
C     1		+ mod(scet(2),100)/3600.
C	   eltime = evtime - tstart
C
 1002	FORMAT(2I8,I4,F9.3,E12.3)
C	

c
	   IF(SCET8.GT.SCETLAST) GO TO 100
	   IF(NPT.GT.995) GO TO 200
	   IF(scet(2).LT.(100*LAST)) GO TO 100

 200	continue

	TYPE*,NPT,' EVENTS FOUND'
	TYPE*,'GO TO PLOT AT ',S_SCET
C
	IW = 4
	TEND = AMAX1(TEND,PTIME(NPT))
	print*,'nptmx,ipnt',nptmx,ipnt
C
 210	    CONTINUE
 	    NPT = MIN0(NPT+1,NPTMX)
C
C	(N_CHANNEL_GROUP)  NCHGP=1 IS HIGH, 2 IS MID, 3 IS LOW
	   NPT = NPTMX
C	call rawprint
	print*,'call tplot if iraw = 1,iraw=',iraw
	if(rawcount.ne.0) call tplot(ch,isrc)
		print*,'fftcount',fftcount
		print*,'rawcount',rawcount
C	   IF(1) STOP
	return
	end
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	common /nrblk/ NHRMN
	integer*4	iq,NHRMN

  6	format(1x,'Enter TM stream type [O=offline(default), R=realtime ]: ',$)
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)

 10	write(6,6)
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
	end
	END
	SUBROUTINE TPLOT(ICH,ISRC)
C
C	PLOTS THE TIME DOMAIN DATA
C		note that ich is stream channel not fft_channel
C
	COMMON /TBLK/ NPT,PTIME(1000),EDATA(1000,5)
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),IDOY
	common /headblk/ major,minor,ifftch,scet,titlex,scet8
	character*32 scet,item
	character*10	titlex(4)
	integer*4 return_size,ok,w_item_r4
	real*8 scet8
c
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	DIMENSION YY(1024)
	CHARACTER*8 TITLE(4)
	CHARACTER*120 STRG(5)
	CHARACTER*120 STR
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/
C
C	ITERM = 3
	ITERM = -2
C
	PRINT*,' IN TPLOT, wind_lib chann,source=',ich,isrc
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  PRINT*,'START TPLOT',GX1,GX2,GY1,GY2
C	  CALL MGOSETLOC(500.,330.,GY2-200.,3000.)
	  CALL MGOSETLOC(GX1,GY1-150.,GX2,GY2-200.)
	ENDIF
C
	  CALL MGOSETEXPAND(.8)
	  TOP = LY2
	  CALL MGOGRELOCATE(GX1,TOP-200.)                    
	  CALL MGOPUTLABEL(6,'SCET   ',3)
	  CALL MGOPUTLABEL(70,SCET(1:11),3)
	  CALL MGOPUTLABEL(20,'   SOURCE FFT-LO, CH',3)
c	  IF(JW.EQ.1  CALL MGOYLABEL(5,'mV/M ')
	  WRITE(STR,802) IFFTCH
 802	  FORMAT(I3)
	  CALL MGOPUTLABEL(3,STR,3)
	  CALL MGOPUTLABEL(10,TITLEX(1),3)
	  IW = 5
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                    
C	  ELSE
C	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy
C	  ENDIF
	  CALL MGOSETEXPAND(1.)
C
	  XMIN = 0.
	  XMAX = 24.
C
	DO JW = 1,IW
	  CALL MGOWINDOW(1,IW,JW)
	  YMIN = EDATA(1,IW)
	  YMAX = YMIN
	  JP = 1
	  DO J = 1,NPT
	    JP = J
	    YY(JP) = 1000.*EDATA(J,JW)
C	if(ifftch.eq.7) write(77,*) j,fdata(j)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
C	  CALL MGOTICKSIZE(20.,100.,0.,0.)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PTIME,YY,JP)
C	  CALL MGOSETEXPAND(.8)
	  IF(JW.EQ.1) THEN
	    CALL MGOBOX(1,2)
	  ELSE
	    CALL MGOBOX(1,2)
	  ENDIF
	  IF(JW.EQ.1) CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  IF(JW.EQ.1)  CALL MGOYLABEL(8,'Ex (GSE)')
	  IF(JW.EQ.2)  CALL MGOYLABEL(8,'Ey (GSE)')
	  IF(JW.EQ.3)  CALL MGOYLABEL(9,'/E/ (GSE)')
	  IF(JW.EQ.4)  CALL MGOYLABEL(8,'V/m')
	  IF(JW.EQ.5)  CALL MGOYLABEL(8,'V/m')
	ENDDO
C
	CALL MGOSETEXPAND(.7)
	CALL MGOPLOTID('[kellogg.wind]FFTDC','TPLOT')
	CALL MGOSETEXPAND(1.)
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
	SUBROUTINE RAWPRINT
C
	common /fftblk/ nexpt(1024),ndata(1024),ipwrdb(1024)
	common /headblk/ major,minor,ich,scet,title,scet8
	real*8 scet8
	character*32 scet
	character*10	title(4)
C
	write(36,*) 'fft data collection at ',scet,' channel',ich
C
	DO I = 1,103
	  I2 = 10*I
	  I1 = I2 - 9
	  I2 = MIN0(I2,1024)
	  WRITE(36,1006) I1-1,(NEXPT(J),NDATA(J),J=I1,I2)
	ENDDO
C
	RETURN
 1006	FORMAT(I5,'.',(10(I3,I5)))
	END
       SUBROUTINE SCFIT(X,SUMSQ)
C
C	THIS ROUTINE FITS A SINE WAVE TO DATA.  X(1) CAN BE VARIED
C	BY HUNTMN OR GIVEN, WHILE Y(1) TO Y(3) ARE DETERMINED BY
C	LEAST SQUARES FIT IN CLOSED FORM.  FDATA IS THE ARRAY OF NPT
C	DATA POINTS.  
C
C	X(1) IS FREQUENCY IN CYCLES PER SAMPLE  (A SMALL NUMBER)
C
C	Y(1) IS AVERAGE (OFFSET)
C	Y(2) IS COEFF OF COSINE
C	Y(3) IS COEFF OF SINE
C
       COMMON /SCFITBLK/ FDATA(1024),WT(1024),ERR(1024),Y(3),NPT
       DOUBLE PRECISION COS1,SIN1,COSN,SINN,SINNT,THT0
       DIMENSION X(25),C(3,3)
       DATA TWOPI /6.2831853/
       DATA WT /1024*1./
C
	DO I = 1,3
	  Y(I)= 0.
	  DO J = 1,3
	    C(I,J) = 0.
	  ENDDO
	ENDDO
C
	THT0 = TWOPI*X(1)
	COS1 = DCOS(THT0)
	SIN1 = DSIN(THT0)
	SINN = SIN1
	COSN = COS1
	DO N = 1,NPT
	  Y(1) = Y(1) + WT(N)*FDATA(N)
	  Y(2) = Y(2) + WT(N)*FDATA(N)*COSN
	  Y(3) = Y(3) + WT(N)*FDATA(N)*SINN
	  C(1,1) = C(1,1) + WT(N)
	  C(1,2) = C(1,2) + WT(N)*COSN
	  C(1,3) = C(1,3) + WT(N)*SINN
C	  C(2,1) = C(2,1) + WT(N)*
	  C(2,2) = C(2,2) + WT(N)*COSN**2
	  C(2,3) = C(2,3) + WT(N)*COSN*SINN
C	  C(3,1) = C(3,1) + WT(N)*
C	  C(3,2) = C(3,2) + WT(N)*
	  C(3,3) = C(3,3) + WT(N)*SINN**2
	  SINNT = COSN*SIN1 + SINN*COS1
	  COSN = COS1*COSN - SIN1*SINN
	  SINN = SINNT
	ENDDO
	C(3,1)= C(1,3)
	C(3,2) = C(2,3)
	C(2,1) = C(1,2)
	CALL GAUSSJ(C,3,3,Y,1,1)
C
	SUMSQ = 0.
	SUMN = 1.E-8
	THT0 = TWOPI*X(1)
	COS1 = DCOS(THT0)
	SIN1 = DSIN(THT0)
	SINN = SIN1
	COSN = COS1
	DO N = 1,NPT
          ERR(N) =  FDATA(N) - Y(2)*COSN - Y(3)*SINN - Y(1)
	  SUMSQ = SUMSQ + WT(N)*ERR(N)**2
	  SUMN = SUMN + WT(N)
	  SINNT = COSN*SIN1 + SINN*COS1
	  COSN = COS1*COSN - SIN1*SINN
	  SINN = SINNT
	ENDDO
	SUMSQ = SUMSQ/SUMN
C	FIXEDV1 = Y(1) + Y(2)*COS(3.*THT0) + Y(3)*SIN(3.*THT0)
	FIXEDV = FDATA(3) - ERR(3)
C	PRINT*,'X,SUMSQ,2FIX',X(1),SUMSQ,FIXEDV1,FIXEDV
       RETURN
       END
	SUBROUTINE FFTLOANG2(ich,VDATA,MAX,MIN,SUNANG,IERR)
C
	DIMENSION VDATA(1)
	character*32	item
	DATA TWOPI /6.2831853/
C
C	THIS ROUTINE USES THE SHADOW GLITCH TO DETERMINE SUN ANGLE.
C	IN GSE COORDINATES, THE ANGLE SUN TO S/C X AXIS DECREASES WITH
C	TIME.  THE MAX POSITIVE DERIVATIVE IS WHEN -EX IS SHADOWED, THE
C	MAX NEGATIVE DERIVATIVE IS WHEN +EX IS SHADOWED.  THE S/C X
C	AXIS POINTS TO THE SUN 45 DEGREES LATER (IN TIME) THAN THE -EX 
C	SHADOW.
C
	IERR = 1
	MAX = 0
	MIN = 0
	DERMAX = 0.
	DERMIN = 0.
	DO I = 1,1023
	  DER = VDATA(I+1) - VDATA(I)
	  IF(DER.GT.DERMAX.AND.VDATA(I).GT.(-.001)) THEN
		MAX = I
		DERMAX = DER
	  ENDIF
	  IF(DER.LT.DERMIN.AND.VDATA(I).LT.(.001)) THEN
		MIN = I
		DERMIN = DER
	  ENDIF
	ENDDO	
C
C	CALCULATE SAMPLES PER REVOLUTION
C
	   item = 'wind_spin_rate_r4'
	   ok = w_item_r4(ich, item, spinrate, 1, return_size)
	SAMPPRV = 1024.*.33356*TWOPI/SPINRATE
C
C	CALCULATE SAMPLE NO. FOR S/C AXIS POINTING TO SUN
C
	IF(DERMAX+DERMIN.GE.0.) THEN
	  SAMPX0 = MAX + SAMPPRV/8.			! USE MAX
	ELSE
	  SAMPX0 = MIN - (3./8.)*SAMPPRV		! USE MIN
	ENDIF
C	
C	CALCULATE ANGLE AT END OF EVENT
C
	SUNANG = -(1024.-SAMPX0)*360./SAMPPRV
	SUNANG = AMOD(SUNANG+720.,360.)
	PRINT*,'ANG',MAX,DERMAX,MIN,DERMIN,SUNANG
	RETURN
	END
	SUBROUTINE MODELADD(VDATA,SPINRATE,SUNANG,IWRITE)
C
	integer*4	NMODEL(1100)
	real		MODELAV(1100),MODELSQ(1100),VDATA(1)
C	
	DATA TWOPI /6.2831853/
	DATA IREADIN /0/
C
C	WRITE ACCUMULATED DATA TO DISK IF THIS IS LAST CALL
C
	IF(IWRITE.EQ.1.AND.IREADIN.EQ.1) THEN
	  OPEN(UNIT=101,NAME='NMODEL.DAT',TYPE='NEW')
	  WRITE(101,*) NMODEL
 	  CLOSE(UNIT=101)
	  OPEN(UNIT=101,NAME='MODELAV.DAT',TYPE='NEW')
	  WRITE(101,*) MODELAV
 	  CLOSE(UNIT=101)
	  OPEN(UNIT=101,NAME='MODELSQ.DAT',TYPE='NEW')
	  WRITE(101,*) MODELSQ
 	  CLOSE(UNIT=101)
	  RETURN
	ENDIF
C
C	READ IN ACCUMULATED DATA IF THIS IS FIRST CALL
C
	IF(IREADIN.EQ.0) THEN
	  OPEN(UNIT=101,NAME='NMODEL.DAT',TYPE='OLD')
	  READ(101,*,END=11) NMODEL
 11	  CLOSE(UNIT=101)
	  OPEN(UNIT=101,NAME='MODELAV.DAT',TYPE='OLD')
	  READ(101,*,END=12) MODELAV
 12	  CLOSE(UNIT=101)
	  OPEN(UNIT=101,NAME='MODELSQ.DAT',TYPE='OLD')
	  READ(101,*,END=13) MODELSQ
 13	  CLOSE(UNIT=101)
	  IREADIN = 1
	ENDIF
C
C	ADD TO ACCUMULATED DATA
C
	DO N = 1,1024
C	  CALCULATE ANGLE INDEX OF EVENT SAMPLE, INDEX=ANGLE*(1098/360)+1
	  ANGLE = SUNANG + (360./TWOPI)*SPINRATE*(1. - N/1024.)/.33356
	  INDEX = ANGLE*(1098./360.) + 1.5
	  IF(INDEX.LE.0) INDEX = INDEX + 1098
	  INDEX = MOD(INDEX-1,1098)+1
C
	  TAV = NMODEL(INDEX)*MODELAV(INDEX) + VDATA(N) 
	  TSQ = NMODEL(INDEX)*MODELSQ(INDEX) + VDATA(N)**2
	  NMODEL(INDEX) = NMODEL(INDEX) + 1
	  MODELAV(INDEX) = TAV/NMODEL(INDEX)	
	  MODELSQ(INDEX) = TSQ/NMODEL(INDEX)	
	ENDDO
	RETURN
	END
