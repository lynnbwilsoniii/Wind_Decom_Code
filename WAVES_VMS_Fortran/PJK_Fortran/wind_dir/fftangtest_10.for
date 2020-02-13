! wind/waves fft data collection and shade plot program
! this program is to look at shot noise

	program		fftangtest
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
	integer*4	iq,i,j,k,n,iterm,nchgp,npt,np,nday
	integer*4	iw
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*80	stream
	parameter	size=1024
	integer*4	return_size
	integer*4	raw(size)
C	REAL		raw(size)
	integer*4	mantissa(size)
	integer*4	gain(size)
	integer*4	pwrdb(size)    ! actually, .5 db steps
	integer*4	fft_channel
	integer*4	temp_waves
	character*32	s
	real*8		scet8
	character*32	s_scet
	integer*4	scet(2)
	integer*4	ch, chhk, chcdf, major, minor,itmcorr,iraw
	character*80	file
	character*10	title(4)
	character*32	item
	integer*4	ios,yyyy,idoy,msec
	integer*4	NHRMN,IPNT,LAST,NPTMX,N1,N2,NFILL,NPPL
	REAL		PTIME,DELT,DELTS
	REAL		tstart,eltime,evtime,tend,tintv
	REAL		FFTSPACE(4),COUNT(4),SPACE
	real		vdata(1024),dbspec(513)
	real		nhist(100,10)
	real		phase,speriod
	integer*2	pwrl
	integer*2	pwrlt(1000,511)
!

	common /fftblk/ gain,mantissa,pwrdb,vdata
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,ISLOT(4),TEND
	COMMON /SHBLK/ PTIME(1000),PWRL(1000,511,4),ISRC(4),nlfb,NLF
	COMMON /TBLK/ PWRLF(1000,4)
	common /headblk/ major,minor,fft_channel,s_scet,title
!
	DATA ITMCORR /1/             ! 1 = CORRECT FOR TIME, 0 = PLOT
C					CONSECUTIVE SPECTRA
	DATA nlfb,NLF /40,80/
	DATA IPNT /4*0/
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1		'BX','BY','BZ'/

	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'

	ok = w_channel_open(ch,stream)
	if (.not. ok) stop 'cannot open tm channel'

	write(6,*) 'type hr,min to start,end, e.g. 0414,0800'
	read(5,*) nhrmn,last
	type*,nhrmn,last
	tstart = (nhrmn/100) + mod(nhrmn,100)/60.
	write(6,4)
	read(5,3) iq, nchgp
	type*,nchgp
 3	format(q,i10)
 4	format(1x,'enter 1 for hi channels, 2 for mid, 3 for low')
 5	format(q,a)
	do n = 1,4
	  title(n) = '          '
	  fftspace(n) = 0.
	  count(n) = 1.E-8
	enddo
 
c	ok = wind_tm_open_channel(chhk,stream)           !pjk
c	if (.not. ok) stop 'cannot open tm channel'      !pjk

c	ok = wind_tm_open_channel(chcdf,stream)          
c	if (.not. ok) stop 'cannot open tm channel'      !pjk

	ok = w_channel_filename(ch,file)
	print*,'file',file

	scet8 = 0.
	call w_channel_position(ch,scet8)
	nday = scet8
	scet8 = dfloat(nday) + ((nhrmn/100) + mod(nhrmn,100)/60.)/24. 
	type*,'set channel position to',scet8
	call w_channel_position(ch,scet8)
	type*,'channel position set to',scet8

c 	ok = wind_tm_get_mfmf(ch,major,minor)
c	if (.not. ok) stop 'cannot get stream position'

	get_set_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events()

c
	  event = 'FFTH'
	  if(nchgp.eq.2) event = 'FFTM'
	  if(nchgp.eq.3) event = 'FFTL'
	  ok = w_event(ch,event)

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, scet, 2, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	   type*,'first event at',scet,'  evtime',evtime

	   item = 'EVENT_SCET_R8'
	   ok = w_item_r8(ch, item, scet8, 1, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
c
	  go to 110

	! this is the main program loop

 100	   continue

c	  if( w_end_of_file(ch) ) then

           ok = w_event(ch,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
c	      if (wind_tm_realtime(ch)) then
c	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
c	         type *, '-reset to latest position: ', major, minor
c	      else
c	         call wind_tm_increment_packet(major,minor)
c	         type *, '-incrementing packet...'
c	      end if
	      err_count = err_count + 1
	      if (err_count .lt. 2) goto 100
	      if (err_count .ge. 2) goto 200
	   end if

 110	  if(wind_tm_eof(ch,major,minor)) then
		PRINT*,'END OF FILE'
		TYPE*,'GO TO PLOT AT ',S_SCET
		print*,'rawcount',rawcount
		print*,'fftcount',fftcount
	  	CALL SHPLOT(NCHGP)
		stop
	  endif

	   ! now get the items
	   item = 'wind_spin_phase_r4'
	   ok = w_item_r4(ch, item, phase, 1, return_size)

	   item = 'channel_number'
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	     iw = fft_channel
	     if(nchgp.eq.2) iw = fft_channel-2
	     if(nchgp.eq.3) iw = fft_channel-6
	   IF(TITLE(iw).EQ.'          ') THEN
	     item = 'SOURCE'
	     ok = w_item_i4(ch, item, IQ, 1, return_size)
	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	     WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(IQ)
	     ISRC(IW) = IQ
	   ENDIF
 1001	   FORMAT('CH',I2,'  ',A4)

C	   item = 'POWER_SPECTRUM'
C	   ok = w_item_i4(ch, item, raw, size, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

C	   item = 'POWER_SPECTRUM'
C 	   item = 'DATA'
C	   iraw = 0
C	   ok = w_item_i4(ch, item, raw, size, return_size)
C	   ok = w_item_R4(ch, item, raw, size, return_size)
C	print*,'power_spect',return_size,raw(1),raw(2)
C	   if (.not. ok) then
C	      iraw = 1
C	      type *, 'cannot get item ', item, ', ok=', ok
C	      type*,'raw after SCET, npt',scet,npt
C
C	      item = 'MANTISSA'
C	      ok = wind_tm_get_item(ch,item,mantissa, size, return_size)
C	      if (.not.ok) type *, 'cannot get item ', item, ', ok=', ok
C
C	      item = 'EXPONENT'
C	      ok = wind_tm_get_item(ch, item, gain, size, return_size)
C	      if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
C
C	      CALL WINDFFT(FFT_CHANNEL)
C
C	WINDFFT PLOTS WIND 'STRAIGHT THROUGH' FFT DATA AND DOES FFT
C
c
c	      do n = 1,return_size
c		raw(n) = pwrdb(n)
c	      enddo
C
C	IW is the number of windows in the shade plot, 2 for hi, 4 for mid,low
c
c	      do i = 1,iw		! to take care of a problem
c		space = amax1(space,fftspace(i))
c		fftspace(i) = space
c	      enddo
c
c	   endif
c
	CALL FFT_PHYS(ch,iraw,vdata,dbspec)

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, scet, 2, return_size)
	   item = 'EVENT_SCET_FRCTN'
	   ok = w_item_I4(ch, item, MSEC, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	   eltime = evtime - tstart

	print*,'time,channel,phase',scet,MSEC,fft_channel,phase
	write(45,45) scet,MSEC,fft_channel,phase
 45	FORMAT('time,channel,phase',2I10,I5,I5,F10.4)

c*************
	if(iraw.eq.0) then
		call w_messages_on(ch)
		fftcount = fftcount + 1.
	else
		call w_messages_off(ch)
		rawcount = rawcount + 1.
	endif
c
c
c	load histogram of intervals
c
	ihist = amin1((eltime - eltimes)*1800.,100.) + .5
	ihist = max0(ihist,1)
	nhist(ihist,fft_channel) = nhist(ihist,fft_channel) + 1
	eltimes = eltime
	
	if(ipnt(iw).gt.0) then
	   if(iraw.eq.0) then
		iq = ipnt(iw)
	  	fftspace(iw) = fftspace(iw) + evtime - ptime(iq) 
	     	count(iw) = count(iw) + 1.
	   else
		iq = ipnt(iw)
	  	fftspace(iw) = fftspace(iw) + .25*(evtime - ptime(iq))
	     	count(iw) = count(iw) + 1.
	   endif
	endif
c	
	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

c
	   NPT = IPNT(IW) + 1
	   IPNT(IW) = NPT
	   PTIME(NPT) = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
c	   type*,'npt,channel,ptime,scet(2)',npt,fft_channel,ptime(npt)
c     1     ,scet(2)
	   do i=1,511
		PWRL(NPT,I,IW) = dbspec(I+1)
	   end do
	   PWRLF(NPT,IW) = 0.
	   do i=nlfb,NLF
		PWRLF(NPT,IW) = PWRLF(NPT,IW) + 10.**(.1*dbspec(I+1))  
	   end do
	   pwrlf(npt,IW) = pwrlf(npt,IW)/(nlf-nlfb+1)
	   IF(NPT.GT.995) GO TO 200
	   IF(scet(2).LT.(100*LAST)) GO TO 100

 200	continue

	TYPE*,NPT,' SPECTRA FOUND, EACH CHANNEL'
	TYPE*,'GO TO PLOT AT ',S_SCET

c	do i = 1,100
c	  write(44,*) 2*i,nhist(i,1),nhist(i,3),nhist(i,7)
c	enddo
c	write(44,*) file
c
	IF(ITMCORR.EQ.0) THEN
	  print*,'rawcount',rawcount
	  print*,'fftcount',fftcount
	  CALL SHPLOT(NCHGP)
	  STOP
	ENDIF

	IW = 4
	IF(NCHGP.EQ.1) IW = 2
	TEND = 0.
	NPTMX = 0
	DO I = 1,IW
	  NPT = IPNT(I)
	  NPTMX = MAX0(NPT,NPTMX)
	  TEND = AMAX1(TEND,PTIME(NPT))
	ENDDO
	print*,'nptmx,ipnt',nptmx,ipnt

C
C	NPT IS NOW USED AS AN ESTIMATE OF THE NEXT PWRL TIME INDEX CORRESPONDING
C		TO THE EXACT TIME   PTIME(1) + (NP-1)*TINTV
C
c	NPT = 1
C
C	DO N = 1,NPTMX-1
C	PRINT*,N,PTIME(N),(PTIME(N)-PTIME(1))/TINTV
C	ENDDO

C
	do i = 1,iw		
		space = amax1(space,fftspace(i))
	enddo

	DO I = 1,IW
C	  COPY PWRL INTO PWRLT
	  DO NP = 1,NPTMX
	     DO J = 1,511
		PWRLT(NP,J) = PWRL(NP,J,I)
	     ENDDO
	  ENDDO
C
C	  FIND TINTV = AVERAGE SPACING OF FFT'S, NOT COUNTING RAW'S
	  TINTV = SPACE/COUNT(I)
C	  FIND NPPL = NUMBER OF PIXELS IN TIME DIRECTION
	  NPPL = (PTIME(NPTMX) - PTIME(1))/TINTV
	  PRINT*,'SPACE,NO OF SLICES,INT',SPACE,NPPL,TINTV
	  IF(NPPL.GT.997) THEN
	  print*,'************** warning, too many slices ********'
	       NPPL = 997
	       TINTV = (PTIME(NPTMX) - PTIME(1))/NPPL
	  ENDIF
C
C	  DO NP = 1,NPPL
	  NP = 0
	  NPT = 0
	  print*,'nptmx',nptmx
	  DOWHILE (NPT.LT.NPTMX)
	    NP = NP+1
	    ELTIME = PTIME(1) + (NP-1)*TINTV
C
	    IF(ABS(PTIME(NP)-ELTIME).LT..5*TINTV) THEN  
C		NO CORRECTION REQUIRED
	      NPT = NP+1
c	      TYPE*,'no corr.',NPT,' INTO SLOT',NP,' TIME',PTIME(NPT)
	      GO TO 210
	    ENDIF
C
C	    SEARCH THE RANGE NPT+-25 FOR NEAREST SPECTRUM
	    N1 = MAX0(NPT-25,1)
	    N2 = MIN0(NPT+25,NPTMX)
	    NFILL = 0
	    DELTS = 20.*TINTV
C	FIND NEAREST SPECTRUM
	    DO N = N1,N2
	      DELT = ABS(PTIME(N)-ELTIME)
	      IF(DELT.LT.DELTS) THEN  
		  DELTS = DELT
		  NFILL = NFILL+1
		  NPT = N
	      ENDIF
	    ENDDO

C
C	IF NEAREST SPECTRUM IS TOO FAR AWAY, LOAD ZERO'S, ELSE LOAD SPECTRUM
c	no, this turned out to be a bad idea
C	    IF(DELTS.GT.2.*TINTV) THEN
C		print*,'skipped a spectrum at',n1,n2
C		DO J = 1,511
C		  PWRL(NP,J,I) = 0.
C		ENDDO
C	    ELSE
C
C	LOAD SPECTRUM INTO PLOT ARRAY
C
c	TYPE*,'PUT NUMBER',NPT,' INTO SLOT',NP,' TIME',PTIME(NPT)
		DO J = 1,511
		  PWRL(NP,J,I) = PWRLT(NPT,J)
		ENDDO
C	    ENDIF
 210	    CONTINUE
 	    NPT = MIN0(NPT+1,NPTMX)
	    ISLOT(I) = NP
	  ENDDO
	ENDDO
C
C	(N_CHANNEL_GROUP)  NCHGP=1 IS HIGH, 2 IS MID, 3 IS LOW
	   NPT = NPTMX
	   CALL TPLOT(NCHGP)
	   CALL SHPLOT(NCHGP)
		print*,'fftcount',fftcount
		print*,'rawcount',rawcount
	   IF(1) STOP
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
	SUBROUTINE SHPLOT(NCHGP)
C
	character*32 scet
	integer*2 npwrl
	REAL*4 TEND
	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4),ISRC(4),nlfb,NLF
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,ISLOT(4),TEND
	common /headblk/ major,minor,ifftch,scet,title
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
	CHARACTER*10 TITLE(4)
	CHARACTER*120 STR
	REAL FUNDF(3)
	DIMENSION PWRL(511000)
	DATA FUNDF /.021348,.005337, .33356/
C
C
	NWNDOW = 2
	IF(NCHGP.NE.1) NWNDOW = 4
	ITERM = -1		! 370a, portrait
C	ITERM = -5		! 476a, portrait
	XSTART = 400.
	XEND = 2300.
	IW = 1
	NPT = ISLOT(IW)
	IF(NPT.LT.1) NPT = 1             	  ! protection
	PRINT*,' IN SHPLOT,NPT',NPT
	PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NPT),NPT
C	CALCULATE BOX SIZE
	PIXSIZ = .1
	IF(NPT.GT.1) PIXSIZ = (TEND - PTIME(1))/(NPT-1)
	HSTART = PTIME(1) - .5*PIXSIZ
	HEND =   PTIME(NPT) + .5*PIXSIZ
	HEND =   TEND + .5*PIXSIZ
	PRINT*,' IN SHPLOT',HSTART,HEND,NPT
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,280.,XEND,3100.)
	ENDIF
C
	DO IW = 1,NWNDOW
	  CALL MGOWINDOW(1,NWNDOW,IW)                          ! 
	  YMIN = 1.E6
	  YMAX = -YMIN
	  CALL HISTOG(2,TJUNK,256,-150.,0.,.01,TOTAL,RET)     ! CLEAR AND INIT
	  DO M = 1,NPT
	  DO N = 1,100
	    NM = M + (N-1)*NPT
	    PWRL(NM) = NPWRL(M,N,IW)
	    CALL HISTOG(1,PWRL(NM),256,-150.,0.,.5,TOTAL,RET)    ! LOAD ARRAY
C
	    YMIN = AMIN1(YMIN,PWRL(NM))
	    YMAX = AMAX1(YMAX,PWRL(NM))
	  ENDDO
	  ENDDO
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C
	  CALL HISTOG(0,TJUNK,256,-150.,0.,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE
	  CALL HISTOG(0,TJUNK,256,-150.,0.,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER
C	  LOWERING YMAX MAKES THE SIGNAL DARKER

	  YAMIN = YHMIN
	  YAMAX = YHMAX
	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX
c	  arguments are: array(m,n),m,n,white,black,linearity
c	  m is the x direction
	  CALL MGOHALFTONE(PWRL,NPT,100,YAMIN,YAMAX,1.E7)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOSETLIM(HSTART,FUNDF(NCHGP),HEND,100.*FUNDF(NCHGP))
	  IF(NCHGP.EQ.3) THEN
	    CALL MGOYLABEL(13,'FREQUENCY, Hz')	
	  ELSE
	    CALL MGOYLABEL(14,'FREQUENCY, kHz')	
	  ENDIF
	  CALL MGOBOX(1,2)
	  CALL MGOGRELOCATE(GX1,GY2)
	  CALL MGOPUTLABEL(10,TITLE(IW),9)
	print*,'title in shplot  ',title(iw)
C
	  WRITE(STR,704) .5*YAMIN,.5*YAMAX
 704	  FORMAT('WHITE,BLACK ',2F6.1,' DB ')
	  CALL MGOSETANGLE(90.)
	  CALL MGOSETEXPAND(.5)
	  XPR = GX2 + .005*(GX2-GX1)
	  YPR = .5*(GY1+GY2)
	  CALL MGOGRELOCATE(XPR,YPR)
	  CALL MGOPUTLABEL(28,STR,2)                  
	  CALL MGOSETANGLE(0.)
	  CALL MGOSETEXPAND(1.)
	  IF(IW.EQ.1) THEN
            WRITE(STR,1001) SCET(1:10),IDOY
 1001	    FORMAT('HOURS OF ',A10,' DOY ',I3)
	    CALL MGOSETEXPAND(.8)
            CALL MGOXLABEL(28,STR)	
	    CALL MGOSETEXPAND(.5)
	    IF(ITMCORR.EQ.0) THEN
		CALL MGOPUTLABEL(23,'    consecutive spectra',6)
	    ELSE
		CALL MGOPUTLABEL(18,'    corrected time',6)
	    ENDIF
	    CALL MGOSETEXPAND(1.)
	  ENDIF
C
	  IF(IW.EQ.NWNDOW) THEN
	    CALL MGOSETEXPAND(.8)
	    CALL MGOPLOTID('FFTNOISE','SHPLOT')
	    CALL MGOSETEXPAND(1.)
	  ENDIF
	ENDDO
C
	  CALL MGOSETEXPAND(1.)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
	END
	SUBROUTINE SPPLOT
C
C	PLOTS THE SPECTRUM
C
	character*32 scet
	character*10	titlex(4)
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,ISLOT(4),TEND
	common /headblk/ major,minor,ifftch,scet,titlex
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
	DIMENSION XX(1024),YY(512),FREQ(512)
	CHARACTER*120 STRG(5)
	CHARACTER*120 STR
	CHARACTER*8 TITLE(4)
	DATA IWINDW /1/
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/
C
c	PRINT*,' IN SPPLOT',NPT
	FFUND = 20.3477
	IF(IFFTCH.GT.2) FFUND = .25*FFUND
	IF(IFFTCH.GT.6) FFUND = .333557
	DO JF = 1,512
	  FREQ(JF) = JF*FFUND
	ENDDO
C
C	ITERM = -5
C	ITERM = 3
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C	PRINT*,'START',GX1,GX2,GY1,GY2
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,330.,2200.,3000.)
	ENDIF
C
	  CALL MGOSETEXPAND(.6)
	  TOP = LY2
	  CALL MGOGRELOCATE(GX1,TOP)                    
	  CALL MGOPUTLABEL(6,'SCET   ',3)
	  CALL MGOPUTLABEL(79,SCET,3)
	  CALL MGOPUTLABEL(20,'   major,minor frame ',3)
	  WRITE(STR,801) MAJOR,MINOR
 801	  FORMAT(I6,'.',I3.3)
	  CALL MGOPUTLABEL(14,STR,3)
	  CALL MGOGRELOCATE(GX1,TOP-50.)        
	  CALL MGOPUTLABEL(12,' FFT CHANNEL',3)
	  WRITE(STR,802) IFFTCH
 802	  FORMAT(I6)
	  CALL MGOPUTLABEL(8,STR,3)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                    
	  ELSE
	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy
	  ENDIF
	  CALL MGOSETEXPAND(1.)
C
C	DO JW = 1,4
C	  CALL MGOWINDOW(1,4,JW)                          ! 
C	  PRINT*,'WINDOW',GX1,GX2,GY1,GY2
C	  IF(JW.EQ.4) GYTOP = GY2
C	ENDDO
C
	  XMIN = ALOG10(FREQ(1))
	  XMAX = ALOG10(FREQ(511))
	  RANGE = ABS(XMAX-XMIN)
	  XMAX = XMAX + .02*RANGE
	  XMIN = XMIN - .02*RANGE	
C
	  CALL MGOWINDOW(1,2,1)                          ! NO WINDOW
	  YMIN = ALOG10(PWRNW(1))
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
	  DO J = 1,511
	    JP = J
	    YY(JP) = ALOG10(PWRNW(J))
	    XX(JP) = ALOG10(FREQ(J))
	    PWRTOT = PWRTOT + PWRNW(J)
	    IF(YY(JP).GT.YMAX) THEN
	      YMAX = AMAX1(YY(JP),YMAX)
	      JMAX = JP
	    ENDIF
	    YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
	PRINT*,'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	WRITE(16,*)'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  PKPWR = PWRNW(JMAX)
	  IF((JMAX-1).GT.0) PKPWR = PKPWR + PWRNW(JMAX-1)
	  IF((JMAX+1).LT.512) PKPWR = PKPWR + PWRNW(JMAX+1)
	  AMP = SQRT(PKPWR)
	  WRITE(16,803) PKPWR,FREQ(JMAX),AMP
C	  WRITE(19,803) PKPWR,FREQ(JMAX),AMP
	  PRINT 803,PKPWR,FREQ(JMAX),AMP
 803	FORMAT(' 3PK PWR',E12.4,' AT',F8.2,' HZ, AMP=',E12.4)
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOTICKSIZE(-1.,0.,-1.,0.)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(XX,YY,JP)
	  CALL MGOBOX(1,2)
	  CALL MGOXLABEL(9,'FREQ (HZ)')
	  CALL MGOYLABEL(12,'V**2/CHANNEL')
	  CALL MGOSETEXPAND(.6)
	  CALL MGOPLOTID('WINDOW','RECT.')
	  CALL MGOSETEXPAND(1.)
C
	  CALL MGOWINDOW(1,2,2)                          ! WITH WINDOW
	  YMIN = ALOG10(PWRW(1))
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
	  DO J = 1,511
	    JP = J
	    YY(JP) = ALOG10(PWRW(J))
	    XX(JP) = ALOG10(FREQ(J))
	    PWRTOT = PWRTOT + PWRW(J)
	    IF(YY(JP).GT.YMAX) THEN
	      YMAX = AMAX1(YY(JP),YMAX)
	      JMAX = JP
	    ENDIF
	    YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  PRINT*,'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  WRITE(16,*)'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  PKPWRW = PWRW(JMAX)
	  IF((JMAX-1).GT.0) PKPWRW = PKPWRW + PWRW(JMAX-1)
	  IF((JMAX+1).LT.512) PKPWRW = PKPWRW + PWRW(JMAX+1)
	  AMPW = SQRT(PKPWRW)
	  WRITE(16,803) PKPWRW,FREQ(JMAX),AMPW
	  PRINT 803,PKPWR,FREQ(JMAX),AMPW
C
	  CALL MGOTICKSIZE(-1.,0.,-1.,0.)
C
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(XX,YY,JP)
	  CALL MGOBOX(1,2)
	  CALL MGOYLABEL(12,'V**2/CHANNEL')
C	  WRITE(TITLE(1),705) IWINDW
C 705	  FORMAT(I2)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOPLOTID('WINDOW',TITLE(IWINDW+1))
	  CALL MGOSETEXPAND(1.)
C
C	  CALL MGOWINDOW(1,4,3)                          ! FAKE DATA
C	  CALL MGOSETLOC(GX1,GY1,GX2,GYTOP)
C	  CALL MGOTICKSIZE(0.,0.,128.,2048.)
C	  CALL MGOSETLIM(-10.,-8192.,1035.,8192.)
C	  EXPSV = EXPAND
C	  CALL MGOSETEXPAND(.4)
C	  DO N = 1,1024
C	    XX(N) = N
C	    CALL MGORELOCATE(XX(N),FDATA(N))
C	    CALL MGOPOINT(4,1)
C	  ENDDO
C	  CALL MGOSETEXPAND(EXPSV)
C	  CALL MGOBOX(1,2)
C	  CALL MGOSETLTYPE(2)
C	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(0)
C	  CALL MGOXLABEL(10,'SAMPLE NO.')
C	  CALL MGOYLABEL(16,'DATA & GAIN STEP')
C
	CALL MGOSETEXPAND(.8)
C	CALL MGOPLOTID('WINDFFT','SPPLOT')
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
C	  CALL MGOTIDLE
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END
	SUBROUTINE TPLOT(NCHGP)
C
C	PLOTS THE TIME DOMAIN DATA
C
	COMMON /SHBLK/ PTIME(1000),PWRL(1000,511,4),ISRC(4),nlfb,NLF
	COMMON /TBLK/ PWRLF(1000,4)
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,ISLOT(4),TEND
	common /headblk/ major,minor,ifftch,scet,titlex
	character*32 scet
	character*10	titlex(4)
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
	integer*2 pwrl
	DIMENSION YY(1024)
	CHARACTER*120 STRG(5)
	CHARACTER*120 STR
C
	PRINT*,' IN TPLOT,IPNT,TERM',IPNT,iterm
C
c	ITERM = 3
	ITERM = -1		! 370a, portrait
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  PRINT*,'START TPLOT',GX1,GX2,GY1,GY2
	  CALL MGOSETLOC(400.,280.,2300.,3100.)
	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-200.)
	ENDIF
C
	FFUND = 20.3477
	IF(IFFTCH.GT.2) FFUND = .25*FFUND
	IF(IFFTCH.GT.6) FFUND = .333557
	  IWM = 4
	  IF(NCHGP.EQ.1) IWM = 2
	DO IW = 1,IWM
	  CALL MGOWINDOW(1,IWM,IW)
	  NPT = IPNT(IW)
	  CALL MGOSETEXPAND(.8)
	  TOP = LY2
cold	  CALL MGOGRELOCATE(GX1,TOP-50.)                    
	  CALL MGOGRELOCATE(GX1,TOP-100.)                    
	  CALL MGOPUTLABEL(6,'SCET   ',3)
	  CALL MGOPUTLABEL(79,SCET,3)
 802	  FORMAT(2F7.1)
c	  CALL MGOGRELOCATE(GX1,TOP-160.)        
	  CALL MGOPUTLABEL(11,'freq range',3)
	  F1 = NLFB*FFUND
	  F2 = NLF*FFUND
	  WRITE(STR,802) F1,F2
	TYPE*,'FREQ',F1,F2,FFUND
	  CALL MGOPUTLABEL(14,STR,3)
	  CALL MGOSETEXPAND(1.)
C
	  XMIN = PTIME(1)
	  XMAX = PTIME(NPT)
	  print*,'time in tplot',xmin,xmax
C
	  YMIN = 1000.
	  YMAX = -YMIN
	  JP = 1
	  DO J = 1,NPT
	    JP = J
	      YY(JP) = 10.*ALOG10(PWRLF(J,IW))
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
c	  CALL MGOTICKSIZE(20.,100.,0.,0.)
	  CALL MGOTICKSIZE(.1667,1.,1.,10.)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PTIME,YY,JP)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOBOX(1,2)
	  CALL MGOGRELOCATE(GX1,GY2)
	  CALL MGOPUTLABEL(10,TITLEX(IW),9)
	print*,'title in tplot  ',titlex(iw)
	  IF(IW.EQ.1) THEN
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
            WRITE(STR,1001) SCET(1:10),IDOY
 1001	    FORMAT('HOURS OF ',A10,' DOY ',I3)
	    CALL MGOSETEXPAND(.7)
            CALL MGOXLABEL(28,STR)	
	  ENDIF
	  IF(ISRC(IW).LT.7) THEN
		CALL MGOYLABEL(18,'dB V**2/HZ ')
	  ELSE
		CALL MGOYLABEL(18,'dB nT**2/HZ ')
	  ENDIF
	  CALL MGOSETEXPAND(1.)
C
	ENDDO
	CALL MGOSETEXPAND(.7)
	CALL MGOPLOTID('[kellogg.wind]','FFTNOISE')
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
	common /headblk/ major,minor,ifftch,scet,title
	character*32 scet
	character*10	title(4)
C
	write(36,*) 'fft data collection at ',scet,' channel',ifftch
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
	SUBROUTINE HISTOG(ILOAD,X,NBIN,XMIN,XMAX,PCTILE,TOTAL,RET)
C
C	THIS IS A SPECIAL VERSION TO CALCULATE LEVEL AT A CERTAIN PERCENTILE
C
C	INTEGER*4 NARR
	DIMENSION NARR(256)
	DATA NARR /256*0/
C
	IF(ILOAD.GT.1) THEN
	  DO N = 1,256
	    NARR(N) = 0
	  ENDDO
	  TOTAL = 0.
	  RETURN
	ENDIF
C
	IF(ILOAD.EQ.1) THEN
	  XT = AMAX1(X,XMIN)
	  XT = AMIN1(XT,XMAX)
	  IBIN = (NBIN-1)*(XT-XMIN)/(XMAX-XMIN) + 1
	  NARR(IBIN) = NARR(IBIN)+1
	  TOTAL = TOTAL + 1.
	  RETURN
	ENDIF
C
	IF(PCTILE.GT..5) THEN
C	  PRINT OUT RESULTS
C    	  PRINT*,' TOTAL NUMBER, NBIN',TOTAL,NBIN
C	  PRINT*,'XMIN,XMAX',XMIN,XMAX
c	  PRINT*,'HISTOGRAM'
c	  PRINT 1111, (NARR(J),J=1,NBIN)
 1111	  FORMAT(10I7)
	ENDIF
C
C	NOW CALCULATE VALUE OF X CORRESPONDING TO PCTILE PERCENTILE
C
C	PCTILE = .2
	SET = PCTILE*TOTAL
	SUM = 0.
	N = 0
  100	N = N+1
	  SUM = SUM + NARR(N)
	  IF(SUM.LT.SET) GO TO 100
C
C	NOW THE DESIRED VALUE LIES IN BIN N
C
	SUMLOW = SUM - NARR(N)
	XN = N
	RETLOW = XMIN + ((XN-1.)/(NBIN-1.))*(XMAX-XMIN)
	RETHI = XMIN +       (XN/(NBIN-1.))*(XMAX-XMIN)
	RET = RETLOW + (SET-SUMLOW)*(RETHI-RETLOW)/NARR(N)
C	PRINT*,'N,SET,SUMLOW,LOW,RET',N,SET,SUMLOW,RET
	RETURN
	END

