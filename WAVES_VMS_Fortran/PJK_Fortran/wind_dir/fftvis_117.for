	program		fft_VIS

!	wind/waves fft data collection and plot program

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
	integer*4	ok,okt
	character*4	event,pa(9)
	integer*4	iq,i,j,k,n,iterm,nchgp,npt,np,nday,isrc
	integer*4	iw
	REAL		XSPEC(1024,5)
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
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
	integer*4	ch, chhk, major, minor,itmcorr,iraw
	character*80	file
	character*10	title(15)
	character*32	item
	integer*4	ios,yyyy,idoy,msec
	integer*4	NHRMN,LAST,NPTMX,N1,N2,NFILL,NPPL
	REAL		PTIME,DELT,DELTS,DPUTIME,PHASE,SPINRATE
	REAL		tstart,eltime,evtime,tend,tintv
	REAL		FFTSPACE(4),COUNT(4),SPACE
	real		nhist(100,10),vdata(1024),dbspec(1024)
	real		ddata(1024)
	integer*2	pwrl
	integer*2	pwrlt(1000,511)
	integer*4 	ncount
!

	common /fftblk/ gain,mantissa,pwrdb
	COMMON /PLTPAR/ ITERM,NDAY,ITMCORR,IDOY
	COMMON /SHBLK/ PTIME(1024)
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),VDATA
	common /headblk/ major,minor,fft_channel,s_scet,title,scet8
!
	DATA ITMCORR /0/             ! 1 = CORRECT FOR TIME, 0 = PLOT
C					CONSECUTIVE SPECTRA
	data ncount /0/
	DATA JP /0/
	DATA IRAW /0/
	DATA TWOPI /6.2831853/
	DATA NTPLOTS /0/
C
C	    type*,'type name of data file, eg: makefile20.results'
C	    read(5,1002) eventfile
C	    open(unit=44,file=eventfile,status='old',readonly)
c	    read(44,1044) scetr,nevtr,evtr,junk
C		FOR FOR070.DAT, RESULTS OF MAKEFILE
C	    read(44,1044) scetr,nevtr,ntmday,evtr,junk
C		FOR MAKEFILE18.RESULTS
C	READ(44,1111,END=200) SCETR,NEVTR,EVTR,ISPS,IZCNT,FREQHZ
C     1	 ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C	1111 IS NOW 114 CHARACTERS
 1111	FORMAT(I10,I8,I10,A2,I2,I4,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1		,F6.3)
C
C		FOR MAKEFILE24.RESULTS, AND FOR058 FROM PROCMK18
C
C	READ(44,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR
C	READ(56,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR,ISPS,IZCNT,FREQHZ
C     1	 ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C	1114 IS NOW 113 CHARACTERS
 1114	FORMAT(I10,I7,I10,I3,A2,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1		,F6.3)

C
	ok = get_stream_name(stream)
	if (ok.ne.1) stop 'no file supplied.'

	ok = w_channel_open(ch,stream)
	if (.not. ok) stop 'cannot open tm channel'

	write(6,*) 'type hr,min to start,end, e.g. 0414,0800'
	read(5,*) nhrmn,last
	type*,nhrmn,last
	tstart = (nhrmn/100) + mod(nhrmn,100)/60.
	tlast = (last/100) + mod(last,100)/60.
	write(6,4)
	write(6,6)
	read(5,3) iq, main_ch
	nchgp = 1
	if(main_ch.gt.2) nchgp = 2
	if(main_ch.gt.6) nchgp = 3
	type*,'channel to plot and group(hi,mid,lo)',main_ch,nchgp
 3	format(q,i10)
 4	format(1x,'enter channel number for first plot')
 6	format(1x,'(other channels at same speed will be saved)')
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
	
c 	ok = wind_tm_get_mfmf(ch,major,minor)
c	if (.not. ok) stop 'cannot get stream position'

	get_set_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events()
c
	  event = 'FFT'
	  if(nchgp.eq.1) event = 'FFTH'
	  if(nchgp.eq.1) iw = 1
	  if(nchgp.eq.2) event = 'FFTM'
	  if(nchgp.eq.2) iw = 3
	  if(nchgp.eq.3) event = 'FFTL'
	  if(nchgp.eq.3) iw = 7
c
c	  find the first event in a set of channels, for time
c
 120	  ok = w_event(ch,event)
	  if(ok.eq.82) then
		PRINT*,'END OF FILE'
		print*,'rawcount',rawcount
		print*,'fftcount',fftcount
		stop
	  endif
	  item = 'channel_number'
	  okt = w_item_i4(ch, item, fft_channel, 1, return_size)
	  if(fft_channel.ne.iw) go to 120
c************test of spin phase
C	phsav = 0.
C	do ix = 1,100
C	   call w_channel_position(ch,scetlast)
	   item = 'WIND_SPIN_PHASE_R4'
	   ok = w_item_r4(ch, item, PHASE, 1, return_size)
	   item = 'WIND_SPIN_scet_R8'
C	   ok = w_item_r8(ch, item, scetspin, 1, return_size)
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
C
C	THIS IS TO ACCOMODATE THE EARLY FLIGHT SOFTWARD, WHICH HAD ONLY
C	FFT AS AN EVENT, AND WAS NOT SEPARATED INTO H,M AND L
C
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

c	  if( w_end_of_file(ch) ) then

           ok = w_event(ch,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
	      err_count = err_count + 1
	      if (err_count .lt. 2) goto 100
	      if (err_count .ge. 2) stop 'too many errors'
	   end if

 110	  CONTINUE	  
C
	   ! now get the items

	   item = 'channel_number'
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)
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
c	   if(scet8.gt.scetlast) stop
	   if(scet8.gt.scetlast) go to 200
c
	   item = 'DPU_MAJOR_FRAME'
	   ok = w_item_I4(ch, item, MAJOR, 1, return_size)
	   item = 'DPU_MINOR_FRAME'
	   ok = w_item_I4(ch, item, MINOR, 1, return_size)
C
	   item = 'SOURCE'
	   ok = w_item_i4(ch, item, ISRC, 1, return_size)
	
           item = 'DPU_CLOCK'
           ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
c           WRITE(PTITLE(14),1014) DPUCLK
	   print*,'source,dpuclock',isrc,dpuclk
 1014      FORMAT(F12.3) 
C
	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
c	calculate spin phase at start of event
c
c	   tdiff = 86400.D00*(scet8 - scetspin)
c	   phdiff = tdiff*spinrate
c	   if(phdiff.lt.0.) then
c		phase = phase + phdiff
c	        dowhile (phase.lt.0.)
c		  phase = phase+twopi
C		  print*,'phase',phase
c		enddo
c		phase = amod(phase,twopi)
c	   else
c		phase = amod(phase + phdiff,twopi)
c	   endif
c
c******************
c	     if(newold.eq.1.and.fft_channel.lt.7) go to 100
C	     if(fft_channel.NE.3) go to 100
	     iw = fft_channel-6
	     if(nchgp.eq.1) iw = fft_channel
	     if(nchgp.eq.2) iw = fft_channel-2
	     if(nchgp.eq.3) iw = fft_channel-6
	     iw = max0(iw,1)
	     iw = min0(iw,4)
c	   IF(TITLE(iw).EQ.'          ') THEN
c	     WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(ISRC)
c	   ENDIF
	   WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(ISRC)
 1001	   FORMAT('CH',I2,'  ',A4)

C
C	TYPE*,'CALL FFT_PHYS'
	call fft_phys(ch,iraw,vdata,dbspec)
	PRINT*,'FFT CHANNEL,IRAW',FFT_CHANNEL,IRAW,VDATA(3)
c	IF(FFT_CHANNEL.EQ.7) THEN
	IF(ISRC.EQ.4) THEN
c	  open(unit=67,name='lowEZ.data',status='new')
c	  write(67,*) fft_channel,scet
c	  DO N = 1,1024	
c		DDATA(N) = 1000.*VDATA(N)
c	  ENDDO
C 	  write(67,4455) DDATA
c	  close(unit=67)
	  CALL FFTLOANGLE(CH,VDATA,SUNANGLE,ANTANG)
C	  WRITE(67,*) SUNANGLE	  
	  PRINT*,'TEMP. SUNANGLE', SUNANGLE	  
 4455	format(12(1x,f6.3))
C	  IF(1) STOP
	ENDIF
c
C	TYPE*,'GET ITEMS SPECTRUM_DB AND VOLTS/METER'
C	   item = 'SPECTRUM_DB'
C	   ok = w_item_R4(ch, item, DBSPEC, 1024, return_size)
C	   item = 'volts/meter'
C	   ok = w_item_R4(ch, item, VDATA, 1024, return_size)
c****
c
	NTPLOTS = NTPLOTS+1
	IF(NTPLOTS.LE.80) THEN
	  print*,'iraw = ',iraw
  	  type*,'tdplot called'
	  call tdplot(ch,isrc,iraw)
	ENDIF
C
	Jp = jp+1
c	if(j.gt.1) stop
C
C	CALCULATE TOTAL POWER
C
	      TPOWER = 0.
	      do n = 128,512
		TPOWER = TPOWER + EXP(.23026*DBSPEC(N))
	      enddo
	      TPOWERDB = 10.*ALOG10(TPOWER)
C
	      do i = 1,iw		! to take care of a problem
		space = amax1(space,fftspace(i))
		fftspace(i) = space
	      enddo

c	   endif

	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	   eltime = evtime - tstart
C
 1002	FORMAT(2I8,I4,F9.3,E12.3)
C	
	if(iraw.eq.0) then
		fftcount = fftcount + 1.
	else
		rawcount = rawcount + 1.
	endif
c
	   NPT = NPT+1
	   PTIME(NPT) = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
c	type*,'npt,channel,ptime,scet(2)',npt,fft_channel,ptime(npt)
c     1     ,scet(2)
c
	   IF(NPT.GT.995) GO TO 200
	   IF(scet(2).LT.(100*LAST)) GO TO 100

 200	continue

	TYPE*,NPT,' SPECTRA FOUND, EACH CHANNEL'
	IF(NPT.EQ.0) GO TO 100 
	TYPE*,'GO TO PLOT AT ',S_SCET
C
	IW = 4
	IF(NCHGP.EQ.1) IW = 2
	TEND = 0.
C
C	(N_CHANNEL_GROUP)  NCHGP=1 IS HIGH, 2 IS MID, 3 IS LOW
C
C	call rawprint
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
	CHARACTER*80	YYYYMMDD
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	integer*4	iq,NREM,NHRMN,IFASTSLOW

10	  write(6,6)
	  write(6,7)
	  read(5,5,err=10,end=20) iq, stream
	print*,'in get_, initial stream=  ',stream
 	if (iq .lt. 1) then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else if (stream(1:1) .eq. 'w' .or. stream(1:1) .eq. 'W') then
	   ! assume the user entered the name of an offline file
	else
	   ! assume the user entered the TIME of an offline file
	   YYYYMMDD = STREAM(1:8)
	   WRITE(STREAM,30) YYYYMMDD
 30	   FORMAT('wi_lz_wav_',A8,'_v*.dat')
	   PRINT*,'in get_stream_name, file= ',STREAM
	end if

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
  7	format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
	end
	SUBROUTINE TDPLOT(ICH,ISRC,IRAW)         
C         
C	PLOTS THE TIME DOMAIN DATA, THE GAIN STEP, AND THE SPECTRUM         
C		note that ich is the stream channel, not fft_channel         
C         
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)         
	COMMON /PLTPAR/ ITERM,NDAY,ITMCORR,IDOY         
	common /headblk/ major,minor,IFFTCH,scet,titlex,scet8         
	COMMON /RAWVOLTS/ RDATA(1026),RAWAVR,SLOPE
	common /angles/iantang,antang,ezang,bzantang         
	character*32 scet,item         
	character*10	titlex(15)         
	integer*4 return_size,ok,w_item_r4         
	real*8 scet8         
	real*4 gain(1024),FREQ(512)         
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
	DIMENSION PTIME(1024),ATIME(1024),STIME(1024),XX(1024),YY(1024)         
	CHARACTER*8 TITLE(4)         
	CHARACTER*120 STRG(5)         
	CHARACTER*120 STR
	CHARACTER*4 PA(9)         
	DATA TWOPI /6.2831853/         
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/  
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1		'BX','BY','BZ'/       
C         
	IFCORR = 1			! CORRECTED FOR FREQ. RESPONSE         
C         
	ITERM = 3         
c	ITERM = 2         
c	ITERM = -1        ! printer 
C       
C*************
	IWINDW = 1  
	PRINT*,' IN tdplot, stream chann,fft chan,source=',ich,ifftch,isrc    
	print*,'tdplot, headblk=',major,minor,ifftch,isrc         
	FFUND = 20.3477         
	IF(IFFTCH.GT.2) FFUND = .25*FFUND         
	IF(IFFTCH.GT.6) FFUND = .333557     
	DO JF = 1,512
	  FREQ(JF) = JF*FFUND
	ENDDO    
	DO JF = 1,1024         
	  PTIME(JF) = (JF-1)/1.024/FFUND		!XAXIS IN mSEC         
	ENDDO
	print*,'got to xrange'         
	XRANGE = PTIME(1024) - PTIME(1)         
	XMIN = PTIME(1) - .022*XRANGE         
	XMAX = PTIME(1024) + .022*XRANGE 
	print*,'orig xmin,max=',xmin,xmax        
	  print*,'check scet8',scet8
C
 100	CONTINUE         
	print*,'going to mgoinit,iterm= ',iterm
	  CALL MGOINIT         
	  CALL MGOSETUP(ITERM)         
	  CALL MGOERASE 
	  GY1SV = GY1
	  GY2SV = GY2        
	  GYRANGE = GY2-GY1
	  GXRANGE = GX2-GX1
	  CALL MGOGRELOCATE(GX1-.02*GXRANGE,GY2)
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOPUTLABEL(6,'SCET  ',3)
	  CALL MGOPUTLABEL(22,SCET,3)                   
	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3) 	     
	  WRITE(STR,802) IFFTCH         
	  CALL MGOPUTLABEL(4,STR,3)                  
	  CALL MGOPUTLABEL(6,PA(ISRC),3)         ! SOURCE IN CHARACTERS      
	  CALL MGOPUTLABEL(9,'  UR8 DAY',3)
	  WRITE(STR,805) SCET8
	  print*,'check scet8',scet8
 805	  FORMAT(F10.3)
	  CALL MGOPUTLABEL(11,STR,3)
	IF(ITERM.LT.0) THEN         
	  PRINT*,'START tdplot, printer',GX1,GX2,GY1,GY2                  
	  CALL MGOSETLOC(GX1,GY2SV-.43*GYRANGE,GX2,GY2SV-.04*GYRANGE)         
	ELSE         
	  print*,'got to setloc'
	  CALL MGOSETLOC(GX1,GY2SV-.43*GYRANGE,GX2,GY2SV-.04*GYRANGE)         
	ENDIF         
C         
	  CALL MGOSETEXPAND(.8)         
	  TOP = LY2 
	print*,'top',top
	print*,'major,minor',major,minor        
C	  CALL MGOGRELOCATE(GX1,TOP)   
c	  WRITE(STR,801) MAJOR,MINOR         
 801	  FORMAT('MF.mf ',I7,'.',I3.3)         
c 	  FORMAT(2I6)         
c	  CALL MGOPUTLABEL(17,STR,3)         
c	  CALL MGOPUTLABEL(12,'  R8 time ',3)        
c	  WRITE(STR,803) SCET8         
 803	  FORMAT(F14.8)         
c	  CALL MGOPUTLABEL(14,STR,3)                 
C	  CALL MGOPUTLABEL(12,' MAX,MIN,AVR',3)
	  GXSAVE = GX
	  GYSAVE = GY         
C
	  IF(IRAW.EQ.0) THEN
	    CALL MGOGRELOCATE(GX1,.8*GY2SV+.2*GY1SV)
	    CALL MGOLABEL(27,'NO RAW DATA FOR TIME SERIES',3)
	    GO TO 210
	  ENDIF
C
	  IF(ISRC.LE.6) THEN         
	   item = 'E_VOLTS/METER'         
	  ELSE         
	   item ='B_NT2/HZ'         
	  ENDIF         
c	  ok = w_item_R4(ICH, item, FDATA, 1024, return_size)         
c	  print*,'in tdplot, item,return_size',item,return_size         
C	  if(ok.eq.0) then         
C	     print*,'ok = 0 in tdplot, no raw data, no item ',item         
C	     return         
C	  endif         
c	  YMIN = FDATA(1)         
	  YMIN = 1.E6         
	  YMAX = -YMIN         
	  JP = 1         
	print*,'got to ifcorr =',ifcorr
	IF(IFCORR.NE.1) THEN    
	  AVR = 0.     
	  DO J = 1,1024         
	    JP = J         
C
c		rdata, called data in fft_phys, is just t/m numbers         
c			multiplied by (volts per step/eff length)         
	    YY(JP) = RDATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)
	    AVR = AVR+YY(JP)         
	    PTIME(JP) = (JP-1)/1.024/FFUND		!XAXIS IN mSEC         
	  ENDDO         
	ELSE         
	  AVR = 0.
	  DO J = 1,1024         
	    JP = J         
c		data, called vdata in fft_phys, is volts/meter         
c			corrected for frequency response         
	    YY(JP) = DATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)         
	    AVR = AVR+YY(JP)         
	    PTIME(JP) = (JP-1)/1.024/FFUND		!XAXIS IN mSEC         
	  ENDDO         
	ENDIF         
	AVR = AVR/1024.
C         
	  RANGE = ABS(YMAX-YMIN)         
	  YMAX = YMAX + .05*RANGE         
	  YMIN = YMIN - .05*RANGE	         
	  IF(IFFTCH.LE.2) THEN         
	    CALL MGOTICKSIZE(5.,20.,0.,0.)         
	  ELSEIF(IFFTCH.LE.6) THEN         
	    CALL MGOTICKSIZE(10.,50.,0.,0.)         
	  ELSE         
	    CALL MGOTICKSIZE(100.,500.,0.,0.)         
	  ENDIF         
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)         
	print*,'whats wrong'
	print*,'fund freq',ffund
	print*,'x',xmin,xmax,jp
	print*,'ptime',ptime(1),ptime(2),ptime(1024)
	  CALL MGOCONNECT(PTIME,YY,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOXLABEL(4,'mSEC')         
	  CALL MGOYLABEL(18,'V/M OR nT')         
	  XTITLE = .5*(XMAX+XMIN)         
	  YTITLE = YMIN + .9*(YMAX-YMIN)         
	  CALL MGORELOCATE(XTITLE,YTITLE)         
C	  IF(IFCORR.EQ.1) THEN         
C	    CALL MGOLABEL(28,'CORRECTED FOR FREQ. RESPONSE')         
C	  ELSE         
C	    CALL MGOLABEL(32,'NOT CORRECTED FOR FREQ. RESPONSE')         
C	  ENDIF         
	  CALL MGOSETEXPAND(1.)         
C         
	CALL MGOSETEXPAND(.7)         
	CALL MGOPLOTID('[kellogg.wind]FFTVIS','TDPLOT')         
	CALL MGOSETEXPAND(1.)         
	print*,'finished time series'
C         
C	PLOT GAIN STEP         
C         
c	IF(IFFTCH.GT.6.AND.ISRC.EQ.4) THEN         
	  ITEM = 'WIND_SPIN_RATE_R4'         
	  OK = W_ITEM_R4(ICH,ITEM,SPINRATE,1,ISIZE)         
	  SAMP_PER_SEC = 1024.*FFUND         
	  DANG = SPINRATE/SAMP_PER_SEC			! IN RADIANS         
	  DANGD = DANG*360./TWOPI         
	  SAMP_PER_SPIN = TWOPI/DANG         
	  NSPPS = SAMP_PER_SPIN + .5			! ROUNDOFF         
c	  CALL FFTLOANGLE(ICH,DATA,SUNCLOCK,ANTANG)         
c	  PRINT*,'FFTLOANGLE CALLED FROM tdplot'         
c	ENDIF         
	item = 'EXPONENT'         
	ok = w_item_r4(ich, item, gain, 1024, return_size)         
	PRINT*,'START gain',GX1,GX2,GY1,GY2         
	IF(IANTANG.NE.0) THEN         
	  DO JF = 1,1024         
	    ATIME(JF) = ANTANG + (1024-JF)*DANGD 
	    STIME(JF) = JF-1        
	  ENDDO         
	ELSE         
	  DO JF = 1,1024         
	    ATIME(JF) = JF  
	    STIME(JF) = JF-1       
	  ENDDO         
	ENDIF         
	  PRINT*,'ANGLE CHECK',IANTANG,ANTANG,ATIME(1),ATIME(1024)         
	  XARANGE = ABS(ATIME(1024) - ATIME(1))         
C	  IF(PTIME(1024).GT.PTIME(1)) THEN         
	    XAMIN = ATIME(1) - .022*XARANGE         
	    XAMAX = ATIME(1024) + .022*XARANGE
C	  ELSE         
C	    XMAX = PTIME(1) + .022*XRANGE         
C	    XMIN = PTIME(1024) - .022*XRANGE         
C	  ENDIF	                   
	  TOP = GY2SV - .49*GYRANGE         
	  BOTT = TOP - .05*GYRANGE          
	  CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
C         
C	  CALL MGOSETLIM(XAMIN,-.5,XAMAX,3.5) 
	SRANGE = STIME(1024) - STIME(1)
	XGMIN = STIME(1) - .022*SRANGE
	XGMAX = STIME(1024) + .022*SRANGE
	CALL MGOSETLIM(XGMIN,-.5,XGMAX,3.5)
	IF(IANTANG.NE.0) THEN         
	  CALL MGOXLABEL(17,'ANGLE, SUN TO +EX')         
	  CALL MGOTICKSIZE(10.,90.,1.,1.)         
	ELSE         
	  CALL MGOXLABEL(10,'SAMPLE NO.')         
	  CALL MGOTICKSIZE(20.,200.,1.,1.)         
	ENDIF         
	  CALL MGOCONNECT(STIME,GAIN,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOYLABEL(18,'GAIN STEP')         
	  CALL MGOSETEXPAND(1.)         
C
 210	CONTINUE
	PRINT*,'START SPECTRUM',GX1,GX2,GY1,GY2
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,330.,2200.,GY2SV-.64*GYRANGE)
	  CALL MGOSETEXPAND(.6)
	ELSE
	  CALL MGOSETLOC(150.,80.,1000.,GY2SV-.64*GYRANGE)
	  CALL MGOSETEXPAND(.8)
	ENDIF
C
	  TOP = LY2
C	  CALL MGOGRELOCATE(GX1,TOP)                    
C	  CALL MGOPUTLABEL(7,'SCET   ',3)
C	  CALL MGOPUTLABEL(79,S_SCET,3)
c	  CALL MGOPUTLABEL(20,'   major,minor frame ',3)
C	  WRITE(STR,804) MAJOR,MINOR
 804	  FORMAT(I6,'.',I3.3)
c	  CALL MGOPUTLABEL(14,STR,3)
C	  CALL MGOGRELOCATE(GX1,TOP-50.)        
C	  CALL MGOPUTLABEL(12,' FFT CHANNEL',3)
C	  WRITE(STR,802) IFFTCH
 802	  FORMAT(I4)
C	  CALL MGOPUTLABEL(6,STR,3)
C	  CALL MGOPUTLABEL(9,'  SOURCE ',3)
C	  CALL MGOPUTLABEL(4,PA(ISRC),3)
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                    
C	  ELSE
C	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy
C	  ENDIF
	  CALL MGOSETEXPAND(1.)
C
	  XFMIN = ALOG10(FREQ(1))
	  XFMAX = ALOG10(FREQ(511))
	  RANGE = ABS(XFMAX-XFMIN)
	  XFMAX = XFMAX + .02*RANGE
	  XFMIN = XFMIN - .02*RANGE	
C
	IF(IRAW.EQ.0) THEN
	  CALL MGOGRELOCATE(.2*GX2,.25*GY2)
	  CALL MGOLABEL(60,NORAW)
	  GO TO 200
	ENDIF
C
	  YMIN = 1.E-6
	  IF(PWRNW(2).GT.0.) YMIN = ALOG10(PWRNW(2))
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
C***************
C
 200	  CONTINUE
	  item = 'SPECTRUM_DB'
	  ok = w_item_R4(ich, item, PWRW, 513, return_size)
	  YMIN = PWRW(2)
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
	  DO J = 1,511
	    JP = J
	    YY(JP) = PWRW(J+1)
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
C	  WRITE(16,*)'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  PKPWRW = PWRW(JMAX)
	  IF((JMAX-1).GT.0) PKPWRW = PKPWRW + PWRW(JMAX-1)
	  IF((JMAX+1).LT.512) PKPWRW = PKPWRW + PWRW(JMAX+1)
	  IF(PKPWRW.GE.0.) AMPW = SQRT(PKPWRW)
C	  WRITE(16,803) PKPWRW,FREQ(JMAX),AMPW
C	  PRINT 803,PKPWR,FREQ(JMAX),AMPW
C
	  CALL MGOTICKSIZE(-1.,0., 0.,0.)
C
	  CALL MGOSETLIM(XFMIN,YMIN,XFMAX,YMAX)
	  CALL MGOCONNECT(XX,YY,JP)
	  CALL MGOBOX(1,2)
	  CALL MGOYLABEL(12,'V**2/CHANNEL')
	  CALL MGOXLABEL(9,'FREQ (HZ)')
C	  WRITE(TITLE(1),705) IWINDW
C 705	  FORMAT(I2)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOPLOTID('WINDOW',TITLE(IWINDW+1))
	  CALL MGOSETEXPAND(1.)
C
	CALL MGOSETEXPAND(.8)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ITERM = 3
	ELSE
	  READ(5,1023) DISPOSE
 1023	  FORMAT(A1)
            IF(DISPOSE.EQ.'P'.OR.DISPOSE.EQ.'p') THEN
              call mgotclose
              ITERM = -1
              GO TO 100
            ENDIF
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN         
C         
	END         
	SUBROUTINE RAWPRINT
C
	common /fftblk/ nexpt(1024),ndata(1024),ipwrdb(1024)
	common /headblk/ major,minor,ICH,s_scet,title,scet8
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
	CHARACTER*4 LABEL1(4)
	CHARACTER*12 LABELT
	character*32 ITEM
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS,DOY,ERT(2),OK
	INTEGER*4 SUNCLOCK,CH
	REAL*8 SCET8
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
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
C	DATA LABELB /'BX(nT)','BY(nT)','BZ(nT)','EX(V)','BZ raw'/
	DATA LABELY /'EX(V)','EY(V)','EZ(V)','EX(V)','EY(V)','EZ(V)',
     1	'BX(nT)','BY(nT)','BZ(nT)','raw'/
	DATA LABEL1 /'E1','E2','E3','B'/
	DATA DOY /0/
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	  NSYS = 0			! POSSIBLY TEMPORARY
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
	  WRITE(title(21),1021) '  Xre',XRE
	  WRITE(title(22),1021) '  Yre',YRE
	  WRITE(title(23),1021) '  Zre',ZRE
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
C
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
	    PRINT*,'NLAB',NLAB
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
C	PLOT MINIMUM VARIANCE FIELDS ON RIGHT SIDE
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
	  DO IW = 2,NW,3
	    IW1 = IW
	    IF(IW.EQ.8) IW1 = 3
	    CALL MGOWINDOW(NXW,NYW,IW1)
	    N1 = IW/2
	    N2 = N1+1
	    IF(IW.EQ.8) THEN
		N1 = 1
		N2 = 3
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
	    CALL MGOXLABEL(2,LABEL1(N1))
	    CALL MGOYLABEL(2,LABEL1(N2))
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
C	IF(DX**2+DY**2.NE.0.) CALL ARROW(XARR,YARR,DX,DY,SIZE)
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
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
C
	END	
