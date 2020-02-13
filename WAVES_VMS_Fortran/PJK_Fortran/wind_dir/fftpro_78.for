! wind/waves fft data collection and shade plot program         
          
	program		fft_process         
	implicit	integer*4 (a-z)         
!         
!	VERSION 3.1 PLOTS STRIPS IN VARIOUS  CHANNELS AT TIME OF         
!		THE FIRST CHANNEL         
! 	VERSION 3.2 ADDED TPLOT = PLOT SEVERAL FREQUENCIES VS TIME         
!		AND CHANGED OLD TPLOT = TIME SERIES PLOT TO TDPLOT         
!		AND ADDED PUBSHPLOT AND EGSSHPLOT (FOR EGS SPRING 2003)         
C	version 3.3 fixed some problems with missing events         
C	at present, require 11459 blocks of memory, plus a about seven hundred
c	for graph
c         
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
C	include		'efflen.for'         
	integer*4	ok,okt         
	character*4	event,pa(9)         
	integer*4	iq,i,j,k,n,iterm,nchgp,npt,np,nday,isrc         
	integer*4	iw,nw         
	integer*4	get_stream_name         
	integer*4	wait_for_events			! an entry point         
	integer*4	err_count         
	character*80	stream         
	parameter	size=1024         
	integer*4	return_size         
	INTEGER*4	TMSPEC         
	integer*4	raw(size)         
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
	integer*4	NHRMN,IPNT,LAST,NPTMX,N1,N2,NFILL,NPPL         
	REAL		PTIME,DELT,DELTS,DPUTIME,PHASE,SPINRATE         
	REAL		tstart,eltime,evtime,tend,tintv         
	REAL		FFTSPACE(4),COUNT(4),SPACE         
	real		nhist(100,10),vdata(1024),dbspec(513)         
	real		ddata(1024)         
	integer*2	pwrl         
	integer*2	pwrlt(500,511)         
	integer*4 	ncount         
	integer*4	first_channel     
	REAL*8 		SCETMFI    
!         
	complex preamp,fftamp,fft_filter         
!         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	COMMON /SHBLK/ PTIME(500),PWRL(500,511,4)         
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),VDATA         
	COMMON /RAWVOLTS/ RDATA(1026),RAWAVR,SLOPE
	COMMON /TELEM/ TMSPEC(1024)         
	COMMON /MFIDATA/ NMFI, BX(600),SCETMFI(600)         
	common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8      
	common /angles/ iantang,antang,ezang,bzantang         
!         
	DATA ITMCORR /1/             ! 1 = CORRECT FOR TIME, 0 = PLOT         
C					CONSECUTIVE SPECTRA         
	data ncount /0/         
	DATA IPNT /4*0/         
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',         
     1		'BX','BY','BZ'/         
	DATA JP /0/         
	DATA IRAW /0/         
	DATA TWOPI /6.2831853/         
	DATA SPACEMIN,SPACEMAX /1.E6, 0./         
	DATA NTPLOTS /0/         
	DATA NUMPLOTS /1000/         
C	NUMPLOTS IS A LIMIT ON THE NUMBER OF SPPLOTS AND T PLOTS         
C	THE PROGRAM SKIPS THESE FOR SHPLOT, I.E. ONLY STARTS         
C	SHPLOT WHEN THESE HAVE BEEN DONE         
C         
C         
C	NUMPLOTS MUST BE EVEN TO MAINTAIN CADENCE OF SERIES         
C         
	IF(NCHGP.EQ.1) NUMPLOTS = 2*(NUMPLOTS/2)         
	IF(NCHGP.GT.1) NUMPLOTS = 4*(NUMPLOTS/4)         
C         
	ok = get_stream_name(stream)         
	if (.not. ok) stop 'no file supplied.'         
          
	ok = w_channel_open(ch,stream)         
	if (.not. ok) stop 'cannot open tm channel'         
C         
C	READ IN DESIRED TIME          
C         
	write(6,*) 'type hr,min to start,end, e.g. 0414,0800'         
	read(5,*) nhrmn,last         
	type*,nhrmn,last         
	tstart = (nhrmn/100) + mod(nhrmn,100)/60.         
	tlast = (last/100) + mod(last,100)/60.         
C         
C	READ IN DESIRED CHANNEL GROUP         
C         
	write(6,4)         
	read(5,3) iq, nchgp         
	type*,nchgp         
 3	format(q,i10)         
 4	format(1x,'enter 1 for hi channels, 2 for mid, 3 for low')         
 5	format(q,a)         
C         
	do n = 1,4         
	  title(n) = '          '         
	  fftspace(n) = 0.         
	  count(n) = 1.E-8         
	enddo         
          
	ok = w_channel_filename(ch,file)         
	print*,'file',file         
C         
C	FIND POSITION IN TELEMETRY CHANNEL         
C         
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
	  if(nchgp.eq.1) event = 'FFTH'         
	  if(nchgp.eq.2) event = 'FFTM'         
	  if(nchgp.eq.3) event = 'FFTL'         
	  first_channel = 1         
	  if(nchgp.eq.2) first_channel = 3         
	  if(nchgp.eq.3) first_channel = 7         
	  nw = 2         
	  if(nchgp.gt.1) nw = 4         
 10	  ok = w_event(ch,event)         
C         
c************test of spin phase         
C	phsav = 0.         
C	do ix = 1,100         
C	   call w_channel_position(ch,scetlast)         
C	   item = 'WIND_SPIN_PHASE_R4'         
C	   ok = w_item_r4(ch, item, PHASE, 1, return_size)         
C	   item = 'WIND_SPIN_scet_R8'         
C	   ok = w_item_r8(ch, item, scetspin, 1, return_size)         
	   item = 'wind_spin_rate_r4'         
	   okt = w_item_r4(ch, item, spinrate, 1, return_size)         
	   if(okt.ne.1) spinrate = 2.         
C	write(88,*) 'phase,scet,rate',phase,scetspin,spinrate         
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
C         
	  NEWOLD = 0         
	  IF(NCHGP.EQ.0) THEN         
		NCHGP = 3         
	        NEWOLD = 1         
	  ENDIF         
          
	   item = 'EVENT_SCET'         
	   ok = w_item_i4(ch, item, scet, 2, return_size)         
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.         
     1		+ mod(scet(2),100)/3600.         
	   type*,'first event at',scet,'  evtime',evtime         
	   ptime(1) = evtime         
C         
	   item = 'EVENT_SCET_R8'         
	   ok = w_item_r8(ch, item, scet8, 1, return_size)         
	   call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)         
C         
	   item = 'channel_number'         
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)         
	   if(fft_channel.ne.first_channel) then         
		last_channel = fft_channel         
		type*,'channel',fft_channel,' is not first, restart'         
		go to 10         
	   endif         
c	   last_channel = fft_channel         
c	print*,'1 last,this,channel',last_channel,fft_channel         
C         
c	Keith's values do not quite agree with mine, so I use mine, which          
c		are stored in efflen.for         
c         
	   item = 'EX_LENGTH'         
	   ok = w_item_r4(ch, item, exphys, 1, return_size)         
c         
c	   item = 'EX_LENGTH_EFF'         
c	   ok = w_item_r4(ch, item, EFFLEN(1), 1, return_size)         
c         
C         
	   item = 'EY_LENGTH'         
	   ok = w_item_r4(ch, item, eyphys, 1, return_size)         
c         
c	   item = 'EY_LENGTH_EFF'         
c	   ok = w_item_r4(ch, item, EFFLEN(2), 1, return_size)         
c         
C         
	   item = 'EZ_LENGTH'         
	   ok = w_item_r4(ch, item, ezphys, 1, return_size)         
c         
c	   item = 'EZ_LENGTH_EFF'         
c	   ok = w_item_r4(ch, item, EFFLEN(3), 1, return_size)         
c         
c	CALL W_MESSAGES_OFF(CH)         
C         
C	GET MAGNETIC FIELD DATA
C
	     item = 'WIND_MFI_BX(GSE)_R4'
	     ok = w_item_R4(ch, item, BX, 600, return_size)
	     NMFI = RETURN_SIZE
	     print*,'got bx, return_size=',return_size
	     item = 'WIND_MFI_SCET_R8'
	     ok = w_item_R8(ch, item, SCETMFI, 600, return_size)
	     print*,'got time, return_size=',return_size
C
	  go to 110         
          
	! this is the main program loop         
          
 100	   continue         
           ok = w_event(ch,event)         
	   if (ok.ne.1) then         
	     if(ok.eq.82) then         
		type*,'end of file'         
		call shplot(nchgp)         
		return         
	     else         
	      type *, char(7), '******** missing packet in event ********'         
	      type *, 'Cannot get event at MF.mf: ', major, minor         
	     endif         
	   end if         
C         
 110	  CONTINUE	           
          
	   ! now get the items         
          
	   last_channel = fft_channel         
	   item = 'channel_number'         
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)         
C         
C	FIX UP ANY MISSING CHANNELS         
C         
	IF(FFT_CHANNEL.LE.LAST_CHANNEL) THEN         
C         
C	  BEFORE STARTING A NEW SET OF EVENTS, LOOK FOR MISSING CHANNELS      
C		IN THE LAST SET         
C         
C		ARE THERE PREVIOUS DATA TO USE IN FIXUP?         
C         
	  IPNTMAX = 0         
	  DO I = 1,NW         
		IPNTMAX = MAX0(IPNTMAX,IPNT(I))         
	  ENDDO         
	  DO I = 1,NW         
	        IF(IPNT(I).EQ.0.AND.IPNTMAX.NE.0) GO TO 100         
	  ENDDO         
C         
C	  FIX UP, IF NOT START, BY COPYING PREVIOUS DATA         
C         
 	    DO I = 1,NW         
	      IF(IPNT(I).NE.IPNTMAX) THEN         
	        print*,'missing channel iw=',I,' at',ipntmax         
		IPNT(I) = IPNTMAX         
	        do ii=1,511           
  	  	    PWRL(IPNTMAX,II,I) = PWRL(IPNTMAX-1,II,I)         
C		    PWRW(II) = DBSPEC(II+1)         
	        end do         
	      ENDIF         
	    ENDDO         
	ENDIF         
c
C		END OF FIXUP
C         
C	print*,'end of fixup'
	iw = fft_channel-6         
	if(nchgp.eq.1) iw = fft_channel         
	if(nchgp.eq.2) iw = fft_channel-2         
	if(nchgp.eq.3) iw = fft_channel-6         
	iw = max0(iw,1)         
	iw = min0(iw,4)
C     
C	ADVANCE NPT
C    
	NPT = IPNT(IW) + 1         
	IPNT(IW) = NPT         
C	print*,'ipnt',ipnt         
C
c	   item = 'WIND_SPIN_PHASE_R4'         
c	   ok = w_item_r4(ch, item, PHASE, 1, return_size)         
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
C	   if(scet8.gt.scetlast) stop         
c         
	   item = 'DPU_MAJOR_FRAME'         
	   ok = w_item_I4(ch, item, MAJOR, 1, return_size)         
	   item = 'DPU_MINOR_FRAME'         
	   ok = w_item_I4(ch, item, MINOR, 1, return_size)         
C         
	   item = 'SOURCE'         
	   ok = w_item_i4(ch, item, ISRC, 1, return_size)         
C	         
	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)         
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//         
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)         
C         
	IF(FFT_CHANNEL.LT.LAST_CHANNEL) THEN         
	   PTIME(NPT) = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.         
     1		+ mod(scet(2),100)/3600.         
	ENDIF         
C         
c	calculate spin phase at start of event         
c         
c	   tdiff = 86400.D00*(scet8 - scetspin)         
c	   phdiff = tdiff*spinrate         
c	   if(phdiff.lt.0.) then         
c		phase = phase + phdiff         
c	        dowhile (phase.lt.0.)         
c		  phase = phase+twopi         
cc	  	   print*,'phase',phase         
c		enddo         
c		phase = amod(phase,twopi)         
c	   else         
c		phase = amod(phase + phdiff,twopi)         
c	   endif         
c         
c******************         
c	     if(newold.eq.1.and.fft_channel.lt.7) go to 100         
C	     if(fft_channel.NE.3) go to 100         
	IF(IW.GE.3) PRINT*,'IW,EVENT,FFT_CHANNEL',IW,EVENT,FFT_CHANNEL         
c         
	   WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(ISRC)         
 1001	   FORMAT('CH',I2,'  ',A4)         
          
C         
	TYPE*,'CALL FFT_PHYS'         
C         
	call fft_phys(ch,iraw,vdata,dbspec)   
C
c******************         
c	do nn = 2,513
c	  write(67,*) nn,21.3477*(nn-1),fft_channel,dbspec(nn),tmspec(nn)
c	enddo
c	if(1) stop
c******************         
C	if(iraw.eq.1) write(22,*) scet,iraw,fft_channel         
C	write(22,*) scet,iraw         
C         
C	CALCULATE TOTAL POWER         
C         
	FFUND = 20.3477         
	IF(FFT_CHANNEL.GT.2) FFUND = .25*FFUND         
	IF(FFT_CHANNEL.GT.6) FFUND = .333557         
	TPOWER = 0.         
C	do n = 128,512         
	do n = 2,512         
		TPOWER = TPOWER + FFUND*EXP(.23026*DBSPEC(N))         
	enddo         
	TPOWERDB = 10.*ALOG10(TPOWER)         
C         
 4455	format(12(1x,f6.3))         
c         
C	TYPE*,'GET ITEMS SPECTRUM_DB AND VOLTS/METER'         
C	   item = 'SPECTRUM_DB'         
C	   ok = w_item_R4(ch, item, DBSPEC, 1024, return_size)         
C	   item = 'volts/meter'         
C	   ok = w_item_R4(ch, item, VDATA, 1024, return_size)         
c****         
C         
C	THIS CHECK DEPENDS ON THE ORDER--CHANNEL 7 IS ASSUMED TO         
C	BE EZ, CHANNEL 8 IS ASSUMED TO BE EX         
C         
	FFUND = 20.3477         
	IF(FFT_CHANNEL.GT.2) FFUND = .25*FFUND         
	IF(FFT_CHANNEL.GT.6) FFUND = .333557         
	ITEM = 'WIND_SPIN_RATE_R4'         
	OK = W_ITEM_R4(CH,ITEM,SPINRATE,1,ISIZE)         
	SAMP_PER_SEC = 1024.*FFUND         
	DANG = SPINRATE/SAMP_PER_SEC			! IN RADIANS         
	DANGD = DANG*360./TWOPI         
	SAMP_PER_SPIN = TWOPI/DANG         
	NSPPS = SAMP_PER_SPIN + .5			! ROUNDOFF         
c	write(65,*) 'spinrate,dangd,samp_per_spin',spinrate,dangd         
c     1		,samp_per_spin         
c         
c	if(isrc.eq.4) then         
c	  do i = 1,1024         
c	    write(77,*) i, vdata(i), rdata(i)         
c	  enddo         
c	  if(1) stop 'write 77'         
c	endif         
c         
	if(fft_channel.eq.7.and.nchgp.eq.3) iantang = 0         
	IF(isrc.eq.6) then         
	  vzmax = 0.         
	  do i = 1,1024         
	    if(rdata(i).gt.vzmax) then         
		vzmax = rdata(i)         
		ismpmax = i         
	    endif         
	  enddo         
	  NZMAX = ISMPMAX         
        endif         
	ezang = -83. - dangd*(1024-nzmax) - 45.         
c	write(65,*) 'ezmax,antang',scet,nzmax,vzmax,ezang		         
	if(vzmax.gt..3) then         
	    antang = ezang         
	    iantang = 1         
	endif         
c         
	IF(isrc.eq.9) then         
	  bzmax = 0.         
	  bzmin = 0.         
	  do i = 1,1024         
	    if(rdata(i).gt.bzmax) then         
		bzmax = rdata(i)         
		ismpmax = i         
	    endif         
	    if(rdata(i).lt.bzmin) then         
		bzmin = rdata(i)         
		ismpmin = i         
	    endif         
	  enddo         
	  bzmxang = antang - dangd*(1024-ismpmax)         
	  bzmnang = antang - dangd*(1024-ismpmin)         
c	  write(65,*) 'bz min,max',scet,ismpmin,ismpmax		         
c	  write(65,*) 'bz min,maxangles',bzmnang,bzmxang	         
	  bzantang = 79. - dangd*(1024-ismpmax) - 45.         
	  if(iantang.eq.0) then         
	    antang = bzantang         
	    iantang = 2         
	  endif         
        endif         
c         
	IF(isrc.eq.4.and.nchgp.eq.3) then         
	  call fftloangle(ch,vdata,sunclock,antangl)         
c	  write(65,*) 'lo angle',scet,sunclock,antangl         
	  if(iantang.eq.0) then         
	    iantang = 3         
	    antang = antangl         
	  endif         
	endif         
C         
C	if(isrc.ne.4.and.isrc.ne.5) go to 100         
	NTPLOTS = NTPLOTS+1         
C	if(iraw.eq.1.and.isrc.eq.4.AND.NTPLOTS.LE.9) call SPECLtplot(ch)         
	IF(NTPLOTS.LE.NUMPLOTS) THEN         
  	  IF(IRAW.EQ.1) then         
C	    type*,'tdplot called AT NTPLOTS',NTPLOTS         
	    if(iraw.eq.1) call tdplot(ch)         
c	    CALL OFFSCHECK(CH,iraw,vdata,dbspec)         
C	IF(IRAW.EQ.1) STOP
c	    if(iraw.eq.1.and.isrc.eq.4) call tdplot(ch)         
	    print*,'call spplot at NTPLOTS'         
	    call spplot(ch,iraw,vdata,dbspec)         
	  else         
	    print*,'call spplot at NTPLOTS'         
	    call spplot(ch,iraw,vdata,dbspec)         
	  endif         
C	  stop          
c  	  type*,'tplot called'         
c	  if(iraw.eq.1.and.isrc.eq.4) call tdplot(ch)         
C	  GO TO 100         ! I don't know why this was here
	ENDIF         
C         
	Jp = jp+1         
c         
C	the rest is for shade plot SHPLOT         
c         
C	NW is the number of windows in the shade plot, 2 for hi, 4 for mid,low         
C         
	      do i = 1,nw		! to take care of a problem         
		space = amax1(space,fftspace(i))         
		fftspace(i) = space         
	      enddo         
C
C	TYPE*,'EVTIME,SPACE,FFTSPACE',EVTIME,SPACE,FFTSPACE(1),fftspace(2)         
c	   endif         

	if(npt.gt.1) then
	  spacemin = amin1(spacemin,(ptime(npt)-ptime(npt-1)))
	  spacemax = amax1(spacemax,(ptime(npt)-ptime(npt-1)))
	endif
          
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.         
     1		+ mod(scet(2),100)/3600.         
	   eltime = evtime - tstart         
C         
	if(iraw.eq.0) then         
		fftcount = fftcount + 1.         
	else         
		rawcount = rawcount + 1.         
	endif         
C         
C	THIS PART OF THE PROGRAM ATTEMPTS TO ASSIGN STRIPS IN SHPLOT         
C	WITHOUT EITHER TOO MUCH DUPLICATION OR TOO MUCH DATA LOSS         
c         
c	load histogram of intervals         
c         
	ihist = amin1((eltime - eltimes)*1800.,100.) + .5         
	ihist = max0(ihist,1)         
	nhist(ihist,fft_channel) = nhist(ihist,fft_channel) + 1         
	eltimes = eltime         
	         
	type 1002,npt,fft_channel,ptime(npt),scet,tpower         
 1002	FORMAT(' npt,chan,hours,scet,v/m**2',I5,I3,F9.4,2I10,E12.3)         
C         
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
	spacemin = amin1(fftspace(iw),spacemin)         
C         
	   do i=1,511         
c		PWRL(NPT,I,IW) = RAW(I+1)         
		PWRL(NPT,I,IW) = DBSPEC(I+1)         
		PWRW(I) = DBSPEC(I+1)         
	   end do         
	   IF(NPT.GT.495) GO TO 200         
C	   IF(NPT.GT.295) GO TO 200         
	   IF(scet(2).LT.(100*LAST)) GO TO 100         
          
 200	continue         
	TYPE*,NPT,' SPECTRA FOUND, EACH CHANNEL'         
	TYPE*,'GO TO PLOT AT ',S_SCET         
          
	print*,'at rewrite npt,npt=',npt         
	TEND = 0.         
	NPTMX = 0         
	DO I = 1,NW         
	  NPT = IPNT(I)         
	  NPTMX = MAX0(NPT,NPTMX)         
	  IF(NPT.GT.0) TEND = AMAX1(TEND,PTIME(NPT))         
	ENDDO         
	print*,'nptmx,ipnt',nptmx,ipnt         
          
C         
c	NPT IS NOW USED AS AN ESTIMATE OF THE NEXT PWRL TIME INDEX          
C		CORRESPONDING TO THE EXACT TIME   PTIME(1) + (NP-1)*TINTV         
C         
	NPT = 1         
C         
C	DO N = 1,NPTMX-1         
C	PRINT*,N,PTIME(N),(PTIME(N)-PTIME(1))/TINTV         
C	ENDDO         
          
C         
	      do i = 1,nw		         
		space = amax1(space,fftspace(i))         
	      enddo         
          
	DO I = 1,NW         
C	  COPY PWRL INTO PWRLT         
	  DO NP = 1,NPTMX         
	     DO J = 1,511         
		PWRLT(NP,J) = PWRL(NP,J,I)         
	     ENDDO         
	  ENDDO         
C         
C	FIND TINTV = AVERAGE SPACING OF FFT'S, NOT COUNTING RAW'S         
	    TINTV = SPACE/COUNT(I)         
C	FIND NPPL = NUMBER OF PIXELS IN TIME DIRECTION         
	    NPPL = (PTIME(NPTMX) - PTIME(1))/TINTV         
	PRINT*,'SPACE,NO OF SLICES,INT',SPACE,NPPL,TINTV         
C	    IF(NPPL.GT.997) THEN         
	    IF(NPPL.GT.499) THEN         
	print*,'************** warning, too many slices ********'         
	       NPPL = 499         
	       TINTV = (PTIME(NPTMX) - PTIME(1))/NPPL         
	    ENDIF         
C         
	  NP = 0         
	  NPT = 0         
	  DOWHILE (NPT.LT.NPTMX)         
	    NP = NP+1         
	    ELTIME = PTIME(1) + (NP-1)*TINTV         
C         
	    IF(ABS(PTIME(NP)-ELTIME).LT..5*TINTV) THEN           
C		NO CORRECTION REQUIRED         
	      NPT = NP+1         
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
C	TYPE*,'PUT NUMBER',NPT,' INTO SLOT',NP,' TIME',PTIME(NPT)         
C         
		DO J = 1,511         
		  PWRL(NP,J,I) = PWRLT(NPT,J)         
		ENDDO         
 210	    CONTINUE         
 	    NPT = MIN0(NPT+1,NPTMX)         
	  ENDDO         
	ENDDO         
C         
C	(N_CHANNEL_GROUP)  NCHGP=1 IS HIGH, 2 IS MID, 3 IS LOW         
	   NPT = NPTMX         
C	call rawprint         
	print*,'call spplot at end'         
	call spplot(ch,iraw,vdata,dbspec)         
C	print*,'call tdplotat end if iraw = 1,iraw=',iraw         
C	if(iraw.ne.0) call tdplot(ch)         
c	if(iraw.ne.0.and.isrc.eq.4) call tdplot(ch)         
ctemp	print*,'CALL SHPLOT'         
ctemp	        CALL SHPLOT(NCHGP)         
	print*,'CALL SHPLOT'         
	        CALL SHPLOT(NCHGP)         
ctemp		CALL TPLOT(NCHGP)         
		ICH = 1         
C	print*,'CALL PUBSHPLOT,CHANNEL',ICH         
C	        CALL PUBSHPLOT(NCHGP,ICH)         
C	print*,'CALL EGSSHPLOT,CHANNEL',ICH         
C	        CALL EGSSHPLOT(CH,NCHGP,ICH)         
		print*,'fftcount',fftcount         
		print*,'rawcount',rawcount         
		print*,'spacemin,max',spacemin,spacemax
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
	SUBROUTINE SHPLOT(NCHGP)         
C         
	character*32 s_scet         
	integer*2 NPWRL         
	CHARACTER*10 TITLE(15)         
	CHARACTER*120 STR         
	REAL FUNDF(3)         
	real*8 scet8         
	integer*4 fft_channel         
C	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4)         
	COMMON /SHBLK/ PTIME(1000),NPWRL(500,511,4)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8         
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
C	DIMENSION PWRL(511000)         
C	DIMENSION PWRL(102200)         
	DIMENSION PWRL(255500)         
	DATA FUNDF /.021348,.005337, .33356/         
C         
C         
	NWNDOW = 2         
	IF(NCHGP.NE.1) NWNDOW = 4         
	ITERM = -1		! 370a, portrait         
	ITERM = -3		! 370a, portrait         
C	ITERM = 3         
	XSTART = 400.         
	XEND = 2300.         
	IW = 1         
	NPT = IPNT(IW)         
	IF(NPT.LT.1) NPT = 1             	  ! protection         
	PRINT*,' IN SHPLOT,NPT',NPT         
	PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NPT),NPT         
C	CALCULATE BOX SIZE         
	PIXSIZ = .1         
	IF(NPT.GT.1) PIXSIZ = (PTIME(NPT) - PTIME(1))/(NPT-1)         
	HSTART = PTIME(1) - .5*PIXSIZ         
	HEND =   PTIME(NPT) + .5*PIXSIZ         
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
	  CALL HISTOG(2,TJUNK,256,-250.,0.,.01,TOTAL,RET)     ! CLEAR AND INIT         
	  DO M = 1,NPT         
	  DO N = 1,511         
	    NM = M + (N-1)*NPT         
	    PWRL(NM) = NPWRL(M,N,IW)         
	    CALL HISTOG(1,PWRL(NM),256,-250.,0.,.5,TOTAL,RET)    ! LOAD ARRAY         
C         
	    YMIN = AMIN1(YMIN,PWRL(NM))         
	    YMAX = AMAX1(YMAX,PWRL(NM))         
	  ENDDO         
	  ENDDO         
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX         
C         
C	  CALL HISTOG(0,TJUNK,256,-250.,0.,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE         
	  CALL HISTOG(0,TJUNK,256,-250.,0.,.10,TOTAL,YHMIN)   !DETERMINE10 PCTILE         
	  CALL HISTOG(0,TJUNK,256,-250.,0.,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE         
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER         
C	  LOWERING YMAX MAKES THE SIGNAL DARKER         
          
	  YAMIN = YHMIN          
	  YAMAX = YHMAX + 20.          
	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX         
c	  arguments are: array(m,n),m,n,white,black,linearity         
c	  m is the x direction         
	  CALL MGOHALFTONE(PWRL,NPT,511,YAMIN,YAMAX,1.E7)         
	  CALL MGOSETEXPAND(.7)         
	  FMIN = FUNDF(NCHGP)         
	  FMAX = 511.*FMIN         
	  CALL MGOSETLIM(HSTART,FMIN,HEND,FMAX)         
	  IF(NCHGP.EQ.3) THEN         
	    CALL MGOYLABEL(13,'FREQUENCY, Hz')	         
	  ELSE         
	    CALL MGOYLABEL(14,'FREQUENCY, kHz')	         
	  ENDIF         
	  CALL MGOTICKSIZE( .16667, 1., 0., 0.)         
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
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//         
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)         
            WRITE(STR,1001) S_SCET(1:10),IDOY         
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
	    CALL MGOPLOTID('FFTPRO 3.1','SHPLOT')         
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
	SUBROUTINE TPLOT(NCHGP)         
C         
	character*32 s_scet         
	integer*2 NPWRL         
	CHARACTER*10 TITLE(15)         
	CHARACTER*120 STR         
	REAL FUNDF(3)         
	real*8 scet8         
	integer*4 fft_channel,FLIST(15)         
C	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4)         
	COMMON /SHBLK/ PTIME(1000),NPWRL(500,511,4)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8         
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
C	DIMENSION PWRL(511000)         
C	DIMENSION PWRL(102200)         
C	DIMENSION PWRL(255500)         
	DIMENSION YY(1000)         
C	DATA FUNDF /.021348,.005337, .33356/	! note first 2 in kHz         
	DATA FUNDF /21.348,5.337, .33356/	         
	DATA FLIST /1,2,3,4,5,6,7,8,7*9/         
C         
C         
C	ITERM = -1		! 370a, portrait         
	ITERM = -1		! 370a, portrait         
	ITERM = -3		! 370a, portrait         
C	ITERM = 3         
	XSTART = 500.         
	XEND = 2300.         
C         
C	IW = NUMBER OF FREQUENCIES TO BE PLOTTED         
C         
	NWNDOW = 6         
	IWCH = 1         
	IF(NCHGP.EQ.2) IWCH = 2         
	NPT = IPNT(IWCH)         
	IF(NPT.LT.1) NPT = 1             	  ! protection         
	PRINT*,' IN SHPLOT,NPT',NPT         
	PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NPT),NPT         
C	CALCULATE BOX SIZE         
	PIXSIZ = .1         
	IF(NPT.GT.1) PIXSIZ = (PTIME(NPT) - PTIME(1))/(NPT-1)         
	HSTART = PTIME(1) - .5*PIXSIZ         
	HEND =   PTIME(NPT) + .5*PIXSIZ         
	PRINT*,' IN TPLOT',HSTART,HEND,NPT         
	  CALL MGOINIT         
	  CALL MGOSETUP(ITERM)         
	  CALL MGOERASE         
C         
	IF(ITERM.LT.0) THEN         
	  CALL MGOSETLOC(XSTART-100.,280.,XEND,3100.)         
	  CALL MGOYLABEL(15,'dB (V/m)\u2/Hz')         
	  CALL MGOSETLOC(XSTART,280.,XEND,3100.)         
	ENDIF         
C         
	DO IW = 1,NWNDOW         
	  CALL MGOWINDOW(1,NWNDOW,IW)                          !          
	  YMIN = 1.E6         
	  YMAX = -YMIN         
	  DO M = 1,NPT         
	    N = FLIST(IW)         
	    YY(M) = NPWRL(M,N,IWCH)         
	    YMIN = AMIN1(YMIN,YY(M))         
	    YMAX = AMAX1(YMAX,YY(M))         
	  ENDDO         
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX         
	  YRANGE = ABS(YMAX-YMIN)         
	  YMAX = YMAX + .02*YRANGE         
	  YMIN = YMIN - .02*YRANGE         
	  CALL MGOSETLIM(HSTART,YMIN,HEND,YMAX)         
C         
	  FHZ = FUNDF(NCHGP)*FLIST(IW)         
	  IF(FHZ.LT.1000.) THEN         
	    WRITE(TITLE(IW),1013) FHZ         
 1013	    FORMAT(F5.1,' Hz')	         
	  ELSE         
	    WRITE(TITLE(IW),1014) .001*FHZ         
 1014	    FORMAT(F6.3,' kHz')	         
	  ENDIF         
	  CALL MGOSETEXPAND(.7)         
	  CALL MGOCONNECT(PTIME,YY,NPT)         
	  CALL MGOTICKSIZE( .16667, 1., 0., 0.)         
	  CALL MGOBOX(1,2)         
C         
	  XPR = GX1 + .005*(GX2-GX1)         
	  YPR = GY1 + .85*(GY2-GY1)         
	  CALL MGOGRELOCATE(XPR,YPR)         
	  CALL MGOSETEXPAND(.6)         
	  CALL MGOPUTLABEL(15,TITLE(IW),9)         
	print*,'title in shplot  ',title(iw)         
	  CALL MGOSETANGLE(0.)         
	  CALL MGOSETEXPAND(1.)         
	  IF(IW.EQ.1) THEN         
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//         
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)         
            WRITE(STR,1001) S_SCET(1:10),IDOY         
 1001	    FORMAT('HOURS OF ',A10,' DOY ',I3)         
	    CALL MGOSETEXPAND(.8)         
            CALL MGOXLABEL(28,STR)	         
	    CALL MGOSETEXPAND(1.)         
	  ENDIF         
C         
	  IF(IW.EQ.NWNDOW) THEN         
	    CALL MGOSETEXPAND(.8)         
	    CALL MGOPLOTID('FFTPRO 3.2','TPLOT')         
	    CALL MGOSETEXPAND(1.)         
	  ENDIF         
	         
	ENDDO         
C         
	  CALL MGOSETEXPAND(1.)         
	  IF(ITERM.LT.0) THEN         
	    CALL MGOPRNTPLOT(NVEC)         
	    PRINT*,' NO. VECTORS PLOTTED',NVEC         
	  ELSE         
C	    CALL MGOTCLOSE         
	  ENDIF         
C         
	TMONGO = SECNDS(0.)         
 987	CONTINUE         
	DELTA = SECNDS(TMONGO)         
	IF(DELTA.LT.2.) GO TO 987         
C         
	RETURN         
	END         
	SUBROUTINE SPPLOT(ICH,IRAW,VDATA,DBSPEC)         
C         
C	PLOTS THE SPECTRUM, WINDOWED, AND NOT WINDOWED IF RAW DATA         
C         
	character*32 scet,item         
	character*10	titlex(15)         
	real*8 scet8         
	character*4	pa(9)         
	COMMON /EFFLDATA/ EFFLEN(4)         
	REAL PWRNW(1026),DBSPEC(513),VDATA(1024)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,IFFTCH,isrc,scet,titlex,scet8         
	common /telem/ tmspec(1024)         
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
	CHARACTER*1 DISPOSE      
	data iwindw /1/         
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/         
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',         
     1		'BX','BY','BZ'/         
C         
C	PRINT*,' IN SPPLOT',NPT         
	FFUND = 20.3477         
	IF(IFFTCH.GT.2) FFUND = .25*FFUND         
	IF(IFFTCH.GT.6) FFUND = .333557         
	DO JF = 1,512         
	  FREQ(JF) = JF*FFUND         
	ENDDO         
C         
	ITERM = -3         
	ITERM = 3         
C         
	CALL MGOINIT         
	CALL MGOSETUP(ITERM)         
	CALL MGOERASE         
	IF(ITERM.LT.0) THEN         
	  CALL MGOSETLOC(500.,330.,2200.,3000.)         
	ELSE         
C	  CALL MGOSETLOC(500.,330.,2200.,3000.)         
	ENDIF         
C         
	  EXPSAVE = EXPAND
	  CALL MGOSETEXPAND(.6)         
	  TOP = GY2         
	  CALL MGOGRELOCATE(GX1,TOP)                             
	  CALL MGOPUTLABEL(6,'SCET   ',3)         
	  CALL MGOPUTLABEL(22,SCET,3)         
	  CALL MGOPUTLABEL(5,' R8  ',3)         
	  WRITE(STR,804) SCET8         
 804	  FORMAT(F14.8)         
c	  CALL MGOPUTLABEL(15,STR,3)         
	  CALL MGOPUTLABEL(8,'  MJ.mn ',3)         
	  WRITE(STR,801) MAJOR,MINOR         
 801	  FORMAT(I6,'.',I3.3)         
	  CALL MGOPUTLABEL(14,STR,3)    
	  CALL MGOSETEXPAND(EXPSAVE)         
	  IF(ITERM.LT.0) THEN         
	    CALL MGOSETLOC(500.,330.,2200.,TOP-120.)         
	  ELSE        
C	    CALL MGOSETLOC(500.,330.,2200.,TOP-100.)         
	  ENDIF         
C         
	  XMIN = ALOG10(FREQ(1))         
	  XMAX = ALOG10(FREQ(511))         
	  RANGE = ABS(XMAX-XMIN)         
	  XMAX = XMAX + .02*RANGE         
	  XMIN = XMIN - .02*RANGE	         
C         
C	print*,' in spplot, iraw=',iraw         
 	  CALL MGOWINDOW(1,2,2)                          ! WITH WINDOW         
C	  item = 'SPECTRUM_DB'         
C	  ok = w_item_R4(ich, item, PWRW, 513, return_size)         
	  YMIN = DBSPEC(2)         
	  YMAX = YMIN         
	  JP = 1         
	  JMAX = 1         
	  PWRTOT = 0.   
	  YAVR = 0.      
	  DO J = 2,512         
	    JP = J-1         
	    YY(JP) = DBSPEC(J)         
	    XX(JP) = ALOG10(FREQ(JP))         
	    PWRTOT = PWRTOT + 10.**(.1*DBSPEC(J))         
	    IF(YY(JP).GT.YMAX) THEN         
	      YMAX = AMAX1(YY(JP),YMAX)         
	      JMAX = JP         
	    ENDIF         
	    YMIN = AMIN1(YY(JP),YMIN)         
	    IF(IFFTCH.LE.2) YMIN = AMAX1(-210.,YMIN)         
	    YAVR = YAVR+YY(JP) 
	  ENDDO         
	  CALL MGOSETEXPAND(.6)         
	  CALL MGORELOCATE(X1,Y1 + .2*(Y2-Y1))                 
	  CALL MGOPUTLABEL(12,' FFT CHANNEL',3)         
	  WRITE(STR,802) IFFTCH         
 802	  FORMAT(I3)         
	  CALL MGOPUTLABEL(3,STR,3)         
	  CALL MGOPUTLABEL(9,'  SOURCE ',3)         
	  ITEM = 'SOURCE'         
	  ok = w_item_I4(ICH, item, ISRC, 1, return_size)         
	  CALL MGOPUTLABEL(6,PA(ISRC),3)         
	  YAVR = YAVR/511.
	  CALL MGOPUTLABEL(12,' MIN,MAX,AVR ',3)
	  WRITE(STR,805) YMIN,YMAX,YAVR
 805	  FORMAT(F8.1,F7.1,F8.2)        
	  CALL MGOPUTLABEL(23,STR,3)
	  CALL MGOSETEXPAND(EXPSAVE)
C         
	  RANGE = ABS(YMAX-YMIN)         
	  YMAX = YMAX + .05*RANGE         
	  YMIN = YMIN - .05*RANGE	         
	  PRINT*,'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT         
C	  WRITE(16,*)'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT     
	  PKPWRW = DBSPEC(JMAX)         
	  IF((JMAX-1).GT.0) PKPWRW = PKPWRW + DBSPEC(JMAX-1)         
	  IF((JMAX+1).LT.512) PKPWRW = PKPWRW + DBSPEC(JMAX+1)         
	  AMPW = 0.         
C	  AMPW = SQRT(PKPWRW)         
C	  WRITE(16,803) PKPWRW,FREQ(JMAX),AMPW         
C	  PRINT 803,PKPWR,FREQ(JMAX),AMPW         
C         
	  CALL MGOTICKSIZE(-1.,0.,0.,0.)         
C         
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)         
	  CALL MGOCONNECT(XX,YY,JP)         
	  CALL MGOBOX(1,2)         
	  IF(ISRC.LE.6) THEN         
	    CALL MGOYLABEL(14,'dB (V/m)\U2/HZ')         
	  ELSE         
	    CALL MGOYLABEL(12,'dB nT\U2/HZ')         
	  ENDIF         
C	  WRITE(TITLE(1),705) IWINDW         
C 705	  FORMAT(I2)         
	  CALL MGOSETEXPAND(.6)         
	  CALL MGOPLOTID('WINDOW',TITLE(IWINDW+1))         
	  CALL MGOSETEXPAND(EXPSAVE)         
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
C	CALL MGOSETEXPAND(.8)         
C	CALL MGOPLOTID('WINDFFT','SPPLOT')         
C	CALL MGOSETEXPAND(1.)         
C
C
	IF(IRAW.EQ.0) THEN
	  CALL MGORELOCATE(.1*X2,.25*Y2)
	  CALL MGOLABEL(41,'NO RAW DATA AVAILABLE FOR UNWINDOWED PLOT')
	  GO TO 200       
	ENDIF  
	  CALL MGOWINDOW(1,2,1)                          ! NO WINDOW         
C         
	  IF(ISRC.LE.6) THEN         
	   item = 'E_VOLTS/METER'         
	  ELSE         
	   item ='B_NT2/HZ'         
	  ENDIF         
C         
	vpower = 0.         
	  NPROC = 1024         
	  NFRQ = NPROC/2         
	  DO I = 1,NPROC         
	    PWRNW(I) = VDATA(I)         
	    vpower = vpower + pwrnw(i)**2         
c	    write(66,*) i,ifftch,pwrnw(i)         
	  ENDDO 
C	  WRITE(37,*) 'VPOWER',VPOWER/2.         
	  PWRNW(1025) = 0.         
	  PWRNW(1026) = 0.         
	  CALL REALFT(PWRNW,NFRQ,1)         
	  DO I = 1,NPROC+1         
	    PWRNW(I) = PWRNW(I)/NFRQ         
	  ENDDO         
	  SAVE = PWRNW(2)         
	  DC = PWRNW(1)         
c	  PWRNW(2) = 0.         
	  PWRTOT = 0.         
C         
C		below, I = 1 is fundamental freuency, in pwrnw         
C         
c	DO I = 1,8         
c	  WRITE(37,*)'BEFORE',I,PWRNW(I)         
c	ENDDO         
	  DO I = 1,NFRQ         
	    PWRNW(I) = (PWRNW(2*I+1)**2 + PWRNW(2*I+2)**2)/FFUND         
	    PWRTOT = PWRTOT + PWRNW(I)         
	    IF(PWRNW(I).GT.0.) PWRNW(I) = 10.*ALOG10(PWRNW(I))         
	    write(66,*) i,ifftch,freq(i),pwrnw(i)+20.*alog10(41.1)         
	  ENDDO         
	  YAVR = 0.
	  JMAX = 1         
	  DO J = 1,NFRQ         
	    JP = J         
	    YY(JP) = PWRNW(J)         
	    XX(JP) = ALOG10(FREQ(J))         
	    IF(YY(JP).GT.YMAX) THEN         
	      YMAXR = AMAX1(YY(JP),YMAXR)         
	      JMAX = JP         
	    ENDIF         
	    YMINR = AMIN1(YY(JP),YMINR)         
	    IF(IFFTCH.LE.2) YMIN = AMAX1(-210.,YMIN)         
	    YAVR = YAVR + YY(JP)
	  ENDDO         
C	write(37,*) 'ch,spec pwr',ifftch,pwrtot,PWRTOT + .5*DC**2	         
	PRINT*,'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT         
	  PKPWR = PWRNW(JMAX)         
	  IF((JMAX-1).GT.0) PKPWR = PKPWR + PWRNW(JMAX-1)         
	  IF((JMAX+1).LT.512) PKPWR = PKPWR + PWRNW(JMAX+1)         
	  AMP = 0.         
C	  AMP = SQRT(PKPWR)         
C	  WRITE(16,803) PKPWR,FREQ(JMAX),AMP         
C	  PRINT 803,PKPWR,FREQ(JMAX),AMP         
 803	FORMAT(' 3PK PWR',E12.4,' AT',F8.2,' HZ, AMP=',E12.4)         
C         
C	  RANGE = ABS(YMAX-YMIN)         
C	  YMAX = YMAX + .05*RANGE         
C	  YMIN = YMIN - .05*RANGE	         
	  CALL MGOTICKSIZE(-1.,0.,0.,0.)         
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)         
	  CALL MGOCONNECT(XX,YY,JP)         
	  CALL MGOBOX(1,2)         
	  CALL MGOXLABEL(9,'FREQ (HZ)')         
	  IF(ISRC.LE.6) THEN         
	    CALL MGOYLABEL(14,'dB (V/m)\U2/HZ')         
	  ELSE         
	    CALL MGOYLABEL(12,'dB nT\U2/HZ')         
	  ENDIF         
C
	  CALL MGOSETEXPAND(.6)
	  CALL MGORELOCATE(X1,Y1 + .2*(Y2-Y1))                 
	  CALL MGOPUTLABEL(12,' FFT CHANNEL',3)         
	  WRITE(STR,802) IFFTCH         
	  CALL MGOPUTLABEL(3,STR,3)         
	  CALL MGOPUTLABEL(9,'  SOURCE ',3)         
	  ITEM = 'SOURCE'         
	  ok = w_item_I4(ICH, item, ISRC, 1, return_size)         
	  CALL MGOPUTLABEL(4,PA(ISRC),3)         
	  YAVR = YAVR/511.
	  CALL MGOPUTLABEL(12,' MIN,MAX,AVR ',3)
	  WRITE(STR,805) YMINR,YMAXR,YAVR
	  CALL MGOPUTLABEL(23,STR,3)
	  CALL MGOSETEXPAND(EXPSAVE)
C
	  CALL MGOSETEXPAND(.6)         
	  CALL MGOPLOTID('WINDOW','RECT.')         
	  CALL MGOSETEXPAND(1.)         
C
 200	CONTINUE
C
	IF(ITERM.LT.0) THEN         
C	  CALL MGOTIDLE         
	  CALL MGOPRNTPLOT(NVEC)         
	  PRINT*,' NO. VECTORS PLOTTED',NVEC         
	ELSE         
C	  CALL MGOTCLOSE         
	  READ(5,1001) DISPOSE
 1001	  FORMAT(A)
	ENDIF         
C         
	TMONGO = SECNDS(0.)         
 987	CONTINUE         
	DELTA = SECNDS(TMONGO)         
	IF(DELTA.LT.2.) GO TO 987         
C         
	RETURN         
C         
	END         
	SUBROUTINE TDPLOT(ICH)         
C         
C	PLOTS THE TIME DOMAIN DATA, AND THE GAIN STEP         
C		note that ich is the stream channel, not fft_channel         
C         
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,IFFTCH,isrc,scet,titlex,scet8         
	COMMON /RAWVOLTS/ RDATA(1026),RAWAVR,SLOPE
	common /angles/iantang,antang,ezang,bzantang         
	character*32 scet,item         
	character*10	titlex(15)         
	integer*4 return_size,ok,w_item_r4         
	real*8 scet8         
	real*4 gain(1024)         
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
	DIMENSION PTIME(1024),YY(1024)         
	CHARACTER*8 TITLE(4)         
	CHARACTER*120 STRG(5)         
	CHARACTER*120 STR         
	DATA TWOPI /6.2831853/         
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/         
C         
	IFCORR = 1			! CORRECTED FOR FREQ. RESPONSE         
C         
C	ITERM = 3         
	ITERM = -2         
	ITERM = -4         
C         
	PRINT*,' IN tdplot, stream chann,fft chan,source=',ich,ifftch,isrc    
	    print*,'tdplot, headblk=',major,minor,ifftch,isrc         
	FFUND = 20.3477         
	IF(IFFTCH.GT.2) FFUND = .25*FFUND         
	IF(IFFTCH.GT.6) FFUND = .333557         
	DO JF = 1,1024         
	  PTIME(JF) = (JF-1)/1.024/FFUND		!XAXIS IN mSEC         
	ENDDO         
	XRANGE = PTIME(1024) - PTIME(1)         
	XMIN = PTIME(1) - .022*XRANGE         
	XMAX = PTIME(1024) + .022*XRANGE         
C         
	  CALL MGOINIT         
	  CALL MGOSETUP(ITERM)         
	  CALL MGOERASE         
	  GYRANGE = GY2-GY1         
	IF(ITERM.LT.0) THEN         
	  PRINT*,'START tdplot',GX1,GX2,GY1,GY2         
C	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-200.)         
	  CALL MGOSETLOC(GX1,600.,GX2,GY2-200.)         
	ELSE         
	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-.08*GYRANGE)         
	ENDIF         
C         
	  CALL MGOSETEXPAND(.8)         
	  TOP = LY2         
cold	  CALL MGOGRELOCATE(GX1,TOP-50.)          
C	  CALL MGOGRELOCATE(GX1,TOP-100.)                             
	  CALL MGOGRELOCATE(GX1,TOP)
	  CALL MGOPUTLABEL(6,'SCET   ',3)         
	  CALL MGOPUTLABEL(79,SCET,3)         
	  WRITE(STR,801) MAJOR,MINOR         
 801	  FORMAT('MF.mf ',I7,'.',I3.3)         
 802	  FORMAT(2I6)         
	  CALL MGOPUTLABEL(17,STR,3)         
	  CALL MGOPUTLABEL(12,'  R8 time ',3)         
	  WRITE(STR,803) SCET8         
 803	  FORMAT(F14.8)         
	  CALL MGOPUTLABEL(14,STR,3)         
	  CALL MGOGRELOCATE(GX1,GY1+.2*(GY2-GY1))                 
	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3)         
	  WRITE(STR,802) IFFTCH         
	  CALL MGOPUTLABEL(8,STR,3)         
	  CALL MGOPUTLABEL(9,'  SOURCE ',3)         
	  IW = IFFTCH         
	  IF(IFFTCH.GT.2) IW = IFFTCH-2         
	  IF(IFFTCH.GT.6) IW = IFFTCH-6         
	  CALL MGOPUTLABEL(10,TITLEX(IW),3)         ! SOURCE IN CHARACTERS
	  CALL MGOPUTLABEL(12,' MAX,MIN,AVR',3)
	  GXSAVE = GX
	  GYSAVE = GY
C	  IF(ITERM.GT.0) THEN         
C	    CALL MGOGRELOCATE(10.,0.)                             
C	  ELSE         
C	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy         
C	  ENDIF         
	  CALL MGOSETEXPAND(1.)         
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
	IF(IFCORR.NE.1) THEN    
	  AVR = 0.     
	  DO J = 1,1024         
	    JP = J         
C	    YY(JP) = DATA(J)         
c		rdata, called data in fft_phys, is just t/m numbers         
c			multiplied by (volts per step/eff length)         
	    YY(JP) = RDATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)
	    AVR = AVR+YY(JP)         
	  ENDDO         
	ELSE         
	  DO J = 1,1024         
	    JP = J         
c		data, called vdata in fft_phys, is volts/meter         
c			corrected for frequency response         
	    YY(JP) = DATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)         
	    AVR = AVR+YY(JP)         
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
	  CALL MGOCONNECT(PTIME,YY,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOXLABEL(4,'mSEC')         
	  CALL MGOYLABEL(18,'V/M OR nT')         
	  XTITLE = .5*(XMAX+XMIN)         
	  YTITLE = YMIN + .9*(YMAX-YMIN)         
	  CALL MGORELOCATE(XTITLE,YTITLE)         
	  IF(IFCORR.EQ.1) THEN         
	    CALL MGOLABEL(28,'CORRECTED FOR FREQ. RESPONSE')         
	  ELSE         
	    CALL MGOLABEL(32,'NOT CORRECTED FOR FREQ. RESPONSE')         
	  ENDIF         
	  CALL MGOSETEXPAND(1.)         
C         
	CALL MGOSETEXPAND(.7)         
	CALL MGOPLOTID('[kellogg.wind]FFTPRO','TPLOT')         
	CALL MGOSETEXPAND(1.)         
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
	    PTIME(JF) = ANTANG + (1024-JF)*DANGD         
	  ENDDO         
	ELSE         
	  DO JF = 1,1024         
	    PTIME(JF) = JF         
	  ENDDO         
	ENDIF         
	  PRINT*,'ANGLE CHECK',IANTANG,ANTANG,PTIME(1),PTIME(1024)         
	  XRANGE = ABS(PTIME(1024) - PTIME(1))         
C	  IF(PTIME(1024).GT.PTIME(1)) THEN         
	    XMIN = PTIME(1) - .022*XRANGE         
	    XMAX = PTIME(1024) + .022*XRANGE         
C	  ELSE         
C	    XMAX = PTIME(1) + .022*XRANGE         
C	    XMIN = PTIME(1024) - .022*XRANGE         
C	  ENDIF	         
	  IF(ITERM.LT.0) THEN         
c	    TOP = GY1 - .01*GYRANGE         
	    TOP = GY1 - .07*GYRANGE         
	    BOTT = 250.          
	    CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
	  ELSE         
	    CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
	  ENDIF         
C         
	  CALL MGOSETLIM(XMIN,-.5,XMAX,3.5)         
	IF(IANTANG.NE.0) THEN         
	  CALL MGOXLABEL(17,'ANGLE, SUN TO +EX')         
	  CALL MGOTICKSIZE(10.,90.,1.,1.)         
	ELSE         
	  CALL MGOXLABEL(10,'SAMPLE NO.')         
	  CALL MGOTICKSIZE(20.,200.,1.,1.)         
	ENDIF         
	  CALL MGOCONNECT(PTIME,GAIN,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOYLABEL(18,'GAIN STEP')         
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
	SUBROUTINE SPECLTPLOT(ICH)         
C         
C	PLOTS THE TIME DOMAIN DATA, AND THE GAIN STEP         
C		note that ich is the stream channel, not fft_channel         
C		this version plots only ex on a fixed scale for         
C		comparisons         
C         
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,IFFTCH,isrc,scet,titlex,scet8         
	COMMON /RAWVOLTS/ RDATA(1026),RAWAVR,SLOPE
	character*32 scet,item         
	character*10	titlex(15)         
	integer*4 return_size,ok,w_item_r4         
	real*8 scet8         
	real*4 gain(1024)         
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
	DIMENSION PTIME(1024),YY(1024)         
	CHARACTER*8 TITLE(4)         
	CHARACTER*120 STRG(5)         
	CHARACTER*120 STR         
	DATA TWOPI /6.2831853/         
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/         
C         
C	ITERM = 3         
	ITERM = -2         
	ITERM = -4         
C         
	PRINT*,' IN tdplot, chann,source=',ich,isrc         
	FFUND = 20.3477         
	IF(IFFTCH.GT.2) FFUND = .25*FFUND         
	IF(IFFTCH.GT.6) FFUND = .333557         
	DO JF = 1,1024         
	  PTIME(JF) = (JF-1)/1.024/FFUND		!XAXIS IN mSEC         
	ENDDO         
	XRANGE = PTIME(1024) - PTIME(1)         
	XMIN = PTIME(1) - .022*XRANGE         
	XMAX = PTIME(1024) + .022*XRANGE         
C         
	  CALL MGOINIT         
	  CALL MGOSETUP(ITERM)         
	  CALL MGOERASE         
	  GYRANGE = GY2-GY1         
	IF(ITERM.LT.0) THEN         
	  PRINT*,'START tdplot',GX1,GX2,GY1,GY2         
C	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-200.)         
	  CALL MGOSETLOC(GX1,600.,GX2,GY2-200.)         
	ELSE         
	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-.08*GYRANGE)         
	ENDIF         
C         
	  CALL MGOSETEXPAND(.8)         
	  TOP = LY2         
cold	  CALL MGOGRELOCATE(GX1,TOP-50.)                             
	  CALL MGOGRELOCATE(GX1,TOP-100.)                             
	  CALL MGOPUTLABEL(6,'SCET   ',3)         
	  CALL MGOPUTLABEL(79,SCET,3)         
	  WRITE(STR,801) MAJOR,MINOR         
 801	  FORMAT('MF.mf ',I7,'.',I3.3)         
 802	  FORMAT(2I6)         
	  CALL MGOPUTLABEL(17,STR,3)         
	  CALL MGOPUTLABEL(12,'  R8 time ',3)         
	  WRITE(STR,803) SCET8         
 803	  FORMAT(F14.8)         
	  CALL MGOPUTLABEL(14,STR,3)         
	  CALL MGOGRELOCATE(GX1,TOP-160.)                 
	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3)         
	  WRITE(STR,802) IFFTCH         
	  CALL MGOPUTLABEL(8,STR,3)         
	  CALL MGOPUTLABEL(10,'   SOURCE ',3)         
	  IW = IFFTCH         
	  IF(IFFTCH.GT.2) IW = IFFTCH-2         
	  IF(IFFTCH.GT.6) IW = IFFTCH-6         
	print*,'in specltplot, ifftch,iw=',ifftch,iw         
	  CALL MGOPUTLABEL(10,TITLEX(IW),3)         
	  CALL MGOSETEXPAND(1.)         
	  YMIN = 1.E6         
	  YMAX = -YMIN         
	  JP = 1         
	  DO J = 1,1024         
	    JP = J         
c	    YY(JP) = FDATA(J)         
C	    YY(JP) = DATA(J)         
	    YY(JP) = RDATA(J)         
c	WRITE(65,*) JP,J,DATA(J),RDATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)         
	  ENDDO         
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
C	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)         
	  Ymin = -.01         
	  Ymax =  .01         
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)         
	  CALL MGOCONNECT(PTIME,YY,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOXLABEL(4,'mSEC')         
	  CALL MGOYLABEL(18,'V/M OR nT')         
	  CALL MGOSETEXPAND(1.)         
C         
	CALL MGOSETEXPAND(.7)         
	CALL MGOPLOTID('[kellogg.wind]FFTPRO','SPECLTPLOT')         
	CALL MGOSETEXPAND(1.)         
C         
C	PLOT GAIN STEP         
C         
	IF(IFFTCH.GT.6.AND.ISRC.EQ.4) THEN         
	  ITEM = 'WIND_SPIN_RATE_R4'         
	  OK = W_ITEM_R4(ICH,ITEM,SPINRATE,1,ISIZE)         
	  SAMP_PER_SEC = 1024.*FFUND         
	  DANG = SPINRATE/SAMP_PER_SEC			! IN RADIANS         
	  DANGD = DANG*360./TWOPI         
	  SAMP_PER_SPIN = TWOPI/DANG         
C	  NSPPS = SAMP_PER_SPIN + .5			! ROUNDOFF         
	  CALL FFTLOANGLE(ICH,DATA,SUNCLOCK,ANTANG)         
	  PRINT*,'FFTLOANGLE CALLED FROM SPECTPLOT'         
	ENDIF         
          
	item = 'EXPONENT'         
	ok = w_item_r4(ich, item, gain, 1024, return_size)         
	PRINT*,'START gain',GX1,GX2,GY1,GY2         
	IF(IFFTCH.GT.6.AND.ISRC.EQ.4) THEN         
	  DO JF = 1,1024         
	    PTIME(JF) = ANTANG + (1024-JF)*DANGD         
	  ENDDO         
	  PRINT*,'ANGLE CHECK',PTIME(1),PTIME(1024)         
	ELSE         
	  DO JF = 1,1024         
	    PTIME(JF) = JF         
	  ENDDO         
	ENDIF         
	  XRANGE = ABS(PTIME(1024) - PTIME(1))         
	  IF(PTIME(1024).GT.PTIME(1)) THEN         
	    XMIN = PTIME(1) - .022*XRANGE         
	    XMAX = PTIME(1024) + .022*XRANGE         
	  ELSE         
	    XMAX = PTIME(1) + .022*XRANGE         
	    XMIN = PTIME(1024) - .022*XRANGE         
	  ENDIF	         
	  IF(ITERM.LT.0) THEN         
c	    TOP = GY1 - .01*GYRANGE         
	    TOP = GY1 - .07*GYRANGE         
	    BOTT = 250.          
	    CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
	  ELSE         
	    CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
	  ENDIF         
C         
	  CALL MGOSETLIM(XMIN,-.5,XMAX,3.5)         
	IF(IFFTCH.GT.6.AND.ISRC.EQ.4) THEN         
	  CALL MGOXLABEL(17,'ANGLE, SUN TO +EX')         
	  CALL MGOTICKSIZE(10.,90.,1.,1.)         
	ELSE         
	  CALL MGOXLABEL(10,'SAMPLE NO.')         
	  CALL MGOTICKSIZE(20.,200.,1.,1.)         
	ENDIF         
	  CALL MGOCONNECT(PTIME,GAIN,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOYLABEL(18,'GAIN STEP')         
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
	SUBROUTINE RAWPRINT         
C         
	common /fftblk/ nexpt(1024),ndata(1024),ipwrdb(1024)         
	common /headblk/ major,minor,ICH,isrc,s_scet,title,scet8         
	real*8 scet8         
	character*32 scet         
	character*10	title(15)         
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
	SUBROUTINE PUBSHPLOT(NCHGP,ICH)         
C         
C	THIS IS LIKE SHADE, BUT MAKES LANDSCAPE PLOTS, PREFERABLY         
C		FFTH, AND A WHOLE DAY ON A PAGE, OR MORE.         
C         
	character*32 s_scet         
	CHARACTER*1 DISPOSE         
	integer*2 NPWRL         
	CHARACTER*10 TITLE(15)         
	CHARACTER*120 STR         
	REAL FUNDF(3)         
	real*8 scet8         
	integer*4 fft_channel         
C	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4)         
	COMMON /SHBLK/ PTIME(1000),NPWRL(500,511,4)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8         
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
C	DIMENSION PWRL(511000)         
C	DIMENSION PWRL(102200)         
	DIMENSION PWRL(255500)         
	DATA FUNDF /.021348,.005337, .33356/         
C         
C         
	NWNDOW = 2         
	IF(NCHGP.NE.1) NWNDOW = 4         
	ITERM = -3		! SAVE         
C	ITERM = 3         
	XSTART = 400.         
	XEND = 2300.         
	XEND = 2175.		! to match kyoto AE plots         
	IW = 1         
	NPT = IPNT(IW)         
	IF(NPT.LT.1) NPT = 1             	  ! protection         
	PRINT*,' IN SHPLOT,NPT',NPT         
	PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NPT),NPT         
C	CALCULATE BOX SIZE         
	PIXSIZ = .1         
	IF(NPT.GT.1) PIXSIZ = (PTIME(NPT) - PTIME(1))/(NPT-1)         
	HSTART = PTIME(1) - .5*PIXSIZ         
	HEND =   PTIME(NPT) + .5*PIXSIZ         
	PRINT*,' IN SHPLOT',HSTART,HEND,NPT         
	  CALL MGOINIT         
	  CALL MGOSETUP(ITERM)         
	  CALL MGOERASE         
C         
	         
	IF(ITERM.LT.0) THEN         
C	  CALL MGOSETLOC(XSTART,280.,XEND,3100.)         
	  CALL MGOSETLOC(XSTART,985.,XEND,2300.)         
	ENDIF         
C         
	IW = ICH         
C	DO IW = 1,NWNDOW         
C	  CALL MGOWINDOW(1,NWNDOW,IW)                          !          
	  YMIN = 1.E6         
	  YMAX = -YMIN         
	  CALL HISTOG(2,TJUNK,256,-250.,0.,.01,TOTAL,RET)     ! CLEAR AND INIT         
	  DO M = 1,NPT         
	  DO N = 1,511         
	    NM = M + (N-1)*NPT         
	    PWRL(NM) = NPWRL(M,N,IW)         
	    CALL HISTOG(1,PWRL(NM),256,-250.,0.,.5,TOTAL,RET)    ! LOAD ARRAY         
C         
	    YMIN = AMIN1(YMIN,PWRL(NM))         
	    YMAX = AMAX1(YMAX,PWRL(NM))         
	  ENDDO         
	  ENDDO         
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX         
C         
C	  CALL HISTOG(0,TJUNK,256,-250.,0.,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE         
C	  CALL HISTOG(0,TJUNK,256,-250.,0.,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE         
	  CALL HISTOG(0,TJUNK,256,-250.,0.,.02,TOTAL,YHMIN)   !DETERMINE 2 PCTILE         
	  CALL HISTOG(0,TJUNK,256,-250.,0.,.98,TOTAL,YHMAX)  !DETERMINE 98 PCTILE         
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER         
C	  LOWERING YMAX MAKES THE SIGNAL DARKER         
C         
	  YAMIN = YHMIN          
	  YAMAX = YHMAX         
C	  YAMIN = YHMIN - 5.         
C	  YAMAX = YHMAX + 10.         
	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX         
c	  arguments are: array(m,n),m,n,white,black,linearity         
c	  m is the x direction         
	  CALL MGOHALFTONE(PWRL,NPT,511,YAMIN,YAMAX,1.E7)         
	  CALL MGOSETEXPAND(.7)         
	  FMIN = FUNDF(NCHGP)         
	  FMAX = 511.*FMIN         
	  CALL MGOSETLIM(HSTART,FMIN,HEND,FMAX)         
	  IF(NCHGP.EQ.3) THEN         
	    CALL MGOYLABEL(13,'FREQUENCY, Hz')	         
	  ELSE         
	    CALL MGOYLABEL(14,'FREQUENCY, kHz')	         
	  ENDIF         
	  CALL MGOTICKSIZE( .16667, 1., 0., 0.)         
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
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//         
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)         
            WRITE(STR,1001) S_SCET(1:10),IDOY         
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
C	  IF(IW.EQ.NWNDOW) THEN         
	    CALL MGOSETEXPAND(.8)         
	    CALL MGOPLOTID('FFTPRO 3.1','SHPLOT')         
	    CALL MGOSETEXPAND(1.)         
C	  ENDIF         
C	ENDDO         
C         
	  CALL MGOSETEXPAND(1.)         
	  IF(ITERM.LT.0) THEN         
	    CALL MGOPRNTPLOT(NVEC)         
	    PRINT*,' NO. VECTORS PLOTTED',NVEC         
	  ELSE         
	    READ(5,1234) DISPOSE         
 1234	    FORMAT(A)         
	    CALL MGOTCLOSE         
	  ENDIF         
C         
	RETURN         
	END         
	SUBROUTINE MAKEBCKG(NCHGP,NPT)
C
	COMMON /SHBLK/ PTIME(1000),NPWRL(500,511,4)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8         
C
C	USES HISTOGRAM ON EACH FREQUENCY TO DETERMINE A BACKGROUND
C
	  YMIN = 1.E6         
	  YMAX = -YMIN         
	  IW = 1
	  DO N = 1,511         
	    CALL HISTOG(2,TJUNK,256,-250.,0.,.01,TOTAL,RET)   ! CLEAR AND INIT 
	    DO M = 1,NPT         
C	      NM = M + (N-1)*NPT         
	      PWRL = NPWRL(M,N,IW)         
	      CALL HISTOG(1,PWRL,256,-250.,0.,.5,TOTAL,RET)    ! LOAD ARRAY         
C         
	      YMIN = AMIN1(YMIN,PWRL)         
	      YMAX = AMAX1(YMAX,PWRL)         
	    ENDDO         
	  ENDDO         
	  PRINT*,'FREQ N, YMIN,MAX ACTUAL',N,YMIN,YMAX         
C         
	  CALL HISTOG(0,TJUNK,256,-250.,0.,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE         
	  CALL HISTOG(0,TJUNK,256,-250.,0.,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE         
C
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER         
C	  LOWERING YMAX MAKES THE SIGNAL DARKER         
C
	  PRINT*,'FREQ N, YMIN,MAX HISTO ',N,YHMIN,YHMAX         
	RETURN
	END
