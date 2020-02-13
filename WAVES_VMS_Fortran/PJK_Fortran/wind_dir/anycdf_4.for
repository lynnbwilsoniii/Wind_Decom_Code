	program		anycdf

!	plots any item of wind/waves data or CDF data

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
	character*8	YYYYMMDD
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

	common /fftblk/ gain,mantissa,pwrdb
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY
	COMMON /SHBLK/ PTIME(1000)
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),VDATA
	common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8
!
	DATA ITMCORR /0/             ! 1 = CORRECT FOR TIME, 0 = PLOT
C					CONSECUTIVE SPECTRA
	data ncount /0/
	DATA IPNT /4*0/
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1		'BX','BY','BZ'/
	DATA JP /0/
	DATA IRAW /0/
	DATA TWOPI /6.2831853/
	DATA NTPLOTS /0/
C
	write(6,*) 'type year,month,day of month  e.g. 19960419'
	READ(5,A) YYYYMMDD
	WRITE(STREAM,30) YYYYMMDD
 30	FORMAT('wi_lz_wav_',A8,'_v*.dat')
	PRINT*,'in get_stream_name, file= ',STREAM

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
	print*,'spinrate',spinrate
	write(88,*) 'phase,scet,rate',phase,scetspin,spinrate
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
		print*,'rawcount',rawcount
		print*,'fftcount',fftcount
		stop
	  endif
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
	      if (err_count .ge. 2) goto 200
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
	
	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
c	  	CALL TSPPLOT(MAIN_CH)
c		call tplot(ch,isrc)
		print*,'fftcount',fftcount
		print*,'rawcount',rawcount
C	   IF(1) STOP
	return
	end
