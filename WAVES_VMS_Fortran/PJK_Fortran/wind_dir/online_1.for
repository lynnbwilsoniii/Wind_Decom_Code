! sample wind/waves fft data collection program

	program		get_fft
	implicit	integer*4 (a-z)

	ok = 1
	if (ok) ok = get_tm_stream()
	if (ok) ok = wait_for_events()

	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_tm_stream()
	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	parameter	event='FFT'
	integer*4	i,j,k,n
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*16	stream
	parameter	size=1024
	integer*4	return_size
	integer*4	raw(size)
	integer*4	mantissa(size)
	integer*4	gain(size)
	integer*4	fft_channel
	character*32	s
	character*32	s_scet
	integer*4	scet(2)
	integer*4	ch, major, minor
	character*80	file
	character*32	item
	integer*4	ios

	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'

	ok = wind_tm_open_channel(ch,stream)
	if (.not. ok) stop 'cannot open tm channel'

	ok = wind_tm_get_filename(ch,file)

	ok = wind_tm_get_mfmf(ch,major,minor)
	if (.not. ok) stop 'cannot get stream position'

	get_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events()

	! this is the main program loop

	do while(.not. wind_tm_eof(ch,major,minor))
 100	   ok = wind_tm_get_next_event(ch,major,minor,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
	      if (wind_tm_realtime(ch)) then
	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
	         type *, '-reset to latest position: ', major, minor
	      else
	         call wind_tm_increment_packet(major,minor)
	         type *, '-incrementing packet...'
	      end if
	      err_count = err_count + 1
	      if (err_count .lt. 2) goto 100
	      if (err_count .ge. 2) goto 200
	   end if

	   ! now get the items
	   item = 'CHANNEL_NUMBER'
	   ok = wind_tm_get_item(ch, item, fft_channel, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'POWER_SPECTRUM'
	   ok = wind_tm_get_item(ch, item, raw, size, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'MANTISSA'
	   ok = wind_tm_get_item(ch, item, mantissa, size, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'EXPONENT'
	   ok = wind_tm_get_item(ch, item, gain, size, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'EVENT_SCET'
	   ok = wind_tm_get_item(ch, item, scet, 2, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

	   ! type the report
	   type *, '--------------------------------------------------'
	   type *, 'FFT Ch ', fft_channel
	   type *, 'SCET ', s_scet
	   type *, 'Offline file: ', file
	   do i=1,size
	      type *, i, raw(i), mantissa(i), gain(i)
	   end do

	end do

 200	continue
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	integer*4	iq

  6	format(1x,'Enter TM stream type [O=offline, R=realtime (default)]: ',$)
  5	format(q,a)

 10	write(6,6)
	read(5,5,err=10,end=20) iq, stream

	if (iq .lt. 1) then
	   stream = 'realtime'
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
