! tds_items.for - 

	program		tds_items
	implicit	integer*4 (a-z)

	ok = get_tm_stream()
	if (ok.eq.1) ok = wait_for_events()

	end


	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_tm_stream()
	implicit	none
	integer*4	ok
	parameter	event='TDSF'
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*16	stream
	integer*4	ch, major, minor
	character*128	file
	integer*4	wind_tm_open_channel
	integer*4	wind_tm_get_filename
	integer*4	wind_tm_get_mfmf
	integer*4	wind_tm_get_next_event

	stream = 'offline'

	ok = wind_tm_open_channel(ch,stream)
	type *, 'ok=',ok
	if (ok.ne.1) stop 'cannot open tm channel'

	ok = wind_tm_get_filename(ch,file)

	ok = wind_tm_get_mfmf(ch,major,minor)

	get_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events()
	! this is the main program loop

	ok = 1
	type *, 'Waiting for first event...'
	do while(ok.eq.1)
 100	   ok = wind_tm_get_next_event(ch,major,minor,event)
	   if (ok.ne.1) then
	      type 1, 'Cannot get event at MF.mf: ', major, minor
	      call wind_tm_increment_packet(major,minor)
	      type 1, '-incremented packet to: ', major, minor
	      err_count = err_count + 1
	      if (err_count .lt. 5) goto 100
	   else
	      call print_all_tds_items(ch,major,minor)
	   end if
	   err_count = 0
	end do

	return
  1	format(1x,'TDS: ', a, i8, '.', i3.3)
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	print_all_tds_items(ch,major,minor)
	implicit	none
	integer*4	ok
	integer*4	v, v2
	character*16	item
	character*32	string
	integer*4	ret_size
	integer*4	ch, major, minor
	integer*4	ios
	integer*4	imajor, iminor, scet(2)
	parameter	event='TDSF'
	integer*4	wind_tm_get_item
	integer*4	wind_tm_xlate_item

	type *, ' '
	write(6,'(80(''-''))') 

	item = 'ERT_MAJOR_FRAME'
	ok = wind_tm_get_item(ch, item, imajor, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok

	item = 'ERT_MINOR_FRAME'
	ok = wind_tm_get_item(ch, item, iminor, 1, ret_size)
	if (ok.ne.1) type *, 'error getting ', item, ', ok=', ok

	item = 'EVENT_SCET'
	ok = wind_tm_get_item(ch, item, scet, 2, ret_size)
	if (ok.ne.1) type *, 'error getting ', item, ', ok=', ok
	type 4, item, scet(1), scet(2)

	item = 'CHANNEL'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok.ne.1) type *, 'error getting ', item, ', ok=', ok

	item = 'EVENT_NUMBER'
	ok = wind_tm_get_item(ch, item, v2, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok

	write(6,3,iostat=ios) v, v2, imajor, iminor, major, minor

	item = 'CHANNEL'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'DATA'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'DPU_CLOCK'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'EVENT_MASK'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'EVENT_NUMBER'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'EVENT_SWEEPS'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'FAST_RX_FILTER'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'FAST_RX_SPEED'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'FAST_RX_TRIGGER'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'FAST_TRIG_CHAN'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'FAST_TRIG_MODE'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'FAST_TRIG_THRSH'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v, string

	item = 'INSTR_HEADER'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

!	item = 'MAJOR_FRAME'
!	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
!	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
!	type 1, item, v

	item = 'MAX_STATE'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'MEASURE_HEADER'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

!	item = 'MINOR_FRAME'
!	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
!	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'PACKET_COUNT'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'PACKET_HEADER'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'PACKET_ID'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'QUALITY'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'SLOW_RX_FILTER'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SLOW_RX_SPEED'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SLOW_RX_TRIGGER'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SLOW_TRIG_CHAN'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v

	item = 'SLOW_TRIG_MODE'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v

	item = 'SLOW_TRIG_THRSH'
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	type 1, item, v

	item = 'SOURCE_CHAN_1'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SOURCE_CHAN_2'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SOURCE_CHAN_3'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SOURCE_CHAN_4'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SOURCE_CHAN_5'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'SOURCE_CHAN_6'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	item = 'TANDEM_STATE'
	string = ' '
	ok = wind_tm_get_item(ch, item, v, 1, ret_size)
	if (ok .ne. 1) type *, 'error getting ', item, ', ok=', ok
	ok = wind_tm_xlate_item(ch,event,item,v,string)
	type 1, item, v, string

	return
   1	format(1x,a16,2x,i,4x,a)
   2	format(1x,a16,2x,z8.8)
   3	format(1x,'CH#',i1,', EV#', i10, ', ERT Start MF.mf:', i8, '.', i3.3,
	1	', End MF.mf:',i8,'.',i3.3,'.')

   4	format(1x,a16,2x,i,2x,i)
 10	type *, 'cannot get item, ok=', ok
	return
	end
