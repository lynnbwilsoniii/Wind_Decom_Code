! trad2a.for - tests the get_event stuff and the dbms stuff for rad2 stuff

	program		test
	implicit	none
	integer*4	ok
	integer*4	ch, major, minor
	character*8	event
	parameter	(event='RAD2')
	integer*4	size
	parameter	(size=1024)
	integer*4	s(size)
	integer*4	z(size)
	integer*4	s_prime(size)
	integer*4	ret_size
	integer*4	f(size)
	integer*4	f_step(size)
	integer*4	fret_size
	integer*4	tds_fill_pointer
	character*20	item
	integer*4	i,j,k,m,n,o
	character*80	file
	integer*4	error_count
	integer*4	event_number
	integer*4	wind_tm_open_channel
	integer*4	wind_tm_get_mfmf
	integer*4	wind_tm_get_event
	integer*4	w_event
	integer*4	wind_tm_get_item
	integer*4	wind_tm_increment_packet
	integer*4	wind_tm_close_channel
	integer*4	w_item_r8 !xxxxx pragma C (w_item_r8)
	integer*4	w_channel_select
	real*8		scet
	integer*4	dpu_mjr, dpu_mnr
	character*20	ver
	character*1	c1
	logical*4	keep_going
	real*8		t1, t2

	write(6,'(1x,a,$)') 'Enter logon [offline,file,time...]: '
	read(5,*) file
	if (file(1:4) .ne. 'time') then
	   ok = wind_tm_open_channel(ch,file)
	else
	   t1 = 0.0
	   t2 = 0.0
	   ok = w_channel_select(ch,'wind', t1, t2)
	end if
	if (ok .ne. 1) stop 'cannot open tm channel'

	call w_version(ver)
	call w_channel_filename(ch,file)
	type *, ' '
	type *, 'Using Wind_lib version ', ver
	type *, 'File: ', file
	type *, ' '

	ok = wind_tm_get_mfmf(ch,major,minor)
	if (ok .ne. 1) stop 'cannot get stream position'
	ok = wind_tm_increment_packet(major,minor)

 1000	continue

	type *, '___calling get event___'
	ok = wind_tm_get_event(ch,major,minor,event)
!	ok = w_event(ch,event)
	if (ok .ne. 1) then
	   type *, 'cannot get event at MF.mf:', major, minor
	   if (ok .eq. 82) stop 'EOF'
	   error_count = error_count + 1
	   ok = wind_tm_increment_packet(major,minor)
	   if (error_count .lt. 25) goto 1000
	   stop 'STOP, too many errors.'
	end if

  4	format(1x,a,' Event #', i5,' ending at MF.mf', i8,'.',i3.3)
	event_number = event_number + 1
	type *, '-------------------------------------------------------------'
	type 4, event, event_number, major, minor
	   
	item = 'EVENT_SCET_R8'
	ok = w_item_r8(ch, item, scet, 1, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,1x,f16.10)', ' item:=', item, scet 
	end if

	item = 'DPU_MAJOR_FRAME'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i9)', ' item:=', item, s(1) 
	end if

	item = 'DPU_MINOR_FRAME'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'PACKET_SUBTYPE'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if
	if (s(1) .ne. 3) goto 3000

	item = 'FREQ_TABLE'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'XLATE_TABLE'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'STEPS'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'SUM_LOOP'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'GROUP_LOOP'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	   n = 2
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	   n = s(1)
	end if

	item = 'GROUP_SIZE'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	   j = 19
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	   j = s(1)
	end if

	item = 'AUTO_MASK'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'XLAT_MASK'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

	item = 'SUM_FLAG'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, s(1) 
	end if

!	item = 'FREQ_LIST'

	item = 'FREQUENCIES'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, ret_size
	   type '(<j>(1x,i3))', (s(i), i=1,j*n*2)
	end if

	item = 'TOGGLE_LIST'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,i4)', ' item:=', item, ret_size
	   type '(<j>(1x,i1))', (s(i), i=1,j*n*2)
	end if

!	goto 4000
 3000	continue
	item = 'SUN_CLOCK'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,z8.8,1x,z8.8)', ' item:=', item, s(1), s(2)
	end if

	item = 'SUN_ANGLE'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,z8.8)', ' item:=', item, s(1)
	end if

	item = 'SUN_SPINS'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) then
	   type *, 'cannot get item ', item, ', ok=', ok
	else
	   type '(a,a,z8.8)', ' item:=', item, s(1) 
	end if

!	item = 'PACKET_SUBTYPE'
!	ok = wind_tm_get_item(ch, item, s, size, ret_size)
!	if (ok .ne. 1) stop 'cannot get item PACKET_SUBTYPE'
!	type *, 'item:=', item, S(1), ', return_size:', ret_size

	item = 'S'
	ok = wind_tm_get_item(ch, item, s, size, ret_size)
	if (ok .ne. 1) stop 'cannot get item S'
	type *, 'item:', item, ', return_size:', ret_size

	item = 'S_PRIME'
	ok = wind_tm_get_item(ch, item, s_prime, size, ret_size)
	if (ok .ne. 1) stop 'cannot get item S_PRIME'
	type *, 'item:', item, ', return_size:', ret_size

	item = 'Z'
	ok = wind_tm_get_item(ch, item, z, size, ret_size)
	if (ok .ne. 1) stop 'cannot get item Z'
	type *, 'item:', item, ', return_size:', ret_size

	item = 'FREQUENCIES'
	ok = wind_tm_get_item(ch, item, f, size, ret_size)
	if (ok .ne. 1) stop 'cannot get item PACKET_TYPE'
	type *, 'item:', item, ', return_size:', ret_size

!	item = 'FREQUENCY_STEP'
!	ok = wind_tm_get_item(ch, item, f_step, size, ret_size)
!	if (ok .ne. 1) stop 'cannot get item PACKET_TYPE'
!	type *, 'item:', item, ', return_size:', ret_size
!
!	item = 'TDS_FILL_POINTER'
!	ok = wind_tm_get_item(ch, item, tds_fill_pointer, 1, ret_size)
!	if (ok .ne. 1) stop 'cannot get item PACKET_TYPE'
!	type *, 'item:', item, ', return_size:', ret_size
!
!	type *, 'Major=', major
!	type *, 'Minor=', minor
!	type *, 'TDS_FILL_POINTER=', tds_fill_pointer
!	type 1, '     S', '  S_prime', '     Z', 'Frequency', 'Freq_Step'
!	type 2
!	do i=1,10 ! 260
!	   type 3, i, s(i), s_prime(i), z(i), f(i), f_step(i)
!	end do
!	type *, ' '

!	call wind_iar_show_eei(ch)
!	call wind_iar_show_eb(ch)

 4000	continue
	ok = wind_tm_increment_packet(major,minor)

	type *, ' '
 10	write(6,'(1x,a,$)') 'Get another event [Y/N]? '
	read(5,'(a)',iostat=o,err=10) c1
	keep_going = .not. (c1 .eq. 'N' .or. c1 .eq. 'n')
	type *, ' '
	if (keep_going) goto 1000

	ok = wind_tm_close_channel(ch)
	if (ok .ne. 1) stop 'cannot close channel'

 1	format(1x,'Obvs', 5(1x,a12))
 2	format(1x,'----', 5(1x,'------------'))
 3	format(1x,i4,     5(1x,i12))
	end
