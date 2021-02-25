! wind_event_lib.for - portion of wind_lib refering to event getting
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Event Retrieval routines
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
	integer*4	function	w_event(ch,event)
! This is a convenience routine for application programs that move
! strictly forward in a stream gathering successive events.
! Eventually wind_tm_get_event is called to extract the event from the stream.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'

	integer*4	ch				! TM channel number
	character*(*)	event				! event type

$IF ABSOFT_FORTRAN
	character*(*)	rn,rn2
	parameter	(rn='W_EVENT')			! name of this routine
	parameter	(rn2='W_CHANNEL_POSITION')	! name of this routine
$ELSE
	parameter	rn='W_EVENT'			! name of this routine
	parameter	rn2='W_CHANNEL_POSITION'	! name of this routine
$ENDIF
	integer*4	wind_tm_get_next_event		! a function
	integer*4	ios
	integer*4	ok, ok2
	integer*4	mjr,mnr
	structure /saved_context/
	   integer*4	mjr
	   integer*4	mnr
	   integer*4	n_opens
	end structure
	record /saved_context/ scxt(max_channels)
	integer*4	wind_tm_mfmf_to_scet		! a function
	integer*4	wind_tm_scet_to_mfmf		! a function
$IF ABSOFT_FORTRAN
	integer*4	o
$ELSE
	integer*4	i,o
$ENDIF
	integer*4	w_channel_position		! an entry point
	integer*4	w_status			! an entry point
	integer*4	last_status(max_channels)
	real*8		t
	real*8		r8
	character*256	f
	logical*4	loop
	integer*4	loop_count
	integer*4	return_code
	integer*4	w_channel_reopen
	integer*4	w_ur8_to_x_filename

	w_event = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) goto 90

	if (scxt(ch).n_opens .ne. user(ch).n_file_opens) then
	   scxt(ch).mjr = user(ch).first_major
	   scxt(ch).mnr = user(ch).first_minor.i4val
	   scxt(ch).n_opens = user(ch).n_file_opens
	end if

	ok = 0
	do while(ok .ne. 1)
	   ok = wind_tm_get_next_event(
	1	ch,
	1	scxt(ch).mjr,
	1	scxt(ch).mnr,
	1	event)
	   last_status(ch) = ok
	   if (ok .eq. w_end_of_file) then
	      w_event = ok
	      return
	   else if (ok .ne. 1) then
	      write(6,2,iostat=o) 'skipping bad/partial event'
	   end if
	end do
	w_event = ok

	! synchronize the MF.mf position with a UR8 time value
	if (ok .eq. 1) then
	   exi(ch).ur8_position = exi(ch).ur8_eoe
	else
	   ok2 = wind_tm_mfmf_to_scet(ch,scxt(ch).mjr,scxt(ch).mnr,r8)
	   if (ok2 .ne. 1) then
	      if (ok .ne. w_end_of_file) goto 92
	      ! coerce r8 to eof time
	      mjr = user(ch).last_major
	      mnr = user(ch).last_minor.i4val
	      ok = wind_tm_mfmf_to_scet(ch,mjr,mnr,r8)
	   end if
	   exi(ch).ur8_position = r8
	end if

	return
  2	format(1x,'W_EVENT: ',a)
  9	format(1x,a,':',1x,a)
 90	write(6,9,iostat=ios) rn, 'invalid channel number.'
	w_event = w_bad_channel
	return
 92	continue
	w_event = ok
	if (user(ch).suppress_messages) return
	write(6,9,iostat=ios) rn, 'error synching MF.mf to UR8 time'
	return

	!----------------------------------------------------------------------
	entry	w_status(ch)
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 200
	w_status = last_status(ch)
	return
 200	write(6,9,iostat=ios) 'W_STATUS', 'invalid channel number.'
	! this is ambiguous, can also be last return code from w_event
	w_status = w_bad_channel
	return

	!----------------------------------------------------------------------
	entry	w_channel_position(ch, t)

	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10

	exi(ch).ur8_context  = t	! ...allows independent cdf access 

	return_code = 1
	loop_count = 0
 1000	continue
	loop = .false.
	loop_count = loop_count + 1
	if (t .eq. 0.0 .or. t .eq. exi(ch).stream_bod) then
	   !
	   ! goto earliest position in current file
	   !
	   mjr = user(ch).first_major
	   mnr = user(ch).first_minor.i4val
	   ok = wind_tm_mfmf_to_scet(ch,mjr,mnr,t)
	   if (ok .eq. w_bad_rec_quality_flag) then
	      ! error already reported, allow user to position on bad record
	      ! may eventually want to advance to valid data...no, w_event()
	      ! will do that...
	   else if (ok .ne. 1) then
	      goto 20
	   end if
	   ! ...don't let context time goto zero
	   if (exi(ch).ur8_context .eq. 0.0d0)
	1     exi(ch).ur8_context = exi(ch).stream_bod
	   !
	else if (t .eq. -1.0) then
	   !
	   ! goto latest position in current file
	   !
	   mjr = user(ch).last_major
	   mnr = user(ch).last_minor.i4val
	   ok = wind_tm_mfmf_to_scet(ch,mjr,mnr,t)
	   if (ok .eq. w_bad_rec_quality_flag) then
	      ! error already reported, allow user to position on bad record
	      ! may eventually want to advance to valid data...no, w_event()
	      ! will do that...
	   else if (ok .ne. 1) then
	      goto 50
	   end if
	   ! ...don't let context time goto -1
	   if (exi(ch).ur8_context .eq. -1.0d0)
	1     exi(ch).ur8_context = exi(ch).stream_bod + 1.0
	   !
$IF ABSOFT_FORTRAN
	else if (t .ge. exi(ch).stream_boi .and. t .le.
	1	exi(ch).stream_eoi) then
$ELSE
	else if (t .ge. exi(ch).stream_boi .and. t .le. exi(ch).stream_eoi) then
$ENDIF
	   !
	   ! goto position within current file
	   !
	   ok = wind_tm_scet_to_mfmf(ch,mjr,mnr,t)
	   if (ok .ne. 1) goto 30
	   !
	else if (t .gt. exi(ch).stream_bod .and.
	1        t .lt. exi(ch).stream_boi) then
	   ! in a gap between beginning of day and beginning of info
	   t = 0.0
	   loop = .true.
	else if (t .gt. exi(ch).stream_eoi .and.
	1        t .le. exi(ch).stream_eod) then
	   ! in a gap between end of info and end of day
	   t = exi(ch).stream_bod + 1.0
	   loop = .true.
	else if (t .ge. exi(ch).stream_bow .and.
	1        t .le. exi(ch).stream_eow) then
	   !
	   ! within the defined window, go to a new file
	   !
	   ok = w_ur8_to_x_filename(t, cdhf_lz_stream, f)
	   if (ok .ne. 1) goto 40
	   ok = w_channel_reopen(ch, f)
	   if (ok .ne. 1) goto 44
	   loop = .true.
	else
	   ! assume the caller is only interested in changing the context
	   if (t .gt. exi(ch).stream_eow) then
	      t = -1.0
	      loop = .true.
	      return_code = w_end_of_file
	      call w_msg_put(ch,rn2,': time is after end of window')
	   else if (t .lt. exi(ch).stream_bow) then
	      t = 0.0
	      loop = .true.
	      return_code = w_beginning_of_file
	      call w_msg_put(ch,rn2,': time is before beginning of window')
	   else
	      return_code = 0
	   end if
	end if

	if (loop_count .gt. 3) goto 46
	if (loop) goto 1000

	scxt(ch).mjr = mjr
	scxt(ch).mnr = mnr
	exi(ch).ur8_position = t
	if (scxt(ch).n_opens .ne. user(ch).n_file_opens) then
	   scxt(ch).n_opens = user(ch).n_file_opens
	end if

	w_channel_position = return_code
	return
  1	format(1x,'W_CHANNEL_POSITION: ', a, :, f16.10)
 10	write(6,1,iostat=ios) 'invalid channel number.'
	w_channel_position = w_bad_channel
	return
 20	w_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get record at beginning of file.'
	return
 30	w_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get record at UR8 time: ', t
	return
 40	w_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get new file at UR8 time: ', t
	return
 44	w_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot open file at UR8 time: ', t
	return
 46	w_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Loop count exceeded, UR8 time t: ', t
	return
 50	w_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get record at end of file.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_next_event
	1				(ch,major,minor,cevent_type)
! This is a convenience routine for application programs.  It is only
! a trivial front end for the wind_tm_get_event routine.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	character*(*)	cevent_type			! event type
	integer*4	ok
	integer*4	wind_tm_get_event
$IF ABSOFT_FORTRAN
!	integer*4	wind_tm_eof
$ELSE
	integer*4	wind_tm_eof
$ENDIF
	character*32	event_name

	event_name = cevent_type
	call to_upper(event_name,0,0)
	if (event_name(1:2) .eq. 'HK' .or. event_name(1:3) .eq. 'CDF') then
	   event_name = 'HK'
	   major = major + 1
	else if (event_name(1:11) .eq. 'MAJOR_FRAME') then
	   major = major + 1
	else if (event_name(1:11) .eq. 'MINOR_FRAME') then
	   call wind_tm_increment_mfmf(major,minor)
	else
	   call wind_tm_increment_packet(major,minor)
	end if

	ok = wind_tm_get_event(ch,major,minor,event_name)
	if (ok .ne. 1) goto 20

	wind_tm_get_next_event = ok

	return
  1	format(1x,'WIND_TM_GET_NEXT_EVENT: ', a, i4)
! 10	wind_tm_get_next_event = w_end_of_file
!	if (user(ch).suppress_messages) return
!	type 1, 'cannot get next event (EOF), status=', w_end_of_file
!	return
 20	wind_tm_get_next_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get next event, status=', ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_previous_event
	1				(ch,major,minor,event_type)
! This is a convenience routine for application programs that move backwards
! and forwards in time through the telemetry stream in "event" units.
! The search for the specified event begins at the stream position defined by
! the packet that include the current major and minor frame numbers
! arguments.  Eventually wind_tm_get_event is called to extract
! the event from the stream.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'

	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	character*(*)	event_type			! event type

$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='WIND_TM_GET_PREVIOUS_EVENT')	! name of this routine
$ELSE
	parameter	rn='WIND_TM_GET_PREVIOUS_EVENT'	! name of this routine
$ENDIF
	integer*4	ok
	integer*4	wind_check_parms		! a function
	integer*4	init_packet_dissect_info	! a function
	integer*4	wind_tm_get_event		! a function
	integer*4	wind_get_plain_packet		! a function
	character*32	ename
	integer*4	i

	logical*4	got_a_last_packet

	wind_tm_get_previous_event = 0

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	ename = event_type
	call to_upper(ename,0,0)

	if (ename .eq. 'HK' .or. ename .eq. 'CDF') then
	   ename = 'HK'
	   major = major - 1
	   goto 200
	else if (ename .eq. 'MAJOR_FRAME') then
	   major = major - 1
	   goto 200
	else if (ename .eq. 'MINOR_FRAME') then
	   call wind_tm_decrement_mfmf(major,minor)
	   goto 200
	else if (ename .eq. 'PACKET') then
	   call wind_tm_decrement_packet(major,minor)
	   goto 200
	else
	   call wind_tm_decrement_packet(major,minor)
	end if

	! query the database for packet/event extraction information
	ok = init_packet_dissect_info(ch,major,minor,ename)
	if (ok .ne. 1) goto 10

	! find the next "last" packet of the event going backwards
	got_a_last_packet = 0
	do while(.not.got_a_last_packet)
	   ok = wind_get_plain_packet(
	1	ch,major,minor,eei(ch).packet_id,reverse,0)
	   if (ok .ne. 1) goto 20
	   i = zext(user(ch).packet.w(0))
	   got_a_last_packet = (i .and. eei(ch).last_packet_mask) .ne. 0
	   if (.not.got_a_last_packet) then
	      call wind_tm_decrement_packet(major,minor)
	   end if
	end do

	! find the next "first" packet of the event going backwards
	ok = wind_get_plain_packet(ch,major,minor,eei(ch).packet_id,reverse,1)
	if (ok .ne. 1) goto 30

 200	continue
	ok = wind_tm_get_event(ch,major,minor,ename)
	if (ok .ne. 1) goto 40

	wind_tm_get_previous_event = 1

	return
  1	format(1x,'WIND_TM_GET_PREVIOUS_EVENT: ', a, a)

  8	wind_tm_get_previous_event = ok
	return

 10	wind_tm_get_previous_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get extract event info for event type: ', ename
	return

 20	wind_tm_get_previous_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get last packet for event: ', ename
	return

 30	wind_tm_get_previous_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get first packet for event: ', ename
	return

  40	wind_tm_get_previous_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get event: ', ename
	return

	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_event
	1				(ch,major,minor,u_event_type)
! This routine gathers a specified type of event from a telemetry stream.  The
! search for the specified event begins at the stream position defined by the
! major and minor frame number arguments.
! Note that packets are selected by packet type (i.e., rad1, rad2, tnr, etc.)
! and not by packet id (i.e., rad1 packet 1 of 4, rad2 packet 4 of 4, etc.),
! except for the case of the first packet.  The first packet is selected by
! packet id.
! Packet validation, with respect to event collection, is performed via
! a call to wind_validate_packet_in_event.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	character*(*)	u_event_type			! user's event type

$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='WIND_TM_GET_EVENT')	! name of this routine
$ELSE
	parameter	rn='WIND_TM_GET_EVENT'		! name of this routine
$ENDIF

	character*36	event_type			! event type

	integer*4	ok				! retrun status variable
	integer*4	i,j,k				! misc. indexes
	integer*4	wind_check_parms		! a function
	integer*4	wind_get_hk_event		! a function
	integer*4	w_f_iiiiiib			! a function
	integer*4	w_f_iii				! a function
	integer*4	w_f_iiiiii			! a function
	integer*4	wind_validate_packet_in_event	! a function
	integer*4	wind_get_event_data_from_packet	! a function
	integer*4	set_event_name_and_pkt_id	! a function
	integer*4	init_packet_dissect_info	! a function
	integer*4	update_packet_dissect_info	! a function
	integer*4	extract_primary_header		! a function
	integer*4	extract_other_headers		! a function
	integer*4	w_get_mjr_fr_event		! a function
	integer*4	w_get_mnr_fr_event		! a function
	integer*4	w_get_packet_event		! a function
	integer*4	dbms_to_ur8			! a function
	integer*4	w_set_event_name
	external	w_set_event_name !$pragma C (w_set_event_name)
	integer*4	validation_code
	integer*4	wind_get_plain_fill_packet
	integer*4	ios

	logical*4	unexpected_first_packet
	logical*4	not_by_fill
	logical*4	boolean_value
	logical*4	w_get_msg_state_updt_pos	! a function
	logical*4	first_time /.true./
	integer*4	dummy1, dummy2
	pointer		(p1,dummy1), (p2,dummy2)
	integer*4	sz_pkt_data_area

	record /low_byte/ lb

	wind_tm_get_event = 0

	event_type = u_event_type
	call to_upper(event_type,0,0)

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	if (first_time) then
	   first_time = .false.
	   do i=1,max_channels
	      ok = w_set_event_name(i, eei(i).event_type_b)
	   end do
$IF ABSOFT_FORTRAN
	   p1 = loc(eb(1).cpn)
	   p2 = loc(eb(1).last_data_byte)
$ELSE
	   p1 = %loc(eb(1).cpn)
	   p2 = %loc(eb(1).last_data_byte)
$ENDIF
	   sz_pkt_data_area = p2 - p1 + 4
	   call w_version_i4(exi(ch).wind_lib_nver)
	end if

	! initialize the event storage area for the current channel
!xxx move area 6 of eb somewhere else.
!!!!	call init_area(eb(ch).event_completion_status, (n_extra_info_lws+2)*4)
	do i=1,12
	   exi(ch).extra_info(i) = 0
	end do
	exi(ch).ur8_scet = 0.0
	exi(ch).ur8_ert  = 0.0
	exi(ch).ur8_eoe  = 0.0
	call init_area(eei(ch), sizeof_eei)

	if (event_type .eq. 'HK' .or. event_type .eq. 'CDF') then
	   ! house keeping event
	   event_type = 'HK'
	   call init_area(eb(ch).data, max_len_hk)
	   wind_tm_get_event = wind_get_hk_event(ch,major,minor)
	   return					! return 
	else if (event_type .eq. 'FILL') then
	   call init_area(eb(ch).cpn, sz_pkt_data_area)
	   not_by_fill = 0
	   ok = wind_get_plain_fill_packet(
	1          ch,major,minor,event_type,forward,.true.)
	   if (ok .ne. 1) goto 18
	else if (event_type .eq. 'MAJOR_FRAME') then
	   call init_area(eb(ch).major_frame, sizeof_major_frame)
	   wind_tm_get_event = w_get_mjr_fr_event(ch,major,minor)
	   return					! return 
	else if (event_type .eq. 'MINOR_FRAME') then
	   call init_area(eb(ch).minor_frame, sizeof_minor_frame)
	   wind_tm_get_event = w_get_mnr_fr_event(ch,major,minor)
	   return					! return 
	else if (event_type .eq. 'PACKET') then
	   call init_area(eb(ch).packet, size_of_packet)
	   wind_tm_get_event = w_get_packet_event(ch,major,minor)
	   return					! return 
	else
	   not_by_fill = 1
	   call init_area(eb(ch).cpn, sz_pkt_data_area)
	end if

	ok = set_event_name_and_pkt_id(ch,event_type)
	if (ok .ne. 1) goto 10

	eb(ch).cpn = 0					! packet # in event
	validation_code = 0
	eei(ch).got_a_first_packet = 0
	eei(ch).got_a_last_packet  = 0
	unexpected_first_packet    = 0

	! get the first packet in the event
	if (not_by_fill) then
	   ! call the packet retrieval routine
	   ok = w_f_iiiiiib( %val(user(ch).f_get_plain_packet),
	1	ch,major,minor,eei(ch).packet_id,forward,1,user(ch).packet.w)
	   if (ok .ne. 1) goto 20
	end if
	eei(ch).got_a_first_packet = 1

	! set current position in main user structure
	user(ch).wind_record.major_frame = major
	lb.i4val = minor
	user(ch).wind_record.minor_frame = lb.b

	ok = init_packet_dissect_info(ch,major,minor,event_type)
	if (ok .ne. 1) goto 12

	do while(eei(ch).got_a_last_packet .eq. 0)

	   eb(ch).cpn = eb(ch).cpn + 1

	   if (eb(ch).cpn .gt. 1) then
	      i = zext(user(ch).packet.w(0))
	      unexpected_first_packet = (i.and.eei(ch).first_packet_mask) .ne.0
	      if (unexpected_first_packet) goto 22
	   end if

	   ok = extract_primary_header(ch,user(ch).packet)
	   if (ok .ne. 1) goto 30

	   ok = update_packet_dissect_info(ch,user(ch).packet)
	   if (ok .ne. 1) goto 40

	   ok = extract_other_headers(ch,user(ch).packet)
	   if (ok .ne. 1) goto 50

	   if (eb(ch).cpn .eq. 1) then		! update extra info
	      ok = w_f_iii(%val(user(ch).f_get_xtra_event_info),ch,major,minor)
	      if (ok .ne. 1) then
	         validation_code = validation_code .or. ok
	      end if
	   end if 

	   ok = wind_validate_packet_in_event(ch,major,minor)
	   if (ok .eq. 1) then
	      validation_code = validation_code .or. ok
	      ok = wind_get_event_data_from_packet(ch,user(ch).packet)
	      if (ok .ne. 1) goto 60
	   else
	      goto 80
	   end if

	   ! check for last packet
	   i = zext(user(ch).packet.w(0))
	   boolean_value = (i .and. eei(ch).last_packet_mask) .ne. 0
	   if (boolean_value) eei(ch).got_a_last_packet = 1

	   if (eei(ch).got_a_last_packet .ne. 1) then
	      call wind_tm_increment_packet(major,minor)
	      if (not_by_fill) then
	         ok = w_f_iiiiiib( %val(user(ch).f_get_plain_packet),
	1	     ch,major,minor,eei(ch).packet_id,forward,0,user(ch).packet)
	         if (ok .ne. 1) goto 20
	      else
	         ok = wind_get_plain_fill_packet(ch,major,minor,
	1             event_type,forward,.false.)
	         if (ok .ne. 1) goto 18
	      end if
	   end if

	end do

	eb(ch).cpn = 1

	! set current position in main user structure
	user(ch).wind_record.major_frame = major
	lb.i4val = minor
	user(ch).wind_record.minor_frame = lb.b

	! now set the end-of-event ur8 time field
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms),
	1	ch,major,minor,
	1	i,j,k)
	ok = dbms_to_ur8(i,j,k, exi(ch).ur8_eoe)

!	call w_get_offline_rec_read_stats(ch,i,j)
!	type *, 'Offline reads:', j, '  calls:', j
!	call wid_get_n_dbms_reads(i)
!	type *, 'DBMS reads:', i
!	call wid_get_n_key_reads(i)
!	type *, 'Keyfile reads:', i

	! set return status
	wind_tm_get_event = validation_code
	eb(ch).event_completion_status = wind_tm_get_event

	! increment the channel's event counter
	user(ch).event_count = user(ch).event_count + 1

	return
  1	format(1x,'WIND_TM_GET_EVENT: ', a, a12)

  8	wind_tm_get_event = ok
	return

 10	wind_tm_get_event = ok
	if (user(ch).suppress_messages) return
$IF ABSOFT_FORTRAN
	write(6,1,iostat=ios) 'cannot get packet id for event type: ',
	1	event_type
$ELSE
	write(6,1,iostat=ios) 'cannot get packet id for event type: ', event_type
$ENDIF
	return

 12	wind_tm_get_event = ok
	if (user(ch).suppress_messages) return
$IF ABSOFT_FORTRAN
	write(6,1,iostat=ios) 'cannot get extract event info 
	1	for event type: ', event_type
$ELSE
	write(6,1,iostat=ios) 'cannot get extract event info for event type: ', event_type
$ENDIF
	return

 18	wind_tm_get_event = ok
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
$IF ABSOFT_FORTRAN
	write(6,1,iostat=ios) 'cannot get FILL packet for event: ',
	1	event_type
$ELSE
	write(6,1,iostat=ios) 'cannot get FILL packet for event: ', event_type
$ENDIF
	return

 20	wind_tm_get_event = ok
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 'cannot get packet for event: ', event_type
	return

 22	wind_tm_get_event = w_bad_multipacket_seq
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 
	1 'unexpected 1st packet in multipacket event: ', event_type
	return

 30	wind_tm_get_event = ok
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 
	1 'cannot extract primary header for event: ', event_type
	return

 40	wind_tm_get_event = ok
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 
	1 'cannot update packet dissection information: ', event_type
	return

 50	wind_tm_get_event = ok
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 
	1 'cannot extract "other" headers from packet, event: ',event_type
	return

 60	wind_tm_get_event = ok
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 
	1 'cannot get event info/data from packet, event: ', event_type
	return

 80	wind_tm_get_event = ok
	eb(ch).cpn = 1
	if (w_get_msg_state_updt_pos(ch,major,minor)) return
	write(6,1,iostat=ios) 'invalid packet for event: ', event_type
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	get_instrument_version(ch, inst, buf)
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'item_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	character*(*)	inst
	byte		buf
	integer*4	ok
	integer*4	w_get_assoc_hk_field		! a function
	integer*4	major
	integer*4	adjmjrok
	integer*4	startbit
	integer*4	bitlen
	record /c_string_16/ c16
	record /low_byte/ val

	get_instrument_version = 0
	major = exi(ch).major
	adjmjrok = 1
	bitlen = 4
	c16.c = inst//'_VERSION'//char(0)

	if (inst .eq. 'DPU') then
	   startbit = 32
	else if (inst .eq. 'FFT') then
	   startbit = 496
	else if (inst .eq. 'TDS') then
	   startbit = 344
	else
	   write(6,*) 
	1  'GET_INSTRUMENT_VERSION: ERROR! invalid instrument key word.'
	   return
	end if

	ok = w_get_assoc_hk_field(ch, major, c16.b, val, 
	1	startbit, bitlen, adjmjrok)

	if (ok .eq. 1) then
	   buf = val.b
	end if

	get_instrument_version = ok

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_eei()
! This routine gets the Event Extraction Information from the database.
! A number of "rules" and "givens" apply:
! 1.  The primary header is always the first thing in every packet.
! 2.  The packet id is always the first field of the primary header.
! 3.  The secondary and tertiary headers and IDP are guaranteed to be present
!     for one packet events and the first packet of multi-packet events
!     provided that event type uses secondary and/or tertiary headers
!     and/or IDP.
! 4.  For non-first packets of multi-packet events only the primary header
!     is guaranteed to exist for all packets, and the IDP is guaranteed
!     in each packet for all events requiring IDP's.
! 5.  The packet/event subtype is contained in the first field of the IDP.
! 6.  Both multi-packet and single-packet events use a first_packet_flag_bit
!     and a last_packet_flag_bit to define starting and stopping points for
!     event generation.
! 7.  A "house_keeping" event is a specially handled event.
! 8.  A "tds_fill" event is a specially handled event.
! 9.  Secondary and tertiary headers and the IDP are guaranteed to be of
!     identical size, respectively, in each packet of a multi-packet event.
!     Note that secondary and tertiary headers may or may not be present in
!     in non-first packets of multi-packet events.
!
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'item_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	character*(*)	event_type			! event type

	logical*4	got_2nd				! logical flag
	logical*4	got_3rd				! logical flag
	logical*4	got_4th				! logical flag
	logical*4	got_data			! logical flag
	integer*4	set_event_name_and_pkt_id	! an entry point
	integer*4	init_packet_dissect_info	! an entry point
	integer*4	update_packet_dissect_info	! an entry point
	integer*4	extract_primary_header		! an entry point
	integer*4	extract_other_headers		! an entry point
	integer*4	n				! # i*4's written
	integer*4	ok				! return status var
	integer*4	wind_tm_get_item		! a function
	integer*4	wind_tm_get_item_from_all	! a function
$IF ABSOFT_FORTRAN
$ELSE
	integer*4	k2len				! a function
$ENDIF
	integer*4	get_instrument_version		! a function
	integer*4	w_f_iiiiii			! a function
	integer*4	w_get_item_substruct
	external	w_get_item_substruct !$pragma C( w_get_item_substruct )
	integer*4	i,j,k				! misc loop/index vars
$IF ABSOFT_FORTRAN
	integer*4	all_subtypes
	parameter	(all_subtypes='0000ffff'x)
$ELSE
	parameter	all_subtypes='0000ffff'x
$ENDIF
	integer*4	ret_size
	byte		packet(size_of_packet)
	record /c_string_32/ item
	record /c_string_32/ all_event
	record /extract/ entry
	record /low_byte/ lb
	integer*4	max_buffered_vers
	parameter	(max_buffered_vers = 4)
	logical*4	get_vers
	structure /saved_instrument_version/
	   integer*4	mjr
	   byte         dpu
	   byte		fft
	   byte		tds
	   byte		dummy
	end structure
	structure /svd_ins_ver_array/
	   record /saved_instrument_version/ vers(max_buffered_vers)
	   integer*4	i
	end structure
	record /svd_ins_ver_array/ sv(max_channels)

	structure /dissect_info/
	   character*8	event
	   record /low_byte/	pri		! entire primary header
	   integer*4	pid			! packet id
	   integer*4	h2_pres_msk		! instrument header present
	   integer*4	h3_pres_msk		! measurement header present
	   integer*4	h4_pres_msk		! packet header mask
	   integer*4	fst_pkt_msk		! first packet mask
	   integer*4	lst_pkt_msk		! last packet mask
	   integer*4	h1_bit_size
	   integer*4	pkt_subtype_msk
	   integer*4	pkt_subtype_shift
	   logical*4	ok
	end structure
	record /dissect_info/ di(max_channels)

	wind_get_eei = 0
	return

	!----------------------------------------------------------------------
	entry				set_event_name_and_pkt_id(ch,event_type)
! This routine is called once with each request for an event.
! It establishes the event name (type) in the event extraction information 
! table and retrieves the associated packet_id value from the item database.

	set_event_name_and_pkt_id = 0

	all_event.c = 'GLOBAL'//char(0)
	eei(ch).event_type	= event_type
	k = 1
	j = len(eei(ch).event_type)
        i = 0
	do while(i .eq. 0)
	   if (eei(ch).event_type(k:k) .gt. ' ' .and.
	1      eei(ch).event_type(k:k) .lt. char(127)) then
	      k = k + 1
	   else
	      i = 1
	   end if
	   if (k .gt. j) then
	      k = k - 1
	      i = 1
	   end if
	end do
	eei(ch).event_type(k:k) = char(0)	! null terminate the string
	eei(ch).event_subtype	= all_subtypes

	! The packet_id is gathered here so the first "raw" packet may be
	! obtained from the stream before the packet dissect information is 
	! retrieved from the item database.  In this way bad areas in offline
	! files, such as beginning of files or times when the dpu is off,
	! can be avoided.  At this writing, the packet_id item is dependant
	! only on the event name (event_type).
	if (di(ch).event .eq. eei(ch).event_type) then
	   eei(ch).packet_id = di(ch).pid
	else
	   item.c = 'PACKET_ID'//char(0)
	   ok = wind_tm_get_item(ch,
	1	'PACKET_ID',
	1	eei(ch).packet_id,
	1	1,
	1	ret_size)
	   if (ok .ne. 1) goto 50
	   di(ch).pid = eei(ch).packet_id
	   di(ch).ok = .false.
	end if

	set_event_name_and_pkt_id = 1

	return
  2	format(1x,'SET_EVENT_NAME_AND_PKT_ID: ', a, a, a)
 50	set_event_name_and_pkt_id = ok
	if (user(ch).suppress_messages) return
	type 2, 'cannot get PACKET_ID item for event: ', event_type
$IF ABSOFT_FORTRAN
	type 2, '(a,a4,a)', ' Event type "',event_type,
	1    '" is unknown or invalid.'
	type 2, '(a,a4,a)', ' Make sure "',event_type,
	1	'" exists in item database.'
$ELSE
	type '(a,a4,a)', ' Event type "',event_type, '" is unknown or invalid.'
	type '(a,a4,a)', ' Make sure "',event_type, '" exists in item database.'
$ENDIF
	return
	
	!----------------------------------------------------------------------
	entry				init_packet_dissect_info
	1				(ch,major,minor,event_type)
! This routine is called once with each request for an event.
! It secures the primary arguments for querying TM item extraction database,
! such as date/time, instrument version numbers,... and the lowest level 
! of packet decomposition information including the size of the packet 
! primary header, the locations of the bits used to signal first and 
! last packets... 

	init_packet_dissect_info = 0

	exi(ch).major = major
        exi(ch).minor = minor

	! get dbms time of current record
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms),
	1	         ch,major,minor,
	1	exi(ch).ert(1),
	1	exi(ch).ert(2),
	1	exi(ch).ert1000)
	if (ok .ne. 1) goto 20
	eei(ch).dbms_date_time(1) = exi(ch).ert(1)
	eei(ch).dbms_date_time(2) = exi(ch).ert(2)
	! estimate the scet for the low level item getting
	exi(ch).scet(1)  = exi(ch).ert(1)
	exi(ch).scet(2)  = exi(ch).ert(2)
	exi(ch).scet1000 = exi(ch).ert1000

	! try to get current instrument version numbers from buffer
	get_vers = .true.
	if (sv(ch).i .ne. 0) then
	   i = 1
	   j = sv(ch).i
	   do while(i .le. max_buffered_vers .and. 
	1           sv(ch).vers(j).mjr .ne. major)
	      i = i + 1
	      j = j - 1
	      if (j .le. 0) j = max_buffered_vers
	   end do
	   if (sv(ch).vers(j).mjr .eq. major) then
	      get_vers = .false.
	      exi(ch).dpu_version = sv(ch).vers(j).dpu
	      exi(ch).fft_version = sv(ch).vers(j).fft
	      exi(ch).tds_version = sv(ch).vers(j).tds
	   end if
	end if

	! get instrument version numbers from associated HK
	if (get_vers) then
	   ok = get_instrument_version(ch, 'DPU', exi(ch).dpu_version)
	   ok = get_instrument_version(ch, 'FFT', exi(ch).fft_version)
	   ok = get_instrument_version(ch, 'TDS', exi(ch).tds_version)
	   sv(ch).i = sv(ch).i + 1
	   if (sv(ch).i .gt. max_buffered_vers) sv(ch).i = 1
	   sv(ch).vers(sv(ch).i).mjr = major
	   sv(ch).vers(sv(ch).i).dpu = exi(ch).dpu_version
	   sv(ch).vers(sv(ch).i).fft = exi(ch).fft_version
	   sv(ch).vers(sv(ch).i).tds = exi(ch).tds_version
	end if

	!
	! get static header extraction information through a series of
	! data base calls
	!

	if (.not. di(ch).ok) then
	   item.c = 'PRIMARY_HEADER'	! bit size of primary header
	   ok = wind_tm_get_item_from_all( ch,
	1	item.c,
	1	di(ch).h1_bit_size,
	1	1,
	1	ret_size)
	   if (ok .ne. 1) goto 40

	   item.c = 'FIRST_PACK_FLAG'//null
	   ok = w_get_item_substruct( %val(ch-1),
	1	all_event.b,
	1	item.b,
	1	entry,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 40
!	   eei(ch).first_packet_flag_bit	= entry.startbit
	   di(ch).fst_pkt_msk = 2**(entry.startbit)

	   item.c = 'LAST_PACK_FLAG'//null
	   ok = w_get_item_substruct( %val(ch-1),
	1	all_event.b,
	1	item.b,
	1	entry,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 40
!	   eei(ch).last_packet_flag_bit	= entry.startbit
	   di(ch).lst_pkt_msk = 2**(entry.startbit)

	   ! packet_subtype is in the primary header
	   item.c = 'PACKET_SUBTYPE'//null
	   ok = w_get_item_substruct( %val(ch-1),
	1	all_event.b,
	1	item.b,
	1	entry,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 40
	   di(ch).pkt_subtype_shift = 2**(entry.startbit)
	   j = 0
	   do i=entry.startbit,entry.startbit+entry.length-1
	      j = j .or. (2**i)
	   end do
	   di(ch).pkt_subtype_msk = j

	   ! Secondary header present field
	   item.c = 'INST_HDR_PRES'//null
	   ok = w_get_item_substruct( %val(ch-1),
	1	all_event.b,
	1	item.b,
	1	entry,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 40
	   di(ch).h2_pres_msk = 2**(entry.startbit)

	   ! Tertiary header present field
	   item.c = 'MEAS_HDR_PRES'//null
	   ok = w_get_item_substruct( %val(ch-1),
	1	all_event.b,
	1	item.b,
	1	entry,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 40
	   di(ch).h3_pres_msk = 2**(entry.startbit)

	   ! Quaterinary header present flag
	   item.c = 'PACK_HDR_PRES'//null
	   ok = w_get_item_substruct( %val(ch-1),
	1	all_event.b,
	1	item.b,
	1	entry,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 40
	   di(ch).h4_pres_msk = 2**(entry.startbit)
	end if

	! Calculate the number of bytes in primary header.
	! Note that the headers are byte aligned.
	! Note that bits are numbered from 0 while bytes are numbered from 1.
	eei(ch).h1_start_bit  = 0
	eei(ch).h1_bit_size   = di(ch).h1_bit_size
	eei(ch).h1_byte_size  = di(ch).h1_bit_size / 8
	eei(ch).h1_start_byte = 1
	eei(ch).h1_end_byte   = eei(ch).h1_byte_size
	!
	eei(ch).first_packet_mask = di(ch).fst_pkt_msk
	eei(ch).last_packet_mask = di(ch).lst_pkt_msk
	eei(ch).h1_start_bit = 0

	init_packet_dissect_info = 1

	return
  1	format(1x,'INIT_PACKET_DISSECT_INFO: ', a, a, a)
! 10	init_packet_dissect_info = ok
!	if(user(ch).suppress_messages) return
!	type 1, 'cannot get ERT of current record.'
!	return
 20	init_packet_dissect_info = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get ERT for event.'
	return
 40	init_packet_dissect_info = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get event extract info for: ', item.c
	return

	!----------------------------------------------------------------------
	entry				extract_primary_header
	1				(ch,packet)
! This routine copies the primary header from the packet to the event
! information storage area for the channel.  The primary header, like all
! headers and the interactive data preamble, is guaranteed to
! be byte aligned by the author of the WIND/WAVES DPU.
!
	extract_primary_header = 0

	do i=eei(ch).h1_start_byte,eei(ch).h1_end_byte
	   eb(ch).ph(eb(ch).cpn).h1(i) = packet(i)
	end do

	extract_primary_header = 1
	return

	!----------------------------------------------------------------------
	entry				extract_other_headers
	1				(ch,packet)
! This routine copies the second, third, and fourth headers
! from the current packet to the event information storage area
! for the channel.

	extract_other_headers = 0

	!
	! extract the non-primary headers
	!

	if (got_2nd) then
	   j = 1
	   do i=eei(ch).h2_start_byte,eei(ch).h2_end_byte
	      eb(ch).ph(eb(ch).cpn).h2(j) = packet(i)
	      j = j + 1
	   end do
	end if

	if (got_3rd) then
	   j = 1
	   do i=eei(ch).h3_start_byte,eei(ch).h3_end_byte
	      eb(ch).ph(eb(ch).cpn).h3(j) = packet(i)
	      j = j + 1
	   end do
	end if

	if (got_4th) then
	   j = 1
	   do i=eei(ch).h4_start_byte,eei(ch).h4_end_byte
	      eb(ch).ph(eb(ch).cpn).h4(j) = packet(i)
	      j = j + 1
	   end do
	end if

	extract_other_headers = 1
	
	return

	!----------------------------------------------------------------------
	entry				update_packet_dissect_info
	1				(ch,packet)
! This routine is called with the reception of each packet in order to
! get the proper extraction information for the current packet.
! Wind_tm_get_item is called to get the packet subtype, 2nd header,
! 3rd header, and 4th header.
! All headers are guaranteed to be byte aligned.

	update_packet_dissect_info = 0

	eb(ch).ph(eb(ch).cpn).got_1st = 1

	di(ch).pri.b  = packet(1)
	di(ch).pri.b1 = packet(2)
	di(ch).pri.b2 = packet(3)
	di(ch).pri.b3 = 0

	! packet_subtype is in the primary header
	i = di(ch).pri.i4val .and. di(ch).pkt_subtype_msk
	eei(ch).event_subtype = i / di(ch).pkt_subtype_shift

	! Secondary header present field
	got_2nd = (di(ch).pri.i4val .and. di(ch).h2_pres_msk) .ne. 0

	! Tertiary header present field
	got_3rd = (di(ch).pri.i4val .and. di(ch).h3_pres_msk) .ne. 0

	! Quaterinary header present flag
	got_4th = (di(ch).pri.i4val .and. di(ch).h4_pres_msk) .ne. 0

	!
	! determine the start and end bytes of each header
	!

	! get the bit size of the second header
	if (got_2nd) then
	   item.c = 'INSTR_HEADER'//null
	   ok = wind_tm_get_item(ch,item.c, i, 1, n)
	   if (ok .ne. 1) goto 400
	   n = i / 8
	   eei(ch).h2_bit_size   = i
	   eei(ch).h2_start_byte = eei(ch).h1_end_byte + 1
	   eei(ch).h2_end_byte   = n + eei(ch).h2_start_byte - 1
	   eei(ch).h2_byte_size	 = eei(ch).h2_bit_size / 8
	end if

	! get the bit size of the third header
	if (got_3rd) then
	   item.c = 'MEASURE_HEADER'//null
	   ok = wind_tm_get_item(ch,item.c, i, 1, n)
	   if (ok .ne. 1) goto 400
	   n = i / 8
	   eei(ch).h3_bit_size = i
	   if (got_2nd) then
	      eei(ch).h3_start_byte = eei(ch).h2_end_byte + 1
	      eei(ch).h3_end_byte   = n + eei(ch).h3_start_byte - 1
	   else
	      eei(ch).h3_start_byte = eei(ch).h1_end_byte + 1
	      eei(ch).h3_end_byte   = n + eei(ch).h3_start_byte - 1
	   end if
	   eei(ch).h3_byte_size	 = eei(ch).h3_bit_size / 8
	end if

	! get the bit size of the fourth header
	if (got_4th) then
	   item.c = 'PACKET_HEADER'//null
	   ok = wind_tm_get_item(ch,item.c, i, 1, n)
	   if (ok .ne. 1) goto 400
	   n = i / 8
	   eei(ch).h4_bit_size = i
	   if (got_3rd) then
	      eei(ch).h4_start_byte = eei(ch).h3_end_byte + 1
	      eei(ch).h4_end_byte   = n + eei(ch).h4_start_byte - 1
	   else if (got_2nd) then
	      eei(ch).h4_start_byte = eei(ch).h2_end_byte + 1
	      eei(ch).h4_end_byte   = n + eei(ch).h4_start_byte - 1
	   else
	      eei(ch).h4_start_byte = eei(ch).h1_end_byte + 1
	      eei(ch).h4_end_byte   = n + eei(ch).h4_start_byte - 1
	   end if
	   eei(ch).h4_byte_size	 = eei(ch).h4_bit_size / 8
	end if

	! calculate where the data must end
	! the tds fill start byte is the start byte of the tds fill
	! as measured from the beginning of the packet using zero as the
	! index to the first packet element--note that wind_get_event stuff
	! uses one (1) as the index to the first packet element while the
	! flight software measures from byte 0 at the beginning of the packet.
	lb.i4val = 0
	lb.b = packet(3)
	lb.b1 = packet(2)
	! Use the octal form of '000001ff'x because the Sun SC0.0 f77 chokes
	! on hex constants in unconceivable ways.
	lb.i4val = lb.i4val .and. '0777'o	! octal constant
	eei(ch).tds_start_byte = lb.i4val
	eei(ch).data_end_byte = eei(ch).tds_start_byte

! commented out 11-aug-1995, jk
!	! now calculate where the data must end
!	item.c = 'TDS_FILL_POINTER'//null
!	ok = wind_tm_get_item_from_all(ch,item.c, eei(ch).tds_start_byte, 1, n)
!	if (ok .ne. 1) goto 400
!	! now the tds fill start byte is the start byte of the tds fill
!	! as measured from the beginning of the packet using zero as the
!	! index to the first packet element--note that wind_get_event stuff
!	! uses one (1) as the index to the first packet element while the
!	! flight software measures from byte 0 at the beginning of the packet.
!	eei(ch).data_end_byte = eei(ch).tds_start_byte

	! now calculate where the data must start
	if (got_4th) then
	   eei(ch).data_start_byte = eei(ch).h4_end_byte + 1
	else if (got_3rd) then
	   eei(ch).data_start_byte = eei(ch).h3_end_byte + 1
	else if (got_2nd) then
	   eei(ch).data_start_byte = eei(ch).h2_end_byte + 1
	else
	   eei(ch).data_start_byte = eei(ch).h1_end_byte + 1
	end if
	eei(ch).data_byte_size=eei(ch).data_end_byte-eei(ch).data_start_byte+1

	got_data = (eei(ch).data_end_byte - eei(ch).data_start_byte) .ge. 0

	eb(ch).ph(eb(ch).cpn).got_2nd  = got_2nd
	eb(ch).ph(eb(ch).cpn).got_3rd  = got_3rd
	eb(ch).ph(eb(ch).cpn).got_4th  = got_4th
	eb(ch).ph(eb(ch).cpn).got_data = got_data

	di(ch).ok = .true.

	update_packet_dissect_info = 1

	return
  4	format(1x,'UPDATE_PACKET_DISSECT_INFO: ', a, a, a)
 400	update_packet_dissect_info = ok
	if (user(ch).suppress_messages) return
	type 4, 'cannot get information on item: ', item.c
	return

	end
!------------------------------------------------------------------------------
	integer*4	function	wind_validate_packet_in_event
	1				(ch,major,minor)
! This routine checks:
!	1.)  that the packet id is the appropriate one, taking into
!	     account the first, middle(s), and last packets in a sequence
!	     of packets or more simply the packet id for a single packet event
!	2.)  that the packet's internal numbering number is sequential
!	     with respect to previously gotten packets in a multi-packet event
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major
	integer*4	minor
	integer*4	ok
	integer*4	ios
	integer*4	current_packet_number
	integer*4	ret_size
	integer*4	wind_tm_get_item
	character*16	item
	logical*4	suppressing_messages

	wind_validate_packet_in_event = 0
	ok = 1

	suppressing_messages = user(ch).suppress_messages
	call wind_tm_set_messages_off(ch)

	! check packet sequence number within event
	item = 'PACKET_COUNT'
	ok = wind_tm_get_item(ch,item,current_packet_number,1,ret_size)

	if (.not. suppressing_messages) call wind_tm_set_messages_on(ch)

	if (ok .eq. 1) then
	   ! item is valid, compare packet sequence numbers
	   if (current_packet_number .ne. eb(ch).cpn) goto 10
	else
	   ! item is not valid, we are probably looking at an old file,
	   ! so, just do nothing
	end if

	wind_validate_packet_in_event = 1

	return
  1	format(1x,'WIND_VALIDATE_PACKET_IN_EVENT: ', a, a)
  2	format(1x,t2,a,i,'.',i3.3,a,i)
  3	format(1x,t2,a,i3,a,i)
 10     wind_validate_packet_in_event = w_bad_multipacket_seq
	if (user(ch).suppress_messages) return
	type 1, 'Bad multipacket sequence number, event: ',eei(ch).event_type
	write(6,2,iostat=ios) 'stream position',
	1	major,
	1	minor,
	1	', DPU ertMF:',exi(ch).dpu_major_ert
	type 3, 'expected packet sequence #',eb(ch).cpn, ' but got',
	1	zext(eb(ch).ph(eb(ch).cpn).h4(1))
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_event_data_from_packet
	1				(ch,packet)
! This routine copies the packet data to the event data buffer.
! The event data buffer holds the data in bytes because some telemetry
! data items must be extracted at the bit level and across byte or longword
! boundaries.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	i,j				! array/loop indexes
	byte		packet(size_of_packet)

	wind_get_event_data_from_packet = 0

	j = eb(ch).last_data_byte + 1
	do i = eei(ch).data_start_byte,eei(ch).data_end_byte
	   eb(ch).data(j) = packet(i)
	   j = j + 1
	end do
	eb(ch).last_data_byte = j - 1

	wind_get_event_data_from_packet = 1

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_hk_event(ch,major,minor)
! This routine builds up an HK event by calling wind_tm_get_hk to get the
! "data" portion of the event.  The various internal wind_lib flags and
! item database extraction arguments are also determined.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_hk_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	integer*4	i,j,k				! loop index var
	integer*4	aok				! status var
	integer*4	ok				! return status var
!	integer*4	wind_get_xtra_event_info	! a function
	integer*4	get_instrument_version
$IF ABSOFT_FORTRAN
	integer*4	all_subtypes
	parameter	(all_subtypes='0000ffff'x)
$ELSE
	parameter	all_subtypes='0000ffff'x
$ENDIF
	integer*4	longword
	byte		byte
	equivalence	(longword,byte)
	integer*4	wind_suppress_messages
	integer*4	w_f_iiii
	integer*4	w_f_iiiiii
	integer*4	w_f_iiiib
	logical*4	message_state
	integer*4	w_item_i4	!-xragma C (w_item_i4)
	record /low_byte/ v
	logical*4	is_fast_tm
	logical*4	is_slow_tm
	integer*4	a
	integer*4	dbms_to_ur8			! a function

	is_fast_tm(a) =a .eq. science_2x .or.
	1              a .eq. maneuver_2x .or.
	1	       a .eq. contingency_2x
	is_slow_tm(a) =a .eq. maneuver_1x .or.
	1	       a .eq. science_1x .or.
	1	       a .eq. contingency_1x 

	wind_get_hk_event = 0

	eei(ch).event_type	= 'HK'
	eei(ch).event_type(3:3) = char(0)	! null terminate the string
	eei(ch).event_subtype	= all_subtypes

	exi(ch).major = major
	exi(ch).minor = 0

	minor = max_minor_frame_num

	aok = 1
	j = 0
	k = 0
	exi(ch).dpu_major_ert = 0

	i = 0
	j = w_last_hk_idx
	message_state = wind_suppress_messages(ch)
	call wind_set_message_state(ch, .true.)
	ok = w_f_iiiib(%val(user(ch).f_get_hk),
	1	ch,major,i,j,eb(ch).data)
	call wind_set_message_state(ch, message_state)
	! set the return code based on the availablity of the HK data at
	! this point to flag partial successes
	eb(ch).event_completion_status = ok
	if (ok .ne. 1 .and. ok .ne. w_limited_success) goto 30

	eb(ch).cpn = 1
	eb(ch).ph(1).got_data  = .true.
	eb(ch).last_data_byte  = max_len_hk

	! get the gse info
	exi(ch).sp_test_number= user(ch).wind_record.sp_test_number
	exi(ch).sp_step_number= user(ch).wind_record.sp_step_number

	! get dbms time of last record in MF
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms),
	1	ch,major,max_minor_frame_num,
	1	exi(ch).ert(1),
	1	exi(ch).ert(2),
	1	exi(ch).ert1000)
	ok = dbms_to_ur8(
	1       exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000,
	1	exi(ch).ur8_eoe)

	! get dbms time of current record
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms),
	1	ch,major,min_minor_frame_num,
	1	exi(ch).ert(1),
	1	exi(ch).ert(2),
	1	exi(ch).ert1000)
	if (ok .ne. 1) goto 20
	eei(ch).dbms_date_time(1) = exi(ch).ert(1)
	eei(ch).dbms_date_time(2) = exi(ch).ert(2)
	exi(ch).scet(1)  = exi(ch).ert(1)
	exi(ch).scet(2)  = exi(ch).ert(2)
	exi(ch).scet1000 = exi(ch).ert1000

	! get the ur8 time formats
	ok = dbms_to_ur8(
	1       exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000,
	1	exi(ch).ur8_scet)
	ok = dbms_to_ur8(
	1       exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000,
	1	exi(ch).ur8_ert)
	exi(ch).ur8_context = exi(ch).ur8_scet

	! get instrument version numbers from associated HK
	message_state = wind_suppress_messages(ch)
	call wind_set_message_state(ch, .true.)
	ok = get_instrument_version(ch, 'DPU', exi(ch).dpu_version)
	ok = get_instrument_version(ch, 'FFT', exi(ch).fft_version)
	ok = get_instrument_version(ch, 'TDS', exi(ch).tds_version)
	call wind_set_message_state(ch, message_state)

	! get the dpu major frame number
	ok = w_item_i4(ch,'DPU_MAJOR_FRAME',exi(ch).dpu_major_ert,1,j)

	! get the sc mode
	ok = w_f_iiii( %val(user(ch).f_get_tm_mode),
	1	         ch,major,min_minor_frame_num,v)
	if (ok .ne. 1) goto 16
	exi(ch).sc_mode = v.b

	! get the bit rate
	if (is_fast_tm(v.i4val)) then
	   exi(ch).bit_rate = 1
	else if (is_slow_tm(v.i4val)) then
	   exi(ch).bit_rate = 0
	else
	   goto 18
	end if

	! indicate partial success if necessary
	if (eb(ch).event_completion_status .ne. 1) goto 31

	wind_get_hk_event = 1
	return
  1	format(1x,'WIND_GET_HK_EVENT: ', a, :, a, a)
  2	format(1x,'WIND_GET_HK_EVENT: ', a, i)
  3	format(1x,'WIND_GET_HK_EVENT: ', a, i5)

! 10	wind_get_hk_event = ok
!	if (user(ch).suppress_messages) return
!	type 1, 'cannot get ERT of current record.'
!	return

 16	wind_get_hk_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get SC mode for current record.'
	return

 18	wind_get_hk_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get determine bit rate for current record.'
	return

 20	wind_get_hk_event = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot convert ERT to DBMS time: '
	return

! 25	wind_get_hk_event = ok
!	if (user(ch).suppress_messages) return
!	type 1, 'cannot convert ERT to UR8 time: '     
!	return

 30	wind_get_hk_event = ok
	if (user(ch).suppress_messages) return
	type 2, 'cannot get HouseKeeping data:', ok
	return

 31	wind_get_hk_event = w_partial_hk_event
	if (user(ch).suppress_messages) return
	type 2, 'partial or bad HouseKeeping event (b):', ok
	return

! 40	wind_get_hk_event = ok
!	if (user(ch).suppress_messages) return
!	type 1, 'cannot get extra event information.'
!	return

	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_assoc_hk_field
	1		(ch,amajor,c_item_name,val,stbit,bitlen,adjmjrok)
! This routine attempts to extract a bit field from the HK associated with
! an event.  Originally designed for version numbers, this routine should work
! for any non-composite HK bit field of 8 bits or less contained entirely 
! within a single HK word.  
! 
! If the desired HK field is after EOF the value is taken from the previous
! major frame.  Similarly, when the desired HK field is prior to BOF the
! value is taken from the following major frame.  The analagous BOF/EOF 
! situations (recording/not_recording) with the realtime stream are handled 
! in identical fashion.
!
! For dpu and/or instrument version numbers this strategy is probably
! ok--the caller must realize that this strategy may not be appropriate for
! all HK fields.
!
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	include		'item_def.for'
	integer*4	ch				! TM channel number
	integer*4	amajor				! major frame number
	byte		c_item_name(*)
	integer*4	val
	integer*4	stbit
	integer*4	bitlen
	integer*4	adjmjrok			! adjacent MF is ok
	record /c_string_32/ event_name
	record /c_string_64/ item_name
	record /low_byte/ value
	integer*4	mf_addr
	integer*4	word_addr
	integer*4	major
	integer*4	ok
	integer*4	wind_get_hk_addr_from_bitoffset	! a function
	integer*4	w_get_item_substruct
	external	w_get_item_substruct !$pragma C( w_get_item_substruct )
	record /extract/ it
	integer*4	startbit
!	integer*4	zext_bits_to_longword		! a function
	integer*4	w_get_assoc_hk_field_		! an entry point
	logical*4	message_state
	logical*4	adjacent_format_ok
	parameter	(adjacent_format_ok=.true.)
	logical*4	first_time /.true./
	integer*4	w_f_iiiib
	integer*4	i, j, o, z
	integer*4	add
$IF ABSOFT_FORTRAN
!	integer*4	n_file_opens(max_channels)
	integer*4	w_first_hk_word,w_last_hk_word
	parameter	(w_first_hk_word=0)
	parameter	(w_last_hk_word=80)
$ELSE
	integer*4	n_file_opens(max_channels)
	parameter	w_first_hk_word=0
	parameter	w_last_hk_word=80
$ENDIF
	structure /saved_hk_words/
	   integer*4	major
	   integer*4	hkword
	   integer*4	file_number
	end structure
	record /saved_hk_words/ 
	1	svhkw(w_first_hk_word:w_last_hk_word,max_channels)
	structure /hk_indexes/
	   integer*4	mf
	   integer*4	wd
	end structure
	record /hk_indexes/ hki(w_first_hk_word:w_last_hk_word)
	logical*4	bv1, bv2, bv3
	logical*4	save_it
	integer*4	loop_count

! ...when n_opens != user(ch).n_opens do initialize shkv to all zeros
! ...return another argument=={-1,0,1} for "hk from prev, current, next"
!    so caller knows ==> handy for incrementing dpu_major_frame_number
	!----------------------------------------------------------------------
	entry	w_get_assoc_hk_field_
	1		(ch,amajor,c_item_name,val,stbit,bitlen,adjmjrok)

	w_get_assoc_hk_field = 0
	val = 0
	major = amajor

	! c_item_name is a null terminated string from C
	item_name.c = ' '
        i = 1
	do while (c_item_name(i) .ne. 0 .and. i .lt. 64)
	   item_name.b(i) = c_item_name(i)
	   i = i + 1
	end do

	if (first_time) then
	   first_time = .false.
	   event_name.c = 'HK'//null
	   ! initialize the hk word buffer, which saves words from the previous
	   ! or current major frame (even across file boundries)
	   do j=1,max_channels
	      do i=w_first_hk_word,w_last_hk_word
	         svhkw(i,j).major  = 0
	         svhkw(i,j).hkword = 0
	         svhkw(i,j).file_number = 0
	      end do
	   end do
	   ! pre-fetch all the hk index information
	   j = 0
	   do i=w_first_hk_word,w_last_hk_word
	      ok = wind_get_hk_addr_from_bitoffset(j, hki(i).mf, hki(i).wd)
	      if (ok .ne. 1) goto 24
	      j = j + 8
	   end do
	end if

	! reset the saved hk values when the stream changes
!	if (n_file_opens(ch) .ne. user(ch).n_file_opens) then
!	   do i=w_first_hk_word,w_last_hk_word
!	      svhkw(i,ch).major = 0
!	      svhkw(i,ch).hkword = 0
!	   end do
!	   n_file_opens(ch) = user(ch).n_file_opens
!	end if

	if (stbit .eq. 0 .and. bitlen .eq. 0) then
	   ! get the item structure for the named item from the item database
	   ok = w_get_item_substruct(%val(ch-1),
	1	event_name.b,
	1	c_item_name,
	1	it,
	1	%val(p_extract))
	   if (ok .ne. 1) goto 20
	else
	   it.startbit = stbit
	   it.length   = bitlen
	end if

	! determine the byte position of desired field in the hk array
	save_it = .true.
	value.i4val = 0
	z = it.startbit / 8
	if (.not. (z .ge. w_first_hk_word .and. z .le. w_last_hk_word)) then
	   goto 22
	end if
	if (svhkw(z,ch).major .eq. amajor .and.
	1   svhkw(z,ch).file_number .eq. user(ch).n_file_opens) then
	    ! the hk word was previously collected and saved
	   value.i4val = svhkw(z,ch).hkword
	else
	   ! determine the HK word and minor frame addresses of the field
	   loop_count = 1
	   mf_addr   = hki(z).mf
	   word_addr = hki(z).wd
	   message_state = user(ch).suppress_messages
	   user(ch).suppress_messages = .true.
	   ok = w_f_iiiib( %val(user(ch).f_get_word),
	1	ch,major,mf_addr, word_addr, value.b)
	   if ((ok .eq. w_bad_word_quality_flag) .and. (adjmjrok .eq. 1)) then
	      ! have detected a fluff value
	      bv1 = abs(major - svhkw(z,ch).major) .lt. 7
	      bv2 = svhkw(z,ch).file_number .eq. user(ch).n_file_opens
	      bv3 = svhkw(z,ch).major .gt. 0
	      if (bv1 .and. bv2 .and. bv3) then
	         value.i4val = svhkw(z,ch).hkword
	         save_it = .false.
	         ok = 1
	      else if (bv2 .and. (svhkw(z,ch).major .eq. 0)) then
	         ! either
	         if (major .gt. user(ch).first_major) then
	            ok = w_end_of_file
	         else
	            ok = w_beginning_of_file
	         end if
	      else if (major .eq. user(ch).first_major) then
	         ! bad data at beginning of file
	         if (adjmjrok .ne. 1) then
	            save_it = .false.
	         else
!	type *, '...setting loop_count...'
	            ok = w_beginning_of_file
	            loop_count = 7
	         end if
	      else
	         save_it = .false.  ! to fall thru to error message report
	      end if
	   end if
	   if (save_it .and. (adjmjrok .eq. 1)) then
	      if (ok .ne. 1) then
	         add = 0
	         ! desired hk is after EOF, get the word from the prev MF
	         if (ok .eq. w_end_of_file) add =  -1
	         ! desired hk is prior to BOF, get the word from the next MF
	         if (ok .eq. w_beginning_of_file) add = 1
	         major = major + add
	         if (add .eq. 0) save_it = .false.
	      end if
	      if (save_it) then
	         if (loop_count .eq. 1) then
	            ok = w_f_iiiib( %val(user(ch).f_get_word),
	1	         ch,major,mf_addr, word_addr, value.b)
	         else
	            ! try to advance over a bad spot at BOF
	            j = 0
	            do while (j .le. loop_count .and. ok .ne. 1)
!	type *, '...looping j,ok,major=',j,ok,major,'...'
	               ok = w_f_iiiib( %val(user(ch).f_get_word),
	1	            ch,major,mf_addr, word_addr, value.b)
	               if (ok .ne. 1) then
	                  j = j + 1
	                  major = major + 1
	               end if
	            end do
	         end if
	      end if
	   end if
	   user(ch).suppress_messages = message_state
	   if (ok .ne. 1) goto 26
	end if

	startbit = mod(it.startbit,8)
	if (startbit .eq. 0 .and. it.length .eq. 8) then
	   val = value.i4val
	else
	   if (startbit .ne. 0) then
	      value.i4val = value.i4val / (2**startbit)
	   end if
	   ! we work only with 8-bit entities so the mask is ff
	   i = '00ff'x / 2**(it.length)
	   val = i .and. value.i4val
	end if
!	val = zext_bits_to_longword(value.i4val,startbit,it.length)

	! save the word to the buffer
	if (save_it) then
	   i = it.startbit / 8
	   svhkw(i,ch).major = major
	   svhkw(i,ch).hkword = value.i4val
	   svhkw(i,ch).file_number = user(ch).n_file_opens
	end if

	w_get_assoc_hk_field = 1
	return
  1	format(1x,'W_GET_ASSOC_HK_FIELD: ', a, :, i9,'.',i3.3)
  2	format(1x,'W_GET_ASSOC_HK_FIELD: ', a, 1x, a, 1x, :, a)
  3	format(1x,'W_GET_ASSOC_HK_FIELD: ', a, i6)
  4	format(1x,'W_GET_ASSOC_HK_FIELD: ', a, a16, ', ok=',i5)
 20	w_get_assoc_hk_field = ok
	if (user(ch).suppress_messages) return
	type 2, 'cannot get item structure for: ', event_name.c, item_name.c
	return
 22	w_get_assoc_hk_field = 0
	if (user(ch).suppress_messages) return
	type 3, 'illegal HK word value:', i
	return
 24	w_get_assoc_hk_field = 0
	if (user(ch).suppress_messages) return
	type 3, 'cannot get HK addr at bit offset: ', j
	return
 26	w_get_assoc_hk_field = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'cannot get HK word at or near MF.mf',
	1	 major, mf_addr
	write(6,4,iostat=o) 'HK sought is ', item_name.c, ok
!	type *, '...bv1,bv2,bv3: ', bv1, bv2, bv3
!	type *, '....major,major,.file#,file#=', 
!	1   major, svhkw(i,ch).major, 
!	1   svhkw(i,ch).file_number, user(ch).n_file_opens
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_dpu_mjr_fr_num(ch,major,minor,dpu)
! Extracts the dpu major frame number from the stream at a given position.
! Attempts to estimate the value at EOF and BOF.  Restores the caller
! to the stream position at entry.
! From minor frame #44:
!	lw.b =  my_record.data(18)
!	lw.b1 = my_record.data(17)
! From minor frame #114:
!	lw.b2 = my_record.data(17)

	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor
	integer*4	dpu				! dpu major frame #
	integer*4	ok				! return status var
	integer*4	i,j
	integer*4	w_f_iiii			! a function
	integer*4	n_to_save
	parameter	(n_to_save=4)
	structure /mjr_dpu_buf/
	   integer*4	mjr	! file/stream MF
	   integer*4	dpu	! dpu MF at mjr
	   integer*4	fnu	! file number
	end structure
	record /mjr_dpu_buf/ bf(max_channels)
	logical*4	first_time /.true./
	logical*4	save_it
	logical*4	this_file
	logical*4	this_mjr
	logical*4	valid_dpu

	w_get_dpu_mjr_fr_num = 1

	if (first_time) then
	   first_time = .false.
	   do i=1,max_channels
	      bf(i).mjr = 0
	      bf(i).dpu = 0
	      bf(i).fnu = 0
	   end do
	end if

	! do we already have it in the buffer?
	this_file = bf(ch).fnu .eq. user(ch).n_file_opens
	this_mjr  = bf(ch).mjr .eq. major
	valid_dpu = bf(ch).dpu .gt. 0
	if (this_file .and. this_mjr .and. valid_dpu) then
	   dpu = bf(ch).dpu
	   return
	end if

	! not in the buffer, so fetch it
	ok = w_f_iiii( %val(user(ch).f_get_dpu_mf),
	1	         ch,major,minor,dpu)

!	type *, '...this_(file,mjr),valid_dpu,ok=',
!	1 this_file, this_mjr, valid_dpu, ok

	! was there a data dropout we can recover from?
	! Note that a dpu reset in conjunction with a dropout is
	! not caught with the current strategy...
	if (ok .eq. 1) then
	   save_it = .true.
	else if ((ok .eq. w_bad_word_quality_flag) .and.
	1   this_file .and. valid_dpu) then
	   save_it = .false.
	   j = major - bf(ch).mjr
!	type *, '...got bad_quality_flag, j=', j
	   if (abs(j) .lt. 4) then
	      dpu = dpu + j
	      return
	   end if
	end if
	if (ok .ne. 1) goto 10

	if (save_it) then
	   bf(ch).dpu = dpu
	   bf(ch).mjr = major
	   bf(ch).fnu = user(ch).n_file_opens
	end if

	w_get_dpu_mjr_fr_num = 1

	return
  1	format(1x,'W_GET_DPU_MJR_FR_NUM: ',a,:,i,'.',i3.3)
 10	w_get_dpu_mjr_fr_num = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get DPU Major Frame (HK) at MF#', major
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_ev_dpu_mfmf_and_hk_diff
	1	(ch,event_dpu_major,event_dpu_minor,
	1	assoc_hk_dpu_major,diff)
! This routine gets the event dpu MF.mf via item calls.  It also determines
! the difference in number of major frames between the event's
! dpu MF.mf (as recorded in a packet header) and the dpu MF.mf extracted
! from the HK of the major frame encompasing the first packet of said event.
! Some effort is given to reconcile an event's dpu MF that has been recorded
! with fewer than the available number of significant bits.
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'item_def.for'
	integer*4	ch				! TM channel number
	integer*4	ok				! return status var
	integer*4	event_dpu_major
	integer*4	event_dpu_minor
	integer*4	assoc_hk_dpu_major
	integer*4	diff
	integer*4	nbits_event_dpu_major
	record	/extract/ it
$IF ABSOFT_FORTRAN
	character*(*)	major_name, minor_name
	parameter	(major_name='DPU_MAJOR_FRAME')
	parameter	(minor_name='DPU_MINOR_FRAME')
$ELSE
	parameter	major_name='DPU_MAJOR_FRAME'
	parameter	minor_name='DPU_MINOR_FRAME'
$ENDIF
	integer*4	i,j,k
	integer*4	old
	record /c_string_36/	name
	integer*4	w_get_item_substruct
	external	w_get_item_substruct !$pragma C( w_get_item_substruct )
	logical*4	wind_suppress_messages		! a function
	integer*4	wind_tm_get_item		! a function

	w_get_ev_dpu_mfmf_and_hk_diff = 0

	! get the DPU minor frame number of the event
!	ok = w_set_val_fail_msg(1)	! show individual validation messages
	ok = wind_tm_get_item(ch,minor_name,event_dpu_minor,1,i)
!	ok = w_set_val_fail_msg(0)
	if (ok .ne. 1) goto 130

	! get the DPU major frame number of the event
	ok = wind_tm_get_item(ch,major_name,event_dpu_major,1,i)
	if (ok .ne. 1) goto 120

	! determine the bit precision of the event dpu major frame number
	name.c = major_name//null
	ok = w_get_item_substruct( %val(ch-1),
	1	eei(ch).event_type_b,
	1	name.b,
	1	it,
	1	%val(p_extract))
	if (ok .ne. 1) goto 140
	nbits_event_dpu_major = it.length

	! Calculate the difference in dpu major frames between the event dpu
	! major frame number and the HK dpu major frame number from the
	! major frame encompassing the first packet of the event by ert.
	old = event_dpu_major
	if (nbits_event_dpu_major .lt. 24) then
	   ! make adjustment: assume high order bits of the instrument's event
	   ! dpu MF are the same as the high order bits of the associated
	   ! housekeeping dpu MF
	   ! i = 'FFFFFFFF'x	! does not work with SC0.0 f77
	   i = -1
	   j = ishft(i, nbits_event_dpu_major)
	   k = j .and. assoc_hk_dpu_major
	   event_dpu_major = event_dpu_major .or. k
	end if
	diff = assoc_hk_dpu_major - event_dpu_major
	if (diff .lt. 0) goto 160

	w_get_ev_dpu_mfmf_and_hk_diff = 1
	return
 1	format(1x,'W_GET_EV_DPU_MFMF_AND_HK_DIFF: ', a,a)
 2	format(1x,'W_GET_EV_DPU_MFMF_AND_HK_DIFF: ', a,i9,'.',i3.3)
 120	w_get_ev_dpu_mfmf_and_hk_diff = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get item: ', major_name
	return

 130	w_get_ev_dpu_mfmf_and_hk_diff = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get item: ', minor_name
	return

 140	w_get_ev_dpu_mfmf_and_hk_diff = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get dbms structure for: ', major_name
	return

 160	w_get_ev_dpu_mfmf_and_hk_diff = w_no_dpu_mf_stream_synch
	if (wind_suppress_messages(ch)) return
	type 1, 'Event DPU MF and HK DPU MF are unreconcilable.'
	type *, '    Corrected Event DPU:', event_dpu_major
	type *, '     Original Event DPU:', old
	type *, '                 HK DPU:', assoc_hk_dpu_major
$IF ABSOFT_FORTRAN
	type 2, '  Event stream position MF.mf is',
	1	exi(ch).major, exi(ch).minor
$ELSE
	type 2, '  Event stream position MF.mf is', exi(ch).major, exi(ch).minor
$ENDIF
	return

	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_event_scet(ch,major,minor)
! Determines the event SCET, which is stored in an internal wind_lib buffer.
! For estimated scet, the bit rate (exi(ch).bit_rate) value must be correctly
! set and the ert time value correctly set (exi(ch).ert, exi(ch).ert1000).
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'ur8_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	integer*4	ok				! return status var
	integer*4	w_get_ev_dpu_mfmf_and_hk_diff	! a function
	integer*4	ur8_to_dbms			! a function
	integer*4	dbms_to_ur8			! a function
	real*8		ur8
	real*8		ur8delta
	integer*4	assoc_hk_dpu_major
	integer*4	event_dpu_major
	integer*4	event_dpu_minor
$IF ABSOFT_FORTRAN
	character*(*)	major_name, minor_name
	parameter	(major_name='DPU_MAJOR_FRAME')
	parameter	(minor_name='DPU_MINOR_FRAME')
$ELSE
	parameter	major_name='DPU_MAJOR_FRAME'
	parameter	minor_name='DPU_MINOR_FRAME'
$ENDIF
	integer*4	diff
	integer*4	scet_major
	integer*4	scet_minor
	logical*4	wind_suppress_messages
	logical*4	must_estimate
	logical*4	state
	integer*4	w_f_iiiiii
	integer*4	add
	integer*4	count /0/

	wind_get_event_scet = 0

! commented this out because new validation mechanism needs an event_scet
! estimate placed in exi(ch).scet by init_packet_dissect_info in order
! to retrieve items.  
!	exi(ch).scet(1) = 0
!	exi(ch).scet(2) = 0

	! get the dpu major frame number from the house keeping associated
	! with the major frame containing the first packet of
	! the desired event in the TM stream
	assoc_hk_dpu_major = exi(ch).dpu_major_ert

	! get the difference in terms of minor frames between the associated
	! HK dpu MF.mf and the event's dpu MF.mf
	ok =  w_get_ev_dpu_mfmf_and_hk_diff
	1	(ch,event_dpu_major,event_dpu_minor,
	1	assoc_hk_dpu_major,diff)
	if (ok .ne. 1) goto 120

!	type *, 'SCET: ch, mjr, mnr: ', ch, major, minor
!	type *, 'SCET: assoc_hk_dpu_major: ', assoc_hk_dpu_major
!	type *, 'SCET: exi(ch).mjr, exi(ch).mnr: ',
!	1	exi(ch).major, exi(ch).minor
!	type *, 'SCET: exi(ch).ert: ', 
!	1	exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000
!	type *, 'SCET: event dpu MF.mf:', event_dpu_major, event_dpu_minor
!	type *, 'SCET: diff:', diff

	! the scet of the event can be approximated by
	! the scet of the "disk" MF.mf that corresponds to dpu MF.mf specified
	! in the event
	scet_major = major - diff
	scet_minor = event_dpu_minor
	must_estimate = 0
	! because this may often fail in realtime+not_recording, suppress
	! any error messages from wind_get_record
	state = wind_suppress_messages(ch)
	call wind_set_message_state(ch,.true.)
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms), 
	1    ch, scet_major, scet_minor, 
	1    exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000)
	call wind_set_message_state(ch, state)
	if (ok .eq. 1) then
	   wind_get_event_scet = 1
	   return
	else if (ok .eq. w_beginning_of_file) then
	   must_estimate = 1
	else if (ok .eq. w_not_recording) then
	   must_estimate = 1
	else if (ok .ne. 1) then
	   ! here we regenerate the error on purpose
	   if (.not. state) then
	      ok = w_f_iiiiii(%val(user(ch).f_get_mf_scet_dbms),
	1          ch, scet_major, scet_minor,
	1    exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000)
	   end if
	   goto 230
	end if

	! the following code deals with estimating the event scet
!	count = count + 1
!	if (mod(count,100) .eq. 0) type *, count, ' SCET ESTIMATES MADE !!!'

	! determine the number of elapsed frames between start of event
	! and the theoretical mf at scet
	add = assoc_hk_dpu_major - event_dpu_major
	add = (add - 1) * 250
	add = add + exi(ch).minor + (250 - event_dpu_minor)
	diff = add

!	type *, 'SCET: new diff=', diff
!	type *, 'SCET: bit_rate:', exi(ch).bit_rate

	! assume the bit rate at ERT has been constant back through time
	! to the scet of the data; calculate a delta time correction
	if (exi(ch).bit_rate .eq. 0) then	! 1x speed (slow)
	   ur8delta = ur8_1x_mf_diff * diff
	else					! 2x speed (fast)
	   ur8delta = ur8_2x_mf_diff * diff
	end if

!	type *, 'SCET: ur8delta= ', ur8delta

	! convert the ert time to ur8
	ok = dbms_to_ur8(exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000, ur8)
	if (ok .ne. 1) goto 300

!	type *, 'SCET: ert in ur8 format: ', ur8

	! apply the delta time correction
	ur8 = ur8 - ur8delta

!	type *, 'SCET: scet in ur8 format: ', ur8
!xxxxxxxjk what about exi(ch).ur8_scet
	! convert the adjusted time to dbms format
$IF ABSOFT_FORTRAN
	ok = ur8_to_dbms(ur8, exi(ch).scet(1),
	1	exi(ch).scet(2), exi(ch).scet1000)
$ELSE
	ok = ur8_to_dbms(ur8, exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000)
$ENDIF
	if (ok .ne. 1) goto 310

!	type *, 'SCET: scet (dbms fmt): ', 
!	1	exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000

	wind_get_event_scet = 1
	return
 1	format(1x,'WIND_GET_EVENT_SCET: ', a,:,z8.8,'X')
 2	format(1x,'WIND_GET_EVENT_SCET: ', a,1x,i12,'.',i3.3)
 3	format(1x,'WIND_GET_EVENT_SCET: ', a,/,
	1	'   rate=', g10.4, ', diff=',i7, ', ERT:', a)

! 100	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot get associated HK for event, ok=', ok
!	return

! 110	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot get "DPU_MF" item from GLOBAL event entries, ok=',ok
!	return

! 114	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot get HK DPU MF, ok=',ok
!	return

 120	wind_get_event_scet = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get event DPU MF.mf or diff, ok=', ok
	return

 230	wind_get_event_scet = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get SCET of position, ok=',ok
	return

! 240	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot calculate the frame difference.'
!	return
!
! 242	wind_get_event_scet = 0
!	if (wind_suppress_messages(ch)) return
!	type 1, 'negative frame difference.'
!	return

! 246	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot get frame period, ok=', ok
!	return
!
! 248	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot convert real*8 time to system time.'
!	return
!
! 249	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 2, 'cannot get record for estimating SCET:',major,minor
!	return
!
! 250	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	i = sys_asctim(,str,scet,)
!	type 3, 'cannot subtract two system times', rate, diff, str
!	return
!
! 252	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot convert (calculated) SCET to 23-char format time.'
!	return

! 270	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot convert (record) SCET to 23-char format time.'
!	return

! 280	wind_get_event_scet = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot convert SCET to DBMS format time.'
!	return

! 290	wind_get_event_scet = w_bad_date_time_in_frame
!	if (wind_suppress_messages(ch)) return
!	type 1, 'cannot get fractional SCET.'
!	return

 300	wind_get_event_scet = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot convert DBMS fmt ERT to UR8, ok=',ok
!	type *, exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000
	return

 310	wind_get_event_scet = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot convert UR8 SCET estimate to DBMS fmt, ok=',ok
	return


	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_plain_fill_packet
	1			(ch,major,minor,event_name,direction,seek_1st)
! This routine gathers up a TM packet from the "fill" of the specified stream.
! The search
! begins at the specified major,minor frame position and continues, if
! necessary, using the direction and seek_1st arguments as criteria.
! The "packet" member of the user's wind_lib internal data structure is
! filled with the gathered packet.
! 
	implicit	none
	include		'wind_os_def.for'
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'

	integer*4	ch			! r
	integer*4	major			! rw
	integer*4	minor			! rw
	character*(*)	event_name		! rw, is 'FILL' on first call
	integer*4	direction		! r
	logical*4	seek_1st		! r, seek a 1st packet of event

	logical*4	waiting_for_fillidx_one
	logical*4	got_fill
	logical*4	building_fill_packet
	logical*4	looking_for_acceptable_packet 

	integer*4	i,j,k
	integer*4	ok
	integer*4	w_f_iiiiiib		! a function
	integer*4	any_id
	integer*4	fillptr
	integer*4	fillidx
	record /low_byte/ lb
	integer*4	expected_fillidx
	integer*4	size_of_fill
	integer*4	wid_get_item_xlate
	external	wid_get_item_xlate !$pragma C ( wid_get_item_xlate )
$IF ABSOFT_FORTRAN
	integer*4	all_subtypes
	parameter	(all_subtypes='0000ffff'x)
$ELSE
	parameter	all_subtypes='0000ffff'x
$ENDIF
	integer*4	id
	integer*4	n_bytes_copied
	record /c_string_32/	c32
	record /c_string_16/	c16
	record /c_string_8/	c8
	byte		fillpacket(512)

	wind_get_plain_fill_packet = 0

	! get a fill packet
	any_id = -99
	looking_for_acceptable_packet = .true.
	do while(looking_for_acceptable_packet)
	   expected_fillidx = 1
	   waiting_for_fillidx_one = .true.
	   do i=1,431
	      fillpacket(i) = 0
	   end do
	   building_fill_packet = .true.
	   n_bytes_copied = 0

	   do while(building_fill_packet)
	      ok = w_f_iiiiiib( %val(user(ch).f_get_plain_packet),
!	1          ch,major,minor,any_id,direction,1,user(ch).packet.w)
	1          ch,major,minor,any_id,direction,0,user(ch).packet.w)
	      if (ok .ne. 1) goto 30
	      fillptr = zext(user(ch).packet.w(2))
	      i = zext(user(ch).packet.w(1))
	      if (btest(i,0)) fillptr = ibset(fillptr,8)
	      if (fillptr.le.428 .and. fillptr.ge.3) then
	         size_of_fill = 431 - fillptr - 2 - 1
	         lb.i4val = 0
	         lb.b  = user(ch).packet.w(fillptr)
	         lb.b1 = user(ch).packet.w(fillptr+1)
	         fillidx = lb.i4val
	         if (fillidx .eq. 1) waiting_for_fillidx_one = .false.
	         got_fill = (fillidx .gt. 0)

	         if ((.not. waiting_for_fillidx_one) .and. got_fill) then
	            ! make sure the the fill index is the expected value
	            if (expected_fillidx .ne. fillidx) then
	               goto 40
	            end if
	            expected_fillidx = fillidx + size_of_fill
	            ! copy the fill to another packet we are building up
	            j = fillidx 
	            k = fillptr + 2
	            do i=1,size_of_fill
	               if (j.le.431) fillpacket(j) = user(ch).packet.w(k)
	               j = j + 1
	               k = k + 1
	            end do
	            if (j.gt.431) building_fill_packet = .false.
	            n_bytes_copied = j - 1
	         end if
	      end if
	      if (building_fill_packet) then
		 call wind_tm_increment_packet(major,minor)
	      end if
	   end do

	   if (seek_1st) then
	      ! low bit is set for 1st packet flag
!	      looking_for_acceptable_packet = .not. zext(fillpacket(1))
	      j = zext(fillpacket(1))
	      looking_for_acceptable_packet = (j .and. 1) .eq. 0
	   else
	      looking_for_acceptable_packet = .false.
	   end if
	end do

	! copy the fill packet into the user's packet buffer for this channel
	j = 0
	do i=1,431
	   user(ch).packet.w(j) = fillpacket(i)
	   j = j + 1
	end do

	if (seek_1st) then
	   ! rename the event from FILL to whatever event name corresponds
	   ! to the packet id
	   i = zext(fillpacket(1))
	   id = i/16 	! shift right 4 bits
	   c8.c = 'GLOBAL'//char(0)
	   c16.c = 'PACKET_TYPE'// char(0)
	   ok = wid_get_item_xlate(%val(ch-1), c8.b, c16.b, id, 
	1	c32.b, %val(32), %val(0))
	   if (ok .ne. 1) goto 20
	   event_name = c32.c
	end if

	wind_get_plain_fill_packet = 1

  1	format(1x, 'WIND_GET_PLAIN_FILL_PACKET: ', a)
  7	format(3x, 'Expecting ', i4, ' (', z8.8,'x)',
	1          ' but got ',  i4, ' (', z8.8, 'x).' )
	return
 20	wind_get_plain_fill_packet = ok
	if (user(ch).suppress_messages) return
	type 1, 'Cannot get reverse xlate for event name.'
	type *, '  Make sure event GLOBAL, item PACKET_SUBTYPE, ',
	1	'is in the item database.'
	return
 30	wind_get_plain_fill_packet = ok
	if (user(ch).suppress_messages) return
	type 1, 'Cannot get packet to extract fill from.'
	return
 40	wind_get_plain_fill_packet = 0
	if (user(ch).suppress_messages) return
	type 1, 'Fill sequential index counter is out of sequence.'
	type 7, expected_fillidx, expected_fillidx, fillidx, fillidx
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_mjr_fr_event
	1				(ch,major,minor)
! Gets "major frame" event and places it in an internal buffer.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	w_f_iiii
	integer*4	i,j,k,m
	integer*4	o
	integer*4	num_missing
	integer*4	got_a_record
	integer*4	ok
$IF ABSOFT_FORTRAN
!	character*(*)	rn
!	parameter	(rn='W_GET_MJR_FR_EVENT')
$ELSE
!	parameter	rn='W_GET_MJR_FR_EVENT'
$ENDIF
	integer*4	get_instrument_version		! a function
	integer*4	w_get_dpu_mjr_fr_num		! a function
	integer*4	wnd_to_dbms			! a function
	integer*4	wnd_to_ur8			! a function
	integer*4	dbms_to_ur8			! a function
	logical*4	first_rec

	k = 0
	first_rec = .true.
	num_missing = 0
	w_get_mjr_fr_event = 0
	eei(ch).event_type	= 'MAJOR_FRAME'//char(0)

	do i=min_minor_frame_num,max_minor_frame_num
	   got_a_record = w_f_iiii(%val(user(ch).f_get_intrnl_rec), 
	1	ch, major, i, user(ch).wind_record)
	   if (got_a_record .eq. 1) then
	      m = k
	      do j=0,255
	         eb(ch).major_frame(m) = user(ch).wind_record.data(j)
	         m = m + 1
	      end do
	      if (first_rec) then
	         ! use the first valid minor frame for event timing info,
	         ! which may not always be mf# 0.  At this writing the
	         ! "event time" for the major frame event is not critical.
	         exi(ch).minor = i
	         exi(ch).major = major
	         ok = wnd_to_dbms(user(ch).wind_record.scet,
	1	   exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000)
	         exi(ch).ert(1) = exi(ch).scet(1)
	         exi(ch).ert(2) = exi(ch).scet(2)
	         ok = dbms_to_ur8(
	1             exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000,
	1	      exi(ch).ur8_scet)
	         exi(ch).ur8_context = exi(ch).ur8_scet
	         exi(ch).ur8_ert     = exi(ch).ur8_scet
	         first_rec = .false.
	      end if
	   else
	      num_missing = num_missing + 1
	      do j=0,255
	         eb(ch).major_frame(k+j) = -1
	      end do
	   end if
	   k = k + 256
	end do

	! get the end-of-event time
	ok = wnd_to_ur8(user(ch).wind_record.scet, exi(ch).ur8_eoe)

	! get instrument version numbers from associated HK
	ok = get_instrument_version(ch, 'DPU', exi(ch).dpu_version)
	ok = get_instrument_version(ch, 'FFT', exi(ch).fft_version)
	ok = get_instrument_version(ch, 'TDS', exi(ch).tds_version)

	! get the DPU major frame number
	ok = w_get_dpu_mjr_fr_num(ch,major,minor,exi(ch).dpu_major_ert)

	if (num_missing .ne. 0) goto 10
	w_get_mjr_fr_event = 1

	return
  1	format(1x,'W_GET_MJR_FR_EVENT: ', a, i3)
 10	w_get_mjr_fr_event = w_missing_record
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Missing minor frame(s), count=',
	1	num_missing
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_mnr_fr_event
	1				(ch,major,minor)
! Gets a "minor frame" event and places it in an internal buffer.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	w_f_iiii
	integer*4	j,k
	integer*4	o
	integer*4	num_missing
	integer*4	got_a_record
	integer*4	ok
$IF ABSOFT_FORTRAN
!	character*(*)	rn
!	parameter	(rn='W_GET_MNR_FR_EVENT')
$ELSE
!	parameter	rn='W_GET_MNR_FR_EVENT'
$ENDIF
	integer*4	get_instrument_version		! a function
	integer*4	w_get_dpu_mjr_fr_num		! a function
	integer*4	wnd_to_dbms			! a function
	integer*4	dbms_to_ur8			! a function

	k = 0
	num_missing = 0
	w_get_mnr_fr_event = 0
	eei(ch).event_type	= 'MINOR_FRAME'//char(0)

	got_a_record = w_f_iiii(%val(user(ch).f_get_intrnl_rec), 
	1	ch, major, minor, user(ch).wind_record)
	if (got_a_record .eq. 1) then
	   do j=0,255
	      eb(ch).minor_frame(j) = user(ch).wind_record.data(j)
	   end do
	   exi(ch).minor = minor
	   exi(ch).major = major
	   ok = wnd_to_dbms(user(ch).wind_record.scet,
	1       exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000)
	   exi(ch).ert(1) = exi(ch).scet(1)
	   exi(ch).ert(2) = exi(ch).scet(2)
	   ok = dbms_to_ur8(
	1       exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000,
	1	exi(ch).ur8_scet)
	   exi(ch).ur8_context = exi(ch).ur8_scet
	   exi(ch).ur8_ert     = exi(ch).ur8_scet
	   exi(ch).ur8_eoe     = exi(ch).ur8_scet
	else
	   do j=0,255
	      eb(ch).minor_frame(j) = -1
	   end do
	end if

	! get instrument version numbers from associated HK
	ok = get_instrument_version(ch, 'DPU', exi(ch).dpu_version)
	ok = get_instrument_version(ch, 'FFT', exi(ch).fft_version)
	ok = get_instrument_version(ch, 'TDS', exi(ch).tds_version)

	! get the DPU major frame number
	ok = w_get_dpu_mjr_fr_num(ch,major,minor,exi(ch).dpu_major_ert)

	if (got_a_record .ne. 1) goto 10
	w_get_mnr_fr_event = 1

	return
  1	format(1x,'W_GET_MNR_FR_EVENT: ', a, i9,'.',i3.3)
 10	w_get_mnr_fr_event = w_missing_record
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Missing minor frame at ',major, minor
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_packet_event
	1				(ch,major,minor)
! Gets a "packet" event and places it in an internal buffer.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	w_f_iiiiiib
	integer*4	i,j,k
	integer*4	o
	integer*4	num_missing
	integer*4	got_a_packet
	integer*4	ok
$IF ABSOFT_FORTRAN
!	character*(*)	rn
!	parameter	(rn='W_GET_PACKET_EVENT')
$ELSE
!	parameter	rn='W_GET_PACKET_EVENT'
$ENDIF
	integer*4	get_instrument_version		! a function
	integer*4	w_get_dpu_mjr_fr_num		! a function
$IF ABSOFT_FORTRAN
	integer*4	wnd_to_dbms			! a function
$ENDIF
	integer*4	dbms_to_ur8			! a function
	integer*4	w_f_iiiiii			! a function
	integer*4	end_minor
	record /low_byte/ lb

	k = 0
	num_missing = 0
	w_get_packet_event = 0
	eei(ch).event_type = 'PACKET'//char(0)

	! call the packet retrieval routine
	got_a_packet = w_f_iiiiiib( %val(user(ch).f_get_plain_packet),
	1	ch,major,minor,
	1	-1,forward,0,		! any pkt id, forward, not first pkt ok
	1	eb(ch).packet)
	! test for successful packet get is below

	! set the position fields for frame numbers
	end_minor = minor
	exi(ch).minor = minor
	exi(ch).major = major

	! get dbms time of first record in packet
	if (minor .ne. 0) then
	   minor = minor - (mod(minor,10))
	end if
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms),
	1	ch,major,minor,
	1	exi(ch).ert(1),
	1	exi(ch).ert(2),
	1	exi(ch).ert1000)
	ok = dbms_to_ur8(
	1       exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000,
	1	exi(ch).ur8_ert)
	exi(ch).scet(1) = exi(ch).ert(1)
	exi(ch).scet(2) = exi(ch).ert(2)
	exi(ch).ur8_context = exi(ch).ur8_scet
	exi(ch).ur8_ert     = exi(ch).ur8_scet

	! now set the end-of-event ur8 time field
	ok = w_f_iiiiii( %val(user(ch).f_get_mf_scet_dbms),
	1	ch,major,minor,
	1	i,j,k)
	ok = dbms_to_ur8(i,j,k, exi(ch).ur8_eoe)

	! set current position in main user structure
	user(ch).wind_record.major_frame = major
	lb.i4val = minor
	user(ch).wind_record.minor_frame = lb.b

	! get instrument version numbers from associated HK
	ok = get_instrument_version(ch, 'DPU', exi(ch).dpu_version)
	ok = get_instrument_version(ch, 'FFT', exi(ch).fft_version)
	ok = get_instrument_version(ch, 'TDS', exi(ch).tds_version)

	! get the DPU major frame number
	ok = w_get_dpu_mjr_fr_num(ch,major,minor,exi(ch).dpu_major_ert)

	! get the sc mode

	! get the current bit rate

	if (got_a_packet .ne. 1) goto 10
	w_get_packet_event = 1

	return
  1	format(1x,'W_GET_PACKET_EVENT: ', a, i9,'.',i3.3)
 10	w_get_packet_event = w_missing_record
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Missing minor frame at ',major, minor
	return
	end