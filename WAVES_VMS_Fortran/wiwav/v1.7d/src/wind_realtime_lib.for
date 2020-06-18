! wind_realtime_lib.for - routines for accessing wind/waves realtime tm stream

!------------------------------------------------------------------------------
! Sets up realtime-specific internal data processing elements.
!------------------------------------------------------------------------------
	integer*4	function	w_setup_rt_stream(ch) !,f)
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_realtime_def.for'
	integer*4	ch
!	character*(*)	f		! argument not used
	integer*4	ok
	integer*4	i
	integer*4	ios
	external	w_rt_close_stream
	external	w_rt_get_rec
	external	w_rt_frame_rate
	external	w_rt_scet_of_mfmf
	external	w_rt_scet_dbms_of_mfmf
	external	w_rt_get_packet
	external	w_rt_get_xtra_event_info
	external	w_rt_get_rec_by_time
	external	w_rt_get_word
	external	w_rt_get_mode
	external	w_rt_get_hk
	external	w_rt_get_dpu_mf
	external	w_rt_get_mfmf
	integer*4	wind_open_realtime
	logical*4	wind_suppress_messages

	w_setup_rt_stream = 0

	ok = wind_open_realtime(ch)
	if (ok .ne. 1) goto 10

	! these values must be non-zero: use dummy functions if necessary
	user(ch).f_get_word            = %loc(w_rt_get_word)
	user(ch).f_get_tm_mode         = %loc(w_rt_get_mode)
	user(ch).f_get_intrnl_rec      = %loc(w_rt_get_rec)
	user(ch).f_get_plain_packet    = %loc(w_rt_get_packet)
	user(ch).f_goto_time           = %loc(w_rt_get_rec_by_time)
	user(ch).f_get_mf_scet         = %loc(w_rt_scet_of_mfmf)
	user(ch).f_get_mf_scet_dbms    = %loc(w_rt_scet_dbms_of_mfmf)
	user(ch).f_get_frame_rate      = %loc(w_rt_frame_rate)
	user(ch).f_get_xtra_event_info = %loc(w_rt_get_xtra_event_info)
	user(ch).f_close_ch            = %loc(w_rt_close_stream)
	user(ch).f_get_hk              = %loc(w_rt_get_hk)
	user(ch).f_get_dpu_mf          = %loc(w_rt_get_dpu_mf)
	user(ch).f_get_rec_idx         = %loc(w_rt_get_mfmf)

	rltm(ch).wait_for_new_records	= .true.
	rltm(ch).ch = ch

	do i=0,sizeof_minor_frame-1
	   user(ch).is_a_valid_ptr(i) = rltm(ch).is_a_valid_ptr(i)
	end do 

	w_setup_rt_stream = 1

	return
   1	format(1x,'W_SETUP_RT_STREAM: ', a)
  10	continue
	w_setup_rt_stream = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) ' cannot setup realtime TM stream.'
	return
	end

!------------------------------------------------------------------------------
! Handles the special cases of updating the various realtime stream positioning 
! capabilities provided by wind_lib (e.g.: earliest_mfmf, latest_mfmf).
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_mfmf
	1				(ch,major,minor,position_type)
	implicit	none
	include		'parm_def.for'
	include		'glbsec_def.for'
	include		'wind_realtime_def.for'
	include		'low_byte_def.for'
	include		'wind_wnd_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	position_type

	integer*4	ok
	integer*4	ios
	integer*4	i
	integer*4	w_wnd_close_stream
	integer*4	w_wnd_open_file
	logical*4	wind_suppress_messages

	w_rt_get_mfmf = 0

	if (position_type .eq. w_tk_stream_mfmf) then
	   major = glsec.last_major_validated
	   minor = glsec.last_minor_validated
	else if (position_type .eq. w_tk_latest_mfmf) then
	   major = glsec.last_major_validated
	   minor = glsec.last_minor_validated
	else if (position_type .eq. w_tk_earliest_mfmf) then
	   ! is there a recording file?
	   if (glsec.filename(1:1) .gt. ' ') then
	      if (rltm(ch).file .eq. glsec.filename) then
	         ! the offline file has already been opened and is current
	         major = wnd(ch).first_major
	         minor = wnd(ch).first_minor.i4val
	      else if (rltm(ch).file(1:1) .gt. ' ') then
	         ! the offline file is not the current recording file
	         ! so close it and open the new one
	         ok = w_wnd_close_stream(ch)
	         rltm(ch).file = ' '
	         rltm(ch).file = glsec.filename
	         ok = w_wnd_open_file(ch,rltm(ch).file)
	         if (ok .ne. 1) goto 20
	         major = wnd(ch).first_major
	         minor = wnd(ch).first_minor.i4val
	      else
	         ! the current recording file has not been opened yet,
                 ! so open it now
	         rltm(ch).file = glsec.filename
	         ok = w_wnd_open_file(ch,rltm(ch).file)
	         if (ok .ne. 1) goto 20
	         major = wnd(ch).first_major
	         minor = wnd(ch).first_minor.i4val
	      end if
	   else
	      if (rltm(ch).file(1:1) .ge. ' ') then
	         ! there was a recording file, but now there is not
	         ok = w_wnd_close_stream(ch)
	         rltm(ch).file = ' '
	      end if
	      if (glsec.number_of_interrupts .gt. 491) then
	         ! when purely online and record buffer is full
	         ! the oldest record in the ring buffer is at glsec.pointer+1,
	         ! we allow a margin of ten records because the very oldest 
	         ! record is very likely to get overwritten before the user 
	         ! can get it
	         i = glsec.pointer + 10
	         if (i.gt.500) i = i - 500
	         major = glsec.recs(i).major_frame
	         minor = zext(glsec.recs(i).minor_frame)
	      else if (glsec.number_of_interrupts .le. 491) then
	         ! when purely online and record buffer is partially 
	         ! full we are in a startup state
	         major = glsec.recs(1).major_frame
	         minor = zext(glsec.recs(1).minor_frame)
	      end if
	   end if
	else if (position_type .eq. w_tk_current_mfmf) then
	   major = rltm(ch).rec.major_frame
	   minor = zext(rltm(ch).rec.minor_frame)
	else
	   goto 30
	end if

	w_rt_get_mfmf = 1

	return
  1	format(1x,'W_RT_GET_MFMF: ', a)
 20	continue
	w_rt_get_mfmf = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot open current recording file.'
	write(6,'(1x,a,a)',iostat=ios) '  File: ', rltm(ch).file(:60)
	return
 30	continue
	w_rt_get_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'unknown MF.mf positioning token'
	return
	end

!------------------------------------------------------------------------------
! Performs channel closing operations specific to the realtime TM stream.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_close_stream(ch)
	implicit	none
	include		'parm_def.for'
	include		'glbsec_def.for'
	include		'wind_realtime_def.for'
	integer*4	ch
	integer*4	ok
	integer*4	w_wnd_close_stream
	integer*4	ios

	if (rltm(ch).file(1:1) .gt. ' ') then
	   ok = w_wnd_close_stream(ch)
	   if (ok .ne. 1) then
	      write(6,1,iostat=ios) 'cannot close associated offline file.'
	      write(6,'(1x,a,a)',iostat=ios) '  File: ', rltm(ch).file(:60)
	   end if
	end if

	rltm(ch).file = ' '
	rltm(ch).ch = 0

	w_rt_close_stream = 1
	return
  1	format(1x,'W_RT_CLOSE_STREAM: ', a, :, a)
	end

!------------------------------------------------------------------------------
! Returns one word of data from the realtime stream.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_word
	1				(ch,major,minor,word,buf)
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	integer*4	word				! word in mnr fr to get
	byte		buf				! byte size buffer

	integer*4	ok
	integer*4	ios
	logical*4	wind_suppress_messages
	integer*4	w_rt_set_rec

	w_rt_get_word = 0

	ok = w_rt_set_rec(ch,major,minor)
	if (.not. ok) goto 10

	buf = rltm(ch).rec.data(word)

	w_rt_get_word = 1
	return
   1	format(1x,'W_RT_GET_WORD: ', a)
 10	w_rt_get_word = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) ' cannot get realtime record.'
	return
	end

!------------------------------------------------------------------------------
! Gets the sc TM mode and converts it to the equivalent cdhf value
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_mode
	1			(ch,major,minor,mode)
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	include		'rt_mode_def.for'
	integer*4	ch			! index into user's structure
	integer*4	major			! major frame number
	integer*4	minor			! minor frame number
	integer*4	mode			! tm mode
	integer*4	imode
	integer*4	mode_byte
	integer*4	speed
	logical*4	wind_suppress_messages
	integer*4	ok
	integer*4	w_rt_set_rec
	integer*4	ios
	integer*4	mnr
	integer*4	i
	integer*4	w_rt_tm_speed
	logical*4	caller_wants_speed

	w_rt_get_mode = 0

	mnr = minor
	i = mod(mnr,5)
	if (i .ne. 0) then
	   mnr = mnr - i
	end if

	ok = w_rt_set_rec(ch,major,mnr)
	if (ok.ne.1) goto 10

	imode = rltm(ch).rec.data(4)
	imode = imode .and. '00f0'x
	caller_wants_speed = .false.
	goto 1000


	!----------------------------------------------------------------------
	entry	w_rt_tm_speed(ch,major,mode_byte, speed)
	imode = mode_byte .and. '00f0'x
	caller_wants_speed = .true.

 1000	continue

	if (imode .eq. s1) then
	   if (caller_wants_speed) then
	      speed = 1
	   else
	      mode = science_1x
	   end if
	else if (imode .eq. s2) then
	   if (caller_wants_speed) then
	      speed = 2
	   else
	      mode = science_2x
	   end if
	else if (imode .eq. m1) then
	   if (caller_wants_speed) then
	      speed = 1
	   else
	      mode = maneuver_1x
	   end if
	else if (imode .eq. m2) then
	   if (caller_wants_speed) then
	      speed = 2
	   else
	      mode = maneuver_2x
	   end if
	else if (imode .eq. c1) then
	   if (caller_wants_speed) then
	      speed = 1
	   else
	      mode = contingency_1x
	   end if
	else if (imode .eq. c2) then
	   if (caller_wants_speed) then
	      speed = 2
	   else
	      mode = contingency_2x
	   end if
	else
	   ! just pass on the funny value
	   if (.not. wind_suppress_messages(ch)) then
	      write(6,1,iostat=ios) 'unrecognized mode: ', imode
	   end if
	   if (caller_wants_speed) then
	      speed = 9
	   else
	      mode = imode
	   end if
	end if

	w_rt_get_mode = 1

	return
  1	format(1x,'W_RT_GET_MODE: ', a, i)
 10	continue
	w_rt_get_mode = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'error getting record, ok= ', ok
	return
	end

!------------------------------------------------------------------------------
! Sets the current realtime record to specified frame numbers.  May call 
! w_rt_get_rec to copy specified record to internal buffer.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_set_rec(ch,major,minor)
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'low_byte_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	record		/low_byte/	minor
	integer*4	ios
	integer*4	ok
	integer*4	w_rt_get_rec
	logical*4	wind_suppress_messages

	w_rt_set_rec = 0

	if (rltm(ch).rec.major_frame .eq. major .and.
	1   rltm(ch).rec.minor_frame .eq. minor.b) then
	   w_rt_set_rec = 1
	   return
	end if

	ok = w_rt_get_rec(ch, major, minor, rltm(ch).rec)
	if (ok .ne. 1) goto 10

	w_rt_set_rec = 1

	return
  1	format(1x,'W_RT_SET_REC: ', a)
 10	continue
	w_rt_set_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) ' cannot set to specified frame.'
	return
	end

!------------------------------------------------------------------------------
! Copies the indicated record into the record buffer of the specified
! channel.  If the desired record is in the TM gatherer's global section
! the record is simply copied.  If the desired record has already been written
! to disk, then the record is read from disk.  If the desired record is yet
! to be received, the "wait_for_new_records" flag for the channel is checked.
! If wait_for_new_records is true, we use the current bit rate to calculate
! a time span to wait and reiterate.  When wait_for_new_records is false,
! a "not_waiting" error is returned.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_rec
	1				(ch,major,minor,wr)
	implicit	none
	include		'parm_def.for'
	include		'glbsec_def.for'
	include		'wind_realtime_def.for'
	include		'low_byte_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	record		/low_byte/	minor
	record 		/wind_record/	wr	! main wind lib internal buffer
	integer*4	ok
	integer*4	diff
	integer*4	wind_tm_delta_mfmf
	integer*4	ptr, vmajor, vminor
	real*4		wait_time
	integer*4	o
	logical*4	wind_suppress_messages
	integer*4	w_rt_get_offline_rec

	w_rt_get_rec = 0

	if (rltm(ch).rec.major_frame .eq. major .and.
	1   rltm(ch).rec.minor_frame .eq. minor.b) then
	   wr = rltm(ch).rec
	   w_rt_get_rec = 1
	   return
	end if

 100	ptr = glsec.pointer
	if (ptr.lt. 1) goto 10
	vmajor = glsec.recs(ptr).major_frame
	vminor = zext(glsec.recs(ptr).minor_frame)

	! routine wind_tm_delta_mfmf checks ranges of first 4 args
	ok = wind_tm_delta_mfmf(major,minor,vmajor,vminor,diff)
	if (.not. ok) goto 30

	if (diff .eq. 0) then
	   ! current record is the desired record
	   wr = glsec.recs(ptr)
	   w_rt_get_rec = 1
	else if (diff .lt. 0) then
	   ! desired record is in the future
	   if (rltm(ch).wait_for_new_records) then
	      ! estimate the wait time
	      wait_time = glsec.frame_rate * abs(diff)
	      wait_time = amax1(wait_time, 0.1)
	      wait_time = amin1(wait_time, 1.0)
	      call lib$wait(wait_time)
	      goto 100
	   else
	      goto 40
	   end if
	else if (diff .lt. 490) then
	   ! desired record is in the global section
	   ptr = ptr - diff
	   if (ptr .lt. 1) ptr = 500 + ptr
	   wr = glsec.recs(ptr)
	   if (wr.minor_frame .ne. minor.b) goto 50
	   w_rt_get_rec = 1
	else
	   ! the desired record is on disk if recording, update
	   ok = w_rt_get_offline_rec(ch,major,minor,wr)
	   if (ok .ne. 1) goto 60
	   w_rt_get_rec = 1
	end if

	return
  1	format(1x,'W_RT_GET_REC: ', a, a)

 10	w_rt_get_rec = w_no_realtime
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 
	1 'Global section pointer is zero, is GSE up? Telemetry?'
	return

 30	w_rt_get_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 'Invalid frame number(s).'
	return

 4	format(1x,'W_RT_GET_REC: ',a,' major=',i,',minor=',i3,'.')
 40	w_rt_get_rec = w_no_record_waiting
	if (wind_suppress_messages(ch)) return
	write(6,4,iostat=o) 'NOT waiting for record',
	1	major, minor.i4val
	return

 5	format(1x,a,i3,a,i3,a,i3,'.')
 50	w_rt_get_rec = w_unexpected_record
	if (wind_suppress_messages(ch)) return
	write(6,5,iostat=o) 'W_RT_GET_REC: Unexpected record, wanted mf ',
	1	 minor.i4val, ' but got ',
	1	 zext(rltm(ch).rec.minor_frame), ', ptr=', ptr
	return

 60	w_rt_get_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 'cannot get specified offline disk record.'
	return
	end

!------------------------------------------------------------------------------
! This routine attempts to get the specified record from the offline .wnd file.
! There are several possibliities:
!	1) the specified record is found in the current offline file
!	2) disk recording is not enabled, the record is ungettable
!	3) disk recording is enabled, but recording has stopped and restarted
!	   since the user first opened the realtime stream, leaving the
!	   the desired record in an (as yet) unopened file.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_offline_rec(ch,major,minor,wr)
	implicit	none
	include		'parm_def.for'
	include		'glbsec_def.for'
	include		'wind_realtime_def.for'
	include		'low_byte_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	record /wind_record/	wr		! main wind lib internal buffer

	integer*4	w_wnd_get_rec_xc
	integer*4	w_wnd_close_stream
	integer*4	w_wnd_open_file
	logical*4	wind_suppress_messages
	integer*4	ok
	integer*4	ios

	w_rt_get_offline_rec = 0

	! case of not currently recording
	if (glsec.filename(1:1) .le. ' ') then
	   ! not recording
	   if (rltm(ch).file(1:1) .gt. ' ') then
	      ! there was an offline recording file, close it now
	      ok = w_wnd_close_stream(ch)
	      rltm(ch).file = ' '
	   end if
	   goto 10
	end if

	! case of recording, but this is the first disk access
	if (rltm(ch).file(1:1) .le. ' ') then
	    rltm(ch).file = glsec.filename
	    ok = w_wnd_open_file(ch,rltm(ch).file)
	    if (ok .ne. 1) goto 20
	end if

	! update the end-of-file frame numbers
	if (wnd(ch).last_major .lt. glsec.last_major_written) then
	   wnd(ch).last_major = glsec.last_major_written
	   wnd(ch).last_minor.b = glsec.last_minor_written_b
	end if

	ok = w_wnd_get_rec_xc(ch, major, minor, wr)

	if (ok .ne. 1) then
	   ! has recording restarted?
	   if (glsec.filename .ne. rltm(ch).file) then
	      ! recording has restarted
	      ok = w_wnd_close_stream(ch)
	      rltm(ch).file = glsec.filename
	      ok = w_wnd_open_file(ch,rltm(ch).file)
	      if (ok .ne. 1) goto 20
	      ok = w_wnd_get_rec_xc(ch, major, minor, wr)
	      if (ok .ne. 1) goto 30
	   end if
	end if

	w_rt_get_offline_rec = 1

	return
  1	format(1x,'W_RT_GET_OFFLINE_REC: ', a, a)
  2	format(1x,'W_RT_GET_OFFLINE_REC: ', a, /,
	1	'User Ch:',i2,1x,i12,'.',i3.3)

  10	w_rt_get_offline_rec = w_not_recording
	if (wind_suppress_messages(ch)) return
	type 2, 'Record not in buffer and disk recording not enabled.',
	1	ch, major, minor
	return

  20	w_rt_get_offline_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot open offline file named:'
	write(6,'(1x,a)',iostat=ios) rltm(ch).file(:60)
	rltm(ch).file = ' '
	return

  30	w_rt_get_offline_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get offline record:'
	return
	end

!------------------------------------------------------------------------------
! This routine enables the user to control wether or not to wait for
! new telemetry to come in when the user requests a minor frame that is
! yet to be received.
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_set_wait(ch)
	implicit	none
	integer*4	ch
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for/nolist'
	logical*4	wind_suppress_messages

	wind_tm_set_wait = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	rltm(ch).wait_for_new_records = .true.

	wind_tm_set_wait = 1
	return
  1	format(1x,'WIND_TM_SET_WAIT: ', a)
 08	wind_tm_set_wait = w_bad_channel
	if (wind_suppress_messages(ch)) return
	type 1, 'invalid channel number.'
	return
	end

!------------------------------------------------------------------------------
! This routine enables the user to control wether or not to wait for
! new telemetry to come in when the user requests a minor frame that is
! yet to be received.
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_set_nowait(ch)
	implicit	none
	integer*4	ch
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for/nolist'
	logical*4	wind_suppress_messages

	wind_tm_set_nowait = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	rltm(ch).wait_for_new_records = .false.

	wind_tm_set_nowait = 1
	return
  1	format(1x,'WIND_TM_SET_NOWAIT: ', a)
 08	wind_tm_set_nowait = w_bad_channel
	if (wind_suppress_messages(ch)) return
	type 1, 'invalid channel number.'
	return
	end

!------------------------------------------------------------------------------
! Maps to the common global section used by the Gatherer process
! to buffer telemetry data as it comes in.
!------------------------------------------------------------------------------
	integer*4	function	wind_open_realtime(ch)
	implicit	none
	integer*4	ch
	include		'parm_def.for'
	include		'glbsec_def.for/nolist'
	include		'wind_realtime_def.for/nolist'
	include		'low_byte_def.for'
	include		'wind_return_code_def.for/nolist'
	include		'($secdef)'
	integer*4	addr(2)
	integer*4	sys$mgblsc
	integer*4	flags /sec$m_sysgbl/
!	parameter	null=char(0)
	integer*4	ok
	integer*4	i
	integer*4	first_time /1/
	logical*4	wind_suppress_messages

	wind_open_realtime = 0

	! map to the TM gatherer's global section
	if (first_time) then
	   addr(1) = %loc( glsec )
	   addr(2) = addr(1) + sizeof(glsec) - 1
	   ok = sys$mgblsc(addr,,,%val(flags),
	1	wind_tm_glbsec_name,,)
	   if (.not. ok) goto 10
	   first_time = 0
	end if

	! read in the most recent TM record and make it the user's current rec
	i = glsec.pointer
	if (i .lt. 1 .or. i .gt. 500) goto 20
	rltm(ch).rec = glsec.recs(i)

	rltm(ch).num_tm_ptrs	= glsec.num_tm_keeper_words
	do i=0,sizeof_minor_frame-1
	   rltm(ch).is_a_valid_ptr(i) = .false.
	end do
	do i=0,rltm(ch).num_tm_ptrs
	   rltm(ch).tm_ptrs(i) = zext(glsec.tm_keeper_words(i))
	   rltm(ch).is_a_valid_ptr(rltm(ch).tm_ptrs(i)) = .true.
	end do

	wind_open_realtime = 1

	return
  1	format(1x,'WIND_OPEN_REALTIME: ',a,:,a)
 10	wind_open_realtime = w_no_realtime
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot access TM realtime data buffers.'
	type '(1x,a,z8.8)',
	1	'Cannot map '//wind_tm_glbsec_name//
	1	' global section, status: ',
	1	 ok
	type *, 'GSE software is probably not running (gss/start/all).'
	return
 20	wind_open_realtime = w_ungettable_record
	if (wind_suppress_messages(ch)) return
	type 1, 'invalid realtime ring buffer index.'
	type *, 'Is telemetry active/available?'
	return
	end

!------------------------------------------------------------------------------
! Gathers a packet of data from a .wnd file or the realtime stream. 
! If argument "type" is not a valid
! packet type (between 0 and 15) the current packet, specified by major,minor
! frame number arguments, is selected.  Otherwise the type, direction, and
! seek_1st flag arguments are used to prescribe packet search and selection
! criteria.
! The user's major and minor arguments are advanced to point to the last
! frame of the selected packet on return from this routine.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_packet
	1			 (ch,major,minor,type,direction,seek_1st,pkt)
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'

	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	type
	integer*4	direction
	logical*4	seek_1st
	byte		pkt(0:*)

	integer*4	w_rt_set_rec
	integer*4	zext_bits_to_longword
	logical*4	wind_suppress_messages
	integer*4	i,j,k,n
	include		'wind_packet_addresses_def.for'
	integer*4	ok
	integer*4	mode
	integer*4	my_type
	integer*4	word
	integer*4	check

	logical*4	good_frame
	logical*4	any_bad_frame
	logical*4	science_mode	
	logical*4	maneuver_mode	
	logical*4	first_frame_ok
	logical*4	by_type

	w_rt_get_packet = 0

	! adjust minor frame number to point to beginning of packet
	! --this could let major,minor indicate a nonexistent record,
	! --such as could happen at the beginning of a file
	if (mod(minor,10) .ne. 0) then
	   minor = minor - mod(minor,10)
	end if

	! This loop gets the first frame of the packet, searching forwards
	! or backwards through the data looking for a specific packet type
	! or a first packet of a specific event (packet_type=event_type, here)
	! as prescribed by the calling arguments.
	by_type = type .ge. min_packet_id .and. type .le. max_packet_id
	first_frame_ok = .false.
	do while(.not. first_frame_ok)
	   ok = w_rt_set_rec(ch,major,minor)
	   if (ok.ne.1) goto 10
	   ! determine the mode: science or maneuver
	   mode = zext(rltm(ch).rec.data(4))
	   mode = mode .and. '00f0'x
	   science_mode = mode .eq. '00'x .or. mode .eq. '40'x
	   maneuver_mode = mode .eq. '10'x .or. mode .eq. '50'x
!	   if (.not. (science_mode .or. maneuver_mode)) science_mode = 1
	   if (.not. (science_mode .or. maneuver_mode)) goto 30
	   if (by_type) then
	      k = 1
	      if (maneuver_mode) k = 432
	      word = zext(rltm(ch).rec.data( p_addrs(k) ) )
	      my_type = zext_bits_to_longword(word,4,4)
	      first_frame_ok = my_type .eq. type
	      if (first_frame_ok .and. seek_1st) then
	         !first_frame_ok = word	! low bit set is 1st packet flag on vax
	         first_frame_ok = (word .and. 1) .ne. 0 ! sun fortran
	      end if
	      if (.not. first_frame_ok) then
	         if (direction .eq. forward) then
	            call wind_tm_increment_packet(major,minor)
	         else if (direction .eq. reverse) then
	            call wind_tm_decrement_packet(major,minor)
	         else
	            goto 20
	         end if
	      end if
	   else
	      first_frame_ok = .true.
	   end if
	end do

	! now gather successive frames and packet words
	k = 1
	if (maneuver_mode) k = 432
	j = 0
	any_bad_frame = .false.
	good_frame = .true.
	do i=1,n_frames_per_packet
	   if (good_frame) then
	      ! copy the data words from the TM record
	      do n=1,n_words_frame(i)
	         pkt(j) = rltm(ch).rec.data( p_addrs(k) )
	         k = k + 1
	         j = j + 1
	      end do
	   else
	      ! set all the bits in each word in the storage area derived
	      ! from this invalid/missing frame
	      do n=1,n_words_frame(i)
	         pkt(j) = 'ff'x
	         k = k + 1
	         j = j + 1
	      end do
	   end if
	   if (i .lt. n_frames_per_packet) then
	      call wind_tm_increment_mfmf(major,minor)
	      ok = w_rt_set_rec(ch,major,minor)
	      good_frame = ok .eq. 1
	      if (ok .eq. w_end_of_file) then
	         goto 40
	      else if (ok .ne. 1) then
	         ! return error code from the get_record routine
	         if (.not. any_bad_frame) w_rt_get_packet = ok
	         any_bad_frame = 1
	         if (.not. wind_suppress_messages(ch)) then
	             type 2, i-1, 'bad frame in packet, MF.mf=', major,minor
	         end if
	      end if
	   end if
	end do

	if (.not. any_bad_frame) w_rt_get_packet = 1

	call wind_plain_packet_sum(pkt,check)
	if (check .ne. 0) goto 50

	return
  1	format(1x,'W_RT_GET_PACKET: ',a, i8,'.',i3.3)
  2	format(1x,t1,'(',i2,')',1x,a,i8,'.',i3.3)
  3	format(1x,'W_RT_GET_PACKET: ',a, i)
  4	format(1x,'W_RT_GET_PACKET: ',a, z2.2)
 10	w_rt_get_packet = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get 1st frame of packet, MF.mf=', major,minor
	return
 20	w_rt_get_packet = 0
	if (wind_suppress_messages(ch)) return
	type 3, 'invalid "direction" argument: ', direction
	return
 30	w_rt_get_packet = 0
	if (wind_suppress_messages(ch)) return
	type 4, 'invalid SC mode from word #4: ', 
	1 rltm(ch).rec.data(4)
	return
 40	w_rt_get_packet = w_end_of_file
	if (wind_suppress_messages(ch)) return
	type 1, 'EOF detected at ', major,minor
	return
 50	w_rt_get_packet = w_bad_checksum
	if (wind_suppress_messages(ch)) return
	type 1, 'invalid checksum in packet at (major.minor): ', major, minor
	return
	end

!------------------------------------------------------------------------------
! For realtime TM access to the ring buffer global section this routine
! is not meaningful.  This routine is therefore implemented as an interface
! to the w_wnd_get_rec_by_time routine for the offline file currently
! being recorded.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_rec_by_time
	1		(ch,arg_major,arg_minor,user_time,by_ert,cpy_to_buf,wr)
	implicit	none
	include		'parm_def.for'
	include		'glbsec_def.for'
	include		'wind_realtime_def.for'
	include		'low_byte_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel id
	integer*4	arg_major	! major frame number
	record /low_byte/ arg_minor	! minor frame number
	character*(*)	user_time	! 23/24 character ERT or SCET
	logical*4	by_ert		! flag for search by ERT or SCET
	logical*4	cpy_to_buf	! 
	record /wind_record/ wr		! wind_tm_lib internal buffer

	integer*4	ok
	integer*4	ios
	integer*4	i,j
	integer*4	w_rt_get_mfmf
	integer*4	w_wnd_get_rec_by_time
	logical*4	wind_suppress_messages

	w_rt_get_rec_by_time = 0

	ok = w_rt_get_mfmf(ch,i,j, w_tk_earliest_mfmf)
	if (ok .ne. 1) goto 10

	if (rltm(ch).file(1:1) .le. ' ') goto 20

	! update the end-of-file frame numbers
	if (wnd(ch).last_major .lt. glsec.last_major_written) then
	   wnd(ch).last_major = glsec.last_major_written
	   wnd(ch).last_minor.b = glsec.last_minor_written_b
	end if	

	ok = w_wnd_get_rec_by_time(
	1    ch,arg_major,arg_minor,user_time,by_ert,cpy_to_buf,wr)

	w_rt_get_rec_by_time = ok

	return
  1	format(1x,'W_RT_GET_REC_BY_TIME: ', a)
 10	continue
	w_rt_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'error determining offline file position.'
	return
 20	continue
	w_rt_get_rec_by_time = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'no current offline recording file.'
	write(6,1,iostat=ios) 'realtime access by time not supported.'
	return
	end

!------------------------------------------------------------------------------
! Returns the SCET of the specified frame.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_scet_of_mfmf(ch,major,minor,scet)
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	record /vms_64bit_time/ scet
	integer*4	ok
	integer*4	w_rt_set_rec
	integer*4	wind_suppress_messages
	integer*4	ios
	integer*4	wnd_to_dbms			! a function
	integer*4	ymd,hms,s1000
	integer*4	w_rt_scet_dbms_of_mfmf
	logical*4	do_dbms_cnvrt

	do_dbms_cnvrt = .false.
	goto 1000

	!----------------------------------------------------------------------
	entry		w_rt_scet_dbms_of_mfmf(ch,major,minor,ymd,hms,s1000)
	do_dbms_cnvrt = .true.

 1000	continue
	w_rt_scet_of_mfmf = 0

	ok = w_rt_set_rec(ch,major,minor)
	if (ok .ne. 1) goto 10

	if (do_dbms_cnvrt) then
	   ok = wnd_to_dbms(rltm(ch).rec.scet, ymd,hms,s1000)
	   w_rt_scet_dbms_of_mfmf = ok
	   return
	else
	   scet = rltm(ch).rec.scet
	end if

	w_rt_scet_of_mfmf = 1

	return
  1	format(1x,'W_RT_SCET_OF_MFMF: ', a)
 10	continue
	w_rt_scet_of_mfmf = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get specified realtime[/.wnd] record.'
	return
	end

!------------------------------------------------------------------------------
! Determines the "frame rate" for a given minor frame by examining the mode
! bits in data word 4.
!------------------------------------------------------------------------------
	integer*4	function	w_rt_frame_rate
	1				(ch,major,minor,rate)
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's data struc
	integer*4	major			! major frame number
	integer*4	minor			! minor frame number
	real*4		rate			! bit rate
	integer*4	ok			! for checking return statuses
	integer*4	ios
	integer*4	mnr
	integer*4	mode
	record /wind_record/ xwr
	integer*4	w_rt_set_rec
	logical*4	wind_suppress_messages
	parameter	s1='0000'x		! science 1x, 92 sec
	parameter	m1='0010'x		! maneuver 1x, 92 sec
	parameter	e ='0020'x		! engineering
	parameter	c1='0030'x		! contingency 1x
	parameter	s2='0040'x		! science 2x, 46 sec
	parameter	m2='0050'x		! maneuver 2x, 46sec
	parameter	na='0060'x		! not applicable (I think)
	parameter	c2='0070'x		! contingency 2x
	real*4		hi,lo
	parameter	(hi=46.0/250.0)
	parameter	(lo=92.0/250.0)

	w_rt_frame_rate = 0
	rate = 0.0
	mnr = minor
	xwr = rltm(ch).rec

	if (mod(mnr,5) .ne. 0) then		! frames 0,5,10,15,...240,245
	   mnr = mnr - mod(mnr,5)		! contain the mode in word 4
	end if

	! establish the current record
	ok = w_rt_set_rec(ch, major, mnr)	! position to mf with mode
	mode = zext(rltm(ch).rec.data(4))	! get the mode
	rltm(ch).rec = xwr			! restore position
	if (ok .ne. 1) goto 10			! branch on error

	mode = mode .and. '00f0'x

	if (mode .eq. s2) then
	   rate = hi
	else if (mode .eq. s1) then
	   rate = lo
	else if (mode .eq. m1) then
	   rate = lo
	else if (mode .eq. m2) then
	   rate = hi
	else if (mode .eq. c1) then
	   rate = lo
	else if (mode .eq. c2) then
	   rate = hi
	else
	   goto 20
	end if

	w_rt_frame_rate = 1

	return
  1	format(1x,'W_RT_FRAME_RATE: ', a)
 10	continue
	w_rt_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get specified realtime[/.wnd] record.'
	return
 20	continue
	w_rt_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'invalid spacecraft mode.'
	return
	end

!------------------------------------------------------------------------------
! This routine stores extra information related to the event for later
! retrieval by the user if desired.  Stored information includes:
!    1) major&minor frame numbers of the 1st frame of 1st packet of the event
!    2) Earth Receive Time of the 1st frame of 1st packet of the event in
!       DBMS format
!    3) DPU major frame number from HK enclosing 1st packet of the event
!    4) SCET of the event
!    5) ScriptPlayer TEST and STEP values
!
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_xtra_event_info
	1				(ch,major,minor)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	integer*4	ok				! return status var
	integer*4	w_rt_set_rec			! a function
	integer*4	wind_get_event_scet		! a function
	integer*4	wind_get_record			! a function
	integer*4	wnd_to_dbms			! a function
	logical*4	wind_suppress_messages
	record	/wind_record/	my_record
	integer*4	xmajor, xminor
	integer*4	w_get_dpu_mjr_fr_num		! a function
	integer*4	w_rt_tm_speed			! a function
	integer*4	speed
	integer*4	return_code
	integer*4	o
	parameter	w_scet_err	= '010'x
	parameter	w_scet_frctn_err= '020'x
	parameter	w_dpu_majf_err	= '040'x
	parameter	w_ert_err	= '100'x
	parameter	w_ert_frctn_err	= '200'x

	w_rt_get_xtra_event_info = 0
	return_code = 1

	! get the major and minor frame numbers of the beginning of the event
	exi(ch).major = major
	exi(ch).minor = minor - 9

	! establish beginning of event as the current record
	ok = wind_get_record(ch,major,exi(ch).minor,my_record)
	if (ok .ne. 1) goto 10

	! convert the ert time
	ok = wnd_to_dbms(my_record.gathertime, 
	1	exi(ch).ert(1),
	1	exi(ch).ert(2),
	1	exi(ch).ert1000)
	if (ok .ne. 1) goto 20

	! get the script player's test and step number
	exi(ch).sp_test_number = rltm(ch).rec.sp_test_number
	exi(ch).sp_step_number = rltm(ch).rec.sp_step_number

	xmajor = major
	xminor = minor
	ok = w_get_dpu_mjr_fr_num(ch,xmajor,xminor,exi(ch).dpu_major_ert)
	if (ok .ne. 1) return_code = return_code .or. w_dpu_majf_err

	! get the bit_rate, put in event buffer
	ok = w_rt_tm_speed(ch,xmajor,zext(my_record.data(4)),speed)
	if (speed .eq. 1) then
	   exi(ch).bit_rate = 0
	else if (speed .eq. 2) then
	   exi(ch).bit_rate = 1
	else
	   exi(ch).bit_rate = 9
	end if

	! get the spacecraft event time
	xmajor = major
	xminor = minor
	ok = wind_get_event_scet(ch,xmajor,xminor)
	if (ok .ne. 1) then
	   return_code = return_code .or. w_scet_err
	   ! use the ert to grossly estimate the scet
	   exi(ch).scet(1) = exi(ch).ert(1)
	   exi(ch).scet(2) = exi(ch).ert(2)
	end if

	! restore user's stream position to end of first packet of event
	ok = w_rt_set_rec(ch,major,minor)
	if (ok .ne. 1) goto 80

	w_rt_get_xtra_event_info = return_code

	if ( (.not. wind_suppress_messages(ch)) .and. (.not.return_code)) then
	   if ((return_code .and. w_scet_err) .ne. 0) then
	      write(6,1,iostat=o) 'cannot get event SCET.'
	   end if
	   if ((return_code .and. w_ert_frctn_err) .ne. 0) then
	      write(6,1,iostat=o) 'cannot get fractional ERT.'
	   end if
	   if ((return_code .and. w_dpu_majf_err) .ne. 0) then
	      write(6,1,iostat=o) 'cannot get DPU MF.'
	   end if
	end if

	return
 1	format(1x,'W_RT_GET_XTRA_EVENT_INFO: ', a, :, a)

 10	w_rt_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 'cannot get record for ERT.'
	return

 20	w_rt_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 'cannot convert record ERT to DBMS format time.'
	return

 80	w_rt_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 'cannot get record for position restoration.'
	return
	end

!------------------------------------------------------------------------------
! Returns one to N (N=w2-w1+1) bytes of WND TM HK data to caller.
! This routine returns the original (more or less) 81 HK words, 25
! packet id words, 125 bytes of minor frame quality (always zero,
! but provided for cdhf compatability in form), 1 byte counting the
! number of minor frames with fill (provided for cdhf compatablility in
! form, though the number of errors encountered gathering the first 106
! hk words), 1 byte containing the number of minor frames with synch err
! (same value as number of minor frames with fill).
!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_hk(ch,mjr,w1,w2,buf)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_hk_addr_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	w1		! first/lowest hk word number
	integer*4	w2		! second/highest hk word number
	byte		buf(*)		! caller's buffer to receive word(s)
	integer*4	ok
	integer*4	w_rt_get_word
	integer*4	w_rt_get_mode
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	integer*4	mnr
	integer*4	mode
	integer*4	i,j,k
	record /low_byte/ lb
	integer*4	n_err

	w_rt_get_hk = 0

	j = 0
	n_err = 0
	do i=w1,w2
	   j = j + 1
	   buf(j) = 0
	   if (i .ge. w_first_core_hk_word .and. 		! reg. hk
	1      i .le. w_last_core_hk_word) then
	      ok = w_rt_get_word(ch, mjr,
	1	   hk_mf_addr(i),		! minor frame number
	1	   hk_word_addr(i),		! word number in minor frame
	1	   buf(j))			! returned word
	      if (ok .ne. 1) n_err = n_err + 1
	   else if (i .ge. w_first_pktid_hk_word .and. 		! pkt id's
	1           i .le. w_last_pktid_hk_word) then
	      mnr = (i - w_first_pktid_hk_word) * 10
	      ok = w_rt_get_mode(ch,mjr,mnr,mode)
	      if (ok .eq. 1) then
	         if (mode .eq. science_1x .or. 
	1            mode .eq. science_2x .or.
	1            mode .eq. contingency_1x .or. 
	1            mode .eq. contingency_2x) then
	             k = 24
	         else if (mode .eq. maneuver_1x .or.
	1              mode .eq. maneuver_2x) then
	             k = 32
	         else
	             goto 35
	         end if
	         ok = w_rt_get_word(ch, mjr, mnr, k, buf(j))
	         if (ok .ne. 1) n_err = n_err + 1
	      else
	         n_err = n_err + 1
	      end if
	   else if (i .eq. w_sc_mode_hk_word) then
	      ok = w_rt_get_mode(ch,mjr,mnr,lb.i4val)
	      if (ok .ne. 1) write(6,1,iostat=ios) 'cannot get HK sc mode word.'
	      buf(j) = lb.b
	   else if (i .ge. w_first_mfq_hk_word .and. 
	1           i .le. w_last_mfq_hk_word) then
	      buf(j) = 0
	   else if (i .eq. w_n_mf_w_fill_hk_word) then
	      lb.i4val = n_err
	      buf(j) = lb.b
	   else if (i .eq. w_n_mf_w_synch_err_hk_word) then
	      lb.i4val = n_err
	      buf(j) = lb.b
	   else
	      goto 13
	   end if
	end do

	if (n_err .ne. 0) then
	   write(6,4,iostat=ios) n_err, ' words with errors.'
	   if (n_err .ge. 106) return ! return err if all errors
	   w_rt_get_hk = w_partial_hk_event
	   return
	end if

	w_rt_get_hk = 1

	return
   1	format(1x,'W_RT_GET_HK: ', a, :, i)
   4	format(1x,'W_RT_GET_HK: ', i3, a)
! 10	continue
!	w_rt_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot goto stream position'
!	return
 13	w_rt_get_hk = w_bad_argument_value
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad house keeping index.'
	return
! 20	w_rt_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot get specified core housekeeping word.'
!	return
! 30	w_rt_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 
!	1 'cannot get sc mode for "packet id" housekeeping word.'
!	return
 35	w_rt_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 
	1 'invalid sc mode for "packet id" housekeeping word.', mode
	return
! 38	w_rt_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 
!	1 'cannot get specified "packet id" housekeeping word.'
!	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_rt_get_dpu_mf(ch,major,minor,dpu)
! Extracts the dpu major frame number from the stream at a given position.
! Attempts to estimate the value at EOF and BOF.  Restores the caller
! to the stream position at entry.
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor
	integer*4	dpu				! dpu major frame #
	integer*4	ok				! return status var
	integer*4	w_rt_get_rec			! a function
	record	/low_byte/	lw
	record	/wind_record/	my_record
	integer*4	omajor,ominor
	integer*4	error_count
	integer*4	wind_get_message_state		! a function
	integer*4	wind_set_message_state		! a function

	logical*4	state
	logical*4	subtract
	logical*4	add

	lw.i4val = 0
	add      = .false.
	subtract = .false.
	omajor   = rltm(ch).rec.major_frame
	ominor   = zext(rltm(ch).rec.minor_frame)
	error_count = 0
	ok = wind_get_message_state(ch, state)
	ok = wind_set_message_state(ch, .true.)

	w_rt_get_dpu_mf = 0

 1000	continue
	if (error_count .gt. 1) goto 2000

	ok = w_rt_get_rec(ch,omajor,44,my_record)
	if (ok .eq. w_beginning_of_file) then
	   subtract = .true.
	   omajor   = omajor + 1
	   error_count = error_count + 1
	   goto 1000
	else if (ok .eq. w_end_of_file) then
	   add      = .true.
	   omajor   = omajor - 1
	   error_count = error_count + 1
	   goto 1000
	else if (ok .ne. 1) then
	   goto 114
	end if
	lw.b =  my_record.data(18)
	lw.b1 = my_record.data(17)

	ok = w_rt_get_rec(ch,omajor,114,my_record)
	if (ok .eq. w_end_of_file) then
	   add      = .true.
	   omajor   = omajor - 1
	   error_count = error_count + 1
	   goto 1000
	else if (ok .ne. 1) then
	   goto 116
	end if
	lw.b2 = my_record.data(17)

 2000	continue
	if (error_count .gt. 1) then
	   w_rt_get_dpu_mf = 0
	else if (error_count .eq. 1) then
	   if (subtract) then
	      lw.i4val = lw.i4val - 1
	   else
	      lw.i4val = lw.i4val + 1
	   end if
	   dpu = lw.i4val
	   w_rt_get_dpu_mf = 1
	else
	   dpu = lw.i4val
	   w_rt_get_dpu_mf = 1
	end if

	ok = w_rt_get_rec(ch,omajor,ominor,my_record)
	if (ok .ne. 1) goto 120

	ok = wind_set_message_state(ch, state)

	return
  1	format(1x,'W_RT_GET_DPU_MF: ',a,:,i,'.',i3.3)
 114	w_rt_get_dpu_mf = ok
	ok = wind_set_message_state(ch, state)
	if (state) return
	type 1, 'cannot get wind record for HK DPU at', omajor,44
	return

! 115	w_rt_get_dpu_mf = ok
!	ok = wind_set_message_state(ch, state)
!	if (state) return
!	type 1, 'cannot get wind record for HK DPU at', omajor,119
!	return

 116	w_rt_get_dpu_mf = ok
	ok = wind_set_message_state(ch, state)
	if (state) return
	type 1, 'cannot get wind record for HK DPU at', omajor,114
	return

 120	w_rt_get_dpu_mf = ok
	ok = wind_set_message_state(ch, state)
	if (state) return
	type 1, 'cannot restore stream position to ', major, minor
	return
	end
