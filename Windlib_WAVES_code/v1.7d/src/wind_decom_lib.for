! wind_decom_lib.for - WIND/WAVES TM lib routines for accessing decom files
!
! For VMS FORTRAN 6.0 and later compile this module with: /warn=(nounused)
! to avoid informational compiler messages regarding unused variables.
!
!
!

!------------------------------------------------------------------------------
! Initializes internal data structures for using wind/waves decom
! format disk files.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_setup_stream(ch,f)
	implicit	none
	integer*4	ch
	character*(*)	f
	include		'wind_tm_user_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ok
	integer*4	w_decom_set_file
	integer*4	w_decom_get_rec_xc
	integer*4	w_decom_fill_xrbuf
	integer*4	i
	integer*4	max_tries_for_1st_valid_rec
	parameter	(max_tries_for_1st_valid_rec=64)
	integer*4	o

	w_decom_setup_stream = 0

	! open the file
	ok = w_decom_set_file(ch, f, xr_idx)
	if (ok .ne. 1) goto 10

	ok = w_decom_fill_xrbuf(ch)
	if (ok .ne. 1) goto 20

	user(ch).major_frame = dc(ch).first_major
	user(ch).first_major = dc(ch).first_major
	user(ch).last_major  = dc(ch).last_major
	user(ch).minor_frame.i4val = 0
	user(ch).first_minor.i4val = 0
	user(ch).last_minor.i4val  = 249

	! assume all decom words are valid 
	! [.wnd files are supplied with a valid word list from the 
	! GSE software]
	user(ch).all_tm_word_priv = .true.

!	! establish the central wind_lib reference record
!	ok = w_decom_get_rec_xc(
!	1	ch, 
!	1	user(ch).first_major, 
!	1	user(ch).first_minor,
!	1	user(ch).wind_record)
!	if (ok .ne. 1) goto 30

	call w_decom_set_functions(ch)

	w_decom_setup_stream= ok

	return
  1	format(1x,'W_DECOM_SETUP_STREAM: ', a)
 10	continue
	w_decom_setup_stream = 0
	write(6,1,iostat=o) 'cannot acquire stream.'
	return
 20	continue
	w_decom_setup_stream = 0
	write(6,1,iostat=o) 'cannot fill cross reference buffer.'
	return
! 30	continue
!	w_decom_setup_stream = 0
!	write(6,1,iostat=o) 'cannot get initial xc record.'
!	return
	end

!------------------------------------------------------------------------------
! A dummy routine
!------------------------------------------------------------------------------
	integer*4	function	w_decom_dummy()
	type *, 'Inside w_decom_dummy routine.'
	w_decom_dummy = 0
	return
	end

!------------------------------------------------------------------------------
! Stores addresses of wind/waves decom-specific functions for later invocation
! by core wind_lib routines.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_set_functions(ch)
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch
	external	w_decom_get_word
	external	w_decom_get_rec_xc
	external	w_decom_get_packet
	external	w_decom_get_rec_by_time
	external	w_decom_scet_of_mfmf
	external	w_decom_scet_dbms_of_mfmf
	external	w_decom_frame_rate
	external	w_decom_get_xtra_event_info
	external	w_decom_close_stream
	external	w_decom_get_mode
	external	w_decom_get_hk
	external	w_decom_get_dpu_mf
	external	w_decom_dummy

	! these values must be non-zero: use dummy functions if necessary
	user(ch).f_get_word            = %loc(w_decom_get_word)
	user(ch).f_get_intrnl_rec      = %loc(w_decom_get_rec_xc)
	user(ch).f_get_plain_packet    = %loc(w_decom_get_packet)
	user(ch).f_goto_time           = %loc(w_decom_get_rec_by_time)
	user(ch).f_get_mf_scet         = %loc(w_decom_scet_of_mfmf)
	user(ch).f_get_mf_scet_dbms    = %loc(w_decom_scet_dbms_of_mfmf)
	user(ch).f_get_frame_rate      = %loc(w_decom_frame_rate)
	user(ch).f_get_xtra_event_info = %loc(w_decom_get_xtra_event_info)
	user(ch).f_close_ch            = %loc(w_decom_close_stream)
	user(ch).f_get_tm_mode         = %loc(w_decom_get_mode)
	user(ch).f_get_hk              = %loc(w_decom_get_hk)
	user(ch).f_get_dpu_mf          = %loc(w_decom_get_dpu_mf)

	w_decom_set_functions = 1
	return
	end

!------------------------------------------------------------------------------
! This routine retrieves a wind_lib format internal record from the ch's
! decom xref file based on time.  For now the algorithm is very simple.  It
! assumes no time gaps in the file.  It positions the caller at the beginning
! of the major frame in which the caller's time occurs.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_rec_by_time
	1			(ch,mjr,mnr,user_time,by_ert,cpy_to_buf,wr)
	implicit	none
	include		'wind_os_def.for'
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_record_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	character*(*)	user_time	! 23/24 character ERT or SCET
	logical*4	by_ert		! flag for search by ERT or SCET
	logical*4	cpy_to_buf	! 
	record /wind_record/ wr		! wind_tm_lib internal buffer

	integer*4	ok
	integer*4	ios
	integer*4	j,k
	integer*4	w_decom_synch_files		! a function
	integer*4	w_decom_read_rec 		! a function
	integer*4	w_decom_get_rec_xc		! a function
	logical*4	wind_suppress_messages		! a function
	integer*4	w_decom_binary_search		! a function
	integer*4	c24_to_ur8			! a function
	real*8		t1, ut, t3
	real*8		dt	! delta time (difference)
	real*8		nt	! 
	integer*4	mjr1
	integer*4	mjr2
	integer*4	recno

	w_decom_get_rec_by_time = 0

	ok = c24_to_ur8(user_time, ut)
	if (ok .ne. 1) goto 10

	if (ut .lt. dc(ch).dfi(xr_idx).scet_initial) then
	   ! prior to bof?
	   recno = dc(ch).dfi(xr_idx).first_rec
	   ok = w_decom_read_rec(ch, xr_idx, recno)
	   if (ok .ne. 1) goto 20
	   mjr = dc(ch).first_major
	else if (ut .gt. dc(ch).dfi(xr_idx).scet_final) then
	   ! after eof?
	   recno = dc(ch).dfi(xr_idx).last_rec
	   ok = w_decom_read_rec(ch, xr_idx, recno)
	   if (ok .ne. 1) goto 20
	   mjr = dc(ch).last_major
	else
	   ! search master file for closest record
	   t1 = dc(ch).dfi(xr_idx).scet_initial
	   t3 = dc(ch).dfi(xr_idx).scet_final
	   mjr1 = dc(ch).first_major
	   mjr2 = dc(ch).last_major
	   ok = w_decom_binary_search(ch,t1,ut,t3, mjr1, mjr2, dt, nt, mjr)
	   if (ok .ne. 1) goto 30
	end if

	mnr = 0

	ok = w_decom_synch_files(ch, mjr, mnr)
	if (ok .ne. 1) goto 40

	! fill the user's buffer
	ok = w_decom_get_rec_xc(
	1	ch, 
	1	mjr,
	1	mnr,
	1	wr)
	if (ok .ne. 1) goto 60

	w_decom_get_rec_by_time = 1

	return
   1	format(1x,'W_DECOM_GET_REC_BY_TIME: ', a, :, i5)
  10	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert caller''s time to UR8.'
	return
  20	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get rec# ', recno
	return
  30	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'binary search for closest time failed.'
	write(6,1,iostat=ios) 'could not position stream to ', user_time
	return
  40	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot synchronize files'
	return
  60	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot copy record to caller''s buffer.'
	return
	end

!------------------------------------------------------------------------------
! This routine performs a binary search of the CDHF file associated with the
! supplied channel based on SCET.  Time t1 should be earlier than time t2.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_binary_search
	1				(ch,t1,ut,t2,p1,p2,dt,zt,mjr)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_return_code_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	real*8		t1,t2,ut	! start/end times, user time
	integer*4	p1, p2		! start/end MF major frame numbers
	real*8		dt		! final delta time
	real*8		zt		! time of closest data record
	integer*4	mjr

	real*8		adt
	integer*4	ok
	integer*4	ios
	integer*4	j
	integer*4	n_reads
!	integer*4	n_calls
!	integer*4	n_cumulative_reads
	integer*4	tm_speed
	integer*4	w_decom_tm_speed			! a function
	integer*4	w_decom_read_rec		! a function
	integer*4	atc_to_ur8			! a function
	logical*4	wind_suppress_messages
	structure /binary_search_index/
	   integer*4	n
	   real*8	t
	end structure
	record /binary_search_index/ lo, hi, mid
	logical*4	done

	integer*4	max_search_depth
	parameter	(max_search_depth=17)

	integer*4	xhi,xlo
	integer*4	midpoint

	midpoint(xlo,xhi) = max(xlo, ((xhi+xlo)/2) )

	w_decom_binary_search = 0

	lo.n = p1
	lo.t = t1

	hi.n = p2
	hi.t = t2

	n_reads = 0
!	n_calls = n_calls + 1
	done = .false.

	do while (.not. done)
	   ! goto the midpoint of the interval
	   mid.n = midpoint(lo.n,hi.n)
	   mid.t = dc(ch).xrbuf(mid.n).scet
!	   ok = w_decom_read_rec(ch,xr_idx,mid.n)
!	   if (ok .ne. 1) goto 10
	   n_reads = n_reads + 1
	   if (n_reads .ge. max_search_depth) goto 20

!	   ! get the time of the MF at the midpoint
!	   ok = atc_to_ur8(dc(ch).dfi(xr_idx).atc_year,
!	1                 dc(ch).dfi(xr_idx).atc_day,
!	1                 dc(ch).dfi(xr_idx).atc_msec, mid.t)
!	   if (ok .ne. 1) goto 30

	   ! get the delta time between midpoint and the user's time
	   dt = mid.t - ut
	   adt = abs(dt)

	   ! are we within one major frame?...if so then success!
	   ok = w_decom_tm_speed(ch,mid.n,tm_speed)
	   if (ok .ne. 1) goto 50
	   j = 2
	   if (tm_speed .eq. 2) then
	      if (adt .lt. ur8_46s) j = 0
	   else
	      if (adt .lt. ur8_92s) j = 0
	   end if
	   !
	   if (j .lt. 1) then
	      done = .true.			! delta time minimized=success
	   else if (mid.n .eq. lo.n) then
	      done = .true.			! time gap? closest convergence
	   else
	      ! setup for next search iteration
	      if (mid.t .gt. ut) then
	         ! current midpoint is greater than target time
	         hi = mid
	      else if (mid.t .lt. ut) then
	         ! current midpoint is less than target time
	         lo = mid
	      else
	         goto 80			! should never get here
	      end if
	      if (lo.n .ge. hi.n) goto 90	! time gap? reverse time?
	   end if
	end do

	zt = mid.t
	mjr = mid.n

!	n_cumulative_reads = n_cumulative_reads + n_reads

!	type *, 'Binary Search used ', n_reads, ' reads.'

	w_decom_binary_search = 1

	return
   1	format(1x,'W_DECOM_BINARY_SEARCH: ', a, :, i)
!  10	continue
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot set current record to ', mid.n
!	return
  20	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'search depth exceeded at ', n_reads
	return
  30	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert ATC to UR8 at', mid.n
	return
  50	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get TM speed at', mid.n
	return
  80	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot set search direction.'
	return
  90	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'indexes messed up.'
	return
	end

!------------------------------------------------------------------------------
! Gets peripheral information associated with event gathering.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_xtra_event_info
	1				(ch,mjr,mnr)
	implicit	none
	include 	'wind_hk_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	integer*4	ok
	integer*4	ios
	integer*4	xmajor
	integer*4	xminor
	integer*4	return_code
	integer*4	ur8_to_dbms
	integer*4	wind_get_event_scet	! a function
	logical*4	wind_suppress_messages	! a function
	parameter	w_scet_err	= '010'x
	parameter	w_scet_frctn_err= '020'x
	parameter	w_dpu_majf_err	= '040'x
	parameter	w_ert_err	= '100'x
	parameter	w_ert_frctn_err	= '200'x

	w_decom_get_xtra_event_info = 0
	return_code = 1
	if (mjr .gt. dc(ch).last_major .or. mjr .lt. dc(ch).first_major) goto 10
!	if (mjr .ne. dc(ch).dfi(xr_idx).recno) goto 10

	! get the major and minor frame numbers of the beginning of the event
	eb(ch).major = mjr
	eb(ch).minor = mnr - 9

	! get the earth receive time
!xxxxxxxx just for time testing, jk
!	eb(ch).ert(1) = 1995
!	eb(ch).ert(2) = 121212
!	eb(ch).ert1000 = 444
	ok = ur8_to_dbms(
	1	dc(ch).xrbuf(mjr).scet,
	1	eb(ch).ert(1),
	1	eb(ch).ert(2),
	1	eb(ch).ert1000)
	if (ok .ne. 1) goto 20

	eb(ch).dpu_major_ert = dc(ch).xrbuf(mjr).dpu_major_frame

	! get the spacecraft event time
!xxxxxx can make this inline, since the decom case is designed to be
! simpler....use xrbuf and items 
! ...well, the TNR uses only an 8-bit dpu major frame number, so the
! bit prescision logic in wind_get_event_scet should be used...
	xmajor = mjr
	xminor = mnr
	ok = wind_get_event_scet(ch,xmajor,xminor)
	if (ok .ne. 1) then
	   return_code = return_code .or. w_scet_err
	   ! use the ert to grossly estimate the scet
!xxxxxxxx just for time testing, jk
	   eb(ch).scet(1) = eb(ch).ert(1)
	   eb(ch).scet(2) = eb(ch).ert(2)
	end if

	w_decom_get_xtra_event_info = return_code

	if ( (.not. wind_suppress_messages(ch)) .and. (.not.return_code)) then
	   if ((return_code .and. w_scet_err) .ne. 0) then
	      type 1, 'cannot get event SCET.'
	   end if
	   if ((return_code .and. w_ert_frctn_err) .ne. 0) then
	      type 1, 'cannot get fractional ERT.'
	   end if
	end if

	w_decom_get_xtra_event_info = 1
	return
   1	format(1x,'W_DECOM_GET_XTRA_EVENT_INFO: ', a)
 10	continue
	w_decom_get_xtra_event_info = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cross reference record not synched with stream'
	return
 20	continue
	w_decom_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get ert of 1st rec of event'
	return
	end

!------------------------------------------------------------------------------
! Determines the SpaceCraft Event Time for a particular MF.mf position in
! file.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_scet_of_mfmf(ch,mjr,mnr,t)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	record /vms_64bit_time/	t		! wind lib internal time format
	integer*4	ok
	integer*4	ios
	real*8		add
	integer*4	ur8_to_wnd		! a function
	integer*4	ur8_to_dbms		! a function
	logical*4	wind_suppress_messages	! a function
	logical*4	is_fast_tm
	logical*4	is_slow_tm
	integer*4	a
	real*8		ur8
	integer*4	ymd,hms,s1000
	integer*4	w_decom_scet_dbms_of_mfmf
	logical*4	do_dbms_cnvrt

	is_fast_tm(a) =a .eq. science_2x .or.
	1              a .eq. maneuver_2x .or.
	1	       a .eq. contingency_2x
	is_slow_tm(a) =a .eq. maneuver_1x .or.
	1	       a .eq. science_1x .or.
	1	       a .eq. contingency_1x 

	do_dbms_cnvrt = .false.
	goto 1000

	!----------------------------------------------------------------------
	entry		w_decom_scet_dbms_of_mfmf(ch,mjr,mnr,ymd,hms,s1000)
	do_dbms_cnvrt = .true.

 1000	continue
	t.i4(1) = 0
	t.i4(2) = 0

	if (mjr .lt. dc(ch).first_major) then
	   w_decom_scet_of_mfmf = w_beginning_of_file
	   return
	else if (mjr .gt. dc(ch).last_major) then
	   w_decom_scet_of_mfmf = w_end_of_file
	   return
	else
	   w_decom_scet_of_mfmf = 0
	end if

	a = zext(dc(ch).xrbuf(mjr).b1) .and. sc_mode_mask
	if (is_fast_tm(a)) then
	   add = mnr * ur8_mfdtf
	else if (is_slow_tm(a)) then
	   add = mnr * ur8_mfdts
	else
	   goto 20
	end if

	ur8 = dc(ch).xrbuf(mjr).scet + add

	if (do_dbms_cnvrt) then
	   ok = ur8_to_dbms(ur8, ymd,hms,s1000)
	   w_decom_scet_dbms_of_mfmf = ok
	   return
	else
	   ok = ur8_to_wnd(ur8,t)
	   if (ok .ne. 1) goto 30
	end if

	w_decom_scet_of_mfmf = 1
	return
   1	format(1x,'W_DECOM_SCET_OF_MFMF: ', a)
   2	format(1x,'W_DECOM_SCET_OF_MFMF: ', a, 1x, z8.8)
   3	format(1x,'W_DECOM_SCET_OF_MFMF: ', a, 1x, g)
 10	continue
	w_decom_scet_of_mfmf = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 20	continue
	w_decom_scet_of_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,2,iostat=ios) 'TM mode not science, maneuver, or contin.', a
	return
 30	continue
	w_decom_scet_of_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert UR8 time to WND time.'
	write(6,3,iostat=ios) ' UR8 time is: ', ur8
	return
	end

!------------------------------------------------------------------------------
! Returns one wind_record of data (essentially a 256-byte minor frame of data
! with some other stuff appended) to the caller.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_rec_xc(ch,mjr,mnr,wr)
	implicit	none
	include		'low_byte_def.for'
	include		'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_record_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	record /wind_record/	wr		! main wind lib internal buffer
	integer*4	ok
	integer*4	w_decom_scet_of_mfmf
	logical*4	wind_suppress_messages	! a function
	record /low_byte/ lbv
	integer*4	ios
	integer*4	i,j

	w_decom_get_rec_xc = 0

	! zero fill the caller's buffer
	do i=0,255
	   wr.data(i) = 0
	end do
	do i=1,8
	   wr.gathertime.b(i) = 0
	   wr.scet.b(i) = 0
	end do
	wr.major_frame = 0
	wr.quality = 0
	wr.sp_test_number = 0
	wr.sp_step_number = 0

!	! establish the stream position
!	ok = w_decom_set_current_rec(ch,mjr)
!	if (ok .ne. 1) goto 10

	! set the minor frame number
	lbv.i4val = mnr
	wr.minor_frame = lbv.b

	! set the major frame number
	wr.major_frame = mjr

	! gather the tm words
	! ...leave all words as zero for now, the decom stream is a
	! a packet/event oriented stream

	! set the quality bits
	wr.quality = 1

	! get the scet and ert times
	ok = w_decom_scet_of_mfmf(ch,mjr,mnr,wr.scet.b)
	if (ok .ne. 1) goto 30
	wr.gathertime = wr.scet

	w_decom_get_rec_xc = 1
	return
   1	format(1x,'W_DECOM_GET_REC_XC: ', a, :, 1x, i)
 10	continue
	w_decom_get_rec_xc = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 30	continue
	w_decom_get_rec_xc = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get SCET for specified position.'
	return
	end

!------------------------------------------------------------------------------
! Returns one byte of CDHF TM data to caller.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_word(ch,mjr,mnr,wrd,buf)
	implicit	none
	include		'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	wrd		! addres of desired word in mnr
	byte		buf		! caller's buffer to receive word
	integer*4	ok
	logical*4	wind_suppress_messages	! a function
	integer*4	ios

	! decom stream does not return "words" at this writing, but
	! does return the instrument version number words (which is
	! the bare minimum required to keep the event building
	! routine happy) - jk

	ok = 0
	if (wrd .eq. 18) then
	   ok = 1
	   if (mjr .lt. dc(ch).first_major .or. mjr .gt. dc(ch).last_major)
	1     goto 10
	   if (mnr .eq. 14) then
	      ! dpu version number
	      buf = dc(ch).xrbuf(mjr).dpu_version
	   else if (mnr .eq. 209) then
	      ! fft version number
	      buf = dc(ch).xrbuf(mjr).fft_version
	   else if (mnr .eq. 144) then
	      ! tds version number
	      buf = dc(ch).xrbuf(mjr).tds_version
	   else
	      ok = 0
	   end if
	end if

	if (ok .ne. 1) then
	   buf = 'ff'x
	   type 1, '...should not call this routine...'
	end if

	w_decom_get_word = ok

	return
   1	format(1x,'W_DECOM_GET_WORD: ', a, :, i8,'.',i3.3)
 10	continue
	w_decom_get_word = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position', mjr, mnr
	return
! 20	continue
!	w_decom_get_word = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot get word'
!	return
	end

!------------------------------------------------------------------------------
! Returns one to N (N=w2-w1+1) bytes of CDHF TM HK data to caller.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_hk(ch,mjr,w1,w2,buf)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	w1		! first/lowest hk word number
	integer*4	w2		! second/highest hk word number
	byte		buf(*)		! caller's buffer to receive word(s)
	integer*4	ok
	integer*4	w_decom_set_file	! a function
	integer*4	w_decom_read_rec	! a function
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	integer*4	i,j
	integer*4	recno

	w_decom_get_hk = 0

	if (dc(ch).dfi(hk_idx).lun .eq. 0) then
	   ok = w_decom_set_file(ch,dc(ch).dfi(xr_idx).file, hk_idx)
	   if (ok .ne. 1) goto 10
	end if

	recno = dc(ch).dfi(hk_idx).first_rec - 1 + mjr
	ok = w_decom_read_rec(ch, hk_idx, recno)
	if (ok .ne. 1) goto 20

	if (w1 .gt. w2) goto 13
	if (w1 .lt. 0) goto 13
	if (w1 .gt. w_last_hk_idx) goto 13
	if (w2 .lt. 0) goto 13
	if (w2 .gt. w_last_hk_idx) goto 13

	j = 0
	do i=w1,w2
	   j = j + 1
	   buf(j) = dc(ch).dfi(hk_idx).ddr_hk.word(i)
	end do

	w_decom_get_hk = 1

	return
   1	format(1x,'W_DECOM_GET_HK: ', a, :, i4, 1x, i4)
 10	w_decom_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot open housekeeping file.'
	return
 13	w_decom_get_hk = w_bad_argument_value
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad house keeping indexes:', w1, w2
	return
 20	continue
	w_decom_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
	end

!------------------------------------------------------------------------------
! Returns the current dpu major frame number from the HK
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_dpu_mf(ch,mjr,mnr,dpu)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	dpu		! caller's buffer for dpu major frame#
	integer*4	ios
	logical*4	wind_suppress_messages	! a function

	w_decom_get_dpu_mf = 0

	if (mjr .lt. dc(ch).first_major .or.
	1   mjr .gt. dc(ch).last_major) goto 10

	dpu = dc(ch).xrbuf(mjr).dpu_major_frame

	w_decom_get_dpu_mf = 1
	return
   1	format(1x,'W_DECOM_GET_DPU_MF: ', a, i)
 10	continue
	w_decom_get_dpu_mf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad major frame index: ', mjr
	return
	end

!------------------------------------------------------------------------------
! Returns the space craft telemetry mode to the caller
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_mode(ch,mjr,mnr,mode)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	mode		! caller's buffer to receive mode
	logical*4	wind_suppress_messages	! a function
	integer*4	ios

	if (mjr .lt. dc(ch).first_major .or.
	1   mjr .gt. dc(ch).last_major) goto 10

	mode = zext(dc(ch).xrbuf(mjr).b1) .and. sc_mode_mask

	w_decom_get_mode = 1

	return
   1	format(1x,'W_DECOM_GET_MODE: ', a)
 10	continue
	w_decom_get_mode = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad major frame index', mjr
	return
	end

!------------------------------------------------------------------------------
! Returns the time between succesive minor frames in real*4 seconds for
! entry w_decom_frame_rate.
! Entry w_decom_tm_speed returns 1 for low bit rate (e.g.: S1x) or 2 for
! high bit rate (e.g.: S2x).
!------------------------------------------------------------------------------
	integer*4	function	w_decom_frame_rate(ch,mjr,mnr,rate)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	real*4		rate
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	real*4		hi,lo
	parameter	(hi=46.0/250.0)
	parameter	(lo=92.0/250.0)
	integer*4	w_decom_tm_speed		! an entry point
	integer*4	speed
	integer*4	mode
	logical*4	caller_wants_rate

	caller_wants_rate = .true.
	goto 1000

	!----------------------------------------------------------------------
	entry	w_decom_tm_speed(ch,mjr,speed)
	caller_wants_rate = .false.

 1000	continue
	w_decom_frame_rate = 0

	if (mjr .lt. dc(ch).first_major .or.
	1   mjr .gt. dc(ch).last_major) goto 10

	mode = zext(dc(ch).xrbuf(mjr).b1) .and. sc_mode_mask

	if (mode .eq. science_2x .or.
	1   mode .eq. maneuver_2x .or.
	1   mode .eq. contingency_2x) then
	   if (caller_wants_rate) then
	      rate = hi
	   else
	      speed = 2
	   end if
	else if (
	1   mode .eq. science_1x .or.
	1   mode .eq. maneuver_1x .or. 
	1   mode .eq. contingency_1x) then
	   if (caller_wants_rate) then
	      rate = lo
	   else
	      speed = 1
	   end if
	else
	   goto 20
	end if

	w_decom_frame_rate = 1

	return
   1	format(1x,'W_DECOM_FRAME_RATE: ', a)
 10	continue
	w_decom_frame_rate = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad major frame index', mjr
	return
 20	continue
	w_decom_frame_rate = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'Troublesome SC mode of ', mode
	return
	end

!------------------------------------------------------------------------------
! Gets one packet of data for the caller.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_packet
	1				(ch,mjr,mnr,type,dir,seek_1st,buf)
	implicit	none
	include		'low_byte_def.for'
	include		'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	type		! packet type to seek
	integer*4	dir		! stream direction, forward or backward
	logical*4	seek_1st	! seek a first packet? y/n
	byte		buf(0:*)	! caller's buffer
	integer*4	ok
	integer*4	w_decom_set_file	! a function
	integer*4	w_decom_synch_rec	! a function
	integer*4	w_decom_read_rec	! a function
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	record /low_byte/ id
	logical*4	by_type
	logical*4	found
	integer*4	check
	integer*4	xref
	integer*4	recno
	integer*4	i

	w_decom_get_packet = 0

!	type *, '...inside w_decom_get_packet...', mjr, mnr

	by_type = type .ge. min_packet_id .and. type .le. max_packet_id
	if (by_type) then
	   if (dc(ch).dfi(type).lun .eq. 0) then
	      ok = w_decom_set_file(ch,dc(ch).dfi(xr_idx).file, type)
	      if (ok .ne. 1) goto 10
	   end if
	   xref = dc(ch).dfi(xr_idx).first_rec + mjr - 1
	   ok = w_decom_synch_rec(ch, type, xref, mnr)
	   if (ok .ne. 1) goto 20
	else
	   type *, 'xxxx Cannot get generic packets via Decom Stream.'
	   return
	end if
	if (dir .ne. forward) then
	   type *, 'xxxx Cannot go backwards via Decom Stream.'
	   return
	end if

	! get the current packet or move sequentially through file until 
	! a first packet is found or an error condition
	! (such as EOF/BOF) occurs.
	recno = dc(ch).dfi(type).recno
	found = .false.
	do while (.not. found)
	   id.b = dc(ch).dfi(type).ddr_ins.pkt(0)
	   mjr = dc(ch).dfi(type).ddr_ins.xref_ptr -
	1	dc(ch).dfi(xr_idx).first_rec + 1
	   if (seek_1st) then
	      found = (id.b .and. 1) .ne. 0	! low bit set = 1st pkt flg
	   else
	      found = .true.
	   end if
	   if (found) then
	      do i=0,size_of_packet-1
	         buf(i) = dc(ch).dfi(type).ddr_ins.pkt(i)
	      end do
	   else
	      recno = recno + 1
	      ok = w_decom_read_rec(ch, type, recno)
	      if (ok .ne. 1) goto 30
	   end if
	end do

	call wind_plain_packet_sum(buf,check)
	if (check .ne. 0) goto 50

	w_decom_get_packet = 1

	return
   1	format(1x,'W_DECOM_GET_PACKET: ', a)
   2	format(1x,'W_DECOM_GET_PACKET: ',a, i8,'.',i3.3)
   3	format(1x,'W_DECOM_GET_PACKET: ', a, i5)
 10	continue
	w_decom_get_packet = ok
	if (wind_suppress_messages(ch)) return
	write(6,3,iostat=ios) 'cannot open file for packet type', type
	return
 20	continue
	w_decom_get_packet = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot synch file to current MF.mf'
	return
 30	continue
	w_decom_get_packet = ok
	if (wind_suppress_messages(ch)) return
	write(6,3,iostat=ios) 'cannot get packet rec, ok=', ok
	return
 50	w_decom_get_packet = w_bad_checksum
	if (wind_suppress_messages(ch)) return
	write(6,2,iostat=ios)
	1  'invalid checksum in packet at (major.minor): ', mjr, mnr
	return
	end

!------------------------------------------------------------------------------
! Do wind/saves decom-stream specific channel shut down stuff like closing
! the open file, zeroing indexes, and so on.
!------------------------------------------------------------------------------
	integer*4	function	w_decom_close_stream(ch)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	integer*4	i,j
	integer*4	o

	do i=0,xr_idx
	   if (dc(ch).dfi(i).lun .ne. 0) then
	      close(dc(ch).dfi(i).lun,iostat=o)
	      call lib$free_lun(dc(ch).dfi(i).lun)
	   end if
           dc(ch).dfi(i).scet_initial     = 0.0
           dc(ch).dfi(i).scet_final       = 0.0
           dc(ch).dfi(i).file             = ' '
           dc(ch).dfi(i).recno            = 0 
           dc(ch).dfi(i).first_rec        = 0
           dc(ch).dfi(i).last_rec         = 0
           dc(ch).dfi(i).byte_lrecl       = 0
           dc(ch).dfi(i).recno_correction = 0
	   dc(ch).dfi(i).ddr_ins.xref_ptr = 0
	   do j=0,size_of_packet
	      dc(ch).dfi(i).ddr_ins.pkt(j) = 0
	   end do
	end do

	dc(ch).first_major = 0
	dc(ch).first_minor = 0
	dc(ch).last_major  = 0
	dc(ch).last_minor  = 0
	do i=1,max_xref_buffer_size
	   dc(ch).xrbuf(i).scet            = 0.0
	   dc(ch).xrbuf(i).dpu_major_frame = 0
	   dc(ch).xrbuf(i).dpu_version     = 0
	   dc(ch).xrbuf(i).fft_version     = 0
	   dc(ch).xrbuf(i).tds_version     = 0
	end do

	w_decom_close_stream = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_decom_set_file(ch, f, idx)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	character*(*)	f
	integer*4	idx
	character*256	file
	integer*4	k3len				! a function
	integer*4	atc_to_ur8			! a function
	integer*4	index_last			! a function
	integer*4	ok
	integer*4	w_decom_open_file		! a function
	integer*4	w_decom_read_rec		! a function
	integer*4	i,j,k,o
	character*3	ext
	integer*4	lrecl
	integer*4	recno

	w_decom_set_file = 0
	if (idx .lt. 0 .or. idx .gt. xr_idx) goto 10
	if (dc(ch).dfi(idx).lun .ne. 0) goto 20

	file = f
	k = k3len(file)
	if (k .lt. 4) goto 30
	i = index_last(file, '.')
	if (i .lt. 1) goto 32
	j = i + 1
	call w_decom_get_extension( ext, idx )
	file(j:j+2) = ext
	k = j + 3
	if (file(k:k) .eq. ';') file(k:k+4) = ' '
	k = k - 1

	ok = w_decom_open_file(
	1	dc(ch).dfi(idx).lun,
	1	file(1:k),
	1	lrecl)
	if (ok .ne. 1) goto 40

	dc(ch).dfi(idx).byte_lrecl = lrecl * 4
	dc(ch).dfi(idx).file = file

	recno = 1
	ok = w_decom_read_rec(ch, idx, recno)
	if (ok .ne. 1) goto 50

	dc(ch).dfi(idx).first_rec = dc(ch).dfi(idx).duh.n_of_hdr_recs + 1
	dc(ch).dfi(idx).last_rec = dc(ch).dfi(idx).first_rec + 
	1	dc(ch).dfi(idx).duh.n_of_data_recs - 1

	! read in the first data rec to establish the stream position
	recno = dc(ch).dfi(idx).first_rec
	ok = w_decom_read_rec(ch, idx, recno)
	if (ok .ne. 1) goto 52

	! gather stream limits from master file
	if (idx .eq. xr_idx) then
	   ok = atc_to_ur8(dc(ch).dfi(idx).ddr_xr.atc_year,
	1		dc(ch).dfi(idx).ddr_xr.atc_day,
	1		dc(ch).dfi(idx).ddr_xr.atc_msec,
	1		dc(ch).dfi(idx).scet_initial)
	   if (ok .ne. 1) goto 60

	   ! goto last data rec and get it's time
	   recno = dc(ch).dfi(idx).last_rec
	   ok = w_decom_read_rec(ch, idx, recno)
	   if (ok .ne. 1) goto 54

	   ok = atc_to_ur8(dc(ch).dfi(idx).ddr_xr.atc_year,
	1		dc(ch).dfi(idx).ddr_xr.atc_day,
	1		dc(ch).dfi(idx).ddr_xr.atc_msec,
	1		dc(ch).dfi(idx).scet_final)
	   if (ok .ne. 1) goto 70

	   ! reset to bof stream position at first data rec
	   recno = dc(ch).dfi(idx).first_rec
	   ok = w_decom_read_rec(ch, idx, recno)
	   if (ok .ne. 1) goto 56
	end if

	! check for record number correction
	if (dc(ch).dfi(idx).duh.n_mcr_hdr_recs .ne.
	1   dc(ch).dfi(xr_idx).duh.n_mcr_hdr_recs) then
	   type *, '...# of header records does not match!!...'
	   i = zext(dc(ch).dfi(idx).duh.n_mcr_hdr_recs)
	   j = zext(dc(ch).dfi(xr_idx).duh.n_mcr_hdr_recs)
	   dc(ch).dfi(idx).recno_correction = i - j
	end if

	w_decom_set_file = 1
	return
  1	format(1x,'W_DECOM_SET_FILE: ', a, :, i3)
 10	continue
	write(6,1,iostat=o) 'bad index value of ', idx
	return
 20	continue
	write(6,1,iostat=o) 'file already open for idx ', idx
	return
 30	continue
	write(6,1,iostat=o) 'this file name is bad: '//file(1:4)
	return
 32	continue
	write(6,1,iostat=o) 'cannot locate extension in '//file(1:k)
	return
 40	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot open file'
	return
 50	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot read first header record'
	return
 52	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot read first data record'
	return
 54	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot read last data record'
	return
 56	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot read first data record (reset)'
	return
 60	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot convert ATC to UR8 (a)'
	return
 70	continue
	w_decom_set_file = ok
	write(6,1,iostat=o) 'cannot convert ATC to UR8 (b)'
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_decom_fill_xrbuf(ch)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	integer*4	ok
	integer*4	w_decom_read_rec		! a function
	integer*4	atc_to_ur8			! a function
	integer*4	mjr
	integer*4	recno
	integer*4	o

	w_decom_fill_xrbuf = 0
	dc(ch).first_major = 1
	dc(ch).first_minor = 0
	dc(ch).last_major = 0
	dc(ch).last_minor = 249
	mjr = 1
	recno = dc(ch).dfi(xr_idx).first_rec

	ok = w_decom_read_rec(ch, xr_idx, recno)
	do while(ok .eq. 1)
	   dc(ch).xrbuf(mjr).flags = dc(ch).dfi(xr_idx).ddr_xr.flags
	   dc(ch).xrbuf(mjr).dpu_major_frame = 
	1	dc(ch).dfi(xr_idx).ddr_xr.dpu_major_frame
	   dc(ch).xrbuf(mjr).dpu_version = dc(ch).dfi(xr_idx).ddr_xr.dpu_version
	   dc(ch).xrbuf(mjr).fft_version = dc(ch).dfi(xr_idx).ddr_xr.fft_version
	   dc(ch).xrbuf(mjr).tds_version = dc(ch).dfi(xr_idx).ddr_xr.tds_version
	   ok = atc_to_ur8(dc(ch).dfi(xr_idx).ddr_xr.atc_year,
	1                 dc(ch).dfi(xr_idx).ddr_xr.atc_day,
	1                 dc(ch).dfi(xr_idx).ddr_xr.atc_msec,
	1		dc(ch).xrbuf(mjr).scet)
	   if (ok .ne. 1) goto 10
	   dc(ch).last_major = dc(ch).last_major + 1
	   recno = recno + 1
	   mjr = mjr + 1
	   if (mjr .gt. max_xref_buffer_size) goto 20
	   ok = w_decom_read_rec(ch, xr_idx, recno)
	end do

	w_decom_fill_xrbuf = 1
	return
  1	format(1x,'W_DECOM_FILL_XRBUF: ', a, i4)
 10	continue
	write(6,1,iostat=o) 'cannot get record number ', recno
	return
 20	continue
	write(6,1,iostat=o) 'Xref buffer overflow at ', mjr
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_decom_read_rec(ch, idx, recno)
	implicit	none
	include		'wind_os_def.for'
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	idx
	integer*4	recno
	integer*4	ios
	integer*4	i,o

	w_decom_read_rec = 1

	if (dc(ch).dfi(idx).recno .eq. recno) return

	if (recno .eq. 1) then
	   read(dc(ch).dfi(idx).lun, rec=1, iostat=ios, err=10)
	1	dc(ch).dfi(idx).duh
	   i = dc(ch).dfi(idx).duh.flags .and. big_endian_encoding
	   if (sunos .and. (i .eq. 0)) then
	      call w_vxtosuni4(dc(ch).dfi(idx).duh.recl)
	      call w_vxtosuni4(dc(ch).dfi(idx).duh.n_of_hdr_recs)
	      call w_vxtosuni4(dc(ch).dfi(idx).duh.n_of_data_recs)
	   end if
	   if (macos_ppc .and. (i .eq. 0)) then
	      call w_vxtosuni4(dc(ch).dfi(idx).duh.recl)
	      call w_vxtosuni4(dc(ch).dfi(idx).duh.n_of_hdr_recs)
	      call w_vxtosuni4(dc(ch).dfi(idx).duh.n_of_data_recs)
	   end if
	   dc(ch).dfi(idx).recno = recno
	else if (recno .lt. 1) then
	   w_decom_read_rec = w_beginning_of_file
	else if (recno .gt. dc(ch).dfi(idx).last_rec) then
	   w_decom_read_rec = w_end_of_file
	else
	   if (idx .ge. 0 .and. idx .le. 15) then
	      read(dc(ch).dfi(idx).lun, rec=recno, iostat=ios, err=10)
	1	  dc(ch).dfi(idx).ddr_ins
	   else if (idx .eq. xr_idx) then
	      read(dc(ch).dfi(idx).lun, rec=recno, iostat=ios, err=10)
	1	  dc(ch).dfi(idx).ddr_xr
	   else if (idx .eq. hk_idx) then
	      read(dc(ch).dfi(idx).lun, rec=recno, iostat=ios, err=10)
	1	  dc(ch).dfi(idx).ddr_hk
	   else
	      read(dc(ch).dfi(idx).lun, rec=recno, iostat=ios, err=10)
	1	  (dc(ch).dfi(idx).byte_rec(i), i=1,dc(ch).dfi(idx).byte_lrecl)
	   end if
	   if (sunos) then
	      if (idx .eq. xr_idx) then
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.atc_year)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.atc_day)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.atc_msec)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.dpu_major_frame)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.ert_major_frame)
	      else if (idx .eq. hk_idx) then
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_hk.xref_ptr)
	      else
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_ins.xref_ptr)
	      end if
	   end if
	   if (macos_ppc) then
	      if (idx .eq. xr_idx) then
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.atc_year)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.atc_day)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.atc_msec)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.dpu_major_frame)
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_xr.ert_major_frame)
	      else if (idx .eq. hk_idx) then
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_hk.xref_ptr)
	      else
	         call w_vxtosuni4(dc(ch).dfi(idx).ddr_ins.xref_ptr)
	      end if
	   end if
	   dc(ch).dfi(idx).recno = recno
	end if

	return
  1	format(1x,'W_DECOM_READ_REC: error reading rec#', i5, ', ios=',i4)
 10	continue
	w_decom_read_rec = 0
	write(6,1,iostat=o) recno, ios
	return
	end

!------------------------------------------------------------------------------
! Opens specified wind/waves decom file and determines the file's record length
!------------------------------------------------------------------------------
	integer*4	function	w_decom_open_file(lun,fname,lrecl)
	implicit	none
	include		'wind_os_def.for'
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	lun		! w, FORTRAN logical unit number
	character*(*)	fname		! r, file name
	integer*4	lrecl		! w, logical record length
	integer*4	j
	integer*4	ios, ios2
	integer*4	ok
	integer*4	lib$get_lun
	logical*4	exist
	logical*4	wind_suppress_internal_msgs	! a function
	integer*4	w_decom_cnvrt_rec		! a function
	record /decom_universal_hdr_disk_rec/ duh

	w_decom_open_file = 0

	lun = 0
	ok = lib$get_lun(lun)
	if (ok .ne. 1) goto 10 

	! Decom files come in three different record lengths.
	! First open the file for sequential access to read in the
	! header record containing the byte record length.
	! Reopen the file with the correct lrecl for direct access.
	!
	! "Regular" Sun FORTRAN requires unformatted record lengths in bytes.
	! VAX VMS FORTRAN requires unformatted record lengths in longwords.
	! Sun FORTRAN programs compiled with the VAX/VMS emulation stuff
	! wants the unformatted record lengths in longwords.
	open(lun, 
	! vax/vms isms first
	2	readonly,
	! standard fortran stuff...
	1	file=fname,
	1	access='sequential',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios,
	1	err=20)

	read(lun, iostat=ios, err=30) duh
	close(lun)
	lrecl = duh.recl
	j = duh.flags .and. big_endian_encoding
	if (sunos .and. (j .eq. 0)) then
	   call w_vxtosuni4(lrecl)
	end if
	if (macos_ppc .and. (j .eq. 0)) then
	   call w_vxtosuni4(lrecl)
	end if
$IF ABSOFT_FORTRAN
$ELSE
	j = mod(lrecl,4)
	lrecl = (lrecl + j) / 4
$ENDIF
	open(lun, 
	2	readonly,
	1	recl=lrecl,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios,
	1	err=40)

	w_decom_open_file = 1
	return
   1	format(1x,'W_DECOM_OPEN_FILE: ', a, :, i4, :, i8)
   2	format(1x,'W_DECOM_OPEN_FILE: ', a, z8.8)
   3	format(1x,'W_DECOM_OPEN_FILE: ', a, a40)
  10	continue
	if (wind_suppress_internal_msgs()) return
	write(6,2,iostat=ios) 'Cannot allocate a LUN, ok=', ok
	lun = 0
	return
  20	continue
	call lib$free_lun(lun)
	lun = 0
	if (wind_suppress_internal_msgs()) return
	inquire(file=fname, exist=exist)
	if (.not. exist) then
	   write(6,1,iostat=ios2) 'Nonexistant file.'
	else
	   write(6,1,iostat=ios2) 'Error opening file (seq), iostat=', ios
	end if
	write(6,3,iostat=ios) 'file: ', fname
	return
  30	continue
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios2) 
	1 'Error reading first sequential access record, ios=',ios
	return
  40	continue
	call lib$free_lun(lun)
	lun = 0
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios2) 
	1 'Unable to open file for direct access, iostat=', ios, lrecl
	write(6,3,iostat=ios) 'file: ', fname
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_decom_get_extension( ext, idx )
	implicit	none
	character*(*)	ext
	integer*4	idx
	character*3	extensions(0:17) /
	1		'000', 'rd1', 'rd2', 'tnr',
	1		'fft', 'tdf', 'tds', 'ftl', 
	1		'ftm', 'fth', '010', '011', 
	1		'012', 'dmp', '014', '015',
	1		'hk ', 'mcr' /

	w_decom_get_extension = 0
	ext = ' '
	if (idx .lt. 0 .or. idx .gt. 17) return

	ext = extensions(idx)	

	w_decom_get_extension = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_decom_synch_files(ch,mjr,mnr)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	integer*4	ok
	integer*4	xr_recno
	integer*4	i,j,k,o
	integer*4	w_decom_synch_rec
	logical*4	wind_suppress_internal_msgs	! a function

	w_decom_synch_files = 0

	xr_recno = mjr + dc(ch).dfi(xr_idx).first_rec - 1

	do i=min_packet_id,max_packet_id
	   if (dc(ch).dfi(i).lun .ne. 0) then
	      if (dc(ch).dfi(i).ddr_ins.xref_ptr .ne. xr_recno) then
	         ok = w_decom_synch_rec(ch, i, xr_recno, mnr)
	         if (ok .ne. 1) goto 10
	      end if
	   end if
	end do

	! hk

	w_decom_synch_files = 1
	return
   1	format(1x,'W_DECOM_SYNCH_FILES: ', a, i2)
  10	continue
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=o) 'cannot synch with file id ', i
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_decom_synch_rec(ch,idx,recno,mnr)
	implicit	none
	include 	'parm_def.for'
	include 	'wind_hk_def.for'
	include		'wind_decom_blk_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	idx
	integer*4	recno
	integer*4	mnr
	integer*4	ok
	integer*4	xr_recno
	integer*4	i,j,k,o
	integer*4	mnr2
	logical*4	wind_suppress_internal_msgs	! a function
	integer*4	w_decom_read_rec		! a function

	w_decom_synch_rec = 0

	! this is a piss poor algorithm...but ok for now...xxxxxx
! idea: could save the xref_ptr value of every 100th ins record in
! the ins header records
! idea: save the entire unique xref_ptr set in the header records
	! read sequentially through records until matching xref 
	mnr2 = (mnr/10) * 10
	i = dc(ch).dfi(idx).ddr_ins.xref_ptr
	j = dc(ch).dfi(idx).recno
	if (i .gt. recno) j = dc(ch).dfi(idx).first_rec
	ok = 1
	do while(ok .eq. 1)
	   ok = w_decom_read_rec(ch, idx, j)

!	type 7, '...synching with recno,j,mnr..', recno, j, mnr,
!	1  dc(ch).dfi(idx).ddr_ins.pkt(0)
! 7	format(1x,a,i5,1x,i5,1x,i3.3,1x,z2.2)

	   if (ok .eq. w_end_of_file) goto 20
	   if (ok .ne. 1) goto 10
	   if (dc(ch).dfi(idx).ddr_ins.xref_ptr .eq. recno) then
	      k = zext(dc(ch).dfi(idx).ddr_ins.ert_minor_frame)
	      if (k .ge. mnr2) ok = 2
	   else if (dc(ch).dfi(idx).ddr_ins.xref_ptr .gt. recno) then
	      ok = 2
	   end if
	   j = j + 1
	end do

	mnr = zext(dc(ch).dfi(idx).ddr_ins.ert_minor_frame)
	w_decom_synch_rec = 1
	return
   1	format(1x,'W_DECOM_SYNCH_REC: ', a, i2, ', rec#', i4)
  10	continue
	w_decom_synch_rec = ok
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=o) 'cannot synch with file id ', idx, i
	return
  20	continue
	w_decom_synch_rec = ok
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=o) 'cannot synch (eof) with file id ', idx, i
	return
	end
