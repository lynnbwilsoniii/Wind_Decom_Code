! wind_cdhf_lib.for - WIND TM lib routines for accessing CDHF files
!
! For VMS FORTRAN 6.0 and later compile this module with: /warn=(nounused)
! to avoid informational compiler messages regarding unused variables.
!

!------------------------------------------------------------------------------
! Initializes internal data structures for using CDHF format disk files.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_setup_stream(ch,f,t1,t2)
	implicit	none
	integer*4	ch
	character*(*)	f
	real*8		t1,t2
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ok
	integer*4	w_cdhf_open_file
	integer*4	w_cdhf_set_current_rec
	integer*4	w_cdhf_get_rec_xc
	integer*4	i
	integer*4	max_tries_for_1st_valid_rec
	parameter	(max_tries_for_1st_valid_rec=64)
	integer*4	good_record
	integer*4	ios
	integer*4	file_number /0/
	integer*4	w_cdhf_set_file_limits
	integer*4	w_cdhf_setup_nrt

	w_cdhf_setup_stream = 0

	if (f(1:3) .eq. 'nrt') then
	   ! open a tcp/ip near realtime socket stream
	   ok = w_cdhf_setup_nrt(ch,f)
	   if (ok .ne. 1) goto 10
	else
	   ! open the file, retrieving the file label record
	   ok = w_cdhf_open_file(cdhf(ch).lun, f, cdhf(ch).lrecl, cdhf(ch).hdr)
	   if (ok .ne. 1) then
	      w_cdhf_setup_stream = ok
	      return
	   end if
	   cdhf(ch).is_nrt = .false.
	end if

	file_number = file_number + 1

	cdhf(ch).ch                = ch
	cdhf(ch).major_frame       = 1
	cdhf(ch).first_major       = 1
	cdhf(ch).last_major        = cdhf(ch).hdr.max_recs_in_file - 1
	cdhf(ch).minor_frame.i4val = 0
	cdhf(ch).first_minor.i4val = 0
	cdhf(ch).last_minor.i4val  = 249
	cdhf(ch).file              = f
	cdhf(ch).first_record      = 2
	cdhf(ch).last_record       = cdhf(ch).hdr.max_recs_in_file
	cdhf(ch).recno             = 0
	cdhf(ch).transcend_file    = .false.
	cdhf(ch).verbose_rollover  = .true. 
	cdhf(ch).file_number       = file_number

	! Read the first valid data record to establish the current position.
	! This loop added to take skip over bad early records found in
	! so-called "near realtime" cdhf files.  Bad records are sometimes
	! identified
	! by nonsensical atc_year field values in the data record header.
	i = 0
	good_record = 0
	do while(i .lt. max_tries_for_1st_valid_rec)
	   ok = w_cdhf_set_current_rec(ch,cdhf(ch).first_major)

	   if (ok .ne. 1) then
	      w_cdhf_setup_stream = ok
	      return
	   end if

	   if (cdhf(ch).lzr.h.atc_year .gt. 1990) then
	      i = max_tries_for_1st_valid_rec
	      good_record = 1
	   else
	      cdhf(ch).first_major  = cdhf(ch).first_major + 1
	      cdhf(ch).first_record = cdhf(ch).first_record + 1
	   end if
	   i = i + 1
	end do
	if (good_record .ne. 1) goto 10

	! set bof/eof limits for current file
	ok = w_cdhf_set_file_limits(ch,t1,t2)
	if (ok .ne. 1) goto 30

	! assign user channel values
	cdhf(ch).major_frame = cdhf(ch).first_major
	!
	user(ch).major_frame = cdhf(ch).major_frame
	user(ch).first_major = cdhf(ch).first_major
	user(ch).last_major  = cdhf(ch).last_major
	user(ch).minor_frame = cdhf(ch).minor_frame
	user(ch).first_minor = cdhf(ch).first_minor
	user(ch).last_minor  = cdhf(ch).last_minor

	! assume all cdhf words are valid 
	! [.wnd files are supplied with a valid word list from the 
	! GSE software]
	user(ch).all_tm_word_priv = .true.

	! establish the central wind_lib reference record
	ok = w_cdhf_get_rec_xc(
	1	ch, 
	1	user(ch).first_major, 
	1	user(ch).first_minor,
	1	user(ch).wind_record)
	if (ok .ne. 1) goto 20

	call w_cdhf_set_functions(ch)

	!call show_cdhf_file_label_record(cdhf(ch).hdr)
	!call show_cdhf_data_record_header(cdhf(ch).lzr)

	w_cdhf_setup_stream= ok

	return
  1	format(1x,'W_CDHF_SETUP_STREAM: ', a)
 10	continue
	w_cdhf_setup_stream = 0
	write(6,1,iostat=ios) 'cannot get valid initial record.'
	return
 20	continue
	w_cdhf_setup_stream = 0
	write(6,1,iostat=ios) 'cannot get initial xc record.'
	return
 30	continue
	w_cdhf_setup_stream = ok
	write(6,1,iostat=ios) 'cannot set file access limits.'
	return
	end
!------------------------------------------------------------------------------
! A dummy routine
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_dummy()
	type *, 'Inside w_cdhf_dummy routine.'
	w_cdhf_dummy = 0
	return
	end
!------------------------------------------------------------------------------
! Stores addresses of CDHF-specific functions for later invocation by
! core wind_lib routines.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_set_functions(ch)
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch
	external	w_cdhf_get_word
	external	w_cdhf_get_rec_xc
	external	w_cdhf_get_packet
	external	w_cdhf_get_rec_by_time
	external	w_cdhf_scet_of_mfmf
	external	w_cdhf_scet_dbms_of_mfmf
	external	w_cdhf_frame_rate
	external	w_cdhf_get_xtra_event_info
	external	w_cdhf_close_stream
	external	w_cdhf_get_mode
	external	w_cdhf_get_hk
	external	w_cdhf_get_dpu_mf
	external	w_cdhf_get_mfmf
	external	w_cdhf_dummy

	! these values must be non-zero: use dummy functions if necessary
$IF ABSOFT_FORTRAN
	user(ch).f_get_word            = loc(w_cdhf_get_word)
	user(ch).f_get_intrnl_rec      = loc(w_cdhf_get_rec_xc)
	user(ch).f_get_plain_packet    = loc(w_cdhf_get_packet)
	user(ch).f_goto_time           = loc(w_cdhf_get_rec_by_time)
	user(ch).f_get_mf_scet         = loc(w_cdhf_scet_of_mfmf)
	user(ch).f_get_mf_scet_dbms    = loc(w_cdhf_scet_dbms_of_mfmf)
	user(ch).f_get_frame_rate      = loc(w_cdhf_frame_rate)
	user(ch).f_get_xtra_event_info = loc(w_cdhf_get_xtra_event_info)
	user(ch).f_close_ch            = loc(w_cdhf_close_stream)
	user(ch).f_get_tm_mode         = loc(w_cdhf_get_mode)
	user(ch).f_get_hk              = loc(w_cdhf_get_hk)
	user(ch).f_get_dpu_mf          = loc(w_cdhf_get_dpu_mf)
	user(ch).f_get_rec_idx         = loc(w_cdhf_get_mfmf)
$ELSE
	user(ch).f_get_word            = %loc(w_cdhf_get_word)
	user(ch).f_get_intrnl_rec      = %loc(w_cdhf_get_rec_xc)
	user(ch).f_get_plain_packet    = %loc(w_cdhf_get_packet)
	user(ch).f_goto_time           = %loc(w_cdhf_get_rec_by_time)
	user(ch).f_get_mf_scet         = %loc(w_cdhf_scet_of_mfmf)
	user(ch).f_get_mf_scet_dbms    = %loc(w_cdhf_scet_dbms_of_mfmf)
	user(ch).f_get_frame_rate      = %loc(w_cdhf_frame_rate)
	user(ch).f_get_xtra_event_info = %loc(w_cdhf_get_xtra_event_info)
	user(ch).f_close_ch            = %loc(w_cdhf_close_stream)
	user(ch).f_get_tm_mode         = %loc(w_cdhf_get_mode)
	user(ch).f_get_hk              = %loc(w_cdhf_get_hk)
	user(ch).f_get_dpu_mf          = %loc(w_cdhf_get_dpu_mf)
	user(ch).f_get_rec_idx         = %loc(w_cdhf_get_mfmf)
$ENDIF

	w_cdhf_set_functions = 1
	return
	end
!------------------------------------------------------------------------------
! This routine retrieves a wind_lib format internal record from the ch's
! CDHF file based on time.  For now the algorithm is very simple.  It assumes
! no time gaps in the file.  It positions the caller at the beginning of the
! major frame in which the caller's time occurs.  It also uses ACT values for
! times because I've not been able use the PB5 times yet.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_rec_by_time
	1				(ch,mjr,mnr,ut,cpy_to_buf,wr)
	implicit	none
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_return_code_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	real*8		ut		! user's time, UR8 format
	logical*4	cpy_to_buf	! 
	record /wind_record/ wr		! wind_tm_lib internal buffer

	integer*4	ok
	integer*4	ios
	integer*4	j,k
	integer*4	mf
	integer*4	nr
	integer*4	w_cdhf_set_current_rec		! a function
	integer*4	w_cdhf_get_rec_xc		! a function
	logical*4	wind_suppress_messages
	integer*4	w_cdhf_binary_search2
	integer*4	w_cdhf_tm_speed
	integer*4	atc_to_ur8
	integer*4	tm_speed
	logical*4	found
	integer*4	w_cdhf_goto_file		! a function
	integer*4	w_cdhf_scet_ur8_of_mfmf		! a function
	real*8		dt, adt, dmft
	real*8		xt
	real*8		t1, t2, t3, t4
	real*8		a,b

	w_cdhf_get_rec_by_time = 0
	mjr = 0
	mnr = 0
	t1 = cdhf(ch).t1
	t2 = cdhf(ch).t2

!	write(6,'(1x,a,3(2x,f12.7))') '...ut, t1, t2', ut, t1, t2
	if (ut .lt. t1 .or. ut .gt. t2) then
!	write(6,'(1x,a,3(2x,f12.7))',iostat=o) '...ut, t1, t2', ut, t1, t2
	   if (ut .lt. t1) then
	      j = t1
	      xt = j
	      if (xt .eq. ut) then
	         ! assume caller is using an integer form of the bof time
	         ut = t1
	         mf = 1
	         mnr = 0
                 goto 2000
	      end if
	   end if
	   ! is the caller outside the current file limits?
	   ok = w_cdhf_goto_file(ch,ut)
	   if (ok .ne. 1) goto 10
	end if

	! number of major frames in file
!	nr = cdhf(ch).hdr.n_major_in_file
	nr = cdhf(ch).last_major - cdhf(ch).first_major + 1

	! calcualte an average delta time between major frames
	! t2 is time of last MF.249
	dt = (t2 - t1) / dble(nr)

	! calculate a delta time between file start and caller's time
	xt = ut - t1

	! divide (caller's - file_start) by the the diff between MF
	! (add first_major because MF's start at 1, first_major is
	! file's lower limit)
	! (add one to get the physical record number)
	mf = (xt / dt) + cdhf(ch).first_major + 1
	if (mf .lt. 1 .or. mf .gt. cdhf(ch).last_major) goto 92
!	write(6,'(1x,a,i5,i5,2(2x,f12.8))',iostat=ios) 
!	1'...mf,nr,xt,dt=', mf, nr, xt, dt

	! establish the stream position
	ok = w_cdhf_set_current_rec(ch,mf)
	if (ok .ne. 1) goto 100

	! verify the stream position by time
	ok = atc_to_ur8(cdhf(ch).lzr.h.atc_year,
	1               cdhf(ch).lzr.h.atc_day,
	1               cdhf(ch).lzr.h.atc_msec,
	1               t3)
	if (ok .ne. 1) goto 110
	dt = t3 - ut
	adt = dabs(dt)
	found = .false.
	ok = w_cdhf_tm_speed(ch,mf,tm_speed)
	if (ok .ne. 1) goto 120
	if (dt .ge. 0.0) then
	   if (tm_speed .eq. 2) then
	      if (adt .lt. ur8_46s) found = .true.
	   else
	      if (adt .lt. ur8_92s) found = .true.
	   end if
	else if (dt .lt. 0.0 .and. mf .gt. 1) then
	   if (tm_speed .eq. 2) then
	      if (adt .lt. ur8_46s) found = .true.
	   else
	      if (adt .lt. ur8_92s) found = .true.
	   end if
	end if
!	write(6,'(1x,a,3(2x,f14.8),1x,l1)',iostat=ios) 
!	1'...t3,adt,dt,found=', t3, adt, dt, found

	if (.not. found) then
	   if (ut .lt. t3) then
	      ! target is between current position and BOF
	      ok = w_cdhf_binary_search2(ch,t1,ut,t3,
	1	   cdhf(ch).first_major, mf, dt, t3)
	   else
	      ! target is between current position and EOF
	      ok = w_cdhf_binary_search2(ch,t3,ut,t2,
	1	   mf, cdhf(ch).last_major, dt, t3)
	   end if
	   if (ok .ne. 1) goto 150
	   mf = cdhf(ch).major_frame
	   ok = w_cdhf_tm_speed(ch,mf,tm_speed)
	   if (ok .ne. 1) goto 120
	end if

	!
	! determine the minor frame number
	!
 1000	continue
	mnr = 0
	j = 0
	k = 0
	nr= 0

!	midtdiff = midt - ut
!	dt = t3 - ut
! negative dt means add minor frames, final t3 must be later than ut

	if (tm_speed .eq. 2) then
	   dmft = ur8_mfdtf
	else
	   dmft = ur8_mfdts
	end if
	xt = dt / dmft

!	write(6,'(1x, a, i2, 3(2x,f12.7))',iostat=ios) 
!	1 '...TM_SPEED,dt,ut=', tm_speed, dt, ut, dmft
!	write(6,'(1x, a, i4,1x, 2(2x,f14.8))',iostat=ios) 
!	1 '...mf,t3,xt=', mf, t3, xt
	
	! dt could be positive or negative
	! t3 is time at some MF.0
	if (xt .le. 0.0 .and. xt .gt. -250.0) then
	   ! negative xt means ut > t3, so we go abs(xt) minor frames forward
	   ! into the current major frame
	   xt = -xt
	else if (xt .gt. 0.0 .and. xt .le. 250.0) then
	   ! positive xt means ut < t3, so we go into the previous major frame
	   mf = mf - 1
	   xt = 250.0 - xt
	   t3 = t3 - (250.0 * dmft)
	   ! should probably check new tm speed here....after a set_rec
	else
	   goto 170
	end if

!	write(6,'(1x, a, i4,1x, 2(2x,f14.8))',iostat=ios) 
!	1 '...mf,t3,xt=', mf, t3, xt

	mnr = xt
	a = mnr
	b = a * dmft
	t3 = t3 + b
	if (t3 .lt. ut) then
!	   type *, '...incrementing MF.mf (1)', mf, mnr
	   call wind_tm_increment_mfmf(mf,mnr)
	   t3 = t3 + dmft
	end if

!	write(6,'(1x, a, i4,''.'',i3.3,1x, 3(2x,f14.8))',iostat=ios) 
!	1 '...mf.mnr,t3,a,b=', mf, mnr, t3, a,b

	! make sure new position is later than caller's time 
	ok = w_cdhf_scet_ur8_of_mfmf(ch,mf,mnr,t4)
	if (ok .ne. 1) goto 200
	if (t4 .lt. ut) then
!	   type *, '...incrementing MF.mf (2)', mf, mnr
	   call wind_tm_increment_mfmf(mf,mnr)
	   if (mf .gt. cdhf(ch).last_major) then
!	type *, '...decrementing MF.mf', mf, mnr
	      call wind_tm_decrement_mfmf(mf,mnr)
	   else
	      ok = w_cdhf_scet_ur8_of_mfmf(ch,mf,mnr,t4)
	   end if
	end if
	ut = t4

 2000	continue

	mjr = mf
	cdhf(ch).minor_frame.i4val = mnr
!	type *, '...filling MF.mf', mjr, mnr

	! fill the user's buffer
	if (cpy_to_buf) then
	   ok = w_cdhf_get_rec_xc(
	1	ch, 
	1	mjr,
	1	mnr,
	1	wr)
	   if (ok .ne. 1) goto 160
	end if

	w_cdhf_get_rec_by_time = 1

	return
   1	format(1x,'W_CDHF_GET_REC_BY_TIME: ', a)
   2	format(1x,'W_CDHF_GET_REC_BY_TIME: ', a, f12.7)
  10	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'time out of range or unable to get new file.'
	return
  92	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'after all this, the MF is not valid.'
	return
 100	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot establish the current record.'
	return
 110	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert targeted time to generic.'
	return
 120	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot determine TM bit rate (fast/slow).'
	return
 150	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'binary search for closest time failed.'
	write(6,2,iostat=ios) 'could not position stream to ', ut
	return
 160	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot copy record to caller''s buffer.'
	return
 170	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'delta time yeilded bad results.'
	return
 200	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get time at target MF.mf.'
	return
	end
!------------------------------------------------------------------------------
! This routine performs a binary search of the CDHF file associated with the
! supplied channel based on SCET.  Time t1 should be earlier than time t2.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_binary_search2
	1				(ch,t1,ut,t2,p1,p2,dt,zt)
	implicit	none
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_return_code_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch
	real*8 		t1,t2,ut	! start/end times, user time
	integer*4	p1, p2		! start/end MF major frame numbers
	real*8		dt, zt, adt
	integer*4	ok
	integer*4	ios
	integer*4	k
	integer*4	n_reads
!	integer*4	n_calls
!	integer*4	n_cumulative_reads
	integer*4	tm_speed
	integer*4	max_search_depth
	parameter	(max_search_depth=17)
	integer*4	w_cdhf_tm_speed			! a function
	integer*4	w_cdhf_set_current_rec		! a function
	integer*4	atc_to_ur8			! a function
	logical*4	wind_suppress_messages
	structure /binary_search2_index/
	   integer*4	n
	   real*8 	t
	   real*8	tdiff
	end structure
	record /binary_search2_index/ lo, hi, mid
	logical*4	done
	integer*4	xhi,xlo
	integer*4	midpoint
	midpoint(xlo,xhi) = max(xlo, ((xhi+xlo)/2) )

	w_cdhf_binary_search2 = 0

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
	   ok = w_cdhf_set_current_rec(ch,mid.n)
	   if (ok .ne. 1) goto 10
	   n_reads = n_reads + 1
	   if (n_reads .ge. max_search_depth) goto 20

	   ! get the time of the MF at the midpoint
	   ok = atc_to_ur8(cdhf(ch).lzr.h.atc_year,
	1                 cdhf(ch).lzr.h.atc_day,
	1                 cdhf(ch).lzr.h.atc_msec,
	1		  mid.t)
	   if (ok .ne. 1) goto 30

	   ! get the delta time between midpoint and the user's time
	   mid.tdiff = mid.t - ut
	   adt = dabs(mid.tdiff)
	   ! are we within one major frame?...if so then success!
	   ok = w_cdhf_tm_speed(ch,mid.n,tm_speed)
	   if (ok .ne. 1) goto 50
	   if (tm_speed .eq. 2) then
	      if (adt .lt. ur8_46s) done = .true.
	   else
	      if (adt .lt. ur8_92s) done = .true.
	   end if
!	write(6,'(1x,a,3(i5))',iostat=ios) '___(lo,mid,hi).n:',lo.n,mid.n,hi.n
!	write(6,'(1x,a,3(1x,f12.7))',iostat=ios)
!	1   '___(lo,mid,hi).t:',lo.t,mid.t,hi.t

$IF ABSOFT_FORTRAN
!
!  2007/07/15:  the Absoft PwrPC Fortran compiler fails on
!
!     else if (mid.n .eq. lo.n) then
!
!  So just use an intermediate variable 'k' to keep going.
!
	   k = mid.n
	   if (done) then
	      done = .true.			! delta time minimized=success
	   else if (k .eq. lo.n) then
$ELSE
	   if (done) then
	      done = .true.			! delta time minimized=success
	   else if (mid.n .eq. lo.n) then
$ENDIF
	      done = .true.			! time gap? closest convergence
	   else
	      ! setup for next search iteration
	      if (ut .gt. mid.t) then
	         ! current midpoint is less than target time
	         lo = mid
	      else if (ut .lt. mid.t) then
	         ! current midpoint is greater than target time
	         hi = mid
	      else
	         goto 80			! should never get here
	      end if
$IF ABSOFT_FORTRAN
!
!  2007/07/15:  the Absoft PwrPC Fortran compiler fails on
!
!     if (lo.n .ge. hi.n) goto 90
!
!  So just use an intermediate variable 'k' to keep goint.
!

	      k = lo.n
	      if (k .ge. hi.n) goto 90	! time gap? reverse time?
$ELSE
	      if (lo.n .ge. hi.n) goto 90	! time gap? reverse time?
$ENDIF
	   end if
	end do

	dt = mid.tdiff
	zt = mid.t
	w_cdhf_binary_search2 = 1

!	n_cumulative_reads = n_cumulative_reads + n_reads
!	type *, 'Binary Search2 used ', n_reads, ' reads.'
	return
   1	format(1x,'W_CDHF_BINARY_SEARCH2: ', a, :, i)
  10	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot set current record to ', mid.n
	return
  20	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'search depth exceeded at ', n_reads
	return
  30	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert time to binary at', mid.n
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
	integer*4	function	w_cdhf_get_xtra_event_info
	1				(ch,mjr,mnr)
	implicit	none
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	integer*4	ok
	integer*4	ios
	integer*4	xmajor
	integer*4	xminor
	integer*4	return_code
	integer*4	wind_get_event_scet	! a function
	integer*4	w_get_dpu_mjr_fr_num	! a function
	logical*4	wind_suppress_messages	! a function
	integer*4	w_cdhf_tm_speed		! a function
	integer*4	w_cdhf_scet_dbms_of_mfmf ! a function
	integer*4	speed
	integer*4	w_scet_err, w_scet_frctn_err
	integer*4	w_dpu_majf_err, w_ert_err
	integer*4	w_ert_frctn_err
	parameter	(w_scet_err	= '010'x)
	parameter	(w_scet_frctn_err= '020'x)
	parameter	(w_dpu_majf_err	= '040'x)
	parameter	(w_ert_err	= '100'x)
	parameter	(w_ert_frctn_err	= '200'x)
	record /low_byte/ lb
	integer*4	dbms_to_ur8

	w_cdhf_get_xtra_event_info = 0
	return_code = 1

	! get the major and minor frame numbers of the beginning of the event
	exi(ch).major = mjr
	exi(ch).minor = mnr - 9

	! get the sc mode
	lb.i4val = cdhf(ch).lzr.h.tm_mode
	exi(ch).sc_mode = lb.b

	! get the bit rate
	ok = w_cdhf_tm_speed(ch, mjr, speed)
	if (speed .eq. 1) then
	   exi(ch).bit_rate = 0
	else if (speed .eq. 2) then
	   exi(ch).bit_rate = 1
	else
	   exi(ch).bit_rate = 9
	end if

	! update the earth receive time value previously estimated
	! in routine init_packet_dissect_info
	ok = w_cdhf_scet_dbms_of_mfmf(ch,exi(ch).major, exi(ch).minor,
	1	exi(ch).ert(1),exi(ch).ert(2), exi(ch).ert1000)
	if (ok .ne. 1) goto 10
	ok = dbms_to_ur8(exi(ch).ert(1), exi(ch).ert(2), exi(ch).ert1000,
	1  exi(ch).ur8_ert)

	xmajor = mjr  
	xminor = mnr  
	ok = w_get_dpu_mjr_fr_num(ch,xmajor,xminor,exi(ch).dpu_major_ert)
	if (ok .ne. 1) return_code = return_code .or. w_dpu_majf_err

	! get the spacecraft event time
	xmajor = mjr
	xminor = mnr
	ok = wind_get_event_scet(ch,xmajor,xminor)
	if (ok .ne. 1) then
	   return_code = return_code .or. w_scet_err
	   ! use the ert to grossly estimate the scet
	   exi(ch).scet(1) = exi(ch).ert(1)
	   exi(ch).scet(2) = exi(ch).ert(2)
	end if
	ok = dbms_to_ur8(exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000,
	1  exi(ch).ur8_scet)
	if (ok .ne. 1) goto 20
	exi(ch).ur8_context = exi(ch).ur8_scet

!	! restore user's stream position to end of first packet of event
!	ok = w_wnd_get_rec(ch,major,minor)
!	if (ok .ne. 1) goto 80

!	w_cdhf_get_xtra_event_info = return_code

!	if ( (.not. wind_suppress_messages(ch)) .and. (.not.return_code)) then
!	   if ((return_code .and. w_scet_err) .ne. 0) then
!	      type 1, 'cannot get event SCET.'
!	   end if
!	   if ((return_code .and. w_ert_frctn_err) .ne. 0) then
!	      type 1, 'cannot get fractional ERT.'
!	   end if
!	   if ((return_code .and. w_dpu_majf_err) .ne. 0) then
!	      type 1, 'cannot get DPU MF.'
!	   end if
!	end if

	w_cdhf_get_xtra_event_info = 1
	return
   1	format(1x,'W_CDHF_GET_XTRA_EVENT_INFO: ', a)
 10	continue
	w_cdhf_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get ERT of 1st rec of event'
	return
 20	continue
	w_cdhf_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert DBMS SCET to UR8.'
	return
! 20	continue
!	w_cdhf_get_xtra_event_info = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot convert ert to ASCII for ert'
!	return
! 30	continue
!	w_cdhf_get_xtra_event_info = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot ASCII time to dbms time for ert'
!	return
	end
!------------------------------------------------------------------------------
! Determines the SpaceCraft Event Time for a particular MF.mf position in
! file.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_scet_of_mfmf(ch,mjr,mnr,t)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_record_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	record /vms_64bit_time/	t		! wind lib internal time format
	integer*4	ymd, hms, s1000
	integer*4	ok
	integer*4	ios
	integer*4	i,j
	integer*4	year, doy, msec, add
	integer*4	mode
	logical*4	is_leap
	integer*4	atc_to_wnd		! a function
	integer*4	atc_to_dbms		! a function
	integer*4	atc_to_ur8		! a function
	integer*4	w_cdhf_set_current_rec	! a function
	logical*4	is_a_leap_year		! a function
	logical*4	wind_suppress_messages	! a function
	integer*4	max_msec
	parameter	(max_msec=60*60*24*1000)
	real*4		rhi,rlo
	parameter	(rhi=(46.0/250.0)*1000.0)
	parameter	(rlo=(92.0/250.0)*1000.0)
	integer*4	hi,lo
	parameter	(hi=rhi)
	parameter	(lo=rlo)
	logical*4	is_fast_tm
	logical*4	is_slow_tm
	integer*4	a
	integer*4	max_buffered_scets
	parameter	(max_buffered_scets=16)
	structure /buffered_scets/
	   integer*4	year
	   integer*4	doy
	   integer*4	msec
	   integer*4	mode
	   integer*4	mjr
	   integer*4	fnum
	end structure
	record /buffered_scets/ bs(max_buffered_scets)
	integer*4	in /0/			! index into bs
	logical*4	set_rec
	logical*4	do_dbms_time
	logical*4	do_ur8_time
	logical*4	use_current
	integer*4	w_cdhf_scet_dbms_of_mfmf	! an entry point
	integer*4	w_cdhf_scet_ur8_of_mfmf		! an entry point
	integer*4	w_cdhf_scet_ur8_of_current	! an entry point
	real*8		ur8

	is_fast_tm(a) =a .eq. science_mode_2 .or.
	1              a .eq. maneuver_mode_2 .or.
	1	       a .eq. contingency_mode_2
	is_slow_tm(a) =a .eq. maneuver_mode_1 .or.
	1	       a .eq. science_mode_1 .or.
	1	       a .eq. contingency_mode_1 

	do_dbms_time = .false.
	do_ur8_time  = .false.
	use_current  = .false.
	goto 1000

	!---------------------------------------------------------------------- 
	entry	w_cdhf_scet_dbms_of_mfmf(ch,mjr,mnr,ymd,hms,s1000)
	do_dbms_time = .true.
	do_ur8_time  = .false.
	use_current  = .false.
	goto 1000

	!---------------------------------------------------------------------- 
	entry	w_cdhf_scet_ur8_of_mfmf(ch,mjr,mnr,ur8)
	do_dbms_time = .false.
	do_ur8_time  = .true.
	use_current  = .false.
	goto 1000

	!---------------------------------------------------------------------- 
	entry	w_cdhf_scet_ur8_of_current(ch,mjr,mnr,ur8)
	do_dbms_time = .false.
	do_ur8_time  = .true.
	use_current  = .true.
	goto 1000

 1000	continue

	if (mjr .lt. cdhf(ch).first_major) then
	   w_cdhf_scet_of_mfmf = w_beginning_of_file
	   return
	else if (mjr .gt. cdhf(ch).last_major) then
	   w_cdhf_scet_of_mfmf = w_end_of_file
	   return
	else
	   w_cdhf_scet_of_mfmf = 0
	end if

	set_rec = .true.
	if (in .ne. 0) then
	   ! check scet buffer for times of mf#0 of MF
	   i = 1
	   j = in
	   do while((i .le. max_buffered_scets) .and. set_rec)
	      if (bs(j).mjr .eq. mjr .and.
	1         bs(j).fnum .eq. cdhf(ch).file_number) then
	         year = bs(j).year
	         doy  = bs(j).doy
	         msec = bs(j).msec
	         mode = bs(j).mode
	         set_rec = .false.
	      else
	         i = i + 1
	         j = j - 1
	         if (j .le. 0) j = max_buffered_scets
	      end if
	   end do
	end if

	if (set_rec) then
	   ! establish the stream position
	   if (.not. use_current) then
	      ok = w_cdhf_set_current_rec(ch,mjr)
	      if (ok .ne. 1) goto 10
	   end if

	   year = cdhf(ch).lzr.h.atc_year
	   doy  = cdhf(ch).lzr.h.atc_day
	   msec = cdhf(ch).lzr.h.atc_msec
	   mode = cdhf(ch).lzr.h.tm_mode

	   in = in + 1
	   if (in .gt. max_buffered_scets) in = 1
	   bs(in).year = year
	   bs(in).doy  = doy
	   bs(in).msec = msec
	   bs(in).mode = mode
	   bs(in).mjr  = mjr
	   bs(in).fnum = cdhf(ch).file_number
	end if

	if (is_fast_tm(mode)) then
	   add = mnr * hi
	else if (is_slow_tm(mode)) then
	   add = mnr * lo
	else
	   goto 20
	end if

	msec = msec + add
	if (msec .gt. max_msec) then
	   msec = msec - max_msec
	   doy = doy + 1
	   is_leap = is_a_leap_year(year)
	   if ( ((.not. is_leap) .and. (doy .gt. 365)) .or.
	1        ( is_leap .and. (doy .gt. 366) ) ) then
	      doy = 1
	      year = year + 1
	   end if
	end if

	if (do_dbms_time) then
	   ok = atc_to_dbms(year,doy,msec,ymd,hms,s1000)
	   if (ok .ne. 1) goto 40
	else if (do_ur8_time) then
	   ok = atc_to_ur8(year,doy,msec,ur8)
	   if (ok .ne. 1) goto 50
	else
	   ok = atc_to_wnd(year,doy,msec,t)
	   if (ok .ne. 1) goto 30
	end if

	w_cdhf_scet_of_mfmf = 1
	return
   1	format(1x,'W_CDHF_SCET_OF_MFMF: ', a)
   2	format(1x,'W_CDHF_SCET_OF_MFMF: ', a, 1x, i, :, 1x, i, 1x, i)
 10	continue
	w_cdhf_scet_of_mfmf = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 20	continue
	w_cdhf_scet_of_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,2,iostat=ios) 'TM mode not science, maneuver, or contin.',
	1 cdhf(ch).lzr.h.tm_mode
	return
 30	continue
	w_cdhf_scet_of_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert ATC time to WND time.'
	write(6,2,iostat=ios) ' ATC time is: ', year, doy, msec
	return
 40	continue
	w_cdhf_scet_of_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert ATC time to DBMS time.'
	write(6,2,iostat=ios) ' ATC time is: ', year, doy, msec
	return
 50	continue
	w_cdhf_scet_of_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert ATC time to UR8 time.'
	write(6,2,iostat=ios) ' ATC time is: ', year, doy, msec
	return
	end
!------------------------------------------------------------------------------
! Returns one wind_record of data (essentially a 256-byte minor frame of data
! with some other stuff appended) to the caller.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_rec_xc(ch,mjr,mnr,wr)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_cdhf_alloc_def.for'
	include		'wind_record_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	record /wind_record/	wr		! main wind lib internal buffer
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	integer*4	w_cdhf_word
	integer*4	w_cdhf_scet_of_mfmf
	logical*4	wind_suppress_messages	! a function
	record /low_byte/ lbv
	integer*4	ios
	integer*4	i,j
	logical*4	is_science_fmt
	logical*4	is_maneuver_fmt
	integer*4	a
	is_science_fmt(a)=  a .eq. science_mode_2 
	1	       .or. a .eq. science_mode_1
	1	       .or. a .eq. contingency_mode_1 
	1	       .or. a .eq. contingency_mode_2
	is_maneuver_fmt(a)= a .eq. maneuver_mode_1
	1              .or. a .eq. maneuver_mode_2

	w_cdhf_get_rec_xc = 0

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

	! establish the stream position
	ok = w_cdhf_set_current_rec(ch,mjr)
	if (ok .ne. 1) goto 10

	! set the minor frame number
	lbv.i4val = mnr
	wr.minor_frame = lbv.b

	! gather the tm words
	wr.major_frame = mjr
	if (is_science_fmt(cdhf(ch).lzr.h.tm_mode)) then
	   do i=1,size_science_allocation
	      j = fixed_col_science_alloc(i)
	      ok = w_cdhf_word(cdhf(ch).lzr, mnr, j, wr.data(j))
	   end do
	else if (is_maneuver_fmt(cdhf(ch).lzr.h.tm_mode)) then
	   do i=1,size_maneuver_allocation
	      j = fixed_col_maneuver_alloc(i)
	      ok = w_cdhf_word(cdhf(ch).lzr, mnr, j, wr.data(j))
	   end do
	else
	   goto 20
	end if

	! set the quality bits
	if (cdhf(ch).lzr.h.quality(mnr) .eq. 0) then
	   ! no problems
	   wr.quality = 1
	else
	   lbv.b = cdhf(ch).lzr.h.quality(mnr)
	   ! shift left 1 bit to clear lowbit but preserve the quality codes
	   lbv.i4val = ishft(lbv.i4val,1)
	   wr.quality = lbv.b
	end if

	! get the scet and ert times
	ok = w_cdhf_scet_of_mfmf(ch,mjr,mnr,wr.scet.b)
	if (ok .ne. 1) goto 30
	wr.gathertime = wr.scet

	w_cdhf_get_rec_xc = 1
	return
   1	format(1x,'W_CDHF_GET_REC_XC: ', a, :, 1x, i)
 10	continue
	w_cdhf_get_rec_xc = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 20	continue
	w_cdhf_get_rec_xc = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'TM mode not science, maneuver, or contin.',
	1 cdhf(ch).lzr.h.tm_mode
	call show_cdhf_data_record_header(cdhf(ch).lzr)
	return
 30	continue
	w_cdhf_get_rec_xc = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get SCET for specified position.'
	return
	end
!------------------------------------------------------------------------------
! Returns one byte of CDHF TM data to caller.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_word(ch,mjr,mnr,wrd,buf)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	wrd		! addres of desired word in mnr
	byte		buf		! caller's buffer to receive word
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	integer*4	w_cdhf_word
	logical*4	wind_suppress_messages	! a function
	integer*4	ios

	ok = w_cdhf_set_current_rec(ch,mjr)
	if (ok .ne. 1) goto 10

	ok = w_cdhf_word(cdhf(ch).lzr, mnr, wrd, buf)
	if (ok .ne. 1) goto 20

	w_cdhf_get_word = 1

	return
   1	format(1x,'W_CDHF_GET_WORD: ', a)
 10	continue
	w_cdhf_get_word = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 20	continue
	w_cdhf_get_word = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get word'
	return
	end
!------------------------------------------------------------------------------
! Returns one to N (N=w2-w1+1) bytes of CDHF TM HK data to caller.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_hk(ch,mjr,w1,w2,buf)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_hk_addr_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	w1		! first/lowest hk word number
	integer*4	w2		! second/highest hk word number
	byte		buf(*)		! caller's buffer to receive word(s)
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	integer*4	w_cdhf_word
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	integer*4	mnr
	integer*4	mode
	integer*4	i,j,k,m,n
	record /low_byte/ lb
	integer*4	n_hk
	integer*4	n_good_hk
	integer*4	n_bad_hk
	integer*4	first_err
	integer*4	n_1st_err

	w_cdhf_get_hk = 0

	ok = w_cdhf_set_current_rec(ch,mjr)
	if (ok .ne. 1) goto 10

	n_hk = w2 - w1 + 1
	n_bad_hk  = 0
	n_good_hk = 0
	j = 0
	do i=w1,w2
	   j = j + 1
	   if (i .ge. w_first_core_hk_word .and. 		! reg. hk
	1      i .le. w_last_core_hk_word) then
	      ok = w_cdhf_word(cdhf(ch).lzr,
	1	   hk_mf_addr(i),		! minor frame number
	1	   hk_word_addr(i),		! word number in minor frame
	1	   buf(j))			! returned word
	      if (ok .eq. 1) then
	         n_good_hk = n_good_hk + 1
	      else
	         if (n_bad_hk .eq. 0) then
	            first_err = ok
	            n_1st_err = i
	         end if
	         n_bad_hk  = n_bad_hk + 1
	      end if
	   else if (i .ge. w_first_pktid_hk_word .and. 		! pkt id's
	1           i .le. w_last_pktid_hk_word) then
	      mnr = (i - w_first_pktid_hk_word) * 10
	      mode = cdhf(ch).lzr.h.tm_mode
	      if (mode .eq. science_1x .or. 
	1         mode .eq. science_2x .or.
	1         mode .eq. contingency_1x .or. 
	1         mode .eq. contingency_2x) then
	          k = 24
	      else if (mode .eq. maneuver_1x .or.
	1           mode .eq. maneuver_2x) then
	          k = 32
	      else
	          goto 35
	      end if
	      ok = w_cdhf_word(cdhf(ch).lzr,mnr,k,buf(j))
	      if (ok .eq. 1) then
	         n_good_hk = n_good_hk + 1
	      else
	         if (n_bad_hk .eq. 0) then
	            first_err = ok
	            n_1st_err = i
	         end if
	         n_bad_hk  = n_bad_hk + 1
	      end if
!	      if (ok .ne. 1) goto 38
	   else if (i .ge. w_first_mfq_hk_word .and. 
	1           i .le. w_last_mfq_hk_word) then
	      ! there are 250 4-bit nibbles in the 125 bytes of 
	      ! "minor frame quality hk", so
	      ! here we combine 2 adjacent bytes of quality (as found in the
	      ! cdhf level zero data record header) to adjacent 4-bit nibbles,
	      ! forming one byte of hk information
	      m = (i - w_first_mfq_hk_word) * 2
	      n = cdhf(ch).lzr.h.quality(m)
	      k = cdhf(ch).lzr.h.quality(m+1)
	      k = k * 16 ! shift left 4 bits
	      lb.i4val = k .or. n
	      buf(j) = lb.b
	   else if (i .eq. w_sc_mode_hk_word) then
	      lb.i4val = cdhf(ch).lzr.h.tm_mode
	      buf(j) = lb.b
	   else if (i .eq. w_n_mf_w_fill_hk_word) then
	      lb.i4val = cdhf(ch).lzr.h.n_mf_with_fill
	      buf(j) = lb.b
	   else if (i .eq. w_n_mf_w_synch_err_hk_word) then
	      lb.i4val = cdhf(ch).lzr.h.n_mf_with_synch_err
	      buf(j) = lb.b
	   else
	      goto 13
	   end if
	end do

	if (n_bad_hk .gt. 0 .and. n_hk .gt. 0) then
	   if (n_good_hk .gt. 0) then
	      ok = w_limited_success
	      goto 22
	   else
	      ok = first_err
	      if (n_1st_err .le. w_last_core_hk_word) goto 20
	      goto 38
	   end if
	end if

	w_cdhf_get_hk = 1

	return
   1	format(1x,'W_CDHF_GET_HK: ', a, :, i)
   2	format(1x,'W_CDHF_GET_HK: ', a, i2, a, i2, a)
 10	continue
	w_cdhf_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 13	w_cdhf_get_hk = w_bad_argument_value
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad house keeping index.'
	return
 20	w_cdhf_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get specified core housekeeping word.'
	return
 22	w_cdhf_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,2,iostat=ios) 'acquired ', n_good_hk, ' of ',
	1	w_last_pktid_hk_word+1, ' regular HK words.'
	return
! 30	w_cdhf_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 
!	1 'cannot get sc mode for "packet id" housekeeping word.'
!	return
 35	w_cdhf_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 
	1 'invalid sc mode for "packet id" housekeeping word.', mode
	return
 38	w_cdhf_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 
	1 'cannot get specified "packet id" housekeeping word.'
	return
	end
!------------------------------------------------------------------------------
! Returns the current dpu major frame number from the HK
!------------------------------------------------------------------------------
! From minor frame #44:
!	lw.b =  wind_record.data(18)
!	lw.b1 = wind_record.data(17)
! From minor frame #114:
!	lw.b2 = wind_record.data(17)
!
	integer*4	function	w_cdhf_get_dpu_mf(ch,mjr,mnr,dpu)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	dpu		! caller's buffer for dpu major frame#
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	integer*4	w_cdhf_word
	record /low_byte/ lb
	integer*4	xmjr
	integer*4	ios
	integer*4	add
	integer*4	i
	integer*4	wind_suppress_messages	! a function
	structure /prev_dpu_key/
	   integer*4	dpu		! dpu MF
	   integer*4	mjr		! cdhf lz MF
	   integer*4	fnu		! file number
	   logical*4	err		! error getting this dpu MF already?
	   integer*4	eno		! this was the last error
	end structure
	record /prev_dpu_key/ pdk(max_channels)
	logical*4	first_time /.true./
	logical*4	must_search
	logical*4	success
	integer*4	max_tries
	parameter	(max_tries=7)
	integer*4	n_tries

	w_cdhf_get_dpu_mf = 1

	if (first_time) then
	   first_time = .false.
	   do i=1,max_channels
	      pdk(i).dpu = 0
	      pdk(i).mjr = 0
	      pdk(i).fnu = 0
	      pdk(i).err = .true.
	      pdk(i).eno = 0
	   end do
	end if

	if (pdk(ch).mjr .eq. mjr .and.
	1   pdk(ch).fnu .eq. cdhf(ch).file_number) then
	   if (pdk(ch).err) then
	      dpu = 0
	      w_cdhf_get_dpu_mf = pdk(ch).eno
	      goto 40
	   end if
	   dpu = pdk(ch).dpu
	   return
	end if

	dpu = 0
	lb.i4val = 0
	pdk(ch).dpu = 0
	pdk(ch).mjr = mjr
	pdk(ch).fnu = cdhf(ch).file_number
	pdk(ch).err = .true.

	ok = w_cdhf_set_current_rec(ch,mjr)
	if (ok .ne. 1) goto 10

	if (cdhf(ch).lzr.h.quality(44) .eq. 0 .and.
	1   cdhf(ch).lzr.h.quality(114) .eq. 0) then
	   must_search = .false.
	   success     = .true.
	else
	   ! zero filled minor frame flag true, try adjacent major frame(s)
	   must_search = .true.
	   success     = .false.
	end if

	i = 2
	add = 0
	n_tries = 0
	do while ( must_search )
	   ! attempt to go both hi and low around target MF
	   add = i / 2
	   if (mod(i,2) .eq. 0) add = -add
	   xmjr = mjr + add
!	type *, '...trying', xmjr, ' for', mjr, ',   i,add=', i, add
	   i = i + 1
	   if (xmjr .ge. cdhf(ch).first_major .and.
	1      xmjr .le. cdhf(ch).last_major ) then
	      n_tries = n_tries + 1
	      ok = w_cdhf_set_current_rec(ch,xmjr)
	      if (ok .eq. 1) then
	         if ((cdhf(ch).lzr.h.quality(44)  .eq. 0) .and.
	1            (cdhf(ch).lzr.h.quality(114) .eq. 0)) then
	            ! quality flags suggest good values, search complete
	            must_search = .false.
	            success = .true.
	         end if
	      end if
	   end if
	   !
	   if ((n_tries .ge. max_tries) .or. (i .gt. (max_tries * 2))) then
	      must_search = .false.
	   end if
	end do
	if (.not. success) goto 11

	ok = w_cdhf_word(cdhf(ch).lzr, 44, 18, lb.b)
	if (ok .ne. 1) goto 20

	ok = w_cdhf_word(cdhf(ch).lzr, 44, 17, lb.b1)
	if (ok .ne. 1) goto 20

	ok = w_cdhf_word(cdhf(ch).lzr, 114, 17, lb.b2)
	if (ok .ne. 1) goto 20

	dpu = lb.i4val

	if (add .ne. 0) then
	   dpu = dpu - add
	   ok = w_cdhf_set_current_rec(ch,mjr)
	end if

	pdk(ch).err = .false.
	pdk(ch).dpu = dpu
	pdk(ch).eno = 1

	return
   1	format(1x,'W_CDHF_GET_DPU_MF: ', a, :, i5)
 10	continue
	w_cdhf_get_dpu_mf = ok
	pdk(ch).eno       = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position (entry), ok=', ok
	return
 11	continue
	w_cdhf_get_dpu_mf = w_bad_word_quality_flag
	pdk(ch).eno       = w_bad_word_quality_flag
	! be silent for quality errors in effort for the calling routine
	! to gracefully handle the situation
!	if (ok .eq. w_bad_word_quality_flag) return
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot get HK in adjacent MF, ok=', ok
	return
 20	continue
	w_cdhf_get_dpu_mf = ok
	pdk(ch).eno       = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get specified HK word, ok=', ok
	return
 40	continue
	ok = pdk(ch).eno
	w_cdhf_get_dpu_mf = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'Same error as before, same MF, ok=', ok
	return
	end
!------------------------------------------------------------------------------
! Returns the space craft telemetry mode to the caller
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_mode(ch,mjr,mnr,mode)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	mode		! caller's buffer to receive mode
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	logical*4	wind_suppress_messages	! a function
	integer*4	ios

	ok = w_cdhf_set_current_rec(ch,mjr)
	if (ok .ne. 1) goto 10

	mode = cdhf(ch).lzr.h.tm_mode

	w_cdhf_get_mode = 1

	return
   1	format(1x,'W_CDHF_GET_MODE: ', a)
 10	continue
	w_cdhf_get_mode = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
	end
!------------------------------------------------------------------------------
! Returns the time between succesive minor frames in real*4 seconds for
! entry w_cdhf_frame_rate.
! Entry w_cdhf_tm_speed returns 1 for low bit rate (e.g.: S1x) or 2 for
! high bit rate (e.g.: S2x).
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_frame_rate(ch,mjr,mnr,rate)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	real*4		rate
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	real*4		hi,lo
	parameter	(hi=46.0/250.0)
	parameter	(lo=92.0/250.0)
	integer*4	w_cdhf_tm_speed		! an entry point
	integer*4	speed
	logical*4	caller_wants_rate

	caller_wants_rate = .true.
	goto 1000

	!----------------------------------------------------------------------
	entry	w_cdhf_tm_speed(ch,mjr,speed)
	caller_wants_rate = .false.

 1000	continue
	w_cdhf_frame_rate = 0

	ok = w_cdhf_set_current_rec(ch,mjr)
	if (ok .ne. 1) goto 10

	if (cdhf(ch).lzr.h.tm_mode .eq. science_mode_2 .or.
	1   cdhf(ch).lzr.h.tm_mode .eq. maneuver_mode_2 .or.
	1   cdhf(ch).lzr.h.tm_mode .eq. contingency_mode_2) then
	   if (caller_wants_rate) then
	      rate = hi
	   else
	      speed = 2
	   end if
	else if (
	1   cdhf(ch).lzr.h.tm_mode .eq. science_mode_1 .or.
	1   cdhf(ch).lzr.h.tm_mode .eq. maneuver_mode_1 .or. 
	1   cdhf(ch).lzr.h.tm_mode .eq. contingency_mode_1) then
	   if (caller_wants_rate) then
	      rate = lo
	   else
	      speed = 1
	   end if
	else
	   goto 20
	end if

	w_cdhf_frame_rate = 1

	return
   1	format(1x,'W_CDHF_FRAME_RATE: ', a)
 10	continue
	w_cdhf_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 20	continue
	w_cdhf_frame_rate = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'Troublesome SC mode of ', cdhf(ch).lzr.h.tm_mode
	return
	end
!------------------------------------------------------------------------------
! Gets one packet of data for the caller.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_packet
	1				(ch,mjr,mnr,type,dir,seek_1st,buf)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel number
	integer*4	mjr		! major frame number
	integer*4	mnr		! minor frame number
	integer*4	type		! packet type to seek
	integer*4	dir		! stream direction, forward or backward
	logical*4	seek_1st	! seek a first packet? y/n
	byte		buf(*)		! caller's buffer
	integer*4	ok
	integer*4	w_cdhf_set_current_rec
	logical*4	wind_suppress_messages	! a function
	integer*4	w_cdhf_pckt
	integer*4	ios
	record /low_byte/ id
	logical*4	by_type
	logical*4	found
	integer*4	my_type
	integer*4	pn
	integer*4	check
	integer*4	w_cdhf_next_file	! a function

	w_cdhf_get_packet = 0
	pn = mnr / 10
	by_type = type .ge. min_packet_id .and. type .le. max_packet_id

	ok = w_cdhf_set_current_rec(ch,mjr)

	if (ok .eq. w_end_of_file) then
	   ok = w_cdhf_next_file(ch)
	   if (ok .ne. 1) goto 10
	   mjr = cdhf(ch).first_major
	   mnr = cdhf(ch).first_minor.i4val
	   ok = w_cdhf_set_current_rec(ch,mjr)
	end if
	if (ok .ne. 1) goto 10

	if (.not. by_type) then
	   ! get the indicated packet directly
	   ok = w_cdhf_pckt(cdhf(ch).lzr, pn, buf, size_of_packet)
	   mnr = (10 * pn) + 9
	   if (ok .ne. 1) goto 20
	   w_cdhf_get_packet = 1
	   return
	end if

	! we search sequentially through the file by type
	! (possibly additionally seeking a "first packet" of caller's type)
	! This loop exits when the packet is found or an error condition
	! (such as EOF/BOF) occurs.
	found = .false.
	do while (.not. found)
	   ok = w_cdhf_pckt(cdhf(ch).lzr, pn, id.b, 1)	! get the packet id
	   if (ok .eq. w_end_of_file) then
	      ok = w_cdhf_next_file(ch)
	      if (ok .ne. 1) goto 10
	      mjr = cdhf(ch).first_major
	      mnr = cdhf(ch).first_minor.i4val
	      ok = w_cdhf_set_current_rec(ch,mjr)
	   else if (ok .ne. 1) then
	      goto 20
	   end if

!	   if (ok .ne. 1) goto 20
	   my_type = id.i4val/16	! shift right 4 bits

	   if (my_type .eq. type) then
	      if (seek_1st) then
	         found = (id.b .and. 1) .ne. 0	! check the low bit flg
	      else
	         found = .true.
	      end if
	   else
	      found = .false.
	   end if

	   if (found) then				! gather the packet
	      ok = w_cdhf_pckt(cdhf(ch).lzr, pn, buf, size_of_packet)
	      mnr = (10 * pn) + 9
	      if (ok .ne. 1) goto 20
	   else						! move to next/prev pkt
	      if (dir .eq. forward) then
	         pn = pn + 1
	         if (pn .gt. 24) then
	            pn = 0
	            mjr = mjr + 1
                    mnr = mnr - 240
	            ok = w_cdhf_set_current_rec(ch,mjr)
	            if (ok .ne. 1) goto 10
	         else
	            mnr = mnr + 10
	         end if
	      else if (dir .eq. reverse) then
	         pn = pn-1
	         if (pn .lt. 0) then
	            pn = 24
	            mjr = mjr - 1
	            mnr = 240 + mnr
	            ok = w_cdhf_set_current_rec(ch,mjr)
	            if (ok .ne. 1) goto 10
	         else
	            mnr = mnr - 10
	         end if
	      else
	         goto 30
	      end if
	   end if
	end do
	cdhf(ch).minor_frame.i4val = mnr
	call wind_plain_packet_sum(buf,check)
	if (check .ne. 0) goto 50
	w_cdhf_get_packet = 1
	return
   1	format(1x,'W_CDHF_GET_PACKET: ', a)
   2	format(1x,'W_CDHF_GET_PACKET: ',a, i8,'.',i3.3)
 10	continue
	w_cdhf_get_packet = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot goto stream position'
	return
 20	continue
	w_cdhf_get_packet = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get packet'
	return
 30	continue
	w_cdhf_get_packet = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'direction specified is not forward or backward.'
	return
 50	w_cdhf_get_packet = w_bad_checksum
	if (wind_suppress_messages(ch)) return
	write(6,2,iostat=ios)
	1  'invalid checksum in packet at (major.minor): ', mjr, mnr
	return
	end
!------------------------------------------------------------------------------
! Do CDHF specific channel shut down stuff like closing the open file and more.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_close_stream(ch)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	integer*4	ch

	if (cdhf(ch).lun .ne. 0) then
	   close(cdhf(ch).lun)
	   call lib$free_lun(cdhf(ch).lun)
	end if

	cdhf(ch).ch = 0
	cdhf(ch).lun = 0
	cdhf(ch).lrecl = 0
	cdhf(ch).file = ' '
	cdhf(ch).first_record = 0
	cdhf(ch).last_record  = 0
	cdhf(ch).first_major  = 0
	cdhf(ch).last_major   = 0
	cdhf(ch).transcend_file = .true.

	w_cdhf_close_stream = 1
	return
	end
!------------------------------------------------------------------------------
! Sets the internal file record pointer to passed value.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_set_current_rec(ch,mf_num)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! chanel number
	integer*4	mf_num		! a major frame number
	integer*4	ok
	integer*4	w_cdhf_get_rec
	integer*4	i
	integer*4	ios
	logical*4	wind_suppress_messages	! a function
	integer*4	w_nrt_get_rec
	real*8		t1, t2
	integer*4	w_cdhf_set_file_limits
	integer*4	nrtrecno /0/

	w_cdhf_set_current_rec = 1		! assume succes here

	! convert major frame number to physical record number
	i = mf_num + 1

	if (i .eq. cdhf(ch).recno) return

	if (i .lt. cdhf(ch).first_record) goto 12

	if (cdhf(ch).is_nrt) then
	   if (nrtrecno .eq. 0 .or. mf_num .gt. nrtrecno) then
	      do while(nrtrecno .lt. mf_num)
	         nrtrecno = nrtrecno + 1
	         ok = w_nrt_get_rec(cdhf(ch).lzr)
	         if (ok .eq. w_end_of_file) goto 30
	         if (ok .ne. 1) goto 40
	      end do
	      cdhf(ch).major_frame = mf_num
	      cdhf(ch).first_major = mf_num
	      cdhf(ch).last_major  = mf_num
	      t1 = 0.0
	      t2 = 0.0
	      ok = w_cdhf_set_file_limits(ch,t1,t2)
	      cdhf(ch).last_major  = mf_num + 1
	   end if
	   return
	end if

	if (i .gt. cdhf(ch).last_record) goto 10

	ok = w_cdhf_get_rec(cdhf(ch).lun, i, cdhf(ch).lrecl, cdhf(ch).lzr)
	if (ok .ne. 1) goto 20

	cdhf(ch).recno = i
	cdhf(ch).major_frame = mf_num
	return
   1	format(1x,'W_CDHF_SET_CURRENT_REC: ', a, i)
  10	continue
	w_cdhf_set_current_rec = w_end_of_file
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'MF out of range (eof): ', mf_num
	return
  12	continue
	w_cdhf_set_current_rec = w_beginning_of_file
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'MF out of range (bof): ', mf_num
	return
  20	continue
	w_cdhf_set_current_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get record#: ', i
	return
  30	continue
	w_cdhf_set_current_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) ' end of pass detected.' 
	return
  40	continue
	w_cdhf_set_current_rec = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) ' error reading from NRT CDHF stream.'
	return
	end
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!- Channel Independant and File Related Routines ------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

$IF ABSOFT_FORTRAN
!
! Fix a bug whereby the local variables:
!
!    spwi, mpwi
!
! in function:
!
!    w_cdhf_pckt
! 
! were assumed to persist across calls.
!
! It may be that usage of the "-s" Absoft compiler option will take
! away the requirement for this "block data" routine, and the line:
!
!     common /ext1/ spwi, mpwi 
!
! in the function, below.
!
! Another possible way to get rid of the "block data" is to learn
! the syntax of data-initializing a "structure".
!
        block data save_mnr_wrd

	include		'parm_def.for'

        structure /cdhf_packet_word_indexes/
           integer*4	mnr		! relative minor frame #, 0..9
           integer*4	wrd		! cdhf data index, 0..44 or 0..60
        end structure

	   record /cdhf_packet_word_indexes/ spwi(0:size_of_packet)
	   record /cdhf_packet_word_indexes/ mpwi(0:size_of_packet)
	   common /ext1/ spwi, mpwi
        end
$ENDIF


!------------------------------------------------------------------------------
! Get a packet of data for the caller, channel independant.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_pckt(r,pn,p,sz_buf)
	implicit	none
	include		'parm_def.for'
	include		'wind_cdhf_rec_def.for'
	include		'wind_cdhf_alloc_def.for'
	record /l0_data_record/ r	! r, data record
	integer*4	pn		! r, packet number, 0..24
	byte		p(0:*)		! w, buffer for packet data
	integer*4	sz_buf		! r, size of buffer
	integer*4	i,j,k,m,n
	include		'wind_packet_addresses_def.for'
	integer*4	ios
	structure /cdhf_packet_word_indexes/
	   integer*4	mnr		! relative minor frame #, 0..9
	   integer*4	wrd		! cdhf data index, 0..44 or 0..60
	end structure
	record /cdhf_packet_word_indexes/ spwi(0:size_of_packet)
	record /cdhf_packet_word_indexes/ mpwi(0:size_of_packet)
$IF ABSOFT_FORTRAN
	common /ext1/ spwi, mpwi
$ENDIF
	logical*4	first_time /.true./

	w_cdhf_pckt = 0

	if (first_time) then
	   first_time = .false.
	   ! the science mode indexes
	   k = 1
	   m = 0
	   do i=1,10
	      n = 1
	      do j=1,n_words_frame(i)
	         do while(fixed_col_science_alloc(n) .ne. packet_address(k))
	            n = n + 1
	         end do
	         spwi(k-1).mnr = i - 1
	         spwi(k-1).wrd = n - 1
	         n = n + 1
	         k = k + 1
	      end do
	   end do
	   ! the maneuver mode indexes
	   k = 1
	   m = 0
	   do i=1,10
	      n = 1
	      do j=1,n_words_frame(i)
	         do while(fixed_col_maneuver_alloc(n) .ne. mpacket_address(k))
	            n = n + 1
	         end do
	         mpwi(k-1).mnr = i - 1
	         mpwi(k-1).wrd = n - 1
	         n = n + 1
	         k = k + 1
	      end do
	   end do
	end if

	k = pn * 10
	if (r.h.tm_mode .eq. science_mode_2 .or.
	1   r.h.tm_mode .eq. science_mode_1 .or.
	1   r.h.tm_mode .eq. contingency_mode_1 .or.
	1   r.h.tm_mode .eq. contingency_mode_2) then
	   do i=0,min(sz_buf-1,size_of_packet)
	      j = k + spwi(i).mnr
	      p(i) = r.s(j).tm( spwi(i).wrd )
	   end do
	else if (
	1   r.h.tm_mode .eq. maneuver_mode_1 .or. 
	1   r.h.tm_mode .eq. maneuver_mode_2) then
	   do i=0,min(sz_buf-1,size_of_packet)
	      j = k + mpwi(i).mnr
	      p(i) = r.m(j).tm( mpwi(i).wrd )
	   end do
	else
	   write(6,1,iostat=ios) 'Troublesome SC mode of ', r.h.tm_mode
	   return
	end if

! pre 23-feb-95 way (much slower!)
!	k = pn * 10
!	do i=0,min(sz_buf-1,size_of_packet)
!	   call get_pckt_back_ptr(i,code,mf,ptr)
!	   j = k + mf
!	   call w_cdhf_word(r, j, ptr, p(i))
!	end do

	w_cdhf_pckt = 1
	return
 1	format(1x,'W_CDHF_PCKT: ', a, i3)
	end
!------------------------------------------------------------------------------
! Used to determine index into CDHF record for an individual packet word.
!------------------------------------------------------------------------------
	integer*4	function	get_pckt_back_ptr(addr,mode,mf,bckptr)
	implicit	none
	integer*4	addr		! r, pckt seq addr : {0..430}
	integer*4	mode		! r, sc mode: 1=sci, 2=maneuv
	integer*4	mf		! w, relative mf # of bckptr: {0..9}
	integer*4	bckptr		! w, back pointer
	integer*4	i,j,k
	include		'wind_packet_addresses_def.for'
	integer*4	c_words_frame(n_frames_per_packet) /10*0/
	integer*4	ios

	get_pckt_back_ptr = 0

	if (c_words_frame(1) .eq. 0) then
	   c_words_frame(1) = n_words_frame(1)
	   do i=2,10
	      c_words_frame(i) = c_words_frame(i-1) + n_words_frame(i)
	   end do
	end if

	if (addr.lt.0 .or. addr.gt.430) then
	   write(6,1,iostat=ios) 'Invalid packet address of: ', addr
	   return
	end if

	if (mode .eq. 1) then
	   k = 1
	else if (mode .eq. 2) then
	   k = 432
	else
	   write(6,1,iostat=ios) 'Invalid mode code (not 1 or 2): ', mode
	   return
	end if

	bckptr = p_addrs(k+addr)

	mf = 0
	i = 1
	j = addr + 1
	do while(j .gt. c_words_frame(i))
	   i = i + 1
!	   if (i.gt.10) stop 'fatal indexing error'
	end do
	mf = i - 1

	get_pckt_back_ptr = 1

	return
  1	format(1x,'GET_PCKT_BACK_PTR: ', a, i)
	end
!------------------------------------------------------------------------------
! Retreives specified word from CDHF stream and copies it to caller's buffer.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_word(lzr,mnr,mf_word,out)
	implicit	none
	include		'wind_cdhf_rec_def.for'
	include		'wind_cdhf_alloc_def.for'
	include		'wind_return_code_def.for'
	record /l0_data_record/ lzr	! r, a cdhf record
	integer*4	mnr		! r, minor frame number
	integer*4	mf_word		! r, minor frame word address
	byte		out		! w, user's buffer

	integer*4	lst_mnr_adr
	parameter	(lst_mnr_adr=255)
	integer*4	sci_ptrs(0:lst_mnr_adr)
	integer*4	mvr_ptrs(0:lst_mnr_adr)
	integer*4	i,j
	logical*4	first_time /.true./
	integer*4	ios
	logical*4	wind_suppress_internal_msgs	! a function

	w_cdhf_word = 0

	if (first_time) then
	   do i=0,lst_mnr_adr
	      sci_ptrs(i) = -1
	      mvr_ptrs(i) = -1
	   end do
	   j = 0
	   do i=1,size_science_allocation
	      sci_ptrs(fixed_col_science_alloc(i)) = i - 1
	   end do
	   j = 0
	   do i=1,size_maneuver_allocation
	      mvr_ptrs(fixed_col_maneuver_alloc(i)) = i - 1
	   end do
	end if

	out = 0
	if (mnr .lt. 0 .or. mnr .gt. 249) goto 10
	if (mf_word .lt. 0 .or. mf_word .gt. lst_mnr_adr) goto 10
	if (lzr.h.quality(mnr) .ne. 0) goto 20

	if (lzr.h.tm_mode .eq. science_mode_2 .or.
	1   lzr.h.tm_mode .eq. science_mode_1 .or.
	1   lzr.h.tm_mode .eq. contingency_mode_1 .or.
	1   lzr.h.tm_mode .eq. contingency_mode_2) then
	   j = sci_ptrs(mf_word)
	   if (j.lt.0) goto 10
	   out = lzr.s(mnr).tm(j)
	else if (
	1   lzr.h.tm_mode .eq. maneuver_mode_1 .or. 
	1   lzr.h.tm_mode .eq. maneuver_mode_2) then
	   j = mvr_ptrs(mf_word)
	   if (j.lt.0) goto 10
	   out = lzr.m(mnr).tm(j)
	else
	   goto 10
	end if

	w_cdhf_word = 1
	return
  1	format(1x,'W_CDHF_WORD: ', a, i, i, i)
 10	continue
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios) 'invalid arguments (mode,mf,addres):', 
	1 lzr.h.tm_mode, mnr, mf_word
	return
 20	continue
	w_cdhf_word = w_bad_word_quality_flag
	end
!------------------------------------------------------------------------------
! Reads lrecl bytes from CDHF stream direct access record number recno 
! to buffer r using unit lun.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_rec(lun,recno,lrecl,r)
	implicit	none
	include		'wind_os_def.for'
	include		'wind_cdhf_rec_def.for'
	integer*4	lun		! r, FORTRAN logical unit number
	integer*4	recno		! r, direct access record number
	integer*4	lrecl		! r, logical record length
	record /l0_data_record/ r	! w, record buffer
	integer*4	i,o
	logical*4	wind_suppress_internal_msgs	! a function
	integer*4	w_cdhf_cnvrt_rec		! a function
	integer*4	ios
	integer*4	ok
	w_cdhf_get_rec = 0

	if (lrecl .eq. science_mode_lrecl) then
	   read(lun, rec=recno, iostat=ios) r.science_rec
	else if (lrecl .eq. maneuver_mode_lrecl) then
	   read(lun, rec=recno, iostat=ios) r.maneuver_rec
	else
	   read(lun, rec=recno, iostat=ios) (r.b(i), i=0,lrecl-1)
	end if

	if (ios .ne. 0) goto 10
	w_cdhf_get_rec = 1
	if (sunos) ok = w_cdhf_cnvrt_rec(r.b, recno)
	if (macos_ppc) ok = w_cdhf_cnvrt_rec(r.b, recno)
	return
  1	format(1x,'W_CDHF_GET_REC: ', a, :, i5, ', iostat=', i4,
	1   ' lrecl=',i5)
 10	continue
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=o) 'error reading rec#', recno, ios, lrecl
	if (vms) then
	   if (ios .eq. 22) type 1, '"input record too long"'
	   if (ios .eq. 67)
	1     type 1, '"input statement requires too much data"'
	   if (ios .eq. 22 .or. ios .eq. 67)
	1     type 1, 'CDHF internal recl does not match physical recl'
	end if
	return
	end
!------------------------------------------------------------------------------
! Converts integer*4 values in CDHF record stored as little-end-first values
! into big-end-first values.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_cnvrt_rec(b, recno)
	implicit	none
	byte		b(0:*)
	integer*4	recno
	integer*4	i

	w_cdhf_cnvrt_rec = 0

	if (recno .gt. 1) then
	   do i=0,8,4
	      call w_vxtosuni4( b(i) )
	   end do
	   do i=20,44,4
	      call w_vxtosuni4( b(i) )
	   end do
	else
	   call w_vxtosuni4( b(0) )
	   call w_vxtosuni4( b(4) )
	   do i=12,28,4
	      call w_vxtosuni4( b(i) )
	   end do
	   do i=48,88,4
	      call w_vxtosuni4( b(i) )
	   end do
	   call w_vxtosuni4( b(96) )
	   call w_vxtosuni4( b(176) )
	   call w_vxtosuni4( b(200) )
	   call w_vxtosuni4( b(228) )
	end if

	return
	end
!------------------------------------------------------------------------------
! Opens specified CDHF file, determines the file's record length, and
! copies the header record into the caller's buffer.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_open_file(lun,fname,lrecl,rh)
	implicit	none
	include		'wind_os_def.for'
	include		'wind_cdhf_rec_def.for'
	integer*4	lun		! w, FORTRAN logical unit number
	character*(*)	fname		! r, file name
	integer*4	lrecl		! w, logical record length
	record /l0_data_file_label_h/ rh ! w, buffer to contain header record
	record /l0_data_file_label/ r

	integer*4	i
	integer*4	j
	integer*4	ios, ios2
	integer*4	ok
	integer*4	lib$get_lun
!	logical*4	exist
	logical*4	wind_suppress_internal_msgs	! a function
	integer*4	w_cdhf_cnvrt_rec		! a function

	w_cdhf_open_file = 0

	lun = 0
	ok = lib$get_lun(lun)
	if (ok .ne. 1) goto 10 

	! CDHF files come in two record lengths.
	! First open the file for direct access with the science mode record
	! length, as this is the most common (?), and reopen it with the
	! maneuver mode record length on error.
	! "Regular" Sun FORTRAN requires unformatted record lengths in bytes.
	! VAX VMS FORTRAN requires unformatted record lengths in longwords.
	! Sun FORTRAN programs compiled with the VAX/VMS emulation stuff
	! wants the unformatted record lengths in longwords.
	if (vms) then
	   j = science_mode_lrecl/4		! a vax ism
	   open(lun, 
	2	readonly,
	1	recl=j,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
	   if (ios .eq. 37) then
	      ! inconsistent record length error
	      j = maneuver_mode_lrecl/ 4
	      open(lun, 
	2	readonly,
	1	recl=j,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
	   end if
	   if (ios .ne. 0) goto 40
	   ! read the file label header record and get the physical lrecl
	   read(lun, rec=1, iostat=ios) rh
	   lrecl = rh.lrecl
	   if (ios .ne. 0) goto 30
	   if (lrecl .eq. 0) lrecl = 15552	! may be a near real time file
	   j = j * 4
	   if (j .ne. lrecl) goto 34
	else if (sunos) then
	   j = science_mode_lrecl/4		! a vax ism, use lV77
	   open(lun, 
	1	recl=j,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
	   read(lun, rec=1, iostat=ios) r.fh
	   ok = w_cdhf_cnvrt_rec(r.b, 1)
	   if (ios .ne. 0) goto 30
	   rh = r.fh
	   lrecl = r.fh.lrecl
	   if (lrecl .eq. 0) lrecl = 15552
           if (lrecl .ne. science_mode_lrecl) then
	      ! reopen the file with the maneuver mode record length
	      close(lun)
	      open(lun, 
	1	recl=lrecl/4,	! on Sun, must use libV77 for this to work
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
	      if (ios .ne. 0) goto 40
	   end if
	else if (macos_ppc) then
!
! With ABSOFT Fortran on the Mac, the default record length
! specification for "read" is in bytes, not 4-byte words.
!
!	   j = science_mode_lrecl/4		! a vax ism, use lV77
!
	   j = science_mode_lrecl
	   open(lun, 
	1	recl=j,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
!
! With ABSOFT Fortran on the Mac, "read" doesn't work correctly if
! the output directive is to "r.fh".  So read into "r.b".
!
! 	   read(lun, rec=1, iostat=ios) r.fh
!
 	   read(lun, rec=1, iostat=ios) r.b(0:11551)
	   ok = w_cdhf_cnvrt_rec(r.b, 1)
	   if (ios .ne. 0) goto 30
	   rh = r.fh
	   lrecl = r.fh.lrecl
	   if (lrecl .eq. 0) lrecl = 15552
           if (lrecl .ne. science_mode_lrecl) then
	      ! reopen the file with the maneuver mode record length
	      close(lun)
	      open(lun, 
!
! With ABSOFT Fortran on the Mac, the default record length
! specification for "read" is in bytes, not 4-byte words.
!
!	1	recl=lrecl/4,	! on Sun, must use libV77 for this to work
!
	1	recl=lrecl,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
	      if (ios .ne. 0) goto 40
	   end if
	else if (macos_intel) then
!
! With ABSOFT Fortran on the Mac, the default record length
! specification for "read" is in bytes, not 4-byte words.
!
!	   j = science_mode_lrecl/4		! a vax ism, use lV77
!
	   j = science_mode_lrecl
	   open(lun, 
	1	recl=j,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
!
! With ABSOFT Fortran on the Mac, "read" doesn't work correctly if
! the output directive is to "r.fh".  So read into "r.b".
!
! 	   read(lun, rec=1, iostat=ios) r.fh
!
 	   read(lun, rec=1, iostat=ios) r.b(0:11551)
!
! Intel Mac is little-endian, so we do NOT call "w_cdf_cnvrt_rec".
!
!	   ok = w_cdhf_cnvrt_rec(r.b, 1)
!
	   if (ios .ne. 0) goto 30
	   rh = r.fh
	   lrecl = r.fh.lrecl
	   if (lrecl .eq. 0) lrecl = 15552
           if (lrecl .ne. science_mode_lrecl) then
	      ! reopen the file with the maneuver mode record length
	      close(lun)
	      open(lun, 
!
! With ABSOFT Fortran on the Mac, the default record length
! specification for "read" is in bytes, not 4-byte words.
!
!	1	recl=lrecl/4,	! on Sun, must use libV77 for this to work
!
	1	recl=lrecl,
	1	file=fname,
	1	access='direct',
	1	form='unformatted',
	1	status='old',
	1	iostat=ios)
	      if (ios .ne. 0) goto 40
	   end if
	end if

	w_cdhf_open_file = 1
	return
   1	format(1x,'W_CDHF_OPEN_FILE: ', a, :, i4, :, i8)
   2	format(1x,'W_CDHF_OPEN_FILE: ', a, z8.8)
   3	format(1x,'W_CDHF_OPEN_FILE: ', a, a40)
   4	format(1x,'W_CDHF_OPEN_FILE: internal record length (', i5, 
	1 ') does not match', /,
	1      1x,'                  physical record length (', i5, ')')
  10	continue
	if (wind_suppress_internal_msgs()) return
	write(6,2,iostat=ios) 'Cannot allocate a LUN, ok=', ok
	lun = 0
	return
!  20	continue
!	if (wind_suppress_internal_msgs()) then
!	   call lib$free_lun(lun)
!	   lun = 0
!	   return
!	end if
!	inquire(file=fname, exist=exist)
!	if (.not. exist) then
!	   write(6,1,iostat=ios2) 'Nonexistant file.'
!	else
!	   write(6,1,iostat=ios2) 'Error opening file, iostat=', ios
!	end if
!	write(6,3,iostat=ios) 'file: ', fname
!	call lib$free_lun(lun)
!	lun = 0
!	return
  30	continue
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios2) 
	1 'Error reading first direct access record, ios=',ios
	return
  34	continue
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	if (wind_suppress_internal_msgs()) return
	write(6,4,iostat=ios2) rh.lrecl, j
	return
  40	continue
	call lib$free_lun(lun)
	lun = 0
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios2) 
	1 'Unable to open file for direct access, iostat=', ios, j
	if (unixos .and. (ios .eq. 30)) then
	   write(6,1,iostat=ios2) 'File protection violation'
	end if
	return
	end
!------------------------------------------------------------------------------
! Writes to standard output a formatted table describing the CDHF Level-Zero
! File Label Record passed as an argument.
!------------------------------------------------------------------------------
	integer*4	function	show_cdhf_file_label_record(r)
	implicit	none
	include		'wind_cdhf_rec_def.for'
	record /l0_data_file_label_h/ r
	integer*4	i,j
	integer*4	ios
!	character*24	atc_to_str

	show_cdhf_file_label_record = 1

	write(6,1,iostat=ios) ' '
	!                      12345678901234567890123456789012345
	write(6,1,iostat=ios) 'CDHF Level-Zero File Label Record:'
	write(6,1,iostat=ios) ' '
	write(6,1,iostat=ios) 'Spacecraft ID:....................',
	1    r.spacecraft_id
	write(6,1,iostat=ios) 'Instrument Number:................',
	1    r.instrument_number
	write(6,3,iostat=ios) 'Instrument Name:..................',
	1    r.instrument_name
	write(6,1,iostat=ios) 'Physical Record Number:...........',
	1    r.physical_record_number
	write(6,1,iostat=ios) 'Recs per Major Frame:.............',
	1    r.recs_per_major_frame
	write(6,1,iostat=ios) 'Number of Records in File:........',
	1    r.max_recs_in_file
	write(6,1,iostat=ios) '8-bit First Major Frame #:........',
	1    r.major_frame_count_first
	write(6,1,iostat=ios) '8-bit Last Major Frame #:.........',
	1    r.major_frame_count_last

	write(6,2,iostat=ios) 'SC Clock at first MF:',
	1			(r.sc_clock_first.b(i), i=1,8)
	write(6,2,iostat=ios) 'SC Clock at last MF:',
	1			(r.sc_clock_last.b(i), i=1,8)

	write(6,1,iostat=ios) 'ATC:year  first MF:...............',
	1    r.atc_year_first
	write(6,1,iostat=ios) 'ATC:day   first MF:...............',
	1     r.atc_day_first
	write(6,1,iostat=ios) 'ATC:msec  first MF:...............',
	1    r.atc_msec_first
	write(6,1,iostat=ios) 'ATC:micro first MF:...............',
	1    r.atc_micro_first
!	write(6,3,iostat=ios) 'ATC nice  first MF:...............',
!	1 atc_to_str(r.atc_year_first, 
!	1            r.atc_day_first,
!	1            r.atc_msec_first,
!	1            r.atc_micro_first)
	write(6,1,iostat=ios) 'ATC:year  last MF:................',
	1    r.atc_year_last
	write(6,1,iostat=ios) 'ATC:day   last MF:................',
	1    r.atc_day_last
	write(6,1,iostat=ios) 'ATC:msec  last MF:................',
	1    r.atc_msec_last
	write(6,1,iostat=ios) 'ATC:micro last MF:................',
	1    r.atc_micro_last
!	write(6,3,iostat=ios) 'ATC nice  last MF:................',
!	1 atc_to_str(r.atc_year_last, 
!	1            r.atc_day_last,
!	1            r.atc_msec_last,
!	1            r.atc_micro_last)

	write(6,1,iostat=ios) 'Expected # of MF:.................',
	1    r.n_major_expected
	write(6,1,iostat=ios) 'Actual # of MF:...................',
	1    r.n_major_in_file
	write(6,1,iostat=ios) 'Number of Gaps:...................',
	1    r.n_major_gaps
	write(6,3,iostat=ios) 'Data Coverage Type:...............',
	1    r.data_coverage_type

	write(6,1,iostat=ios) 'Decom Rerun Number:...............',
	1    r.decom_rerun_number
	write(6,3,iostat=ios) 'Decom Prog Ver #:.................',
	1    r.decom_prog_version
	write(6,3,iostat=ios) 'Decom Database Ver #:.............',
	1    r.decom_db_version
	write(6,3,iostat=ios) 'Decom Run Time:...................',
	1    r.decom_run_time
	write(6,5,iostat=ios) 'Instrument Filename:..............',
	1    r.instrument_filename

	write(6,1,iostat=ios) 'Physical Record Length:...........',
	1    r.lrecl
	write(6,4,iostat=ios) 'Spares:', (r.spares(i:i), i=1,20)

	write(6,1,iostat=ios) 'Merge Rerun Number:...............',
	1    r.merge_rerun_number
	write(6,3,iostat=ios) 'Merge Prog Ver #:.................',
	1    r.merge_prog_version
	write(6,3,iostat=ios) 'Merge Run Time:...................',
	1    r.merge_run_time

	write(6,1,iostat=ios) 'Number of Edit Files:', r.n_edit_files
	j = min(20,r.n_edit_files)
	j = max(j,1)
	do i=1,j
	   write(6,6,iostat=ios) i, 'Edit Filename:', r.ef(i).edit_filename
	   write(6,7,iostat=ios) i, 'Edit Key', r.ef(i).edit_key
	   write(6,8,iostat=ios) i, 'Edit Rerun #', r.ef(i).edit_rerun_number
	   write(6,7,iostat=ios) i, 'Edit Prog Ver #',
	1        r.ef(i).edit_prog_version
	   write(6,7,iostat=ios) i, 'Edit Run Time:',
	1        r.ef(i).edit_run_time
	   write(6,7,iostat=ios) i, 'Edit Data Type:',
	1        r.ef(i).edit_data_type
	   write(6,7,iostat=ios) i, 'Edit Message Key:',
	1        r.ef(i).edit_message_key
	end do

	return
  1	format(1x,a,t40,i)
  2	format(1x,a,t40,8(z2.2,1x))
  3	format(1x,a,t40,a)
  4	format(1x,a,4x,20(z2.2,1x))
  5	format(1x,a, t35, a)
  6	format(1x,'(',i2,')', 1x, a, t35, a)
  7	format(1x,'(',i2,')', 1x, a, t40, '"',a,'"')
  8	format(1x,'(',i2,')', 1x, a, t40, i)
	end
!------------------------------------------------------------------------------
! Writes to standard output a formatted table describing the CDHF data record
! header of the level zero data record passed as an argument.
!------------------------------------------------------------------------------
	integer*4	function	show_cdhf_data_record_header(r)
	implicit	none
	include		'wind_cdhf_rec_def.for'
	record /l0_data_header/ r
	integer*4	i,j
	integer*4	ios
!	character*24	atc_to_str

	show_cdhf_data_record_header = 1

	write(6,1,iostat=ios) 'Instrument Number:................',
	1   r.instrument_number
	write(6,1,iostat=ios) 'Physical Record Number:...........',
	1    r.physical_record_number
	write(6,1,iostat=ios) '8-bit Major Frame #:..............',
	1    r.major_frame

	write(6,2,iostat=ios) 'SC Clock at first MF:',
	1			(r.sc_clock.b(i), i=1,8)

	write(6,1,iostat=ios) 'ATC:year  MF:....................',
	1    r.atc_year
	write(6,1,iostat=ios) 'ATC:day   MF:....................',
	1    r.atc_day
	write(6,1,iostat=ios) 'ATC:msec  MF:....................',
	1    r.atc_msec
	write(6,1,iostat=ios) 'ATC:micro MF:....................',
	1    r.atc_micro
!	write(6,5,iostat=ios) 'ATC nice  MF:....................',
!	1 atc_to_str(r.atc_year, 
!	1            r.atc_day,
!	1            r.atc_msec,
!	1            r.atc_micro)

	write(6,1,iostat=ios) '# minor frames w/fill:............',
	1    r.n_mf_with_fill
	write(6,1,iostat=ios) '# minor frames w/synch err:.......',
	1    r.n_mf_with_synch_err
	write(6,1,iostat=ios) 'TM Mode:..........................',
	1    r.tm_mode
	write(6,3,iostat=ios) 'mf Quality:'
	do j=0,238,20
	   write(6,4,iostat=ios) j, (r.quality(i), i=j,j+19)
	end do
	write(6,4,iostat=ios) 240, (r.quality(i), i=240,249)

	return
  1	format(1x,a,t40,i)
  2	format(1x,a,t40,8(z2.2,1x))
  3	format(1x,a)
  4	format(1x, i3,':', 10(1x,z2.2), 4x, 10(1x,z2.2))
  5	format(1x,a,t40,a)
	end
!------------------------------------------------------------------------------
! works only with one-channel access..
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_next_file(ch)
	implicit	none
	integer*4	ch
	character*256	f
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_return_code_def.for'
	integer*4	ok, o
!	integer*4	i,j
!	integer*4	ymd,hms,msec
!	integer*4	ur8_to_dbms
!	integer*4	dbms_to_ur8
	integer*4	w_channel_reopen
	real*8		r8
	integer*4	w_cdhf_goto_file		! an entry point
	real*8		nt				! new time, ur8 format
	logical*4	use_next_seq_file
	integer*4	w_ur8_to_x_filename		! a function

	w_cdhf_next_file = 1
	use_next_seq_file = .true.
	goto 1000

	!----------------------------------------------------------------------
	entry	w_cdhf_goto_file(ch,nt)
	r8 = nt
	use_next_seq_file = .false.
	goto 1000

 1000	continue
	if (.not. cdhf(ch).transcend_file) then
	   w_cdhf_next_file = w_end_of_file
	   return
	end if

!	f = cdhf(ch).file
!	if (user(ch).stream_type .eq. cdhf_lz_stream) then
!xxxxxx adapt for CD cdhf files...
!	   ! EDR's from CD-ROM:  yymmddvv.dat 
!	   i = index(f,'.dat')
!	else
!	   i = index(f,'wi_lz_wav_')
!	   if (i .eq. 0) goto 10
!	   j = i + 10
!	   read(f(j:j+7),2,iostat=o,err=20) ymd
!	end if

!	if (use_next_seq_file) then
!	   hms = 121212
!	   msec = 1234
!	   ok = dbms_to_ur8(ymd,hms,msec,r8)
!	   if (ok .ne. 1) goto 30
!	   r8 = r8 + 1.0
!	end if

!	ok = ur8_to_dbms(r8,ymd,hms,msec)
!	if (ok .ne. 1) goto 40

	if (use_next_seq_file) then
	   r8 = ( (cdhf(ch).t2 - cdhf(ch).t1) / 2.0 ) + 1.0
	end if

	ok = w_ur8_to_x_filename(r8, user(ch).stream_type, f)
	if (ok .ne. 1) goto 70

!	write(f(j:j+7),2,iostat=o,err=50) ymd

!	f(j+8:len(f)) = '_v*.dat '
	if (cdhf(ch).verbose_rollover) then
	   type *, 'Switching to: ', f(1:60)
	   type *, 't1,t2,nt=', cdhf(ch).t1, cdhf(ch).t2
	end if

	ok = w_channel_reopen(ch,f)
	if (ok .ne. 1) goto 60

  2	format(i8.8)
	return
  1	format(1x,'W_CDHF_NEXT_FILE: ',a)
  3	format(1x,'W_CDHF_NEXT_FILE: cannot get file at ur8 ',f6.1,
	1    ', type ',i)
! 10	continue
!	w_cdhf_next_file = 0
!	write(6,1,iostat=o) 'cannot index "wi_lz_wav_" in current file.'
!	return
! 20	continue
!	w_cdhf_next_file = 0
!	write(6,1,iostat=o) 'cannot read YYYYMMDD from current file.'
!	return
! 30	continue
!	w_cdhf_next_file = 0
!	write(6,1,iostat=o) 'cannot convert dbms to ur8 format.'
!	return
! 40	continue
!	w_cdhf_next_file = 0
!	write(6,1,iostat=o) 'cannot convert ur8 to dbms format.'
!	return
! 50	continue
!	w_cdhf_next_file = 0
!	write(6,1,iostat=o) 'cannot write YYYYMMDD to new file name.'
!	return
 60	continue
	w_cdhf_next_file = 0
	write(6,1,iostat=o) 'cannot REopen channel with new file.'
	return
 70	continue
	w_cdhf_next_file = 0
	write(6,3,iostat=o) r8, user(ch).stream_type
	return
	end
!------------------------------------------------------------------------------
! The integer portion of t1 must associate with the current cdhf file or
! less than or equal to zero.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_set_file_limits(ch,t1,t2)
	implicit	none
	integer*4	ch
	real*8		t1,t2			! ur8 times, t1 < t2
	include		'wind_os_def.for'
	include		'parm_def.for'
 	include		'low_byte_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	integer*4	ok
!	integer*4	atc_to_ur8
!	integer*4	w_cdhf_tm_speed
	integer*4	w_cdhf_set_current_rec
	integer*4	w_cdhf_get_rec_by_time
	integer*4	w_cdhf_scet_ur8_of_mfmf
	integer*4	w_cdhf_scet_ur8_of_current
	integer*4	i
!	integer*4	j
	real*8		a
!	real*8		b,c,d
	logical*4	set_lower_limit /.false./
	logical*4	set_upper_limit /.false./
	integer*4	mjr, mnr
	integer*4	ios
	integer*4	dummy

	w_cdhf_set_file_limits = 0

	! It is assumed that this routine is called from w_cdhf_setup_stream
	! after the file is opened and the physical record limits determined.

	if (cdhf(ch).is_nrt) then
	   ! start and end times are from the current record
	   ! get and set the lower physical limit for ur8 time
	   mjr = cdhf(ch).first_major
	   ok = w_cdhf_scet_ur8_of_current(ch, mjr, 0, cdhf(ch).t1)
	   if (ok .ne. 1) goto 10

	   ! get and set the upper physical limit for ur8 time
	   mjr = cdhf(ch).last_major
	   ok = w_cdhf_scet_ur8_of_current(ch, mjr, 249, cdhf(ch).t2)
	   if (ok .ne. 1) goto 20
	else
	   ! get and set the lower physical limit for ur8 time
	   mjr = cdhf(ch).first_major
	   ok = w_cdhf_scet_ur8_of_mfmf(ch, mjr, 0, cdhf(ch).t1)
	   if (ok .ne. 1) goto 10

	   ! get and set the upper physical limit for ur8 time
	   mjr = cdhf(ch).last_major
	   ok = w_cdhf_scet_ur8_of_mfmf(ch, mjr, 249, cdhf(ch).t2)
	   if (ok .ne. 1) goto 20
	end if

	exi(ch).stream_bof = cdhf(ch).t1
	exi(ch).stream_eof = cdhf(ch).t2
	! determine limits of hypothetical "full" day
	i = exi(ch).stream_bof
	exi(ch).stream_bod = i
	exi(ch).stream_eod = exi(ch).stream_bod + 0.9999999999d0

	! Reset this file's lower limit for MF, recno, and ur8
	! (rounding down to the beginning of the current major frame)
	! if so specified by the caller.
	if (t1 .gt. 0.0) then
	   ! caller has specified a particular file by time
	   i = t1
	   a = i
	   if (t1 .gt. a) then
	      ! caller has specified other than BOF
	      ok = w_cdhf_get_rec_by_time(ch,mjr,mnr,t1,.false.,dummy)
	      if (ok .ne. 1) goto 30
	      ok = w_cdhf_scet_ur8_of_mfmf(ch, mjr, 0, cdhf(ch).t1)
	      if (ok .ne. 1) goto 40
	      ok = w_cdhf_set_current_rec(ch, mjr)
	      if (ok .ne. 1) goto 42
 	      cdhf(ch).first_major = mjr
 	      cdhf(ch).first_record = cdhf(ch).recno
	   end if
	end if

	exi(ch).stream_boi = cdhf(ch).t1
	if (exi(ch).stream_bow .le. 0.0) then
	   exi(ch).stream_bow = exi(ch).stream_boi
	end if

	! Reset this file's upper limit for MF, recno, and ur8
	! (rounding to the end of the current MF for mf)
	! if so specified by the caller.
	if (t2 .gt. 0.0 .and. t2 .gt. t1) then
	   ok = w_cdhf_get_rec_by_time(ch,mjr,mnr,t2,.false.,dummy)
	   if (ok .eq. 1) then
	      ! do nothing
	   else if (ok .eq. w_end_of_file) then
	      mjr = cdhf(ch).last_major
	   else
	      goto 50
	   end if
	   ok = w_cdhf_scet_ur8_of_mfmf(ch, mjr, 249, cdhf(ch).t2)
	   if (ok .ne. 1) goto 60
	   ok = w_cdhf_set_current_rec(ch, mjr)
	   if (ok .ne. 1) goto 62
 	   cdhf(ch).last_major = mjr
 	   cdhf(ch).last_record = cdhf(ch).recno
	end if

	exi(ch).stream_eoi = cdhf(ch).t2
	if (exi(ch).stream_eow .le. 0.0) then
	   exi(ch).stream_eow = exi(ch).stream_eoi
	end if

!	type *, '...t1,t2=', t1, t2
!	type *, '...MF1,rec#1', cdhf(ch).first_major, cdhf(ch).first_record
!	type *, '...MF2,rec#2', cdhf(ch).last_major, cdhf(ch).last_record


!	ok = w_cdhf_set_current_rec(ch, mjr)
!	if (ok .ne. 1) goto 32
!	cdhf(ch).last_major = mjr
!	cdhf(ch).last_record = cdhf(ch).recno
!	ok = atc_to_ur8(
!	1	cdhf(ch).lzr.h.atc_year,
!	1	cdhf(ch).lzr.h.atc_day,
!	1	cdhf(ch).lzr.h.atc_msec,
!	1	cdhf(ch).t2)
!	if (ok .ne. 1) goto 40
!	ok = w_cdhf_tm_speed(ch, mjr, speed)
!	if (ok .ne. 1) goto 60
!	if (speed .eq. 1) then
!	   cdhf(ch).t2 = cdhf(ch).t2 + (249.0 * ur8_mfdts)
!	else
!	   cdhf(ch).t2 = cdhf(ch).t2 + (249.0 * ur8_mfdtf)
!	end if

	w_cdhf_set_file_limits =1
	if (t1 .le. 0.0 .and. t2 .le. 0.0) return

	!xxxxxxxx
!	xxxxx must preserve t1,t2 over file switches in some cases
!	xxxxx so, need to add t1,t2 args to w_channel_reopen
!	xxxxx must watch for reopens on current file when what we really
!	xxxxx   want to do is change the file limits. -- Maybe reopen
!	xxxxx   should be clever enough to trap for same file name diff limits?
	! case a: file rollover true, t1,t2 apply only to current file
	! case b: file rollover true, t1,t2 span multiple files
	! case c: file rollover false, t1,t2 within current file

!	if (set_lower_limit) then
!	end if

!	if (set_upper_limit) then
!	end if

	return
  1	format(1x,'W_CDHF_SET_FILE_LIMITS: ', a)
 10	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot get physical UR8 t1'
	return
 20	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot get physical UR8 t2'
	return
 30	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot position to logical UR8 t1'
	return
 40	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot get logical UR8 t1'
	return
 42	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot set rec to UR8 t1'
	return
 50	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot position to logical UR8 t2'
	return
 60	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot get logical UR8 t2'
	return
 62	continue
	w_cdhf_set_file_limits = ok
	write(6,1,iostat=ios) 'cannot set rec to UR8 t2'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_setup_nrt(ch,f)
	implicit	none
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_cdhf_def.for'
	include		'c_string_def.for'
	integer*4	ch
	character*(*)	f
	integer*4	ok
	integer*4	w_nrt_open
	logical*4	first_time /.true./
	record /c_string_256/ c
	integer*4	k3len
	integer*4	k

	w_cdhf_setup_nrt = 0
	c.c = f
	f = 'nrt'
	k = k3len(c.c) + 1
	if (k .gt. len(c.c)) k = len(c.c)
	c.b(k) = 0
	ok = w_nrt_open(c.b)
	if (ok .ne. 1) return

	cdhf(ch).is_nrt = .true.
	cdhf(ch).lrecl  = 15552
	cdhf(ch).hdr.max_recs_in_file = 2
	cdhf(ch).hdr.n_major_in_file  = 1

	w_cdhf_setup_nrt = 1
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_mfmf(ch,major,minor,tk)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_cdhf_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	tk
	integer*4	ios
	logical*4	wind_suppress_messages

	w_cdhf_get_mfmf = 1
	major = 0
	minor = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	if (tk .eq. w_tk_stream_mfmf) then
	   ! the current record's maj/min value in the file
	   major = cdhf(ch).major_frame
	   minor = cdhf(ch).minor_frame.i4val
	else if (tk .eq. w_tk_next_mfmf) then
	   major = cdhf(ch).major_frame
	   minor = cdhf(ch).minor_frame.i4val
	   call wind_tm_increment_mfmf(major,minor)
	   if (major .gt. cdhf(ch).last_major) then
	      call wind_tm_decrement_mfmf(major,minor)
	   end if
	else if (tk .eq. w_tk_earliest_mfmf) then
	   major = cdhf(ch).first_major
	   minor = cdhf(ch).first_minor.i4val
	else if (tk .eq. w_tk_latest_mfmf) then
	   major = cdhf(ch).last_major
	   minor = cdhf(ch).last_minor.i4val
	else if (tk .eq. w_tk_current_mfmf) then
	   major = cdhf(ch).major_frame
	   minor = cdhf(ch).minor_frame.i4val
	else
	   goto 40
	end if

	return
  1	format(1x,'W_CDHF_GET_MFMF: ',a,i)
  8	w_cdhf_get_mfmf = w_bad_channel
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'invalid channel number=', ch
	return
 40	w_cdhf_get_mfmf = 0
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'invalid MF.mf position token.', tk
	return
	end
