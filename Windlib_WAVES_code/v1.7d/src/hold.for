! hold.for - a place to hold routines that are no longer used by wind_lib
! but JK wants to save for a while just in case...

!------------------------------------------------------------------------------
	integer*4	function	wind_get_hk_or_adjacent
	1				(ch,major,hkindex,hkdatum,diff)
!
! Attempts to return the house keeping word indicated by the users calling
! arguments of major frame number and house keeping word number.
! If this word is not available due to EOF, BOF, or other error condition
! then an adjacent major frame is accessed in attempt to secure the
! specified word.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! user's channel number
	integer*4	major			! major frame number
	integer*4	hkindex			! house keeping word number
	integer*4	hkdatum			! hk word returned
	integer*4	diff			! num MF +- from original pos.
	integer*4	ok
	integer*4	mjr
	integer*4	err_count
	integer*4	max_errors /1/
	integer*4	wind_tm_get_hk

	wind_get_hk_or_adjacent = 0
	diff = 0
	err_count = 0
	mjr = major

	do while(err_count .le. max_errors)
	   ok = wind_tm_get_hk(ch,mjr,hkindex,hkdatum)
	   if (ok .eq. 1) then
	      wind_get_hk_or_adjacent = 1
	      return
	   end if

	   ! try to get the word from an adjacent major frame
	   ! (this is usually for HK words like version numbers)

	   if (ok .eq. w_end_of_file) then
	      mjr = user(ch).last_major - 1
	      err_count = err_count + 1
	      diff = major - mjr
	   else if (ok .eq. w_beginning_of_file) then
	      mjr = user(ch).first_major + 1
	      err_count = err_count + 1
	      diff = mjr - major
	   else
	      ! try the next major frame
	      mjr = major + 1
	      diff = diff + 1
	      err_count = err_count + 1
	      if (major .le. user(ch).last_major) err_count = 1
	   end if
	end do

	return
	end

!------------------------------------------------------------------------------
! Writes to the caller's standard output a formatted represention of the
! wind_lib internal item structure passed as the sole argument.
!------------------------------------------------------------------------------
	integer*4	function	show_item_structure(it)
	include		'dbms_item_def.for'
	record		/dbms_item/ it

	type *, 'Event Name : ', it.packet_name
	type *, 'Item Name  : ', it.item_name
	type *, 'Record Type: ', it.record_type
	type *, 'Area       : ', it.c_area
	type *, 'Valid Start: ', it.validity_start_date(1),
	1	it.validity_start_date(2)
	type *, 'Valid Stop : ', it.validity_stop_date(1),
	1	it.validity_stop_date(2)
	type 4, 'Subtype    : ', it.subtype
	type *, 'DPUv start : ', it.dpu_version_start
	type *, 'DPUv stop  : ', it.dpu_version_stop
	type *, 'FFTv start : ', it.fft_version_start
	type *, 'FFTv stop  : ', it.fft_version_stop
	type *, 'TDSv start : ', it.tds_version_start
	type *, 'TDSv stop  : ', it.tds_version_stop
	type *, 'Validation : ', it.validation
	type *, ' v1,v2,op  : ', it.val1, it.val2, it.op
	type *, 'Chain      : ', it.chain
	type *, 'Count      : ', it.count
	type *, 'sb,ln,of,rp: ', it.startbit, it.length, it.offset, it.rep
	type *, 'vc,v1,v2,v3: ', it.value_count, it.values(1),
	1	it.values(2), it.values(3)
	type *, 'Function   : ', it.function_number
	type *, 'File       : ', it.myfile(:60)

	show_item_structure = 1
	return
 4	format(1x,a, z8.8)
	end


!------------------------------------------------------------------------------
! Writes to the caller's standard output a formatted table of the search
! arguments currently maintained internally by wind_lib to query the
! TM item extraction database.
!------------------------------------------------------------------------------
	integer*4	function	w_show_dbms_search_args(ch,item_name)
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	include		'wind_tm_event_def.for'
	integer*4	ch		! read, channel number
	character*(*)	item_name

	type *, 'Item Database Search Arguments:'
	type *, '-------------------------------'
	type *, '        Event: ', eei(ch).event_type
	type *, '         Item: ', item_name
	type *, '      Subtype: ', eei(ch).event_subtype
	type *, '  VersionMask: ', eb(ch).fsw_versions(1)
	type *, '  DPU Version: ', eb(ch).fsw_versions(2)
	type *, '  Ins Version: ', eb(ch).fsw_versions(3)
	type *, 'SCET yyyymmdd: ', eb(ch).scet(1)
	type *, '  SCET hhmmss: ', eb(ch).scet(2)
	type *, 'DBMS yyyymmdd: ', eei(ch).dbms_date_time(1)
	type *, '  DBMS hhmmss: ', eei(ch).dbms_date_time(2)
	return
	end

!------------------------------------------------------------------------------
! This routine retrieves a wind_lib format internal record from the ch's
! CDHF file based on time.  For now the algorithm is very simple.  It assumes
! no time gaps in the file.  It positions the caller at the beginning of the
! major frame in which the caller's time occurs.  It also uses ACT values for
! times because I've not been able use the PB5 times yet.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_get_rec_by_time_old
	1			(ch,mjr,mnr,user_time,by_ert,cpy_to_buf,wr)
	implicit	none
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_return_code_def.for'
	include		'wind_cdhf_def.for'
	include		'vms_time_constants_def.for'
	include		'ui8_def.for'
	integer*4	ch
	integer*4	mjr
	integer*4	mnr
	character*(*)	user_time	! 23/24 character ERT or SCET
	logical*4	by_ert		! flag for search by ERT or SCET
	logical*4	cpy_to_buf	! 
	record /wind_record/ wr		! wind_tm_lib internal buffer

	record /ui8/	ut, xt, rt	! ut=user's time, xt=whatever
	record /ui8/	t1, t2, t3	! 64bit system time
	record /ui8/	dt, nr		! dt=delta_time, nr=number_of_records
	record /ui8/	remainder
	integer*4	ok
	integer*4	ok2
	integer*4	ios
	integer*4	j,k
	character*24	ct1, ct2, ct3
	character*24	atc_to_str2			! a function
	integer*4	sys_bintim			! a function
	integer*4	vxtosuni4			! a function
	integer*4	suntovxi4			! a function
	integer*4	mf
	integer*4	w_cdhf_set_current_rec		! a function
	integer*4	w_cdhf_get_rec_xc		! a function
	logical*4	wind_suppress_messages
	integer*4	w_cdhf_binary_search
	integer*4	w_cdhf_tm_speed
	integer*4	tm_speed
	logical*4	found
	integer*4	ui8_is_eq2
	external        ui8_is_eq2              !$pragma C ( ui8_is_eq2 )
	integer*4	ui8_divide
	external        ui8_divide              !$pragma C ( ui8_divide )
	integer*4	ui8_sub
	external        ui8_sub		        !$pragma C ( ui8_sub )
	integer*4	ui8_add
	external        ui8_add		        !$pragma C ( ui8_add )
	external        ui8_cpy		        !$pragma C ( ui8_cpy )

	w_cdhf_get_rec_by_time_old = 0

!	if (cdhf(ch).get_time_search_limits) then
!	   cdhf(ch).get_time_search_limits = .false.
!
!	   ! convert the file's start and stop times to "generic" format
!	   ct1 = atc_to_str2(cdhf(ch).hdr.atc_year_first,
!	1                 cdhf(ch).hdr.atc_day_first,
!	1                 cdhf(ch).hdr.atc_msec_first)
!	   ok = sys_bintim(ct1, cdhf(ch).t1)
!	   if (ok .ne. 1) goto 10
!
!	   ct2 = atc_to_str2(cdhf(ch).hdr.atc_year_last,
!	1                 cdhf(ch).hdr.atc_day_last,
!	1                 cdhf(ch).hdr.atc_msec_last)
!	   ok = sys_bintim(ct2, cdhf(ch).t2)
!	   if (ok .ne. 1) goto 20
!	end if

	if (.true.) stop 'w_cdhf_get_rec_by_time_old: should not be here.'

	call ui8_cpy(cdhf(ch).t1, t1)
	call ui8_cpy(cdhf(ch).t2, t2)

	! convert the caller's char time string to "generic"
	ok = sys_bintim(user_time, ut)
	if (ok .ne. 1) goto 30

	! is the caller prior to (or equal to) BOF?
	ok = ui8_is_eq2(ut, t1)
	if (ok .eq. -1 .or. ok .eq. 0) then
	   ! yes, prior to bof
	   mf = cdhf(ch).first_major
	   ! establish the stream position
	   ok = w_cdhf_set_current_rec(ch,mf)
	   if (ok .ne. 1) goto 100
	   mnr = cdhf(ch).first_minor.i4val
	   goto 2000
	end if

	! is the caller after (or equal to) EOF?
	ok = ui8_is_eq2(ut, t2)
	if (ok .eq. 1 .or. ok .eq. 0) then
	   ! yes, after eof or in last major frame
	   mf = cdhf(ch).last_major
	   ! establish the stream position
	   ok = w_cdhf_set_current_rec(ch,mf)
	   if (ok .ne. 1) goto 100
	   ok = w_cdhf_tm_speed(ch,mf,tm_speed)
	   if (ok .ne. 1) goto 120
	   if (tm_speed .eq. 2) then
	      j = ui8_add(t2,ui8_d2x,xt,ok)
	   else
	      j = ui8_add(t2,ui8_d1x,xt,ok)
	   end if
	   ok = ui8_is_eq2(ut, xt)
	   if (ok .eq. -1) then
	      ! target is in the last major frame
	      mf = cdhf(ch).last_major
	      ok = ui8_sub(ut,t2,dt,ok)
	      call ui8_cpy(t2,t3)
	      goto 1000
	   end if
	   mnr = cdhf(ch).last_minor.i4val
	   goto 2000
	end if

	! number of records in file minus header
	nr.i = cdhf(ch).hdr.n_major_in_file - 1
	if (sunos) nr.i = suntovxi4(nr.i)
	if (macos_ppc) nr.i = suntovxi4(nr.i)

	! calcualte a delta time between major frames
	ok2= ui8_sub(t2,t1,xt,ok)
	if (ok .ne. 1) goto 60
	ok2= ui8_divide(xt, nr, dt, remainder, ok)
	if (ok .ne. 1) goto 70

	! calculate a delta time between file start and caller's time
	ok2= ui8_sub(ut, t1, xt, ok)
	if (ok .ne. 1) goto 80

	! divide (caller's - file_start) by the the diff between MF
	ok2= ui8_divide(xt, dt, nr, remainder, ok)
	if (ok .le. 0) then
	   ! the caller's time is in the first major frame
	end if
	if (nr.j .ne. 0) goto 90

	mf = nr.i
	if (sunos) mf = vxtosuni4(nr.i)
	if (macos_ppc) mf = vxtosuni4(nr.i)

	mf = mf + 1			! to get the physical record #
	if (mf .lt. 1 .or. mf .gt. cdhf(ch).last_major) goto 92

	! establish the stream position
	ok = w_cdhf_set_current_rec(ch,mf)
	if (ok .ne. 1) goto 100

	! verify the stream position by time
	ct3 = atc_to_str2(cdhf(ch).lzr.h.atc_year,
	1                 cdhf(ch).lzr.h.atc_day,
	1                 cdhf(ch).lzr.h.atc_msec)
	ok = sys_bintim(ct3, t3)
	if (ok .ne. 1) goto 110

	ok2= ui8_sub(ut, t3, xt, ok)
	ok = w_cdhf_tm_speed(ch,mf,tm_speed)
	if (ok .ne. 1) goto 120
	if (tm_speed .eq. 2) then
	   j = ui8_is_eq2(xt,ui8_46s)
	else
	   j = ui8_is_eq2(xt,ui8_92s)
	end if
	found = .false.
	if (j .lt. 1) found = .true.

	if (.not. found) then
	   if (ok .eq. 1) then
	      ! target is between current position and BOF
	      ok = w_cdhf_binary_search(ch,t1,ut,t3,
	1	   cdhf(ch).first_major, mf, dt, t3)
	   else
	      ! target is between current position and EOF
	      ok = w_cdhf_binary_search(ch,t3,ut,t2,
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
	nr.i = 0
	nr.j = 0

!	type *, 'mjr,mnr=',mf,mnr, '  TM_SPEED is ', tm_speed

	if (tm_speed .eq. 2) then
	   j = ui8_is_eq2(dt,ui8_d2x)
	   if (j .gt. 0) then
	      ok2= ui8_divide(dt, ui8_d2x, nr, rt, ok)
	   end if
	else
	   j = ui8_is_eq2(dt,ui8_d1x)
	   if (j .gt. 0) then
	      ok2= ui8_divide(dt, ui8_d1x, nr, rt, ok)
	   end if
	end if

	if (sunos) nr.i = vxtosuni4(nr.i)
	if (macos_ppc) nr.i = vxtosuni4(nr.i)
	k = ui8_is_eq2(t3,ut)
	if (nr.i .lt. 250 .and. nr.i .ge. 0) then
	   if (k .lt. 0) then
	      mnr = nr.i 
	   else if (k .gt. 0) then
	      mf = mf - 1
	      mnr = 249 - nr.i + 1
	   else
	   end if
	else if (nr.i .eq. 250) then
	   if (k .ge. 0) then
	      mf = mf - 1
	   else
	      mf = mf + 1
	   end if
	end if

!	type *, 'mjr,mnr=',mf,mnr, '  j,k,nr.i=', j, k, nr.i

 2000	continue

	mjr = mf

	! fill the user's buffer
	ok = w_cdhf_get_rec_xc(
	1	ch, 
	1	mjr,
	1	mnr,
	1	wr)
	if (ok .ne. 1) goto 160

	w_cdhf_get_rec_by_time_old = 1

	return
   1	format(1x,'W_CDHF_GET_REC_BY_TIME_OLD: ', a)
!  10	continue
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot convert file start time to generic.'
!	return
!  20	continue
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot convert file stop time to generic.'
!	return
  30	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot convert caller''s time to generic.'
	return
  60	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get file stop-start delta time.'
	return
  70	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get major frame delta time.'
	return
  80	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get caller-start delta time.'
	return
  90	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot divide to get the target MF.'
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
	write(6,1,iostat=ios) 'could not position stream to ', user_time
	return
 160	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot copy record to caller''s buffer.'
	return
	end

!------------------------------------------------------------------------------
! This routine performs a binary search of the CDHF file associated with the
! supplied channel based on SCET.  Time t1 should be earlier than time t2.
!------------------------------------------------------------------------------
	integer*4	function	w_cdhf_binary_search
	1				(ch,t1,ut,t2,p1,p2,dt,zt)
	implicit	none
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_return_code_def.for'
	include		'wind_cdhf_def.for'
	include		'ui8_def.for'
	include		'vms_time_constants_def.for'
	integer*4	ch
	record /ui8/	t1,t2,ut	! start/end times, user time
	integer*4	p1, p2		! start/end MF major frame numbers
	record /ui8/	dt, zt
	character*24	ct2
	integer*4	ok
	integer*4	ios
	integer*4	j
	integer*4	n_reads
!	integer*4	n_calls
!	integer*4	n_cumulative_reads
	integer*4	dir
	integer*4	tm_speed
	integer*4	max_search_depth
	parameter	(max_search_depth=17)
	integer*4	w_cdhf_tm_speed			! a function
	integer*4	sys_bintim			! a function
	integer*4	w_cdhf_set_current_rec		! a function
	character*24	atc_to_str2			! a function
	logical*4	wind_suppress_messages
	integer*4	ui8_is_eq2
	external        ui8_is_eq2            !$pragma C ( ui8_is_eq2 )
	integer*4	ui8_sub
	external        ui8_sub               !$pragma C ( ui8_sub )
	external        ui8_cpy               !$pragma C ( ui8_cpy )
	structure /binary_search_index/
	   integer*4	n
	   record /ui8/ t
	   record /ui8/ tdiff
	end structure
	record /binary_search_index/ lo, hi, mid
	logical*4	done
	integer*4	xhi,xlo
	integer*4	midpoint
	midpoint(xlo,xhi) = max(xlo, ((xhi+xlo)/2) )

	w_cdhf_binary_search = 0

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
	   ct2 = atc_to_str2(cdhf(ch).lzr.h.atc_year,
	1                 cdhf(ch).lzr.h.atc_day,
	1                 cdhf(ch).lzr.h.atc_msec)
	   ok = sys_bintim(ct2, mid.t)
	   if (ok .ne. 1) goto 30

	   ! get the delta time between midpoint and the user's time
	   ! ...go left for positive difference, 
	   ! ...go right for negative difference
	   ok = ui8_sub(mid.t,ut,mid.tdiff,dir)
	   if (dir .gt. 1 .or. dir .lt. -1) goto 40
	   ! are we within one major frame?...if so then success!
	   ok = w_cdhf_tm_speed(ch,mid.n,tm_speed)
	   if (ok .ne. 1) goto 50
	   if (tm_speed .eq. 2) then
	      j = ui8_is_eq2(mid.tdiff,ui8_46s)
	   else
	      j = ui8_is_eq2(mid.tdiff,ui8_92s)
	   end if
	   if (j .lt. 1) then
	      done = .true.			! delta time minimized=success
	   else if (mid.n .eq. lo.n) then
	      done = .true.			! time gap? closest convergence
	   else
	      ! setup for next search iteration
	      if (dir .eq. 1) then
	         ! current midpoint is less than target time
	         hi = mid
	      else if (dir .eq. -1) then
	         ! current midpoint is greater than target time
	         lo = mid
	      else
	         goto 80			! should never get here
	      end if
	      if (lo.n .ge. hi.n) goto 90	! time gap? reverse time?
	   end if
	end do

	call ui8_cpy(mid.tdiff,dt)
	call ui8_cpy(mid.t,zt)

!	n_cumulative_reads = n_cumulative_reads + n_reads

!	type *, 'Binary Search used ', n_reads, ' reads.'

	w_cdhf_binary_search = 1

	return
   1	format(1x,'W_CDHF_BINARY_SEARCH: ', a, :, i)
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
  40	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot subtract (ui8) times at', mid.n
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
