! rad_list_mode.for - routines for generating frequency, channel, or toggle
! state lists from disk pointer tables for RAD1 and RAD2 list-mode operation.

	integer*4	function	w_rad1_lsmode_prn_tbls(tp,
	1	rad1_ctable, rad1_ftable, rad1_ptable)
	implicit none
	include		'rad_list_mode_def.for'
	include		'c_string_def.for'
	record /tm_list_parms/ tp
	integer*4 	rad1_ctable(size_flist,n_flists)           ! channels 
	real   *4 	rad1_ftable(size_flist,n_flists)           ! frequency 
	integer*4 	rad1_ptable(size_rad1_plist,n_rad1_plists) ! pointers
	integer*4	i,j,k,o

	w_rad1_lsmode_prn_tbls = 1

  1	format(1x,a16,':',1x,i5)
	write(6,1,iostat=o) 'xlate_table', tp.xlate_table
	write(6,1,iostat=o) 'freq_table', tp.freq_table
	write(6,1,iostat=o) 'steps', tp.steps
	write(6,1,iostat=o) 'sum_loop', tp.sum_loop
	write(6,1,iostat=o) 'group_loop', tp.group_loop
	write(6,1,iostat=o) 'xlat_mask', tp.xlat_mask
	write(6,1,iostat=o) 'auto_mask', tp.auto_mask
	write(6,1,iostat=o) 'sum_flag', tp.sum_flag
	write(6,1,iostat=o) 'radio', tp.radio
	write(6,1,iostat=o) 'list_type', tp.list_type

  2	format(1x,<size_flist>(i4))
	write(6,*) 'Channel table:'
	write(6,2,iostat=o) ((rad1_ctable(i,j), i=1,size_flist), j=1,n_flists)

  3	format(1x,8(f9.0))
	write(6,*) 'Freq table:'
	write(6,3,iostat=o) ((rad1_ftable(i,j), i=1,size_flist), j=1,n_flists)

  4	format(1x, 16(i4))
	do k=1,n_rad1_plists
	   write(6,'(1x,a,i3)',iostat=o) 'Pointer table #', k
	   write(6,4,iostat=o) (rad1_ptable(i,k), i=1,size_rad1_plist)
	end do

	return
	end
!------------------------------------------------------------------------------
! Calls read_rad_disk_Xtable, and either of
! extract_rad_freq_list or calc_rad_toggle_list depending on the flag args,
! to generate a RAD[1,2] frequency, channel, or toggle state list.
!------------------------------------------------------------------------------
	integer*4	function	w_get_rad_lsmode_list
	1		(tp,pfileb,cfileb,userbuf,size_userbuf,ret_size)
	implicit none
	include		'rad_list_mode_def.for'
	include		'c_string_def.for'
	record /tm_list_parms/ tp
	byte		pfileb(*)	! c string, pointer file name
	byte		cfileb(*)	! c string, chan or freq file name
	integer*4	userbuf(*)	! freq or chan or toggle outputs
	integer*4	size_userbuf	! size of userbuf
	integer*4	ret_size	! number of values written to userbuf

	logical*4	got_rad1_ptr_tbl /.false./
	logical*4	got_rad1_ch_tbl  /.false./
	logical*4	got_rad1_hz_tbl  /.false./
	integer*4 	rad1_ctable(size_flist,n_flists)           ! channels 
	real   *4 	rad1_ftable(size_flist,n_flists)           ! frequency 
	integer*4 	rad1_ptable(size_rad1_plist,n_rad1_plists) ! pointers
	record /tm_list_parms/ rad1_default_parm_tbl(n_rad1_plists)

	logical*4	got_rad2_ptr_tbl /.false./
	logical*4	got_rad2_ch_tbl  /.false./
	logical*4	got_rad2_hz_tbl  /.false./
	integer*4 	rad2_ctable(size_flist,n_flists)           ! channels 
	real   *4 	rad2_ftable(size_flist,n_flists)	   ! frequencies
	integer*4 	rad2_ptable(size_rad2_plist,n_rad2_plists) ! pointers
	record /tm_list_parms/ rad2_default_parm_tbl(n_rad2_plists)

	integer*4	ok
	integer*4	ios
	integer*4	read_rad_disk_ptable		! a function
	integer*4	read_rad_disk_ctable		! a function
	integer*4	read_rad_disk_ftable		! a function

	integer*4	extract_rad_freq_list		! a function
	integer*4	calc_rad_toggle_list		! a function

	logical*4	wind_suppress_internal_msgs	! a function
	external	w_cnvrt_c_fn_by_os
	integer*4	w_cnvrt_c_fn_by_os !$pragma C (w_cnvrt_c_fn_by_os)

	integer*4	callers_task
$IF ABSOFT_FORTRAN
	integer*4	get_frequency_list
	integer*4	get_toggle_list
	integer*4	get_channel_list
	parameter	(get_frequency_list=1)
	parameter	(get_toggle_list=2)
	parameter	(get_channel_list=3)
$ELSE
	parameter	get_frequency_list=1
	parameter	get_toggle_list=2
	parameter	get_channel_list=3
$ENDIF
	record /c_string_256/ bstr
	character*4	event(2) /'RAD1', 'RAD2'/
	character*10	taskname(3) /'FREQUENCY', 'TOGGLE', 'CHANNEL'/

	if (tp.list_type .eq. rad_hz_list) then
	   callers_task = get_frequency_list
	else if (tp.list_type .eq. rad_ch_list) then
	   callers_task = get_channel_list
	else if (tp.list_type .eq. rad_tgl_list) then
	   callers_task = get_toggle_list
	else
	   goto 10
	end if

	if (tp.radio .eq. 2) goto 2000
	if (tp.radio .ne. 1) goto 20
 1000	continue					! read rad1 files

	if (.not. got_rad1_ptr_tbl) then
	   ok = w_cnvrt_c_fn_by_os(pfileb, bstr.b, len(bstr.c))
	   if (ok .ne. 1) goto 30
	   ok = read_rad_disk_ptable(
	1	bstr.c,
	1	size_rad1_plist,
	1	n_rad1_plists, 
	1	n_p_per_line_rad1, 
	1	rad1_ptable,
	1	rad1_default_parm_tbl)
	   if (ok .ne. 1) goto 40
	   got_rad1_ptr_tbl = .true.
	end if

	if (callers_task .eq. get_channel_list) then

	   if (.not.got_rad1_ch_tbl) then
	      ok = w_cnvrt_c_fn_by_os(cfileb, bstr.b, len(bstr.c))
	      if (ok .ne. 1) goto 30
	      ok = read_rad_disk_ctable(
	1		bstr.c,
	1		rad1_ctable)
	      if (ok .ne. 1) goto 40
	      got_rad1_ch_tbl = .true.
	   end if
	   ok = extract_rad_freq_list(
	1	tp,
	1	rad1_ptable, size_rad1_plist, n_rad1_plists,
	1	rad1_ctable,
	1	userbuf, size_userbuf, ret_size)
	   if (ok .ne. 1) goto 50

!	   call w_rad1_lsmode_prn_tbls(tp,
!	1	rad1_ctable, rad1_ftable, rad1_ptable)

	else if (callers_task .eq. get_frequency_list) then

	   if (.not.got_rad1_hz_tbl) then
	      ok = w_cnvrt_c_fn_by_os(cfileb, bstr.b, len(bstr.c))
	      if (ok .ne. 1) goto 30
	      ok = read_rad_disk_ftable(
	1		bstr.c,
	1		rad1_ftable)
	      if (ok .ne. 1) goto 40
	      got_rad1_hz_tbl = .true.
	   end if
	   ok = extract_rad_freq_list(
	1	tp,
	1	rad1_ptable, size_rad1_plist, n_rad1_plists,
	1	rad1_ftable,
	1	userbuf, size_userbuf, ret_size)
	   if (ok .ne. 1) goto 50

	else if (callers_task .eq. get_toggle_list) then

	   ok = calc_rad_toggle_list(
	1	tp,
	1	rad1_ptable, size_rad1_plist, n_rad1_plists,
	1	userbuf, size_userbuf, ret_size)
	   if (ok .ne. 1) goto 50

	end if

	goto 3000

 2000	continue					! read rad2 files
	if (.not. got_rad2_ptr_tbl) then
	   ok = w_cnvrt_c_fn_by_os(pfileb, bstr.b, len(bstr.c))
	   if (ok .ne. 1) goto 30
	   ok = read_rad_disk_ptable(
	1	bstr.c,
	1	size_rad2_plist,
	1	n_rad2_plists, 
	1	n_p_per_line_rad2, 
	1	rad2_ptable,
	1	rad2_default_parm_tbl)
	   if (ok .ne. 1) goto 40
	   got_rad2_ptr_tbl = .true.
	end if

	if (callers_task .eq. get_channel_list) then

	   if (.not.got_rad2_ch_tbl) then
	      ok = w_cnvrt_c_fn_by_os(cfileb, bstr.b, len(bstr.c))
	      if (ok .ne. 1) goto 30
	      ok = read_rad_disk_ctable(
	1		bstr.c,
	1		rad2_ctable)
	      if (ok .ne. 1) goto 40
	      got_rad2_ch_tbl = .true.
	   end if
	   ok = extract_rad_freq_list(
	1	tp,
	1	rad2_ptable, size_rad2_plist, n_rad2_plists,
	1	rad2_ctable,
	1	userbuf, size_userbuf, ret_size)
	   if (ok .ne. 1) goto 50

	else if (callers_task .eq. get_frequency_list) then

	   if (.not.got_rad2_hz_tbl) then
	      ok = w_cnvrt_c_fn_by_os(cfileb, bstr.b, len(bstr.c))
	      if (ok .ne. 1) goto 30
	      ok = read_rad_disk_ftable(
	1		bstr.c,
	1		rad2_ftable)
	      if (ok .ne. 1) goto 40
	      got_rad2_hz_tbl = .true.
	   end if
	   ok = extract_rad_freq_list(
	1	tp,
	1	rad2_ptable, size_rad2_plist, n_rad2_plists,
	1	rad2_ftable,
	1	userbuf, size_userbuf, ret_size)
	   if (ok .ne. 1) goto 50

	else if (callers_task .eq. get_toggle_list) then

	   ok = calc_rad_toggle_list(
	1	tp,
	1	rad2_ptable, size_rad2_plist, n_rad2_plists,
	1	userbuf, size_userbuf, ret_size)
	   if (ok .ne. 1) goto 50

	end if

 3000	continue
	w_get_rad_lsmode_list = 1

	return
  1	format(1x,'W_GET_RAD_LSMODE_LIST: ', a,:,a, a,:,a, a)
 10	continue
	w_get_rad_lsmode_list = 0
	write(6,1,iostat=ios) 'Invalid list type'
	return
 20	continue
	w_get_rad_lsmode_list = 0
	write(6,1,iostat=ios) 'RAD1/RAD2 not specified'
	return
 30	w_get_rad_lsmode_list = ok
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios) 'Cannot get file name for '//
	1	event(tp.radio)//
	1	' list mode '//
	1	taskname(callers_task)//
	1	' list.'
	return
 40	w_get_rad_lsmode_list = ok
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios) 'Cannot use file for '//
	1	event(tp.radio)//
	1	' list mode '//
	1	taskname(callers_task)//
	1	' list.'
	return
 50	w_get_rad_lsmode_list = ok
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=ios) 'Cannot extract '//
	1	event(tp.radio)//
	1	' list mode '//
	1	taskname(callers_task)//
	1	' list.'
	return

	end
!------------------------------------------------------------------------------
! This routine extracts/builds the frequency list from a list of pointer
! lists, a frequency table, and a complicated set of extraction instructions.
!------------------------------------------------------------------------------
	integer*4	function	extract_rad_freq_list
	1				(extract,
	1				pointers, n_p_in_plist, n_plists,
	1				ftable,
	1				userbuf,size_userbuf,ret_size)
	implicit	none
	include		'rad_list_mode_def.for'
	record /tm_list_parms/ extract		! TM info for list extraction
	integer*4	n_p_in_plist
	integer*4	n_plists
	integer*4	pointers(0:n_p_in_plist-1,0:n_plists-1)
	integer*4	ftable(0:last_fdex)
	integer*4	userbuf(*)	!FREQ OUTPUTS
	integer*4	size_userbuf
	integer*4	ret_size
	integer*4	fdex
	integer*4	fnot			! index to first freq of table
	logical*4	wind_suppress_internal_msgs

	integer *4	count		        !COUNT OF STEPS
	integer *4	jndex			!POSITION IN POINTER
	integer *4	i,j,k,ik		!LOOP COUNTERS
	logical *4	translate		!FALSE WHEN POINTER = FREQUENCY
	integer *4	group_start	        !STARTING POINTER FOR THIS GROUP
	integer *4	ip			!LOOP COUNTER

	! validate the translate and frequency table numbers
!	if (extract.xlate_table .gt. (n_plists-1)) goto 101
!	if (extract.freq_table  .gt. (n_flists-1)) goto 102
	! Dec 13, 1993 ==> xlate_table refers to flists
	!              and freq_table  refers to plists
	if (extract.xlate_table .gt. (n_flists-1)) goto 101
	if (extract.freq_table  .gt. (n_plists-1)) goto 102

	! set misc variables
	translate = extract.xlat_mask .ne. 0
!	i = extract.xlate_table
	i = extract.freq_table
!	fnot  = extract.freq_table * size_flist
	fnot  = extract.xlate_table * size_flist
	count = 1
	group_start = 0

	do ip = 1,extract.steps			! steps loop
	   do k = 1,extract.sum_loop		! sum_loop
	      do j = 1,extract.group_loop	! group repeat loop
	         jndex = group_start
	         do ik =1,extract.group_size	! group size loop
	            ! check for indexing errors
	            if (jndex .ge. n_p_in_plist) goto 103
	            if (count .gt. size_userbuf) goto 104
	            if (translate) then
	               ! use pointer table to translate to frequency table
$IF ABSOFT_FORTRAN
	               fdex = fnot + (extract.xlat_mask .and.
	1                 pointers(jndex,i))
$ELSE
	               fdex = fnot + (extract.xlat_mask .and. pointers(jndex,i))
$ENDIF
	               if (fdex .gt. last_fdex) goto 105
                       userbuf(count) = ftable(fdex)
	            else
	               ! no translation, the pointer value is the frequency
	               userbuf(count) = pointers(jndex,i)
	            endif
	            jndex = jndex + 1
	            count = count + 1
	         end do				! group_size loop
	      end do				! group_loop
	   end do				! sum_loop
	   ! reset the group start pointer that moves down the pointer list
	   group_start = group_start + extract.group_size
	end do					! steps loop

	ret_size = count - 1
	extract_rad_freq_list = 1
	return
  1	format(1x,'EXTRACT_RAD_FREQ_LIST: ', a, :, a)
  2	format(1x,'EXTRACT_RAD_FREQ_LIST: ', a, :, i)
 101	extract_rad_freq_list = 0
	if (wind_suppress_internal_msgs()) return
	type  1, 'XLATE_TABLE is too large in value.'
	goto 200
 102	extract_rad_freq_list = 0
	if (wind_suppress_internal_msgs()) return
	type  1, 'FREQ_TABLE is too large in value.'
	goto 200
 103	extract_rad_freq_list = 3
	if (wind_suppress_internal_msgs()) return
	ret_size = count - 1
	type 1, 'looping directions carry beyond bounds of translation list.'
	goto 200
 104	extract_rad_freq_list = 3
	if (wind_suppress_internal_msgs()) return
	ret_size = count - 1
	type 2, 'frequency list is longer than user''s buffer of', size_userbuf
	goto 200
 105	extract_rad_freq_list = 3
	if (wind_suppress_internal_msgs()) return
	ret_size = count - 1
	type 1, 'looping directions carry beyond bounds of frequency list.'
	goto 200
 200	continue
	type *, '	      steps=', extract.steps
	type *, '	   sum_loop=', extract.sum_loop
	type *, '	 group_loop=', extract.group_loop
	type *,	'	 group_size=', extract.group_size
	type *, '	xlate_table=', extract.xlate_table
	type *, '	 freq_table=', extract.freq_table
	type *, '	  xlat_mask=', extract.xlat_mask
$IF ABSOFT_FORTRAN
	type *, '(1x,a,5(2x,i5))', 'ip,k,j,ik,jndex=',ip,k,j,ik,jndex
$ELSE
	type '(1x,a,5(2x,i5))', 'ip,k,j,ik,jndex=',ip,k,j,ik,jndex
$ENDIF
	return
	end
!------------------------------------------------------------------------------
! This routine calcualtes the toggle list from a list of pointer lists and
! a complicated set of extraction instructions.
!------------------------------------------------------------------------------
	integer*4	function	calc_rad_toggle_list
	1				(extract,
	1				pointers, n_p_in_plist, n_plists,
	1				userbuf,size_userbuf,ret_size)
	implicit	none
	include		'rad_list_mode_def.for'
	record /tm_list_parms/ extract		! TM info for list extraction
	integer*4	n_p_in_plist
	integer*4	n_plists
	integer*4	pointers(0:n_p_in_plist-1,0:n_plists-1)
	integer*4	userbuf(*)	!FREQ OUTPUTS
	integer*4	size_userbuf
	integer*4	ret_size
	logical*4	wind_suppress_internal_msgs

	integer *4	count		        !COUNT OF STEPS
	integer *4	jndex			!POSITION IN POINTER
	integer *4	i,j,k,ik		!LOOP COUNTERS
	integer *4	group_start	        !STARTING POINTER FOR THIS GROUP
	integer *4	ip			!LOOP COUNTER

	logical*4	after_every_measurement
	logical*4	after_every_group
	logical*4	after_every_group_loop
	logical*4	after_every_sum_loop
	logical*4	take_state_from_pointer
	integer*4	current_toggle_state

	! validate the translate and frequency table numbers
!	if (extract.xlate_table .gt. (n_plists-1)) goto 101
	! Dec 13, 1993 ==> xlate_table refers to flists
	!              and freq_table  refers to plists
	if (extract.freq_table .gt. (n_plists-1)) goto 101

	! set misc variables
	current_toggle_state = extract.sum_flag
	! added the following line Dec 13, 1993 ==> initial sum/sep state
	! is opposite of the sum_flag value
	current_toggle_state = current_toggle_state .xor. 1
!	i = extract.xlate_table
	i = extract.freq_table
	count = 1
	group_start = 0
$IF ABSOFT_FORTRAN
!
!  2007/07/15:  the Absoft Fortran compiler fails on:
!
!     after_every_measurement = (extract.auto_mask .and.  '01'x) .ne. 0
!
! with a complaint that '01'x is not the right length to match the
! number of bits in "extract.auto_mask".   I also tried '00000001'x,
! which should be 32-bits (integer*4) but the same happens.  So just
! use the intermediate variable "i":
!
	i = '0001'x
	after_every_measurement	= (extract.auto_mask .and. i) .ne. 0
	i = '0002'x
	after_every_group	= (extract.auto_mask .and. i) .ne. 0
	i = '0004'x
	after_every_group_loop	= (extract.auto_mask .and. i) .ne. 0
	i = '0008'x
	after_every_sum_loop	= (extract.auto_mask .and. i) .ne. 0
	i = '0080'x
	take_state_from_pointer	= (extract.auto_mask .and. i) .ne. 0
$ELSE
	after_every_measurement	= (extract.auto_mask .and.  '01'x) .ne. 0
	after_every_group	= (extract.auto_mask .and.  '02'x) .ne. 0
	after_every_group_loop	= (extract.auto_mask .and.  '04'x) .ne. 0
	after_every_sum_loop	= (extract.auto_mask .and.  '08'x) .ne. 0
	take_state_from_pointer	= (extract.auto_mask .and.  '80'x) .ne. 0
$ENDIF

	do ip = 1,extract.steps			! steps loop
	   do k = 1,extract.sum_loop		! sum_loop
	      do j = 1,extract.group_loop	! group repeat loop
	         jndex = group_start
	         do ik =1,extract.group_size	! group size loop
	            ! check for indexing errors
	            if (jndex .ge. n_p_in_plist) goto 103
	            if (count .gt. size_userbuf) goto 104
	            if (take_state_from_pointer) then
	               ! slide bit seven down to bit zero
	               userbuf(count) = pointers(jndex,i)/128
	            else
	               userbuf(count) = current_toggle_state
	            end if
	            jndex = jndex + 1
	            count = count + 1
	            if (after_every_measurement) then
	               current_toggle_state = current_toggle_state .xor. 1
	            end if
	         end do				! group_size loop
	         if (after_every_group) then
	            current_toggle_state = current_toggle_state .xor. 1
	         end if
	      end do				! group_loop
	      if (after_every_group_loop) then
	         current_toggle_state = current_toggle_state .xor. 1
	      end if
	   end do				! sum_loop
	   ! reset the group start pointer that moves down the pointer list
	   group_start = group_start + extract.group_size
	   if (after_every_sum_loop) then
	      current_toggle_state = current_toggle_state .xor. 1
	   end if
	end do					! steps loop

	ret_size = count - 1
	calc_rad_toggle_list = 1
	return
  1	format(1x,'CALC_RAD_TOGGLE_LIST: ', a, :, a)
  2	format(1x,'CALC_RAD_TOGGLE_LIST: ', a, :, i)
 101	calc_rad_toggle_list = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'FREQ_TABLE is too large in value.'
	goto 200
 103	calc_rad_toggle_list = 3
	if (wind_suppress_internal_msgs()) return
	ret_size = count - 1
	type 1, 'looping directions carry beyond bounds of translation list.'
	goto 200
 104	calc_rad_toggle_list = 3
	if (wind_suppress_internal_msgs()) return
	ret_size = count - 1
	type 2, 'toggle list is longer than user''s buffer of', size_userbuf
	goto 200
! 105	calc_rad_toggle_list = 3
!	if (wind_suppress_internal_msgs()) return
!	ret_size = count - 1
!	type 1, 'looping directions carry beyond bounds of frequency list.'
!	goto 200
 200	continue
	type *, '	      steps=', extract.steps
	type *, '	   sum_loop=', extract.sum_loop
	type *, '	 group_loop=', extract.group_loop
	type *,	'	 group_size=', extract.group_size
	type *, '	xlate_table=', extract.xlate_table
	type *, '	 freq_table=', extract.freq_table
	type *, '	  xlat_mask=', extract.xlat_mask
$IF ABSOFT_FORTRAN
	type *, '(1x,a,5(2x,i5))', 'ip,k,j,ik,jndex=',ip,k,j,ik,jndex
$ELSE
	type '(1x,a,5(2x,i5))', 'ip,k,j,ik,jndex=',ip,k,j,ik,jndex
$ENDIF
	return
	end
!------------------------------------------------------------------------------
! This routine opens and reads in a set of pointers from one precisely
! formatted file.
!------------------------------------------------------------------------------
	integer*4	function	read_rad_disk_ptable
	1		(ptrfile, 
	1		size_plist, 
	1		n_plists, 
	1		n_p_per_line, 
	1		ptr_buf,
	1		default_parm_buf)
	implicit	none
	include		'rad_list_mode_def.for'
	character*(*)	ptrfile				! pointer file name
	integer*4	size_plist			! size of pointer list
	integer*4	n_plists			! number of p-lists
	integer*4	n_p_per_line			! # of ptrs per line
	integer*4	ptr_buf(size_plist, n_plists)	! where to put pointers
	record /tm_list_parms/ default_parm_buf(n_plists) ! default freq list
							! building parameters
	integer*4	lun /0/
	integer*4	line_number
	character*80	line
	integer*4	i,j,k,n
	integer*4	k2len
	integer*4	ios
	logical*4	wind_suppress_internal_msgs	! a function
	integer*4	dummy

	read_rad_disk_ptable = 0

	!
	! Open and read the pointer/translation file
	!
	line_number = 0
	k = k2len(ptrfile)
	if (lun .eq. 0) call lib$get_lun(lun)
$IF ABSOFT_FORTRAN
!
! (2007/07/16):  Absoft Fortran must replace "type='old'" with
!  "status='old'".
!
	open(lun,
	1	name=ptrfile(:k),
 	1	status='old',
	1	form='formatted',
	1	iostat=ios,
	1	readonly,
	1	shared,
	1	err=10)
$ELSE
	open(lun,
	1	name=ptrfile(:k),
	1	type='old',
	1	form='formatted',
	1	iostat=ios,
	1	readonly,
	1	shared,
	1	err=10)
$ENDIF
	!
	! first two lines in file are comments
	line_number = line_number + 1
	read(lun,8,iostat=ios,err=12) line
	line_number = line_number + 1
	read(lun,8,iostat=ios,err=12) line
	do i=1,n_plists
	   ! first line of every pointer list is a textual description
	   line_number = line_number + 1
	   read(lun,8,iostat=ios,err=12) line
	   ! next line contains the default indexing parameters
	   line_number = line_number + 1
	   read(lun,8,iostat=ios,err=12) line
	   read(line,*,iostat=ios,err=12) 
	1	    default_parm_buf(i).steps,
	1           default_parm_buf(i).sum_loop,
	1           default_parm_buf(i).group_loop,
	1           default_parm_buf(i).group_size,
	1           default_parm_buf(i).xlate_table,
	1           default_parm_buf(i).auto_mask,
	1           default_parm_buf(i).xlat_mask,
	1           dummy	! not implemented=default_parm_buf(i).zavg
	   ! now read 4 lines of n_p_per_line (16 or 12) hex values

!xxxxxxxxxxx use var m to read m numbers in total per current list
!	   m = default_parm_buf(i).steps * default_parm_buf(i).group_size

	   n = n_p_per_line
	   do j=0,3
	      line_number = line_number + 1
	      read(lun,8,iostat=ios,err=12) line
	      read(line,'(<n_p_per_line>(z2,1x))',iostat=ios,err=12)
	1	(ptr_buf(k,i), k=(j*n)+1,(j*n)+n)
	   end do
	end do
	close(lun)
	call lib$free_lun(lun)
	lun = 0

	read_rad_disk_ptable = 1

  8	format(a)
	return
  1	format(1x,'READ_RAD_DISK_PTABLE: ', a, :, i3)
 10	read_rad_disk_ptable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot open pointer/translation file, iostat=', ios
	type *, '  Filename=', ptrfile
	call lib$free_lun(lun)
	lun = 0
	return
 12	read_rad_disk_ptable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'error reading line #',line_number
	type *, '  Filename=', ptrfile
	k = max(1,k2len(line))
	type *, '  Line=',line(:k)
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	return
	end
!------------------------------------------------------------------------------
! This routine opens and reads in a set of channel numbers from one precisely
! formatted file.
!------------------------------------------------------------------------------
	integer*4	function	read_rad_disk_ctable(chfile, ch_buf)
	implicit	none
	include		'rad_list_mode_def.for'
	character*(*)	chfile			! channel file name
	integer*4	ch_buf(size_flist, n_flists)	! where to put ch's
	integer*4	lun /0/
	integer*4	line_number
	character*80	line
	integer*4	i,j,k
	integer*4	k2len
	integer*4	ios
	logical*4	wind_suppress_internal_msgs	! a function

	read_rad_disk_ctable = 0

	!
	! Open and read in the channel list file
	!
	call lib$get_lun(lun)
	line_number = 0
	k = k2len(chfile)
$IF ABSOFT_FORTRAN
!
! (2007/07/16):  Absoft Fortran must replace "type='old'" with
!  "status='old'".
!
	open(lun,
	1	name=chfile(:k),
 	1	status='old',
	1	form='formatted',
	1	readonly,
	1	shared,
	1	iostat=ios,
	1	err=40)
$ELSE
	open(lun,
	1	name=chfile(:k),
	1	type='old',
	1	form='formatted',
	1	readonly,
	1	shared,
	1	iostat=ios,
	1	err=40)
$ENDIF
	!
	! first two lines in file are comments
	line_number = line_number + 1
	read(lun,8,iostat=ios,err=42) line
	line_number = line_number + 1
	read(lun,8,iostat=ios,err=42) line

	do i=1,n_flists
	   line_number = line_number + 1
	   read(lun,8,iostat=ios,err=42) line
	   read(line,*,iostat=ios,err=42) (ch_buf(j,i),j=1,size_flist)
	end do
	close(lun)

	call lib$free_lun(lun)
	lun = 0

	read_rad_disk_ctable = 1

  8	format(a)
	return
  1	format(1x,'READ_RAD_DISK_CTABLE: ', a, :, i3)
 40	read_rad_disk_ctable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot open list mode channel file, iostat=', ios
	type *, '  Filename=', chfile
	call lib$free_lun(lun)
        lun = 0
	return
 42	read_rad_disk_ctable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'error reading line #',line_number
	type *, '  Filename=', chfile
	k = max(1,k2len(line))
	type *, '  Line=',line(:k)
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	return
	end
!------------------------------------------------------------------------------
! This routine opens and reads in a set of frequencies from a precisely
! formatted file.
!------------------------------------------------------------------------------
	integer*4	function	read_rad_disk_ftable(freqfile, fq)
	implicit	none
	include		'rad_list_mode_def.for'
	character*(*)	freqfile			! file name
	real*4		fq(size_flist, n_flists)	! where to put freqs
	integer*4	lun /0/
	integer*4	line_number
	character*80	line
	integer*4	i,j,k
	integer*4	k2len
	integer*4	ios
	logical*4	wind_suppress_internal_msgs	! a function

	read_rad_disk_ftable = 0

	!
	! Open and read in the frequency list file
	!
	call lib$get_lun(lun)
	line_number = 0
	k = k2len(freqfile)
$IF ABSOFT_FORTRAN
!
! (2007/07/16):  Absoft Fortran must replace "type='old'" with
!  "status='old'".
!
	open(lun,
	1	name=freqfile(:k),
 	1	status='old',
	1	form='formatted',
	1	readonly,
	1	shared,
	1	iostat=ios,
	1	err=40)
$ELSE
	open(lun,
	1	name=freqfile(:k),
	1	type='old',
	1	form='formatted',
	1	readonly,
	1	shared,
	1	iostat=ios,
	1	err=40)
$ENDIF

	! first two lines in file are comments, skip over
	line_number = line_number + 1
	read(lun,8,iostat=ios,err=42) line
	line_number = line_number + 1
	read(lun,8,iostat=ios,err=42) line

	! read in the table of real values
	read(lun,*,iostat=ios,err=46) ((fq(i,j), i=1,size_flist), j=1,n_flists)

	close(lun)
	call lib$free_lun(lun)
	lun = 0

	read_rad_disk_ftable = 1

  8	format(a)
	return
  1	format(1x,'READ_RAD_DISK_FTABLE: ', a, :, i3)
 40	read_rad_disk_ftable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot open frequency list file, iostat=', ios
	type *, '  Filename=', freqfile(:k)
	call lib$free_lun(lun)
	lun = 0
	return
 42	read_rad_disk_ftable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'error reading line #',line_number
	type *, '  Filename=', freqfile
	k = max(1,k2len(line))
	type *, '  Line=',line(:k)
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	return
 46	read_rad_disk_ftable = 0
	if (wind_suppress_internal_msgs()) return
	type 1, 'error reading frequency list, iostat=', ios
	type *, '  Filename=', freqfile(:k)
	close(lun)
	call lib$free_lun(lun)
	lun = 0
	return
	end
