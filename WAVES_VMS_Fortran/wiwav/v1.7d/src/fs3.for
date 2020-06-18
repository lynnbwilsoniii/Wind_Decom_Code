! fs3.for - wind/waves File Select program #3, this "chooser" uses a set of
! edit_at fields to enter starting and stopping SCET values.
!
!------------------------------------------------------------------------------
	integer*4	function	w_fs3(file, ur8_beg, ur8_end)
	implicit	none
	include		'fs3_def.for/nolist'
	character*(*)	file
	real*8		ur8_beg, ur8_end
	integer*4	ok, aok
	integer*4	manage_screen
	integer*4	w_fs3_err
	external	w_fs3_err
	integer*4	i,k
	integer*4	k2len
	integer*4	w_get_file_list_from_dir
	integer*4	ret_size
	integer*4	get_tt_pagelen
	integer*4	lib$set_logical

	w_fs3 = 0
	file = ' '
	ur8_beg = 0.0
	ur8_end = 0.0

	! get terminal screen size
	ok = get_tt_pagelen('tt', i)
	if (.not. ok) then
	   chooser_bottom_line = 24
	else
	   chooser_bottom_line = i
	end if
	chooser_start_line = chooser_bottom_line - chooser_row_height

	call putstr(soft_reset)
	call setup_fs3_fields()
	call w_fs3_build_screen()

	! fit chooser to bottom of screen after scrolling out enough room
	call w_fs3_draw_screen()

	ok = manage_screen(g,on,w_fs3_err)
	call post_fs3_screen()
	call goto_fs3_bottom()
	if (ok) then
	   prev_ur8a = ur8a
	   prev_ur8b = ur8b
	   result = 
	1  'wi_lz_wav_'//f(1).str(1:4)//f(3).str(1:2)//f(4).str(1:2)//'_v*.dat'
!	   file = 'WIND_DATA:'//result
	   ur8_beg = ur8a
	   ur8_end = ur8b
	   ok = w_get_file_list_from_dir('WIND_DATA', result, 
	1	file, 1, len(file), ret_size, '03'x)
	   if (ok .ne. 0) then
	      k = k2len(file)
	      aok = lib$set_logical(
	1		wind_last_file_used,	! logical name
	1		file(:k),		! equivalence string
	1		'LNM$PROCESS_TABLE',	! table name
	1		,			! attributes
	1		)			! item list
	      if (.not. aok) then
	         type '(1x,a,a,a,z8.8,a)',
	1	'Cannot define ', wind_last_file_used,
	1	' in process table, status:', aok, '.'
	      end if
!	   else
!	      type *, 'ok=', ok
!	      type *, 'err: ', file(1:60)
	   end if
	end if

	w_fs3 = ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	post_fs3_screen()
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	ok
	integer*4	k2len

	post_fs3_screen = 0

	! screen reset code
	call putstr(soft_reset)
!	call putstr(csi//'1;99r'//csi//'24;1f')
	call putstr(csi//'1;99r')

!	type *, 'Field #1: ', f(1).str(:40)
!	type *, 'Field #2: ', f(2).str(:40)
!	type *, 'Field #3: ', f(3).str(:40)
!	type *, 'Field #4: ', f(4).str(:40)
!	type *, 'Field #5: ', f(5).str(:40)
!	type *, 'Field #6: ', f(6).str(:40)
!	type *, 'Field #7: ', f(7).str(:40)
!	type *, 'Field #8: ', f(8).str(:40)
!	type *, 'Field #9: ', f(9).str(:40)

	post_fs3_screen = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_draw_screen()
	implicit	none
	include		'fs3_def.for'
	integer*4	i

	w_fs3_draw_screen = 1
	call goto_fs3_bottom()
	call lib$put_output(' ')
	do i=chooser_start_line,chooser_bottom_line
	   call lib$put_output(' ')
	end do
	call w_fs3_refresh()

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	setup_fs3_fields()
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	i,j,k,m,n,o
	integer*4	row
	integer*4	col
	character*8	ctime
	character	s4*16
	integer*4	year,month,day,doy
	integer*4	hour,minute,second
	integer*4	iday_of_year
	integer*4	ok
	integer*4	ios
	integer*4	get_logical_name_str
	integer*4	edit_at
	external	edit_at
	integer*4	w_fs3_err
	external	w_fs3_err
	integer*4	w_fs3_refresh
	external	w_fs3_refresh
	integer*4	w_fs3_exit
	external	w_fs3_exit
	integer*4	w_fs3_up
	external	w_fs3_up
	integer*4	w_fs3_down
	external	w_fs3_down
	integer*4	w_fs3_next_field
	external	w_fs3_next_field
	integer*4	w_fs3_prev_field
	external	w_fs3_prev_field
	integer*4	w_fs3_year_after
	external	w_fs3_year_after
	integer*4	w_fs3_doy_after
	external	w_fs3_doy_after
	integer*4	w_fs3_month_after
	external	w_fs3_month_after
	integer*4	w_fs3_day_after
	external	w_fs3_day_after
	integer*4	w_fs3_hour_after
	external	w_fs3_hour_after
	integer*4	w_fs3_minute_after
	external	w_fs3_minute_after
	integer*4	w_fs3_set_object_pointer
	external	w_fs3_set_object_pointer
	integer*4	w_fs3_do_routine
	external	w_fs3_do_routine
	integer*4	w_fs3_help_routine
	external	w_fs3_help_routine
	integer*4	w_fs3_pf3
	external	w_fs3_pf3

	setup_fs3_fields = 0

	! get the last-file-used
!	ok = get_logical_name_str(wind_last_file_used, f(1).str)

	! get initial date and time values
	call idate(month,day,year)
	if (year .gt. 91) then
	   year = year + 1900
	else
	   year = year + 2000
	end if
	doy = iday_of_year(year,month,day)
	default_year = year
	call time(ctime)
	read(ctime,'(i2,1x,i2)',iostat=o) hour, minute
	call w_ur8_from_ymd(ur8a,year,month,day,hour,minute,0,0)
	write(s4,ur8_fmt,iostat=o) ur8a
	ur8b = ur8a

	!
	! start times
	!

	n = 1
	row = chooser_start_line + 3
	col = 22
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).obj  = %loc(f(n))
	g(n).after = %loc(w_fs3_year_after)
	write(f(n).str(1:4),'(i4.4)',iostat=ios) year		! year
	f(n).col = col
	f(n).row = row
	f(n).siz = 4
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_doy_after)
	g(n).obj  = %loc(f(n))
	write(f(n).str(1:3),'(i3.3)',iostat=ios) doy		! day of year
	f(n).col = f(n-1).col + f(n-1).siz + 2
	f(n).row = row
	f(n).siz = 3
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_month_after)
	g(n).obj  = %loc(f(n))
	write(f(n).str(1:2),'(i2.2)',iostat=ios) month		! month
	f(n).col = f(n-1).col + f(n-1).siz + 2 + 1
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_day_after)
	g(n).obj  = %loc(f(n))
	write(f(n).str(1:2),'(i2.2)',iostat=ios) day		! day of month
	f(n).col = f(n-1).col + f(n-1).siz + 2 + 1
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_hour_after)
	g(n).obj  = %loc(f(n))
	f(n).str = ctime(1:2)					! hour
	f(n).col = f(n-1).col + f(n-1).siz + 2
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_minute_after)
	g(n).obj  = %loc(f(n))
	f(n).str = ctime(4:5)					! minute
	f(n).col = f(n-1).col + f(n-1).siz + 2
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = readonly_type				! ur8 start
	g(n).proc = 0
	g(n).after = 0
	g(n).obj  = %loc(f(n))
	f(n).str = s4
	f(n).col = f(n-1).col + f(n-1).siz + 4
	f(n).row = row
	f(n).siz = 15
	f(n).iva = ' '
	f(n).fva = 'n'

	!
	! stop times
	!

	n = n + 1
	row = chooser_start_line + 4
	col = 22
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).obj  = %loc(f(n))
	g(n).after = %loc(w_fs3_year_after)
	write(f(n).str(1:4),'(i4.4)',iostat=ios) year		! year
	f(n).col = col
	f(n).row = row
	f(n).siz = 4
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_doy_after)
	g(n).obj  = %loc(f(n))
	write(f(n).str(1:3),'(i3.3)',iostat=ios) doy		! day of year
	f(n).col = f(n-1).col + f(n-1).siz + 2
	f(n).row = row
	f(n).siz = 3
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_month_after)
	g(n).obj  = %loc(f(n))
	write(f(n).str(1:2),'(i2.2)',iostat=ios) month		! month
	f(n).col = f(n-1).col + f(n-1).siz + 2 + 1
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_day_after)
	g(n).obj  = %loc(f(n))
	write(f(n).str(1:2),'(i2.2)',iostat=ios) day		! day of month
	f(n).col = f(n-1).col + f(n-1).siz + 2 + 1
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_hour_after)
	g(n).obj  = %loc(f(n))
	f(n).str = ctime(1:2)					! hour
	f(n).col = f(n-1).col + f(n-1).siz + 2
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = edit_at_type
	g(n).proc = %loc(edit_at)
	g(n).after = %loc(w_fs3_minute_after)
	g(n).obj  = %loc(f(n))
	f(n).str = ctime(4:5)					! minute
	f(n).col = f(n-1).col + f(n-1).siz + 2
	f(n).row = row
	f(n).siz = 2
	f(n).iva = 'rb'
	f(n).fva = 'b'

	n = n + 1
	g(n).type = readonly_type				! ur8 stop
	g(n).proc = 0
	g(n).after = 0
	g(n).obj  = %loc(f(n))
	f(n).str = s4
	f(n).col = f(n-1).col + f(n-1).siz + 4
	f(n).row = row
	f(n).siz = 15
	f(n).iva = ' '
	f(n).fva = 'n'

	n_f = n
	n_g = n

	on(1).key = 'CNTL/W'
	on(1).routine = %loc(w_fs3_refresh)
	on(2).key = 'CNTL/Z'
	on(2).routine = %loc(w_fs3_exit)
	on(3).key = 'UP'
	on(3).routine = %loc(w_fs3_up)
	on(4).key = 'DOWN'
	on(4).routine = %loc(w_fs3_down)
	on(5).key = 'CNTL/B'
	on(5).routine = %loc(w_fs3_prev_field)
	on(6).key = 'CNTL/I'
	on(6).routine = %loc(w_fs3_next_field)
	on(7).key = 'CNTL/M'
	on(7).routine = %loc(w_fs3_exit)
	on(8).key = 'NEXT'
	on(8).routine = %loc(w_fs3_set_object_pointer)
	on(9).key = 'PREV'
	on(9).routine = %loc(w_fs3_set_object_pointer)
	on(10).key = 'DO'
	on(10).routine = %loc(w_fs3_do_routine)
	on(11).key = 'SELECT'
	on(11).routine = %loc(w_fs3_do_routine)
	on(12).key = 'HELP'
	on(12).routine = %loc(w_fs3_help_routine)
	on(13).key = 'PF2'
	on(13).routine = %loc(w_fs3_help_routine)
	on(14).key = 'PF3'
	on(14).routine = %loc(w_fs3_pf3)

	setup_fs3_fields = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	w_fs3_help_routine(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	integer*4	i

	call goto_position(chooser_start_line-1,1)
	call lib$put_output(erase_to_end)
	call goto_position(chooser_start_line,1)

	call lib$put_output('Enter values for Year, Day-of_year, Month, etc.'//
	1 ', or')
	call lib$put_output('use the UP/DOWN Arrow keys to change values.')
	call lib$put_output(' ')
	call lib$put_output('    CNTL/Z - exit without selecting')
	call lib$put_output('    RETURN - accept current values')
	call lib$put_output('    TAB    - move to next field')
	call lib$put_output('    CNTL/B - move to previous field')
	call lib$put_output('    NEXT   - move to same field other line')
	call lib$put_output('    PREV   - move to same field other line')
	call lib$put_output('    PF3    - use date/time specified earlier')
	call lib$put_output('    PF2    - help')
	call lib$put_output(' ')

	call w_fs3_draw_screen()
	call scr_mgr_show_gets(f)

	w_fs3_help_routine = 1

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	w_fs3_do_routine(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi

	w_fs3_do_routine = 0

	call w_fs3_msg('inside DO routine')

	w_fs3_do_routine = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	goto_fs3_bottom()
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	goto_position		! an entry point
	character*8	screen_lower_left
	logical*4	first_time /.true./
	character*8	s8
	integer*4	row,col
	integer*4	o

	if (first_time) then
	   first_time = .false.
	   write(screen_lower_left,3,iostat=o) csi, chooser_bottom_line
	end if
  3	format(a2,i2.2,';1f')
  4	format(a2,i2.2,';',i2.2,'f')

	call putstr(screen_lower_left)
	goto_fs3_bottom = 1
	return

	entry	goto_position(row,col)
	write(s8,4,iostat=o) csi, row, col
	call putstr(s8)
	goto_position = 1
	return
	end
 
!------------------------------------------------------------------------------
	integer*4	function	w_fs3_refresh(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	call goto_position(chooser_start_line,1)
!	call lib$put_output(cls)
	call lib$put_output(erase_to_end)
	call lib$put_output(w_fs3_screen(:w_fs3_screen_size))
	call scr_mgr_show_gets(f)
	w_fs3_refresh = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_exit(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	! return low bit clear for screen exit
	w_fs3_exit = 0
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_err(str)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	character*(*)	str
	integer*4	k
	integer*4	k2len
	! just print a message at bottom
	call goto_fs3_bottom
	k = k2len(str)
	if (k .eq. 0) then
	   call putstr(erase_line)
	else
	   call putstr(beep//erase_line//str(:k))
	end if
	w_fs3_err = 0
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_msg(str)
	implicit	none
	include		'fs3_def.for/nolist'
	character*(*)	str
	integer*4	k
	integer*4	k2len
	! just print a message at bottom
	call goto_fs3_bottom
	k = k2len(str)
	if (k .eq. 0) then
	   call putstr(erase_line)
	else
	   call putstr(erase_line//str(:k))
	end if
	w_fs3_msg = 0
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_next_field(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	integer*4	i,j

	w_fs3_next_field = 1

	! cycle to next field
	xi = xi + 1
	if (xi .gt. n_g) xi = 1
!	if (g(xi).type .lt. 1) xi = 1
	i = 0
	j = xi
	do while(g(xi).type .eq. readonly_type)
           i = i + 1
	   if (i .gt. 2*n_g) then
	      call w_fs3_err('Err: Cannot find next field.')
	      xi = j
              return
	   end if
	   xi = xi + 1
	   if (xi .gt. n_g) xi = 1
!	   if (g(xi).type .lt. 1) xi = 1
	end do
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_prev_field(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	integer*4	i,j

	w_fs3_prev_field = 1

	! cycle to previous field
	xi = xi - 1
	if (xi .lt. 1) xi = n_g
	i = 0
	j = xi
	do while(g(xi).type .eq. readonly_type)
           i = i + 1
	   if (i .gt. 2*n_g) then
	      call w_fs3_err('Err: Cannot find prev field.')
	      xi = j
              return
	   end if
	   xi = xi - 1
	   if (xi .lt. 1) xi = n_g
	end do
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_up(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	call w_fs3_prev_field(xi)
	w_fs3_up = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_down(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	call w_fs3_next_field(xi)
	w_fs3_down = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_set_object_pointer(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	if (g(xi).retkey .eq. 'NEXT') then
	   if (xi .gt. 7) then
	      xi = xi - 7
	   else
	      xi = xi + 7
	   end if
	else if (g(xi).retkey .eq. 'PREV') then
	   if (xi .gt. 7) then
	      xi = xi - 7
	   else
	      xi = xi + 7
	   end if
	else if (g(xi).retkey .eq. 'CNTL/I') then
	   call w_fs3_prev_field(xi)
	else if (g(xi).retkey .eq. 'CNTL/M') then
	   call w_fs3_next_field(xi)
	else
	   call w_fs3_err('Cannot set object pointer (G).')
	end if
	w_fs3_set_object_pointer = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	w_fs3_build_screen()
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	i,j,k,m,n,o
	integer*4	k2len
	integer*4	make_box
	integer*4	make_line
	integer*4	ok
	character	box*1024
	integer*4	len_box

	parameter	v1=
	1'Year  DOY  Mon  Day  Hr  Mn    (UR8 equivalent)'
	parameter	v2=
	1'----  ---  ---  ---  --  --    ----------------'
	parameter	v3='Start Time:'
	parameter	v4=' Stop Time:'
	character*8	pv1, pv2, pv3, pv4
	character*1024	a

	integer*4	r1, c1 /1/, r2, c2 /78/

	w_fs3_build_screen = 0
	j = len(selg1)
	w_fs3_screen(:j) = selg1

	r1 = chooser_start_line
	r2 = chooser_start_line + chooser_row_height - 1

!	i = j + 1
!	j = i + len(bright) - 1
!	w_fs3_screen(i:j) = bright
	i = j + 1
	ok = make_box(r1,c1,r2,c2,w_fs3_screen(i:),len_box)

	i = j + len_box + 1
	j = i + len(primary) - 1
	w_fs3_screen(i:j) = primary

  3	format(a2,i2.2,';',i2.2,'f')
	write(pv1,3,iostat=o) csi, chooser_start_line + 1, 22
	write(pv2,3,iostat=o) csi, chooser_start_line + 2, 22
	write(pv3,3,iostat=o) csi, chooser_start_line + 3, 09
	write(pv4,3,iostat=o) csi, chooser_start_line + 4, 09

	a = pv1//v1//pv2//v2//pv3//v3//pv4//v4

	i = j + 1
	k = k2len(a)
	j = i + k - 1
	w_fs3_screen(i:j) = a(1:k)

	w_fs3_screen_size = j

	w_fs3_build_screen = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_various_time_afters(x,xi)
	implicit	none
	include		'fs3_def.for/nolist'
	record	/tv_field/ x
	integer*4	xi
	integer*4	i,j,k,m,n,o
	integer*4	k2len
	integer*4	w_fs3_year_after	! an entry point
	integer*4	w_fs3_doy_after		! an entry point
	integer*4	w_fs3_month_after	! an entry point
	integer*4	w_fs3_day_after		! an entry point
	integer*4	w_fs3_hour_after	! an entry point
	integer*4	w_fs3_minute_after	! an entry point
	integer*4	year
	integer*4	month
	integer*4	day
	integer*4	iday_of_year		! a function
	integer*4	ndays_in_month		! a function
	integer*4	is_a_leap_year		! a function
	integer*4	max_days
	integer*4	i_yr	! year
	integer*4	i_dy	! day of year
	integer*4	i_mo	! month
	integer*4	i_da	! day of month
	integer*4	i_hr	! hour
	integer*4	i_mn	! minute


	!----------------------------------------------------------------------
	entry	w_fs3_year_after(x,xi)
	w_fs3_year_after = 0
	f(xi).str(5:) = ' '
	read(f(xi).str(:4),'(i4)',iostat=o,err=10) n
	if (f(xi).retkey .eq. 'UP') then
	   n = n + 1
	   if (n.gt.9999) n = 0
	   write(f(xi).str(:4),'(i4.4)',iostat=o) n
	   f(xi).chgflg = 1
	   call scr_mgr_show_one_get(f(xi))
	else if (f(xi).retkey .eq. 'DOWN') then
	   n = n - 1
	   if (n.lt.0) n = 9999
	   write(f(xi).str(:4),'(i4.4)',iostat=o) n
	   f(xi).chgflg = 1
	   call scr_mgr_show_one_get(f(xi))
	else
	   w_fs3_year_after = 1
	end if	
	call update_related_fields(xi)
	return

	!----------------------------------------------------------------------
	entry	w_fs3_doy_after(x,xi)
	w_fs3_doy_after = 0
	i_yr = xi - 1
        i_mo = xi + 1
	i_da = xi + 2
	! get the year for updating the month and day-of-month fields
	read(f(i_yr).str(:4),4,iostat=o) year
	if (o .ne. 0) then
	   ! take care of bad year value
	   year = default_year
	   write(f(i_yr).str(:4),4) year
	   call scr_mgr_show_one_get(f(i_yr))
	end if
	! adjust the julian day for leap  year
	max_days = 365
	if (is_a_leap_year(year)) max_days = 366
	f(xi).str(4:) = ' '
	read(f(xi).str(:3),'(i3)',iostat=o,err=10) n
	if (f(xi).retkey .eq. 'UP') then
	   n = n + 1
	   if (n.gt.max_days) n = 1
	   f(xi).chgflg = 1
	else if (f(xi).retkey .eq. 'DOWN') then
	   n = n - 1
	   if (n.lt.1) n = max_days
	   f(xi).chgflg = 1
	else
	   w_fs3_doy_after = 1
	   if (n.gt.max_days) then
	      n = max_days
	      f(xi).chgflg = 1
	   else if (n.lt.1) then
	      n = 1
	      f(xi).chgflg = 1
	   end if
	end if
	write(f(xi).str(:3),3,iostat=o) n
	call scr_mgr_show_one_get(f(xi))

	call julian_to_mmdd(n, year, month, day)
	write(f(i_mo).str(:2),2,iostat=o) month
	call scr_mgr_show_one_get(f(i_mo))
	write(f(i_da).str(:2),2,iostat=o) day
	call scr_mgr_show_one_get(f(i_da))
	call update_related_fields(xi)
	return

	!----------------------------------------------------------------------
	entry	w_fs3_month_after(x,xi)
	w_fs3_month_after = 0
	f(xi).str(3:) = ' '
	read(f(xi).str(:2),2,iostat=o,err=10) n
	if (f(xi).retkey .eq. 'UP') then
	   n = n + 1
	   if (n.gt.12) n = 1
	   f(xi).chgflg = 1
	else if (f(xi).retkey .eq. 'DOWN') then
	   n = n - 1
	   if (n.lt.1) n = 12
	   f(xi).chgflg = 1
	else
	   w_fs3_month_after = 1
	   if (n.gt.12) then
	      n = 12
	      f(xi).chgflg = 1
	   else if (n.lt.1) then
	      n = 1
	      f(xi).chgflg = 1
	   end if
	end if
	write(f(xi).str(:2),2,iostat=o) n
	call scr_mgr_show_one_get(f(xi))
	month = n
	i_yr = xi - 2
	i_da = xi + 1
	i_dy = xi - 1
	! get the year for updating the day_of_year and day-of-month fields
	read(f(i_yr).str(:4),4,iostat=o) year
	if (o .ne. 0) then
	   ! take care of bad year value
	   year = default_year
	   write(f(i_yr).str(:4),4) year
	   call scr_mgr_show_one_get(f(i_yr))
	end if
	! get the current day of month for updating
	read(f(i_da).str(:2),2,iostat=o) day
	if (o.ne.0) then
	   ! take care of bad day value
	   day = 1
	   write(f(i_da).str(:2),2) day 
	   call scr_mgr_show_one_get(f(i_da))
	end if
	n = ndays_in_month(year,month)
	if (day.gt.n) then
	   day = n
	   write(f(i_da).str(:2),2,iostat=o) day
	   call scr_mgr_show_one_get(f(i_da))
	end if
	! get the julian day for updating
	j = iday_of_year(year,month,day)
	write(f(i_dy).str(:3),3,iostat=o) j
	call scr_mgr_show_one_get(f(i_dy))
	call update_related_fields(xi)
	return

	!----------------------------------------------------------------------
	entry	w_fs3_day_after(x,xi)
	w_fs3_day_after = 0
	i_yr = xi - 3
	i_dy = xi - 2
	i_mo = xi - 1
	! get the year for updating the various fields
	read(f(i_yr).str(:4),4,iostat=o) year
	if (o .ne. 0) then
	   ! take care of bad year value
	   year = default_year
	   write(f(i_yr).str(:4),4) year
	   call scr_mgr_show_one_get(f(i_yr))
	end if
	! get the month for updating the various fields
	read(f(i_mo).str(:2),2,iostat=o) month
	if (o .ne. 0) then
	   ! take care of bad year value
	   month = 1
	   write(f(i_mo).str(:2),2) month
	   call scr_mgr_show_one_get(f(i_mo))
	end if
	max_days = ndays_in_month(year,month)
	! process the day of month field
	f(xi).str(3:) = ' '
	read(f(xi).str(:2),2,iostat=o,err=10) n
	if (f(xi).retkey .eq. 'UP') then
	   n = n + 1
	   if (n.gt.max_days) n = 1
	   f(xi).chgflg = 1
	else if (f(xi).retkey .eq. 'DOWN') then
	   n = n - 1
	   if (n.lt.1) n = max_days
	   f(xi).chgflg = 1
	else
	   w_fs3_day_after = 1
	   if (n.gt.max_days) then
	      n = max_days
	      f(xi).chgflg = 1
	   else if (n.lt.1) then
	      n = 1
	      f(xi).chgflg = 1
	   end if
	end if
	write(f(xi).str(:2),2,iostat=o) n
	call scr_mgr_show_one_get(f(xi))
	day = n
	! get the julian day for updating
	j = iday_of_year(year,month,day)
	write(f(i_dy).str(:3),3,iostat=o) j
	call scr_mgr_show_one_get(f(i_dy))
	call update_related_fields(xi)
	return

	!----------------------------------------------------------------------
	entry	w_fs3_hour_after(x,xi)
	w_fs3_hour_after = 0
	f(xi).str(3:) = ' '
	read(f(xi).str(:2),'(i2)',iostat=o,err=10) n
	if (f(xi).retkey .eq. 'UP') then
	   n = n + 1
	   if (n.gt.23) n = 0
	   f(xi).chgflg = 1
	else if (f(xi).retkey .eq. 'DOWN') then
	   n = n - 1
	   if (n.lt.0) n = 23
	   f(xi).chgflg = 1
	else
	   w_fs3_doy_after = 1
	   if (n.gt.23) then
	      n = 23
	      f(xi).chgflg = 1
	   else if (n.lt.0) then
	      n = 0
	      f(xi).chgflg = 1
	   end if
	end if
	write(f(xi).str(:2),2,iostat=o) n
	call scr_mgr_show_one_get(f(xi))

	call update_related_fields(xi)
	return

	!----------------------------------------------------------------------
	entry	w_fs3_minute_after(x,xi)
	w_fs3_minute_after = 0
	f(xi).str(3:) = ' '
	read(f(xi).str(:2),'(i2)',iostat=o,err=10) n
	if (f(xi).retkey .eq. 'UP') then
	   n = n + 1
	   if (n.gt.59) n = 0
	   f(xi).chgflg = 1
	else if (f(xi).retkey .eq. 'DOWN') then
	   n = n - 1
	   if (n.lt.0) n = 59
	   f(xi).chgflg = 1
	else
	   w_fs3_doy_after = 1
	   if (n.gt.59) then
	      n = 59
	      f(xi).chgflg = 1
	   else if (n.lt.0) then
	      n = 0
	      f(xi).chgflg = 1
	   end if
	end if
	write(f(xi).str(:2),2,iostat=o) n
	call scr_mgr_show_one_get(f(xi))

	call update_related_fields(xi)
	return

 10	continue
  2	format(i2.2)
  3	format(i3.3)
  4	format(i4.4)
	call w_fs3_err('Invalid integer value: '//f(xi).str(:4))
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	update_related_fields(p)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	p
	integer*4	i,j,k,m,n,o,q
	integer*4	year, month, day, doy, hour, minute
	integer*4	max_days
	integer*4	i_yr	! year
	integer*4	i_dy	! day of year
	integer*4	i_mo	! month
	integer*4	i_da	! day of month
	integer*4	i_hr	! hour
	integer*4	i_mn	! minute
	integer*4	i_ur8   ! ur8 field index
	real*8		r8

	if (p .ge. 1 .and. p .le. 6) then
	   i_yr  = 1
	   i_dy  = 2
           i_mo  = 3
	   i_da  = 4
	   i_hr  = 5
	   i_mn  = 6
	   i_ur8 = 7
	else
	   i_yr  = 8
	   i_dy  = 9
           i_mo  = 10
	   i_da  = 11
	   i_hr  = 12
	   i_mn  = 13
	   i_ur8 = 14
	end if

	read(f(i_yr).str(:4),4,iostat=o) year
	read(f(i_mo).str(:2),2,iostat=o) month
	read(f(i_da).str(:2),2,iostat=o) day
	read(f(i_hr).str(:2),2,iostat=o) hour
	read(f(i_mn).str(:2),2,iostat=o) minute

	call w_ur8_from_ymd(r8,year,month,day,hour,minute,0,0)
	if (p .ge. 1 .and. p .le. 6) then
	   ur8a = r8
	else
	   ur8b = r8
	end if
	write(f(i_ur8).str(1:16),ur8_fmt,iostat=o) r8
	call scr_mgr_show_one_get(f(i_ur8))

	if (year .lt. 1982) call w_fs3_err(
	1'Ulysses REAL*8 time invalid for dates before 1982')

	update_related_fields = 1
	return
  2	format(i2.2)
  3	format(i3.3)
  4	format(i4.4)
	end

!------------------------------------------------------------------------------
	integer*4	function	w_fs3_pf3(xi)
	implicit	none
	include		'fs3_def.for/nolist'
	integer*4	xi
	integer*4	year, month, day, doy, hour, minute, second, msec
	integer*4	iday_of_year		! a function
	integer*4	i_yr	! year
	integer*4	i_dy	! day of year
	integer*4	i_mo	! month
	integer*4	i_da	! day of month
	integer*4	i_hr	! hour
	integer*4	i_mn	! minute
	integer*4	i_ur8   ! ur8 field index
	real*8		r8
	integer*4	o

	w_fs3_pf3 = 1
	if (prev_ur8a .le. 0) then
	   call w_fs3_err('No previous times availble for this session.')
	   return
	else
	   call w_fs3_err('Using previous times.')
	end if

	i_yr  = 1
	i_dy  = 2
        i_mo  = 3
	i_da  = 4
	i_hr  = 5
	i_mn  = 6
	i_ur8 = 7
	ur8a = prev_ur8a
	r8 = ur8a
	call w_ur8_to_ymd(r8,year,month,day,hour,minute,second,msec)
	doy = iday_of_year(year,month,day)
	write(f(i_ur8).str(1:16),ur8_fmt,iostat=o) r8
	write(f(i_yr).str(:4),4,iostat=o) year
	write(f(i_dy).str(:3),3,iostat=o) doy
	write(f(i_mo).str(:2),2,iostat=o) month
	write(f(i_da).str(:2),2,iostat=o) day
	write(f(i_hr).str(:2),2,iostat=o) hour
	write(f(i_mn).str(:2),2,iostat=o) minute

	i_yr  = 8
	i_dy  = 9
        i_mo  = 10
	i_da  = 11
	i_hr  = 12
	i_mn  = 13
	i_ur8 = 14
	ur8b = prev_ur8b
	r8 = ur8b
	call w_ur8_to_ymd(r8,year,month,day,hour,minute,second,msec)
	doy = iday_of_year(year,month,day)
	write(f(i_ur8).str(1:16),ur8_fmt,iostat=o) r8
	write(f(i_yr).str(:4),4,iostat=o) year
	write(f(i_dy).str(:3),3,iostat=o) doy
	write(f(i_mo).str(:2),2,iostat=o) month
	write(f(i_da).str(:2),2,iostat=o) day
	write(f(i_hr).str(:2),2,iostat=o) hour
	write(f(i_mn).str(:2),2,iostat=o) minute

	call scr_mgr_show_gets(f)

	return
  2	format(i2.2)
  3	format(i3.3)
  4	format(i4.4)
	end
