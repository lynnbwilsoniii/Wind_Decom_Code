! file_select2.for - WIND/WAVES offline file selection screen program
! [VMS only]
!
!
	options/extend_source
!------------------------------------------------------------------------------
! This funciton serves as the coarse control routine for manipulating the
! major modules of the full screen file selection software.
! Returns 1 if a file was selected or 0 otherwise.
!------------------------------------------------------------------------------
	integer*4	function	w_select_filename(file)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	character*(*)	file
	integer*4	ok
	integer*4	manage_screen
	integer*4	my_err
	external	my_err
	integer*4	w_fs4

	w_select_filename = 0
	file = ' '

	! this loop is a horrible kludge to make chooser4/5 crudely callable
	! from chooser2
	use_other_chooser = .true.
	do while (use_other_chooser)
	   use_other_chooser = .false.
	   call reset_file_list()
	   call reset_dir_list()

	   call putstr(soft_reset)
	   call setup_screen_fields()
	   call my_build_screen()
	   call my_refresh()
	   ok = manage_screen(g,on,my_err)
	   call post_screen()
	   if (use_other_chooser) then
	      w_select_filename = w_fs4(file)
	   else if (ok) then
	      file = result_file
	      w_select_filename = 1
	   end if
	end do

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Performs cleanup tasks after the user has exited the file selection screen.
! Sets a VMS process logical name to save the name of the last file selected
! for subsequent direct recall via screen "hot keys".
!------------------------------------------------------------------------------
	integer*4	function	post_screen()
	implicit	none
	include		'($lnmdef)'
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ok
	integer*4	lib$set_logical
	integer*4	nstr
	integer*4	k2len
	structure	/item_list_3/
	   integer*2	buflen
	   integer*2	itmcod
	   integer*4	bufadr
	   integer*4	rlnadr
	end structure
	record		/item_list_3/ il3(2)

	post_screen = 0

	! screen reset code
	call putstr(soft_reset)
	! csi//'1;99r' means set top and bottom scroll margins
	! csi//'25;1f' means position to row 25 column 1
	call putstr(csi//'1;r'//csi//'25;1f')

!	nstr = k2len(result_file)
	nstr = max(1,index(result_file,' '))
	if (result_file .eq. 'REALTIME' .or. result_file .eq. 'nrt') then
	   post_screen = 1
	   return
	end if

	if (k2len(result_file) .lt. 1) return

	il3(1).buflen = nstr
	il3(1).itmcod = lnm$_string
	il3(1).bufadr = %loc(result_file)
	il3(1).rlnadr = 0

	il3(2).buflen = 0
	il3(2).itmcod = 0
	il3(2).bufadr = 0
	il3(2).rlnadr = 0

!	ok = sys$crelnm(,			! attr 
!	1		'LNM$PROCESS_TABLE',	! tabnam
!	1		wind_last_file_used,	! lognam
!	1		,			! acmode
!	1		il3)			! itmlst
	ok = lib$set_logical(
	1		wind_last_file_used,	! logical name
	1		result_file(:nstr),	! equivalence string
	1		'LNM$PROCESS_TABLE',	! table name
	1		,			! attributes
	1		)			! item list
	if (.not. ok) then
	   type '(1x,a,a,a,z8.8,a)',
	1	'Cannot define ', wind_last_file_used,
	1	' in process table, status:', ok, '.'
!	else
!	   type '(1x,5(a),z8.8)', 
!	1	'Defined ',wind_last_file_used,' to ',
!	1	result_file(:nstr),', ok=',ok
	end if

	post_screen = 1

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Initializes data structures used by screen manager to control the screen.
! Defines field entry and exit routines and various "hot keys".
!------------------------------------------------------------------------------
	integer*4	function	setup_screen_fields()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ok
	integer*4	get_logical_name_str		! a function
	integer*4	get_search_list			! a function
	integer*4	edit_at
	external	edit_at
	integer*4	scroll_this
	external	scroll_this
	integer*4	my_err
	external	my_err
	integer*4	my_refresh
	external	my_refresh
	integer*4	my_exit
	external	my_exit
	integer*4	my_up
	external	my_up
	integer*4	my_down
	external	my_down
	integer*4	my_next_field
	external	my_next_field
	integer*4	my_prev_field
	external	my_prev_field
	integer*4	my_file_data
	external	my_file_data
	integer*4	get_my_file_list
	external	get_my_file_list
	integer*4	my_dir_list
	external	my_dir_list
	integer*4	get_my_dir_list
	external	get_my_dir_list
	integer*4	my_dir_list_after
	external	my_dir_list_after
	integer*4	my_set_object_pointer
	external	my_set_object_pointer
	integer*4	my_scroll_on_key_handler
	external	my_scroll_on_key_handler
	integer*4	my_file_list_after
	external	my_file_list_after
	integer*4	my_do_routine
	external	my_do_routine
	integer*4	my_help_routine
	external	my_help_routine
	integer*4	my_up_one_dir_lev_routine
	external	my_up_one_dir_lev_routine
	integer*4	my_select_realtime
	external	my_select_realtime
	integer*4	my_select_near_realtime
	external	my_select_near_realtime
	integer*4	my_select_lastfile
	external	my_select_lastfile
	integer*4	my_select_by_prompt
	external	my_select_by_prompt
	integer*4	my_size_toggle
	external	my_size_toggle
	integer*4	display_last_file_used
	external	display_last_file_used
	integer*4	display_user_message
	external	display_user_message
	integer*4	use_virtual_dir
	external	use_virtual_dir
	integer*4	i_g, i_f, i_s

	setup_screen_fields = 0

	! establish the current directory
!	ok = get_file_dev_logical_name_str(default_dir, current)
	! get search list directories
	n_search_list_entries = max_search_list_entries
	ok = get_search_list(default_dir, search_list, n_search_list_entries)
	if (ok .ne. 1) then
	   call my_err('Error getting search list: '//default_dir)
	end if
	got_search_list = .true.
	current = search_list(1)
!	current = default_dir
	call update_top_status_line()
	ok = get_logical_name_str(wind_last_file_used, last_file_used)

	! directory name scroll box
	i_g = 1
	i_s = 1
	g(i_g).type = scroll_this_type
	g(i_g).before = %loc(get_my_dir_list)
	g(i_g).proc = %loc(scroll_this)
	g(i_g).after = %loc(my_dir_list_after)
	g(i_g).obj  = %loc(s(i_s))
	g(i_g).errhdlr = %loc(my_err)
	s(i_s).flags    = top_is_sticky .or. bottom_moves_to_next_field
	s(i_s).r1	= 3
	s(i_s).r2	= 8
	s(i_s).idz	= 6
	s(i_s).gtscrldt	= %loc(my_dir_list)
	s(i_s).on_key	= %loc(my_scroll_on_key_handler)
	s(i_s).retkey   = ' '
	s(i_s).xr       = 0
	s(i_s).idw      = 0
	s(i_s).idx      = 0
	s(i_s).idy      = 0
	s(i_s).idz      = 0

	! file name scroll box
	i_g = 2
	i_s = 2
	g(i_g).type = scroll_this_type
	g(i_g).before = %loc(get_my_file_list)
	g(i_g).proc = %loc(scroll_this)
	g(i_g).after = %loc(my_file_list_after)
	g(i_g).obj  = %loc(s(i_s))
	g(i_g).errhdlr = %loc(my_err)
	s(i_s).flags    = top_moves_to_prev_field .or. bottom_is_sticky
	s(i_s).r1	= 10
	s(i_s).r2	= 21
	s(i_s).idz	= 35
	s(i_s).gtscrldt	= %loc(my_file_data)
	s(i_s).on_key	= %loc(my_scroll_on_key_handler)
	s(i_s).retkey   = ' '
	s(i_s).xr       = 0
	s(i_s).idw      = 0
	s(i_s).idx      = 0
	s(i_s).idy      = 0
	s(i_s).idz      = 0

	! get the last-file-used
	i_f = 0
!	i_g = 3
!	i_f = 1
!	ok = get_logical_name_str(wind_last_file_used, f(1).str)
!	g(i_g).type = edit_at_type
!	g(i_g).proc = %loc(edit_at)
!	g(i_g).obj  = %loc(f(i_f))
!	f(i_f).col = 12
!	f(i_f).row = 23
!	f(i_f).siz = 64
!	f(i_f).iva = 'rb'
!	f(i_f).fva = 'b'
!
	n_f = i_f
	n_g = i_g
	n_s = 2

	on(1).key = 'CNTL/W'
	on(1).routine = %loc(my_refresh)
	on(2).key = 'CNTL/Z'
	on(2).routine = %loc(my_exit)
	on(3).key = 'UP'
	on(3).routine = %loc(my_up)
	on(4).key = 'DOWN'
	on(4).routine = %loc(my_down)
	on(5).key = 'CNTL/B'
	on(5).routine = %loc(my_prev_field)
	on(6).key = 'CNTL/I'
	on(6).routine = %loc(my_next_field)
	on(7).key = 'CNTL/M'
	on(7).routine = %loc(my_exit)
	on(8).key = 'NEXT'
	on(8).routine = %loc(my_set_object_pointer)
	on(9).key = 'PREV'
	on(9).routine = %loc(my_set_object_pointer)
	on(10).key = 'DO'
	on(10).routine = %loc(my_do_routine)
	on(11).key = 'SELECT'
	on(11).routine = %loc(my_do_routine)
	on(12).key = 'HELP'
	on(12).routine = %loc(my_help_routine)
	on(13).key = 'H'
	on(13).routine = %loc(my_help_routine)
	on(14).key = 'h'
	on(14).routine = %loc(my_help_routine)
	on(15).key = '?'
	on(15).routine = %loc(my_help_routine)
	on(16).key = 'U'
	on(16).routine = %loc(my_up_one_dir_lev_routine)
	on(17).key = 'u'
	on(17).routine = %loc(my_up_one_dir_lev_routine)
	on(18).key = 'R'
	on(18).routine = %loc(my_select_realtime)
	on(19).key = 'r'
	on(19).routine = %loc(my_select_realtime)
	on(20).key = 'L'
	on(20).routine = %loc(my_select_lastfile)
	on(21).key = 'l'
	on(21).routine = %loc(my_select_lastfile)
	on(22).key = 'P'
	on(22).routine = %loc(my_select_by_prompt)
	on(23).key = 'p'
	on(23).routine = %loc(my_select_by_prompt)
	on(24).key = 'I'
	on(24).routine = %loc(my_size_toggle)
	on(25).key = 'i'
	on(25).routine = %loc(my_size_toggle)

	on(26).key = 'K'
	on(26).routine = %loc(display_last_file_used)
	on(27).key = 'k'
	on(27).routine = %loc(display_last_file_used)

	on(28).key = 'M'
	on(28).routine = %loc(display_user_message)
	on(29).key = 'm'
	on(29).routine = %loc(display_user_message)

	on(29).key = 'V'
	on(29).routine = %loc(use_virtual_dir)
	on(30).key = 'v'
	on(30).routine = %loc(use_virtual_dir)

	on(31).key = 'N'
	on(31).routine = %loc(my_select_near_realtime)
	on(32).key = 'n'
	on(32).routine = %loc(my_select_near_realtime)

	! keys must also be registered in my_scroll_on_key_handler

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This routine calls lib$put_output to print several lines of helpful
! screen usage information.  It is intended to be invoked when the keyboard's
! HELP key is pressed.  Hot keys described in the help text should match
! those actually coded in routine setup_screen_fields().
!------------------------------------------------------------------------------
	integer*4	function	my_help_routine(xi)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	parameter	blank_line=' '
	character*8	dummy

	call lib$put_output(soft_reset)
	call lib$put_output(cls)
	call lib$put_output(home)
	call lib$put_output(' WIND/WAVES Telemetry File Selection Help Screen')
	call lib$put_output(blank_line)
	call lib$put_output(' Key          Function')
	call lib$put_output(' ----------   ----------------------------')
	call lib$put_output(' NEXT         Show next screen of files or dirs')
	call lib$put_output(' PREV         Show previous screen of files or dirs')
	call lib$put_output(' UP Arrow     Move selection bar down one line')
	call lib$put_output(' Down Arrow   Move selection bar up one line')
	call lib$put_output(' Return       Select current file or directory')
	call lib$put_output(' Select       Select current file or directory')
	call lib$put_output(' U or u       Move up one directory level')
	call lib$put_output(' Tab          Move to next scrolling window')
	call lib$put_output(' K or k       Display file name used during previous run this process')
	call lib$put_output(' L or l       Select same file as in previous run this process')
	call lib$put_output(' F or f       Prompt for file name specification')
	call lib$put_output(' I or i       Information on files')
	call lib$put_output(' R or r       Select Realtime TM stream')
	call lib$put_output(' N or n       Invoke Near Realtime CDHF client')
	call lib$put_output(' V or v       Toggle virtual directory display')
	call lib$put_output(' Cntl-W       Refresh screen')
	call lib$put_output(' Cntl-Z       Exit screen, no selection made')
	call lib$put_output(blank_line)
	call lib$put_output(blank_line)
	call lib$put_output(' Press any key to continue...')
	call get_key(dummy)
	call my_refresh(xi)

!	if (xi .eq. i_lf) then
!	   call my_msg('Press RETURN to accept this file, CNTL/Z to quit.')
!	else if (xi .eq. i_sdir) then
!	   call my_msg('Use arrows to move bar, press DO to change directory.')
!	else if (xi .eq. i_sdir) then
!	   call my_msg('Use arrows to move bar, press DO to select file.')
!	end if

	my_help_routine = 1

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Toggles a global logical variable to flag a file chooser change
!------------------------------------------------------------------------------
	integer*4	function	use_virtual_dir(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi

	use_virtual_dir = 0	! return low bit clear for screen exit

	use_other_chooser = .not. use_other_chooser

	if (xi .eq. 0 .or. xi .ne. 0) ! dummy test for compiler
	1 call my_msg('inside use_virtual_dir')

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! A generic "do" routine continueing to linger in the module for anticipated
! future debugging efforts.
!------------------------------------------------------------------------------
	integer*4	function	my_do_routine(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi

	my_do_routine = 0

	if (xi .eq. 0 .or. xi .ne. 0) ! dummy test
	1 call my_msg('inside my_do_routine')

	my_do_routine = 1

	return
	end

!------------------------------------------------------------------------------
! Transmits escape sequence to go to bottom of screen.
!------------------------------------------------------------------------------
	integer*4	function	goto_bottom()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	call putstr(csi//'24;1f')
	goto_bottom = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Writes a status line at top of screen that contains the current dir and date.
!------------------------------------------------------------------------------
	integer*4	function	update_top_status_line() 
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	pos=csi//'1;1f'
	character*78	line
	integer*4	j
	integer*4	k2len

	line = ' '
	j = max(k2len(current),1)
	line = ' Current Directory is '//current(:j)
	call date(line(69:77))
	line(78:78) = ' '
	call lib$put_output(reverse//pos//line//primary)	

	update_top_status_line = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Writes a line at screen bottom containing some usefull hot key definitions.
!------------------------------------------------------------------------------
	integer*4	function	update_bottom_status_line() 
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	pos=csi//'23;1f'
	character*256	line
	integer*4	j,k
	integer*4	k2len

	line = ' '
	j = max(k2len(current),1)
	line = ' '//bright//'OPTIONS:  '//primary//
	1	bright//'H'//primary//'elp  '//
	1	bright//'U'//primary//'p  '//
	1	bright//'R'//primary//'ealtime  '//
	1	bright//'L'//primary//'astFile  '//
	1	bright//'P'//primary//'rompt  '//
	1	bright//'I'//primary//'nfo  '//
	1	bright//'K'//primary//'-ShowLast  '//
	1	bright//'V'//primary//'irtualDir'
	k = k2len(line)
	call lib$put_output(pos//line(:k)//primary)

	update_bottom_status_line = 1
	return
	end

!------------------------------------------------------------------------------
! Redraws the current screen.  Usually invoked after cntl/w or HELP keys.
!------------------------------------------------------------------------------
	integer*4	function	my_refresh()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
!	integer*4	xi		! argument unused
	call lib$put_output(cls)
	call lib$put_output(my_screen(:my_screen_size))
	call update_top_status_line()
	call update_bottom_status_line()
	call scr_mgr_show_gets(f)
	call scr_mgr_show_scrolls(s)
	my_refresh = 1
	return
	end

!------------------------------------------------------------------------------
! Designated screen exit routine.  Always returns zero to signal the screen
! manager to relenquish program control.
!------------------------------------------------------------------------------
	integer*4	function	my_exit()
	implicit	none
	include		'scr_manager_def.for'
!	integer*4	xi		! argument unused
	! return low bit clear for screen exit
	my_exit = 0
	return
	end

!------------------------------------------------------------------------------
! Prints the message passed via argument str at screen bottom.  Also beeps.
!------------------------------------------------------------------------------
	integer*4	function	my_err(str)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	character*(*)	str
	integer*4	k
	integer*4	k2len
	! just print a message at bottom
	call goto_bottom
	k = k2len(str)
	if (k .eq. 0) then
	   call putstr(erase_line)
	else
	   call putstr(beep//erase_line//str(:k))
	end if
	my_err = 0
	return
	end

!------------------------------------------------------------------------------
! Prints message str at screen bottom without beeping.
!------------------------------------------------------------------------------
	integer*4	function	my_msg(str)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	character*(*)	str
	integer*4	k
	integer*4	k2len
	! just print a message at bottom
	call goto_bottom
	k = k2len(str)
	if (k .eq. 0) then
	   call putstr(erase_line)
	else
	   call putstr(erase_line//str(:k))
	end if
	my_msg = 0
	return
	end

!------------------------------------------------------------------------------
! Sets screen field index xi to next field skipping over readonly fields.
!------------------------------------------------------------------------------
	integer*4	function	my_next_field(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi

	! cycle to next field
	xi = xi + 1
	if (g(xi).type .lt. 1) xi = 1
	do while(g(xi).type .eq. readonly_type)
	   xi = xi + 1
	   if (g(xi).type .lt. 1) xi = 1
	   if (xi .gt. n_g) xi = 1
	end do
	my_next_field = 1
	return
	end

!------------------------------------------------------------------------------
! Sets screen field index xi to previous field skipping over readonly fields.
!------------------------------------------------------------------------------
	integer*4	function	my_prev_field(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi

	! cycle to previous field
	xi = xi - 1
	if (xi .lt. 1) xi = n_g
	do while(g(xi).type .eq. readonly_type)
	   xi = xi - 1
	   if (xi .lt. 1) xi = n_g
	end do
	my_prev_field = 1
	return
	end

!------------------------------------------------------------------------------
! Calls my_prev_field to move to previous field on screen.
!------------------------------------------------------------------------------
	integer*4	function	my_up(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	call my_prev_field(xi)
	my_up = 1
	return
	end

!------------------------------------------------------------------------------
! Calls my_next_field to move to next field on screen.
!------------------------------------------------------------------------------
	integer*4	function	my_down(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	call my_next_field(xi)
	my_down = 1
	return
	end

!------------------------------------------------------------------------------
! Sets screen field index pointer xi to point to a screen object based on
! the user's last keystroke.
!------------------------------------------------------------------------------
	integer*4	function	my_set_object_pointer(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi

	if (g(xi).retkey .eq. 'NEXT') then
	   if (xi .eq. i_sdir) then
	      xi = i_sfile
	   else if (xi .eq. i_sfile) then
	      xi = i_sdir
	   else
	      call my_err('Cannot set object pointer (A).')
	   end if
	else if (g(xi).retkey .eq. 'PREV') then
	   if (xi .eq. i_sdir) then
	      xi = i_sfile
	   else if (xi .eq. i_sfile) then
	      xi = i_sdir
	   else
	      call my_err('Cannot set object pointer (B).')
	   end if
	else if (g(xi).retkey .eq. 'CNTL/I') then
	   call my_prev_field(xi)
	else if (g(xi).retkey .eq. 'CNTL/M') then
	   call my_next_field(xi)
	else
	   call my_err('Cannot set object pointer (G).')
	end if
	my_set_object_pointer = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Calls make_box and make_line to build the screen outlines.
!------------------------------------------------------------------------------
	integer*4	function	my_build_screen()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	i,j
	integer*4	make_box
	integer*4	make_line
	integer*4	ok
	integer*4	len_box
!	parameter	f1=csi//'23;1f'//bright//'L'//primary//'ast File:'
!	parameter	f2=csi//'23;70f'//bright//'R'//primary//'ealtime'
!	parameter	a=f1//f2
	integer*4	r1 /2/, c1 /1/, r2 /22/, c2 /78/
!	logical*4	first_time /.true./

	my_build_screen = 0

!	if (.not. first_time) return
!	first_time = .false.

	j = len(selg1)
	my_screen(:j) = selg1

	r1 = 2
	i = j + 1
	ok = make_box(r1,c1,r2,c2,my_screen(i:),len_box)

	r1=9
	i = j + len_box + 1
	j = i - 1
	ok = make_line(r1,c1,r2,c2,my_screen(i:),len_box)

	i = j + len_box + 1
	j = i + len(primary) - 1
	my_screen(i:j) = primary

	my_screen_size = j
	my_build_screen = 1
	return
	end

!------------------------------------------------------------------------------
! Entry my_file_data is called by the scroll-window manager to place the si-th
! completely formatted line item in global buffer s(2).str.
! Entry get_my_file_list is called to initialize the file list with all the
! file names from the target directory.
!------------------------------------------------------------------------------
	integer*4	function	my_file_data(si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	bc=g1on//vc//g1off
	integer*4	si
	integer*4	ok
	integer*4	i,j,k,n
	integer*4	k2len
	integer*4	lib$find_file
	integer*4	lib$find_file_end
	integer*4	context
	integer*4	rms_status
	integer*4	flags
	integer*4	get_my_file_list		! an entry point
	integer*4	reset_file_list			! an entry point
	character*256	prev_dir
	integer*4	rms_no_more_files /'182ca'x/
	integer*4	no_such_file /'0910'x/
	integer*4	file_not_found /'18292'x/
	integer*4	ios
	character*256	file_spec
	character*256	file
	character*80	msg
	integer*4	first_time /1/

	my_file_data = 0
	have_shown_file_list_once = 1

	if (si .lt. 0) then		! bold current line, ++scrobject
	   si = - si
	   write(s(2).str,1) 
	1	bc,bright,
	1	si,files(si),
	1	primary,bc
	else if (si .eq. s(2).idx) then	! current line, show reverse
	   write(s(2).str,1) 
	1	bc,reverse,
	1	si,files(si),
	1	primary,bc
	else if (si .ge. 1 .and. si .le. s(2).idz) then	! not current, normal
	   write(s(2).str,1) 
	1	bc,primary,
	1	si,files(si),
	1	primary,bc
	else if (si .gt. s(2).idz) then		! blank line
	   write(s(2).str,2) 
	1	bc,primary,
	1	' ',
	1	primary,bc
	end if
	s(2).l_str = 7 + 6 + 70 + 7 

	my_file_data = 1
	return
 1	format(a3,a4,' ',i3,'. ',a70,    a4,a3)
 2	format(a3,a4,            a01,75x,a4,a3)


	!----------------------------------------------------------------------
	entry	reset_file_list()
	reset_file_list = 1
	have_shown_file_list_once = 0
	first_time = 1
	prev_dir = ' '
	do i=1,s(2).idz
	   files(i) = ' '
	end do
	s(2).idz = 0
	s(2).xr  = 0
	return

	!----------------------------------------------------------------------
	entry	get_my_file_list()
	get_my_file_list = 1
	if ((prev_dir .eq. current) .and. 
	1   (.not. first_time)      .and.
	1   (.not. newly_including_sizes) ) return
	newly_including_sizes = 0
	first_time = 0
	flags = 0 ! had 2 ! had 'fe'x
	if (context .ne. 0) then
	   ok = lib$find_file_end(context)
	end if
	context = 0
	j = k2len(current)
	if (j.lt.1) then
	   current = default_dir 
	   j = sizeof(default_dir)
	end if
	prev_dir = current
	file_spec = current(:j)//default_mask
	k = j + sizeof(default_mask)
	n = 0
	s(2).idz = 0
	s(2).xr  = 0
	files(1) = '*NONE*'

	ok = 1
	call my_msg('Getting file list, please wait...')
	do while(ok .and. n .lt. max_files)
	   ok = lib$find_file(
	1	file_spec(:k),
	1	file,
	1	context,
	1	,			! default file spec
	1	,			! related file spec, not used
	1	rms_status,		! additional rms status return
	1	flags)			! 1=use search list logical name

	   if (ok) then
	      if (index(file,'.DIR;') .eq. 0) then
	         if (index(file,'.DAT;') .ne. 0 .or.
	1	     index(file,'.WND;') .ne. 0 .or.
	1            index(file,'.MCR;') .ne. 0) then
	            n = n + 1
	            s(2).idz = s(2).idz + 1
	            call process_file_name(file)
	            files(n) = file
	         end if
	      end if
	   else if (ok .eq. no_such_file) then
	      call my_err('No files found, please check selection criteria.')
	      files(1) = '*NONE*'
	   else if (ok .eq. file_not_found) then
	      call my_err('No files found in this directory.')
	      files(1) = '*NONE*'
	   else if (ok .ne. rms_no_more_files) then
	      write(msg,11,iostat=ios) n+1, ok, rms_status
	      call my_err(msg)
	   end if
	end do
	if ((n .ge. max_files) .and. ok) then
	   call my_err('Additional files are not listed.')
	else if (ok .eq. rms_no_more_files) then
	   call my_msg(' ')
	end if

	! see if search list has introduced a sort problem
	i = 1
	do while (i .lt. n)
	   j = i + 1
	   if (files(i) .gt. files(j)) then
	      call ch_heapsort(files, n, file)
	      i = n + 1
	   end if
	   i = i + 1
	end do

	! reverse the order of the files
	j = n
	do i=1,n/2
	   file = files(i)
	   files(i) = files(j)
	   files(j) = file
	   j = j - 1
	end do

	return
 11	format('Error getting file #',i3,', ok=',z8.8,', rms=',z8.8)
	end

	options/extend_source
!------------------------------------------------------------------------------
! Converts a standard .WND file name specification into something more
! pleasant for the display.  Expands Julian day-of-year into 3-character
! month memonic.  Optionally obtains and formats the file size.
!------------------------------------------------------------------------------
	integer*4	function	process_file_name(file)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	character*(*)	file
	integer*4	i,j,k,n
	integer*4	k2len
	integer*4	doy, year, month, day
	integer*4	ios, ios1, ios2
	character*3	month_list(0:13) /'_?_', 'JAN', 'FEB', 'MAR', 'APR',
	1		'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV',
	1		'DEC', '<?>' /
	integer*4	index_last		! a function
	record /file_display_line/ fdl

	process_file_name = 1

!	call lib$trim_filespec(file,files(n),72,j)
!	i = index(file,']')
	i = index_last(file,']')
	if (i.eq.0) i = index(file,':')
	i = i + 1
	j = k2len(file)
	fdl.s = ' '
	fdl.file_name	= file(i:j)
	fdl.file_name(27:27) = ' '
	fdl.space3	= ' '

	if (fdl.file_name(:5) .eq. 'WIND_') then
	   read(fdl.file_name(06:09),'(i)',iostat=ios1) year
	   read(fdl.file_name(11:13),'(i)',iostat=ios2) doy 
	   if (ios1 .eq. 0 .and. ios2 .eq. 0) then
	      call julian_to_mmdd(doy,year,month,day)
	      write(fdl.day_of_month,'(i2.2)',iostat=ios) day
	      fdl.month = month_list(month)
	   else
	      fdl.month = '?'
	      fdl.day_of_month = ' '
	   end if
	end if

	file = fdl.s
	process_file_name = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w2_add_file_info(dir,file)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	include		'c_string_def.for'
	character*(*)	dir
	character*(*)	file
	integer*4	i,j,k,n
	integer*4	ios
	integer*4	k2len
	record /c_string_256/ c256
	integer*4	file_info		! c routine
	integer*4	create_date(2)
	integer*4	protect
	integer*4	group_uic
	integer*4	member_uic
	integer*4	size
	integer*4	ok
	character*4	prot_str /'RWED'/
	record /file_display_line/ fdl

	w2_add_file_info = 1

	fdl.s = ' '
	fdl.space3	= ' '
	fdl.file_name	= file
	k = k2len(dir)
	c256.c = dir(1:k)//file
!	call my_msg(c256.c(1:70))
	n = k2len(c256.c) + 1
	c256.b(n) = 0
	create_date(1) = 0
	create_date(2) = 0
	protect = 0
	group_uic = 0
	member_uic = 0
	ok = file_info(c256.b, create_date, protect,
	1	group_uic, member_uic, size)
!	uic(1) = member_uic
!	uic(2) = group_uic
!	ok = sys$idtoasc(uic,m,fdl.owner,,,)
	write(fdl.block_size,'(i7)',iostat=ios) size
	write(fdl.owner,'(''['',o3.3,'','',o3.3,'']'')',iostat=ios)
	1	group_uic, member_uic
	fdl.protection = ' '
	i = 1
	do j = 0,12,4
	   n = 1
	   do k=j,j+3
	      if (.not. btest(protect,k)) then
	         fdl.protection(i:i) = prot_str(n:n)
	         i = i + 1
	      end if
	      n = n + 1
	   end do
	   fdl.protection(i:i) = ','
	   i = i + 1
	end do
	i = i - 1
	fdl.protection(i:i) = ' '

	file = fdl.s
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Entry my_dir_data is called by the scroll-window manager to place the si-th
! completely formatted line item in global buffer s(2).str.
! Entry get_my_dir_list is called to initialize the dir list with all the
! directory names from the target directory.
!------------------------------------------------------------------------------
	integer*4	function	my_dir_list(si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	bc=g1on//vc//g1off
	integer*4	si
	integer*4	ok
	integer*4	i,j,k,m,n,o
	integer*4	k2len
	integer*4	lib$find_file
	integer*4	lib$find_file_end
	integer*4	context
	integer*4	rms_status
	integer*4	flags
	integer*4	get_my_dir_list		! an entry point
	integer*4	reset_dir_list		! an entry point
	character*256	prev_dir
	integer*4	rms_no_more_files /'182ca'x/
	integer*4	no_such_file /'0910'x/
	integer*4	rms_file_not_found /'18292'x/
	integer*4	ios
	character*256	file_spec
	character*256	file
	character*80	msg
	integer*4	first_time /1/
	integer*4	index_last		! a function
	integer*4	first_whitespace	! a function
	character*80	blank /' '/
	integer*4	w_nrt_is_available	! a function

	my_dir_list = 0
	have_shown_dir_list_once = 1

	if (si .lt. 0) then		! bold current line, ++scrobject
	   si = - si
	   write(s(1).str,1) 
	1	bc,bright,
	1	si,dirs(si),
	1	primary,bc
	else if (si .eq. s(1).idx) then	! current line, show reverse
	   write(s(1).str,1) 
	1	bc,reverse,
	1	si,dirs(si),
	1	primary,bc
	else if (si .ge. 1 .and. si .le. s(1).idz) then	! not current, normal
	   write(s(1).str,1) 
	1	bc,primary,
	1	si,dirs(si),
	1	primary,bc
	else if (si .gt. s(1).idz) then		! blank line
	   write(s(1).str,2) 
	1	bc,primary,
	1	blank,
	1	primary,bc
	end if
	s(1).l_str = 7 + 6 + 70 + 7

	my_dir_list = 1
	return
 1	format(a3,a4,' ',i3,'. ',a70,    a4,a3)
 2	format(a3,a4,            a76,    a4,a3)
! 2	format(a3,a4,            a01,75x,a4,a3)

	!----------------------------------------------------------------------
	entry	reset_dir_list()
	reset_dir_list = 1
	have_shown_dir_list_once = 0
	first_time = 1
	prev_dir = ' '
	do i=1,s(1).idz
	   dirs(i) = ' '
	end do
	s(1).idz = 0
	s(1).xr  = 0
	return

	!----------------------------------------------------------------------
	entry	get_my_dir_list()
	get_my_dir_list = 1
!	if (first_time) then
!	   do i=1,len(blank)
!	      blank(i:i) = ' '
!	   end do
!	end if
	i = max(2,first_whitespace(current)) - 1
	if ((prev_dir(:i) .eq. current(:i)) .and. (.not. first_time)) return
	if (.not. have_shown_dir_list_once) then
	   s(i_sfile).idw = 1
	   s(i_sfile).idx = 1
	   call get_my_file_list()
	   call scr_mgr_show_one_scroll(s(2))
	end if
	first_time = 0
	flags = 2!'fe'x
	if (context .ne. 0) then
	   ok = lib$find_file_end(context)
	end if
	context = 0

	k = k2len(current)
	if (k.lt.1) then
	   current = default_dir
	   k = sizeof(default_dir)
	end if
	prev_dir = current
	file_spec = current(:k)//'*.dir'
	k = max(1,k2len(file_spec))
	n = 0
	s(1).idz = 0
	s(1).xr  = 0

	ok = 1
	dirs(1) = '[]'		! current directory from directory field
!	i = index(file_spec,']')
	i = index_last(file_spec,']')
	j = index(file_spec,'.')
	m = index(file_spec,'...')
	o = index(file_spec,'*')
	if (i.gt.j .and. i.gt.0 .and. m.eq.0 .and. (o.eq.0 .or. o.gt.i)) then
	   dirs(2) = '[-]'	! parent directory
	   n = 2
	else if (dir_search_depth .gt. 0) then
	   dirs(2) = '[-]'	! parent directory
	   n = 2
	else
	   if (1 .eq. w_nrt_is_available()) then
              dirs(2) = nrt_id_label
	      n = 2
	   else
	      n = 1
	   end if
	end if
	s(1).idz = n
	call my_msg('Retrieving directory list, please wait...')
	do while(ok .and. (n .lt. max_dirs))
	   ok = lib$find_file(
	1	file_spec(:k),
	1	file,
	1	context,
	1	,			! default file spec
	1	,			! related file spec, not used
	1	rms_status,		! additional rms status return
	1	flags)			! 1=use search list logical name

	   if (ok) then
	      n = n + 1
	      s(1).idz = s(1).idz + 1
	      dirs(n) = file
	   else if (ok .eq. no_such_file) then
	      n = n + 1
	      call my_err('No Directories found, please check selection criteria.')
	      dirs(n) = '*NONE*'
	   else if (ok .eq. rms_no_more_files) then
	   else if (ok .eq. rms_file_not_found) then
	   else
	      write(msg,11,iostat=ios) n+1, ok, rms_status
	      call my_err(msg)
	   end if
	end do

	if (dir_search_depth .eq. 0) then
	   i = 1	! first entry is already listed in files
	   do while (n .lt. max_dirs .and. i .lt. n_search_list_entries)
	      n = n + 1
	      i = i + 1
	      s(1).idz = s(1).idz + 1
	      k = max(1, first_whitespace(search_list(i)))
	      search_list(i)(k:) = ' '
	      dirs(n) = search_list(i)
	   end do
	end if

	if ((n .ge. max_dirs) .and. ok) then
	   call my_err('Additional directories are not listed.')
	else if (ok .eq. rms_no_more_files) then
	   call my_msg(' ')
	else if (ok .eq. rms_file_not_found) then
	   call my_msg(' ')
	end if

	return
 11	format('Error getting directory #',i3,', ok=',z8.8,', rms=',z8.8)
	end

	options/extend_source
!------------------------------------------------------------------------------
! Sets the user's "current directory" for examining WIND/WAVES data files.
! Does NOT change the user's default directory in the "SET DEFAULT" DCL sense.
!------------------------------------------------------------------------------
	integer*4	function	set_current_dir()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	i,j
	character*64	new
	integer*4	show_file_display	! an entry point
	integer*4	index_last		! a function

	set_current_dir = 0

	if (dirs(s(1).idx) .eq. '[]') then
	   return
	else if (dirs(s(1).idx) .eq. '*NONE*') then
	   return
	else if (dirs(s(1).idx) .eq. nrt_id_label) then
	   result_file = 'nrt'
	   set_current_dir = ok_and_exit_now
	   return
	else if (dirs(s(1).idx) .eq. '[-]') then
!	   i = index(current,']')
	   i = index_last(current,']')
	   j = index(current,'.')
	   if (i .eq. 0 .or. j .eq. 0 .or. j.gt.i) then
	      ! top of the directory heirichy, present root level
	      ! dir's and search list
	      current = search_list(1)
	   else if (dir_search_depth .eq. 1) then
	      current = search_list(1)
	   else
	      new = current(:i)
	      j = i-1
	      do while(new(i:i) .ne. '.')
	         new(i:i) = ' '
	         i = i - 1
	      end do
	      new(i:i) = ']'
	      current = new
!	      dir_search_depth = dir_search_depth - 1
	   end if
	   dir_search_depth = dir_search_depth - 1
	else
	   new = dirs(s(1).idx)
	   j = index(new,'.DIR;')
	   if (j .ne. 0) then		! regular something.dir file spec
	      i = index_last(new,']')
	      new(i:i) = '.'
	      new(j:) = ']'
	   else
	      ! do nothing, we have a [something.else] type path spec
	   end if
	   current = new
	   dir_search_depth = dir_search_depth + 1
	end if

	call get_my_dir_list()
	s(1).idw = 1
	s(1).idx = 1
	call scr_mgr_show_one_scroll(s(1))

	!----------------------------------------------------------------------
	entry	show_file_display()

	call get_my_file_list()
	call scr_mgr_show_one_scroll(s(2))
	call update_top_status_line()

	set_current_dir = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key R,r routine to select the REALTIME stream.
! Triggers the screen manager to return control to parent module.
!------------------------------------------------------------------------------
	integer*4	function	my_select_realtime()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'

	my_select_realtime = 0
	result_file = 'REALTIME'

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key N,n routine to select the Near RealTime (NRT) stream.
! Triggers the screen manager to return control to parent module.
!------------------------------------------------------------------------------
	integer*4	function	my_select_near_realtime()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	w_nrt_is_available		! a function

	if (1 .eq. w_nrt_is_available()) then
	   my_select_near_realtime = 0
	   call my_msg('Selected CDHF Near RealTime client stream.')
	   result_file = 'nrt'
	else
	   my_select_near_realtime = 1
	   call my_msg('CDHF Near RealTime client stream not available.')
	end if

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key L,l routine to select the last file used stream.
! Triggers the screen manager to return control to parent module.
!------------------------------------------------------------------------------
	integer*4	function	my_select_lastfile()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'

	my_select_lastfile = 0
	result_file = last_file_used

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key K,k routine to display the file name selected last program run.
!------------------------------------------------------------------------------
	integer*4	function	display_last_file_used()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	k
	integer*4	k2len

	display_last_file_used = 1
	if (last_file_used(1:1) .gt. ' ') then
	   k = k2len(last_file_used)
	   call my_msg('Last file was: '//last_file_used(:k))
	else
	   call my_msg('There is no previously used .WND file for this process.')
	end if

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The "secret" routine developed for Claude Perche during the instrument
! refurbishment and integration at Minnesota in NOV-DEC 1993.
! Invoked by a "hot key", this routine displays a message previously
! buffered via a call to the entry point wind_tm_opt_fss_title.
!------------------------------------------------------------------------------
	integer*4	function	display_user_message()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	k
	integer*4	k2len
	integer*4	wind_tm_opt_fss_title		! an entry point
	character*60	user_message
	character*(*)	user_arg_message

	display_user_message = 1
	k = k2len(user_message)
	if (k.gt.0) then
	   call my_msg('>> '//user_message(:k))
	end if

	return

	!----------------------------------------------------------------------
	entry	wind_tm_opt_fss_title(user_arg_message)
	user_message = user_arg_message
	wind_tm_opt_fss_title = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This is the action routine called when the user presses S or s on the
! keyboard to toggle display of file sizes.
!------------------------------------------------------------------------------
	integer*4	function	my_size_toggle(xi)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	integer*4	switch_back
	integer*4	i

	my_size_toggle = 1
!	show_file_size = .not. show_file_size
!	if (show_file_size) newly_including_sizes = 1

!	if (xi .ne. i_sfile) then
!	   switch_back = xi
!	   xi = i_sfile
!	   if (.not. have_shown_file_list_once) s(i_sfile).idx = 1
!	   call get_my_file_list()
!	   call scr_mgr_show_one_scroll(s(2))
!	   xi = switch_back
!	else
!	   if (.not. have_shown_file_list_once) s(i_sfile).idx = 1
!	   call get_my_file_list()
!	   call scr_mgr_show_one_scroll(s(2))
!	   switch_back = 0
!	end if

	i = s(i_sfile).idx
	call w2_add_file_info(current, files(i))

!	if (switch_back .ne. 0) xi = switch_back
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This is the action routine called when the user presses P or p on the
! keyboard to request a prompt for a "manual" file name input.
!------------------------------------------------------------------------------
	integer*4	function	my_select_by_prompt()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ios
	integer*4	k2len
	character*3	err
	integer*4	i,o
	parameter	prompt=csi//'24;1f'//erase_line//bright//
	1		'Enter file spec: '//primary

	my_select_by_prompt = 0
	call putstr(prompt)
	read(5,'(q,a)',iostat=ios) i,result_file
	if (ios .ne. 0) then
	   write(err,'(i3.3)',iostat=o) ios
	   call my_refresh()
	   call my_err('Error entering file name, iostat='//err)
	   my_select_by_prompt = 1
	else if (k2len(result_file) .lt. 1) then
	   call my_msg('No file entered.')
	   my_select_by_prompt = 1
	end if

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Calls set_current_dir to move the current file search/display directory
! up one level in the heirarchy.
!------------------------------------------------------------------------------
	integer*4	function	my_up_one_dir_lev_routine()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'

	my_up_one_dir_lev_routine = 0

	if (dirs(1) .eq. '[-]') then
	   s(i_sdir).idx = 1
	   call set_current_dir()
	else if (dirs(2) .eq. '[-]') then
	   s(i_sdir).idx = 2
	   call set_current_dir()
	else
	   call my_err('Parent directory request invalid here.')
	end if	

	my_up_one_dir_lev_routine = 1
	return
	end

!------------------------------------------------------------------------------
! This routine is called after the user exits the directory list scrolling
! box (or window--terminology!).  The action taken here depends on the
! keystroke used to exit the scroll region.
!------------------------------------------------------------------------------
	integer*4	function	my_dir_list_after(x) !,si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	record		/tv_scroll/ x
!	integer*4	si
	character*16	keyname
	integer*4	ok
	integer*4	set_current_dir

	my_dir_list_after = 0
	keyname = x.retkey

	if ( (keyname .eq. 'DO') .or.
	1    (keyname .eq. 'FIND') .or.
	1    (keyname .eq. 'SELECT') .or.
	1    (keyname .eq. 'CNTL/M') .or.
	1    (keyname .eq. 'PF4')
	1  ) then
	   ok = set_current_dir()
	   if (ok .eq. ok_and_exit_now) then
	      x.retkey = 'CNTL/Z'
	   else
	      x.retkey = 'CNTL/I'
	   end if
	else
	   my_dir_list_after = 1
	end if

	my_dir_list_after = 1
	return
	end

!------------------------------------------------------------------------------
! This routine is called after the user exits the file list scrolling
! box (or window--terminology!).  The action taken here depends on the
! keystroke used to exit the scroll region.
!------------------------------------------------------------------------------
	integer*4	function	my_file_list_after(x) !,si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	record		/tv_scroll/ x
!	integer*4	si
	integer*4	i,j,k
	integer*4	k2len			! a function
	character*16	keyname
	integer*4	index_last		! a function

	my_file_list_after = 0
	keyname = x.retkey

	if ( (keyname .eq. 'DO') .or.
	1    (keyname .eq. 'FIND') .or.
	1    (keyname .eq. 'SELECT') .or.
	1    (keyname .eq. 'CNTL/M') .or.
	1    (keyname .eq. 'PF4')
	1) then
	   i = k2len(current)
	   if (i.lt.1) goto 10
	   if (s(2).idx .lt. 1) goto 20
	   j = index_last(files(s(2).idx),']') + 1
!	   j = index(files(s(2).idx),']') + 1
!	   k = k2len(files(s(2).idx))
	   k = index(files(s(2).idx), ' ')
	   if (j .gt. k) then
	      ! uic is displayed
	      j = 1
	   end if
	   result_file = current(:i)//files(s(2).idx)(j:k)
!	   f(i_lf).str = current(:i)//files(s(2).idx)(j:k)
!	   call scr_mgr_show_one_get(f(i_lf))
	   x.retkey = 'CNTL/Z'
	   my_file_list_after = 1
	else
	   my_file_list_after = 1
	end if

	return
 10	continue
	call my_err('No current directory is specified.')
	return
 20	continue
	call my_err('Zero length directory specified.')
	return
	end

!------------------------------------------------------------------------------
! This routine is called with the scroll_region manager receives a keystroke
! that it does not interpret (most anything other than PREV, NEXT, and the
! arrow keys).
!
! returns: 0 to exit scroll routine
!          1 to continue scroll routine
! Note that keyname argument is passed on to the manage_screen routine
! for additional processing.
!------------------------------------------------------------------------------
	integer*4	function	my_scroll_on_key_handler(xi,keyname)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	character*(*)	keyname
	integer*4	i

	my_scroll_on_key_handler = 0
	i = xi ! dummy statement

	if (keyname .eq. 'CNTL/I') then
	else if (keyname .eq. 'CNTL/M') then
	else if (keyname .eq. 'CNTL/W') then
	else if (keyname .eq. 'CNTL/Z') then
	else if (keyname .eq. 'CNTL/B') then
	else if (keyname .eq. 'DO') then
	else if (keyname .eq. 'FIND') then
	else if (keyname .eq. 'SELECT') then
	else if (keyname .eq. 'PF4') then
	else if (keyname .eq. 'HELP') then
	else if (keyname .eq. '?') then
	else if (keyname .eq. 'H') then
	else if (keyname .eq. 'h') then
	else if (keyname .eq. 'U') then
	else if (keyname .eq. 'u') then
	else if (keyname .eq. 'R') then
	else if (keyname .eq. 'r') then
	else if (keyname .eq. 'L') then
	else if (keyname .eq. 'l') then
	else if (keyname .eq. 'P') then
	else if (keyname .eq. 'p') then
	else if (keyname .eq. 'I') then
	else if (keyname .eq. 'i') then
	else if (keyname .eq. 'K') then
	else if (keyname .eq. 'k') then
	else if (keyname .eq. 'M') then
	else if (keyname .eq. 'm') then
	else if (keyname .eq. 'V') then
	else if (keyname .eq. 'v') then
	else if (keyname .eq. 'N') then
	else if (keyname .eq. 'n') then
	else
	   call my_msg('Key (scroll)='//keyname)
	   my_scroll_on_key_handler = 1
	end if

	return
	end
