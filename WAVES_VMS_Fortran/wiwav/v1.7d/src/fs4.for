! fs4.for - wind/waves file chooser #4.  This chooser is closely coupled with
! chooser #2 because of shared routines, commons, and terminal screen
! interface.

!------------------------------------------------------------------------------
! Entry w_c4_file_data is called by the scroll-window manager to place the si-th
! completely formatted line item in global buffer s(2).str.
! Entry get_file_list is called to initialize the file list with all the
! file names from the target directory.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_file_data(si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	bc=g1on//vc//g1off
	integer*4	si
	integer*4	ok
	integer*4	i,j,k,n
	integer*4	k2len
	integer*4	flags
	integer*4	w_c4_get_file_list		! an entry point
	integer*4	w_c4_reset_file_list		! an entry point
	integer*4	ios
	character*80	msg
	integer*4	first_time /1/
	integer*4	w_c4_build_file_sublist		! a function

	w_c4_file_data = 0
	have_shown_file_list_once = 1
!	call lib$wait(0.15)

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
	else
	   write(s(2).str,2) 
	1	bc,primary,
	1	'?',
	1	primary,bc
	end if
!	s(2).l_str = 7 + 6 + 72 + 7 
	s(2).l_str = 7 + 6 + 70 + 7 

	w_c4_file_data = 1
	return
 1	format(a3,a4,' ',i3,'. ',a70,    a4,a3)
 2	format(a3,a4,            a01,75x,a4,a3)


	!----------------------------------------------------------------------
	entry	w_c4_reset_file_list()
	w_c4_reset_file_list = 1
	have_shown_file_list_once = 0
	first_time = 1
	do i=1,s(2).idz
	   files(i) = ' '
	end do
	s(2).idz = 0
	s(2).idx = -1
	s(2).xr  = 0
	return

	!----------------------------------------------------------------------
	entry	w_c4_get_file_list()
	w_c4_get_file_list = 1
!	if (
!	1   (.not. first_time) .and. (.not. newly_including_sizes) ) return
!	newly_including_sizes = 0
!	first_time = 0
!	s(2).xr  = 0
	files(1) = '*NONE*'
	ok = w_c4_build_file_sublist()

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Entry w_c4_dir_data is called by the scroll-window manager to place the si-th
! completely formatted line item in global buffer s(2).str.
! Entry w_c4_get_dir_list is called to initialize the dir list with all the
! "virtual" directory names from the target directory.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_dir_list(si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	bc=g1on//vc//g1off
	integer*4	si
	integer*4	ok
	integer*4	i,j,k,m,n,o
	integer*4	k2len
	integer*4	w_c4_get_dir_list		! an entry point
	integer*4	w_c4_reset_dir_list		! an entry point
	integer*4	ios
	character*80	msg
	integer*4	first_time /1/
	integer*4	index_last		! a function
	integer*4	first_whitespace	! a function
	character*80	blank /' '/
	integer*4	yr,mn
	character*3	month_list(0:13) /'_?_', 'JAN', 'FEB', 'MAR', 'APR',
	1		'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV',
	1		'DEC', '<?>' /

	w_c4_dir_list = 0

!	call lib$wait(0.25)

	if (si .lt. 0) then
	   ! bold current line (display may not be current)
	   si = - si
	   write(s(1).str, 1) bc, bright, si, dirs(si), primary, bc
	   !
	else if (si .eq. s(1).idx) then
	   ! current line, show reverse (display is current)
	   write(s(1).str, 1) bc, reverse, si, dirs(si), primary, bc
	   !
	else if (si .ge. 1 .and. si .le. s(1).idz) then
	   ! not current, normal
	   write(s(1).str, 1) bc, primary, si, dirs(si), primary, bc
	   !
	else if (si .gt. s(1).idz) then
	   ! blank line
	   write(s(1).str, 2) bc, primary, blank, primary, bc
	   !
	else
	   call w_c4_err('index for directory list out of range')
	   s(1).str = 'jon '
	end if
	s(1).l_str = 7 + 6 + 70 + 7

	w_c4_dir_list = 1
	return
 1	format(a3,a4,' ',i3,'. ',a70,    a4,a3)
 2	format(a3,a4,            a76,    a4,a3)
! 2	format(a3,a4,            a01,75x,a4,a3)

	!----------------------------------------------------------------------
	entry	w_c4_reset_dir_list()
	! only called once on entry to w_fs4
	w_c4_reset_dir_list = 1
!	have_shown_dir_list_once = 0
!	first_time = 1
	do i=1,s(1).idz
	   dirs(i) = ' '
	end do
!	s(1).xr  = 0
!	s(1).idx = 0
!	s(1).idy = 0
!	s(1).idw = 0
!	s(1).idz = 0
	return

	!----------------------------------------------------------------------
	entry	w_c4_get_dir_list()

	w_c4_get_dir_list = 1

	if (first_time .eq. 1) then
	   call w_c4_init_file_list()	! this should be the only call
	   first_time = 0
	end if

	if (vfl.i .eq. 0) then
	   ! build a list of availble years
	   n = 0
	   do i=1,max_years_of_mission
	      if (vfl.vy(i).year(1:1) .gt. ' ') then
	         n = n + 1
	         dirs(n) = vfl.vy(i).year
	      end if
	   end do
	   s(1).xr  = 0 ! an absolute terminal screen row number
!	   s(1).idx = 1
!	   s(1).idw = 1
	   s(1).idz = n
	else if (vfl.vy( vfl.i ).i .eq. 0) then
	   ! build a list of availble months
	   n = 1
	   dirs(1) = '[-]'
	   yr = vfl.i
	   do i=1,12
	      if (vfl.vy(yr).vm(i).n .gt. 0) then
	         n = n + 1
	         dirs(n) = month_list(i)
	      end if
	   end do
	   s(1).xr  = 0 ! an absolute terminal screen row number
!	   s(1).idw = 1 ! data index of first line in scroll region
!	   s(1).idx = 1 ! data index of current line item
!	   s(1).idy = 6 ! data index of last line in scroll region
	   s(1).idz = n ! data index of last available line item
	else
	   ! do nothing to dir list, current list is ok, 
	   ! we are reentering dir scroll region, blank file scroll region
!	   call w_c4_reset_file_list()
!	   call scr_mgr_show_one_scroll(s(2))
	end if

!	if (.not. have_shown_dir_list_once) then
!	   s(i_sfile).idw = 1
!	   s(i_sfile).idx = 1
!	   call w_c4_get_file_list()
!	   call scr_mgr_show_one_scroll(s(2))
!	end if

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Sets the user's "current directory" for examining WIND/WAVES data files.
! Does NOT change the user's default directory in the "SET DEFAULT" DCL sense.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_set_cur_dir()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	i,j
	character*64	new
	integer*4	w_c4_show_file_display	! an entry point
	integer*4	index_last		! a function
	logical*4	reshow
	integer*4	yr,mn
	character*3	month_list(0:13) /'_?_', 'JAN', 'FEB', 'MAR', 'APR',
	1		'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV',
	1		'DEC', '<?>' /
	character	c4*4, c3*3, c2*2

	w_c4_set_cur_dir = 0

	if (vfl.i .eq. 0) then
	   ! years are displayed and user has picked a year
	   c4 = dirs(s(1).idx)
	   call w_c4_msg('selected year '//c4)
	   ! match the year
	   i = 1
	   do while(i .le. max_years_of_mission
	1           .and.
	1           vfl.vy(i).year .ne. c4)
	      i = i + 1
	   end do
	   if (i.gt.max_years_of_mission) then
	      call w_c4_err('cannot set current vdir, sit.1')
	   end if
	   vfl.i = i
	   vfl.vy(i).i = 0
 	   call w_c4_get_dir_list()
!!	   s(1).idw = 1
!!	   s(1).idx = 1
 	   call scr_mgr_show_one_scroll(s(1))
	   call w_c4_update_top_status_line()
	   w_c4_set_cur_dir = 0
	   return
	else if (dirs(s(1).idx)(1:1) .eq. '[') then
	   ! months were displayed, move back to years
	   i = vfl.i
	   if (i .gt. 0) vfl.vy(i).i = 0
	   vfl.i = 0
 	   call w_c4_get_dir_list()
!!	   s(1).idw = 1
!!	   s(1).idx = 1
 	   call scr_mgr_show_one_scroll(s(1))
	   call w_c4_update_top_status_line()
	   w_c4_set_cur_dir = 0
	   return
	else
	   ! years and months displayed, a month was picked
	   yr = vfl.i
	   j = 1
	   c3 = dirs(s(1).idx)
	   call w_c4_msg('selected month '//c3)
	   do while(c3 .ne. month_list(j) .and. j .le. 12)
	      j = j + 1
	   end do
	   if (j .gt. 13) then
	      call w_c4_err('cannot set current vdir, sit.2')
	      return
	   end if
	   vfl.vy(yr).i = j
	   call w_c4_update_top_status_line()
	   call w_c4_get_file_list()
	   call scr_mgr_show_one_scroll(s(2))
	end if

	w_c4_set_cur_dir = 1
	return
	end

!------------------------------------------------------------------------------
! This routine is called after the user exits the directory list scrolling
! box (or window--terminology!).  The action taken here depends on the
! keystroke used to exit the scroll region.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_dir_list_after(x) !,si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	record		/tv_scroll/ x
!	integer*4	si
	character*16	keyname
	integer*4	ok
	integer*4	w_c4_set_cur_dir

	w_c4_dir_list_after = 0
	keyname = x.retkey

!	if (keyname .eq. 'CNTL/Z') then
!	   ok = 1
!	   result_file = ' '
!	else if (keyname .eq. 'CNTL/W') then
!	   ok = 1
!	   result_file = ' '
	if ( (keyname .eq. 'DO') .or.
	1    (keyname .eq. 'FIND') .or.
	1    (keyname .eq. 'SELECT') .or.
	1    (keyname .eq. 'CNTL/M') .or.
	1    (keyname .eq. 'PF4')
	1  ) then
	   ok = w_c4_set_cur_dir()
	   if (ok .eq. 1) x.retkey = 'CNTL/I'
	else
	   ok = 1
!	   if (vfl.i 
	end if

	w_c4_dir_list_after = ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Calls w_c4_set_cur_dir to move the current file search/display directory
! up one level in the heirarchy.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_up_one_dir_lev_routine()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'

	w_c4_up_one_dir_lev_routine = 0

	call w_c4_err('"Hot Key" for up one directory not implemented')

	w_c4_up_one_dir_lev_routine = 1
	return
	end

!------------------------------------------------------------------------------
! This routine is called after the user exits the file list scrolling
! box (or window--terminology!).  The action taken here depends on the
! keystroke used to exit the scroll region.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_file_list_after(x) !,si)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	record		/tv_scroll/ x
!	integer*4	si
	integer*4	i,j,k,o
	integer*4	k2len			! a function
	character*16	keyname
	integer*4	index_last		! a function
	integer*4	ch_number_of_month
	character	c2*2

	w_c4_file_list_after = 0
	keyname = x.retkey

	if (keyname .eq. 'CNTL/Z') then
	   w_c4_file_list_after = 1
	   result_file = ' '
	else if ( (keyname .eq. 'DO') .or.
	1    (keyname .eq. 'FIND') .or.
	1    (keyname .eq. 'SELECT') .or.
	1    (keyname .eq. 'CNTL/M') .or.
	1    (keyname .eq. 'PF4')
	1) then
	   read(files(s(2).idx)(1:2),'(i2.2)',iostat=o) j
	   call ch_number_of_month(c2,files(s(2).idx)(4:6))
	   result_file = 'wind_data:wi_lz_wav_'
	1	//vfl.vy(vfl.i).year
	1	//c2
	1	//files(s(2).idx)(1:2)
	1	//'_v'
	1	//vfl.vy(vfl.i).vm( vfl.vy(vfl.i).i ).dayver(j)
	1	//'.dat'
	   x.retkey = 'CNTL/Z'
	   w_c4_file_list_after = 1
	else
	   call w_c4_reset_file_list()
	   call scr_mgr_show_one_scroll(s(2))
	   w_c4_file_list_after = 1
	end if

	return
 10	continue
	call w_c4_err('No current directory is specified.')
	return
 20	continue
	call w_c4_err('Zero length directory specified.')
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_c4_build_file_sublist()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ok
	integer*4	i,j,k,n,o,p
	character	c4*4, c3*3, c2*2
	integer*4	i_mon, i_dom
	character*3	month_list(0:13) /'_?_', 'JAN', 'FEB', 'MAR', 'APR',
	1		'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV',
	1		'DEC', '<?>' /
	integer*4	iday_of_year
	integer*4	year

	w_c4_build_file_sublist = 0

	j = vfl.i
	k = vfl.vy(j).i
	n = 0
	read(vfl.vy(j).year,'(i4)',iostat=o) year
	do i=1,32
	   if (vfl.vy(j).vm(k).dayver(i) .gt. '  ') then
	      n = n + 1
	      p = iday_of_year(year,k,i)
	      write(c3,'(i3.3)',iostat=o) p ! julian day
	      write(c2,'(i2.2)',iostat=o) i
	      files(n) = c2
	1	//'-'//month_list(k)
	1	//'-'//vfl.vy(j).year
	1	//'   ['//c3//']'
	1	//'   v'//vfl.vy(j).vm(k).dayver(i)
	   end if
	end do
	s(2).idx = 1
	s(2).idw = 1
	s(2).idz = n
	write(c4,'(i4)',iostat=o) n
	call w_c4_msg(c4//' days found for month '//month_list(k))

	w_c4_build_file_sublist = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_c4_reset_vfl(x)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	x
	integer*4	i,j,k

	w_c4_reset_vfl = 1
	i = vfl.i
	if (i .ne. 0) then
	   j = vfl.vy(i).i
	   if (j .ne. 0) then
	      vfl.vy(i).vm(j).i = 0
	   end if
	   vfl.vy(i).i = 0
	end if
	vfl.i = 0
	!
	if (x .eq. 0) return
	!
	do i=1,max_years_of_mission
	   vfl.vy(i).i = 0
	   vfl.vy(i).year = '    '
	   do j=1,12
	      vfl.vy(i).vm(j).i = 0
	      do k=1,32
	         vfl.vy(i).vm(j).dayver(k) = '  '
	      end do
	   end do
	end do

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_c4_init_file_list()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ok
	integer*4	i,j,k,n,m,o,x
	integer*4	k2len
	integer*4	lib$find_file
	integer*4	lib$find_file_end
	integer*4	context /0/
	integer*4	rms_status
	integer*4	flags
	integer*4	w_c4_get_file_list		! an entry point
	character*256	prev_dir
	integer*4	rms_no_more_files /'182ca'x/
	integer*4	no_such_file /'0910'x/
	integer*4	file_not_found /'18292'x/
	integer*4	ios
	character*256	file_spec
	character*256	file
	character*80	msg
	integer*4	first_time /1/
	character*1	thing(8) /'|', '/', '-', '\', '|', '/', '-', '\'/
	integer*4	i_thing /0/
	integer*4	local_max_files
	character	c4*4, c2*2
	integer*4	i_mon, i_dom

	w_c4_init_file_list = 0

	call w_c4_msg('Getting file list, please wait...')
	! assume all files are in a single directory
	file_spec = 'wind_data:wi_lz_wav_*v%%.dat'
	k = k2len(file_spec)

	call w_c4_reset_vfl(1)

	local_max_files = max_years_of_mission * 365
	flags = 0 ! had 2 ! had 'fe'x
	if (context .ne. 0) ok = lib$find_file_end(context)
	context = 0
	n = 0
	ok = 1
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
!	      if (n .gt. 100) then
	         if (mod(n,30) .eq. 0) then
	            i_thing = i_thing + 1
	            if (i_thing .gt. 8) i_thing = 1
	            call w_c4_msg(thing(i_thing))
	         end if
!	      end if
	      i = index(file,'WI_LZ_WAV_')
	      if (i .lt. 1) goto 20
	      i = i + 10
	      c4 = file(i:i+3)
	      c2 = file(i+10:i+11)
!	call lib$wait(0.2)
	      read(file(i+4:i+7),'(i2.2,i2.2)',iostat=o,err=30) i_mon, i_dom
	      if (i_mon .lt. 1 .or. i_mon .gt. 12) goto 32
	      if (i_dom .lt. 1 .or. i_dom .gt. 31) goto 32
	      if (n .eq. 0) then
	         x = 1
	         vfl.vy(x).year = file(i:i+4)
	      end if
	      if (c4 .ne. vfl.vy(x).year) then
	         ! see if year already used (ie, when wind_data is search list)
	         m = 0
	         do j=1,x
	            if (c4 .eq. vfl.vy(j).year) m = j
	         end do
	         if (m .ne. 0) then
	            x = m
	         else
	            ! it is a new year, go on to next slot in buffer
	            x = x + 1
	            if (x .gt. max_years_of_mission) goto 40
	            vfl.vy(x).year = c4
	         end if
	      end if
	      if (vfl.vy(x).vm(i_mon).dayver(i_dom) .le. '  ') then
	         vfl.vy(x).vm(i_mon).n = vfl.vy(x).vm(i_mon).n + 1
	         n = n + 1
	         vfl.vy(x).vm(i_mon).dayver(i_dom) = c2
	      else if (c2 .gt. vfl.vy(x).vm(i_mon).dayver(i_dom)) then
	         vfl.vy(x).vm(i_mon).dayver(i_dom) = c2
	      end if
!	call w_c4_msg('Year '//c4//' ver:'//c2//' mndy: '//file(i+4:i+7))
	   else if (ok .eq. no_such_file) then
	      call w_c4_err('No files found, please check selection criteria.')
	      files(1) = '*NONE*'
	   else if (ok .eq. file_not_found) then
	      call w_c4_err('No files found in this directory.')
	      files(1) = '*NONE*'
	   else if (ok .ne. rms_no_more_files) then
	      write(msg,11,iostat=ios) n+1, ok, rms_status
	      call w_c4_err(msg)
	   end if
	end do
	if (ok .eq. rms_no_more_files) then
	   write(c4,'(i4)',iostat=o) n
	   call w_c4_msg(c4//' files found.')
	end if

	w_c4_init_file_list = 1
	return
 11	format('Error getting file #', i3, ', ok=', z8.8, ', rms=',z8.8)
 20	continue
	call w_c4_err('Unexpected file name: '//file(1:60))
	return
 30	continue
	call w_c4_err('Err getting mon./day: '//file(1:60))
	return
 32	continue
	call w_c4_err('Range err mon./day: '//file(1:60))
	return
 40	continue
	call w_c4_err('Overflow: Additional files are not listed.')
	return
	end

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
	integer*4	function	w_fs4(file)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	character*(*)	file
	integer*4	ok
	integer*4	manage_screen
	integer*4	w_c4_err
	external	w_c4_err

	w_fs4 = 0
	file = ' '
	result_file = ' '

	use_other_chooser = .false.
	call w_c4_reset_file_list()
	call w_c4_reset_dir_list()
	call w_c4_reset_vfl(0)

	call putstr(soft_reset)
	call w_c4_setup_screen_fields()
	call w_c4_build_screen()
	call w_c4_refresh()
	ok = manage_screen(g,on,w_c4_err)
	call w_c4_post_screen()
	if (ok) then
!	   file = file = f(i_lf).str
	   file = result_file
	   w_fs4 = 1
	end if

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Performs cleanup tasks after the user has exited the file selection screen.
! Sets a VMS process logical name to save the name of the last file selected
! for subsequent direct recall via screen "hot keys".
!------------------------------------------------------------------------------
	integer*4	function	w_c4_post_screen()
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

	w_c4_post_screen = 0

	! screen reset code
	call putstr(soft_reset)
	call putstr(csi//'1;99r'//csi//'25;1f')

!	nstr = k2len(result_file)
	nstr = max(1,index(result_file,' '))
	if (result_file .eq. 'REALTIME') then
	   w_c4_post_screen = 1
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

	w_c4_post_screen = 1

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Initializes data structures used by screen manager to control the screen.
! Defines field entry and exit routines and various "hot keys".
!------------------------------------------------------------------------------
	integer*4	function	w_c4_setup_screen_fields()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ok
	integer*4	get_logical_name_str		! a function
!	integer*4	get_search_list			! a function
	integer*4	edit_at
	external	edit_at
	integer*4	scroll_this
	external	scroll_this
	integer*4	w_c4_err
	external	w_c4_err
	integer*4	w_c4_refresh
	external	w_c4_refresh
	integer*4	w_c4_exit
	external	w_c4_exit
	integer*4	w_c4_up
	external	w_c4_up
	integer*4	w_c4_down
	external	w_c4_down
	integer*4	w_c4_next_field
	external	w_c4_next_field
	integer*4	w_c4_prev_field
	external	w_c4_prev_field
	integer*4	w_c4_file_data
	external	w_c4_file_data
	integer*4	w_c4_get_file_list
	external	w_c4_get_file_list
	integer*4	w_c4_dir_list
	external	w_c4_dir_list
	integer*4	w_c4_get_dir_list
	external	w_c4_get_dir_list
	integer*4	w_c4_dir_list_after
	external	w_c4_dir_list_after
	integer*4	w_c4_set_object_pointer
	external	w_c4_set_object_pointer
	integer*4	w_c4_scroll_on_key_handler
	external	w_c4_scroll_on_key_handler
	integer*4	w_c4_file_list_after
	external	w_c4_file_list_after
	integer*4	w_c4_do_routine
	external	w_c4_do_routine
	integer*4	w_c4_help_routine
	external	w_c4_help_routine
	integer*4	w_c4_up_one_dir_lev_routine
	external	w_c4_up_one_dir_lev_routine
	integer*4	w_c4_select_realtime
	external	w_c4_select_realtime
	integer*4	w_c4_select_lastfile
	external	w_c4_select_lastfile
	integer*4	w_c4_select_by_prompt
	external	w_c4_select_by_prompt
	integer*4	w_c4_size_toggle
	external	w_c4_size_toggle
	integer*4	w_c4_display_last_file_used
	external	w_c4_display_last_file_used
	integer*4	use_virtual_dir
	external	use_virtual_dir
	integer*4	i_g, i_f, i_s

	w_c4_setup_screen_fields = 0

	! establish the current directory
!	ok = get_file_dev_logical_name_str(default_dir, current)
	! get search list directories
!	n_search_list_entries = max_search_list_entries
!	ok = get_search_list(default_dir, search_list, n_search_list_entries)
!	if (ok .ne. 1) then
!	   call w_c4_err('Error getting search list: '//default_dir)
!	end if
!	got_search_list = .true.
!	current = search_list(1)
	current = default_dir
	call w_c4_update_top_status_line()
	ok = get_logical_name_str(wind_last_file_used, last_file_used)

	! directory name scroll box
	i_g = 1
	i_s = 1
	g(i_g).type = scroll_this_type
	g(i_g).before = %loc(w_c4_get_dir_list)
	g(i_g).proc = %loc(scroll_this)
	g(i_g).after = %loc(w_c4_dir_list_after)
	g(i_g).obj  = %loc(s(i_s))
	g(i_g).errhdlr = %loc(w_c4_err)
	s(i_s).r1	= 3
	s(i_s).r2	= 8
	s(i_s).idz	= 6
	s(i_s).gtscrldt	= %loc(w_c4_dir_list)
	s(i_s).on_key	= %loc(w_c4_scroll_on_key_handler)
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
	g(i_g).before = %loc(w_c4_get_file_list)
	g(i_g).proc = %loc(scroll_this)
	g(i_g).after = %loc(w_c4_file_list_after)
	g(i_g).obj  = %loc(s(i_s))
	g(i_g).errhdlr = %loc(w_c4_err)
	s(i_s).r1	= 10
	s(i_s).r2	= 21
	s(i_s).idz	= 35
	s(i_s).gtscrldt	= %loc(w_c4_file_data)
	s(i_s).on_key	= %loc(w_c4_scroll_on_key_handler)
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
	on(1).routine = %loc(w_c4_refresh)
	on(2).key = 'CNTL/Z'
	on(2).routine = %loc(w_c4_exit)
	on(3).key = 'UP'
	on(3).routine = %loc(w_c4_up)
	on(4).key = 'DOWN'
	on(4).routine = %loc(w_c4_down)
	on(5).key = 'CNTL/B'
	on(5).routine = %loc(w_c4_prev_field)
	on(6).key = 'CNTL/I'
	on(6).routine = %loc(w_c4_next_field)
	on(7).key = 'CNTL/M'
	on(7).routine = %loc(w_c4_exit)
	on(8).key = 'NEXT'
	on(8).routine = %loc(w_c4_set_object_pointer)
	on(9).key = 'PREV'
	on(9).routine = %loc(w_c4_set_object_pointer)
	on(10).key = 'DO'
	on(10).routine = %loc(w_c4_do_routine)
	on(11).key = 'SELECT'
	on(11).routine = %loc(w_c4_do_routine)
	on(12).key = 'HELP'
	on(12).routine = %loc(w_c4_help_routine)
	on(13).key = 'H'
	on(13).routine = %loc(w_c4_help_routine)
	on(14).key = 'h'
	on(14).routine = %loc(w_c4_help_routine)
	on(15).key = '?'
	on(15).routine = %loc(w_c4_help_routine)
	on(16).key = 'U'
	on(16).routine = %loc(w_c4_up_one_dir_lev_routine)
	on(17).key = 'u'
	on(17).routine = %loc(w_c4_up_one_dir_lev_routine)
	on(18).key = 'R'
	on(18).routine = %loc(w_c4_select_realtime)
	on(19).key = 'r'
	on(19).routine = %loc(w_c4_select_realtime)
	on(20).key = 'L'
	on(20).routine = %loc(w_c4_select_lastfile)
	on(21).key = 'l'
	on(21).routine = %loc(w_c4_select_lastfile)
	on(22).key = 'P'
	on(22).routine = %loc(w_c4_select_by_prompt)
	on(23).key = 'p'
	on(23).routine = %loc(w_c4_select_by_prompt)
	on(24).key = 'I'
	on(24).routine = %loc(w_c4_size_toggle)
	on(25).key = 'i'
	on(25).routine = %loc(w_c4_size_toggle)
	on(26).key = 'K'
	on(26).routine = %loc(w_c4_display_last_file_used)
	on(27).key = 'k'
	on(27).routine = %loc(w_c4_display_last_file_used)
	on(28).key = 'V'
	on(28).routine = %loc(use_virtual_dir)
	on(29).key = 'v'
	on(29).routine = %loc(use_virtual_dir)

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This routine calls lib$put_output to print several lines of helpful
! screen usage information.  It is intended to be invoked when the keyboard's
! HELP key is pressed.  Hot keys described in the help text should match
! those actually coded in routine w_c4_setup_screen_fields().
!------------------------------------------------------------------------------
	integer*4	function	w_c4_help_routine(xi)
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
	call lib$put_output(' V or v       other chooser, if applicable')
	call lib$put_output(' Cntl-W       Refresh screen')
	call lib$put_output(' Cntl-Z       Exit screen, no selection made')
	call lib$put_output(blank_line)
	call lib$put_output(blank_line)
	call lib$put_output(' Press any key to continue...')
	call get_key(dummy)
	call w_c4_refresh(xi)

!	if (xi .eq. i_lf) then
!	   call w_c4_msg('Press RETURN to accept this file, CNTL/Z to quit.')
!	else if (xi .eq. i_sdir) then
!	   call w_c4_msg('Use arrows to move bar, press DO to change directory.')
!	else if (xi .eq. i_sdir) then
!	   call w_c4_msg('Use arrows to move bar, press DO to select file.')
!	end if

	w_c4_help_routine = 1

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! A generic "do" routine continueing to linger in the module for anticipated
! future debugging efforts.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_do_routine(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi

	w_c4_do_routine = 0

	if (xi .eq. 0 .or. xi .ne. 0) ! dummy test
	1 call w_c4_msg('inside w_c4_do_routine')

	w_c4_do_routine = 1

	return
	end

!------------------------------------------------------------------------------
! Transmits escape sequence to go to bottom of screen.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_goto_bottom()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	call putstr(csi//'24;1f')
	w_c4_goto_bottom = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Writes a status line at top of screen that contains the current dir and date.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_update_top_status_line() 
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	parameter	pos=csi//'1;1f'
	character*78	line
	integer*4	j
	integer*4	k2len
	character*3	month_list(0:13) /'_?_', 'JAN', 'FEB', 'MAR', 'APR',
	1		'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV',
	1		'DEC', '<?>' /

	line = ' '
	if (vfl.i .eq. 0) then
	   line = ' Current Directory is WIND_DATA:'
	else if (vfl.vy(vfl.i).i .eq. 0) then
	   line = ' Current Directory is WIND_DATA:['//vfl.vy(vfl.i).year//']'
	else
	   line = ' Current Directory is WIND_DATA:['
	1	//vfl.vy(vfl.i).year//'.'
	1	//month_list(vfl.vy(vfl.i).i)//']'
	end if
	call date(line(69:77))
	line(78:78) = ' '
	call lib$put_output(reverse//pos//line//primary)	

	w_c4_update_top_status_line = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Writes a line at screen bottom containing some usefull hot key definitions.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_update_bottom_status_line() 
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
	1	bright//'P'//primary//'romptFile  '//
	1	bright//'I'//primary//'nfo  '//
	1	bright//'K'//primary//'-DisplayLastFile '
	k = k2len(line)
	call lib$put_output(pos//line(:k)//primary)

	w_c4_update_bottom_status_line = 1
	return
	end

!------------------------------------------------------------------------------
! Redraws the current screen.  Usually invoked after cntl/w or HELP keys.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_refresh()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
!	integer*4	xi		! argument unused
	call lib$put_output(cls)
	call lib$put_output(my_screen(:my_screen_size))
	call w_c4_update_top_status_line()
	call w_c4_update_bottom_status_line()
	call scr_mgr_show_gets(f)
	call scr_mgr_show_scrolls(s)
	w_c4_refresh = 1
	return
	end

!------------------------------------------------------------------------------
! Designated screen exit routine.  Always returns zero to signal the screen
! manager to relenquish program control.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_exit()
	implicit	none
	include		'scr_manager_def.for'
!	integer*4	xi		! argument unused
	! return low bit clear for screen exit
	w_c4_exit = 0
	return
	end

!------------------------------------------------------------------------------
! Prints the message passed via argument str at screen bottom.  Also beeps.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_err(str)
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
	w_c4_err = 0
	return
	end

!------------------------------------------------------------------------------
! Prints message str at screen bottom without beeping.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_msg(str)
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
	w_c4_msg = 0
	return
	end

!------------------------------------------------------------------------------
! Sets screen field index xi to next field skipping over readonly fields.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_next_field(xi)
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
	w_c4_next_field = 1
	return
	end

!------------------------------------------------------------------------------
! Sets screen field index xi to previous field skipping over readonly fields.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_prev_field(xi)
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
	w_c4_prev_field = 1
	return
	end

!------------------------------------------------------------------------------
! Calls w_c4_prev_field to move to previous field on screen.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_up(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	call w_c4_prev_field(xi)
	w_c4_up = 1
	return
	end

!------------------------------------------------------------------------------
! Calls w_c4_next_field to move to next field on screen.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_down(xi)
	implicit	none
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	call w_c4_next_field(xi)
	w_c4_down = 1
	return
	end

!------------------------------------------------------------------------------
! Sets screen field index pointer xi to point to a screen object based on
! the user's last keystroke.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_set_object_pointer(xi)
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
	      call w_c4_err('Cannot set object pointer (A).')
	   end if
	else if (g(xi).retkey .eq. 'PREV') then
	   if (xi .eq. i_sdir) then
	      xi = i_sfile
	   else if (xi .eq. i_sfile) then
	      xi = i_sdir
	   else
	      call w_c4_err('Cannot set object pointer (B).')
	   end if
	else if (g(xi).retkey .eq. 'CNTL/I') then
	   call w_c4_prev_field(xi)
	else if (g(xi).retkey .eq. 'CNTL/M') then
	   call w_c4_next_field(xi)
	else
	   call w_c4_err('Cannot set object pointer (G).')
	end if
	w_c4_set_object_pointer = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Calls make_box and make_line to build the screen outlines.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_build_screen()
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
	logical*4	first_time /.true./

	w_c4_build_screen = 0

	if (.not. first_time) return
	first_time = .false.

	j = len(selg1)
	my_screen(:j) = selg1

!	i = j + 1
!	j = i + len(bright) - 1
!	my_screen(i:j) = bright
	i = j + 1
	ok = make_box(r1,c1,r2,c2,my_screen(i:),len_box)

	r1=9
	i = j + len_box + 1
	j = i - 1
	ok = make_line(r1,c1,r2,c2,my_screen(i:),len_box)

!	r1=12
!	i = j + len_box + 1
!	j = i - 1
!	ok = make_line(r1,c1,r2,c2,my_screen(i:),len_box)

	i = j + len_box + 1
	j = i + len(primary) - 1
	my_screen(i:j) = primary

!	i = j + 1
!	j = i + len(a) - 1
!	my_screen(i:j) = a

	my_screen_size = j
	w_c4_build_screen = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key R,r routine to select the REALTIME stream.
! Triggers the screen manager to return control to parent module.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_select_realtime()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'

	w_c4_select_realtime = 0
	result_file = 'REALTIME'

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key L,l routine to select the last file used stream.
! Triggers the screen manager to return control to parent module.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_select_lastfile()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'

	w_c4_select_lastfile = 0
	result_file = last_file_used

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! The on_key K,k routine to display the file name selected last program run.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_display_last_file_used()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	k
	integer*4	k2len

	w_c4_display_last_file_used = 1
	if (last_file_used(1:1) .gt. ' ') then
	   k = k2len(last_file_used)
	   call w_c4_msg('Last file was: '//last_file_used(:k))
	else
	   call w_c4_msg('There is no previously used .WND file for this process.')
	end if

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This is the action routine called when the user presses S or s on the
! keyboard to toggle display of file sizes.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_size_toggle(xi)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	integer*4	switch_back

	w_c4_size_toggle = 1

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This is the action routine called when the user presses P or p on the
! keyboard to request a prompt for a "manual" file name input.
!------------------------------------------------------------------------------
	integer*4	function	w_c4_select_by_prompt()
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	ios
	integer*4	k2len
	character*3	err
	integer*4	i
	parameter	prompt=csi//'24;1f'//erase_line//bright//
	1		'Enter file spec: '//primary

	w_c4_select_by_prompt = 0
	call putstr(prompt)
	read(5,'(q,a)',iostat=ios) i,result_file
	if (ios .ne. 0) then
	   write(err,'(i3.3)') ios
	   call w_c4_refresh()
	   call w_c4_err('Error entering file name, iostat='//err)
	   w_c4_select_by_prompt = 1
	else if (k2len(result_file) .lt. 1) then
	   call w_c4_msg('No file entered.')
	   w_c4_select_by_prompt = 1
	end if

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
	integer*4	function	w_c4_scroll_on_key_handler(xi,keyname)
	implicit	none
	include		'termvideo_def.for'
	include		'scr_manager_def.for'
	include		'file_select2_def.for'
	integer*4	xi
	character*(*)	keyname
	integer*4	i

	w_c4_scroll_on_key_handler = 0
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
	else
	   w_c4_scroll_on_key_handler = 1
	end if

	return
	end
