! wind_print_lib.for -- A suite of routines for directing output to
! the WIND GSE system printer.
!
! The WIND system printer is a queued device.  These routines allow the
! user to generate hardcopy output concurrent with ongoing program execution.
!
! Users interface with this library through the following routines:
!
!	call wind_print_open_channel(channel,orientation)
!	call wind_print_line(channel,line)
!	call wind_print_close_channel(channel)
!
!

!------------------------------------------------------------------------------
! Opens a channel to a printed output stream.  This channel is in no way
! associated with a wind_lib telemetry channel.
!-----------------------------------------------------------------------------
	integer*4	function	wind_print_open_channel
	1				(ch, orientation, hardcopy_timing)
	implicit	none
	include		'wind_print_def.for/nolist'
	integer*4	ch		! channel number
	character*(*)	orientation	! 'landscape' or 'portrait', punch
	character*(*)	hardcopy_timing	! print pages at end, or by page
	integer*4	ok		! return status variable
	integer*4	first_time /1/	! initial call flag
	integer*4	wind_print_find_free_channel
	integer*4	wind_print_open_file
	integer*4	declare_wind_print_exit_handler
	integer*4	wind_print_get_printer_parms
	character*16	v_timing, v_orient

	wind_print_open_channel = 0
	v_timing = hardcopy_timing
	v_orient = orientation

	if (first_time) then
	   ok = declare_wind_print_exit_handler()
	   if (.not. ok) goto 10
	   ok = wind_print_get_printer_parms()
	   if (.not. ok) goto 20
	   first_time = 0
	end if

	ok = wind_print_find_free_channel(ch)
	if (.not. ok) goto 30

	ok = wind_print_open_file(ch, v_orient)
	if (.not. ok) goto 40

	! set the hardcopy paging flag
	call to_upper(v_timing,0,0)
	if (v_timing .eq. 'PRINT_BY_PAGE' .or.
	1   v_timing .eq. 'DEFAULT') then
	   wpu(ch).wait_until_end_to_print = 0
	else if (v_timing .eq. 'PRINT_AT_END') then
	   wpu(ch).wait_until_end_to_print = 1
	else
	   goto 50
	end if


	wind_print_open_channel = 1
	return
  1	format(1x,'WIND_PRINT_OPEN_CHANNEL: ', a, a, a)
 10	type 1, 'cannot declare exit handler.'
	return
 20	type 1, 'cannot retrieve printer parameters from input file.'
	return
 30	type 1, 'no free channels.'
	return
 40	type 1, 'cannot open print file.'
	return
 50	type 1, 'hardcopy timing must be "PRINT_BY_PAGE" or "PRINT_AT_END"',
	1	', not: ', v_timing
	return
	end

!------------------------------------------------------------------------------
! Opens a file to collect user's print channel output in.
!-----------------------------------------------------------------------------
	integer*4	function	wind_print_open_file
	1				(ch, orientation)
	implicit	none
	include		'wind_print_def.for/nolist'
	integer*4	ch		! channel number
	character*(*)	orientation	! either 'landscape' or 'portrait'
	integer*4	ok		! return status variable
	integer*4	ios		! FORTRAN iostat variable
	integer*4	lib$get_lun	! RTL function
	integer*4	k2len		! string length function
	integer*4	i_ch

	wind_print_open_file = 0
	call to_upper(orientation,0,0)
	if (ch .lt. first_printer_channel .or.
	1   ch .gt. last_printer_channel) goto 10

	! get a logical unit number for the channel
	if (wpu(ch).lun .eq. 0) then
	   ok = lib$get_lun(wpu(ch).lun)
	   if (.not. ok) goto 20
	end if

	! validate the orientation
	if (orientation .eq. 'LANDSCAPE') then
	   wpu(ch).orientation    = orientation
	   wpu(ch).lines_per_page = lines_per_landscape_page
	   wpu(ch).chars_per_line = chars_per_landscape_line
	else if (orientation .eq. 'LANDSCAPE_PUNCH') then
	   wpu(ch).orientation    = orientation
	   wpu(ch).lines_per_page = lines_per_landscape_page
	   wpu(ch).chars_per_line = chars_per_landscape_line
	else if (orientation .eq. 'PORTRAIT') then
	   wpu(ch).orientation    = orientation
	   wpu(ch).lines_per_page = lines_per_portrait_page
	   wpu(ch).chars_per_line = chars_per_portrait_line
	else if (orientation .eq. 'PORTRAIT_PUNCH') then
	   wpu(ch).orientation    = orientation
	   wpu(ch).lines_per_page = lines_per_portrait_page
	   wpu(ch).chars_per_line = chars_per_portrait_line
	else
	   goto 30
	end if
	wpu(ch).lines_per_page = wpu(ch).lines_per_page -
	1	number_of_footer_lines

	wpu(ch).version = wpu(ch).version + 1
	i_ch = ch - first_printer_channel + 1
	write(wpu(ch).filename,'(a,i1.1,i3.3)',iostat=ios,err=80)
	1 wind_print_filename(:k2len(wind_print_filename)),
	1 i_ch,
	1 wpu(ch).version

	open(	unit		= wpu(ch).lun,
	1	file		= wpu(ch).filename,
	1	carriagecontrol	= 'list',
	1	form		= 'formatted',
	1	status		= 'new',
	1	recordtype	= 'variable',
	1	recl		= wpu(ch).chars_per_line,
	1	err		= 90,
	1	iostat		= ios)

	wind_print_open_file = 1
	return
  1	format(1x,'WIND_PRINT_OPEN_FILE: ', a, i3)
 10	type 1, 'invalid channel number.'
	return
 20	type 1, 'cannot allocate a free logical unit number.'
	return
 30	type 1, 'invalid orientation, use "LANDSCAPE" or "PORTRAIT".'
	return
 80	type 1, 'cannot create new print-file name, iostat=',ios
	return
 90	type 1, 'cannot open print file, iostat=',ios
	return
	end

!------------------------------------------------------------------------------
! Opens and reads the namelist data set containing the printed page
! characteristics (e.g.: lines per page, characters per page, output queue
! name, ...)
!-----------------------------------------------------------------------------
	integer*4	function	wind_print_get_printer_parms()
	implicit	none
	include		'wind_print_def.for/nolist'
	integer*4	lun
	integer*4	ios
	character*96	filename

	namelist /wind_printer_parms/
	1	lines_per_landscape_page,
	1	lines_per_portrait_page,
	1	chars_per_landscape_line,
	1	chars_per_portrait_line,
	1	number_of_header_lines,
	1	number_of_footer_lines,
	1	max_pages_without_printing,
	1	entry_count_threshold,
	1	wind_print_filename,
	1	wind_print_queue_name,
	1	landscape_print_command,
	1	landshift_print_command,
	1	portrait_print_command,
	1	portshift_print_command

	wind_print_get_printer_parms = 0

	call lib$get_lun(lun)

	call get_logical_name_str(logical_parm_file, filename)

	open(unit		=lun,
	1    file		=filename,
	1    defaultfile	=default_parm_file,
	1    status		='old',
	1    iostat		=ios,
	1    err		=10,
	1    readonly)
	read(lun, nml=wind_printer_parms, iostat=ios, err=20)
	close(lun)
	call lib$free_lun(lun)

	wind_print_get_printer_parms = 1
	return
  1	format(1x,'WIND_PRINT_GET_PRINTER_PARMS: ', a, i3)
 10	type 1, 'cannot open parm file, ios=', ios
	return
 20	type 1, 'cannot read parm file, ios=', ios
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Allocates a free channel number which is an index into a data structure
! containing parameters and variables pertinent to processing the user's
! selected print data stream.
!------------------------------------------------------------------------------
	integer*4	function	wind_print_find_free_channel(channel)
	implicit	none
	integer*4	channel
	include		'wind_print_def.for/nolist'
	integer*4	i
	integer*4	found

	! find a free channel number
	wind_print_find_free_channel = 0
	found = 0
	i     = first_printer_channel - 1
	do while(.not. found .and. i .lt. last_printer_channel)
	   i = i + 1
	   if (wpu(i).channel .eq. 0) found = 1
	end do
	if (.not. found) goto 20

	channel	= i

	wind_print_find_free_channel = 1
	return
  1	format(1x,'WIND_PRINT_FIND_FREE_CHANNEL: ', a)
  20	type 1,  'No free channels available.'
	return
	end

!------------------------------------------------------------------------------
! Closes the user's print channel by closeing the output disk file and
! performing interior data structure maintainence.
!-----------------------------------------------------------------------------
	integer*4	function	wind_print_close_channel(ch,abort)
	implicit	none
	include		'wind_print_def.for/nolist'
	integer*4	ch		! channel number
	character*(*)	abort		! to print or not-print flag
	integer*4	ios		! FORTRAN iostat variable
	integer*4	ok		! return status variable
	integer*4	wind_print_file
	character*16	v_abort

	wind_print_close_channel = 0

	if (ch .lt. first_printer_channel .or.
	1   ch .gt. last_printer_channel) goto 10

	close(wpu(ch).lun,iostat=ios,err=90)

	v_abort = abort
	call to_upper(v_abort,0,0)
	if (v_abort .eq. 'ABORT' .or. v_abort .eq. 'CANCEL') then
	   type 1, 'printing canceled at user''s request.'
	else
	   ok = wind_print_file(wpu(ch).filename,wpu(ch).orientation)
	   if (.not. ok) goto 30
	   wpu(ch).number_print_jobs = wpu(ch).number_print_jobs + 1
	end if

	wpu(ch).page_number	= 0
	wpu(ch).line_number	= 0
	wpu(ch).filename	= ' '
	wpu(ch).last_page_printed = 0
	wpu(ch).version		= 0
	wpu(ch).channel		= 0
	wpu(ch).wait_until_end_to_print = 0

	ch = 0

	wind_print_close_channel = 1
	return
  1	format(1x,'WIND_PRINT_CLOSE_CHANNEL: ', a, i3)
 10	type 1, 'invalid channel number.'
	return
 30	type 1, 'cannot print file.'
	return
 90	type 1, 'cannot close print file, iostat=', ios
	return
	end

!------------------------------------------------------------------------------
! This routine calls lib$spawn to execute a "print" command.  Valid values
! for the "flags" variable are given by:
!	bit 0,	if set, don't wait for completion
!	bit 1,	if set, don't inherit symbols
!	bit 2,	if set, don't inherit logicals
!	bit 3,	if set, do inherit keypad
!	bit 4,	if set, send a broadcast on completion
!	bit 5,	if set, no cr-lf prefixed
!
!-----------------------------------------------------------------------------
	integer*4	function	wind_print_file
	1				(file, orientation)
	implicit	none
	include		'wind_print_def.for/nolist'
	character*(*)	file
	character*(*)	orientation
	integer*4	ok		! return status variable
	integer*4	lib$spawn
	integer*4	k2len		! returns useable string length
	character*256	print_command	! VMS DCL command that is spawned
	integer*4	flags /1/	! switches for lib$spawn

	wind_print_file = 0

	if (orientation .eq. 'LANDSCAPE') then
	   print_command = landscape_print_command//' '//file
	else if (orientation .eq. 'LANDSCAPE_PUNCH') then
	   print_command = landshift_print_command//' '//file
	else if (orientation .eq. 'PORTRAIT') then
	   print_command = portrait_print_command//' '//file
	else if (orientation .eq. 'PORTRAIT_PUNCH') then
	   print_command = portshift_print_command//' '//file
	else
	   goto 20
	end if

	ok = lib$spawn(	print_command(:k2len(print_command)),
	1		'NL:',
	1		'NL:',
	1		flags)
	if (.not. ok) goto 50

	wind_print_file = 1
	return
  1	format(1x,'WIND_PRINT_FILE: ', a, z8.8)
 20	type 1, 'invalid orientation, must be LANDSCAPE, PORTRAIT'//
	1	', LANDSCAPE_PUNCH, or PORTRAIT_PUNCH.'
	return
 50	type 1, 'cannot spawn print command, status: ', ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Writes a formfeed character to the channel ch's output file.
!------------------------------------------------------------------------------
	integer*4	function	wind_print_eject_page(ch)
	implicit	none
	integer*4	ch
	integer*4	ok
	integer*4	wind_print_line
	parameter	formfeed=char(12)

	wind_print_eject_page = 0

	ok = wind_print_line(ch,formfeed)
	if (.not. ok) goto 10

	wind_print_eject_page = 1

	return
 1	format(1x,'WIND_PRINT_EJECT_PAGE: ', a)
 10	type 1, 'cannot send a form feed to the print stream.'
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This routine
!	1) copies the contents of line to the output file
!	2) maintains page number
!	3) maintains line number
!	4) checks print queue load 
!	5) decides when to print the disk file
!	6) closes disk file prior to printing
!	7) opens new disk file after printing
!
!------------------------------------------------------------------------------
	integer*4	function	wind_print_line(ch,line)
	implicit	none
	include		'wind_print_def.for/nolist'
	integer*4	ch		! user's channel number
	character*(*)	line		! line to print
	parameter	ff=char(12)	! form feed character
	integer*4	lenline		! length of line argument
	integer*4	k		! useable length of line argument
	integer*4	got_ff		! logical, got formfeed char from user
	integer*4	new_page	! logical, new page conditions met
	integer*4	ios		! FORTRAN iostat return variable
	integer*4	ok		! return status variable
	integer*4	holding		! number of queue entries
	integer*4	executing	! number of queue entries
	integer*4	pending		! number of queue entries
	integer*4	hep		! total holding+executing+pending
	integer*4	elapsed_pages	! #pages to disk since last print
	integer*4	time_to_print	! logical, true when time to print
	integer*4	count_queue_entries
	integer*4	wind_print_file
	integer*4	wind_print_open_file
	integer*4	k2len		! a string length function

	wind_print_line = 0
	lenline = len(line)
	if (lenline .lt. 1) goto 10
	if (ch .lt. first_printer_channel .or.
	1   ch .gt. last_printer_channel) goto 20
	wpu(ch).line_number = wpu(ch).line_number + 1
	k = max(k2len(line),1)
	k = min(k,wpu(ch).chars_per_line)
	got_ff   = line(:1) .eq. ff
	new_page = wpu(ch).line_number .eq. wpu(ch).lines_per_page
	new_page = new_page .or. got_ff

	if (wpu(ch).line_number .eq. 1) call wind_print_page_header(ch)
	if (.not. got_ff) write(wpu(ch).lun, '(a)', iostat=ios, err=99) line(:k)

	if (new_page) then
	   ok = count_queue_entries(wind_print_queue_name,
	1	holding, executing, pending)
	   if (.not. ok) goto 60
	   ! how tricky should this be?
	   hep = holding + executing + pending
	   elapsed_pages = wpu(ch).page_number - wpu(ch).last_page_printed
	   time_to_print = hep .le. entry_count_threshold .or.
	1                  elapsed_pages .ge. max_pages_without_printing
	end if

	if (wpu(ch).wait_until_end_to_print) time_to_print = 0

	if (new_page .and. time_to_print) then
	   close(wpu(ch).lun)
	   ok = wind_print_file(wpu(ch).filename,wpu(ch).orientation)
	   if (.not. ok) goto 70
	   ok = wind_print_open_file(ch,wpu(ch).orientation)
	   if (.not. ok) goto 80
	   wpu(ch).line_number = 0
	   wpu(ch).number_print_jobs = wpu(ch).number_print_jobs + 1
	else if (new_page) then
	   ! this next if statement controls pagination, it also competes
	   ! with the max_lines_per_*_page values set in wind_print_def.nml
	   ! for pagination control (also account for number_of_footer_lines)
	   write(wpu(ch).lun, '(a1)') ff
	   wpu(ch).line_number = 0
	end if

	wind_print_line = 1
	return
  1	format(1x,'WIND_PRINT_LINE: ', a, i3)
 10	type 1, 'cannot print zero length line.'
	return
 20	type 1, 'invalid channel number.'
	return
 60	type 1, 'cannot count printer queue entries.'
	return
 70	type 1, 'cannot print file: '//wpu(ch).filename
	return
 80	type 1, 'cannot open disk file.'
	return
 99	type 1, 'error writing to ', wpu(ch).filename(:k2len(wpu(ch).filename))
	type '(1x,a,i3)', 'iostat=', ios
	end

!------------------------------------------------------------------------------
! Generates and copies to output a page header line containing the process
! name, image name, time, date, and page number.
!------------------------------------------------------------------------------
	integer*4	function	wind_print_page_header(ch)
	implicit	none
	include		'wind_print_def.for/nolist'
	parameter	len_right_label = 8+1+9+6+4
	integer*4	ch		! user's channel number
	integer*4	pid		! user's process identification number
	integer*4	first_time /1/	! first call flag
	integer*4	ipos		! where right label of header starts
	character*256	line /' '/	! line to print
	character*32	process_name	! user's process name
	character*80	image_name	! name of user's image 
	character*9	date_		! current date
	character*8	time_		! current time
	integer*4	ios		! FORTRAN iostat return variable
	integer*4	ok		! return status variable
	integer*4	get_pid		! a function
	integer*4	get_process_name! a function
	integer*4	get_image_name	! a function
	integer*4	k2len		! a string length function

	wind_print_page_header = 0

	if (ch.lt.first_printer_channel .or.
	1   ch.gt.last_printer_channel) goto 10

	! get the static header info and partially build the header line
	if (first_time) then
	   line = ' '
	   ok = get_pid(pid)
	   if (.not. ok) goto 20
	   ok = get_process_name(pid,process_name)
	   if (.not. ok) goto 30
	   ok = get_image_name(pid,image_name)
	   if (.not. ok) goto 40
	   first_time = 0
	   line = 'Printed by '//process_name(:k2len(process_name))//
	1         ' using '    //image_name(:k2len(image_name))
	   if (number_of_header_lines .lt. 2) number_of_header_lines = 2
	end if

	call date(date_)
	call time(time_)

	! generate additional blank lines as required
	do while(wpu(ch).line_number .lt. number_of_header_lines - 1)
	   wpu(ch).line_number = wpu(ch).line_number + 1
	   write(wpu(ch).lun, *) ' '
	end do

	! format the date, time, and page number in an internal buffer
	ipos = wpu(ch).chars_per_line - len_right_label		! not static
	wpu(ch).page_number = wpu(ch).page_number + 1
	wpu(ch).total_page_number = wpu(ch).total_page_number + 1
	write(line(ipos:),'(a8,'' '',a9,''  Page'',i4)',iostat=ios)
	1 time_, date_, wpu(ch).page_number

	! copy header line to output
	write(wpu(ch).lun,'(a)',iostat=ios) line(:wpu(ch).chars_per_line)

	! put one blank line after the header
	write(wpu(ch).lun, *) ' '

	! update the line counter
	wpu(ch).line_number = wpu(ch).line_number + 2

	wind_print_page_header = 1
	return
  1	format(1x,'WIND_PRINT_PAGE_HEADER: ', a)
 10	type 1, 'invalid channel number.'
	return
 20	type 1, 'cannot get PID.'
	return
 30	type 1, 'cannog get process name.'
	return
 40	type 1, 'cannot get image name.'
	return
	end

!------------------------------------------------------------------------------
! Returns the the number of entries (holding, executing, and pending) in
! specified queue.
!------------------------------------------------------------------------------
	integer*4	function	count_queue_entries
	1				(qname,n_hold,n_exec,n_pend)
	implicit	none
	include		'($quidef)/nolist'
	character*(*)	qname			! input queue name
	integer*4	n_hold			! number of entries holding
	integer*4	n_exec			! number of entries executing
	integer*4	n_pend			! number of entries pending
	integer*4	ok			! return status variable
	integer*4	sys$getquiw		! a system service
	integer*4	qstatus			! queue status flags
	integer*4	k2len			! string length function

	structure	/itmlst/
	   union
	   map
	      integer*2	buflen, itmcod
	      integer*4	bufadr, retadr
	   end map
	   map
	      integer*4	end_of_list
	   end map
	   end union
	end structure
	record /itmlst/ q_list(6)

	structure	/iosblk/
	   integer*4	status, zeroed
	end structure
	record /iosblk/	iosb

	n_hold = 0
	n_exec = 0
	n_pend = 0

	q_list(1).buflen = k2len(qname)
	q_list(1).itmcod = qui$_search_name
	q_list(1).bufadr = %loc(qname)
	q_list(1).retadr = 0
	q_list(2).buflen = 4
	q_list(2).itmcod = qui$_holding_job_count
	q_list(2).bufadr = %loc(n_hold)
	q_list(2).retadr = 0
	q_list(3).buflen = 4
	q_list(3).itmcod = qui$_executing_job_count
	q_list(3).bufadr = %loc(n_exec)
	q_list(3).retadr = 0
	q_list(4).buflen = 4
	q_list(4).itmcod = qui$_pending_job_count
	q_list(4).bufadr = %loc(n_pend)
	q_list(4).retadr = 0
	q_list(5).buflen = 4
	q_list(5).itmcod = qui$_queue_status
	q_list(5).bufadr = %loc(qstatus)
	q_list(5).retadr = 0
	q_list(6).end_of_list = 0

	ok = sys$getquiw(
	1		,				! efn
	1		%val(qui$_display_queue),	! func
	1		,				! nullarg
	1		q_list,				! itmlst
	1		iosb,				! io status block
	1		,				! astadr
	1		,)				! astprm
	if (ok) ok = iosb.status
	if (.not. ok) call lib$signal(%val(ok))

!	type '(1x,a,z8.8)', 'queue status flags: ', qstatus

!	if ((qstatus .and. qui$m_queue_idle) .ne. 0) type *, 'queue idle'
!	if ((qstatus .and. qui$m_queue_paused) .ne. 0) type *, 'queue paused'
!	if ((qstatus .and. qui$m_queue_stopped) .ne. 0) type *, 'queue stalled'

	count_queue_entries = ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Declares the exit handler routine that prints out the final pages of
! the user's print file.
!------------------------------------------------------------------------------
	integer*4	function	declare_wind_print_exit_handler()
	implicit	none
	integer*4	icond				! receives exit status
	integer*4	sys$dclexh			! a system service
	external	wind_print_exit_routine		! the exit routine
	structure /exhblock/				! exit handler block
	   integer*4	flink				! forward link
	   integer*4	exit_handler_addr		! exit handler address
	   byte		arg_count /0/			! argument count
	   byte		always_zero(3) /3*0/		! fill
	   integer*4	cond_value_addr			! condition value addr
	end structure
	record /exhblock/ wind_print_exit

	! declare the exit handler
	wind_print_exit.exit_handler_addr = %loc(wind_print_exit_routine)
	wind_print_exit.cond_value_addr   = %loc(icond)
	declare_wind_print_exit_handler = sys$dclexh(wind_print_exit)
	if (.not. declare_wind_print_exit_handler) goto 30
	
	return
  1	format(1x,'DECLARE_WIND_PRINT_EXIT_HANDLER: ', a, z8.8)
 30	type 1, 'Cannot declare exit handler.', declare_wind_print_exit_handler
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Called as an image exit handler this procedure prints any pending user
! print channel output.
! call to sys$exit at the end of the main program.
!------------------------------------------------------------------------------
	integer*4	function	wind_print_exit_routine(icond)
	implicit	none
	include		'wind_print_def.for/nolist'
	integer*4	icond
	integer*4	i,j
	integer*4	ok
	integer*4	wind_print_file

	wind_print_exit_routine = 0
	if (icond .eq. 0) ok = 0 ! dummy statement

	j = 0
	do i=first_printer_channel,last_printer_channel
	   j = j + 1
	   if (wpu(i).lun .gt. 0) then
	      ! channel was active during image execution
	      close(wpu(i).lun)
	      if (wpu(i).channel .ne. 0) then
	         ! wind_print_close_channel was never called
	         ok = wind_print_file(wpu(i).filename,wpu(i).orientation)
	         if (.not. ok) goto 70
	         wpu(i).number_print_jobs = wpu(i).number_print_jobs + 1
	      end if
	      type 2, j, wpu(i).total_page_number, wpu(i).number_print_jobs
	   end if
	end do

	wind_print_exit_routine = 1

	return
  1	format(1x,'WIND_PRINT_EXIT_ROUTINE: ', a)
 70	type 1, 'cannot print file: '//wpu(i).filename
	return
  2	format(1x,'Channel ',i1,' printed', i4' pages using ',i3,
	1 ' queue entries.')
	end

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Extra functions for additions to shareable image
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

	integer*4	function	wind_print_xx1
	wind_print_xx1 = 0
	return
	end

	integer*4	function	wind_print_xx2
	wind_print_xx2 = 0
	return
	end

	integer*4	function	wind_print_xx3
	wind_print_xx3 = 0
	return
	end

	integer*4	function	wind_print_xx4
	wind_print_xx4 = 0
	return
	end

	integer*4	function	wind_print_xx5
	wind_print_xx5 = 0
	return
	end

	integer*4	function	wind_print_xx6
	wind_print_xx6 = 0
	return
	end

	integer*4	function	wind_print_xx7
	wind_print_xx7 = 0
	return
	end

	integer*4	function	wind_print_xx8
	wind_print_xx8 = 0
	return
	end
