! wind_sys_lib.for - sundry calls to sys$..., lib$... routines

!------------------------------------------------------------------------------
! Gets the username associated with a given process id.
*-----------------------------------------------------------------------------
	integer*4	function	get_username(pid,username)
	implicit	none
	integer*4	pid		! VMS process id
	character*(*)	username	! VMS ASCII user name
	include		'($jpidef)'
	integer*4	lib$getjpi

	get_username = lib$getjpi(jpi$_username,pid,,,username,)
	return
	end

!------------------------------------------------------------------------------
! Gets the process name for a given process id.
*-----------------------------------------------------------------------------
	integer*4	function	get_process_name(pid,process_name)
	implicit	none
	integer*4	pid		! VMS process id
	character*(*)	process_name	! process name
	include		'($jpidef)'
	integer*4	sys$getjpiw

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /itmlst3_1item/
		structure  item
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_1item/ jpi_list

	process_name = ' '
!	if (pid .eq. 0) return

	jpi_list.item.code = jpi$_prcnam
	jpi_list.item.buffer_length = len(process_name)
	jpi_list.item.buffer_address = %loc(process_name)
	jpi_list.item.retlen_address = 0

	jpi_list.end_of_list = 0

	get_process_name = sys$getjpiw (,pid,,jpi_list,iosb,,)
!	if (.not. get_process_name) call lib$signal(%val(get_process_name))
	
	return
	end

!------------------------------------------------------------------------------
! Gets the VMS image name for a given process id.
*-----------------------------------------------------------------------------
	integer*4	function	get_image_name(pid,image_name)
	implicit	none
	integer*4	pid		! VMS process id
	character*(*)	image_name
	include		'($jpidef)'
	integer*4	sys$getjpiw

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /itmlst3_1item/
		structure  item
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_1item/ jpi_list

	image_name = ' '
!	if (pid .eq. 0) return

	jpi_list.item.code = jpi$_imagname
	jpi_list.item.buffer_length = len(image_name)
	jpi_list.item.buffer_address = %loc(image_name)
	jpi_list.item.retlen_address = 0

	jpi_list.end_of_list = 0

	get_image_name = sys$getjpiw (,pid,,jpi_list,iosb,,)
!	if (.not. get_image_name) call lib$signal(%val(get_image_name))
	
	return
	end

!------------------------------------------------------------------------------
! This collection of entry points returns the equivalence strings associated
! with VMS logical names.  Each entry point works on specific VMS logical
! name table with the exception of get_file_dev_logical_name_str which works
! like RMS file name resolution in that it searches the process, group, and
! system tables in order.
*-----------------------------------------------------------------------------
	! specify the LNM$PROCESS_TABLE
	integer*4	function get_logical_name_str(logical_name, ret_string)
	implicit 	none
	include		'($lnmdef)'
	character*(*)	logical_name
	character*(*)	ret_string
	character*255	working_logical_name
	character*24	tabnam
	character*17	protab		/'LNM$PROCESS_TABLE'/
	character*9	grotab		/'LNM$GROUP'/
	character*16	systab		/'LNM$SYSTEM_TABLE'/
	character*21	dirtab		/'LNM$PROCESS_DIRECTORY'/
	character*12	fildevtab	/'LNM$FILE_DEV'/ ! works like rms
	integer*4	ret_attrib
	integer*4	ret_length
	character*(*)	esc_null
	parameter	(esc_null=char(27)//char(0))
	integer*4	get_group_logical_name_str	! an entry point
	integer*4	get_system_logical_name_str	! an entry point
	integer*4	get_dir_logical_name_str	! an entry point
	integer*4	get_file_dev_logical_name_str	! an entry point
	integer*4	len_tabnam
	integer*4	status
	integer*4	sys$trnlnm
	integer*4	k2len

	structure /itmlst3_3items/
		structure  item(3)
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_3items/ trnlst

	! specify the LNM$PROCESS_TABLE
	tabnam = protab
	len_tabnam = len(protab)
	goto 10

	! specify the LNM$GROUP table
	entry get_group_logical_name_str(logical_name, ret_string)
	tabnam = grotab
	len_tabnam = len(grotab)
	goto 10

	! specify the LNM$SYSTEM_TABLE
	entry get_system_logical_name_str(logical_name, ret_string)
	tabnam = systab
	len_tabnam = len(systab)
	goto 10

	! specify the LNM$PROCESS_TABLE
	entry get_dir_logical_name_str(logical_name, ret_string)
	tabnam = dirtab
	len_tabnam = len(dirtab)
	goto 10

	! specify LNM$FILE_DEV for RMS-like file name resolution
	entry get_file_dev_logical_name_str(logical_name, ret_string)
	tabnam = fildevtab
	len_tabnam = len(fildevtab)
	goto 10

 10	ret_string = ' '
	get_logical_name_str = 0
	working_logical_name = logical_name
	ret_length = index(working_logical_name,' ') - 1
	if (ret_length .lt. 1) ret_length = len(working_logical_name)
	if (ret_length .lt. 1) return

	trnlst.item(1).code = lnm$_string
	trnlst.item(1).buffer_length = len(ret_string)
	trnlst.item(1).buffer_address = %loc(ret_string)
	trnlst.item(1).retlen_address = 0

	trnlst.item(2).code = lnm$_attributes
	trnlst.item(2).buffer_length = 4
	trnlst.item(2).buffer_address = %loc(ret_attrib)
	trnlst.item(2).retlen_address = 0

	trnlst.item(3).code = lnm$_length
	trnlst.item(3).buffer_length = 4
	trnlst.item(3).buffer_address = %loc(ret_length)
	trnlst.item(3).retlen_address = 0

	trnlst.end_of_list = 0

100	status = sys$trnlnm(lnm$m_case_blind,
	1	tabnam(:len_tabnam),
	1	working_logical_name(1:ret_length),
	1	,trnlst)
!	if (.not. status) call lib$stop(%val(status))
 	if (status .and. (iand(lnm$m_terminal, ret_attrib) .eq. 0)) then
	   working_logical_name = ret_string(1:ret_length)
	   go to 100
	end if

	if (ret_string(1:2) .eq. esc_null) then
	   ret_string = ret_string(5:ret_length)
	   ret_length = ret_length - 4
	end if

	status = k2len(ret_string) .gt. 0
	get_logical_name_str        = status
	return
	end

!------------------------------------------------------------------------------
! Gets the VMS process id for the current process.
*-----------------------------------------------------------------------------
	subroutine get_pid ( pid)
	implicit 	integer*4 (a-z)
	integer*4	pid
	include		'($jpidef)'
	include		'($syssrvnam)'

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /itmlst3_1item/
		structure  item
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_1item/ jpi_list

	jpi_list.item.code = jpi$_pid
	jpi_list.item.buffer_length = 4
	jpi_list.item.buffer_address = %loc(pid)
	jpi_list.item.retlen_address = 0

	jpi_list.end_of_list = 0


	status = sys$getjpiw (,,,jpi_list,iosb,,)
	if (.not. status) call lib$signal(%val(status))
	if (.not. iosb.status) call lib$stop(%val(iosb.status))

	return
	end

!------------------------------------------------------------------------------
! Determines the VMS process id number from the process name.
*-----------------------------------------------------------------------------
	subroutine get_pid_from_name (process_name, pid)

	implicit 	integer*4 (a-z)
	include		'($jpidef)'
	include		'($syssrvnam)'
	character*(*)	process_name
	integer*4	pid

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /itmlst3_1item/
		structure  item
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_1item/ jpi_list

	jpi_list.item.code = jpi$_pid
	jpi_list.item.buffer_length = 4
	jpi_list.item.buffer_address = %loc(pid)
	jpi_list.item.retlen_address = 0

	jpi_list.end_of_list = 0

	i = index(process_name,' ') - 1
	if (i.lt.0) i = len(process_name)
	pid = 0

	status = sys$getjpiw (,,process_name(1:i),jpi_list,iosb,,)
!	if (.not. status) call lib$stop(%val(status))
!	if (.not. iosb.status) call lib$stop(%val(status))
	
	return
	end

!------------------------------------------------------------------------------
! Gets the count (number) of references with named device.  Used by wind_lib
! to determine the number of wind_lib users on a given WIND/WAVES GSE system.
*-----------------------------------------------------------------------------
	integer*4	function	get_refcnt (device_name,num)

	implicit 	none
	include		'($dvidef)'
	character*(*)	device_name
	integer*4	num
	integer*4	ok
	integer*4	sys$getdviw
	integer*4	i

	structure /getdvi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getdvi_iosb/ iosb

	structure /itmlst3_1item/
		structure  item
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_1item/ dvi_list

	get_refcnt = 0

	dvi_list.item.code = dvi$_refcnt
	dvi_list.item.buffer_length = 4
	dvi_list.item.buffer_address = %loc(num)
	dvi_list.item.retlen_address = 0

	dvi_list.end_of_list = 0

	i = index(device_name,' ') - 1
	if (i.le.0) i = len(device_name)
	num = 0
	if (i.le.0) return

	ok = sys$getdviw (,,device_name(1:i),dvi_list,iosb,,,)
!	if (.not. ok) call lib$signal(%val(ok))
!	if (.not. iosb.status) then
!	   type *, '**** from iosb ****'
!	   call lib$signal(%val(iosb.status))
!	end if
	
	get_refcnt = ok

	return
	end

!------------------------------------------------------------------------------
! Gets the number of display lines on the specified terminal.
*-----------------------------------------------------------------------------
	integer*4	function	get_tt_pagelen (device_name,num)
	implicit 	none
	include		'($dvidef)'
	character*(*)	device_name
	integer*4	num
	integer*4	ok
	integer*4	sys$getdviw
	integer*4	i

	structure /getdvi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getdvi_iosb/ iosb

	structure /itmlst3_1item/
		structure  item
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_1item/ dvi_list

	get_tt_pagelen = 0

	dvi_list.item.code = dvi$_tt_page
	dvi_list.item.buffer_length = 4
	dvi_list.item.buffer_address = %loc(num)
	dvi_list.item.retlen_address = 0

	dvi_list.end_of_list = 0

	i = index(device_name,' ') - 1
	if (i.le.0) i = len(device_name)
	num = 0
	if (i.le.0) return

	ok = sys$getdviw (,,device_name(1:i),dvi_list,iosb,,,)
!	if (.not. ok) call lib$signal(%val(ok))
!	if (.not. iosb.status) then
!	   type *, '**** from iosb ****'
!	   call lib$signal(%val(iosb.status))
!	end if
	
	get_tt_pagelen = ok

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Allows detached process to use "group" mailboxes.
! This routine uses sys$crelnm to "perform" the dcl command:
!     define/table=lnm$process_directory lnm$temporary_mailbox lnm$group
c------------------------------------------------------------------------------
	 integer*4	function define_group_table()
! 
	implicit	 none
	include	  '($lnmdef)/nolist'
	character	tabnam*32, lognam*32, equnam*32
	integer*4	len_lognam, len_tabnam, len_equnam
	structure /itmlst3_1item/
	   structure item
		integer*2	buffer_length
		integer*2	code
		integer*4	buffer_address
		integer*4	retlen_address
	   end structure
	   integer*4	end_of_list
	end structure
	record /itmlst3_1item/ def_list
	integer*4	sys$crelnm

	tabnam = 'LNM$PROCESS_DIRECTORY'
	lognam = 'LNM$TEMPORARY_MAILBOX'
	equnam = 'LNM$GROUP'
	len_tabnam = index(tabnam,' ')-1
	len_lognam = index(lognam,' ')-1
	len_equnam = index(equnam,' ')-1
	def_list.item.code = lnm$_string
	def_list.item.buffer_length = len_equnam
	def_list.item.buffer_address = %loc(equnam)
	def_list.item.retlen_address = 0
	def_list.end_of_list = 0

	define_group_table = 
	1	sys$crelnm(,tabnam(:len_tabnam),lognam(:len_lognam),,def_list)
	if (.not. define_group_table) call lib$signal(%val(define_group_table))

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Traps cntl/y and cntl/c by via qiow to the terminal.  Control is
! given to the routine passed as the user_routine argument when a
! control/y/c is entered.  The user's routine must re-enable the
! control/y/c trap by recalling this routine (I think) if the user wants
! to trap control/y/c again.
c------------------------------------------------------------------------------
	integer*4	function	trap_cntl_y_c(user_routine)
	implicit	none
	include		'($iodef)/nolist'
	integer*4	status
	integer*4	stdin
	integer*4	sys$assign
	integer*4	sys$qiow
	integer*4	user_routine

	! io status block returned by $qio 
	structure /qio_iosb/
		integer*2	status
		integer*2	bytes
		integer*4	pid
	end structure

	record /qio_iosb/ iosb

	trap_cntl_y_c = 0
        
! get a channel to stantard input

	status = sys$assign('sys$input',stdin,,)
	if (.not. status) goto 10

! this qio sets up the trap

	status = sys$qiow(,
	1		%val(stdin),
	1		%val(io$_setmode.or.io$m_ctrlcast.or.io$m_ctrlyast),
	1		iosb,,,
	1		%val(user_routine),
	1		,
	1		,,,)
	if (.not. status) goto 20
	if (.not. iosb.status) goto 30
                                                       
	trap_cntl_y_c = 1

	return
 10	type '(1x,a,1x,z8.8)', 'Cannot assign channel to sys$input.', status
	return
 20	type '(1x,a,1x,z8.8)', 'Error from qio to trap cntl/y/c.', status
	return
 30	type '(1x,a,1x,z8.8)', 'Error from qio iosb to trap cntl/y/c.', 
	1	iosb.status
	return
	end

!------------------------------------------------------------------------------
! Enable/disable cntl/y as per run-time library lib-122
!------------------------------------------------------------------------------
	integer*4	function	disable_cntl_y_c()
	integer*4	disable_mask /'02000000'x/
	integer*4	lib$disable_ctrl
	integer*4	lib$enable_ctrl
	integer*4	enable_cntl_y_c


	disable_cntl_y_c = lib$disable_ctrl(disable_mask,)
	if (.not. disable_cntl_y_c) goto 9

	return
  9	type '(1x,a,1x,z8.8)', 'Cannot disable cntl/y, status:',
	1	disable_cntl_y_c
	return

	!-----------------------------------------------------------------------
	entry		enable_cntl_y_c()

	enable_cntl_y_c  = lib$enable_ctrl(disable_mask,)
	if (.not. enable_cntl_y_c) goto 50

	return
 50	type '(1x,a,z8.8)', 'Cannot enable cntl/y, status: ',
	1	enable_cntl_y_c
	return

	end

	options/extend_source
!------------------------------------------------------------------------------
! Allows detached process to use "system" mailboxes.
! This routine uses sys$crelnm to "perform" the dcl command:
!     define/table=lnm$process_directory lnm$temporary_mailbox lnm$system
c------------------------------------------------------------------------------
	integer*4	function define_system_table()
	implicit	 integer*4 (a-z)
	include	  '($lnmdef)/nolist'
	character	tabnam*32, lognam*32, equnam*32
	integer*4	len_lognam, len_tabnam, len_equnam
	structure /itmlst3_1item/
		  structure  item
			   integer*2	buffer_length
			   integer*2	code
			   integer*4	buffer_address
			   integer*4	retlen_address
		  end structure
		  integer*4	end_of_list
	end structure
	record /itmlst3_1item/ def_list

	tabnam = 'LNM$PROCESS_DIRECTORY'
	lognam = 'LNM$TEMPORARY_MAILBOX'
	equnam = 'LNM$SYSTEM'
	len_tabnam = index(tabnam,' ')-1
	len_lognam = index(lognam,' ')-1
	len_equnam = index(equnam,' ')-1
	def_list.item.code = lnm$_string
	def_list.item.buffer_length = len_equnam
	def_list.item.buffer_address = %loc(equnam)
	def_list.item.retlen_address = 0
	def_list.end_of_list = 0

	define_system_table = 
	1	sys$crelnm(,tabnam(:len_tabnam),lognam(:len_lognam),,def_list)
	if (.not. define_system_table) 
	1	call lib$signal(%val(define_system_table))

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Calls the system service sys$getmsg for the message string associated with a
! return status value.  Msgtxt need be no larger that 256 characters, but
! smaller values may result in truncation.
!------------------------------------------------------------------------------
	integer*4	function	get_msg_text(msgid,msgtxt,msglen)
	implicit	none
	integer*4	msgid		! status value needing assoc. text
	character	msgtxt*(*)	! message text returned via arg 2
	integer*4	msglen		! #chars written by $getmsg
	parameter	flags='0f'x	! says return all components of text
	integer*4	sys$getmsg	! a function

	get_msg_text = sys$getmsg(%val(msgid),msglen,msgtxt, %val(flags),)
	if (.not. get_msg_text) call lib$signal(%val(get_msg_text))

	return
	end

!------------------------------------------------------------------------------
! Puts specified VMS process id into the lock value block identified by
! the resource name conained in argument resname.
!------------------------------------------------------------------------------
	integer*4	function	put_pid_in_lckvalblk(resnam,lksb)
	implicit	none
	character	resnam*(*)
	include		'($lckdef)/nolist'		! definitions
	integer*4	sys$enqw			! a system service
	integer*4	sys$deq				! a system service
	integer*4	lkmode				! lock mode
	integer*4	lksb(6)				! lock status + val blks
	integer*4	flags				! $enqw switches
	integer*4	put_status_in_lckvalblk		! an entry point

	! first take an exclusive lock
	lkmode = lck$k_exmode				! an exclusive lock
	flags = lck$m_system .or. lck$m_valblk		! lock characteristics
	put_pid_in_lckvalblk =
	1	sys$enqw(,%val(lkmode),lksb,%val(flags),resnam,,,,,,)
	if (.not. put_pid_in_lckvalblk) return

	! now convert the lock, updates the master copy of the value block
	lksb(3) = 'ffffffff'x				! make it true
	call get_pid(lksb(4))				! the process pid
	lkmode = lck$k_nlmode				! a null lock
	flags = lck$m_system .or. lck$m_valblk .or. lck$m_convert
	put_pid_in_lckvalblk =
	1	sys$enqw(,%val(lkmode),lksb,%val(flags),resnam,,,,,,)
	if (.not. put_pid_in_lckvalblk) return

	return

	!----------------------------------------------------------------------
	entry		put_status_in_lckvalblk(resnam,lksb)

	! convert the lock to exclusive so we can deque at exclusive mode
	lkmode = lck$k_exmode
	flags = lck$m_system .or. lck$m_convert
	put_status_in_lckvalblk = 
	1	sys$enqw(,%val(lkmode),lksb,%val(flags),resnam,,,,,,)
	if (.not. put_status_in_lckvalblk) return

	! deque the lock, this writes the value block to the data base
	! (note that icond is lksb(4))
	put_status_in_lckvalblk = sys$deq(%val(lksb(2)),lksb(3),,)
	if (.not. put_status_in_lckvalblk) return

	return

	end

	options/extend_source
!------------------------------------------------------------------------------
! Makes an entry in the system logical name table for argument lname and
! associates it with equivalence string ename.
!------------------------------------------------------------------------------
	integer*4	function	set_system_logical_name(lname,ename)
	implicit	none
	include		'($lnmdef)'
	character*(*)	lname				! logical name
	character*(*)	ename				! equivalence name
	integer*4	ok
	integer*4	sys$crelnm
	integer*4	nstr
	integer*4	k2len
	structure	/item_list_3/
	   integer*2	buflen
	   integer*2	itmcod
	   integer*4	bufadr
	   integer*4	rlnadr
	end structure
	record		/item_list_3/ il3(2)

	nstr = k2len(ename)

	il3(1).buflen = nstr
	il3(1).itmcod = lnm$_string
	il3(1).bufadr = %loc(ename)
	il3(1).rlnadr = 0

	il3(2).buflen = 0
	il3(2).itmcod = 0
	il3(2).bufadr = 0
	il3(2).rlnadr = 0

	ok = sys$crelnm(,			! attr 
	1		'LNM$SYSTEM_TABLE',	! tabnam
	1		lname,			! lognam
	1		,			! acmode
	1		il3)			! itmlst
	if (.not. ok) then
	   type '(1x,a,a,a,z8.8,a)',
	1	'Cannot define ', lname,
	1	' in system table, status:', ok, '.'
	else
	   nstr = max(nstr,1)
!	   type '(1x,a,a,a,a)',
!	1	lname, ' has been defined to ', ename(:nstr),
!	1	'.'
	end if

	set_system_logical_name = ok

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Makes an entry in the process logical name table for argument lname and
! associates it with equivalence string ename.
!------------------------------------------------------------------------------
	integer*4	function	set_process_logical_name(lname,ename)
	implicit	none
	include		'($lnmdef)'
	character*(*)	lname				! logical name
	character*(*)	ename				! equivalence name
	integer*4	ok
	integer*4	sys$crelnm
	integer*4	nstr
	integer*4	k2len
	structure	/item_list_3/
	   integer*2	buflen
	   integer*2	itmcod
	   integer*4	bufadr
	   integer*4	rlnadr
	end structure
	record		/item_list_3/ il3(2)
	integer*4	set_job_log_name_c		! an entry point
	byte		cname(*)
	byte		cvalue(*)
	parameter	c_len=256
	structure /c_string/
	   union
	   map
	      character*(c_len)	name
	      character*(c_len)	value
	   end map
	   map
	      byte		bname(c_len)
	      byte		bvalue(c_len)
	   end map
	   end union
	end structure
	record /c_string/ r
	integer*4	name_len
	integer*4	i, k
	character*32	table

	nstr = k2len(ename)

	il3(1).buflen = nstr
	il3(1).itmcod = lnm$_string
	il3(1).bufadr = %loc(ename)
	il3(1).rlnadr = 0

	r.name = lname
	name_len = k2len(r.name)
	table = 'LNM$PROCESS_TABLE'

	goto 1000

	!----------------------------------------------------------------------
	! C callable
	entry	set_job_log_name_c(cname, cvalue)

	i = 1
	do while(cvalue(i) .ne. 0 .and. i .lt. c_len)
	   r.bvalue(i) = cvalue(i)
	   i = i + 1
	end do
	i = i - 1

	il3(1).buflen = i
	il3(1).itmcod = lnm$_string
	il3(1).bufadr = %loc(r.value)
	il3(1).rlnadr = 0

	i = 1
	do while(cname(i) .ne. 0 .and. i .lt. c_len)
	   r.bname(i) = cname(i)
	   i = i + 1
	end do
	name_len = i - 1

	table = 'LNM$JOB'

	goto 1000

 1000	continue

	il3(2).buflen = 0
	il3(2).itmcod = 0
	il3(2).bufadr = 0
	il3(2).rlnadr = 0

	k = k2len(table)

	ok = sys$crelnm(,			! attr 
	1		table(1:k),		! tabnam
	1		r.name(1:name_len),	! lognam
	1		,			! acmode
	1		il3)			! itmlst
	if (.not. ok) then
	   type '(1x,a,a,a,z8.8,a)',
	1	'Cannot define ', r.name(1:32),
	1	' in process table, status:', ok, '.'
	else
!	   nstr = max(nstr,1)
!	   type '(1x,a,a,a,a)',
!	1	lname, ' has been defined to ', ename(:nstr),
!	1	'.'
	end if

	set_process_logical_name = ok

	return
	end

!------------------------------------------------------------------------------
! Calls the standard VMS FORTRAN routines date and time to generate
! a character representation of the current date and time.
!------------------------------------------------------------------------------
	integer*4	function	get_ascii_time_stamp(stamp)
	character*(*)	stamp			! argument
	character	date_*9, time_*8 	! date/time strings
	character	stamp_*17		! time stamp YYMMMDD HH:MM:SS

	call date(date_)			! dd-mmm-yy
	call time(time_)			! hh:mm:ss
	stamp_ =  date_(8:9)//date_(4:6)//date_(1:2)//' '//time_//' '
	stamp  = stamp_				! no truncation error

	get_ascii_time_stamp = 1

	return
	end


!------------------------------------------------------------------------------
! Creates an ASCII time stamp of the form:
!	YYYY_DDD_HH_MM_SS
! where
!	YYYY	- is the four digit year
!	DDD	- is the Julian day of the year
!	HH	- is the hour of the day
!	MM	- is the minute of the hour
!	SS	- is the second of the minute
!------------------------------------------------------------------------------
	integer*4	function	get_doy_time_stamp(stamp)
	implicit	none
	character*(*)	stamp
	character	time_*8
	integer*4	day_of_year
	integer*4	xiday_of_year
	integer*4	month, day, year
	character*18	s

	call idate(month,day,year)
	year = year + 1900
	if (year .lt. 1990) year = year + 100
	call time(time_)
	time_(3:3) = '_'
	time_(6:6) = '_'
	day_of_year = xiday_of_year(year,month,day)
	write(s,3,err=10) year, day_of_year, time_
	stamp = s

 3	format(i4.4,'_',i3.3,'_',a8)

	get_doy_time_stamp = 1
 10	continue
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This routine is for VAX/VMS wind_sys_lib only.
! Satisfies a global shareable image reference for backward compatability
! earlier versions of wind_lib.
! calculates Julian day of year -- minimal error checking of arguments
!------------------------------------------------------------------------------
	integer*4	function	xiday_of_year(year,month,day)
	implicit	none
	integer*4	year,month,day
	integer*4	julian
	integer*4	got_a_leap_year
	parameter	a=31		! January
	parameter	b=28+a		! February
	parameter	c=31+b		! March  
	parameter	d=30+c		! April   
	parameter	e=31+d		! May    
	parameter	f=30+e		! June   
	parameter	g=31+f		! July   
	parameter	h=31+g		! August 
	parameter	i=30+h		! September
	parameter	j=31+i		! October
	parameter	k=30+j		! November
	parameter	l=31+k		! December
	integer*4	m(0:12) / 0,a,b,c,d,e,f,g,h,i,j,k,l /
	integer*4	xis_a_leap_year	! returns 1 for leap year, 0 otherwise
	integer*4	xjulian_to_mmdd	! an entry point
	integer*4	xndays_in_month	! an entry point
	integer*4	x,y,z

	xiday_of_year = 0
	if (day .lt. 1 .or. day .gt. 31) return
	if (month .lt. 1 .or. month .gt. 12) return

	xiday_of_year = m(month-1) + day +
	1	 (xis_a_leap_year(year) .and. (month .gt. 2))

	return

	!---------------------------------------------------------------------
! This routine is for VAX/VMS wind_sys_lib only.
	entry	xndays_in_month(year,month)
	xndays_in_month = 0
	if (month .lt. 1 .or. month .gt. 12) return
	xndays_in_month = m(month) - m(month-1)
	if (xis_a_leap_year(year) .and. month.eq.2)
	1 xndays_in_month = xndays_in_month + 1
	return

	!---------------------------------------------------------------------
! This routine is for VAX/VMS wind_sys_lib only.
	entry	xjulian_to_mmdd(julian, year, month, day)
	! Arguments julian and year are read only, month and day are written.

	xjulian_to_mmdd = 0
	month = 0
	day   = 0
	x     = julian
	if (julian .lt. 1 .or. julian .gt. 366) return
	got_a_leap_year = xis_a_leap_year(year)
	if ((.not. got_a_leap_year) .and. (julian .gt. 365)) return
	if (got_a_leap_year .and. (julian .gt. b)) x = x - 1

	y = 0
	do while(month .eq. 0 .and. y .lt. 12)
	   z = y + 1
	   if (x .gt. m(y) .and. x .le. m(z)) month = z
	   y = y + 1
	end do

	day = x - m(month-1)
	if ((got_a_leap_year) .and. (julian .gt. (b+1))) then
	   if (julian .gt. (m(month)+1)) then
	      day = 1
	      month = month + 1
	   end if
	else if (got_a_leap_year .and. julian .eq. 60) then
	   day = 29
	end if

	xjulian_to_mmdd = 1
	return
	end

!------------------------------------------------------------------------------
! This routine is for VAX/VMS wind_sys_lib only.
! Satisfies a global shareable image reference for backward compatability
! earlier versions of wind_lib.
! Returns 0 if year is not a leap year or 1 if year is a leap year.
!------------------------------------------------------------------------------
	integer*4	function	xis_a_leap_year(year)
	implicit	none
	integer*4	year
	xis_a_leap_year = 0
	if (mod(year,4) .gt. 0) return		! year not divisible by 4
	if (mod(year,100) .eq. 0 .and.
	1   mod(year,400) .ne. 0) return	! century year not div by 400
	xis_a_leap_year = 1
	return
	end

!------------------------------------------------------------------------------
! This routine is for VAX/VMS wind_sys_lib only.
! Satisfies a global shareable image reference for backward compatability
! earlier versions of wind_lib.
!------------------------------------------------------------------------------
	integer*4	function	xadjust_doy_and_year(doy,year)
	implicit	none
	integer*4	doy
	integer*4	year
	integer*4	leap
	integer*4	xis_a_leap_year

	leap = xis_a_leap_year(year)
	if (leap .and. (doy .gt. 366)) then
	   year = year + 1
	   doy = doy - 366
	else if (doy .gt. 365) then
	   year = year + 1
	   doy = doy - 366
	else if (doy .lt. 1) then
	   year = year - 1
	   leap = xis_a_leap_year(year)
	   if (leap) then
	      doy = 366 + doy
	   else
	      doy = 365 + doy
	   end if
	end if

	xadjust_doy_and_year = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_search_list(logname, list, sz_list)
	implicit 	none
	include		'($lnmdef)'
	character*(*)	logname		! logical name
	character*(*)	list(*)		! buffer for equivalence strings
	integer*4	sz_list

	structure /itmlst3_3items/
		structure  item(4)
			integer*2	buffer_length
			integer*2	code
			integer*4	buffer_address
			integer*4	retlen_address
		end structure
		integer*4	end_of_list
	end structure
	record /itmlst3_3items/ trnlst

	integer*4	log_index
	integer*4	ret_attrib
	integer*4	len_name
	character*(*)	esc_null
	parameter	(esc_null=char(27)//char(0))
!	character*16	tabnam /'LNM$FILE_DEV'/	 ! works like rms
	character*20	tabnam		/'LNM$PROCESS_TABLE'/
	integer*4	len_tabnam
	integer*4	ok
	integer*4	sys$trnlnm
	character*128	working_logical_name
	integer*4	i,j
	integer*4	k3len

	get_search_list = 0

	working_logical_name = logname//' '
	len_name = index(working_logical_name,' ') - 1
	if (len_name .lt. 1) len_name = len(logname)
	if (len_name .lt. 1) return
	log_index = 0
	len_tabnam = k3len(tabnam)

	trnlst.item(1).code = lnm$_index
	trnlst.item(1).buffer_length = 4
	trnlst.item(1).buffer_address = %loc(log_index)
	trnlst.item(1).retlen_address = 0

	trnlst.item(2).code = lnm$_string
	trnlst.item(2).buffer_length = len(list(1))
	trnlst.item(2).buffer_address = %loc(list(1))
	trnlst.item(2).retlen_address = 0

	trnlst.item(3).code = lnm$_attributes
	trnlst.item(3).buffer_length = 4
	trnlst.item(3).buffer_address = %loc(ret_attrib)
	trnlst.item(3).retlen_address = 0

	trnlst.item(4).code = lnm$_length
	trnlst.item(4).buffer_length = 4
	trnlst.item(4).buffer_address = %loc(len_name)
	trnlst.item(4).retlen_address = 0

	trnlst.end_of_list = 0

	i = 1
	ok = 1
	j = lnm$m_terminal
	do while(ok .and. (i .le. sz_list))

 100	   ok = sys$trnlnm(lnm$m_case_blind,
	1	tabnam(1:len_tabnam),
	1	working_logical_name(1:len_name),
	1	,
	1	trnlst)
! need to check lnm$m_terminal for recursive translations
	   if (ok) then
	      if (list(i)(1:2) .eq. esc_null) then
	         list(i) = list(i)(5:len_name)
	      end if
	      if ( (ret_attrib .and. lnm$m_exists) .ne. 0 ) then
	         if (k3len(list(i)) .gt. 0) then
	            i = i + 1
	            working_logical_name = logname
	            len_name = k3len(logname)
	            if (i .lt. sz_list) 
	1		trnlst.item(2).buffer_address = %loc(list(i))
	         end if
	      else
	         i = i - 1
	         ok = 0
	      end if
	   end if

	   log_index = log_index + 1
	end do

	if (i .ge. 1) then
	   get_search_list = 1
	   sz_list = min(i,sz_list)
	else
	   get_search_list = 0
	   sz_list = 0
	end if

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_get_file_list2(files, max_files, spec)
! Each element of array files should be 256 chars long to allow for maximum
! possible VMS file name size, otherwise truncation may occur.
	implicit	none
	character*(*)	files(*)		! write
	integer*4	max_files		! write
	character*(*)	spec			! read,mask, eg. 'wind_*def.for'
	integer*4	lib$find_file
	integer*4	lib$find_file_end
	integer*4	context /0/
	integer*4	rms_status
	integer*4	flags
	integer*4	i,j,k
	integer*4	ok
	integer*4	rms_no_more_files /'182ca'x/
	integer*4	no_such_file /'0910'x/
	integer*4	file_not_found /'18292'x/
	integer*4	k3len

	w_get_file_list2 = 0
	if (context .ne. 0) then
	   ok = lib$find_file_end(context)
	end if
	context = 0
	k = k3len(spec)

	i = 1
	ok = 1
	do while(ok .and. (i .le. max_files))
	   ok = lib$find_file(
	1	spec(:k),
	1	files(i),
	1	context,
	1	,			! default file spec
	1	,			! related file spec, not used
	1	rms_status,		! additional rms status return
	1	flags)			! 1=use search list logical name
	   if (ok) then
!	      type *, 'found ', files(i)(:60)
	      i = i + 1
	   else if (ok .eq. no_such_file) then
	      type *, '---No files found, please check selection criteria.'
	   else if (ok .eq. file_not_found) then
	      type *, '---No files found in this directory.'
	   else if (ok .ne. rms_no_more_files) then
	      type *, '---other rms error---', ok
	   end if
	end do

	j = lib$find_file_end(context)
	context = 0
	if (ok .ne. rms_no_more_files) return

	max_files = i - 1
	if (max_files .eq. 0) return
	w_get_file_list2 = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_usage
	1		(pid,bufio,cputim,dirio,pageflts,virtpeak)
	implicit 	none
	include		'($jpidef)'
	integer*4	sys$getjpiw
	integer*4	pid		! w, process id
	integer*4	bufio		! w, buffered io count
	integer*4	cputim		! w, accumulated cpu time 10msec ticks
	integer*4	dirio		! w, direct io count
	integer*4	pageflts	! w, total # of page faults
	integer*4	virtpeak	! w, peak virtual address size
	integer*4	ok

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /item_list/
		structure  it(6)
			integer*2	bufsz
			integer*2	code
			integer*4	bufaddr 
			integer*4	rla
		end structure
		integer*4	end_of_list
	end structure
	record /item_list/ x

	x.it(1).code    = jpi$_pid
	x.it(1).bufsz   = 4
	x.it(1).bufaddr = %loc(pid)
	x.it(1).rla     = 0

	x.it(2).code    = jpi$_bufio
	x.it(2).bufsz   = 4
	x.it(2).bufaddr = %loc(bufio)
	x.it(2).rla     = 0

	x.it(3).code    = jpi$_cputim
	x.it(3).bufsz   = 4
	x.it(3).bufaddr = %loc(cputim)
	x.it(3).rla     = 0

	x.it(4).code    = jpi$_dirio
	x.it(4).bufsz   = 4
	x.it(4).bufaddr = %loc(dirio)
	x.it(4).rla     = 0

	x.it(5).code    = jpi$_pageflts
	x.it(5).bufsz   = 4
	x.it(5).bufaddr = %loc(pageflts)
	x.it(5).rla     = 0

	x.it(6).code    = jpi$_virtpeak
	x.it(6).bufsz   = 4
	x.it(6).bufaddr = %loc(virtpeak)
	x.it(6).rla     = 0

	x.end_of_list = 0

	ok = sys$getjpiw (,,,x,iosb,,)
	if (.not. ok) call lib$signal(%val(ok))
	if (.not. iosb.status) call lib$stop(%val(iosb.status))

	get_usage = 1
	return
	end


	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_working_set
	1 (dfwscnt,wsauth,wsauthext,wsextent,wspeak,wsquota,wssize,ppgcnt)
	implicit 	none
	include		'($jpidef)'
	integer*4	sys$getjpiw
	integer*4	dfwscnt		! w, default working set size
	integer*4	ppgcnt		! w, # pages in the working set
	integer*4	wsauth		! w, max authorized working set size
	integer*4	wsauthext	! w, ... extent
	integer*4	wsextent	! w, current working set extent
	integer*4	wspeak		! w, peak working set size
	integer*4	wsquota		! w, working set size quota
	integer*4	wssize		! w, current working set size
	integer*4	ok

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /item_list/
		structure  it(8)
			integer*2	bufsz
			integer*2	code
			integer*4	bufaddr 
			integer*4	rla
		end structure
		integer*4	end_of_list
	end structure
	record /item_list/ x

	x.it(1).code    = jpi$_dfwscnt
	x.it(1).bufsz   = 4
	x.it(1).bufaddr = %loc(dfwscnt)
	x.it(1).rla     = 0

	x.it(2).code    = jpi$_wsauth
	x.it(2).bufsz   = 4
	x.it(2).bufaddr = %loc(wsauth)
	x.it(2).rla     = 0

	x.it(3).code    = jpi$_wsauthext
	x.it(3).bufsz   = 4
	x.it(3).bufaddr = %loc(wsauthext)
	x.it(3).rla     = 0

	x.it(4).code    = jpi$_wsextent
	x.it(4).bufsz   = 4
	x.it(4).bufaddr = %loc(wsextent)
	x.it(4).rla     = 0

	x.it(5).code    = jpi$_wspeak
	x.it(5).bufsz   = 4
	x.it(5).bufaddr = %loc(wspeak)
	x.it(5).rla     = 0

	x.it(6).code    = jpi$_wsquota
	x.it(6).bufsz   = 4
	x.it(6).bufaddr = %loc(wsquota)
	x.it(6).rla     = 0

	x.it(7).code    = jpi$_wssize
	x.it(7).bufsz   = 4
	x.it(7).bufaddr = %loc(wssize)
	x.it(7).rla     = 0

	x.it(8).code    = jpi$_ppgcnt
	x.it(8).bufsz   = 4
	x.it(8).bufaddr = %loc(ppgcnt)
	x.it(8).rla     = 0

	x.end_of_list = 0

	ok = sys$getjpiw (,,,x,iosb,,)
	if (.not. ok) call lib$signal(%val(ok))
	if (.not. iosb.status) call lib$stop(%val(iosb.status))

	get_working_set = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_sys_desc(nodename,hw_name,version)
	implicit 	none
	include		'($syidef)'
	integer*4	sys$getsyiw
	character*(*)	nodename	! w, 15 char max
	character*(*)	hw_name		! w, 31 char max
	character*(*)	version		! w, 8 char max
	integer*4	ok

	structure /getjpi_iosb/
		integer*4	status
		integer*4	%fill
	end structure
	record /getjpi_iosb/ iosb

	structure /item_list/
		structure  it(3)
			integer*2	bufsz
			integer*2	code
			integer*4	bufaddr 
			integer*4	rla
		end structure
		integer*4	end_of_list
	end structure
	record /item_list/ x

	x.it(1).code    = syi$_hw_name
	x.it(1).bufsz   = len(hw_name)
	x.it(1).bufaddr = %loc(hw_name)
	x.it(1).rla     = 0

	x.it(2).code    = syi$_nodename
	x.it(2).bufsz   = len(nodename)
	x.it(2).bufaddr = %loc(nodename)
	x.it(2).rla     = 0

	x.it(3).code    = syi$_version
	x.it(3).bufsz   = len(version)
	x.it(3).bufaddr = %loc(version)
	x.it(3).rla     = 0

	x.end_of_list = 0

	ok = sys$getsyiw (,,,x,iosb,,)
	if (.not. ok) call lib$signal(%val(ok))
	if (.not. iosb.status) call lib$stop(%val(iosb.status))

	get_sys_desc = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	mark_usage_1a()
	implicit	none

	integer*4	pid		! process id
	integer*4	bufio,bufio2	! buffered io count
	integer*4	cputim,cputim2	! accumulated cpu time 10msec ticks
	integer*4	dirio,dirio2	! direct io count
	integer*4	pageflts,pageflts2	! total # of page faults
	integer*4	virtpeak,virtpeak2	! peak virtual address size

	integer*4	ppgcnt		! # pages in the working set
	integer*4	dfwscnt		! default working set size
	integer*4	wsauth		! max authorized working set size
	integer*4	wsauthext	! ... extent
	integer*4	wsextent	! current working set extent
	integer*4	wspeak		! peak working set size
	integer*4	wsquota		! working set size quota
	integer*4	wssize		! current working set size

	integer*4	ok
	integer*4	o
	integer*4	get_usage	! a function
	integer*4	get_working_set	! a function
	integer*4	mark_usage_2a	! an entry point
	integer*4	show_usage_a	! an entry point
	integer*4	show_node_info	! an entry point
	integer*4	show_working_set! an entry point
	character*60	imagname	! image name
	character*9	cdate
	character*8	ctime
	character*16	nodename
	character*32	hw_name
	character*8	vms_version
	real*4		minutes
	real*4		seconds

	mark_usage_1a = 1
	ok = get_usage(pid,bufio,cputim,dirio,pageflts,virtpeak)
	if (ok .ne. 1) stop 'Cannot get usage (1).'
	return

	entry	mark_usage_2a()
	mark_usage_2a = 1
	ok = get_usage(pid,bufio2,cputim2,dirio2,pageflts2,virtpeak2)
	if (ok .ne. 1) stop 'Cannot get usage (2).'
	return

	entry	show_usage_a()

	show_usage_a = 1
	write(6,2,iostat=o) ' '
	write(6,2,iostat=o) 'Usage Summary (a):'
	write(6,2,iostat=o) '------------------'
	write(6,1,iostat=o) 'Buffered I/O:..............', bufio2-bufio
	seconds = cputim2-cputim
	seconds = seconds / 100.0
	minutes = seconds / 60.0
	write(6,4,iostat=o) 'CPU Time:..................', seconds, minutes
	write(6,1,iostat=o) 'Direct I/O:................', dirio2-dirio
	write(6,1,iostat=o) 'Page Faults:...............', pageflts2-pageflts

	if (virtpeak2 .gt. virtpeak) then
	write(6,1,iostat=o) 'PeakVirtualAddressSize:....', virtpeak2
	else
	write(6,3,iostat=o) 'PeakVirtualAddressSize:....', 'did not increase'
	end if
	write(6,2,iostat=o) ' '

	return
  1	format(1x,a,i)
  2	format(1x,a)
  3	format(1x,a,a)
  4	format(1x,a,f10.2,' seconds, or ',f10.4,' minutes')

	entry	show_node_info()
	show_node_info = 1
	call date(cdate)
	call time(ctime)
	call get_image_name(pid,imagname)
	call get_sys_desc(nodename, hw_name, vms_version)

	write(6,3,iostat=o) 'Program:  ', imagname
	write(6,2,iostat=o) 'Date: '//cdate//'  Time: '//ctime
	write(6,2,iostat=o) 'Node '//nodename//', a '//hw_name//
	1	' using VMS '//vms_version

	return

	entry	show_working_set()

	show_working_set = 1
	ok = get_working_set
	1 (dfwscnt,wsauth,wsauthext,wsextent,wspeak,wsquota,wssize,ppgcnt)
	if (ok .ne. 1) stop 'Cannot get working set.'

	write(6,2,iostat=o) ' '
	write(6,2,iostat=o) 'Working Set Values:'
	write(6,2,iostat=o) '-------------------'
	write(6,1,iostat=o) 'default ws size:...........', dfwscnt
	write(6,1,iostat=o) 'authorized size:...........', wsauth 
	write(6,1,iostat=o) 'authorized extent:.........', wsauthext
	write(6,1,iostat=o) 'peak ws size:..............', wspeak 
	write(6,1,iostat=o) 'ws extent:.................', wsextent
	write(6,1,iostat=o) 'ws quota:..................', wsquota
	write(6,1,iostat=o) 'ws size:...................', wssize 
	write(6,1,iostat=o) '#pages in ws:..............', ppgcnt

	return
	end

!------------------------------------------------------------------------------
! This routine calls lib$find_file to put at most sz file names into caller's
! buffer "list".  The arg_flags argument directs processing as follows:
!
!	bit 0 clr -> get first sz matching filenames
!	bit 0 set -> get last sz matching filenames
!
!	bit 1 clr -> do not search subdirectories for matching filenames
!	bit 1 set -> search subdirectories for matching filenames
!
!	bit 2 clr -> return filenames in acquired order (ascending within
!	             any given subdirectory)
!	bit 2 set -> return filenames in reverse acquired order (descending
!	             within any given subdirectory
!
! Note that including an elipse ("...") in the spec argument is effectively
! the same as flagging "search subdirectories on".
!
! Note that string arguments "dir" and "spec" should be in upper case
! when subdirectory searching is turned on.
!
	integer*4	function	w_get_file_list_from_dir
	1			(dir,spec,list,sz,sz_el,ret_sz,arg_flags)
	implicit	none
	character*(*)	dir		! directory specification in upper case
	character*(*)	spec		! filename specification in upper case
	character*(*)	list(*)		! file name buffer
	integer*4	sz		! size in #elements of file name buffer
	integer*4	sz_el		! size of one element (not used)
	integer*4	ret_sz		! # elements put in file name buffer
	integer*4	arg_flags	! argument flag bits
	integer*4	context /0/
	integer*4	lib$find_file_end
	integer*4	lib$find_file
	integer*4	rms_no_more_files /'182ca'x/
	integer*4	no_such_file /'0910'x/
	integer*4	file_not_found /'18292'x/
	integer*4	no_priv_or_protected /'1829a'x/
	integer*4	ios
	integer*4	ok
	integer*4	i,j,k,n
	integer*4	x,y,z
	integer*4	rms_status
	integer*4	flags
	integer*4	k3len
	character*80	msg
	character*256	file
	integer*4	total
	logical*4	done
	integer*4	ff_push
	integer*4	ff_pop
	integer*4	ff_clear
	character*256	srch_spec
	character*256	match_spec
	integer*4	wc_match_spec		! a function
	logical*4	test_match
	logical*4	it_matched
	integer*4	n_matched

	w_get_file_list_from_dir = 0

	if (context .ne. 0) then
	   ok = lib$find_file_end(context)
	   ok = ff_clear()
	end if
	context = 0
	test_match = .false.

	ret_sz = 0
	k = k3len(spec)
	if (k .lt. 1 .or. sz_el .lt. 1) return
	flags = 0 ! had 2 ! had 'fe'x
!	if (index(spec,',').ne.0) flags = 2
	i = k3len(dir)
	if (i .lt. 1) then
	   ! dir is blank, assume spec is a logical name
	   ! eg., at DCL: 
	   ! define mylist wind_data:wi_lz_wav_19941110_v%%.dat,wind_data:*.dir
	   !  in FORTRAN: spec = 'mylist'
	   srch_spec = spec
	else if (dir(i:i) .eq. ']' .or. dir(i:i) .eq. ':') then
	   if ( (arg_flags .and. '02'x) .ne. 0) then
	      srch_spec = dir(1:i)//'*.*;'
	      match_spec = spec
	      test_match = .true.
	      call null_terminate(match_spec)
	   else
	      srch_spec = dir(1:i)//spec(1:k)
	   end if
	else
	   ! assume dir is an appropriately defined logical name
	   if ( (arg_flags .and. '02'x) .ne. 0) then
	      srch_spec = dir(1:i)//':*.*;'
	      match_spec = spec
	      test_match = .true.
	      call null_terminate(match_spec)
	   else
	      srch_spec = dir(1:i)//':'//spec(1:k)
	   end if
	end if
	k = k3len(srch_spec)
	if (test_match) call to_upper(match_spec,0,0)

	ok = 1
	j = 0
	total = 0
	done = .false.
	n_matched = 0
	do while(ok .and. (.not. done))
	   total = total + 1
	   ok = lib$find_file(
	1	srch_spec(:k),
	1	file,
	1	context,
	1	,			! default file spec
	1	,			! related file spec, not used
	1	rms_status,		! additional rms status return
	1	flags)			! 0=nowild, 1=temporary defaulting
	   if (ok) then
	      it_matched = .true.
	      if (test_match) then
	         x = index(file,'.DIR;')
	         if (x .ne. 0) then
	            i = max(k3len(file),1)
!	            type *, '...found dir: ', file(:i)
	            ok = ff_push(srch_spec,k,context)
	            if (ok .ne. 1) then
	               write(6,1,iostat=ios) 'Directory tree is too deep.'
	               write(6,1,iostat=ios) 'cannot use '//file(:i)
	            else
	               i = index(file,']')
	               srch_spec = file(1:i-1)//'.'//file(i+1:x-1)//']*.*;'
	               k = k3len(srch_spec)
!	               type *, '...new srch_spec: ', srch_spec(:k)
	               context = 0
	            end if
	            it_matched = .false.
	         else
	            y = index(file,';')
	            file(y:y) = char(0)
	            i = index(file,']') + 1
	            ok = wc_match_spec(
	1                %val(%loc(match_spec(1:1))),
	1                %val(%loc(file(i:i))) )
	            file(y:y) = ';'
	            if (ok .ne. 1) it_matched = .false.
	            ok = 1
	         end if
	      end if
	      if (it_matched) then
	         n_matched = n_matched + 1
	         j = j + 1
	         if (j .le. sz) then
	            list(j) = file
	            ret_sz = ret_sz + 1
	         else if ((arg_flags .and. '01'x) .ne. 0) then
	            ! get the last #sz matching files, use ring buffer
	            j = 1
	            list(j) = file
	            ret_sz = 1
	         end if
	         if (ret_sz .eq. sz) done = .true.
	         if ((arg_flags .and. '01'x) .ne. 0) done = .false.
	      end if
	   else if (ok .eq. no_such_file) then
	      goto 10
	   else if (ok .eq. file_not_found) then
	      if ( (arg_flags .and. '02'x) .eq. 0) goto 20
	      ok = 1
	   else if (ok .eq. rms_no_more_files) then
	      ok = lib$find_file_end(context)
	      ok = ff_pop(srch_spec, k, context)
	      if (ok .ne. 1) then
	         total = total - 1
	      end if
	   else if (ok .eq. no_priv_or_protected) then
	      ! just silently skip over these
	      if ( (arg_flags .and. '02'x) .eq. 0) goto 40
	      ok = 1
	   else
	      goto 30
	   end if
	end do

	if ((arg_flags .and. '01'x) .ne. 0) then
	   ! getting the last #sz matching files, here we
	   ! reorder the ring buffer so the files are in
	   ! the acquired order
	   if (n_matched .gt. sz .and. mod(n_matched,sz) .ne. 0) then
!	type *, '...here are the files in buffer...j=', j
!	do x=1,sz
!	   type *, x, '. ', list(x)(1:60)
!	end do
	      x = 1
	      y = min(x+j-1,sz-j)
	      done = .false.
	      do while (.not. done)
	         do k=x,y
	            z = k + j
	            file = list(k)
	            list(k) = list(z)
	            list(z) = file
	         end do
	         z = y - x + 1
	         if (z .ne. j) j = j - z
	         x = y + 1
	         y = min(x+j-1,sz-j)
	         done = x .ge. sz .or. y .lt. x
	      end do
!	type *, '...here are the reordered files in buffer...'
!	do x=1,sz
!	   type *, x, '. ', list(x)(1:60)
!	end do
	   end if
	   if (n_matched .gt. sz) ret_sz = sz
	end if

	if ((arg_flags .and. '04'x) .ne. 0) then
	   ! reverse the order of the files
	   n = ret_sz
	   j = n
	   do i=1,n/2
	      file = list(i)
	      list(i) = list(j)
	      list(j) = file
	      j = j - 1
	   end do
	end if

	if (n_matched .gt. ret_sz) n_matched = -n_matched
	w_get_file_list_from_dir = n_matched

	return
  1	format(1x, 'W_GET_FILE_LIST_FROM_DIR: ', a)
 11	format('Error getting file #',i3,', ok=',z8.8,', rms=',z8.8)
 10	continue
	write(6,1,iostat=ios) 
	1 'No files found, please check selection criteria.'
	return
 20	continue
	k = k3len(srch_spec)
	write(6,1,iostat=ios) 'No files found in this directory: '//
	1	srch_spec(1:k)
	return
 30	continue
	write(msg,11,iostat=ios) total, ok, rms_status
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	write(6,1,iostat=ios) 'search spec is: '//srch_spec(1:k)
	return
 40	continue
	k = k3len(srch_spec)
	j = index(srch_spec,']') + 1
	if (j .gt. 1) k = j
	write(6,1,iostat=ios) srch_spec(1:k)//' is a protected directory.'
	return
	end

!------------------------------------------------------------------------------
! FindFile
!------------------------------------------------------------------------------
	integer*4	function	ff_stack_routines()
	implicit	none
	character*(*)	spec
	integer*4	context
	integer*4	sz
	integer*4	ff_push
	integer*4	ff_pop
	integer*4	ff_clear
	integer*4	ff_stack_size
	integer*4	stack_size
	parameter	(stack_size=16)
	structure /stack/
	   integer*4	cntx	! context
	   integer*4	len	! useable length of spec
	   character*256 spec	! search spec
	end structure
	record /stack/ stk(stack_size)
	integer*4	p /0/
	integer*4	ok
	integer*4	lib$find_file_end

	ff_stack_routines = 0
	return

	entry	ff_push(spec,sz,context)
	ff_push = 1
	if (p .lt. stack_size) then
	   p = p + 1
	   stk(p).spec = spec
	   stk(p).len  = sz
	   stk(p).cntx = context
	   ff_push = 1
	else
	   ff_push = 0
	end if
	return

	entry	ff_pop(spec,sz,context)
	ff_pop = 1
	if (p .gt. 0) then
	   sz = stk(p).len
	   spec = stk(p).spec
	   context = stk(p).cntx
	   stk(p).spec = ' '
	   stk(p).len  = 0
	   stk(p).cntx = 0
	   p = p - 1
	   ff_pop = 1
	else
	   ff_pop = 0
	end if
	return

	entry	ff_clear()
	ff_clear = 1
	do p=1,stack_size
	   if (stk(p).cntx .ne. 0) ok = lib$find_file_end(stk(p).cntx)
	   stk(p).cntx = 0
	   stk(p).len  = 0
	   stk(p).spec = ' '
	end do
	p = 0
	return

	entry	ff_stack_size()
	ff_stack_size = p
	return
	end

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Extra functions for additions to shareable image
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

	integer*4	function	wind_sys_xx1
	wind_sys_xx1 = 0
	return
	end

	integer*4	function	wind_sys_xx2
	wind_sys_xx2 = 0
	return
	end

	integer*4	function	wind_sys_xx3
	wind_sys_xx3 = 0
	return
	end

	integer*4	function	wind_sys_xx4
	wind_sys_xx4 = 0
	return
	end

	integer*4	function	wind_sys_xx5
	wind_sys_xx5 = 0
	return
	end

	integer*4	function	wind_sys_xx6
	wind_sys_xx6 = 0
	return
	end

	integer*4	function	wind_sys_xx7
	wind_sys_xx7 = 0
	return
	end

	integer*4	function	wind_sys_xx8
	wind_sys_xx8 = 0
	return
	end
