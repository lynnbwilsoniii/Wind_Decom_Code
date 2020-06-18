! keystroke.for - gets key or escape sequence through an AST.  Caller provides
! the address of an action routine to process the keystroke/escape_sequence
! when calling get_key_by_ast to activate/initialize the key trapping code.
! The exact keystroke/escape_sequence is passed to the caller's action routine
! as a FORTRAN character string argument.  A convenience routine, get_key_name,
! can be called to obtain an English-like key name such as "DO", "F12", "PREV",
! "CNTL/Z", etc., for the keystroke/escape_sequence value passed to the
! caller's action routine.  The key-trapping ast can be reset by calling the
! routine get_key_by_ast_again (without the action_routine argument) or by
! calling get_key_by_ast as before.
!
! sample routine declaration and call:
!	integer*4	action_routine
!	external	action_routine
!	call get_key_by_ast(action_routine)
!	
! sample action routine:
!	integer*4	function	action_routine(c)
!	integer*4	i
!	character*(*)	c
!	character*16	name
!
!	call get_key_name(c,name)
!
!	type *, 'The key was: ', name
!
!	call get_key_by_ast_again()
!
!	return
!	end

!------------------------------------------------------------------------------
! Gets the next keystroke, which may be a vt200 style escape sequence or
! a single character or control character, and executes the routine
! supplied by the user.  Note that get_key_by_ast returns immediately
! to the caller.  The action_routine is invoked only after a key has
! been pressed, that is, the action_routine preempts the caller's program,
! possibly seconds, minutes, hours, or days (!) later,
! which is put in a wait state until the action_routine completes.
!------------------------------------------------------------------------------
	integer*4	function	get_key_by_ast(action_routine)
	implicit	none
	integer*4	action_routine
	integer*4	get_key_by_ast_again	! an entry point
	integer*4	cancel_get_key_by_ast	! an entry point
	character*16	c
	integer*4	user_action_routine
	common /xget_c_by_ast_blk0/ c, user_action_routine

	user_action_routine = %loc(action_routine)

	!----------------------------------------------------------------------
	entry		get_key_by_ast_again()

	get_key_by_ast_again = 1
	call get_c_by_ast()

	return

	!----------------------------------------------------------------------
	entry		cancel_get_key_by_ast()

	cancel_get_key_by_ast = 1
	call cancel_get_c_by_ast()

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! This function is called as an ast from a qio system call.  One character,
! freshly received during the qio, is already in a character variable stored
! in a common area.  After the escape sequence is gathered a routine is called.
! Uses 7-bit control codes, returns -2 on error, -1 on success.  This routine
! tries to snatch up the common escape sequences sent by the vt200 series 
! keypads and function keys.  This routine also calls the routine that calls
! the user's action_routine.
!------------------------------------------------------------------------------
	integer*4	function	get_rest_of_sequence()
	implicit	none
	character*1	c
	integer*4	ok, kgetc
	parameter	esc=char(27)
	parameter	csi_8=char(155)
	parameter	ss3_8=char(143)
	parameter	left_bracket='['
	character*16	e
	integer*4	user_action_routine
	common /xget_c_by_ast_blk0/ e, user_action_routine

!	ok = sys$setast(%val(0))

	get_rest_of_sequence = -2
	if (e(1:1) .ne. esc) then
	   ! convert common eight-bit sequences
	   if (e(1:1) .eq. CSI_8) then
	      e(1:1) = esc
	      c = '['
	   else if (e(1:1) .eq. SS3_8) then
	      e(1:1) = esc
	      c = 'O'
	   else
	      goto 100
	   end if
	else
	   ok = kgetc(c)
	end if

	e(2:2) = c
	if (c.eq. left_bracket) then
	   ok = kgetc(c)
	   e(3:3) = c
	   if (c .ge. 'A' .and. c .le. 'D') goto 100	! arrow key
	   if (c .ge. '1' .and. c .le. '9') then
	      ok = kgetc(c)
	      e(4:4) = c
	      if (c .eq. '~') goto 100			! small keypad key
	      if (c .ge. '1' .and. c .le. '9') then
	         ok = kgetc(c)
	         e(5:5) = c
	         if (c .eq. '~') goto 100		! function key
	         goto 999
	      end if
	      goto 999
	   end if
	   return
	else if (c .eq. 'O') then
	   ok = kgetc(c)
	   e(3:3) = c
	   if (c .ge. 'P' .and. c .le. 'S') goto 100	! PF key
	   if (c .ge. 'A' .and. c .le. 'D') goto 100	! Appl cursor keys
	   if (c .eq. 'M') goto 100			! Appl KP Enter
	   if (c .ge. 'm' .and. c .le. 'y') goto 100	! Appl KP{0123456789,.-}
	   goto 999
	else
	   goto 999
	end if

 999	continue	! unknown escape sequence, flag error???
 100	continue
	get_rest_of_sequence = -1
	call call_user_action_routine()

!	ok = sys$setast(%val(1))

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Entry kgetc uses sys$qiow to read a single character from the terminal 
! logically defined as TT:, which is normally the interactive keyboard.
! This kgetc is normally called by get_rest_of_sequence to complete
! escape sequences (that's why qiow is used in kgetc instead of qio).
! Entry putc uses sys$quiow to write a single character to TT:.
!------------------------------------------------------------------------------
	integer*4	function	kgetc(c)
	implicit	none
	include		'($iodef)'
	integer*4	ok
	integer*4	sys$qiow
	integer*4	sys$assign
	integer*4	ch
	integer*4	func
	character*(*)	c
	structure	/myiosb/
	   integer*2	status
	   integer*2	count
	   integer*4	dsi
	end structure
	record /myiosb/ iosb
	integer*4	putstr		! an entry point
	integer*4	i
	integer*4	func2

	if (ch .le. 0) then
	   ok = sys$assign('TT:',ch,,,)
	   if (.not. ok) call lib$stop(%val(ok))
	end if
	if (func .eq. 0) then
	   func =  io$_readvblk.or.io$m_noecho.or.io$m_nofiltr !io$m_escape
	end if

	ok = sys$qiow(,					! efn
	1		%val(ch),			! channel
	1		%val(func),			! func
	1		iosb,				! io status block
	1		,				! astadr
	1		,				! astparm
	1		%ref(c),			! p1 = buffer
	1		%val(1),			! p2 = buffer size
	1		,,,)				! p3..p6
	if (ok) ok = iosb.status
	if (.not. ok) call lib$stop(%val(ok))

	kgetc = ok
	return

	!----------------------------------------------------------------------
	entry	putstr(c)

	i = len(c)

	if (ch .le. 0) then
	   ok = sys$assign('TT:',ch,,,)
	   if (.not. ok) call lib$stop(%val(ok))
	end if
	if (func2 .eq. 0) then
	   func2 = io$_writevblk
	end if

	ok = sys$qiow(,					! efn
	1		%val(ch),			! channel
	1		%val(func2),			! func
	1		iosb,				! io status block
	1		,				! astadr
	1		,				! astparm
	1		%ref(c),			! p1 = buffer
	1		%val(i),			! p2 = buffer size
	1		,,,)				! p3..p6
	if (ok) ok = iosb.status
	if (.not. ok) call lib$stop(%val(ok))
	putstr = ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Uses sys$qio (not qiow) to que a request for the next character entered
! from the keyboard (TT:).  Routine get_rest_of_sequence is passed as
! the AST routine upon qio completion.  This routine returns immediately.
! The sys$qio call here is what eventually generates the AST.
!------------------------------------------------------------------------------
	integer*4	function	get_c_by_ast()
	implicit	none
	include		'($iodef)'
	integer*4	ok
	integer*4	sys$qio
	integer*4	sys$cancel
	integer*4	sys$assign
	integer*4	sys$dassgn
	integer*4	ch /0/
	integer*4	func
	structure	/myiosb/
	   integer*2	status
	   integer*2	count
	   integer*4	dsi
	end structure
	record /myiosb/ iosb
	integer*4	cancel_get_c_by_ast		! an entry point
	integer*4	get_rest_of_sequence
	external	get_rest_of_sequence
	character*16	c
	integer*4	user_action_routine
	common /xget_c_by_ast_blk0/ c, user_action_routine

	get_c_by_ast = 1

	if (ch .le. 0) then
	   ok = sys$assign('TT:',ch,,,)
	   if (.not. ok) call lib$stop(%val(ok))
	   func =  io$_readvblk.or.io$m_noecho.or.io$m_nofiltr !io$m_escape
	end if

	c = ' '

	ok = sys$qio(,					! efn
	1		%val(ch),			! channel
	1		%val(func),			! func
	1		iosb,				! io status block
	1		get_rest_of_sequence,		! astadr
	1		,				! astparm
	1		%ref(c),			! p1 = buffer
	1		%val(1),			! p2 = buffer size
	1		,,,)				! p3..p6
	if (ok .and. (iosb.status .ne. 0)) ok = iosb.status
	if (.not. ok) call lib$signal(%val(ok))

	return

	!----------------------------------------------------------------------
	entry		cancel_get_c_by_ast

	ok = sys$cancel(%val(ch))
	if (.not. ok) type '(1x,a,z8.8)','sys$cancel error=', ok

	ok = sys$dassgn(%val(ch))
	if (.not. ok) type '(1x,a,z8.8)','sys$dassgn error=', ok

	ch =0

	cancel_get_c_by_ast = ok
	return

	end

!------------------------------------------------------------------------------
! Uses lib$callg to invoke the user's action routine originally passed to
! get_key_by_ast.
!------------------------------------------------------------------------------
	integer*4	function	call_user_action_routine()
	character*16	c
	integer*4	user_action_routine
	common /xget_c_by_ast_blk0/ c, user_action_routine
	integer*4	a(4)
	integer*4	ok
	integer*4	lib$callg

	a(1) = 1
	a(2) = %loc(a(3))
	a(3) = len(c)
	a(4) = %loc(c)

	ok = lib$callg(a, %val(user_action_routine))
!	if (.not. ok) goto 10

	call_user_action_routine = 1

	return
  1	format(1x,'CALL_USER_ACTION_ROUTINE: ', a, z8.8)
! 10	type 1, 'error calling user routine, status=', ok
!	return
	end

!------------------------------------------------------------------------------
! Returns an English-like key name for the character string found in argument
! s.  S is guaranteed to contain only a single vt200 format keystroke, which
! is a single alpha-numeric character, a control character, or a 7-bit
! escape sequence.
!------------------------------------------------------------------------------
	integer*4	function	get_key_name(s,name)
	implicit	none
	character*(*)	s
	character*(*)	name
	parameter	esc=char(27)
	parameter	del=char(127)
	parameter	left_bracket='['

	get_key_name = 0

	name = ' '

	if (s(1:1) .eq. esc) then				! escape seq's
	   if (s(2:2) .eq. left_bracket) then
	      if (s(3:3) .eq. 'A') then				! arrow keys
	         name = 'UP'
	      else if (s(3:3) .eq. 'B') then
	         name = 'DOWN'
	      else if (s(3:3) .eq. 'C') then
	         name = 'RIGHT'
	      else if (s(3:3) .eq. 'D') then
	         name = 'LEFT'
	      else if (s(3:4) .eq. '1~') then			! small keypad
	         name = 'FIND'
	      else if (s(3:4) .eq. '2~') then
	         name = 'INSERT_HERE'
	      else if (s(3:4) .eq. '3~') then
	         name = 'REMOVE'
	      else if (s(3:4) .eq. '4~') then
	         name = 'SELECT'
	      else if (s(3:4) .eq. '5~') then
	         name = 'PREV'
	      else if (s(3:4) .eq. '6~') then
	         name = 'NEXT'
	      else if (s(3:5) .eq. '17~') then			! function keys
	         name = 'F6'			!?
	      else if (s(3:5) .eq. '18~') then
	         name = 'F7'
	      else if (s(3:5) .eq. '19~') then
	         name = 'F8'
	      else if (s(3:5) .eq. '20~') then !?
	         name = 'F9'
	      else if (s(3:5) .eq. '21~') then
	         name = 'F10'
	      else if (s(3:5) .eq. '23~') then
	         name = 'F11'
	      else if (s(3:5) .eq. '24~') then
	         name = 'F12'
	      else if (s(3:5) .eq. '25~') then
	         name = 'F13'
	      else if (s(3:5) .eq. '26~') then
	         name = 'F14'
	      else if (s(3:5) .eq. '28~') then			! help
	         name = 'HELP'
	      else if (s(3:5) .eq. '29~') then			! do
	         name = 'DO'
	      else if (s(3:5) .eq. '31~') then
	         name = 'F17'
	      else if (s(3:5) .eq. '32~') then
	         name = 'F18'
	      else if (s(3:5) .eq. '33~') then
	         name = 'F19'
	      else if (s(3:5) .eq. '34~') then
	         name = 'F20'
	      else
	         name = 'UNKNOWN'
	      end if
	   else if (s(2:2) .eq. 'O') then			! PF keys
	      if (s(3:3) .eq. 'P') then
	         name = 'PF1'
	      else if (s(3:3) .eq. 'Q') then
	         name = 'PF2'
	      else if (s(3:3) .eq. 'R') then
	         name = 'PF3'
	      else if (s(3:3) .eq. 'S') then
	         name = 'PF4'
	      else if (s(3:3) .eq. 'p') then			! application KP
	         name = 'KP0'
	      else if (s(3:3) .eq. 'q') then
	         name = 'KP1'
	      else if (s(3:3) .eq. 'r') then
	         name = 'KP2'
	      else if (s(3:3) .eq. 's') then
	         name = 'KP3'
	      else if (s(3:3) .eq. 't') then
	         name = 'KP4'
	      else if (s(3:3) .eq. 'u') then
	         name = 'KP5'
	      else if (s(3:3) .eq. 'v') then
	         name = 'KP6'
	      else if (s(3:3) .eq. 'w') then
	         name = 'KP7'
	      else if (s(3:3) .eq. 'x') then
	         name = 'KP8'
	      else if (s(3:3) .eq. 'y') then
	         name = 'KP9'
	      else if (s(3:3) .eq. 'm') then
	         name = 'KP-'
	      else if (s(3:3) .eq. 'n') then
	         name = 'KP,'
	      else if (s(3:3) .eq. 'o') then
	         name = 'KP.'
	      else if (s(3:3) .eq. 'M') then
	         name = 'KPENTER'
	      else if (s(3:3) .eq. 'A') then
	         name = 'UP'
	      else if (s(3:3) .eq. 'B') then
	         name = 'DOWN'
	      else if (s(3:3) .eq. 'C') then
	         name = 'RIGHT'
	      else if (s(3:3) .eq. 'D') then
	         name = 'LEFT'
	      else
	         name = 'UNKNOWN'
	      end if
	   else
	      name = 'UNKNOWN'
	   end if
	else if (s(1:1) .gt. ' ' .and. s(1:1) .le. '~') then	! alpha/numerics
	   name = s(1:1)
	else if (s(1:1) .eq. ' ') then				! space bar
	   name = 'SPACE'
	else if (s(1:1) .eq. del) then				! delete
	   name = 'DEL'
	else if (s(1:1) .lt. ' ') then				! control chars
	   name = 'CNTL/'//char(ichar(s(1:1))+64)
	else
	   name = 'UNKNOWN'
	end if
	
	get_key_name = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Uses 7-bit control codes, returns -2 on error, -1 on success.  This routine
! tries to snatch up the common escape sequences sent by the vt200 series 
! keypads and function keys.
!
! [Non-AST version of get_key_by_ast.]
!------------------------------------------------------------------------------
	integer*4	function	get_key(s)
	implicit	none
	character*(*)	s
	character*1	c
	character*16	e
	integer*4	ok, kgetc
	parameter	esc=char(27)
	parameter	left_bracket='['
	parameter	csi_8=char(155)
	parameter	ss3_8=char(143)

	s = ' '
	e = ' '
	get_key = -2
	ok = kgetc(c)
	if (c .ne. esc) then
	   ! convert common eight-bit sequences
	   if (e(1:1) .eq. CSI_8) then
	      e(1:1) = esc
	      c = '['
	   else if (e(1:1) .eq. SS3_8) then
	      e(1:1) = esc
	      c = 'O'
	   else
	      s = c
	      get_key = -1
	      return
	   end if
	else
	   e(1:1) = c
	   ok = kgetc(c)
	end if

	e(2:2) = c
	if (c.eq. left_bracket) then
	   ok = kgetc(c)
	   e(3:3) = c
	   if (c .ge. 'A' .and. c .le. 'D') goto 100	! arrow key
	   if (c .ge. '1' .and. c .le. '9') then
	      ok = kgetc(c)
	      e(4:4) = c
	      if (c .eq. '~') goto 100			! small keypad key
	      if (c .ge. '1' .and. c .le. '9') then
	         ok = kgetc(c)
	         e(5:5) = c
	         if (c .eq. '~') goto 100		! function key
	         return
	      end if
	      return
	   end if
	   return
	else if (c .eq. 'O') then
	   ok = kgetc(c)
	   e(3:3) = c
	   if (c .ge. 'P' .and. c .le. 'S') goto 100	! PF key
	   if (c .ge. 'A' .and. c .le. 'D') goto 100	! Appl cursor keys
	   if (c .eq. 'M') goto 100			! Appl KP Enter
	   if (c .ge. 'm' .and. c .le. 'y') goto 100	! Appl KP{0123456789,.-}
	   return
	else
	   return
	end if

 100	s = e
	get_key = -1
	return
	end
