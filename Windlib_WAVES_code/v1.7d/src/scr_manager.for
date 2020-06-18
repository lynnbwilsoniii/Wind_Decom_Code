! scr_manager.for - routines used to coordinate the various activities
! related to full screen data entry

	options/extend_source
!------------------------------------------------------------------------------
! Coordinates the various activities related to full screen data entry/display.
!------------------------------------------------------------------------------
	integer*4	function	manage_screen(g,o,err)
	implicit	none
	include		'scr_manager_def.for/nolist'
	record /scr_mgr_object/ g(*)	! list of screen objects
	record /tv_on_key/ o(*)		! list of "hot keys"
	integer*4	err		! tragic error routine
	external	err
	integer*4	i,j,n
	integer*4	ok
!	integer*4	edit_at		! a function
	integer*4	scr_mgr_call
	integer*4	scr_mgr_call4
	integer*4	key_not_used
	integer*4	prev_i

	manage_screen = 0

	! loop, event style, through the fields
	i = 1
	n = 0
	ok = 1
	do while(ok)
	   n = n + 1
 1000	   continue
	   prev_i = i
	   ! call the before routine
	   if (g(i).before .ne. 0) then
	      ok = scr_mgr_call4(
	1	g(i),
	1	%val(g(i).before),
	1	%val(g(i).obj),
	1	i,
	1	g(i).errhdlr,
	1	g(i).auxkey
	1	)
	      if (n .gt. 1) goto 30
	      if (.not. ok) goto 1000
	      if (prev_i .ne. i) goto 1000
	   end if
 2000	   continue
	   ! call the item processing routine
	   prev_i = i
	   ok = scr_mgr_call4(
	1	g(i),
	1	%val(g(i).proc),
	1	%val(g(i).obj),
	1	i,
	1	g(i).errhdlr,
	1	g(i).auxkey
	1	)
	   if (.not. ok) goto 20
	   if (prev_i .ne. i) goto 1000
	   ! call the after routine
	   if (g(i).after .ne. 0) then
	      ok = scr_mgr_call4(
	1	g(i),
	1	%val(g(i).after),
	1	%val(g(i).obj),
	1	i,
	1	g(i).errhdlr,
	1	g(i).auxkey
	1	)
	      if (.not. ok) goto 2000
	      if (prev_i .ne. i) goto 1000
	   end if
	   ! check the on_key action list
	   j = 1
	   key_not_used = 1
	   do while(j.gt.0)
	      if (g(i).retkey .eq. o(j).key) then
	         ! call the on key routine
	         ok = scr_mgr_call( %val(o(j).routine), i)
	         j = 0
	         key_not_used = 0
	      else
	         j = j + 1
	         if (o(j).routine .eq. 0) j = 0
	      end if
	   end do
	   if (key_not_used) then
	      call err('Key not used: '//g(i).retkey)
	   end if
 3000	   continue
	   n = 0
	end do

	call goto_bottom()
	manage_screen = 1
	return
  1	format(1x,'MANAGE_SCREEN: ', a, :, i3, :, a)
 20	call goto_bottom()
	type 1, 'Error editing field.'
	call scr_mgr_show_object(g,i)
	return
 30	call goto_bottom()
	type 1, '"Before" function infinite loop detected.'
	call scr_mgr_show_object(g,i)
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Displays the current values of all fields in the screen.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_show_gets(f)
	implicit	none
	include		'scr_manager_def.for/nolist'
	record /tv_field/ f(*)
	integer*4	i

	scr_mgr_show_gets = 0
	i = 1
	do while(f(i).siz .gt. 0)
	   call scr_mgr_show_one_get(f(i))
	   i = i + 1
	end do
	scr_mgr_show_gets = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Displays the current value of one field in the screen.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_show_one_get(f)
	implicit	none
	include		'termvideo_def.for/nolist'
	include		'scr_manager_def.for/nolist'
	record /tv_field/ f
	character*24	seq
	character*9	pos
	integer*4	i,j
	integer*4	ios
	integer*4	putstr
	integer*4	ok

	scr_mgr_show_one_get = 0

	call build_video_sequence(f.fva,seq,j)
	write(pos,2,iostat=ios,err=10) csi, f.row, f.col
	ok = putstr(pos//seq(:j)//f.str(:f.siz)//primary)

	scr_mgr_show_one_get = 1
	return
  1	format(1x,'SCR_MGR_SHOW_ONE_GET: ', a, :, i3, :, a)
  2	format(a2,i2.2,';',i3.3,'f')
 10	call goto_bottom()
	type 1, 'Error creating field screen position.'
	call scr_mgr_show_field(f,i)
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Displays the current view of all scrolling regions in the screen.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_show_scrolls(s)
	implicit	none
	include		'scr_manager_def.for/nolist'
	record /tv_scroll/ s(*)
	integer*4	i

	scr_mgr_show_scrolls = 0
	i = 1
	do while(s(i).gtscrldt .ne. 0)
	   call scr_mgr_show_one_scroll(s(i))
	   i = i + 1
	end do
	scr_mgr_show_scrolls = 1
	return
	end

!------------------------------------------------------------------------------
! Debugging routine to display a table of a screen object's attributes
! and fields.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_show_object(g,i)
	implicit	none
	include		'termvideo_def.for/nolist'
	include		'scr_manager_def.for/nolist'
	record /scr_mgr_object/ g(*)
	integer*4	i

	scr_mgr_show_object = 1

	type *, '	Object#: ', i
	type *, '	 RetKey: ', g(i).retkey
	type *, '	   Type: ', g(i).type
	type 8, '	 Before: ', g(i).before
	type 8, '	   Proc: ', g(i).proc
	type 8, '	  After: ', g(i).after
	type 8, '	   Show: ', g(i).show
	type 8, '	    Err: ', g(i).errhdlr
	type 8, '	 AuxKey: ', g(i).auxkey
	type 8, '	Objaddr: ', g(i).obj

	if (g(i).type .eq. edit_at_type) then
	   call scr_mgr_show_field(%val(g(i).obj))
	end if

 8	format(1x,a,z8.8)
	return
	end

!------------------------------------------------------------------------------
! Debugging, displays a table of attributes of a single field of the screen.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_show_field(f)
	implicit	none
	include		'termvideo_def.for/nolist'
	include		'scr_manager_def.for/nolist'
	record /tv_field/ f

	scr_mgr_show_field = 1

	type *, '	Showing Field:'
	type *, '	     col:', f.col
	type *, '	     row:', f.row
	type *, '	     siz:', f.siz
	type *, '	str(:60):', f.str(:60)
	type *, '	   l_str:', f.l_str
	type *, '	     iva:', f.iva
	type *, '	     fva:', f.fva
	type *, '	  chgflg:', f.chgflg
	type *, '	  retkey:', f.retkey
	type 8, '	  before:', f.before
	type 8, '	   after:', f.after

 8	format(1x,a,1x,z8.8)
	return
	end

!------------------------------------------------------------------------------
! Facilitator subprogram to call argument routine with one argument arg.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_call(routine,arg)
	implicit	none
	integer*4	routine
	external	routine
	integer*4	arg

	scr_mgr_call = routine(arg)
	return
	end

!------------------------------------------------------------------------------
! Facilitator subprogram to call a routine with two arguments.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_call2(routine,arg,str)
	implicit	none
	integer*4	routine
	external	routine
	integer*4	arg
	character*(*)	str

	scr_mgr_call2 = routine(arg,str)
	return
	end

!------------------------------------------------------------------------------
! Facilitator subprogram to call a routine with four arguments.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_call4(g,routine,a1,a2,a3,a4)
	implicit	none
	include		'scr_manager_def.for/nolist'
	record /scr_mgr_object/ g
	integer*4	routine
	external	routine
	integer*4	a1		! object structure
	integer*4	a2		! scr mgr object #
	integer*4	a3		! error handler routine
	integer*4	a4		! auxiliary key handling routine

!	type *, 'routine=', %loc(routine)
!	type *, 'a1=', a1
!	type *, 'a2=', a2
!	type *, 'a3=', a3
!	type *, 'a4=', a4

	scr_mgr_call4 = routine(a1,a2,%val(a3),%val(a4))
	call scr_mgr_get_retkey(g,a1)
	return
	end

!------------------------------------------------------------------------------
! Copies the return key value form one buffer to another.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_get_retkey(g1,g2)
	implicit	none
	include		'scr_manager_def.for/nolist'
	record /scr_mgr_object/ g1, g2
	g1.retkey = g2.retkey
	scr_mgr_get_retkey = 1
	return
	end
