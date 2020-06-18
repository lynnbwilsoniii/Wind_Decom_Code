! scroll.for - scroll manager

!------------------------------------------------------------------------------
! Displays the contents of a scrolling [box,window,region] based on values
! of the members of data structure s.
!------------------------------------------------------------------------------
	integer*4	function	scr_mgr_show_one_scroll(s)
	implicit	none
	include		'scr_manager_def.for/nolist'
	include		'termvideo_def.for/nolist'
	record /tv_scroll/ s
	integer*4	i,j,m
	integer*4	nrows
	integer*4	ios

	scr_mgr_show_one_scroll = 0

!	if (s.xr .eq. 0) then
	if (s.idx .eq. 0) then
	   ! no current row, no data displayed, so just return
!	   call my_err('s.xr is zero')
!	   call my_err('s.idx is zero')
	   return
	end if

	nrows = s.r2 - s.r1 + 1
	j = s.r1
	m = s.idw
	do i=1,nrows
	   ! show current row in bold
	   if (m.eq.s.idx) m = -m
	   call scr_mgr_call( %val(s.gtscrldt), m)
	   write(s.pos,6,iostat=ios,err=20) csi, j
	   call putstr(s.pstr(:s.l_str+9))
	   m = m + 1
	   j = j + 1
	end do

	scr_mgr_show_one_scroll = 1
	return
  6	format(a2,i2.2,';001f')
 20	call my_err('SHOW_ONE_SCROLL: error writing position string')
	type *, s.xr, s.idw, s.idx, s.idy, s.idz
	type *, i,j,m
	return
	end

!------------------------------------------------------------------------------
! Manages the user's interaction with the scrolling region until an unknown
! key is pressed, well, until something other than the NEXT, PREV, or UP and
! DOWN arrow keys is pressed.
!------------------------------------------------------------------------------
	integer*4	function	scroll_this(s,xi,err)
	implicit	none
	include		'scr_manager_def.for/nolist'
	include		'termvideo_def.for/nolist'
	record /tv_scroll/ s		! scroll region definitions
	integer*4	xi		! object number in screen
	integer*4	err		! function to call on error
	external	err
	integer*4	i,j,k,m,n
	integer*4	ios
	integer*4	ok
	integer*4	k2len
	integer*4	putstr
	character*8	margins
	integer*4	nrows
	character*9	pos
	integer*4	scrolling_keystrokes
	character*8	s3
	integer*4	get_key
	integer*4	scr_mgr_call
	integer*4	scr_mgr_call2
	character*16	keyname
	integer*4	top_behavior
	integer*4	bottom_behavior

	scroll_this = 0
	bottom_behavior = s.flags .and. '0007'x
	top_behavior    = s.flags .and. '0070'x

	write(margins,1,iostat=ios,err=10) csi, s.r1, s.r2
	call putstr(margins)
	call putstr(cursor_off)
	nrows = s.r2 - s.r1 + 1

	if (s.xr .eq. 0) then
	   ! no current row, so initialize
	   j = s.r1
	   s.xr = s.r1
	   s.idw = 1
	   s.idx = 1
	   s.idy = min(nrows,s.idz)
	   do i=1,nrows
	      call scr_mgr_call( %val(s.gtscrldt), i)
	      write(s.pos,6,iostat=ios,err=20) csi, s.r1+i-1
	      call putstr(s.pstr(:s.l_str+9))
	   end do
	   write(pos,8,iostat=ios,err=20) csi, s.xr
	   call putstr(pos)
	else
	   call scr_mgr_call( %val(s.gtscrldt), s.idx)
	   write(s.pos,6,iostat=ios,err=20) csi, s.xr
	   call putstr(s.pstr(:s.l_str+9))
	end if

	! loop while scrolling
	scrolling_keystrokes = 1
	do while(scrolling_keystrokes)
	   ok = get_key(s3)
	   call get_key_name(s3,keyname)
	   k = k2len(keyname)
	   if (s.idx .eq. 1 .and. (keyname.eq.'UP' .or. keyname.eq.'PREV')) then
	      if (top_behavior .eq. top_is_sticky) then
	         ! do nothing
	      else if (top_behavior .eq. top_moves_to_prev_field) then
	         goto 9000
	      else if (top_behavior .eq. top_rolls_to_bottom) then
	         ! not implemented, do nothing for now
	      else
	         ! do nothing as default behavior
	      end if
	   else if (s.idx .eq. s.idz .and.
	1          (keyname .eq. 'DOWN' .or. keyname .eq. 'NEXT')) then
	      if (bottom_behavior .eq. bottom_is_sticky) then
	         ! do nothing
	      else if (bottom_behavior .eq. bottom_moves_to_next_field) then
	         goto 9000
	      else if (bottom_behavior .eq. bottom_rolls_to_top) then
	         ! not implemented, do nothing for now
	      else
	         ! do nothing as default behavior
	      end if
	   else if (keyname .eq. 'UP') then
	      j = s.xr
	      k = s.idx
	      if (s.xr .ne. s.r1) then
	         ! not at the top of the scrolling region
	         s.xr = s.xr - 1
	         s.idx = s.idx - 1
	         ! show the previous current line in normal
	         call scr_mgr_call( %val(s.gtscrldt), k)
	         write(s.pos,6,iostat=ios,err=20) csi, j
	         call putstr(s.pstr(:s.l_str+9))
	         ! show the new current line in reverse
	         call scr_mgr_call( %val(s.gtscrldt), s.idx)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pstr(:s.l_str+9))
!	      else if (s.idx .eq. 1) then
!	         ! at top of scroll region and top of scrollable data
!	         call putstr(beep)
	      else
	         ! scroll the region down one line from the top
	         if ((s.idy-s.idw+1) .ge. nrows) s.idy = s.idy - 1
	         s.idx = s.idx - 1
	         s.idw = s.idw - 1
	         ! show the previous current line in normal
	         call scr_mgr_call( %val(s.gtscrldt), k)
	         write(s.pos,6,iostat=ios,err=20) csi, j
	         call putstr(s.pstr(:s.l_str+9))
	         ! show the new current line in reverse
	         call scr_mgr_call( %val(s.gtscrldt), s.idx)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pos//ins1line)
	         call putstr(s.pstr(:s.l_str+9))
	      end if
	   else if (keyname .eq. 'DOWN') then
	      j = s.xr
	      k = s.idx
	      if (s.xr .ne. s.r2) then
	         ! not at the bottom of the scrolling region
	         s.xr = s.xr + 1
	         s.idx = s.idx + 1
	         ! show the previous current line in normal
	         call scr_mgr_call( %val(s.gtscrldt), k)
	         write(s.pos,6,iostat=ios,err=20) csi, j
	         call putstr(s.pstr(:s.l_str+9))
	         ! show the new current line in reverse
	         call scr_mgr_call( %val(s.gtscrldt), s.idx)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pstr(:s.l_str+9))
!	      else if (s.idx .ge. s.idz) then
!	         ! at bottom of scroll region and bottom of scrollable data
!	         call putstr(beep)
	      else
	         ! scroll the region up one line from the bottom
	         s.idx = s.idx + 1
	         s.idw = s.idw + 1
	         s.idy = s.idy + 1
	         ! show the previous current line in normal
	         call scr_mgr_call( %val(s.gtscrldt), k)
	         write(s.pos,6,iostat=ios,err=20) csi, j
	         call putstr(s.pstr(:s.l_str+9))
	         ! delete the top line
	         write(pos,6,iostat=ios,err=20) csi, s.r1
	         call putstr(pos//del1line)
	         ! show the new current line in reverse
	         call scr_mgr_call( %val(s.gtscrldt), s.idx)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pos//ins1line)
	         call putstr(s.pstr(:s.l_str+9))
	      end if
	   else if (keyname .eq. 'NEXT') then
	      if (s.idy .lt. s.idz) then
	         m = s.idx - s.idw
	         n = s.idx + nrows
	         s.idw = s.idy + 1
	         s.idy = min(s.idy+nrows,s.idz)
	         s.idx = min(s.idy,s.idw+m)
	         s.xr  = s.r1 + (s.idx-s.idw)
	         j = s.idw 
	         do i=1,nrows
	            call scr_mgr_call( %val(s.gtscrldt), j)
	            write(s.pos,6,iostat=ios,err=20) csi, s.r1+i-1
	            call putstr(s.pstr(:s.l_str+9))
	            j = j + 1
	         end do
	         write(pos,7,iostat=ios,err=20) csi, s.xr
	         call putstr(pos)
	      else if (s.idy .eq. s.idz .and. s.idx .lt. s.idz) then
	         ! move current position to bottom of last page
	         j = s.idx
	         s.idx = s.idz
	         ! show the old current line in normal
	         call scr_mgr_call( %val(s.gtscrldt), j)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pstr(:s.l_str+9))
	         ! show the new current line in reverse
	         s.xr = s.xr + (s.idz-j)
	         call scr_mgr_call( %val(s.gtscrldt), s.idx)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pstr(:s.l_str+9))
	      else
	         call putstr(beep)
	      end if
	   else if (keyname .eq. 'PREV') then
	      if (s.idy .gt. nrows) then
	         ! at least one full page
	         m = s.xr-s.r1
	         s.idy = s.idw -1
	         s.idw = max(s.idw - nrows,1)
	         s.idx = s.idw + m
	         j = s.idw 
	         do i=1,nrows
	            call scr_mgr_call( %val(s.gtscrldt), j)
	            write(s.pos,6,iostat=ios,err=20) csi, s.r1+i-1
	            call putstr(s.pstr(:s.l_str+9))
	            j = j + 1
	         end do
	         write(pos,7,iostat=ios,err=20) csi, s.xr
	         call putstr(pos)
	      else if (s.idx .gt. 1 .and. s.idx .le. nrows) then
	         ! move current position to top of first page
	         j = s.idx
	         s.idx = 1
	         ! show the old current line in normal
	         call scr_mgr_call( %val(s.gtscrldt), j)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pstr(:s.l_str+9))
	         ! show the new current line in reverse
	         s.xr = s.r1
	         call scr_mgr_call( %val(s.gtscrldt), s.idx)
	         write(s.pos,6,iostat=ios,err=20) csi, s.xr
	         call putstr(s.pstr(:s.l_str+9))
	      else
	         call putstr(beep)
	      end if
	   else if (s.on_key .ne. 0) then
	      ok = scr_mgr_call2( %val(s.on_key), xi, keyname)
	      if (.not. ok) then
	         goto 9000
	      end if
	   else
	      goto 30
	   end if
	end do

 9000	continue
	s.retkey = keyname

	s.idx = -s.idx
	call scr_mgr_call( %val(s.gtscrldt), s.idx)
	write(s.pos,6,iostat=ios,err=20) csi, s.xr
	call putstr(s.pstr(:s.l_str+9))

	call putstr(cursor_on)
	scroll_this = 1

	return
  1	format(a2,i2.2,';',i2.2,'r')
  2	format(i2.2)
  3	format(i3.3)
  6	format(a2,i2.2,';001f')
  7	format(a2,i2.2,';003f')
  8	format(a2,i2.2,';080f')
 10	call err('SCROLL_THIS: illegal start/end row values.')
	call putstr(cursor_on)
	return
 20	call err('SCROLL_THIS: error writing position string')
	type *, s.xr, s.idw, s.idx, s.idy, s.idz
	type *, i,j,k,m,n
	call putstr(cursor_on)
	return
 30	call err('SCROLL_THIS: no on_key routine to handle '//keyname(:k))
	call putstr(cursor_on)
	return
	end
