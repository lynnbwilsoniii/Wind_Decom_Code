! sayget.for - source for a one-line editor suitable for full screen prompting
!
! JK
	options/extend_source
!------------------------------------------------------------------------------
! Prompts user for input at specified screen location using specified video
! attributes.  Input field may contain default data which can be edited
! using regular DCL command line conventions.
!------------------------------------------------------------------------------
	integer*4	function	edit_at(f)
	implicit	none
	include		'termvideo_def.for/nolist'
	include		'scr_manager_def.for/nolist'
	record /tv_field/ f
	integer*4	i,j,k,n
	integer*4	ok
	integer*4	k2len
!	integer*4	getc
	integer*4	putstr
	integer*4	get_key
	integer*4	ios
	integer*4	vj,sj
	character*12	xdva
	character*9	pos
	character*4	s2
	character*8	s3
	character*256	s8
	integer*4	insert_mode
	integer*4	an_edit_keystroke
	character*16	keyname
	integer*4	zcol

	edit_at = 0

	! check for argument errors
	if (f.col .lt. 1 .or. f.col .gt. 132) goto 10
	if (f.row .lt. 1 .or. f.row .gt. 99 ) goto 20
	if (f.siz .lt. 1 .or. f.siz .gt. 132) goto 30
	if ((f.col+f.siz) .gt. 132) goto 40

	! initialize arg's and var's
	f.chgflg = 0
	insert_mode = 1
	f.retkey = ' '
	sj = f.siz
	s8 = f.str
	zcol = f.col + f.siz - 1
	i = 1
	k = k2len(s8)
	j = max(k,1)
	an_edit_keystroke = 1

	! build the video attributes used to display the field during edit
	! eg, reverse during edit, bright otherwise
	call build_video_sequence(f.iva,xdva,vj)

	! build the screen position escape sequence
	write(s2,'(i3.3)',iostat=ios,err=10) f.col
	write(s3,'(i2.2)',iostat=ios,err=20) f.row
	pos=csi//s3(1:2)//';'//s2(1:3)//'f'

	ok = putstr(pos//xdva(:vj)//s8(:sj)//pos)

	do while(an_edit_keystroke)
	   ok = get_key(s3)
	   call get_key_name(s3,keyname)
	   k = k2len(keyname)
	   if (k .eq. 1 .or. keyname.eq.'SPACE') then
	      ! a character key
	      f.chgflg = 1
	      if (insert_mode) then
	         s8(i+1:j+1) = s8(i:j)
	         s8(i:i) = s3(1:1)
	         if (i .le. sj) then
	            k = min(j+1,sj)
	            ok = putstr(pos//s8(i:k)//pos//s8(i:i))
	         end if
	         i = i + 1
	         j = j + 1
	         k = min(f.col+i-1,zcol)
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) k
	      else
	         s8(i:i) = s3(1:1)
	         if (i .lt. sj) then
	            ok = putstr(pos//s8(i:i))
	         end if
	         i = i + 1
	         k = min(f.col+i-1,zcol)
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) k
	      end if
	   else if (keyname .eq. 'DEL') then
	      if (i.ne.1) then
	         f.chgflg = 1
	         s8(i-1:j-1) = s8(i:j)
	         s8(j:j) = ' '
	         i = i - 1
	         j = j - 1
	         k = min(f.col+i-1,zcol)
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) k
	         ok = putstr(pos)
	         n = min(j+1,f.siz)
	         if (i .le. sj) then
	            ok = putstr(pos//s8(i:n)//pos)
	         else
	            ! deleting nonviewable chars to right of visible field
	         end if
	      end if
	   else if (keyname .eq. 'CNTL/A') then
	      insert_mode = .not. insert_mode
	   else if (keyname .eq. 'CNTL/H') then
	      if (i.ne.1) then
	         i = 1
	         k = min(f.col+i-1,zcol)
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) k
	         ok = putstr(pos)
	      end if
	   else if (keyname .eq. 'CNTL/E') then
	      j = k2len(s8)
	      i = j + 1
	      if (j.eq.0) j = 1
	      k = min(f.col+i-1,zcol)
	      write(pos(6:8),'(i3.3)',iostat=ios,err=60) k
	      ok = putstr(pos)
	   else if (keyname .eq. 'CNTL/J') then
	   else if (keyname .eq. 'CNTL/U') then
	      if (i.gt.1) then
	         f.chgflg = 1
	         if (i.gt.j) then
	            ! at end of line
	            s8 = ' '
	            i = 1
	            j = 1
	         else
	            s8 = s8(i:j)
	            j = j - i + 1
	            i = 1
	         end if
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) f.col+i-1
	         ok = putstr(pos//s8(:sj)//pos)
	      end if
	   else if (keyname .eq. 'CNTL/D' .or. keyname .eq. 'LEFT') then
	      if (i.gt.1) then
	         i = i - 1
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) f.col+i-1
	         ok = putstr(pos)
	      end if
	   else if (keyname .eq. 'CNTL/F' .or. keyname .eq. 'RIGHT') then
	      if (i.le.j) then
	         i = i + 1
	         write(pos(6:8),'(i3.3)',iostat=ios,err=60) f.col+i-1
	         ok = putstr(pos)
	      end if
	   else
	      an_edit_keystroke = 0
	      f.retkey = keyname
	   end if
	end do

	! redisplay the field with the "final" video attributes
	! eg, reverse during edit, bright otherwise
	ok = putstr(primary)
	call build_video_sequence(f.fva,xdva,vj)
	write(pos(6:8),'(i3.3)',iostat=ios,err=60) f.col
	ok = putstr(pos//xdva(:vj)//s8(:sj)//pos)

	! update the caller's field value
	f.str = s8(:j)
	f.l_str = j

	! set the terminal video attributes to plain
	ok = putstr(primary)

	edit_at = 1
	return
 10	type *, 'Invalid column position: ', f.col
	return
 20	type *, 'Invalid row position: ', f.row
	return
 30	type *, 'Invalid display size of field: ', f.siz
	return
 40	type *, 'Invalid column position for display size: ', f.col, f.siz
	return
 60	type *, 'Error creating screen column address: ', f.col, f.siz, i, j
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
! Builds a video escape sequence based on the following key letters:
!
!	u - underline
!	n - normal
!	r - reverse
!	b - bright
!------------------------------------------------------------------------------
	integer*4	function	build_video_sequence(code,seq,j)
	implicit	none
	include		'termvideo_def.for/nolist'
	character*(*)	code		! any combination of {u,n,r,b}
	character*(*)	seq		! resulting video sequence
	integer*4	j		! length of resulting video sequence
	integer*4	i
	character*8	s2
	seq = ' '
	i   = 0
	j   = 0
	s2  = code
	call to_lower(s2,0,0)
	if (index(s2,'b') .ne. 0) then	! bright
	   i = i + 1
	   j = i + 3
	   seq(i:j) = bright
	end if
	if (index(s2,'r') .ne. 0) then	! reverse
	   i = i + 1
	   j = i + 3
	   seq(i:j) = reverse
	end if
	if (index(s2,'u') .ne. 0) then	! underline
	   i = i + 1
	   j = i + 3
	   seq(i:j) = underline
	end if
	if (index(s2,'n') .ne. 0 .or. i.eq.0) then	! normal
	   i = i + 1
	   j = i + 3
	   seq(i:j) = primary
	end if
	build_video_sequence = 1
	return
	end
