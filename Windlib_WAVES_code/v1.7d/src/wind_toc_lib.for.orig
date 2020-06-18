!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!--  TOC (Table Of Contents) routines supporting .WND files    ----------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	wind_open_toc(ch)
! Attempts to open the table-of-contents (TOC) file associated with the ".wnd"
! file opened on this channel.
! 
	implicit	none
	integer*4	ch
	include		'wind_record_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ok
	integer*4	lib$get_lun
	integer*4	lib$free_lun
	integer*4	ios
	integer*4	i
	integer*4	wind_close_toc			! an entry point
	integer*4	wind_prev_toc			! an entry point
	integer*4	wind_next_toc			! an entry point
	integer*4	wind_toc_use_stats		! an entry point
	character*128	f
	integer*4	found

	wind_open_toc = 0

	user(ch).toc.exists = .false.
	inquire(user(ch).lun,name=f,iostat=ios)
	if (ios .ne. 0) goto 10

	i = index(f,'.wnd')
	if (i.eq.0) i = index(f,'.WND')
	if (i.eq.0) return

	f = f(:i)//'WND_TOC'
	inquire(file=f,exist=user(ch).toc.exists,iostat=ios)
	if (ios .ne. 0) goto 20
	if (.not. user(ch).toc.exists) return

	ok = lib$get_lun(user(ch).toc.lun)
	if (ok .ne. 1) goto 30

	open(user(ch).toc.lun, file=f,
	1	shared, access='direct',
	1	status='old', readonly, iostat=ios, err=40)
	if (ios .ne. 0) goto 40

	wind_open_toc = 1
	return
  1	format(1x,'WIND_OPEN_TOC: ',a, a, i3)
  4	format(1x,t4,'File=',a,', iostat=',i3)

 10	wind_open_toc = 0
	type 1, 'Inquire error getting .WND file name, ', 'iostat=', ios
	return
 20	wind_open_toc = 0
	type 1, 'Inquire error getting .WND_TOC exists, ', 'iostat=', ios
	return
 30	wind_open_toc = w_ungettable_lun
	if (user(ch).suppress_messages) return
	type 1, 'Can''t get lun.'
	return
 40	ok = lib$free_lun(user(ch).toc.lun)
	user(ch).toc.lun    = 0
	user(ch).toc.exists = 0
	wind_open_toc = w_cannot_open_file
	if (user(ch).suppress_messages) return
	type 1, 'Can''t open TOC file-- '
	type 4, f(:56), ios
	return

	!----------------------------------------------------------------------
	entry wind_next_toc(ch)
	user(ch).toc.recnum = user(ch).toc.recnum + 1
	read(user(ch).toc.lun,rec=user(ch).toc.recnum,err=50,iostat=ios)
	1	 toc_rec
	user(ch).toc.packet_id = toc_rec.id
	user(ch).toc.is_first  = toc_rec.is_first
	user(ch).toc.is_last   = toc_rec.is_last
	user(ch).toc.pointer   = toc_rec.ptr
	user(ch).toc.major     = toc_rec.major
	user(ch).toc.minor     = zext(toc_rec.minor)
	wind_next_toc = 1
	return
  2	format(1x,'WIND_NEXT_TOC: ', a, i3, ', rec#',i)
 50	wind_next_toc = 0
	if (ios .eq. 36) goto 52
	if (user(ch).suppress_messages) return
	type 2, 'error reading TOC record, iostat=', ios, user(ch).toc.recnum
	return
 52	wind_next_toc = w_end_of_file
	user(ch).toc.recnum = user(ch).toc.recnum - 1
	return

	!----------------------------------------------------------------------
	entry wind_prev_toc(ch)
	user(ch).toc.recnum = user(ch).toc.recnum - 1
	if (user(ch).toc.recnum .le. 0) then
	   user(ch).toc.recnum = 1
	   wind_prev_toc = w_beginning_of_file
	   return
	end if
	read(user(ch).toc.lun,rec=user(ch).toc.recnum,err=60,iostat=ios) toc_rec
	user(ch).toc.packet_id = toc_rec.id
	user(ch).toc.is_first  = toc_rec.is_first
	user(ch).toc.is_last   = toc_rec.is_last
	user(ch).toc.pointer   = toc_rec.ptr
	user(ch).toc.major     = toc_rec.major
	user(ch).toc.minor     = zext(toc_rec.minor)
	wind_prev_toc = 1
	return
  3	format(1x,'WIND_PREV_TOC: ', a, i3, ', rec#',i)
 60	wind_prev_toc = 0
	if (user(ch).suppress_messages) return
	type 3, 'error reading TOC record, iostat=', ios, user(ch).toc.recnum
	return

	!----------------------------------------------------------------------
	entry wind_close_toc(ch)
	wind_close_toc = 1
	if (.not. user(ch).toc.exists) return
	if (user(ch).toc.lun .ne. 0) then
	   close(user(ch).toc.lun)
	   ok = lib$free_lun(user(ch).toc.lun)
	end if
	user(ch).toc.lun       = 0
	user(ch).toc.exists    = 0
	user(ch).toc.recnum    = 0
	user(ch).toc.packet_id = 0
	user(ch).toc.pointer   = 0
	user(ch).toc.major     = -1
	user(ch).toc.minor     = -1
	return

	!----------------------------------------------------------------------
	entry wind_toc_use_stats(ch)
	type *, 'TOC ncalls_____: ', user(ch).toc.ncalls
	type *, 'TOC coarse hits: ', user(ch).toc.coarse_hits
	type *, 'TOC fine hits__: ', user(ch).toc.fine_hits
	type *, 'TOC nfine_reads: ', user(ch).toc.nfine_reads
	user(ch).toc.ncalls      = 0
	user(ch).toc.coarse_hits = 0
	user(ch).toc.fine_hits   = 0
	user(ch).toc.nfine_reads = 0
	return

	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_packet_by_toc
	1			(ch,major,minor,type,direction,seek_1st)
	implicit	none
	include		'wind_record_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_routine_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	type				! 0..15, packet id
	integer*4	direction
	integer*4	seek_1st
	integer*4	i,j,k,n,m
	integer*4	by_type
	integer*4	diff
	integer*4	n_skip
	integer*4	found
	integer*4	ok
	integer*4	wind_next_toc
	integer*4	wind_prev_toc
	integer*4	get_packet_by_wnd
	integer*4	wind_get_record_by_recno

	get_packet_by_toc = 0

	user(ch).toc.ncalls = user(ch).toc.ncalls + 1

	if (mod(minor,10) .ne. 0) minor = minor - mod(minor,10)
	by_type = type .ge. min_packet_id .and. type .le. max_packet_id

	! compare caller's stream position with current toc stream position
	ok = wind_tm_delta_mfmf(major,minor,
	1	user(ch).toc.major,user(ch).toc.minor,
	1	diff)
	if (.not. ok) goto 10

	! adjust the toc position to match the caller's stream position (by
	! calculating the number of toc records to skip over in order to match)
	if (diff .ne. 0) then
	   ! make the "coarse" adjustment, which should be exact except
	   ! if the data has dropouts
	   n_skip = diff/10
	   ! n_skip should be = -1 for normal forward seeking event gathering
	   ! (ie., using wind_tm_get_next_event)
	   if (diff .lt. 0) then
	      ! caller's position is later
	      user(ch).toc.recnum = user(ch).toc.recnum - n_skip - 1
	      ok = wind_next_toc(ch)
	      if (.not. ok) goto 20
	   else
	      ! caller's position is earlier
	      user(ch).toc.recnum = user(ch).toc.recnum - n_skip + 1
	      ok = wind_prev_toc(ch)
	      if (.not. ok) goto 22
	   end if
	   ok = wind_tm_delta_mfmf(major,minor,
	1	user(ch).toc.major,user(ch).toc.minor,
	1	diff)
	   if (.not. ok) goto 30
	   ! make the "fine" adjustment
	   if (diff .eq. 0) then
	      ! toc and caller's stream positions match
	      user(ch).toc.coarse_hits = user(ch).toc.coarse_hits + 1
	   else if (diff .gt. 0) then
	      ! caller's position is earlier than toc position
	      user(ch).toc.fine_hits = user(ch).toc.fine_hits + 1
	      do while(diff .gt. 0)
	         ok = wind_prev_toc(ch)
	         user(ch).toc.nfine_reads = user(ch).toc.nfine_reads + 1
	         if (.not. ok) goto 40
	         ok = wind_tm_delta_mfmf(major,minor,
	1		user(ch).toc.major,user(ch).toc.minor,
	1		diff)
	      end do
	   else if (diff .lt. 0) then
	      ! caller's position is later than toc position
	      user(ch).toc.fine_hits = user(ch).toc.fine_hits + 1
	      do while(diff .lt. 0)
	         ok = wind_next_toc(ch)
	         user(ch).toc.nfine_reads = user(ch).toc.nfine_reads + 1
	         if (.not. ok) goto 40
	         ok = wind_tm_delta_mfmf(major,minor,
	1		user(ch).toc.major,user(ch).toc.minor,
	1		diff)
	      end do
	   end if
	end if

	found = 0
	do while(.not. found)
	   if (by_type) then
	      found = type .eq. user(ch).toc.packet_id
	      if (seek_1st.and.found) found = user(ch).toc.is_first
	   else
	      found = 1
	   end if
	   if (.not. found) then
	      if (direction .eq. forward) then
	         ok = wind_next_toc(ch)
	         if (.not. ok) goto 50
	      else
	         ok = wind_prev_toc(ch)
	         if (.not. ok) goto 60
	      end if
	   end if
	   major = user(ch).toc.major
	   minor = user(ch).toc.minor
	end do

	ok = wind_get_record_by_recno(ch,user(ch).toc.pointer)
	if (.not. ok) goto 70
	ok = get_packet_by_wnd(ch,major,minor,type,direction,seek_1st)
	if (.not. ok) goto 80

	get_packet_by_toc = 1

	return
  1	format(1x,'GET_PACKET_BY_TOC: ', a)
  2	format(1x,t8,a,i9,:,'.',i3.3)
 10	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'bad stream position (10):'
	type 2, 'caller''s -', major, minor
	type 2, 'toc''s    -', user(ch).toc.major, user(ch).toc.minor
	return
 20	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get next TOC entry "coarse adjustment"'
	return
 22	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get previous TOC entry "coarse adjustment"'
	return
 30	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'bad stream position (30):'
	type 2, 'caller''s -', major, minor
	type 2, 'toc''s    -', user(ch).toc.major, user(ch).toc.minor
	return
 40	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get next TOC entry "fine adjustment"'
	return
 50	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get next TOC entry "seeking by type"'
	return
 60	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get prev TOC entry "seeking by type"'
	return
 70	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get .WND record by TOC direct access pointer.'
	type 2, ' stream position - ', major, minor
	type 2, ' pointer - ', user(ch).toc.pointer
	return
 80	get_packet_by_toc = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get next TOC entry "seeking by type"'
	type 2, ' stream position - ', major, minor
	return
	end
