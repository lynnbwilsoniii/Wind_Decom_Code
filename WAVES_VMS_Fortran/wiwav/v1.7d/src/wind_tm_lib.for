! wind_tm_lib.for -- library of routines for accessing WIND/WAVES telemetry
!
! (* SunOS Compatable Version *)
!
! Routines that are
! perceived as being generally used by the "public" begin 'wind_tm_' while
! routines internal to this library and probably not of interest to most
! users begin 'wind_' (without the 'tm_') or 'w_'.  Note that most routines
! not beginning with 'wind_tm' have not been provided with transfer vectors
! and are hence not accessable to the public in shareable image form.
!
! Each WIND_TM routine is documented in the file wind_doc:user.manual.
!
!
! Shareable Image Info:
!
! These routines compose a shareable image.  For more information on shareable
! images, refer to the VMS Linker Utility Manual (Programming 2B) and/or
! VAX Professional vol. 9, no. 3, June 1987.  Here are some important
! maintenance tips:
!	1.  Transfer vectors in the VAX MACRO transfer vector file,
!	    wind_tm_lib_transfer.mar, should remain in the same order.  This
!	    keeps the locations of the transfer vectors constant in the
!	    shareable image and saves on relinking.  The contents of the
!	    vectors are the addresses of the FORTRAN routines in
!	    wind_tm_lib.exe.  The addresses of the FORTRAN routines are not
!	    known to the application
!	    program until runtime, but the address of the transfer vectors
!	    are known at link time.  So, the transfer vectors should remain
!	    in the same order in wind_tm_lib_transfer.mar to save relinking,
!	    but, the FORTRAN routines in wind_tm_lib.for may be in any order.
!	1.  Deleting Routines:  Replace a routine with a dummy routine rather
!	    than removing it entirely.  When a routine is deleted, the listing
!	    recompiled, and transfer vectors updated, the new shareable image's
!	    transfer vectors for routines after the deleted routine are in
!	    different locations--meaning the old applications won't be able
!	    to find some or all of the routines correctly.  Replacing with
!	    dummy routines means applications won't have to be relinked
!	    (unless they reference the deleted routine, but then you have
!	    to rewrite/recompile/relink the application anyway).
!	    A dummy routine can be as short as:
!		subroutine no_longer_needed
!		return
!		end
!	2.  Adding a new routine will only necessitate relinking those
!	    applications directly referencing the added routine.  New routines
!	    are added in a two-step process: (1) replace a dummy routine at
!	    the end of the FORTRAN source, and (2) replace the dummy transfer
!	    vector at the end of wind_tm_lib_transfer.mar.
!
!
! To Build: Use the make.com and dependancies.make in the source directory.
!
! To Use with an Application Program:
!
! $ define wind_lib wind_dir:wind_shr_lib
! $ fortran my_application
! $ link my_application,wind_lib/lib		! link to the shareable library
! $ define wind_tm_lib wind_dir:wind_tm_lib
! $ run my_application				! run with the shareable image
!
! The logical names must be defined in order to link and run the application.
! At this writing the logical names are provided by the system manager
! automatically through a startup file.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! User's initialization/setup routines
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_open_channel(ch,query)
! Opens a channel to the real time telemetry or to offline disk storage.
! Argument 'query' may be a file specification or either of the keywords
! "realtime" or "offline".
	implicit	none
	integer*4	ch		! telemetry channel number
	character*(*)	query		! keyword or VMS file specification
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	wind_get_channel
	integer*4	ok
	integer*4	i, j, k, m
	character*16	xquery
	integer*4	w_get_sun_filename	! a function
	integer*4	w_select_filename	! a function
	integer*4	w_setup_stream		! a function
	integer*4	ios
	integer*4	k2len			! a function
	logical*4	offline_query
	integer*4	w_channel_open	! an entry point
	integer*4	w_channel_reopen	! an entry point
	integer*4	w_channel_select	! an entry point
	integer*4	w_resolve_filename	! a function
	integer*4	w_simple_time_chooser	! a function
	integer*4	w_ur8_to_x_filename 	! a function
	integer*4	w_fs3			! a function
	integer*4	w_fs4			! a function
	real*8		r8a, r8b
	real*8		t1, t2
	logical*4	return_times
	character*256	f
	integer*4	k3len

	!----------------------------------------------------------------------

	entry	w_channel_open(ch, query)

	wind_tm_open_channel = wind_get_channel(ch)
	if (wind_tm_open_channel .ne. 1) goto 5
	xquery = query
	goto 1000

	!----------------------------------------------------------------------
	entry	w_channel_reopen(ch, query)
	f = query
	! resolve any wild carding and verify file existance prior to
	! closing the current channel, otherwise the user is left in 
	! an indeterminite position in both stream and wind_lib code
	ok = w_resolve_filename(f)
	if (ok .ne. 1) goto 50
	call w_channel_close(ch)
	user(ch).n_file_opens = user(ch).n_file_opens + 1
	xquery = query

	goto 1000

	!----------------------------------------------------------------------
	entry	w_channel_select(ch, query, t1, t2)

	wind_tm_open_channel = wind_get_channel(ch)
	if (wind_tm_open_channel .ne. 1) goto 5
!	xquery = query
	xquery = 'time'
	goto 1000

 1000	continue
	offline_query = .false.
	return_times  = .false.
	r8a = 0.0
	r8b = 0.0
	call to_lower(xquery,0,0)

	! this is test is for older realtime gse programs being used
	! in offline systems
	if ((xquery .eq. 'realtime') .and. .not. realtime) then
	   xquery = 'offline'
	end if

!	type *, 'xquery=', xquery

	! open the channel
	if ((xquery .eq. 'offline') .and. vms) then
	   offline_query = .true.
	   ok = w_select_filename(user(ch).file)
	   if (ok .ne. 1) goto 10
	else if (xquery .eq. 'time') then
	   ! t1 = 0  = call chooser, sets r8a and r8b
	   ! t1 = -1 = first recorded data in wind_data:
	   ! t1 = -2 = start of last recorded day in wind_data:
	   !
	   ! t2 = 0  = end of day specified by t1
	   ! t2 = -1 = end of last recorded day in wind_data:, substitute
	   !           w_ur8_infinity value so when caller gets to eow the
	   !           maximum eow may be obtained
	   offline_query = .true.
	   return_times  = .true.
	   r8a = t1
	   r8b = t2
	   if (r8a .le. 0.0) then
	      ok = w_simple_time_chooser(r8a, r8b)
	      if (ok .ne. 1) goto 40
	   end if
	   ok = w_ur8_to_x_filename(r8a, cdhf_lz_stream, user(ch).file)
	   if (ok .ne. 1) goto 50
!	type *, 'r8a,r8b=', r8a, r8b
!	type *, 'file=',user(ch).file(1:66)
	else if ((xquery .eq. 'chooser3') .and. vms) then
	   offline_query = .true.
	   ok = w_fs3(user(ch).file, r8a, r8b)
	   if (ok .ne. 1) goto 10
	else if ((xquery .eq. 'chooser4') .and. vms) then
	   offline_query = .true.
	   ok = w_fs4(user(ch).file)
	   if (ok .ne. 1) goto 10
	else if ((xquery .eq. 'offline') .and. unixos) then
	   offline_query = .true.
	   ok = w_get_sun_filename(user(ch).file)
	   if (ok .ne. 1) goto 10
	else if (xquery(1:3) .eq. 'nrt' .or. xquery(1:4) .eq. '"nrt') then
	   ! NRT, near realtime stream
	   i = 1
	   if (xquery(1:1) .eq. '"') i = 2
	   user(ch).file = query(i:len(query))
	else
	   user(ch).file = query
	   ! prepend a path specification
	   i = index(user(ch).file,'wi_lz_wav_')
	   if (vms .and. (i.eq.0)) i = index(user(ch).file, 'WI_LZ_WAV_')
	   j = index(user(ch).file, ':')
	   if (vms) then
	      k = index(user(ch).file, '[')
	   else
	      k = index(user(ch).file, '/')
	   end if
	   if (j .eq. 0 .and. k .eq. 0) then
	      user(ch).file = 'WIND_DATA:'//user(ch).file
	   end if
	   ! append a '.dat' if absent from wind-cdhf-type files
	   k = k3len(user(ch).file)
	   j = k - 3
	   if ((j .gt. 1) .and. 
	1      (user(ch).file(j:k) .ne. '.dat') .and.
	1      (user(ch).file(j:k) .ne. '.DAT') ) then
	      j = k
	      m = 0
	      do while (j .gt. 0 .and. m .eq. 0)
	         if (user(ch).file(j:j) .eq. ']') m = 1
	         if (user(ch).file(j:j) .eq. ':') m = 1
	         if (user(ch).file(j:j) .eq. '/') m = 1
	         if (user(ch).file(j:j) .eq. '.') m = 2
	         j = j - 1
	      end do
	      if (m .eq. 1) then
	         user(ch).file = user(ch).file(1:k)//'.dat'
	      end if
	   end if
	   ok = w_resolve_filename(user(ch).file)
	end if

	if (vms) call to_lower(user(ch).file,0,0)
	if (k2len(user(ch).file) .lt. 1) goto 20
!	type *, '...using: ', user(ch).file(1:60)
	ok = w_setup_stream(ch, user(ch).file, r8a, r8b)
	if ( (ok .ne. 1) .and. offline_query) goto 26
	if (ok .ne. 1) goto 30

	if (return_times) then
	   t1 = r8a
	   t2 = r8b
	end if

	wind_tm_open_channel = 1
	return
  1	format(1x,'WIND_TM_OPEN_CHANNEL: ', a, :, a, :, a)
  5	return ! error already reported
 10	continue
	user(ch).channel = 0
	ch = 0
	wind_tm_open_channel = ok
	return ! error already reported
 20	continue
	wind_tm_open_channel = 0
	user(ch).channel = 0
	i = ch
	ch = 0
	if (user(i).suppress_messages) return
	write(6,1,iostat=ios) 'No file selected.'
	return
 26	continue
	wind_tm_open_channel = ok
	user(ch).channel = 0
	i = ch
	ch = 0
	if (user(i).suppress_messages) return
	j = max( k2len(user(i).file), 1)
	write(6,1,iostat=ios) 'error opening stream: ', user(i).file(:j)
	return
 30	continue
	wind_tm_open_channel = ok
	user(ch).channel = 0
	i = ch
	ch = 0
	if (user(i).suppress_messages) return
	j = max( k2len(query), 1)
!	if (realtime) then
!	   write(6,1,iostat=ios) 'parameter not ',
!	1 'REALTIME, OFFLINE, or valid file name: ', query(1:j)
!	else
	   write(6,1,iostat=ios) 'keyword or file name ',
	1 'not accepted: ', query(1:j)
	   j = max( k2len(user(i).file), 1 )
	   write(6,*,iostat=ios) '  File: ', user(i).file(1:j)
!	end if
	return
 40	continue
	wind_tm_open_channel = ok
	if (user(i).suppress_messages) return
	j = max( k2len(f), 1)
	write(6,1,iostat=ios) 'start/stop times not chosen.'
	return
 50	continue
	wind_tm_open_channel = ok
	if (user(i).suppress_messages) return
	j = max( k2len(f), 1)
	write(6,1,iostat=ios) 'cannot resolve file: ', f(:j)
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_resolve_filename(f)
	implicit	none
	include		'wind_os_def.for'
	character*(*)	f
	integer*4	w_get_file_list_from_dir !$pragma C (w_get_file_list_from_dir)
	integer*4	i,j,k
	logical*4	wild
	character*256	dir
	character*256	spec
	character*256	newf
	integer*4	sz, ret_sz, flags
	integer*4	ok
	integer*4	k3len

	w_resolve_filename = 0

	! check for wild cards
	wild = .false.
	wild = index(f,'*') .ne. 0
	if (.not. wild) wild = index(f,'%') .ne. 0
	if (.not. wild) wild = index(f,'?') .ne. 0

	if (unixos) then
	   i = index(f,':')
	   j = index(f,'/')
	   if (j .eq. 0) j = i + 1
	   if (i .gt. 1 .and. i .lt. j) then
	      dir = f(1:i-1)
$IF ABSOFT_FORTRAN
!  2008/03/03:  Usage of the Absoft compiler libraries appears to
!  cause the "C" version of "getenv" to be invoked, instead of the
!  Fortran version.   To get everything to work, I had to use the
!  "YEXT_LCS" compiler option (otherwise linkage lead to unresolved
!  externals).  This option appears to be what caused the "C"
!  version of "getenv" to be called, instead of the "getenv" from
!  the Absoft "U77" library.   The fix for this was to directly
!  list $ABSOFT/lib/libU77.a before the other libraries, and
!  refer, here to "getenv" as "getenv_".
              call getenv(dir, spec)
$ELSE
              call getenv(dir, spec)
$ENDIF
	      if (index(spec,':') .ne. 0) then
	         ! environment variable is a search list
	         wild = .true.
	      else if (spec(1:1) .ne. ' ') then
	         k = k3len(spec)
	         newf = spec(1:k)//'/'//f(i+1:len(f))
	         f = newf
	      end if
	   end if
	end if

	if (.not. wild) then
	   w_resolve_filename = 1
	   return
	end if

	if (vms) then
	   k = len(f)
	   i = index(f,']')
	   if (i .eq. k) return
	   if (i .eq. 0) then
	      i = index(f,':')
	      if (i .eq. k) return
	      if (i .le. 1) then
	         dir = 'wind_data'
	         spec = f
	      else
	         dir = f(1:i)
	         spec = f(i+1:k)
	      end if
	   else
	      dir = f(1:i)
	      spec = f(i+1:k)
	   end if
	else if (unixos) then
	   i = len(f)
	   k = index(f,':')
	   j = k3len(f)
	   if (k .gt. 1 .and. k .lt. i) then
	      dir = f(1:k-1)//char(0)
	      spec = f(k+1:j)//char(0)
	   else if (f(1:1) .ne. '/') then
	      dir = 'WIND_DATA'//char(0)
	      spec = f(1:j)//char(0)
	   else
	      dir = ' '
	      spec = ' '
	      k = 0
	      do while(i .gt. 0)
	         if (k .eq. 0 .and. f(i:i) .gt. ' ') k = i
	         if (f(i:i) .eq. '/' .or. f(i:i) .eq. ':') then
	            if (i .eq. len(f)) return
	            if (i .eq. k) return
	            dir = f(1:i)
	            j = i + 1
	            dir(j:j) = char(0)
	            if (dir(i:i) .eq. '/') dir(i:i) = char(0)
	            spec = f(i+1:k)
	            j = k - i + 1
	            spec(j:j) = char(0)
	            i = 0
	         end if
	         i = i - 1
	      end do
	   end if
	end if

	sz = 1
	ret_sz = 0
	! 3 = search subdirectories and get last matching file name
	! (eg., so wi_lz*_v02.dat is picked over wi_lz*_v01.dat for
	! a wild card file spec like wi_lz_*_v%%.dat)
	flags = '03'x	! bit mask, see w_get_file_list_from_dir source
	newf = ' '
	i = len(newf)
$IF ABSOFT_FORTRAN
	ok = w_get_file_list_from_dir(trim(dir)//char(0),
     +      trim(spec)//char(0),newf,sz,i,ret_sz,flags)
$ELSE
	ok = w_get_file_list_from_dir(dir,spec,newf,sz,i,ret_sz,flags)
$ENDIF
	if (ok .ne. 0 .and. ret_sz .eq. 1) then
	   f = newf
	   w_resolve_filename = 1
	else
	   w_resolve_filename = 0
	end if

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_setup_stream(ch,f,t1,t2)
! Branches to appropriate setup routine based on clues obtained from file
! name f.  E.g., files ending in ".wnd" are pased to w_wnd_setup_stream
! while files beginning with "wi_lz_wav" are send to w_cdhf_setup_stream.
	implicit	none
	integer*4	ch
	character*(*)	f
	real*8		t1,t2
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	integer*4	ok
	integer*4	w_wnd_setup_stream
	integer*4	w_cdhf_setup_stream
!	integer*4	w_decom_setup_stream
!	integer*4	w_setup_toc_stream
	integer*4	w_setup_rt_stream
	integer*4	w_get_env
	integer*4	k3len
	integer*4	ios
	logical*4	done
	integer*4	n_tries
	character*256	f2
	integer*4	i,j

	w_setup_stream = 1
	ok = 0

	n_tries = 0
	done = .false.
	do while(.not. done)
	   n_tries = n_tries + 1
	   done = .true.

	   if (index(f,'.wnd') .ne. 0) then
	      ! ".wnd" files, eg. WIND_1993_296_18_44.WND
	      ok = w_wnd_setup_stream(ch,f)
	      if (ok .eq. 1) user(ch).stream_type = wnd_stream
	   else if (index(f,'wi_lz_wav_') .ne. 0) then
	      ! EDR's/CDHF's, eg. WI_LZ_WAV_19930430_V01.DAT
	      ok = w_cdhf_setup_stream(ch,f,t1,t2)
	      if (ok .eq. 1) user(ch).stream_type = cdhf_lz_stream
	   else if (index(f,'.dat') .ne. 0) then
	      ! EDR's from CD-ROM:  yymmddvv.dat (also yyyydddh.dat format?)
	      i = index(f,'.dat') - 8
	      if (i .le. 0) goto 10
	      do j=i,i+7
	         if (f(j:j) .lt. '0' .or. f(j:j) .gt. '9') goto 10
	      end do
	      ok = w_cdhf_setup_stream(ch,f,t1,t2)
	      if (ok .eq. 1) user(ch).stream_type = cdhf_cdrom_stream
!	   else if (index(f, '.mcr') .ne. 0) then
!	      ! decom files, named in YYYYDDDH.MCR format 
!	      ! (MCR = Master Cross Reference file)
!	      ok = w_decom_setup_stream(ch,f)
!	   else if (index(f,'.toc') .ne. 0) then
!	      ! Table-Of-Contents files for .wnd files
!	      ok = w_wnd_setup_stream(ch,f)
	   else if ((f .eq. 'realtime') .and. realtime) then
	      ! realtime tm stream
	      ok = w_setup_rt_stream(ch,f)
	      if (ok .eq. 1) user(ch).stream_type = realtime_stream
	   else if (f(1:3) .eq. 'nrt') then
	      ! Near RealTime cdhf tcp/ip records
	      ok = w_cdhf_setup_stream(ch,f,t1,t2)
	      if (ok .eq. 1) user(ch).stream_type = cdhf_nrt_stream
	   else
	      ok = 0
	      if (n_tries .eq. 1) then
	         ! try to resolve the string as an environment variable
	         ok = w_get_env(f, f2)
	         if (ok .eq. 1 .and. k3len(f2) .gt. 1) then
	            done = .false.
	            f = f2
	         else
	            ok = 0
	         end if
	      else
	         goto 10
	      end if
	   end if

	end do

	user(ch).n_file_opens = user(ch).n_file_opens + 1

	w_setup_stream = ok

	return
  1	format(1x,'W_SETUP_STREAM: ', a, /, 1x, a)
 10	continue
	w_setup_stream = 0
	write(6,1,iostat=ios) 'Cannot identify file/stream type for ', f
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_channel(channel)
! Allocates a free channel number which is an index into a data structure
! containing parameters and variables pertinent to processing the user's
! selected data stream.
	implicit	none
	integer*4	channel
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	i
	integer*4	found
	integer*4	first_time /1/

	! find a free channel number
	wind_get_channel = 0
	if (first_time) then
	   first_time = 0
	   do i=1,max_channels
	      user(i).channel = 0
	      user(i).n_file_opens = 0
	      user(i).suppress_messages = .false.
	   end do
	end if

	found = 0
	i     = 0
	do while(found .eq. 0 .and. i .lt. max_channels)
	   i = i + 1
	   if (user(i).channel .eq. 0) found = 1
	end do
	if (found.eq.0) goto 5

	! initialize the data structure for this channel
	user(i).event_count		= 0
	user(i).major_frame		= -1
	user(i).minor_frame.b  		= -1
	user(i).file        		= ' '
	user(i).first_major		= 0
	user(i).last_major		= 0
	user(i).first_minor.i4val	= 0
	user(i).last_minor.i4val	= 0
	user(i).stream_type		= 0
	user(i).suppress_messages	= .false.
	user(i).channel			= i
	channel				= i

	wind_get_channel = 1
	return
  5	wind_get_channel = w_no_free_channel
	if (user(channel).suppress_messages) return
	type *, 'WIND_GET_CHANNEL: No free channels available.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_close_channel(ch)
! Closes offline file associated with channel ch and unsets certain flags.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	w_f_i
	integer*4	ok
	integer*4	w_channel_close	! an entry point

	!----------------------------------------------------------------------
	entry	w_channel_close(ch)

	wind_tm_close_channel = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	user(ch).file			= ' '
	user(ch).channel		= 0
	user(ch).all_tm_word_priv	= 0

	ok = w_f_i(%val(user(ch).f_close_ch), ch)
	if (ok .ne. 1) goto 20

	wind_tm_close_channel = ok

	return
  1	format(1x,'WIND_TM_CLOSE_CHANNEL: ', a, a)
  8	wind_tm_close_channel = w_bad_channel
	if (user(ch).suppress_messages) return
	type 1, 'invalid channel number.'
	return
! 10	wind_tm_close_channel = w_bad_inquire
!	if (user(ch).suppress_messages) return
!	type 1, 'Error using inquire stmt.'
!	return
 20	wind_tm_close_channel = ok
	if (user(ch).suppress_messages) return
	type 1, 'stream specific closing error'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_check_parms(rn,ch,major,minor)
! Checks arguments frequently passed to wind_tm_* routines.
! Note:  A bad channel number is considered a fatal error; this routine
! will call sys$exit.
	implicit	none
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch,major,minor
	character	rn*(*)		! routine name, caller's id string

	wind_check_parms = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10
	if (major .lt. 0) goto 30
	if (minor .lt. min_minor_frame_num .or.
	1   minor .gt. max_minor_frame_num) goto 40

	wind_check_parms = 1

	return
  1	format(1x,a,': ',a,a,:,1x,i)
  2	format(1x,a,': ',a, i,i)
  3	format(1x,a,': ',a,1x,i)
 10	wind_check_parms = w_bad_channel
	type 3, rn, 'invalid channel number.', ch

	stop 'Stopped because of invalid or corrupted channel number.'

 30	wind_check_parms = w_bad_major_frame_number
	if (user(ch).suppress_messages) return
	type 2, rn, 'invalid major frame number, MF.mf=', major, minor
	return
 40	wind_check_parms = w_bad_minor_frame_number
	if (user(ch).suppress_messages) return
	type 2, rn, 'invalid minor frame number, MF.mf=', major, minor
	if (realtime) type *, ' ...is the SCIM on?'
	return
	end
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! record processing routines
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_mfmf(ch,major,minor)
! Returns various major/minor frame indexes from current record, stream
! position, earliest available record, last record, or next record
! according to entry point.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	token
	integer*4	wind_tm_get_next_mfmf
	integer*4	wind_tm_get_earliest_mfmf
	integer*4	wind_tm_get_latest_mfmf
	integer*4	wind_tm_get_stream_mfmf
	integer*4	w_f_iiii
	integer*4	o
	integer*4	ok

	token = w_tk_current_mfmf
	goto 1000

	!----------------------------------------------------------------------
	entry	wind_tm_get_stream_mfmf(ch,major,minor)
	token = w_tk_stream_mfmf
	goto 1000

	!----------------------------------------------------------------------
	entry	wind_tm_get_earliest_mfmf(ch,major,minor)
	token = w_tk_earliest_mfmf
	goto 1000

	!----------------------------------------------------------------------
	entry	wind_tm_get_latest_mfmf(ch,major,minor)
	token = w_tk_latest_mfmf
	goto 1000

	!----------------------------------------------------------------------
	entry	wind_tm_get_next_mfmf(ch,major,minor)
	token = w_tk_next_mfmf
	goto 1000

 1000	continue
	wind_tm_get_mfmf = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	ok = w_f_iiii(
	1	%val(user(ch).f_get_rec_idx),
	1	ch, major, minor, token)

	wind_tm_get_mfmf = ok

	return
  1	format(1x,'WIND_TM_GET_MFMF: ',a,i)
  8	wind_tm_get_mfmf = w_bad_channel
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'invalid channel number.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_increment_mfmf(major,minor)
! Adds 1 to the minor frame number (modulo 250).  Adjusts the major frame
! number if necessary (when minor=250 on entry).
	implicit	none
	integer*4	major
	integer*4	minor
	integer*4	max
	parameter	(max='7fffffff'x)

	minor = minor + 1
	if (minor .gt. 249) minor = 0
	if (minor.eq.0) then
	   if (major .ge. max) major = 0
	   major = major + 1
	end if

	wind_tm_increment_mfmf = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wind_tm_increment_packet
	1				(major,minor)
! Adds 1 to 10 to the minor frame number (modulo 250) so frame indexes
! identify the beginning of the next packet (minor frame numbers 0, 10, 20,
! 30, ..., 230, 240, 0, 10, ...).  Adjusts the major frame
! number if necessary.
	implicit	none
	integer*4	major
	integer*4	minor
	integer*4	max
	parameter	(max='7fffffff'x)

	minor = minor + 10 - mod(minor,10)

	if (minor .gt. 249) then
	   minor = 0
	   if (major .ge. max) major = 0
	   major = major + 1
	end if

	wind_tm_increment_packet = 1

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_decrement_packet
	1				(major,minor)
! Subtracts 1 to 10 from the minor frame number (modulo 250) so frame indexes
! identify the beginning of the previous packet (minor frame numbers 0, 10, 20,
! 30, ..., 230, 240, 0, 10, ...).  Adjusts the major frame
! number if necessary.
	implicit	none
	integer*4	major
	integer*4	minor
	integer*4	minval
	parameter	(minval='0'x)

	minor = minor - 10 - mod(minor,10)

	if (minor .lt. 0) then
	   minor = 240
	   if (major .le. minval) major = 1
	   major = major - 1
	end if

	wind_tm_decrement_packet = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wind_tm_decrement_mfmf(major,minor)
! Subtracts 1 from the minor frame number (accounting for the modulo 250
! numbering) and adjusts the major frame counter if necessary.
	implicit	none
	integer*4	major
	integer*4	minor
	integer*4	max
	parameter	(max='7fffffff'x)

	minor = minor - 1
	if (minor .lt. 0) minor = 249
	if (minor.eq.249) major = major - 1
	if (major .lt. 1) major = max

	wind_tm_decrement_mfmf = 1
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_frame_period
	1				(ch,major,minor,rate)
! Determines the "frame rate" in real seconds for a given position in the
! stream.  "Hi" should be 5.4346 frames/sec and "lo" should be 2.7173 frame/sec.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's data struc
	integer*4	major			! major frame number
	integer*4	minor			! minor frame number
	real*4		rate			! bit rate
	integer*4	ok			! for checking return statuses
	integer*4	w_f_iiir		! a function

	ok = w_f_iiir(%val(user(ch).f_get_frame_rate),
	1	ch, major, minor, rate)

	wind_frame_period = ok

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_bit_rate
	1				(ch,major,minor,bit_rate)
! Returns the effective TM bit rate in bits per second.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	real*4		bit_rate			! bits per second
	integer*4	ch				! channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	real*4		frame_period			! seconds per minor fr.
	real*4		bits_per_frame
	parameter	(bits_per_frame=256.0*8.0)
	integer*4	ok				! status variable
	logical*4	wind_suppress_messages		! a function
	integer*4	wind_check_parms		! a function
	integer*4	rn
	parameter	(rn='WIND_TM_BIT_RATE')
	integer*4	w_f_iiir

	wind_tm_bit_rate = 0

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8
	bit_rate = 0

	ok = w_f_iiir(%val(user(ch).f_get_frame_rate),
	1	ch,major,minor,frame_period)
	if (ok .ne. 1) goto 10
	if (frame_period .eq. 0.0) goto 20

	bit_rate = bits_per_frame / frame_period

	wind_tm_bit_rate = 1

	return
  1	format(1x,'WIND_TM_BIT_RATE: ', a)
  8	wind_tm_bit_rate = ok
	return
 10	wind_tm_bit_rate = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get frame period to determine bit rate.'
	return
 20	wind_tm_bit_rate = w_bad_frame_period
	if (wind_suppress_messages(ch)) return
	type 1, 'frame period is 0.0, check data.'
	return
	end
!------------------------------------------------------------------------------
	logical*4	function	wind_tm_eof(ch,major,minor)
! Always returns logical false (0) for realtime stream.  Returns 0 for offline
! stream when the last record read is less the last record in the file (by
! frame numbers).  Returns 1 for offline stream when the last record read
! is the last record in the file.
	implicit	none
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	last_major, last_minor
	integer*4	token
	integer*4	w_f_iiii
	integer*4	ok
	integer*4	o

	wind_tm_eof = .false.
	if (ch.lt.1 .or. ch.gt.max_channels) goto 10

	if (realtime .and. (rltm(ch).ch .ne. 0)) return
	if (user(ch).stream_type .eq. cdhf_nrt_stream) return

	token = w_tk_latest_mfmf
	ok = w_f_iiii(
	1	%val(user(ch).f_get_rec_idx),
	1	ch, last_major, last_minor, token)
	if (ok .ne. 1) goto 20

	if (major .lt. last_major) return
	if (major .eq. last_major .and. minor .lt. last_minor) return

	wind_tm_eof = .true.
	return
  1	format(1x,'WIND_TM_EOF: ', a)
 10	continue
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'invalid channel number.'
	return
 20	continue
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'cannot get latest (eof) stream position.'
	return
	end
!------------------------------------------------------------------------------
	logical*4	function	wind_tm_realtime(ch)
! Returns true if the specified channel (ch) is opened for realtime
! access and false if the channel is opened for offline access.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_realtime_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch

	wind_tm_realtime = .false.
	if (ch.lt.1 .or. ch.gt.max_channels) goto 10

	wind_tm_realtime = rltm(ch).ch .ne. 0

	return
  1	format(1x,'WIND_TM_REALTIME: ', a)
 10	continue
!	wind_tm_realtime = w_bad_channel
	if (user(ch).suppress_messages) return
	type 1, 'invalid channel number.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_get_record
	1				(ch,major,minor,rec)
! Gets one TM record of data (essentially a complete minor frame of data with
! some extra stuff glued on) based on channel and MF/mf frame numbers.
! Note that for most stream types the non-WIND data words are null.
! Checks the current record to see if it is the specified record.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	record		/low_byte/ minor
	record		/wind_record/ rec
	integer*4	ok
	integer*4	wind_check_parms
	integer*4	rn
	parameter	(rn='WIND_GET_RECORD')
	integer*4	w_f_iiii
	integer*4	o

	wind_get_record = 0
	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	! check if current record is the desired record
	if (user(ch).wind_record.major_frame .eq. major .and.
	1   user(ch).wind_record.minor_frame .eq. minor.b) then
	   wind_get_record = w_success
	else
	   ! get a new record
	   ok = w_f_iiii(%val(user(ch).f_get_intrnl_rec), 
	1	ch, major, minor, user(ch).wind_record)
	   if (ok .ne. 1) goto 10
	end if

	rec = user(ch).wind_record

	user(ch).major_frame = user(ch).wind_record.major_frame
	user(ch).minor_frame.b = user(ch).wind_record.minor_frame

$IF ABSOFT_FORTRAN
	ok = zext( user(ch).wind_record.quality ) .and. '01'x
$ELSE
	ok = zext( user(ch).wind_record.quality ) .and. 1
$ENDIF
	if (ok .ne. 1) goto 40
!	if (.not. zext( user(ch).wind_record.quality ) ) goto 40

	wind_get_record = 1

	return
  1	format(1x,'WIND_GET_RECORD: ', a, a)
  2	format(1x,'WIND_GET_RECORD: ', a, i9,'.',i3.3)
  8	wind_get_record = ok
	return
 10	wind_get_record = ok
	return			! error message already printed (if enabled)
 40	wind_get_record = w_bad_rec_quality_flag
	if (user(ch).suppress_messages) return
	write(6,2,iostat=o) 'Bad quality flag in minor frame',
	1	user(ch).major_frame, user(ch).minor_frame.i4val
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_delta_mfmf
	1				(major1,minor1,major2,minor2,diff)
! Calculates the number of records (minor frames) between two given records
! identified by (major-frame,minor-frame) indexes.  The (major1,minor1) record
! is normally considered to be the earlier in time, returning a positive
! valued diff.  Negatively valued diffs are returned when the (major1,minor1)
! record is the later (implied by time) of the two records.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	major1, major2
	record		/low_byte/	minor1, minor2
	integer*4	diff
	integer*4	num_majors
	logical*4	wind_suppress_internal_msgs

	wind_tm_delta_mfmf = 0

	! check for bad values
	if (major1 .lt. 0) goto 10
	if (major2 .lt. 0) goto 11
	if (minor1.i4val .lt. 0) goto 20
	if (minor2.i4val .lt. 0) goto 21
	if (minor1.i4val .gt. max_minor_frame_num) goto 20
	if (minor2.i4val .gt. max_minor_frame_num) goto 21

	num_majors = major2 - major1
	if (num_majors .eq. 0) then
	   ! same major frame
	   diff = minor2.i4val - minor1.i4val
	else if (num_majors .gt. 0) then
	   ! normal condition, record 2 is later than record 1
	   diff = num_minors_per_major - minor1.i4val + minor2.i4val
	1	+ ((num_majors-1)*num_minors_per_major)
	else
	   ! reverse condition, record 2 is earlier than record 1
	   diff = -(num_minors_per_major - minor2.i4val + minor1.i4val) +
	1	  ((num_majors+1)*num_minors_per_major)
	end if

	wind_tm_delta_mfmf = 1
	return
 10	wind_tm_delta_mfmf = w_bad_major_frame_number
	if (wind_suppress_internal_msgs()) return
	type *, 'WIND_TM_DELTA_MFMF: Bad major1 frame value:', major1
	return
 11	wind_tm_delta_mfmf = w_bad_major_frame_number
	if (wind_suppress_internal_msgs()) return
	type *, 'WIND_TM_DELTA_MFMF: Bad major2 frame value:', major2
	return
 20	wind_tm_delta_mfmf = w_bad_minor_frame_number
	if (wind_suppress_internal_msgs()) return
	type *, 'WIND_TM_DELTA_MFMF: Bad minor1 frame value:', minor1.i4val
	return
 21	wind_tm_delta_mfmf = w_bad_minor_frame_number
	if (wind_suppress_internal_msgs()) return
	type *, 'WIND_TM_DELTA_MFMF: Bad minor2 frame value:', minor2.i4val
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wind_tm_set_messages_off(ch)
! This routine disables the logging of error messages to the user's terminal.
	implicit	none
	integer*4	ch
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	w_messages_off	! an entry point

	!----------------------------------------------------------------------
	entry	w_messages_off(ch)

	wind_tm_set_messages_off = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	user(ch).suppress_messages = .true.

	wind_tm_set_messages_off = 1
	return
  1	format(1x,'WIND_TM_SET_MESSAGES_OFF: ', a)
 08	wind_tm_set_messages_off = w_bad_channel
	type 1, 'invalid channel number.'
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wind_tm_set_messages_on(ch)
! This routine disables the logging of error messages to the user's terminal.
	implicit	none
	integer*4	ch
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	w_messages_on

	!----------------------------------------------------------------------
	entry	w_messages_on(ch)

	wind_tm_set_messages_on = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	user(ch).suppress_messages = .false.

	wind_tm_set_messages_on = 1
	return
  1	format(1x,'WIND_TM_SET_MESSAGES_ON: ', a)
 08	wind_tm_set_messages_on = w_bad_channel
	type 1, 'invalid channel number.'
	return
	end

!------------------------------------------------------------------------------
	logical*4	function	wind_suppress_messages(ch)
	! returns state of message suppression for given channel
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	wind_set_message_state		! an entry
	integer*4	wind_get_message_state		! an entry
	logical*4	w_get_msg_state_updt_pos	! an entry
	logical*4	state
	integer*4	major
	record /low_byte/ minor

	wind_suppress_messages = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	wind_suppress_messages = user(ch).suppress_messages
	return
  1	format(1x,'WIND_SUPPRESS_MESSAGES: ', a)
 08	wind_suppress_messages = w_bad_channel
	type 1, 'invalid channel number.'
	return


	!----------------------------------------------------------------------
	entry	w_get_msg_state_updt_pos(ch, major, minor)
! updates the current stream position from arguments and returns the current
! message state for the channel
	! set current position in main user structure
	user(ch).wind_record.major_frame = major
	user(ch).wind_record.minor_frame = minor.b
	w_get_msg_state_updt_pos = user(ch).suppress_messages
	return

	!----------------------------------------------------------------------
	entry	wind_set_message_state(ch, state)
! Directly sets the channel's message state to argument value.
	user(ch).suppress_messages = state
	wind_set_message_state = 1
	return

	!----------------------------------------------------------------------
	entry	wind_get_message_state(ch, state)
! Directly puts the channel's message state to argument value.
	state = user(ch).suppress_messages
	wind_get_message_state = 1
	return
	end

!------------------------------------------------------------------------------
	logical*4	function	wind_suppress_internal_msgs()
! returns .true. if message suppression is true for any channel
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	logical*4	ch_msg_mask
	integer*4	i
	integer*4	wind_suppress_internal_msgs_	! an entry, C sun-ism
	entry	wind_suppress_internal_msgs_()
	ch_msg_mask = .false.
	do i=1, max_channels
	   if (user(i).channel .ne. 0) then
	      ch_msg_mask = ch_msg_mask .or. user(i).suppress_messages
	   end if
	end do
	wind_suppress_internal_msgs = ch_msg_mask
	return
	end
!------------------------------------------------------------------------------
!
! Centralized output message handling service
!
	integer*4	function	w_message_output_routines()
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch			! TM channel or zero
	character*(*)	s1			! prefix, eg.: routine name
	character*(*)	s2			! one line message body
	integer*4	iarg
	integer*4	i,j,n,o
	integer*4	k2len
	integer*4	style
	logical*4	show
	logical*4	wind_suppress_internal_msgs
	integer*4	u /6/			! output logical unit number
	character*256	w
	integer*4	w_msg_file 		! an entry point
	integer*4	w_msg_put		! an entry point
	integer*4	w_msg_put2		! an entry point
	integer*4	w_msg_put_always	! an entry point
	integer*4	w_msg_put_always2	! an entry point

	entry	w_msg_put(ch,s1,s2)
	style = 1
	goto 500

	entry	w_msg_put2(ch,s1,s2,iarg)
	style = 2
	goto 500

 500	continue
	w_message_output_routines = 1

	! display or not display message based on user's current message state
	if (ch .ge. 1 .and. ch .le. max_channels) then
	   show = .not. user(ch).suppress_messages
	else
	   ! the channel number is a dummy argument (assume it is not invalid!)
	   ! probably invoked from a "channelless" routine
	   ! ...try to determine the message state
	   i = 0
           n = 0
	   do j=max_channels,1,-1
	      if (user(j).channel .ne. 0) then
	         i = j
	         n = n + 1
	      end if
	   end do
	   if (i .eq. 0) then		! no channel open
	      show = .true.
	   else if (n .eq. 1) then	! only one channel open
	      show = .not. user(i).suppress_messages
	   else				! mulitple channels open
	      show = wind_suppress_internal_msgs()
	   end if
	end if

	if (show) goto 1000
	return

	entry	w_msg_put_always(s1,s2)
	style = 1
	goto 1000

	entry	w_msg_put_always2(s1,s2,iarg)
	style = 2
	goto 1000

 1000	continue
	w = ' '
	i = max(1, k2len(s1))
	j = max(1, k2len(s2))
	if (style .eq. 1) then
	   if (s2(1:1) .eq. ':') then
	      write(w,11,iostat=o) s1(1:i), s2(1:j)
	   else
	      write(w,1,iostat=o) s1(1:i), s2(1:j)
	   end if
	else if (style .eq. 2) then
	   write(w,2,iostat=o) s1(1:i), s2(1:j), iarg
	else
	   write(w,'(1x,a,a,a)',iostat=o)  s1(1:i), s2(1:j), ' ?style?'
	end if
   1	format(a,1x,a)
  11	format(a,a)
   2	format(a,1x,a,i6)
 100	format(1x,a)
	w_msg_put = 1

	! write output in chunks so it fits in 79 character wide display
	! without triggering unwanted line feeds
	i = k2len(w)
	if (i .le. 78) then
	   write(u,100,iostat=o) w(1:78)
	end if
	if (i .ge. 79) then
	   write(u,100,iostat=o) w(79:156)
	end if
	if (i .ge. 156) then
	   write(u,100,iostat=o) w(156:233)
	end if
	if (i .ge. 234) then
	   write(u,100,iostat=o) w(234:256)
	end if

	if (o .ne. 0) w_msg_put = 0
	return

	!---------------------------------------------------------------------
	entry	w_msg_file(s1)
	if (u .eq. 6) then
	   call lib$get_lun(i)
	else
	   ! close current message file
	   close(u)
	   i = u
	end if
	open(i,
	1	file=s1,
	1	carriagecontrol='list',
	1	status='new',
	1	iostat=o)
	if (o .eq. 0) then
	   w_msg_file = 1
	   u = i
	else
	   u = 6
	   call lib$free_lun(i)
	   w_msg_file = 0
	end if

	return
	end
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Telemetry retrieval functions
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!	options/check=bounds
!------------------------------------------------------------------------------
	logical*4	function	is_a_collected_word(ch,word)
! Determines wether or not the specified word is a collected word, which is
! some indication of the word's validity.
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch
	integer*4	word
 
	is_a_collected_word = .false.
	if (word .lt. 0 .or. word .gt. 255) return

	is_a_collected_word = user(ch).is_a_valid_ptr(word)
	is_a_collected_word = is_a_collected_word .or.
	1			user(ch).all_tm_word_priv

	return
	end
!	options/check=bounds
!------------------------------------------------------------------------------
	integer*4	function	wind_set_tm_word_priv(ch,priv)
! Allows the user to access all TM words from specified stream.  For disk files
! this means the caller may request TM words that were not collected--so junk
! is returned.  For the realtime stream the caller is allowed to retrieve
! TM words not collected by the disk recording process.
! Argument priv is logically true (1) to allow access to all TM words or
! logically false (0, default) to deny access to non-collected TM words.
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch
	integer*4	priv

	wind_set_tm_word_priv = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) return

	user(ch).all_tm_word_priv = priv

	wind_set_tm_word_priv = 1

	return
	end
!	options/check=bounds
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_word
	1				(channel,major,minor,word,datum)
! Returns a zero-extended byte "word" of data from the specified frame.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	channel
	integer*4	major
	!record		/low_byte/	minor
	integer*4	minor
	integer*4	word
	integer*4	datum
	integer*4	ok
	integer*4	wind_get_record
	integer*4	wind_check_parms
	logical*4	is_a_collected_word
	integer*4	rn
	parameter	(rn='WIND_TM_GET_WORD')

	wind_tm_get_word = 0
	datum = 0
	ok = wind_check_parms(rn,channel,major,minor)
	!ok = wind_check_parms(rn,channel,major,i)
	!ok = wind_check_parms(rn,channel,major,minor.i4val)
	!ok = wind_check_parms(rn,channel,major,loc(minor.i4val))
	!ok = wind_check_parms(rn,channel,major)
	if (ok .ne. 1) goto 8
	if (word.lt.0 .or. word .gt. 255) goto 9

	if (.not. is_a_collected_word(channel,word)) goto 7

	ok = wind_get_record(channel,major,minor,
	1	user(channel).wind_record)
	if (ok .ne. 1) goto 10

	datum = zext(user(channel).wind_record.data(word))

	wind_tm_get_word = 1

	return
  1	format(1x,'WIND_TM_GET_WORD: ',a)
  7     wind_tm_get_word = w_word_not_collected
	if (user(channel).suppress_messages) return
	type 1, 'specified TM word not collected.'
	return
  8	wind_tm_get_word = ok
	return
  9	wind_tm_get_word = w_bad_argument_value
	if (user(channel).suppress_messages) return
	type 1, 'invalid word index.'
	return
 10	wind_tm_get_word = ok
	if (user(channel).suppress_messages) return
	type 1, 'Cannot get word of data.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_minor_frame
	1				(ch,major,minor,data)
! Returns a 250 longword array (a minor frame with each byte zero-extended
! into a 32-bit integer) referenced by the major and
! minor frame counters that are supplied as calling arguments.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	data(*)		! must be 250 or greater
	integer*4	i
	integer*4	ok
	integer*4	wind_check_parms
	integer*4	w_f_iiii
	integer*4	rn
	parameter	(rn='WIND_TM_GET_MINOR_FRAME')

	wind_tm_get_minor_frame = 0
	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	ok = w_f_iiii(%val(user(ch).f_get_intrnl_rec), 
	1	ch, major, minor, user(ch).wind_record)
	if (ok .ne. 1) goto 10

	do i=1,sizeof_minor_frame
	   data(i) = zext(user(ch).wind_record.data(i-1))
	end do

	wind_tm_get_minor_frame = 1

	return
  1	format(1x,'WIND_TM_GET_MINOR_FRAME: ', a, a)
  8	wind_tm_get_minor_frame = ok
	return
 10	wind_tm_get_minor_frame = ok
	if (user(ch).suppress_messages) return
	type 1, 'Cannot get minor frame.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_major_frame
	1				(ch,major,data)
! Returns a major frame.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	data(*)		! must be (250*256) or greater
	integer*4	w_f_iiii
	integer*4	i,j,k
	integer*4	num_missing
	integer*4	got_a_record
	integer*4	ok
	integer*4	wind_check_parms
	integer*4	rn
	parameter	(rn='WIND_TM_GET_MAJOR_FRAME')

	k = 0
	num_missing = 0
	wind_tm_get_major_frame = 0
	ok = wind_check_parms(rn,ch,major,min_minor_frame_num)
	if (ok .ne. 1) goto 8

	do i=min_minor_frame_num,max_minor_frame_num
	   got_a_record = w_f_iiii(%val(user(ch).f_get_intrnl_rec), 
	1	ch, major, i, user(ch).wind_record)
	   if (got_a_record .eq. 1) then
	      do j=1,256
	         data(k+j) = zext(user(ch).wind_record.data(j-1))
	      end do
	   else
	      num_missing = num_missing + 1
	      do j=1,256
	         data(k+j) = -1
	      end do
	   end if
	   k = k + 256
	end do

	if (num_missing .ne. 0) goto 10
	wind_tm_get_major_frame = 1

	return
  8	wind_tm_get_major_frame = ok
	return
 10	wind_tm_get_major_frame = w_missing_record
	if (user(ch).suppress_messages) return
	type *, 'WIND_TM_GET_MAJOR_FRAME: Missing minor frame(s), count=',
	1	num_missing
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_packet
	1				(ch,major,minor,packet)
! Returns the packet that includes the major and minor frame indexes specified.
! Major and minor frame indexes may be anywhere within the selected packet.
! The user's major and minor arguments are advanced to point to the last
! frame of the selected packet on return from this routine.  All TM words
! are zero-extended into 32-bit integers.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	packet(431)
	integer*4	ok
	integer*4	wind_check_parms
C	integer*4	rn
	character	rn*(*)
	parameter	(rn='WIND_TM_GET_PACKET')
	integer*4	i,j
	integer*4	w_f_iiiiiib		! a function

	wind_tm_get_packet = 0

	j = 0
	do i=1,431
	   user(ch).packet.w(j) = 'ff'x
	   packet(i) = 'ffffffff'x
	   j = j + 1
	end do

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok.ne.1) goto 8

	ok = w_f_iiiiiib(%val(user(ch).f_get_plain_packet),
	1	ch,major,minor,-1,forward,0,user(ch).packet.w)

	j = 0
	do i=1,431
	   packet(i) = zext(user(ch).packet.w(j))
	   j = j + 1
	end do

	if (ok.ne.1) goto 10

	wind_tm_get_packet = ok
	return


  1	format(1x,'WIND_TM_GET_PACKET: ',a, i4)
  8	wind_tm_get_packet = ok
	return
 10	wind_tm_get_packet = ok
	if (user(ch).suppress_messages) return
	type 1, 'cannot get packet, ok=', ok
	return
	end
!	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	wind_get_plain_packet
	1			(ch,major,minor,type,direction,seek_1st)
! This routine gathers up a TM packet from the specified stream.  The search
! begins at the specified major,minor frame position and continues, if
! necessary, using the by_type, direction, and seek_1st arguments as criteria.
! The "packet" member of the user's wind_lib internal data structure is
! filled with the gathered packet.
! Note: this routine may not be called directly by wind_lib routines--it is
! provided as a convenience for user's (low level) applications.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	type			! 0..15, packet id
	integer*4	direction		! forward/reverse in stream
	logical*4	seek_1st		! seek a 1st packet of type

	integer*4	ok
	integer*4	w_f_iiiiiib
	integer*4	w_get_plain_pkt
	byte		pkt(*)

	wind_get_plain_packet = 0

	ok = w_f_iiiiiib( %val(user(ch).f_get_plain_packet),
	1	ch,major,minor,type,direction,seek_1st,user(ch).packet)

	wind_get_plain_packet = ok
	return

	!----------------------------------------------------------------------
	entry w_get_plain_pkt (ch,major,minor,type,direction,seek_1st,pkt)

	ok = w_f_iiiiiib( %val(user(ch).f_get_plain_packet),
	1	ch,major,minor,type,direction,seek_1st,pkt)

	w_get_plain_pkt = ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_packet_sum(packet,check)
! Calculates a checksum for integer*4 array packet.
	implicit	none
	integer*4	packet(431)
	integer*4	check
	integer*4	i
	check = 0
	do i=1,431
	    check = check + zext(packet(i))
	end do
	check = check .and. 255
	wind_packet_sum = check
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_plain_packet_sum(packet,check)
! Calculates a checksum for byte array packet.
	implicit	none
	byte		packet(431)
	integer*4	check
	integer*4	i
	check = 0
	do i=1,431
	    check = check + zext(packet(i))
	end do
	check = check .and. 255
	wind_plain_packet_sum = check
	return
	end
!	options/extend_source/check=bounds
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_hk
	1				(ch,major,hkindex,hkdatum)
! Returns the house keeping word indicated by the users calling arguments
! of major frame number and house keeping word number.
!
	implicit	none
	include		'wind_hk_addr_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! user's channel number
	integer*4	major			! major frame number
	integer*4	hkindex			! house keeping word number
	integer*4	hkdatum			! hk word returned
	integer*4	ok			! status variable
	integer*4	wind_get_hk_addr_from_bitoffset		! an entry point
	integer*4	bitoffset, mf_addr, word_addr
	integer*4	w_f_iiiib		! a function
	integer*4	ios
	record /low_byte/ lb
	integer*4	i

	wind_tm_get_hk = 0

	hkdatum = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10
	if (major .lt. 1) goto 11

	ok = w_f_iiiib(%val(user(ch).f_get_hk),
	1	ch,major,hkindex,hkindex,lb.b)
	if (ok .ne. 1) goto 12

	hkdatum = lb.i4val

	wind_tm_get_hk = 1

	return
  1	format(1x,'WIND_TM_GET_HK: ',a, :, i)
 10	wind_tm_get_hk = w_bad_channel
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'bad channel number.'
	return
 11	wind_tm_get_hk = w_bad_major_frame_number
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'bad major frame number.'
	return
 12	wind_tm_get_hk = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'cannot get HK word.'
	return

	!---------------------------------------------------------------------
	entry	wind_get_hk_addr_from_bitoffset(bitoffset, mf_addr, word_addr)
! Determines the the hk minor_frame/word address from a given bitoffset
! for the first 80 hk words.
	i = bitoffset / 8
        if (i .ge. w_first_core_hk_word .and.
     1        i .le. w_last_core_hk_word) then
	   mf_addr   = hk_mf_addr(i)
	   word_addr = hk_word_addr(i)
	   wind_get_hk_addr_from_bitoffset = 1
	else
	   mf_addr   = -1
	   word_addr = -1
	   wind_get_hk_addr_from_bitoffset = 0
	end if

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_filename(ch,file)
! Returns to the caller, for the specified channel, the name of the offline
! file when the access is offline or the string 'REALTIME' when the access
! stream is realtime.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! user's channel number
	character*(*)	file			! user's string storage
	logical*4	wind_tm_realtime
	integer*4	w_channel_filename	! an entry point
	integer*4	ios

	!----------------------------------------------------------------------
	entry	w_channel_filename(ch,file)

	wind_tm_get_filename = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10

	file = user(ch).file

	if (wind_tm_realtime(ch)) then
	   if (user(ch).file(1:1) .le. ' ') then ! realtime & not recording
	      file = 'NONE'
	   end if
	end if

	wind_tm_get_filename = 1

	return
  1	format(1x,'WIND_TM_GET_FILENAME: ', a,a)
 10	wind_tm_get_filename = w_bad_channel
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number.'
	return
	end
!	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	zext_bits_to_longword(source,start,size)
!
! xxxxx Need to rewrite this to work with byte aligned source
!
! This routine extracts size (1 to 32) bits from source at bit position start
! and copies the extracted bits to the low order bits of the function return
! value.  The selected bits may cross a longword boundary.
	implicit	none
	include		'wind_return_code_def.for'
	integer*4	source(*)	! an arbitrary number of bits
	integer*4	start		! bit start position
	integer*4	size		! number of bits to get
	integer*4	i		! longword containing bit start position
	integer*4	num_bits
	integer*4	end_bit
	integer*4	start_bit
	integer*4	offset
	integer*4	value
	logical*4	wind_suppress_internal_msgs

	zext_bits_to_longword = 0
	value = 0
	if (size .lt. 1 .or. size .gt. 32) goto 10
	i = start/32 + 1
	start_bit = mod(start,32)
	end_bit   = start_bit + size - 1
	if (end_bit .gt. 31) then
	   !num_bits = size						! this
	   !call mvbits(source(i), start_bit, num_bits, value, 0)	! works

	   num_bits = 32 - start_bit
	   call mvbits(source(i), start_bit, num_bits, value, 0)
	   offset = num_bits
	   num_bits = size - num_bits
	   i = i + 1
	   call mvbits(source(i), 0, num_bits, value, offset)
	else
	   call mvbits(source(i), start_bit, size, value, 0)
	end if
	zext_bits_to_longword = value

!	type '(1x,''source: '',z8.8,''.'',z8.8,8x,z8.8)',
!	1	source((start/32)+1), source(start/32), value


	return
  1	format(1x,'ZEXT_BITS_TO_LONGWORD: ', a, i)
 10	if (wind_suppress_internal_msgs()) return
	type 1, 'invalid size parameter: ', size
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	copy_bits_to_longword
	1				(source,start,size,dest,dsb)
!
! xxxxx Need to rewrite this to work with byte aligned source
!
! This routine extracts size (1 to 32) bits from source at bit position start
! and copies the extracted bits to destination dest beginning at bit dsb.
! The selected bits may cross a longword boundary.
	implicit	none
	include		'wind_return_code_def.for'
	integer*4	source(*)	! an arbitrary number of bits
	integer*4	start		! bit start position of source
	integer*4	size		! number of bits to get from source
	integer*4	dest		! destination for source bits
	integer*4	dsb		! destination start bit: [0..31]
	integer*4	i		! longword containing bit start position
	integer*4	num_bits
	integer*4	end_bit
	integer*4	start_bit
	integer*4	offset
	logical*4	wind_suppress_internal_msgs

	copy_bits_to_longword = 0
	if (size .lt. 1 .or. size .gt. 32) goto 10
	if (dsb .lt. 0 .or. dsb .gt. 31) goto 20
	i = start/32 + 1
	start_bit = mod(start,32)
	end_bit   = start_bit + size - 1
	if (end_bit .gt. 31) then
	   !num_bits = size						! this
	   !call mvbits(source(i), start_bit, num_bits, dest, 0)	! works

	   num_bits = 32 - start_bit
	   call mvbits(source(i), start_bit, num_bits, dest, dsb)
	   offset = num_bits
	   num_bits = size - num_bits
	   i = i + 1
	   call mvbits(source(i), 0, num_bits, dest, offset+dsb)
	else
	   call mvbits(source(i), start_bit, size, dest, dsb)
	end if
	copy_bits_to_longword = 1

!	type '(1x,''source: '',z8.8,''.'',z8.8,8x,z8.8)',
!	1	source((start/32)+1), source(start/32), dest


	return
  1	format(1x,'COPY_BITS_TO_LONGWORD: ', a, i)
 10	copy_bits_to_longword = w_bad_bit_size_arg
	if (wind_suppress_internal_msgs()) return
	type 1, 'invalid size parameter: ', size
	return
 20	copy_bits_to_longword = w_bad_bit_dest_arg
	if (wind_suppress_internal_msgs()) return
	type 1, 'invalid value for destination start bit: ', dsb
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_test(ch,major,minor,test)
! Returns the WIND/WAVES Script Player (STOL) test number for the given
! stream position.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! read, channel number
	integer*4	major		! read, major frame number
	integer*4	minor		! read, minor frame number
	integer*4	test		! write, script test number
	integer*4	ok		! return status variable
	integer*4	wind_get_record	! a function
	integer*4	wind_check_parms! a function
	integer*4	rn
	parameter	(rn='WIND_TM_GET_TEST')

	wind_tm_get_test = 0

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 10

	ok = wind_get_record(ch,major,minor,user(ch).wind_record)
	if (ok .ne. 1) goto 20

	test = zext(user(ch).wind_record.sp_test_number)

	wind_tm_get_test = 1

	return
  1	format(1x,'WIND_TM_GET_TEST: ', a)
 10	wind_tm_get_test = ok
	return
 20	if (user(ch).suppress_messages) return
	type 1, 'cannot get record.'
	wind_tm_get_test = ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_step(ch,major,minor,step)
! Returns the WIND/WAVES Script Player (STOL) step number of test for the
! given stream position.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! read, channel number
	integer*4	major		! read, major frame number
	integer*4	minor		! read, minor frame number
	integer*4	step		! write, script step number
	integer*4	ok		! return status variable
	integer*4	wind_get_record	! a function
	integer*4	wind_check_parms! a function
	integer*4	rn
	parameter	(rn='WIND_TM_GET_STEP')

	wind_tm_get_step = 0

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 10

	ok = wind_get_record(ch,major,minor,user(ch).wind_record)
	if (ok .ne. 1) goto 20

	step = zext(user(ch).wind_record.sp_step_number)

	wind_tm_get_step = 1

	return
  1	format(1x,'WIND_TM_GET_STEP: ', a)
 10	wind_tm_get_step = ok
	return
 20	if (user(ch).suppress_messages) return
	type 1, 'cannot get record.'
	wind_tm_get_step = ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_stream_type(ch,stream_type)
! Returns a coded integer stream type.
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch		! read, channel number
	integer*4	stream_type	! write, coded stream type

	stream_type = user(ch).stream_type
	wind_tm_get_stream_type = 1
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_sc_mode(ch, mjr, mnr, mode)
! Returns a coded integer space craft telemetry mode type.  .wnd/realtime
! codes are converted to cdhf equivalent values.
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch		! read, channel number
	integer*4	mjr		! read, major frame number
	integer*4	mnr		! read, minor frame number
	integer*4	mode		! write, coded space craft tm mode
	integer*4	ok
	integer*4	w_f_iiii	! a function
	integer*4	ios

	wind_tm_get_sc_mode = 0

	ok = w_f_iiii(%val(user(ch).f_get_tm_mode), ch, mjr, mnr, mode)
	if (ok .ne. 1) goto 10

	wind_tm_get_sc_mode = 1
	return
  1	format(1x,'WIND_TM_GET_SC_MODE: ', a, :, z8.8, 'x')
 10	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'cannot TM mode.'
	wind_tm_get_sc_mode = ok
	return
	end

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!--- stored function address call facilitator ---------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
	integer*4	function	w_f_external_calls()
	implicit	none
	integer*4	i1,i2,i3,i4,i5,i6
	character*(*)	c1
	real*4		r1
	real*8		d1
	byte		b1
	byte		b_1(*)
	logical*4	l1,l2
	integer*4	fun		! passed function address
	integer*4	w_f_i		! an entry point
	integer*4	w_f_ii		! an entry point
	integer*4	w_f_iii		! an entry point
	integer*4	w_f_iiii	! an entry point, 4 i's
!	integer*4	w_f_iiiilb	! an entry point, 4i, 1l, 1b
	integer*4	w_f_iiiib	! an entry point, 4i, 1b
	integer*4	w_f_iiilb	! an entry point, 3i, 1l, 1b
	integer*4	w_f_iiiiii	! an entry point, 6 i's
	integer*4	w_f_iiiiiib	! an entry point, 6 i's, 1 b
	integer*4	w_f_iiir	! an entry point
	integer*4	w_f_iiic	! an entry point
	integer*4	w_f_iiiibl	! an entry point
	integer*4	w_f_iiili	! an entry point
	integer*4	w_f_iiiclli	! an entry point
	integer*4	w_f_iiidli	! an entry point

	entry		w_f_iiiclli(fun, i1,i2,i3, c1, l1, l2, i4)
	w_f_iiiclli = fun(i1,i2,i3, c1, l1, l2, i4)
	return

	entry		w_f_iiidli(fun, i1,i2,i3, d1, l1, i4)
	w_f_iiidli = fun(i1,i2,i3, d1, l1, i4)
	return

	entry		w_f_i(fun,i1)
	w_f_i = fun(i1)
	return

	entry		w_f_ii(fun,i1,i2)
	w_f_ii = fun(i1, i2)
	return

	entry		w_f_iii(fun,i1,i2,i3)
	w_f_iii = fun(i1, i2, i3)
	return

	entry		w_f_iiii(fun,i1,i2,i3,i4)
	w_f_iiii = fun(i1, i2, i3, i4)
	return

	entry		w_f_iiiiii(fun,i1,i2,i3,i4,i5,i6)
	w_f_iiiiii = fun(i1, i2, i3, i4, i5, i6)
	return

	entry		w_f_iiiiiib(fun,i1,i2,i3,i4,i5,i6,b_1)
	w_f_iiiiiib = fun(i1, i2, i3, i4, i5, i6, b_1)
	return

	entry		w_f_iiir(fun,i1,i2,i3,r1)
	w_f_iiir = fun(i1,i2,i3,r1)
	return

	entry		w_f_iiic(fun,i1,i2,i3,c1)
	w_f_iiic = fun(i1,i2,i3,c1)
	return

	entry		w_f_iiiib(fun,i1,i2,i3,i4,b1)
	w_f_iiiib = fun(i1, i2, i3, i4, b1)
	return

	entry		w_f_iiiibl(fun,i1,i2,i3,i4,b1,l1)
	w_f_iiiibl = fun(i1, i2, i3, i4, b1, l1)
	return

	entry		w_f_iiilb(fun,i1,i2,i3,l1,b1)
	w_f_iiilb = fun(i1, i2, i3, l1, b1)
	return

	entry		w_f_iiili(fun,i1,i2,i3,l1,i4)
	w_f_iiili = fun(i1, i2, i3, l1, i4)
	return

	end
!------------------------------------------------------------------------------
	subroutine	w_show_wind_rec(r)
! Writes a formatted version of a binary wind_record to the terminal screen.
	implicit	none
	include		'parm_def.for'
	include		'wind_record_def.for'
	record /wind_record/ r
	integer*4	i

	integer*4	x

$IF ABSOFT_FORTRAN
	write(6,1,iostat=x) 'data', loc(r.data(0)), (r.data(i), i=0,15)
	write(6,2,iostat=x) 'MF', loc(r.major_frame), r.major_frame
	write(6,3,iostat=x) 'ert', loc(r.gathertime), 
	1	(r.gathertime.b(i), i=1,8)
	write(6,2,iostat=x) 'seq#', loc(r.sequence_number),
	1       r.sequence_number
	write(6,3,iostat=x) 'scet', loc(r.scet), 
	1	(r.scet.b(i), i=1,8)
	write(6,2,iostat=x) 'dpuMF', loc(r.dpu_major_frame),
	1       r.dpu_major_frame
$ELSE
	write(6,1,iostat=x) 'data', %loc(r.data(0)), (r.data(i), i=0,15)
	write(6,2,iostat=x) 'MF', %loc(r.major_frame), r.major_frame
	write(6,3,iostat=x) 'ert', %loc(r.gathertime), 
	1	(r.gathertime.b(i), i=1,8)
	write(6,2,iostat=x) 'seq#', %loc(r.sequence_number), r.sequence_number
	write(6,3,iostat=x) 'scet', %loc(r.scet), 
	1	(r.scet.b(i), i=1,8)
	write(6,2,iostat=x) 'dpuMF', %loc(r.dpu_major_frame), r.dpu_major_frame
$ENDIF

	return
  1	format(1x,a12,': ', z8.8, 16(1x,z2.2))
  2	format(1x,a12,': ', z8.8, i)
  3	format(1x,a12,': ', z8.8, 8(1x,z2.2))
	end
