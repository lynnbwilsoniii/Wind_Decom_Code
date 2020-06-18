! wind_wnd_lib.for - routines for processing WIND/WAVES .wnd files

!------------------------------------------------------------------------------
! Initialize wind_lib internal data structures specific to working with .wnd
! format files.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_setup_stream(ch,f)
	implicit	none
	integer*4	ch
	character*(*)	f
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_wnd_def.for'
	integer*4	w_wnd_open_file
	integer*4	ok
	integer*4	i
	external	w_wnd_close_stream
	external	w_wnd_get_rec_xc
	external	w_wnd_frame_rate
	external	w_wnd_scet_of_mfmf
	external	w_wnd_scet_dbms_of_mfmf
	external	w_wnd_get_packet
	external	w_wnd_get_xtra_event_info
	external	w_wnd_get_rec_by_time
	external	w_wnd_get_word
	external	w_wnd_get_mode
	external	w_wnd_get_hk
	external	w_wnd_get_dpu_mf
	external	w_wnd_get_mfmf

	w_wnd_setup_stream = 0

	! these values must be non-zero: use dummy functions if necessary
$IF ABSOFT_FORTRAN
	user(ch).f_get_word            = loc(w_wnd_get_word)
	user(ch).f_get_intrnl_rec      = loc(w_wnd_get_rec_xc)
	user(ch).f_get_plain_packet    = loc(w_wnd_get_packet)
	user(ch).f_goto_time           = loc(w_wnd_get_rec_by_time)
	user(ch).f_get_mf_scet         = loc(w_wnd_scet_of_mfmf)
	user(ch).f_get_mf_scet_dbms    = loc(w_wnd_scet_dbms_of_mfmf)
	user(ch).f_get_frame_rate      = loc(w_wnd_frame_rate)
	user(ch).f_get_xtra_event_info = loc(w_wnd_get_xtra_event_info)
	user(ch).f_close_ch            = loc(w_wnd_close_stream)
	user(ch).f_get_tm_mode         = loc(w_wnd_get_mode)
	user(ch).f_get_hk              = loc(w_wnd_get_hk)
	user(ch).f_get_dpu_mf          = loc(w_wnd_get_dpu_mf)
	user(ch).f_get_rec_idx         = loc(w_wnd_get_mfmf)

!	user(ch).f_get_hk_data_array   = loc(w_wnd_get_hk_data_array)
$ELSE
	user(ch).f_get_word            = %loc(w_wnd_get_word)
	user(ch).f_get_intrnl_rec      = %loc(w_wnd_get_rec_xc)
	user(ch).f_get_plain_packet    = %loc(w_wnd_get_packet)
	user(ch).f_goto_time           = %loc(w_wnd_get_rec_by_time)
	user(ch).f_get_mf_scet         = %loc(w_wnd_scet_of_mfmf)
	user(ch).f_get_mf_scet_dbms    = %loc(w_wnd_scet_dbms_of_mfmf)
	user(ch).f_get_frame_rate      = %loc(w_wnd_frame_rate)
	user(ch).f_get_xtra_event_info = %loc(w_wnd_get_xtra_event_info)
	user(ch).f_close_ch            = %loc(w_wnd_close_stream)
	user(ch).f_get_tm_mode         = %loc(w_wnd_get_mode)
	user(ch).f_get_hk              = %loc(w_wnd_get_hk)
	user(ch).f_get_dpu_mf          = %loc(w_wnd_get_dpu_mf)
	user(ch).f_get_rec_idx         = %loc(w_wnd_get_mfmf)

!	user(ch).f_get_hk_data_array   = %loc(w_wnd_get_hk_data_array)
$ENDIF

	ok = w_wnd_open_file(ch,f)
	if (ok .ne. 1) return

	! assign user channel values
	user(ch).major_frame = wnd(ch).major_frame
	user(ch).first_major = wnd(ch).first_major
	user(ch).last_major  = wnd(ch).last_major
	user(ch).minor_frame = wnd(ch).minor_frame
	user(ch).first_minor = wnd(ch).first_minor
	user(ch).last_minor  = wnd(ch).last_minor
	call w_convert_wnd_disk_record(ch, wnd(ch).wdr, user(ch).wind_record)

	do i=0,sizeof_minor_frame-1
	   user(ch).is_a_valid_ptr(i) = wnd(ch).is_a_valid_ptr(i)
	end do 

	w_wnd_setup_stream= ok

	return
	end
!------------------------------------------------------------------------------
! Opens the disk file whose name has been passed via argument "file".
! Note that file must be a .wnd file (uses the ".WND" extension and
! is of the proper format specified in the FORTRAN open statement).
! "File" is copied into the user's data structure for the channel.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_open_file(ch,file)
	implicit	none
	integer*4	ch
	character	file*(*)
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	integer*4	ok
	integer*4	lib$get_lun
	integer*4	lib$free_lun
	integer*4	ios
	integer*4	ios2
	integer*4	i, j
	integer*4	w_wnd_get_rec
!	integer*4	wind_offline_file_is_active
	integer*4	w_wnd_last_record
	integer*4	w_rd_wnd_data_rec
	logical*4	wind_suppress_messages
	integer*4	wnd_to_ur8
	real*8		ur8
	record /header_record_1/ h1
	record /header_record_2/ h2
	record /header_record_3/ h3
	record /header_record_4/ h4
$IF ABSOFT_FORTRAN
	byte		tmpb1(disk_record_length)
$ENDIF

	w_wnd_open_file = 0

	wnd(ch).file = file

	ok = lib$get_lun(wnd(ch).lun)
	if (ok.ne.1) goto 30

$IF ABSOFT_FORTRAN
!
!  The Absoft "recl" specifier is in bytes, not 4-byte words.
!
!  The Absoft "read" does not compile when specifying the input
!  variable to be a "structure" (in this case:  h1), so use
!  an intermediate buffer ("tmpb1").
!
	open(wnd(ch).lun, file=wnd(ch).file,
	1	access='direct', 
	1	form='unformatted', recl=disk_record_length,
	1	readonly, shared,
	1	status='old', iostat=ios, err=40)
 
	! read in the header records for file processing information
	read(wnd(ch).lun, rec=1, iostat=ios, err=61) tmpb1
	do i=0,disk_record_length-1
	    h1.b(i) = tmpb1(i)
	end do 
$ELSE
	open(wnd(ch).lun, file=wnd(ch).file,
	1	access='direct', 
	1	form='unformatted', recl=disk_record_length/4,
	1	readonly, shared,
	1	status='old', iostat=ios, err=40)
 
	! read in the header records for file processing information
	read(wnd(ch).lun, rec=1, iostat=ios, err=61) h1
$ENDIF
	if (sunos) call convert_h1_wnd(h1)
	if (macos_ppc) call convert_h1_wnd(h1)

$IF ABSOFT_FORTRAN
!
!  The Absoft "read" does not compile when specifying the input
!  variables to be "structure"s (in this case:  h2, h3, h4), so use
!  an intermediate buffer ("tmpb1").
!
	read(wnd(ch).lun, rec=2, iostat=ios, err=62) tmpb1
	do i=0,disk_record_length-1
	    h2.b(i) = tmpb1(i)
	end do 

	read(wnd(ch).lun, rec=3, iostat=ios, err=63) tmpb1
	do i=0,disk_record_length-1
	    h3.b(i) = tmpb1(i)
	end do 

	read(wnd(ch).lun, rec=4, iostat=ios, err=64) tmpb1
	do i=0,disk_record_length-1
	    h4.b(i) = tmpb1(i)
	end do 
$ELSE
	read(wnd(ch).lun, rec=2, iostat=ios, err=62) h2
	read(wnd(ch).lun, rec=3, iostat=ios, err=63) h3
	read(wnd(ch).lun, rec=4, iostat=ios, err=64) h4
$ENDIF

	if (sunos) call convert_h4_wnd(h4)
	if (macos_ppc) call convert_h4_wnd(h4)
	ios = w_rd_wnd_data_rec(ch, 5)
	if (ios .ne. 0) goto 65
	wnd(ch).recno = 5

	wnd(ch).first_major       = wnd(ch).wdr.major_frame
	wnd(ch).first_minor.b     = wnd(ch).wdr.minor_frame
	wnd(ch).major_frame       = wnd(ch).wdr.major_frame
	wnd(ch).minor_frame.b     = wnd(ch).wdr.minor_frame
	wnd(ch).last_major        = h1.last_major_frame_number
	wnd(ch).last_minor.i4val  = h1.last_minor_frame_number
	wnd(ch).first_record      = h1.number_of_header_records+1
	wnd(ch).last_record       = h1.number_of_header_records +
	1				  h1.number_of_data_records
	wnd(ch).num_tm_ptrs	= h4.num_tm_ptrs
	do i=0,sizeof_minor_frame-1
	   wnd(ch).is_a_valid_ptr(i) = .false.
	   wnd(ch).bckptrs(i) = -1
	end do 
	do i=0,wnd(ch).num_tm_ptrs	! h2 is padded at end
	   wnd(ch).tm_ptrs(i) = zext(h2.addr(i))
	   j = wnd(ch).tm_ptrs(i)
	   wnd(ch).is_a_valid_ptr(j) = .true.
	   wnd(ch).bckptrs(j) = i
	end do

	!
	! set the various time/positioning earlier limits
	!
	ok = wnd_to_ur8(wnd(ch).wdr.scet, ur8)
	if (ok .ne. 1) then
	   write(6,1,iostat=ios) 'cannot set "earlier" time limits'
	end if
	exi(ch).ur8_position = ur8
	exi(ch).ur8_context  = ur8
	exi(ch).stream_boi   = ur8
	exi(ch).stream_bow   = ur8
	exi(ch).stream_bod   = ur8
	exi(ch).stream_eoy   = ur8

	if (wnd(ch).last_major .eq. -1) then
	      ! the offline file wasn't closed properly or it is bad,
	      ! ...we'll try to make a go of it...
	!     if (.not. wnd(ch).suppress_messages) then
	         type 1,
     1	      '.wnd file contains bad header records, improvising...'
	!     end if
	      ok = w_wnd_last_record(ch)
	      if (ok .ne.1) goto 72
	      if (wnd(ch).last_record .le. h1.number_of_header_records+1)
	1	goto 74
	end if

	!
	! set the various time/positioning later limits
	!
	ok = w_rd_wnd_data_rec(ch, wnd(ch).last_record)
	if (ok .ne. 0) then
	   write(6,1,iostat=ios) 'cannot get last record for later limits'
	end if
	ok = wnd_to_ur8(wnd(ch).wdr.scet, ur8)
	if (ok .ne. 1) then
	   write(6,1,iostat=ios) 'cannot set "later" time limits'
	end if
	exi(ch).stream_eoi   = ur8
	exi(ch).stream_eow   = ur8
	exi(ch).stream_eod   = ur8
	exi(ch).stream_bot   = ur8

	! read the first record from the file to establish the stream position
	wnd(ch).recno = 5
	wnd(ch).next_record = 6
	ok = w_wnd_get_rec(ch,
	1	wnd(ch).first_major,
	1	wnd(ch).first_minor)
	if (ok .ne.1) goto 80

	w_wnd_open_file  = 1

	return
  1	format(1x,'W_WND_OPEN_FILE: ',a, :, a, i3)
 30	w_wnd_open_file = w_ungettable_lun
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t get lun.'
	return
 40	ok = lib$free_lun(wnd(ch).lun)
	wnd(ch).lun = 0
	w_wnd_open_file = w_cannot_open_file
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t open file-- '
	write(6,1,iostat=ios2) wnd(ch).file(1:40), ' iostat=', ios
	return
! 50	w_wnd_open_file = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios2) 'Can''t determine last record in file--'
!	write(6,1,iostat=ios2)	wnd(ch).file(:40), ', iostat=', ios
!	return
 61	w_wnd_open_file = w_ungettable_record
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t read first header record from file--'
	write(6,1,iostat=ios2)	wnd(ch).file(:40), ' iostat=', ios
	return
 62	w_wnd_open_file = w_ungettable_record
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t read second header record from file--'
	write(6,1,iostat=ios2)	wnd(ch).file(:40), ' iostat=', ios
	return
 63	w_wnd_open_file = w_ungettable_record
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t read third header record from file--'
	write(6,1,iostat=ios2)	wnd(ch).file(:40), ' iostat=', ios
	return
 64	w_wnd_open_file = w_ungettable_record
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t read fourth header record from file--'
	write(6,1,iostat=ios2)	wnd(ch).file(:40), ' iostat=', ios
	return
 65	w_wnd_open_file = w_ungettable_record
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2)
     1	    'Can''t read first data record (rec=5) from file--'
	write(6,1,iostat=ios2)	wnd(ch).file(:40), ' iostat=', ios
	return
! 70	w_wnd_open_file = w_bad_header_record
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios2) 'Bad header record in disk file.'
!	return
 72	w_wnd_open_file = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'cannot determine last record in file: ',
	1	wnd(ch).file(:40)
	return
 74	w_wnd_open_file = w_empty_tm_file
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'no data records in offline file: ',
	1	wnd(ch).file(:40)
	return
 80	w_wnd_open_file = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'Can''t get first data record in file--'
	write(6,1,iostat=ios2)	wnd(ch).file(:40), ', status=', ok
	return
	end
!------------------------------------------------------------------------------
! Finds the last WIND record in a telemetry disk file.  A vax macro routine
! is called to return the file's allocation in blocks.  The last record
! becomes the "current" record for the channel .
!------------------------------------------------------------------------------
	integer*4       function	w_wnd_last_record(ch)
	implicit	none
	integer*4       ch	        ! index into the user's data structure
	integer*4       ios             ! i/o status indicater
	integer*4       block_file_size !$pragma C (block_file_size)
	integer*4       size            ! file size
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	include		'c_string_def.for'
$IF ABSOFT_FORTRAN
	integer*4       ner
	parameter       (ner=36)          ! VMS FORTRAN 'non-existent record' err
$ELSE
	parameter       ner=36          ! VMS FORTRAN 'non-existent record' err
$ENDIF
	integer*4       k2len           ! length-of-string function
	integer*4	w_rd_wnd_data_rec
	logical*4	wind_suppress_messages
	record /c_string_128/ c_file
	integer*4	i

	w_wnd_last_record = 0

	! get the file's allocation size in blocks
	c_file.c = wnd(ch).file
	i = k2len(c_file.c) + 1
	c_file.b(i) = 0
	size = block_file_size(c_file.b)
	if (size.le. 0) goto 10

	! determine the whole number of records that will fit in the allocation
	wnd(ch).recno = (size*512) / wind_record_length

	! The last record will be number recnum or something less
	ios = ner
	do while(ios .eq. ner .and. wnd(ch).recno .gt. 0)
	   ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	   if (ios .ne. 0) wnd(ch).recno = wnd(ch).recno - 1
	end do

	! check for errors
	if (ios.ne.0) goto 20
	if (wnd(ch).last_major .ne. wnd(ch).wdr.major_frame) then
	   ! this is an old file, the
	   ! last record--from truncate on put--is a dummy
	   wnd(ch).recno = wnd(ch).recno - 1
	end if
	if (wnd(ch).recno .lt. 1) goto 30

	! save the last record's frame numbers
	ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	wnd(ch).last_record  = wnd(ch).recno

	if (wnd(ch).last_major .le. 0) then
	   ! file was improperly closed, make a go of it...
	   wnd(ch).last_major   = wnd(ch).wdr.major_frame
	   wnd(ch).last_minor.b = wnd(ch).wdr.minor_frame
	else if (wnd(ch).last_major .ne. wnd(ch).wdr.major_frame) then
	   ! for some reason the header record's last_major isn't accurate
	   goto 40
	end if

	w_wnd_last_record = 1

	return
  1     format(1x,'W_WND_LAST_RECORD: ', a, z8.8)
  2     format(1x,'W_WND_LAST_RECORD: ', a, i3)
 10	w_wnd_last_record = w_cannot_get_file_size
	if (wind_suppress_messages(ch)) return
	type 1, 'Can''t get size of allocation, status: ', size
	return
 20	w_wnd_last_record = w_tm_file_read_err
	if (wind_suppress_messages(ch)) return
	type 2, 'Error reading file: '//wnd(ch).file(:k2len(wnd(ch).file)),ios
	return
 30	w_wnd_last_record = w_empty_tm_file
	if (wind_suppress_messages(ch)) return
	type 1, 'No records in file: '//wnd(ch).file(:k2len(wnd(ch).file))
	return
 40	w_wnd_last_record = w_empty_tm_file
	if (wind_suppress_messages(ch)) return
	type 1, 'Last MF not as described in header: '
	1	//wnd(ch).file(:k2len(wnd(ch).file))
	return
	end
!------------------------------------------------------------------------------
! Performs channel closing duties specific to .wnd format files and
! associated internal data elements.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_close_stream(ch)
	implicit	none
	integer*4	ch
$IF ABSOFT_FORTRAN
	integer*4	ok
	integer*4	lib$free_lun
$ENDIF
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	logical*4	opened
	integer*4	ios, ios2
	logical*4	wind_suppress_messages

	w_wnd_close_stream = 0

	inquire(wnd(ch).lun, err=10, opened=opened, iostat=ios)
	if (opened) then
	   close(wnd(ch).lun)
	end if
$IF ABSOFT_FORTRAN
	if (wnd(ch).lun .ne. 0) then
             ok = lib$free_lun(wnd(ch).lun)
	     wnd(ch).lun = 0
	endif
$ELSE
	if (wnd(ch).lun .ne. 0) call lib$free_lun(wnd(ch).lun)
$ENDIF
	wnd(ch).first_major = 0
	wnd(ch).last_major  = 0
	wnd(ch).recno       = 0
	wnd(ch).last_record = 0

	w_wnd_close_stream = 1

	return
  1	format('W_WND_CLOSE_STREAM: ', a, i3)
 10	continue
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios2) 'inquire error, iostat=',ios
	write(6,*) ' File is ', wnd(ch).file(:40)
	return
	end
!------------------------------------------------------------------------------
! This should be the only routine that reads a data record from a .wnd file.
! Header records are read only in the open channel/file routine. 
! Integer values written on the vax need to be rearranged
! a bit after reading on the sun.
!------------------------------------------------------------------------------
	integer*4	function	w_rd_wnd_data_rec(ch, recno)
	implicit	none
	integer*4	ch
	integer*4	recno
	integer*4	ios
	include		'wind_os_def.for'
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	integer*4	i,j,k
	structure /alignment_fun/
	 union
	  map
	   record /byte_array_wnd_record/ bwr
	  end map
	  map
	   record /aligned_wnd_record/ awr
	  end map
	  map
	   record /wind_disk_record/ xdr
	  end map
	 end union
	end structure
	record /alignment_fun/ af
	record /low_byte/ val

	read(wnd(ch).lun, rec=recno, iostat=ios) af.bwr
	wnd(ch).wdr = af.awr
	w_rd_wnd_data_rec = ios

	if (ios .ne. 0) return
	wnd(ch).wdr.sp_step_number = af.bwr.b(71)
	wnd(ch).wdr.sp_test_number = af.bwr.b(70)

	if (vms) then
	   val.b  = af.bwr.b(50)
	   val.b1 = af.bwr.b(51)
	   val.b2 = af.bwr.b(52)
	   val.b3 = af.bwr.b(53)
	   wnd(ch).wdr.major_frame = val.i4val
! changed for alpha alignment
!	   wnd(ch).wdr.major_frame = af.xdr.major_frame
!	   wnd(ch).wdr.gathertime  = af.xdr.gathertime
!	   wnd(ch).wdr.scet        = af.xdr.scet
	   j = 54
	   k = 62
	   do i=1,8
	      wnd(ch).wdr.gathertime.vmstime(i) = af.bwr.b(j)
	      j = j + 1
	      wnd(ch).wdr.scet.vmstime(i) = af.bwr.b(k)
	      k = k + 1
	   end do
	else if (sunos) then
	   call vx2sun(af.bwr.b(50), wnd(ch).wdr.major_frame, 4)
!	   call vx2sun(af.bwr.b(54), wnd(ch).wdr.gathertime, 8)
!	   call vx2sun(af.bwr.b(62), wnd(ch).wdr.scet, 8)
	   j = 54
	   k = 62
	   do i=1,8
	      wnd(ch).wdr.gathertime.vmstime(i) = af.bwr.b(j)
	      j = j + 1
	      wnd(ch).wdr.scet.vmstime(i) = af.bwr.b(k)
	      k = k + 1
	   end do
	else if (macos_ppc) then
	   call vx2sun(af.bwr.b(50), wnd(ch).wdr.major_frame, 4)
!	   call vx2sun(af.bwr.b(54), wnd(ch).wdr.gathertime, 8)
!	   call vx2sun(af.bwr.b(62), wnd(ch).wdr.scet, 8)
	   j = 54
	   k = 62
	   do i=1,8
	      wnd(ch).wdr.gathertime.vmstime(i) = af.bwr.b(j)
	      j = j + 1
	      wnd(ch).wdr.scet.vmstime(i) = af.bwr.b(k)
	      k = k + 1
	   end do
	else if (macos_intel) then
!	   call vx2sun(af.bwr.b(50), wnd(ch).wdr.major_frame, 4)
!	   call vx2sun(af.bwr.b(54), wnd(ch).wdr.gathertime, 8)
!	   call vx2sun(af.bwr.b(62), wnd(ch).wdr.scet, 8)
	   j = 54
	   k = 62
	   do i=1,8
	      wnd(ch).wdr.gathertime.vmstime(i) = af.bwr.b(j)
	      j = j + 1
	      wnd(ch).wdr.scet.vmstime(i) = af.bwr.b(k)
	      k = k + 1
	   end do
	end if
!xxxx	wnd(ch).next_record = wnd(ch).recno + 1 ...can't be right, jk, 26apr95
	wnd(ch).next_record = recno + 1

	return
	end
!------------------------------------------------------------------------------
! Reads a record from disk via direct access record number (recno).  Record
! is read into the user's data structure for the specified channel.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_rec_by_recno(ch,recno)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's structure
	integer*4	recno			! relative record number
	integer*4	ios			! FORTRAN iostat variable
$IF ABSOFT_FORTRAN
	integer*4	eof
	parameter	(eof=36)		! nonexistent record
$ELSE
	parameter	eof=36			! nonexistent record
$ENDIF
	integer*4	last_good
	integer*4	w_rd_wnd_data_rec

	if (recno .lt. 1) then
	   w_wnd_get_rec_by_recno = w_beginning_of_file
	   return
	end if

	last_good = wnd(ch).recno
	wnd(ch).recno = recno
	ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	if (ios .eq. 0) then
	   w_wnd_get_rec_by_recno = w_success
	else if (ios .eq. eof) then
	   w_wnd_get_rec_by_recno = w_end_of_file
	   ! reset position to eof
	   wnd(ch).recno = wnd(ch).last_record
	   ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	else
	   w_wnd_get_rec_by_recno = w_tm_file_read_err
	   ! restore position
	   wnd(ch).recno = last_good
	   ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	   return
	end if

	return
	end
!------------------------------------------------------------------------------
! Gets one TM word (byte) of data from specified major.minor frame.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_word
	1			(ch,major,minor,word,buf)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's structure
	integer*4	major			! major frame number
	record /low_byte/ minor			! minor frame number
	integer*4	word			! word in minor frame {0..255}
	byte		buf			! caller's buffer
	logical*4	wind_suppress_messages
	integer*4	ok
	integer*4	w_wnd_get_rec
	integer*4	ios

	w_wnd_get_word = 0
	buf = 0

	if (word .gt. 255 .or. word .lt. 0) goto 10

	ok = w_wnd_get_rec(ch,major,minor)
	if (ok.ne.1) goto 20
	buf = wnd(ch).wdr.data(wnd(ch).bckptrs(word))

	w_wnd_get_word = 1

	return
  1	format(1x,'W_WND_GET_WORD: ', a, i)
 10	continue
	w_wnd_get_word = w_invalid_argument
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'invalid word index: ', word
	return
 20	continue
	w_wnd_get_word = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'error getting record, ok= ', ok
	return
	end
!------------------------------------------------------------------------------
! Gets the sc TM mode and converts it to the equivalent cdhf value
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_mode
	1			(ch,major,minor,mode)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	include		'rt_mode_def.for'
	integer*4	ch			! index into user's structure
	integer*4	major			! major frame number
	integer*4	minor			! minor frame number
	integer*4	mode			! tm mode
	integer*4	imode
	integer*4	mode_byte
	integer*4	speed
	logical*4	wind_suppress_messages
	integer*4	ok
	integer*4	w_wnd_get_rec
	integer*4	ios
	integer*4	mnr
	integer*4	i
	integer*4	w_wnd_tm_speed		! an entry point
	logical*4	caller_wants_speed

	w_wnd_get_mode = 0

	mnr = minor
	i = mod(mnr,5)
	if (i .ne. 0) then
	   mnr = mnr - i
	end if

	ok = w_wnd_get_rec(ch,major,mnr)
	if (ok.ne.1) goto 10

	imode = zext(wnd(ch).wdr.data( wnd(ch).bckptrs(4) ))
$IF ABSOFT_FORTRAN
!
! Absoft errors on "imode = imode .and. '00f0'x, so use an
! intermediate variable
!
        i = '00f0'x
	imode = imode .and. i
$ELSE
	imode = imode .and. 'f0'x
$ENDIF
	caller_wants_speed = .false.
	goto 1000

	!----------------------------------------------------------------------
	entry	w_wnd_tm_speed(ch,major,mode_byte, speed)
$IF ABSOFT_FORTRAN
!
! Absoft errors on "imode = mode_byte .and. '00f0'x, so use an
! intermediate variable
!
        i = '00f0'x
	imode = mode_byte .and. i
$ELSE
	imode = mode_byte .and. 'f0'x
$ENDIF
	caller_wants_speed = .true.

 1000	continue

	if (imode .eq. s1) then
	   if (caller_wants_speed) then
	      speed = 1
	   else
	      mode = science_1x
	   end if
	else if (imode .eq. s2) then
	   if (caller_wants_speed) then
	      speed = 2
	   else
	      mode = science_2x
	   end if
	else if (imode .eq. m1) then
	   if (caller_wants_speed) then
	      speed = 1
	   else
	      mode = maneuver_1x
	   end if
	else if (imode .eq. m2) then
	   if (caller_wants_speed) then
	      speed = 2
	   else
	      mode = maneuver_2x
	   end if
	else if (imode .eq. c1) then
	   if (caller_wants_speed) then
	      speed = 1
	   else
	      mode = contingency_1x
	   end if
	else if (imode .eq. c2) then
	   if (caller_wants_speed) then
	      speed = 2
	   else
	      mode = contingency_2x
	   end if
	else
	   ! just pass on the funny value
	   if (.not. wind_suppress_messages(ch)) then
	      write(6,1,iostat=ios) 'unrecognized mode: ', imode
	   end if
	   if (caller_wants_speed) then
	      speed = 9
	   else
	      mode = imode
	   end if
	end if

	w_wnd_get_mode = 1

	return
  1	format(1x,'W_WND_GET_MODE: ', a, i)
 10	continue
	w_wnd_get_mode = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'error getting record, ok= ', ok
	return
	end
!------------------------------------------------------------------------------
! Reads specified record from sequential file opened for direct access.
! Major/minor frame numbers for the first and last records of the file
! are stored in the data structure for the channel.
!
! Disk record is copied to user's wind_tm_lib channel structure through entry
! w_wnd_get_rec_xc, otherwise disk record remains in local wind_wnd_lib buffer.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_rec_xc
	1				(ch,major,minor,wr)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's structure
	integer*4	major			! major frame number
	record /low_byte/ minor			! minor frame number
	record /wind_record/	wr		! main wind lib internal buffer

	logical*4	cpy_to_buf		! 
	record /low_byte/ work			! temporary minor frame number
	integer*4	ios			! FORTRAN read iostat variable
	integer*4	ok			! function return value
	integer*4	diff			! number of intervening records
	integer*4	wind_tm_delta_mfmf
	integer*4	k2len			! function returning string len
	integer*4	wind_check_parms
	integer*4	i,j		! loop index variable
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='W_WND_GET_REC')
$ELSE
	parameter	rn='W_WND_GET_REC'
$ENDIF
	integer*4	n_calls, n_reads
	integer*4	g_calls, g_reads
	integer*4	w_get_offline_rec_read_stats	! an entry point
	integer*4	last_good
	integer*4	section
	integer*4	w_wnd_file_binary_search
	integer*4	w_rd_wnd_data_rec
	logical*4	wind_suppress_messages
	integer*4	w_wnd_get_rec		! an entry point

	cpy_to_buf = .true.
	goto 800

	!---------------------------------------------------------------------
	entry	w_wnd_get_rec(ch,major,minor)
	cpy_to_buf = .false.

  800	continue

	w_wnd_get_rec = 0

	n_calls = n_calls + 1
	last_good = max(wnd(ch).next_record - 1, wnd(ch).first_record)
!	wnd(ch).last_rec_from_realtime = 0

	if (major .eq. wnd(ch).wdr.major_frame .and.
	1   minor.b .eq. wnd(ch).wdr.minor_frame) then
	   w_wnd_get_rec = 1
	   goto 1000
	end if

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	diff = -1

	! make sure frame numbers are within the file's range
	if (major .le. wnd(ch).first_major) then
	   if (major .lt. wnd(ch).first_major) goto 40
	   if (minor.i4val .lt. wnd(ch).first_minor.i4val) goto 40
	end if
	if (major .ge. wnd(ch).last_major) then
	   if (major .gt. wnd(ch).last_major) goto 50
	   if (minor.i4val .gt. wnd(ch).last_minor.i4val) goto 50
	end if

	! make sure the current record is current
	section = 0
	if (wnd(ch).recno .lt. wnd(ch).first_record) then
	   section = 1
	   wnd(ch).recno = wnd(ch).first_record
	   ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	   if (ios .ne. 0) goto 32
	   n_reads = n_reads + 1
	   last_good = wnd(ch).recno
	else if (wnd(ch).recno .gt. wnd(ch).last_record) then
	   section = 2
	   wnd(ch).recno = wnd(ch).last_record
	   ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	   if (ios .ne. 0) goto 32
	   n_reads = n_reads + 1
	   last_good = wnd(ch).recno
	else if ( (wnd(ch).next_record - 1) .ne. wnd(ch).recno) then
	   section = 3
	   ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	   if (ios .ne. 0) goto 32
	   n_reads = n_reads + 1
	   last_good = wnd(ch).recno
	end if
	work.b = wnd(ch).wdr.minor_frame

	! get the difference in frames
	ok = wind_tm_delta_mfmf(
	1	wnd(ch).wdr.major_frame,
	1	work,
	1	major,minor,diff)
	if (ok .ne. 1) goto 10

	! apply the difference, maintain bof/eof limits
	i = wnd(ch).recno + diff
	if (i.gt. wnd(ch).last_record) then
	   wnd(ch).recno = wnd(ch).last_record
	else if (i .lt. wnd(ch).first_record) then
	   wnd(ch).recno = wnd(ch).first_record
	else
	   wnd(ch).recno = i
	end if

	! get the target record
	section = 4
	ios = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	if (ios .ne. 0) goto 32
	n_reads = n_reads + 1
	last_good = wnd(ch).recno
	work.b = wnd(ch).wdr.minor_frame

	! get the new frame difference
	ok = wind_tm_delta_mfmf(
	1	wnd(ch).wdr.major_frame,
	1	work,
	1	major,minor,diff)
	if (ok .ne. 1) goto 20

	if (diff .eq. 0) then
	   ! frame numbers match, have found the desired record
	   w_wnd_get_rec = 1
	else
	   ! file has gaps, so we do a binary search
	   j = wnd(ch).recno
	   i = wnd(ch).recno + diff
	   if (i.gt. wnd(ch).last_record) then
	      i = wnd(ch).last_record
	   else if (i .lt. wnd(ch).first_record) then
	      i = wnd(ch).first_record
	   end if
	   ok = w_wnd_file_binary_search(ch,i,j,major,minor,n_reads)
	   if (ok .ne. 1) goto 70
	   w_wnd_get_rec = 1
	end if

 1000	continue

	! copy the disk record fields into the full-size internal record
	if (cpy_to_buf) then
	   call w_convert_wnd_disk_record(ch, wnd(ch).wdr, wr)
	end if

	return

 3	format(1x,'W_WND_GET_REC: ',a,:,i,a,i3,'.')
 4	format(1x,'W_WND_GET_REC: (',i1,') ',a,i,a,i3,'.')

  8	w_wnd_get_rec = ok
	return

 10	w_wnd_get_rec = ok
	if (wind_suppress_messages(ch)) return
	type 3, 'Invalid initial major/minor frame values.'
	return

 20	w_wnd_get_rec = ok
	if (wind_suppress_messages(ch)) return
	type 3, 'Invalid major/minor frame values for differencing.'
	return

! 20	w_wnd_get_rec = ok
!	if (wind_suppress_messages(ch)) return
!	type 3, 'Invalid major/minor frame values in record.'
!	type 3, 'Data format error in relative record number:', wnd(ch).recno
!	type 3, 'Record values are Major frame=', 
!	1	wnd(ch).wind_record.major_frame,
!	1	', Minor frame= ', work.i4val
!	type 3, 'Invalid record in file '//
!	1	wnd(ch).file(:k2len(wnd(ch).file))
!	return

 32	w_wnd_get_rec = w_tm_file_read_err
	i = wnd(ch).recno
	wnd(ch).recno = last_good
	j = w_rd_wnd_data_rec(ch, wnd(ch).recno)
	if (wind_suppress_messages(ch)) return
	type 4, section, 'Error reading record ', 
	1	i, ', iostat=',ios
	type 3, 'Error reading file: '//
	1	wnd(ch).file(:k2len(wnd(ch).file))
	return

 40	w_wnd_get_rec = w_beginning_of_file
	if (wind_suppress_messages(ch)) return
	type 3, 'Record is prior to BOF, major=',major, ', minor=', minor.i4val
	type 3, 'File '//wnd(ch).file(:k2len(wnd(ch).file))
	type 3, 'BOF is ', wnd(ch).first_major, '.', wnd(ch).first_minor.i4val
	return

 50	w_wnd_get_rec = w_end_of_file
	if (wind_suppress_messages(ch)) return
	type 3, 'Record is after EOF, major=',major, ', minor=', minor.i4val
	type 3, 'File '//wnd(ch).file(:k2len(wnd(ch).file))
	return

 70	w_wnd_get_rec = w_missing_record
	if (wind_suppress_messages(ch)) return
	type 3, 'Cannot find specified frame in file.'
	return

	!----------------------------------------------------------------------
	entry	w_get_offline_rec_read_stats(ch,g_calls,g_reads)
	w_get_offline_rec_read_stats = 1
	g_calls = n_calls
	g_reads = n_reads
	return
	end
!------------------------------------------------------------------------------
! Performs a binary search on channel ch's .wnd file to locate specified
! record.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_file_binary_search
	1				(ch,p1,p2,major,minor,n_reads)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's structure
	integer*4	p1,p2			! direct access record numbers
	integer*4	major			! major frame number
	integer*4	minor			! minor frame number
	integer*4	n_reads			! bookkeeping counter
	integer*4	ios			! FORTRAN read iostat variable
	integer*4	ok			! function return value
$IF ABSOFT_FORTRAN
	integer*4	ii
$ENDIF
	integer*4	diff			! number of intervening records
	integer*4	wind_tm_delta_mfmf
	integer*4	k2len			! function returning string len
	integer*4	w_rd_wnd_data_rec
	logical*4	wind_suppress_messages
	structure /binary_search_index/
	   integer*4	n
	   integer*4	mjr
	   integer*4	mnr
	end structure
	record /binary_search_index/ lo, hi, mid
	integer*4	xhi,xlo
	integer*4	midpoint
	midpoint(xlo,xhi) = max(xlo, ((xhi+xlo)/2) )

	w_wnd_file_binary_search = 0

	if (p1 .lt. p2) then
	   lo.n = p1
	   hi.n = p2
	else
	   lo.n = p2
	   hi.n = p1
	end if

	mid.n = midpoint(lo.n,hi.n)
	ios = w_rd_wnd_data_rec(ch, mid.n)
	if (ios .ne. 0) goto 10
	n_reads = n_reads + 1
	mid.mnr = zext(wnd(ch).wdr.minor_frame)
	mid.mjr = wnd(ch).wdr.major_frame
	ok = wind_tm_delta_mfmf(major,minor,mid.mjr,mid.mnr,diff)
	if (ok .ne. 1) goto 20

	do while (diff .ne. 0)
$IF ABSOFT_FORTRAN
!
!  2007/07/15:  the Absoft PwrPC Fortran compiler fails on
!
!     if (hi.n .le. lo.n) goto 60
!
!  So just use an intermediate variable to keep going.
!
	   ii = hi.n
	   if (ii .le. lo.n) goto 60
$ELSE
	   if (hi.n .le. lo.n) goto 60
$ENDIF
	   if (diff .lt. 0) then
	      lo.n = mid.n + 1
	   else
	      hi = mid
	   end if
	   mid.n = midpoint(lo.n,hi.n)
	   ios = w_rd_wnd_data_rec(ch, mid.n)
	   if (ios .ne. 0) goto 10
	   n_reads = n_reads + 1
	   mid.mnr = zext(wnd(ch).wdr.minor_frame)
	   mid.mjr = wnd(ch).wdr.major_frame
	   ok = wind_tm_delta_mfmf(major,minor,mid.mjr,mid.mnr,diff)
	   if (ok .ne. 1) goto 20
	end do

	w_wnd_file_binary_search = 1

	return
 3	format(1x,'W_WND_FILE_BINARY_SEARCH: ',a,i,a,i3,'.')

 10	w_wnd_file_binary_search = w_tm_file_read_err
	if (wind_suppress_messages(ch)) return
	type 3, 'Error reading record ', mid.n, ', iostat=',ios
	type 3, 'Error reading file: '//
	1	wnd(ch).file(:k2len(wnd(ch).file))
	return

 20	w_wnd_file_binary_search = ok
	if (wind_suppress_messages(ch)) return
	type 3, 'Error calling wind_tm_delta_mfmf.'
	return

 60	w_wnd_file_binary_search = w_missing_record
	if (wind_suppress_messages(ch)) return
	type 3, 'Missing record with major=',major, ', minor=', minor
	type 3, 'Missing record in file '//
	1	wnd(ch).file(:k2len(wnd(ch).file))
	return

	end
!------------------------------------------------------------------------------
! This routine takes a .WND file format disk record specified as a calling
! argument and converts it to
! the full size minor frame used internally by wind_lib.
!------------------------------------------------------------------------------
	integer*4	function	w_convert_wnd_disk_record(ch,wdr,wr)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	integer*4	ch		! user's channel number
	record /aligned_wnd_record/ wdr	! a wind disk record
	integer*4	i		! a loop index
	record /wind_record/ wr

	w_convert_wnd_disk_record = 1

	do i=0,wnd(ch).num_tm_ptrs-1
	   wr.data( wnd(ch).tm_ptrs(i) ) = wdr.data(i)
	end do
	wr.dpu_major_frame = zext(wdr.dpu_major_frame)
	wr.major_frame     = wdr.major_frame
	wr.gathertime      = wdr.gathertime
	wr.scet            = wdr.scet
	wr.quality	   = 1
	wr.sp_test_number  = wdr.sp_test_number
	wr.sp_step_number  = wdr.sp_step_number

	return
	end
!------------------------------------------------------------------------------
! Retrieves a record based on the earth received time or space craft event
! time..  The major and minor
! frame numbers of the existing record with the closest ert are returned.
! (If the closest record is a year from the specified ERT, that's what ya get.)
! Algorithm:
!	1. get ert of current record
!	2. calculate difference between ert of current record and target ert
!	3. calculate the bit rate at the time the current record was received
!	4. estimate the number of records between current record and target
!	5. read the estimated target record, make it the new current record
!	6. calculate difference between ert of current record and target ert
!	7. read rec by rec, either forwards or backwards, until time
!	   difference is minimized
!
! Also works for SCET based on value of by_ert argument:
!
!	by_ert = 1	- search by ert (any logical true value)
!	by_ert = 2	- search by scet (any logical false value)
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_rec_by_time
	1		(ch,arg_major,arg_minor,user_time,by_ert,cpy_to_buf,wr)

	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel id
	integer*4	arg_major	! major frame number
	record /low_byte/ arg_minor	! minor frame number
	character*(*)	user_time	! 23/24 character ERT or SCET
	logical*4	by_ert		! flag for search by ERT or SCET
	logical*4	cpy_to_buf	! 
	record /wind_record/ wr		! wind_tm_lib internal buffer

	integer*4	major		! major frame number
	record /low_byte/ minor		! minor frame number
	record /low_byte/ last_minor	! previous minor frame number
	integer*4	last_major	! major frame number
	integer*4	ok		! used for checking return status
	record /vms_64bit_time/ target_time ! VMS form of ert in argument list
	real*8		diff		! time difference in sec/100
	real*8		last_diff	! time difference in sec/100
	real*4		frame_rate	! Telemetry minor frame rate in seconds
	integer*4	num_recs	! number of records
	integer*4	minimized	! loop exit condition
	integer*4	reading_forwards! direction to go thru file
	integer*4	wind_ert_to_sys_time
	integer*4	w_wnd_get_rec
	integer*4	wind_delta_time
	integer*4	w_wnd_frame_rate
	integer*4	wind_tm_increment_mfmf
	integer*4	wind_tm_decrement_mfmf
	logical*4	wind_suppress_messages
	integer*4	addr
$IF ABSOFT_FORTRAN
	real*4		default_frame_rate, minimum_frame_rate
	parameter	(default_frame_rate=0.5)
	parameter	(minimum_frame_rate=0.01)
$ELSE
	parameter	default_frame_rate=0.5
	parameter	minimum_frame_rate=0.01
$ENDIF

	w_wnd_get_rec_by_time = 0
	arg_major = 0
	arg_minor.i4val = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

$IF ABSOFT_FORTRAN
	if (by_ert) then
	   addr = loc(wnd(ch).wdr.gathertime)
	else
	   addr = loc(wnd(ch).wdr.scet)
	end if
$ELSE
	if (by_ert) then
	   addr = %loc(wnd(ch).wdr.gathertime)
	else
	   addr = %loc(wnd(ch).wdr.scet)
	end if
$ENDIF

	! get the system form of the ascii user_time
	ok = wind_ert_to_sys_time(user_time,target_time)
	if (ok .ne. 1) goto 10

	! get the current major/minor frame numbers
	major = wnd(ch).wdr.major_frame
	minor.b = wnd(ch).wdr.minor_frame

	! get the time difference beteen current record and target time
	ok = wind_delta_time(%val(addr), target_time, diff)
	if (ok .ne. 1) goto 30

	! get the current frame rate (time between minor frames in seconds)
	ok = w_wnd_frame_rate(ch,major,minor,frame_rate)
	if (ok .ne. 1) goto 40
	if (frame_rate .lt. minimum_frame_rate) frame_rate = default_frame_rate

	! estimate the major/minor frame numbers of target record
	frame_rate = frame_rate * 100.0		  ! convert secs to secs/100
	num_recs = -jidnnt(diff/dble(frame_rate)) ! # of intervening records
	major = major + (num_recs/250)
	minor.i4val = minor.i4val + mod(num_recs,250)
	if (minor.i4val .lt. 0) then
	   major = major - 1
	   minor.i4val = minor.i4val + 250
	else if (minor.i4val .gt. 249) then
	   major = major + 1
	   minor.i4val = minor.i4val - 250
	end if

	! get estimated target record, it is the new current record
	ok = w_wnd_get_rec(ch,major,minor)
	if (ok .ne. 1) goto 50

	! get the time difference again, indicates relative position of target
	ok = wind_delta_time( %val(addr), target_time, diff)
	if (ok .ne. 1) goto 60

	! negative diff means means the target is later than current, so
	! we read forwards thru the file
	reading_forwards = diff .lt. 0.0

	! read records one at a time until the minimum time difference is
	! found, including end-of-file and beginning-of-file conditions
	last_diff = dabs(diff)
	last_major = major
	last_minor.b = minor.b
	minimized = 0
 	do while(.not. minimized)
	   if (reading_forwards) then
	      ok = wind_tm_increment_mfmf(major,minor)
	   else
	      ok = wind_tm_decrement_mfmf(major,minor)
	   end if
	   ok = w_wnd_get_rec(ch,major,minor)
	   if (ok .eq. w_end_of_file) then
	      minimized = 1
	   else if (ok .eq. w_beginning_of_file) then
	      minimized = 1
	   else if (ok .eq. w_missing_record) then
	      ! nothing, loop back
	   else if (ok .eq. w_success) then
	      ok = wind_delta_time( %val(addr), target_time, diff)
	      if (ok .ne. 1) goto 70
	      diff = dabs(diff)
	      minimized = last_diff .le. diff
	      if (.not. minimized) then
	         last_diff = diff
	         last_major = major
	         last_minor.b = minor.b
	      end if
	   else if (ok .ne. 1) then
	      goto 80
	   end if
	end do

	! get the previously read record, it is the closest to the target
	ok = w_wnd_get_rec(ch,last_major,last_minor)
	if (ok .ne. 1) goto 90

	! copy the disk record fields into the full-size internal record
	if (cpy_to_buf) then
	   call w_convert_wnd_disk_record(ch, wnd(ch).wdr, wr)
	end if

	! return the successful completion code and frame numbers
	w_wnd_get_rec_by_time = 1
	arg_major = last_major
	arg_minor.b = last_minor.b

	return
  1	format(1x,'W_WND_GET_REC_BY_TIME: ', a)

 08	w_wnd_get_rec_by_time = w_bad_channel
	if (wind_suppress_messages(ch)) return
	type 1, 'invalid channel number.'
	return

 10	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get system form of ERT: '
	return

 30	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get initial time difference between records.'
	return

 40	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get the bit rate.'
	return

 50	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get first target record.'
	return

 60	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get second time difference.'
	return

 70	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get delta time (in loop).'
	return

 80	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Bad return status from get_record.'
	return

 90	w_wnd_get_rec_by_time = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get target record (final).'
	return
	end
!------------------------------------------------------------------------------
! Determines the "frame rate" at ERT for a given minor frame of an offline file
! by subtracting the ERT of the given minor frame from the ERT of the following
! minor frame.  For realtime access, the frame rate value in the telemetry
! global section is returned.
! Handles the following special cases for offline files:
!	1.  Specified record is last record in file--frame rate determined
!	    from specified record and previous record.
!	2.  Missing records follow the specified record--the time between
!	    two successive physical records is divided by the number of
!	    records indicated by the major and minor frame counters.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_frame_rate
	1				(ch,major,minor,rate)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch			! index into user's data struc
	integer*4	major			! major frame number
	integer*4	minor			! minor frame number
	real*4		rate			! bit rate
	integer*4	ok			! for checking return statuses
	integer*4	recno			! a relative record number
	integer*4	major2			! another major frame number
	integer*4	diff			! # of intervening records
	record /low_byte/ minor2		! another minor frame number
	record /vms_64bit_time/ current_ert	! ert of specified record
	record /vms_64bit_time/ following_ert	! ert of adjacent record
	real*8		delta_time		! time diff between adjacents
	integer*4	w_wnd_get_rec
	integer*4	w_wnd_get_rec_by_recno
	integer*4	wind_delta_time
	integer*4	wind_tm_delta_mfmf
	logical*4	wind_suppress_messages
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='W_WND_FRAME_RATE')
$ELSE
	parameter	rn='W_WND_FRAME_RATE'
$ENDIF

	w_wnd_frame_rate = 0
	rate = 0.0

	! get the ert of the current record in sec/100
	ok = w_wnd_get_rec(ch, major, minor)
	if (ok .ne. 1) goto 10
	current_ert = wnd(ch).wdr.gathertime

	! get the ert of the next record
	ok = w_wnd_get_rec_by_recno(ch, wnd(ch).next_record)
	if (ok .eq. w_end_of_file) then
	   ! current record was last record in file, use the previous record
	   recno = wnd(ch).next_record - 2
	   if (recno .lt. 5) goto 30			! a one-data-record file
	   ok = w_wnd_get_rec_by_recno(ch,recno)
	   if (ok .ne. 1) goto 40
	else if (ok .ne. 1) then
	   goto 50
	end if
	following_ert = wnd(ch).wdr.gathertime

	! determine the adjustment for missing records
	major2 = wnd(ch).wdr.major_frame
	minor2.b = wnd(ch).wdr.minor_frame
	ok = wind_tm_delta_mfmf(major,minor,major2,minor2,diff)
	if (ok .ne. 1) goto 70

	! subtract the two ert's, result is in hundredth's of seconds (sec*100)
	ok = wind_delta_time(current_ert, following_ert, delta_time)

	! calculate the bit rate, divide by 100 to get fractional seconds
	delta_time = delta_time / 100.0
	if (diff .eq. 0) goto 80
	rate = sngl(delta_time) / float(diff)
	rate = abs(rate)

	w_wnd_frame_rate = 1

	return
  1	format(1x,'W_WND_FRAME_RATE: ', a)
 10	w_wnd_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get specified record.'
	return
 30	w_wnd_frame_rate = w_one_record_file
	if (wind_suppress_messages(ch)) return
	type 1, 'File has only one record.'
	return
 40	w_wnd_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get previous record.'
	return
 50	w_wnd_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot get next record.'
	return
! 60	w_wnd_frame_rate = ok
!	if (wind_suppress_messages(ch)) return
!	type 1, 'Cannot use ERT of next record.'
!	return
 70	w_wnd_frame_rate = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'Cannot determine elapsed records.'
	return
 80	w_wnd_frame_rate = w_divide_by_zero
	if (wind_suppress_messages(ch)) return
	type 1, 'Records have same frame numbers.'
	return
	end
!------------------------------------------------------------------------------
! Gathers a packet of data from a .wnd file or the realtime stream. 
! If argument "type" is not a valid
! packet type (between 0 and 15) the current packet, specified by major,minor
! frame number arguments, is selected.  Otherwise the type, direction, and
! seek_1st flag arguments are used to prescribe packet search and selection
! criteria.
! The user's major and minor arguments are advanced to point to the last
! frame of the selected packet on return from this routine.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_packet
	1			 (ch,major,minor,type,direction,seek_1st,pkt)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'

	integer*4	ch
	integer*4	major
	integer*4	minor
	integer*4	type
	integer*4	direction
	logical*4	seek_1st
	byte		pkt(0:*)

	integer*4	w_wnd_get_rec
	logical*4	wind_suppress_messages
	integer*4	i,j,k,n
	include		'wind_packet_addresses_def.for'
	integer*4	wind_tm_increment_mfmf
	integer*4	ok
	integer*4	mode
	integer*4	my_type
	integer*4	word
	integer*4	check

	logical*4	good_frame
	logical*4	any_bad_frame
	logical*4	science_mode	
	logical*4	maneuver_mode	
	logical*4	first_frame_ok
	logical*4	by_type

	w_wnd_get_packet = 0

	! adjust minor frame number to point to beginning of packet
	! --this could let major,minor indicate a nonexistent record,
	! --such as could happen at the beginning of a file
	if (mod(minor,10) .ne. 0) then
	   minor = minor - mod(minor,10)
	end if

	! This loop gets the first frame of the packet, searching forwards
	! or backwards through the data looking for a specific packet type
	! or a first packet of a specific event (packet_type=event_type, here)
	! as prescribed by the calling arguments.
	by_type = type .ge. min_packet_id .and. type .le. max_packet_id
	first_frame_ok = .false.
	do while(.not. first_frame_ok)
	   ok = w_wnd_get_rec(ch,major,minor)
	   if (ok.ne.1) goto 10
	   ! determine the mode: science or maneuver
	   mode = zext(wnd(ch).wdr.data( wnd(ch).bckptrs(4) ) )
$IF ABSOFT_FORTRAN
	   i = '00f0'x
	   mode = mode .and. i
$ELSE
	   mode = mode .and. 'f0'x
$ENDIF
	   science_mode = mode .eq. '00'x .or. mode .eq. '40'x
	   maneuver_mode = mode .eq. '10'x .or. mode .eq. '50'x
!	   if (.not. (science_mode .or. maneuver_mode)) science_mode = 1
	   if (.not. (science_mode .or. maneuver_mode)) goto 30
	   if (by_type) then
	      k = 1
	      if (maneuver_mode) k = 432
	      word = zext(wnd(ch).wdr.data( wnd(ch).bckptrs(p_addrs(k)) ) )
	      my_type = word / 16
	      first_frame_ok = my_type .eq. type
	      if (first_frame_ok .and. seek_1st) then
	         !first_frame_ok = word	! low bit set is 1st packet flag on vax
	         first_frame_ok = (word .and. 1) .ne. 0 ! sun fortran SC0.0
	      end if
	      if (.not. first_frame_ok) then
	         if (direction .eq. forward) then
	            call wind_tm_increment_packet(major,minor)
	         else if (direction .eq. reverse) then
	            call wind_tm_decrement_packet(major,minor)
	         else
	            goto 20
	         end if
	      end if
	   else
	      first_frame_ok = .true.
	   end if
	end do

	! now gather successive frames and packet words
	k = 1
	if (maneuver_mode) k = 432
	j = 0
	any_bad_frame = .false.
	good_frame = .true.
	do i=1,n_frames_per_packet
	   if (good_frame) then
	      ! copy the data words from the TM record
	      do n=1,n_words_frame(i)
	         pkt(j) = wnd(ch).wdr.data( wnd(ch).bckptrs(p_addrs(k)))
	         k = k + 1
	         j = j + 1
	      end do
	   else
	      ! set all the bits in each word in the storage area derived
	      ! from this invalid/missing frame
	      do n=1,n_words_frame(i)
	         pkt(j) = 'ff'x
	         k = k + 1
	         j = j + 1
	      end do
	   end if
	   if (i .lt. n_frames_per_packet) then
	      ok = wind_tm_increment_mfmf(major,minor)
	      ok = w_wnd_get_rec(ch,major,minor)
	      good_frame = ok .eq. 1
	      if (ok .eq. w_end_of_file) then
	         goto 40
	      else if (ok .ne. 1) then
	         ! return error code from the get_record routine
	         if (.not. any_bad_frame) w_wnd_get_packet = ok
	         any_bad_frame = 1
	         if (.not. wind_suppress_messages(ch)) then
	             type 2, i-1, 'bad frame in packet, MF.mf=', major,minor
	         end if
	      end if
	   end if
	end do

	if (.not. any_bad_frame) w_wnd_get_packet = 1

	call wind_plain_packet_sum(pkt,check)
	if (check .ne. 0) goto 50

	return
  1	format(1x,'W_WND_GET_PACKET: ',a, i8,'.',i3.3)
  2	format(1x,t1,'(',i2,')',1x,a,i8,'.',i3.3)
  3	format(1x,'W_WND_GET_PACKET: ',a, i)
  4	format(1x,'W_WND_GET_PACKET: ',a, z2.2)
 10	w_wnd_get_packet = ok
	if (wind_suppress_messages(ch)) return
	type 1, 'cannot get 1st frame of packet, MF.mf=', major,minor
	return
 20	w_wnd_get_packet = 0
	if (wind_suppress_messages(ch)) return
	type 3, 'invalid "direction" argument: ', direction
	return
 30	w_wnd_get_packet = 0
	if (wind_suppress_messages(ch)) return
	type 4, 'invalid SC mode from word #4: ', 
	1 wnd(ch).wdr.data( wnd(ch).bckptrs(4))
	return
 40	w_wnd_get_packet = w_end_of_file
	if (wind_suppress_messages(ch)) return
	type 1, 'EOF detected at ', major,minor
	return
 50	w_wnd_get_packet = w_bad_checksum
	if (wind_suppress_messages(ch)) return
	type 1, 'invalid checksum in packet at (major.minor): ',
     1    major, minor
	return
	end
!------------------------------------------------------------------------------
! This routine stores extra information related to the event for later
! retrieval by the user if desired.  Stored information includes:
!    1) major&minor frame numbers of the 1st frame of 1st packet of the event
!    2) Earth Receive Time of the 1st frame of 1st packet of the event in
!       DBMS format
!    3) DPU major frame number from HK enclosing 1st packet of the event
!    4) SCET of the event
!    5) ScriptPlayer TEST and STEP values
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_xtra_event_info
	1				(ch,major,minor)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor				! minor frame number
	integer*4	ok				! return status var
	integer*4	w_wnd_get_rec			! a function
	integer*4	wind_get_event_scet		! a function
	integer*4	wind_get_record			! a function
	integer*4	wnd_to_dbms			! a function
	integer*4	dbms_to_ur8
	logical*4	wind_suppress_messages
	record	/wind_record/	my_record
	integer*4	xmajor, xminor
	integer*4	w_get_dpu_mjr_fr_num		! a function
	integer*4	w_wnd_tm_speed			! a function
	integer*4	speed
	! xxxx it is probably bad to create an orred return code here...jk
	integer*4	return_code
$IF ABSOFT_FORTRAN
	integer*4	w_scet_err, w_scet_frctn_err
	integer*4	w_dpu_majf_err, w_ert_err, w_ert_frctn_err
	parameter	(w_scet_err	= '10'x)
	parameter	(w_scet_frctn_err= '20'x)
	parameter	(w_dpu_majf_err	= '40'x)
	parameter	(w_ert_err	= '100'x)
	parameter	(w_ert_frctn_err	= '200'x)
$ELSE
	parameter	w_scet_err	= '10'x
	parameter	w_scet_frctn_err= '20'x
	parameter	w_dpu_majf_err	= '40'x
	parameter	w_ert_err	= '100'x
	parameter	w_ert_frctn_err	= '200'x
$ENDIF
	integer*4	i
	integer*4	o

	w_wnd_get_xtra_event_info = 0
	return_code = 1

	! get the major and minor frame numbers of the beginning of the event
	exi(ch).major = major
	exi(ch).minor = minor - 9

	! establish beginning of event as the current record
	ok = wind_get_record(ch,major,exi(ch).minor,my_record)
	if (ok .ne. 1) goto 10

	! convert the ert time
	ok = wnd_to_dbms(my_record.gathertime, 
	1	exi(ch).ert(1),
	1	exi(ch).ert(2),
	1	i)
	if (ok .ne. 1) goto 20
	exi(ch).ert1000 = i

	! get the script player's test and step number
	exi(ch).sp_test_number = wnd(ch).wdr.sp_test_number
	exi(ch).sp_step_number = wnd(ch).wdr.sp_step_number

	xmajor = major
	xminor = minor
	ok = w_get_dpu_mjr_fr_num(ch,xmajor,xminor,exi(ch).dpu_major_ert)
	if (ok .ne. 1) return_code = return_code .or. w_dpu_majf_err

	! get the bit_rate, put in event buffer
	ok = w_wnd_tm_speed(ch,xmajor,zext(my_record.data(4)),speed)
	if (speed .eq. 1) then
	   exi(ch).bit_rate = 0
	else if (speed .eq. 2) then
	   exi(ch).bit_rate = 1
	else
	   exi(ch).bit_rate = 9
	end if

	! get the spacecraft event time
	xmajor = major
	xminor = minor
	ok = wind_get_event_scet(ch,xmajor,xminor)
	if (ok .ne. 1) then
	   return_code = return_code .or. w_scet_err
	   ! use the ert to grossly estimate the scet
	   exi(ch).scet(1) = exi(ch).ert(1)
	   exi(ch).scet(2) = exi(ch).ert(2)
	end if
	ok = dbms_to_ur8(exi(ch).scet(1), exi(ch).scet(2), exi(ch).scet1000,
	1  exi(ch).ur8_scet)
	if (ok .ne. 1) goto 30
	exi(ch).ur8_context = exi(ch).ur8_scet

	! restore user's stream position to end of first packet of event
	ok = w_wnd_get_rec(ch,major,minor)
	if (ok .ne. 1) goto 80

	w_wnd_get_xtra_event_info = return_code

	if ( (.not. wind_suppress_messages(ch)) .and. (.not.return_code)) then
	   if ((return_code .and. w_scet_err) .ne. 0) then
	      write(6,1,iostat=o)'cannot get event SCET.'
	   end if
	   if ((return_code .and. w_ert_frctn_err) .ne. 0) then
	      write(6,1,iostat=o)'cannot get fractional ERT.'
	   end if
	   if ((return_code .and. w_dpu_majf_err) .ne. 0) then
	      write(6,1,iostat=o)'cannot get DPU MF.'
	   end if
	end if

	return
 1	format(1x,'W_WND_GET_XTRA_EVENT_INFO: ', a,a)

 10	w_wnd_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o)'cannot get record for ERT.'
	return

 20	w_wnd_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o)'cannot convert ERT to DBMS format time.'
	return

 30	w_wnd_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o) 'cannot convert DBMS SCET to UR8.'
	return

 80	w_wnd_get_xtra_event_info = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=o)'cannot get record for position restoration.'
	return
	end
!------------------------------------------------------------------------------
! Returns the SCET of the specified frame.
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_scet_of_mfmf(ch,major,minor,scet)
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_wnd_def.for'
	integer*4	ch
	integer*4	major
	integer*4	minor
	record /vms_64bit_time/ scet
	integer*4	ok
	integer*4	w_wnd_get_rec
	integer*4	wind_suppress_messages
	integer*4	ios
	integer*4	wnd_to_dbms			! a function
	integer*4	ymd,hms,s1000
	integer*4	w_wnd_scet_dbms_of_mfmf
	logical*4	do_dbms_cnvrt

	do_dbms_cnvrt = .false.
	goto 1000

	!----------------------------------------------------------------------
	entry		w_wnd_scet_dbms_of_mfmf(ch,major,minor,ymd,hms,s1000)
	do_dbms_cnvrt = .true.

 1000	continue
	w_wnd_scet_of_mfmf = 0

	ok = w_wnd_get_rec(ch,major,minor)
	if (ok .ne. 1) goto 10

	if (do_dbms_cnvrt) then
	   ok = wnd_to_dbms(wnd(ch).wdr.scet, ymd,hms,s1000)
	   w_wnd_scet_dbms_of_mfmf = ok
	   return
	else
	   scet = wnd(ch).wdr.scet
	end if

	w_wnd_scet_of_mfmf = 1

	return
  1	format(1x,'W_WND_SCET_OF_MFMF: ', a)
 10	continue
	w_wnd_scet_of_mfmf = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'cannot get specified .WND record.'
	return
	end
!------------------------------------------------------------------------------
! Converts the little-end-first integer elements of a .wnd file's first
! record to big-end-first format.  First used for reading .wnd files 
! under SunOS.
!------------------------------------------------------------------------------
	integer*4	function	convert_h1_wnd(h1)
	implicit	none
	include	'parm_def.for'
	include 'low_byte_def.for'
	include 'wind_record_def.for'
	include	'wind_wnd_def.for'
	record /header_record_1/ h1
	integer*4	vxtosuni4

	convert_h1_wnd = 1

	h1.number_of_header_records = vxtosuni4(h1.number_of_header_records)
	h1.number_of_data_records   = vxtosuni4(h1.number_of_data_records)
	h1.number_of_footer_records = vxtosuni4(h1.number_of_footer_records)
	h1.byte_record_length       = vxtosuni4(h1.byte_record_length)
	h1.first_major_frame_number = vxtosuni4(h1.first_major_frame_number)
	h1.first_minor_frame_number = vxtosuni4(h1.first_minor_frame_number)
	h1.last_major_frame_number  = vxtosuni4(h1.last_major_frame_number)
	h1.last_minor_frame_number  = vxtosuni4(h1.last_minor_frame_number)

	return
	end
!------------------------------------------------------------------------------
! Converts the little-end-first integer elements of a .wnd file's fourth
! record to big-end-first format.  First used for reading .wnd files 
! under SunOS.
!---------------------------------------------------------------------------
	integer*4	function	convert_h4_wnd(h4)
	implicit	none
	include	'parm_def.for'
	include 'low_byte_def.for'
	include 'wind_record_def.for'
	include	'wind_wnd_def.for'
	record /header_record_4/ h4
	integer*2	vxtosuni2

	convert_h4_wnd = 1

	h4.num_tm_ptrs = vxtosuni2(h4.num_tm_ptrs)

	return
	end
!------------------------------------------------------------------------------
! Returns one to N (N=w2-w1+1) bytes of WND TM HK data to caller.
! This routine returns the original (more or less) 81 HK words, 25
! packet id words, 125 bytes of minor frame quality (always zero,
! but provided for cdhf compatability in form), 1 byte counting the
! number of minor frames with fill (provided for cdhf compatablility in
! form, though the number of errors encountered gathering the first 106
! hk words), 1 byte containing the number of minor frames with synch err
! (same value as number of minor frames with fill).
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_hk(ch,mjr,w1,w2,buf)
	implicit	none
	include		'wind_os_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'wind_wnd_def.for'
	include		'wind_hk_addr_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! caller's channel number
	integer*4	mjr		! major frame number
	integer*4	w1		! first/lowest hk word number
	integer*4	w2		! second/highest hk word number
	byte		buf(*)		! caller's buffer to receive word(s)
	integer*4	ok
	integer*4	w_wnd_get_word
	integer*4	w_wnd_get_mode
	logical*4	wind_suppress_messages	! a function
	integer*4	ios
	integer*4	mnr
	integer*4	mode
	integer*4	i,j,k
	record /low_byte/ lb
	integer*4	n_err

	w_wnd_get_hk = 0

	j = 0
	n_err = 0
	do i=w1,w2
	   j = j + 1
	   buf(j) = 0
	   if (i .ge. w_first_core_hk_word .and. 		! reg. hk
	1      i .le. w_last_core_hk_word) then
	      ok = w_wnd_get_word(ch, mjr,
	1	   hk_mf_addr(i),		! minor frame number
	1	   hk_word_addr(i),		! word number in minor frame
	1	   buf(j))			! returned word
	      if (ok .ne. 1) n_err = n_err + 1
	   else if (i .ge. w_first_pktid_hk_word .and. 		! pkt id's
	1           i .le. w_last_pktid_hk_word) then
	      mnr = (i - w_first_pktid_hk_word) * 10
	      ok = w_wnd_get_mode(ch,mjr,mnr,mode)
	      if (ok .eq. 1) then
	         if (mode .eq. science_1x .or. 
	1            mode .eq. science_2x .or.
	1            mode .eq. contingency_1x .or. 
	1            mode .eq. contingency_2x) then
	             k = 24
	         else if (mode .eq. maneuver_1x .or.
	1              mode .eq. maneuver_2x) then
	             k = 32
	         else
	             goto 35
	         end if
	         ok = w_wnd_get_word(ch, mjr, mnr, k, buf(j))
	         if (ok .ne. 1) n_err = n_err + 1
	      else
	         n_err = n_err + 1
	      end if
	   else if (i .eq. w_sc_mode_hk_word) then
	      ok = w_wnd_get_mode(ch,mjr,mnr,lb.i4val)
	      if (ok .ne. 1) write(6,1,iostat=ios)
	1	    'cannot get HK sc mode word.'
	      buf(j) = lb.b
	   else if (i .ge. w_first_mfq_hk_word .and. 
	1           i .le. w_last_mfq_hk_word) then
	      buf(j) = 0
	   else if (i .eq. w_n_mf_w_fill_hk_word) then
	      lb.i4val = n_err
	      buf(j) = lb.b
	   else if (i .eq. w_n_mf_w_synch_err_hk_word) then
	      lb.i4val = n_err
	      buf(j) = lb.b
	   else
	      goto 13
	   end if
	end do

	if (n_err .ne. 0) then
	   write(6,4,iostat=ios) n_err, ' words with errors.'
	   if (n_err .ge. 106) return ! return err if all errors
	   w_wnd_get_hk = w_partial_hk_event
	   return
	end if

	w_wnd_get_hk = 1

	return
   1	format(1x,'W_WND_GET_HK: ', a, :, i)
   4	format(1x,'W_WND_GET_HK: ', i3, a)
! 10	continue
!	w_wnd_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot goto stream position'
!	return
 13	w_wnd_get_hk = w_bad_argument_value
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 'bad house keeping index.'
	return
! 20	w_wnd_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 'cannot get specified core housekeeping word.'
!	return
! 30	w_wnd_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 
!	1 'cannot get sc mode for "packet id" housekeeping word.'
!	return
 35	w_wnd_get_hk = ok
	if (wind_suppress_messages(ch)) return
	write(6,1,iostat=ios) 
	1 'invalid sc mode for "packet id" housekeeping word.', mode
	return
! 38	w_wnd_get_hk = ok
!	if (wind_suppress_messages(ch)) return
!	write(6,1,iostat=ios) 
!	1 'cannot get specified "packet id" housekeeping word.'
!	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_dpu_mf(ch,major,minor,dpu)
! Extracts the dpu major frame number from the stream at a given position.
! Attempts to estimate the value at EOF and BOF.  Restores the caller
! to the stream position at entry.
	implicit	none
	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'
	include		'wind_wnd_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	major				! major frame number
	integer*4	minor
	integer*4	dpu				! dpu major frame #
	integer*4	ok				! return status var
	integer*4	w_wnd_get_rec_xc			! a function
	record	/low_byte/	lw
	record	/wind_record/	my_record
	integer*4	omajor,ominor
	integer*4	error_count
	integer*4	wind_get_message_state		! a function
	integer*4	wind_set_message_state		! a function

	logical*4	state
	logical*4	subtract
	logical*4	add

	lw.i4val = 0
	add      = .false.
	subtract = .false.
!	omajor   = wnd(ch).major_frame
!	ominor   = wnd(ch).minor_frame.i4val
	omajor   = major
	ominor   = minor
	error_count = 0
	ok = wind_get_message_state(ch, state)
	ok = wind_set_message_state(ch, .true.)

	w_wnd_get_dpu_mf = 0

!	type *, 'WND_DPU: MF.mf: ', omajor, ominor

 1000	continue
	if (error_count .gt. 1) goto 2000

	ok = w_wnd_get_rec_xc(ch,omajor,44,my_record)
	if (ok .eq. w_beginning_of_file) then
	   subtract = .true.
	   omajor   = omajor + 1
	   error_count = error_count + 1
	   goto 1000
	else if (ok .eq. w_end_of_file) then
	   add      = .true.
	   omajor   = omajor - 1
	   error_count = error_count + 1
	   goto 1000
	else if (ok .ne. 1) then
	   goto 114
	end if
	lw.b =  my_record.data(18)
	lw.b1 = my_record.data(17)

	ok = w_wnd_get_rec_xc(ch,omajor,114,my_record)
	if (ok .eq. w_end_of_file) then
	   add      = .true.
	   omajor   = omajor - 1
	   error_count = error_count + 1
	   goto 1000
	else if (ok .ne. 1) then
	   goto 116
	end if
	lw.b2 = my_record.data(17)

!	type '(1x,a,i2,4x,4(2x,z2.2))', 'WND_DPU: err_cnt, b,b1,b2,b3: ',
!	1	error_count, lw.b, lw.b1, lw.b2, lw.b3

 2000	continue
	if (error_count .gt. 1) then
	   w_wnd_get_dpu_mf = 0
	else if (error_count .eq. 1) then
	   if (subtract) then
	      lw.i4val = lw.i4val - 1
	   else
	      lw.i4val = lw.i4val + 1
	   end if
	   dpu = lw.i4val
	   w_wnd_get_dpu_mf = 1
	else
	   dpu = lw.i4val
	   w_wnd_get_dpu_mf = 1
	end if

	! "restore" caller's stream position (for event/packet oriented
	! streams this is more of a synchronization for internal pointers)
	ok = w_wnd_get_rec_xc(ch,major,minor,my_record)
	if (ok .ne. 1) goto 120

	ok = wind_set_message_state(ch, state)

	return
  1	format(1x,'W_WND_GET_DPU_MF: ',a,:,i,'.',i3.3)
 114	w_wnd_get_dpu_mf = ok
	ok = wind_set_message_state(ch, state)
	if (state) return
	type 1, 'cannot get wind record for HK DPU at', omajor,44
	return

! 115	w_wnd_get_dpu_mf = ok
!	ok = wind_set_message_state(ch, state)
!	if (state) return
!	type 1, 'cannot get wind record for HK DPU at', omajor,119
!	return

 116	w_wnd_get_dpu_mf = ok
	ok = wind_set_message_state(ch, state)
	if (state) return
	type 1, 'cannot get wind record for HK DPU at', omajor,114
	return

 120	w_wnd_get_dpu_mf = ok
	ok = wind_set_message_state(ch, state)
	if (state) return
	type 1, 'cannot restore stream position to ', major, minor
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_wnd_get_mfmf(ch,major,minor,tk)
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	wind_tm_increment_mfmf
	integer*4	ch
	integer*4	major
	record		/low_byte/ minor
	integer*4	tk
	integer*4	ok
	logical*4	what
	integer*4	mjr
	integer*4	mnr
	integer*4	ios
	byte		buf
	integer*4	w_f_iiiibl
	record /wind_record/ save_rec

	w_wnd_get_mfmf = 1
	major = 0
	minor.i4val = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	if (tk .eq. w_tk_stream_mfmf) then
	   ! the current record's maj/min value in the file
	   major   = user(ch).wind_record.major_frame
	   minor.b = user(ch).wind_record.minor_frame
	else if (tk .eq. w_tk_next_mfmf) then
	   save_rec = user(ch).wind_record
	   mjr = user(ch).wind_record.major_frame
	   mnr = zext(user(ch).wind_record.minor_frame)
	   what = user(ch).suppress_messages
	   user(ch).suppress_messages = .true.
	   ok = w_end_of_file + 77
	   do while(ok .ne. 1 .and. ok .ne. w_end_of_file)
	      ok = wind_tm_increment_mfmf(mjr,mnr)
	      ok = w_f_iiiibl( 
	1	   %val(user(ch).f_get_word),
	1	   ch, mjr, mnr, safe_word, buf, .false.)
	   end do
	   user(ch).suppress_messages = what
	   if (ok .eq. 1) then
	      major = mjr
	      minor.i4val = mnr
	   endif
	   user(ch).wind_record = save_rec
	   if (ok .ne. 1) goto 20
	else if (tk .eq. w_tk_earliest_mfmf) then
	   major = user(ch).first_major
	   minor = user(ch).first_minor
	else if (tk .eq. w_tk_latest_mfmf) then
	   major = user(ch).last_major
	   minor = user(ch).last_minor
	else if (tk .eq. w_tk_current_mfmf) then
	   major = user(ch).wind_record.major_frame
	   minor.i4val = zext(user(ch).wind_record.minor_frame)
	else
	   goto 40
	end if

	return
  1	format(1x,'W_WND_GET_MFMF: ',a,i)
  2	format(1x,'W_WND_GET_MFMF: ',a,i9,'.',i3.3)
  8	w_wnd_get_mfmf = w_bad_channel
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number=', ch
	return
 20	w_wnd_get_mfmf = ok
	if (user(ch).suppress_messages) return
	write(6,2,iostat=ios) 
	1 'Cannot get next stream position (MF.mf)', mjr, mnr
	return
 40	w_wnd_get_mfmf = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'invalid MF.mf position token.', tk
	return
	end
