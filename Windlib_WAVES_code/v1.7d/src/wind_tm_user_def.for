! wind_tm_user_def.for -- defines data structures and commons for variables
! describing/storing a user's selected options during a telemetry (disk file
! or real time) examination session.  Also includes event manipulating
! data structures.
!
! NOTE: tmdir:wind_record_def.for must be included in source prior to
! including this file.

	include		'parm_def.for'
	include		'low_byte_def.for'
	include		'wind_record_def.for'

	structure /wind_packet/
	   byte		w(0:size_of_packet)	! word array of 431+1 bytes
	end structure

!
! TM user
!
	structure /user_tm_profile/
	   integer*4	channel
	   integer*4	event_count
	   integer*4	major_frame
	   record /low_byte/	minor_frame
	   integer*4	first_major
	   record /low_byte/	first_minor
	   integer*4	last_major
	   record /low_byte/	last_minor
	   character	file*128
	   record	/wind_record/	wind_record
	   logical*4	suppress_messages
	   logical*4	all_tm_word_priv
	   logical*1	is_a_valid_ptr(0:255)
	   record	/wind_packet/ packet
	   integer*4	n_file_opens
	   integer*4	stream_type

	   ! function pointers
	   integer*4	f_get_word
	   integer*4	d0
	   integer*4	f_get_mf_scet
	   integer*4	d1
	   integer*4	f_get_mf_scet_dbms
	   integer*4	d2
	   integer*4	f_get_plain_packet
	   integer*4	d3
	   integer*4	f_close_ch
	   integer*4	d4
	   integer*4	f_goto_time
	   integer*4	d5
	   integer*4	f_get_intrnl_rec
	   integer*4	d6
	   integer*4	f_get_frame_rate
	   integer*4	d7
	   integer*4	f_get_xtra_event_info
	   integer*4	d8
	   integer*4	f_get_tm_mode
	   integer*4	d9
	   integer*4	f_get_hk
	   integer*4	d10
	   integer*4	f_get_dpu_mf
	   integer*4	d11
	   integer*4	f_get_rec_idx
	   integer*4	d12

!	   integer*4	f_get_mnr_frm
!	   integer*4	f_get_mjr_frm
!	   integer*4	f_get_rec
	end structure

	record 		/user_tm_profile/	user(max_channels)
	common		/wind_user_blk0/	user

! ensure that each process gets private data

$IF ABSOFT_FORTRAN
!
! This doesn't compile in Absoft:
! cdec$ psect /wind_user_blk0/ noshr
$ELSE
! Apparently, "cdec" overrides conditional compilation - so, when
! using Absoft, we still have to comment this out.
! cdec$ psect /wind_user_blk0/ noshr
$ENDIF

