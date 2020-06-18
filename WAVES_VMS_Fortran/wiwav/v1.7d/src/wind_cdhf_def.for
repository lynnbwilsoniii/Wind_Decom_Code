! wind_cdhf_def.for - 

!	include		'low_byte_def.for'
!	include		'wind_os_def.for'
!	include		'param_def.for'
	include		'wind_cdhf_rec_def.for'

! user's channel data structure

	structure /users_cdhf_info/
	   integer*4	ch
	   integer*4	lun
	   integer*4	lrecl
	   integer*4	recno
	   integer*4	first_record
	   integer*4	last_record
	   integer*4	major_frame
	   integer*4	first_major
	   integer*4	last_major
	   record /low_byte/       minor_frame
	   record /low_byte/       first_minor 
	   record /low_byte/       last_minor 
	   record /l0_data_file_label_h/ hdr
	   record /l0_data_record/ lzr
	   character*256   file
	   logical*4	transcend_file
	   integer*4	file_number
	   logical*4	verbose_rollover
	   real*8	t1, t2		! current file limits
	   real*8	ut1, ut2	! original user specified times
	   logical*4	reopen_in_progress
	   logical*4	is_nrt
	end structure
	record /users_cdhf_info/ cdhf(max_channels)

	common /wind_user_blk6/ cdhf
