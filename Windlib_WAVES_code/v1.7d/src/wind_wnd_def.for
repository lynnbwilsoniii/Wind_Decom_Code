! wind_wnd_def.for - data definitions for using WIND/WAVES .wnd files

$IF ABSOFT_FORTRAN
	integer*4	num_tm_words_selected
	integer*4	disk_record_length
	integer*4	wind_record_length
	parameter	(num_tm_words_selected=49)
	parameter	(disk_record_length=72)
	parameter	(wind_record_length=72)
$ELSE
	parameter	num_tm_words_selected=49
	parameter	disk_record_length=72
	parameter	wind_record_length=72
$ENDIF

	structure /wind_disk_record/
	   union
	      map
	         byte	data(0:num_tm_words_selected-1)	! selected words of mf
	      end map
	      map
	         byte	minor_frame	! minor frame number
	      end map
	   end union
	   byte		dpu_major_frame		! dpu major frame number
	   integer*4	major_frame		! ERT major frame number
	   record /vms_64bit_time/ gathertime	! eather receive time
	   record /vms_64bit_time/ scet		! spacecraft event time
	   byte 	sp_test_number		! script player test number
	   byte		sp_step_number		! script player step number
	end structure

	structure /byte_array_wnd_record/
	   byte		b(0:disk_record_length-1)
	end structure

	structure /aligned_wnd_record/
	   union
	      map
	         byte	data(0:num_tm_words_selected-1)	! selected words of mf
	      end map
	      map
	         byte	minor_frame	! minor frame number
	      end map
	   end union
	   byte		dpu_major_frame		! dpu major frame number
	   byte		sp_test_number		! script player test number
	   byte 	sp_step_number		! script player step number
	   integer*4	major_frame		! ERT major frame number
	   record /vms_64bit_time/ gathertime	! eather receive time
	   record /vms_64bit_time/ scet		! spacecraft event time
	end structure

	! The following structures are for the disk file header records.
	! This is the first header record; it contains misc file info
	structure /header_record_1/
	   union
	      map
	        byte		b(0:disk_record_length-1)
	      end map
	      map
	        character*16	version
	        character*10	date
	        character*10	time
	        integer*4	number_of_header_records
	        integer*4	number_of_data_records
	        integer*4	number_of_footer_records
		integer*4	byte_record_length
	        integer*4	first_major_frame_number
	        integer*4	first_minor_frame_number
		integer*4	last_major_frame_number
	        integer*4	last_minor_frame_number
	        logical*1	file_is_opened
	        logical*1	has_packet_pointers
	      end map
	   end union
	end structure

	! this is the second header record; it contains the minor frame address
	! table used to extract selected TM words from the stream
	structure /header_record_2/
	   union
	      map
	        byte		b(0:disk_record_length-1)
	      end map
	      map
	        byte		addr(0:disk_record_length-1)
	      end map
	   end union
	end structure

	! this is the third header record; it contains the pointer table
	! used to backlink a given disk record field address to the
	! minor frame address table used to extract selected TM words from
	! the stream
	structure /header_record_3/
	   union
	      map
	        byte		b(0:disk_record_length-1)
	      end map
	      map
	         byte		ptr(0:disk_record_length-1)
	      end map
	   end union
	end structure

	! this is the fourth header record; it contains
	! 1) the number of tm word pointers
	! 2) packed binary forward packet pointer addresses
	! 3) packed binary backward packet pointer addresses
	! 4) packed binary first packet pointers
	structure /header_record_4/
	   union
	      map
	        byte		b(0:disk_record_length-1)
	      end map
	      map
	        integer*2	num_tm_ptrs
	        byte		fpp(5)
	        byte		bpp(5)
	        integer*4	first_packet_of_type(12)
	      end map
	   end union
	end structure

	! this is the first footer record; it contains
	! 1) pointers to last packets of each packet type
	structure /footer_record_1/
	   union
	      map
	        byte		b(0:disk_record_length-1)
	      end map
	      map
	        integer*4	last_packet_of_type(12)
	      end map
	   end union
	end structure


	structure /users_wnd_info/
	   integer*4	ch
	   integer*4	lun
	   integer*4	recno
	   integer*4	next_record
	   integer*4	first_record
	   integer*4	last_record
	   character	file*128
	   integer*4	major_frame
	   record	/low_byte/	minor_frame
	   integer*4	first_major
	   record	/low_byte/	first_minor
	   integer*4	last_major
	   record	/low_byte/	last_minor
	   integer*4	tm_ptrs(0:sizeof_minor_frame-1)
	   integer*4	num_tm_ptrs
	   logical*1	is_a_valid_ptr(0:sizeof_minor_frame-1)
	   logical*4	all_tm_word_priv
	   integer*4	bckptrs(0:sizeof_minor_frame-1)
	   record	/aligned_wnd_record/ wdr
	end structure

	record /users_wnd_info/ wnd(max_channels)
	common /wind_user_blk4/ wnd
