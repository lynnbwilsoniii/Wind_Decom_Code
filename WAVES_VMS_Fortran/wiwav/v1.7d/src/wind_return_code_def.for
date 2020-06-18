! wind_return_code_def.for - return codes used by routines in WIND_TM_LIB.
! Note that with VAX VMS FORTRAN even numbered values are logically false
! and odd numbered values are logically true (low bit set).  So, errors
! are coded as even values so failures can be tested for logically in
! applications.
! 
$IF ABSOFT_FORTRAN
	integer*4	lib_negtime
	parameter	(lib_negtime		=-2)

	integer*4	w_failure, w_success
	integer*4	w_bad_channel, w_invalid_argument
	integer*4	w_no_realtime, w_no_free_channel
	integer*4	w_ungettable_record, w_no_realtime_mailbox

	parameter	(w_failure		= 0)
	parameter	(w_success		= 1)
	parameter	(w_bad_channel		= 2)

	parameter	(w_invalid_argument	= 4)
	parameter	(w_no_realtime		= 6)
	parameter	(w_no_free_channel	= 8)
	parameter	(w_ungettable_record	= 10)
	parameter	(w_no_realtime_mailbox	= 12)


	integer*4	w_bad_terminal_io, w_cntl_z

	parameter	(w_bad_terminal_io	= 20)
	parameter	(w_cntl_z		= 22)

	integer*4	w_ungettable_lun, w_bad_inquire
	integer*4	w_cannot_open_file

	parameter	(w_ungettable_lun	= 30)
	parameter	(w_bad_inquire		= 32)
	parameter	(w_cannot_open_file	= 34)

	integer*4	w_unopened_channel, w_bad_major_frame_number
	integer*4	w_bad_minor_frame_number

	parameter	(w_unopened_channel	= 40)
	parameter	(w_bad_major_frame_number= 42)
	parameter	(w_bad_minor_frame_number= 44)

	integer*4	w_cannot_get_file_size, w_tm_file_read_err
	integer*4	w_empty_tm_file

	parameter	(w_cannot_get_file_size	= 50)
	parameter	(w_tm_file_read_err	= 52)
	parameter	(w_empty_tm_file		= 54)

	integer*4	w_one_record_file, w_bad_frame_period
	integer*4	w_divide_by_zero, w_bad_header_record

	parameter	(w_one_record_file	= 72)
	parameter	(w_bad_frame_period	= 74)
	parameter	(w_divide_by_zero	= 76)
	parameter	(w_bad_header_record	= 78)

	integer*4	w_missing_record, w_end_of_file
	integer*4	w_beginning_of_file, w_bad_rec_quality_flag
	integer*4	w_no_record_waiting, w_bad_word_quality_flag

	parameter	(w_missing_record	= 80)
	parameter	(w_end_of_file		= 82)
	parameter	(w_beginning_of_file	= 84)
	parameter	(w_bad_rec_quality_flag	= 86)
	parameter	(w_no_record_waiting	= 88)
	parameter	(w_bad_word_quality_flag = 90)

	integer*4	w_unexpected_record, w_not_recording

	parameter	(w_unexpected_record	= 94)
	parameter	(w_not_recording		= 96)

	integer*4	w_bad_argument_value, w_word_not_collected

	parameter	(w_bad_argument_value	= 100)
	parameter	(w_word_not_collected	= 102)

	integer*4	w_bad_date_time_in_frame, w_bad_ascii_date_time
	integer*4	w_bad_64bit_date_time, w_bad_time_difference

	parameter	(w_bad_date_time_in_frame= 110)
	parameter	(w_bad_ascii_date_time	= 112)
	parameter	(w_bad_64bit_date_time	= 114)
	parameter	(w_bad_time_difference	= 116)

	integer*4	w_buffer_too_small

	parameter	(w_buffer_too_small	= 140)

	integer*4	w_bad_packet_id, w_bad_checksum
	integer*4	w_bad_multipacket_seq, w_bad_bit_size_arg
	integer*4	w_bad_bit_dest_arg, w_partial_hk_event
	integer*4	w_limited_success, w_no_dpu_mf_stream_synch

	parameter	(w_bad_packet_id		= 160)
	parameter	(w_bad_checksum		= 162)
	parameter	(w_bad_multipacket_seq   = 164)
	parameter	(w_bad_bit_size_arg	= 170)
	parameter	(w_bad_bit_dest_arg	= 172)
	parameter	(w_partial_hk_event	= 200)
	parameter	(w_limited_success	= 200)	! same on purpose
	parameter	(w_no_dpu_mf_stream_synch= 202)

! Return codes valued 300 to 499 are reserved for the use of dbms_item.for

	integer*4	w_dbms_open_error
	integer*4	w_dbms_input_error
	integer*4	w_dbms_too_many_dates
	integer*4	w_dbms_date_record_error
	integer*4	w_dbms_too_many_items
	integer*4	w_dbms_std_field_error
	integer*4	w_dbms_bit_field_error
	integer*4	w_dbms_vldtn_field_error
	integer*4	w_dbms_file_field_error
	integer*4	w_dbms_value_field_error
	integer*4	w_dbms_item_not_found
	integer*4	w_dbms_chain_field_error
	integer*4	w_dbms_count_field_error
	integer*4	w_dbms_invalid_subtype 
	integer*4	w_dbms_function_field_error

	parameter	(w_dbms_open_error	= 300)
	parameter	(w_dbms_input_error	= 302)
	parameter	(w_dbms_too_many_dates	= 304)
	parameter	(w_dbms_date_record_error= 306)
	parameter	(w_dbms_too_many_items	= 308)
	parameter	(w_dbms_std_field_error	= 310)
	parameter	(w_dbms_bit_field_error	= 312)
	parameter	(w_dbms_vldtn_field_error= 314)
	parameter	(w_dbms_file_field_error	= 316)
	parameter	(w_dbms_value_field_error= 318)
	parameter	(w_dbms_item_not_found	= 320)
	parameter	(w_dbms_chain_field_error= 322)
	parameter	(w_dbms_count_field_error= 324)
	parameter	(w_dbms_invalid_subtype  = 326)
	parameter	(w_dbms_function_field_error = 328)

	integer*4	w_dbms_item_file_open_error
	integer*4	w_dbms_key_file_open_error
	integer*4	w_dbms_key_not_found
	integer*4	w_dbms_item_file_read_error

	parameter	(w_dbms_item_file_open_error = 330)
	parameter	(w_dbms_key_file_open_error  = 332)
	parameter	(w_dbms_key_not_found        = 334)
	parameter	(w_dbms_item_file_read_error = 336)

! Return codes valued 500 to 699 are reserved for the use of get_item.c,
! but, are included here for completeness

	integer*4	w_ungettable_vldtn_item
	integer*4	w_item_validation_fail
	integer*4	w_bad_validation_op

	parameter	(w_ungettable_vldtn_item	= 500)
	parameter	(w_item_validation_fail	= 502)
	parameter	(w_bad_validation_op	= 504)

	integer*4	w_no_1st_header_for_item
	integer*4	w_no_2nd_header_for_item
	integer*4	w_no_3rd_header_for_item
	integer*4	w_no_idp_area_for_item
	integer*4	w_no_data_area_for_item
	integer*4	w_invalid_item_area

	parameter	(w_no_1st_header_for_item= 510)
	parameter	(w_no_2nd_header_for_item= 512)
	parameter	(w_no_3rd_header_for_item= 514)
	parameter	(w_no_idp_area_for_item	= 516)
	parameter	(w_no_data_area_for_item	= 518)
	parameter	(w_invalid_item_area	= 520)

	integer*4	w_bad_table_name
	integer*4	w_bad_table_file

	parameter	(w_bad_table_name	= 530)
	parameter	(w_bad_table_file   	= 532)

	integer*4	w_ungettable_cmpsit_item
	integer*4	w_ungettable_count_item
	integer*4	w_ungettable_function_item
	integer*4	w_item_name_not_found

	parameter	(w_ungettable_cmpsit_item= 540)
	parameter	(w_ungettable_count_item = 542)
	parameter	(w_ungettable_function_item=544)
	parameter	(w_item_name_not_found	= 546)


! return codes valued 800 to 899 are reserved for opening unnamed offline files
!	Error returns	700  SMG
!			702  error calling sys$setddir
!			704  no logical defined
!			706  no file selected

	integer*4	w_no_file_selected
	parameter	(w_no_file_selected	=706)

!			708  error calling lib$find_file (no logical defined)
!			710  error calling lib$find_file_end
!			712  error calling get_file_size.mar
!			714  unable to assign channel
!			716  unable to deassign channel
!			718  error calling str$trim
!
!			730  function down_one_dir
!			732  function back_one_dir
!			734  function get_suffix
!			736  function clear_list
!			738  function trim_size

! return codes valued 800 to 899 are reserved for item translate (xlate) stuff

	integer*4	w_corrupt_xlate_database
	integer*4	w_xlate_open_error
	integer*4	w_xlate_item_not_found

	parameter	(w_corrupt_xlate_database= 800)
	parameter	(w_xlate_open_error      = 802)
	parameter	(w_xlate_item_not_found  = 804)
$ELSE
	parameter	lib_negtime		=-2

	parameter	w_failure		= 0
	parameter	w_success		= 1
	parameter	w_bad_channel		= 2

	parameter	w_invalid_argument	= 4
	parameter	w_no_realtime		= 6
	parameter	w_no_free_channel	= 8
	parameter	w_ungettable_record	= 10
	parameter	w_no_realtime_mailbox	= 12


	parameter	w_bad_terminal_io	= 20
	parameter	w_cntl_z		= 22

	parameter	w_ungettable_lun	= 30
	parameter	w_bad_inquire		= 32
	parameter	w_cannot_open_file	= 34

	parameter	w_unopened_channel	= 40
	parameter	w_bad_major_frame_number= 42
	parameter	w_bad_minor_frame_number= 44

	parameter	w_cannot_get_file_size	= 50
	parameter	w_tm_file_read_err	= 52
	parameter	w_empty_tm_file		= 54

	parameter	w_one_record_file	= 72
	parameter	w_bad_frame_period	= 74
	parameter	w_divide_by_zero	= 76
	parameter	w_bad_header_record	= 78

	parameter	w_missing_record	= 80
	parameter	w_end_of_file		= 82
	parameter	w_beginning_of_file	= 84
	parameter	w_bad_rec_quality_flag	= 86
	parameter	w_no_record_waiting	= 88
	parameter	w_bad_word_quality_flag = 90

	parameter	w_unexpected_record	= 94
	parameter	w_not_recording		= 96

	parameter	w_bad_argument_value	= 100
	parameter	w_word_not_collected	= 102

	parameter	w_bad_date_time_in_frame= 110
	parameter	w_bad_ascii_date_time	= 112
	parameter	w_bad_64bit_date_time	= 114
	parameter	w_bad_time_difference	= 116

	parameter	w_buffer_too_small	= 140

	parameter	w_bad_packet_id		= 160
	parameter	w_bad_checksum		= 162
	parameter	w_bad_multipacket_seq   = 164
	parameter	w_bad_bit_size_arg	= 170
	parameter	w_bad_bit_dest_arg	= 172
	parameter	w_partial_hk_event	= 200
	parameter	w_limited_success	= 200	! same on purpose
	parameter	w_no_dpu_mf_stream_synch= 202

! Return codes valued 300 to 499 are reserved for the use of dbms_item.for

	parameter	w_dbms_open_error	= 300
	parameter	w_dbms_input_error	= 302
	parameter	w_dbms_too_many_dates	= 304
	parameter	w_dbms_date_record_error= 306
	parameter	w_dbms_too_many_items	= 308
	parameter	w_dbms_std_field_error	= 310
	parameter	w_dbms_bit_field_error	= 312
	parameter	w_dbms_vldtn_field_error= 314
	parameter	w_dbms_file_field_error	= 316
	parameter	w_dbms_value_field_error= 318
	parameter	w_dbms_item_not_found	= 320
	parameter	w_dbms_chain_field_error= 322
	parameter	w_dbms_count_field_error= 324
	parameter	w_dbms_invalid_subtype  = 326
	parameter	w_dbms_function_field_error = 328

	parameter	w_dbms_item_file_open_error = 330
	parameter	w_dbms_key_file_open_error  = 332
	parameter	w_dbms_key_not_found        = 334
	parameter	w_dbms_item_file_read_error = 336

! Return codes valued 500 to 699 are reserved for the use of get_item.c,
! but, are included here for completeness

	parameter	w_ungettable_vldtn_item	= 500
	parameter	w_item_validation_fail	= 502
	parameter	w_bad_validation_op	= 504

	parameter	w_no_1st_header_for_item= 510
	parameter	w_no_2nd_header_for_item= 512
	parameter	w_no_3rd_header_for_item= 514
	parameter	w_no_idp_area_for_item	= 516
	parameter	w_no_data_area_for_item	= 518
	parameter	w_invalid_item_area	= 520

	parameter	w_bad_table_name	= 530
	parameter	w_bad_table_file   	= 532

	parameter	w_ungettable_cmpsit_item= 540
	parameter	w_ungettable_count_item = 542
	parameter	w_ungettable_function_item=544
	parameter	w_item_name_not_found	= 546


! return codes valued 800 to 899 are reserved for opening unnamed offline files
c	Error returns	700  SMG
c			702  error calling sys$setddir
c			704  no logical defined
c			706  no file selected
	parameter	w_no_file_selected	=706
c			708  error calling lib$find_file (no logical defined)
c			710  error calling lib$find_file_end
c			712  error calling get_file_size.mar
c			714  unable to assign channel
c			716  unable to deassign channel
c			718  error calling str$trim
c
c			730  function down_one_dir
c			732  function back_one_dir
c			734  function get_suffix
c			736  function clear_list
c			738  function trim_size

! return codes valued 800 to 899 are reserved for item translate (xlate) stuff

	parameter	w_corrupt_xlate_database= 800
	parameter	w_xlate_open_error      = 802
	parameter	w_xlate_item_not_found  = 804
$ENDIF
