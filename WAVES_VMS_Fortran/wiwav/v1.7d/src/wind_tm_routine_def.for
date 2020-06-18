! wind_tm_routine_def.for - include file for application programs referencing WIND_TM_
! routines.  This file is meant to be "included" into the application's FORTRAN source
! via the FORTRAN "include" statement:
!
!	include	'wind_examples:wind_tm_routine_def.for/nolist'
!
	integer*4	w_channel_open
	integer*4	w_channel_close
	integer*4	w_channel_position
	integer*4	w_channel_filename

	integer*4	w_messages_off
	integer*4	w_messages_on
	integer*4	w_version

	integer*4	w_event

	integer*4	w_item_char	!-xragma C (w_item_char)
	integer*4	w_item_i4	!-xragma C (w_item_i4)
	integer*4	w_item_r4	!-xragma C (w_item_r4)
	integer*4	w_item_r8	!-xragma C (w_item_r8)
	integer*4	w_item_xlate
	integer*4	w_item_format

	integer*4	w_phys_fft_r8
	integer*4	w_phys_rad1_r8
	integer*4	w_phys_rad2_r8
	integer*4	w_phys_tds_r8
	integer*4	w_phys_tnr_r8

	integer*4	w_physical_tnr_r4
	integer*4	w_physical_rad1_r4
	integer*4	w_physical_rad2_r4

	integer*4	w_ur8_from_ydoy
	integer*4	w_ur8_from_ymd
	integer*4	w_ur8_to_string
	integer*4	w_ur8_to_string_fr
	integer*4	w_ur8_to_ydoy
	integer*4	w_ur8_to_ymd
	integer*4	w_ur8_to_ymd_i
	integer*4	w_ur8_from_ymd_i

	integer*4	WIND_TM_OPEN_CHANNEL    
	integer*4	WIND_TM_CLOSE_CHANNEL   
	integer*4	WIND_TM_GET_STREAM_MFMF 
	integer*4	WIND_TM_GET_MFMF        
	integer*4	WIND_TM_GET_EARLIEST_MFMF
	integer*4	WIND_TM_GET_LATEST_MFMF 
	integer*4	WIND_TM_INCREMENT_MFMF
	integer*4	WIND_TM_DECREMENT_MFMF  
	integer*4	WIND_TM_INCREMENT_PACKET
	integer*4	WIND_TM_GET_WORD        
	integer*4	WIND_TM_GET_MINOR_FRAME 
	integer*4	WIND_TM_GET_MAJOR_FRAME 
	integer*4	WIND_TM_GET_PACKET      
	integer*4	WIND_TM_GET_HK          
	integer*4	WIND_TM_DATE            
	integer*4	WIND_TM_TIME            
	integer*4	WIND_TM_ERT_TO_MFMF     
	integer*4	WIND_TM_SCET_TO_MFMF     
	integer*4	WIND_TM_MFMF_TO_ERT
	integer*4	WIND_TM_MFMF_TO_SCET
	integer*4	WIND_TM_DELTA_MFMF      
	integer*4	WIND_TM_BIT_RATE        
	integer*4	WIND_TM_SET_WAIT        
	integer*4	WIND_TM_SET_NOWAIT      
	integer*4	WIND_TM_VERSION
	integer*4	WIND_TM_GET_EVENT
	integer*4	WIND_TM_GET_ITEM
	integer*4	WIND_TM_GET_ITEM_INTEGER
	integer*4	WIND_TM_GET_ITEM_REAL4
	integer*4	WIND_TM_GET_ITEM_REAL8
	integer*4	WIND_TM_GET_ITEM_CHAR
	integer*4	WIND_TM_GET_FILENAME
	integer*4	WIND_TM_SET_MESSAGES_ON
	integer*4	WIND_TM_SET_MESSAGES_OFF
	integer*4	WIND_ICP
	integer*4	WIND_TM_GET_TEST
	integer*4	WIND_TM_GET_STEP
	integer*4	WIND_TM_DECREMENT_PACKET
	integer*4	WIND_TM_GET_PREVIOUS_EVENT
	integer*4	WIND_TM_GET_NEXT_EVENT
	integer*4	WIND_TM_XLATE_ITEM
	logical*4	WIND_TM_EOF
	logical*4	WIND_TM_REALTIME

! end of wind_tm_routine_def.for
