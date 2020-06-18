! wind_tm_event_def.for - data structures and declarations for working with
! WIND/WAVES telemetry events

! NOTE:  This file must be "included" after wind_tm_user_def.for in the
! wind_tm_lib.for source because of the definitions for max_channels
! and size_of_packet.
$IF ABSOFT_FORTRAN
!	integer*4       size_of_packet, max_channels, size_of_packet
!	parameter	(size_of_packet=431)
!	parameter	(max_channels=5)
!	parameter	(size_of_packet=431)

	integer*4       max_packets_per_event
	integer*4       perfect_event
	integer*4       truncated_event
	integer*4       gapped_event
	parameter	(max_packets_per_event=32)
	parameter	(perfect_event=1)
	parameter	(truncated_event=3)
	parameter	(gapped_event=5)
$ELSE
!	parameter	max_channels=5
!	parameter	size_of_packet=431

	parameter	max_packets_per_event=32
	parameter	perfect_event=1
	parameter	truncated_event=3
	parameter	gapped_event=5
$ENDIF

	! this is the event built by the event builder and operated on
	! by wind_tm_get_item

$IF ABSOFT_FORTRAN
	integer*4       max_len_h1, max_len_h2
	integer*4       max_len_h3, max_len_h4
	integer*4       max_len_hk
	parameter	(max_len_h1 = 4)
	parameter	(max_len_h2 = 72)
	parameter	(max_len_h3 = 72)
	parameter	(max_len_h4 = 32)
	parameter	(max_len_hk = 81+25+125+5)
$ELSE
	parameter	max_len_h1 = 4
	parameter	max_len_h2 = 72
	parameter	max_len_h3 = 72
	parameter	max_len_h4 = 32
	parameter	max_len_hk = 81+25+125+5
$ENDIF

	structure	/wind_packet_headers/
	   byte		h1(max_len_h1)			! primary header
	   byte		h2(max_len_h2)			! instrument header
	   byte		h3(max_len_h3)			! measurement header
	   byte		h4(max_len_h4)			! packet header
	   logical*4	got_1st
	   logical*4	got_2nd
	   logical*4	got_3rd
	   logical*4	got_4th
	   logical*4	got_data
	end structure
$IF ABSOFT_FORTRAN
	integer*4	sz_wind_packet_headers
	parameter	(sz_wind_packet_headers=max_len_h1+max_len_h2+
	1		 max_len_h3+max_len_h4+ (5*4))
	integer*4	sz_max_event_data
	parameter	(sz_max_event_data=max_packets_per_event *
	1                size_of_packet)
$ELSE
	parameter	sz_wind_packet_headers=max_len_h1+max_len_h2+
	1		max_len_h3+max_len_h4+ (5*4)
	parameter	sz_max_event_data=max_packets_per_event*size_of_packet
$ENDIF

	structure	/wind_event_buffer/
	   integer*4	event_completion_status
	   integer*4	alignment_dummy
	   union
	    map
	      integer*4	cpn	 		! current_packet_number
	      integer*4	num_packets_to_build_this_event
	      record	/wind_packet_headers/ ph(max_packets_per_event)
	      byte		data(sz_max_event_data)
	      integer*4	last_data_byte
	    end map
	    map
	      byte	major_frame(0:sizeof_major_frame-1)
	    end map
	    map
	      byte	minor_frame(0:sizeof_minor_frame-1)
	    end map
	    map
	      byte	packet(0:size_of_packet-1)
	    end map
	   end union
	   integer*4	hk_major		! MF of hk data assoc. w/ event
	   byte		hk(max_len_hk)		! array of hk data
	end structure
	! declare the event buffer, eb
	record		/wind_event_buffer/ eb(max_channels)
	common		/wind_user_blk1/ eb
$IF ABSOFT_FORTRAN
	integer*4	sizeof_eb
	parameter	(sizeof_eb= 4 + 4 + 
	1		sizeof_major_frame +
	1		 4 + max_len_hk)
$ELSE
	parameter	sizeof_eb= 4 + 4 + 
	1		sizeof_major_frame +
	1		 4 + max_len_hk 

$ENDIF
	! this is the internal structure filled by wind_tm_get_event
	! in order to extract the event from the packet(s)

	structure	/wind_event_extract_info/
	   union
	   map
	   character*40	event_type		! event name
	   end map
	   map
	   byte		event_type_b
	   end map
	   map
	   real*8	alignment_dummy
	   end map
	   end union
	   integer*4	event_subtype
	   integer*4	event_status
	   integer*4	dbms_date_time(2)
	   integer*4	last_date_time(2)
	   integer*4	h1_start_bit
	   integer*4	h1_bit_size
	   integer*4	h2_bit_size
	   integer*4	h3_bit_size
	   integer*4	h4_bit_size

	   ! packet id information, constant for every packet in event
	   integer*4	packet_id_start_bit
	   integer*4	packet_id_bit_size
	   integer*4	packet_id

	   integer*4	h1_start_byte,   h1_byte_size,   h1_end_byte
	   integer*4	h2_start_byte,   h2_byte_size,   h2_end_byte
	   integer*4	h3_start_byte,   h3_byte_size,   h3_end_byte
	   integer*4	h4_start_byte,   h4_byte_size,   h4_end_byte
	   integer*4	data_start_byte, data_byte_size, data_end_byte
	   integer*4	tds_start_byte,  tds_byte_size,  tds_end_byte

	   ! flag bits
	   integer*4	first_packet_mask
	   integer*4	last_packet_mask

	   ! values calculated from first packet in event,
	   ! constant for every packet in event
	   integer*4	got_a_first_packet
	   integer*4	got_a_last_packet

	end structure
	record		/wind_event_extract_info/
	1		eei(max_channels)	! event_extract_info
	common		/wind_user_blk2/ eei
$IF ABSOFT_FORTRAN
	integer*4	sizeof_eei
	parameter	(sizeof_eei= 40 + (4*2) + (8*2) + (4*5) +
	1		(4*3) + (4*6*3) + (4*2) + (4*2))
$ELSE
	parameter	sizeof_eei= 40 + (4*2) + (8*2) + (4*5) +
	1		(4*3) + (4*6*3) + (4*2) + (4*2)
$ENDIF
