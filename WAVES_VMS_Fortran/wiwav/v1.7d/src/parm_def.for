! parm_def.for - misc parameter definitions for wind_lib

$IF ABSOFT_FORTRAN
	integer*4	max_channels, sizeof_minor_frame
	integer*4	num_minors_per_major, max_minor_frame_num
	integer*4	min_minor_frame_num, sizeof_major_frame
	integer*4	max_tm_pointers, size_of_packet
	integer*4	min_packet_id, max_packet_id
	integer*4	forward, reverse

	parameter	(max_channels=8)
	parameter	(sizeof_minor_frame=256)
	parameter	(num_minors_per_major=250)
	parameter	(max_minor_frame_num=249)
	parameter	(min_minor_frame_num=0)
	parameter	(sizeof_major_frame=256*250)

	parameter	(max_tm_pointers=256)
	parameter	(size_of_packet=431)

	parameter	(min_packet_id=0)
	parameter	(max_packet_id=15)

	parameter	(forward=1)
	parameter	(reverse=0)
$ELSE
	parameter	max_channels=8
	parameter	sizeof_minor_frame=256
	parameter	num_minors_per_major=250
	parameter	max_minor_frame_num=249
	parameter	min_minor_frame_num=0
	parameter	sizeof_major_frame=256*250

	parameter	max_tm_pointers=256
	parameter	size_of_packet=431

	parameter	min_packet_id=0
	parameter	max_packet_id=15

	parameter	forward=1
	parameter	reverse=0
$ENDIF

	structure /vms_64bit_time/
	   union
	   map
	   byte		vmstime(8)
	   end map
	   map
	   byte		b(8)
	   end map
	   map
	   integer*4	i4(2)
	   end map
	   end union
	end structure

$IF ABSOFT_FORTRAN
	integer*4	safe_word
	parameter	(safe_word=32)

	real*4		hi_frame_rate
	parameter	(hi_frame_rate=5.4346)
	real*4		lo_frame_rate
	parameter	(lo_frame_rate=2.7173)

	real*8		ur8_1s
	parameter	(ur8_1s=1.0/(24.0*60.0*60.0))
	real*8		ur8_46s
	parameter	(ur8_46s=46.0*ur8_1s)
	real*8		ur8_92s
	parameter	(ur8_92s=92.0*ur8_1s)
	real*8		ur8_mfdts
	parameter	(ur8_mfdts=ur8_92s/250)
	real*8		ur8_mfdtf
	parameter	(ur8_mfdtf=ur8_46s/250)

	character*1	null
	parameter	(null=char(0))

	integer*4	realtime_stream, any_cdhf_stream
	integer*4	cdhf_lz_stream, cdhf_cdrom_stream
	integer*4	cdhf_nrt_stream, wnd_stream

	parameter	(realtime_stream  ='01'x)
	parameter	(wnd_stream        ='02'x)
	parameter	(any_cdhf_stream   ='10'x)
	parameter	(cdhf_lz_stream    ='11'x)
	parameter	(cdhf_cdrom_stream ='12'x)
	parameter	(cdhf_nrt_stream   ='13'x)

	! these mode definitions should match those of the cdhf files
	integer*4	science_1x
	integer*4	science_2x
	integer*4	maneuver_1x
	integer*4	maneuver_2x
	integer*4	contingency_1x
	integer*4	contingency_2x

	parameter	(science_1x=1)
	parameter	(maneuver_1x=3)
	parameter	(contingency_1x=4)
	parameter	(science_2x=5)
	parameter	(maneuver_2x=7)
	parameter	(contingency_2x=8)

	! crib = non-hk-stream critical hk information buffer
	integer*4	max_crib_size
	parameter	(max_crib_size=8)

	! scet context key values used by w_event, w_channel_position,
	! w_get_scet_in_context
!	integer*4	w_use_event_scet_context
!	parameter	(w_use_event_scet_context=1)
!	integer*4	w_use_position_scet_context
!	parameter	(w_use_position_scet_context=2)
!	integer*4	w_use_current_scet_context
!	parameter	(w_use_current_scet_context=3)

	! stream MF.mf/record tokens, for wind_tm_*mfmf routines
	integer*4	w_tk_stream_mfmf,   w_tk_next_mfmf
	integer*4	w_tk_earliest_mfmf, w_tk_latest_mfmf
	integer*4	w_tk_current_mfmf
	parameter	(w_tk_stream_mfmf  =21)
	parameter	(w_tk_next_mfmf    =22)
	parameter	(w_tk_earliest_mfmf=23)
	parameter	(w_tk_latest_mfmf  =24)
	parameter	(w_tk_current_mfmf =25)

	real*8		w_ur8_infinity
	parameter	(w_ur8_infinity=99999.d0)
$ELSE
	integer*4	safe_word
	parameter	(safe_word=32)

	real*4		hi_frame_rate
	parameter	(hi_frame_rate=5.4346)
	real*4		lo_frame_rate
	parameter	(lo_frame_rate=2.7173)

	real*8		ur8_1s
	parameter	(ur8_1s=1.0/(24.0*60.0*60.0))
	real*8		ur8_46s
	parameter	(ur8_46s=46.0*ur8_1s)
	real*8		ur8_92s
	parameter	(ur8_92s=92.0*ur8_1s)
	real*8		ur8_mfdts
	parameter	(ur8_mfdts=ur8_92s/250)
	real*8		ur8_mfdtf
	parameter	(ur8_mfdtf=ur8_46s/250)

	character*1	null
	parameter	(null=char(0))

	integer*4	realtime_stream,
	1		any_cdhf_stream,
	1		cdhf_lz_stream,
	1		cdhf_cdrom_stream,
	1		cdhf_nrt_stream,
	1		wnd_stream

	parameter	(realtime_stream  ='01'x,
	1		wnd_stream        ='02'x,
	1		any_cdhf_stream   ='10'x,
	1		cdhf_lz_stream    ='11'x,
	1		cdhf_cdrom_stream ='12'x,
	1		cdhf_nrt_stream   ='13'x)

	! these mode definitions should match those of the cdhf files
	integer*4	science_1x
	integer*4	science_2x
	integer*4	maneuver_1x
	integer*4	maneuver_2x
	integer*4	contingency_1x
	integer*4	contingency_2x

	parameter	(science_1x=1)
	parameter	(maneuver_1x=3)
	parameter	(contingency_1x=4)
	parameter	(science_2x=5)
	parameter	(maneuver_2x=7)
	parameter	(contingency_2x=8)

	! crib = non-hk-stream critical hk information buffer
	integer*4	max_crib_size
	parameter	(max_crib_size=8)

	! scet context key values used by w_event, w_channel_position,
	! w_get_scet_in_context
!	integer*4	w_use_event_scet_context
!	parameter	(w_use_event_scet_context=1)
!	integer*4	w_use_position_scet_context
!	parameter	(w_use_position_scet_context=2)
!	integer*4	w_use_current_scet_context
!	parameter	(w_use_current_scet_context=3)

	! stream MF.mf/record tokens, for wind_tm_*mfmf routines
	integer*4	w_tk_stream_mfmf,   w_tk_next_mfmf,
	1		w_tk_earliest_mfmf, w_tk_latest_mfmf,
	1		w_tk_current_mfmf
	parameter	(w_tk_stream_mfmf  =21)
	parameter	(w_tk_next_mfmf    =22)
	parameter	(w_tk_earliest_mfmf=23)
	parameter	(w_tk_latest_mfmf  =24)
	parameter	(w_tk_current_mfmf =25)

	parameter	w_ur8_infinity=99999.d0
$ENDIF
