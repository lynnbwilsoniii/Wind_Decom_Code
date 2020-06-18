! wind_extra_info_def.for - extra event and stream positioning info available
! via extract area six.

$IF ABSOFT_FORTRAN
	integer*4	n_extra_info_lws
	parameter	(n_extra_info_lws=64)	! lsw=longwords
$ELSE
	parameter	n_extra_info_lws=64	! lsw=longwords
$ENDIF

	structure	/wind_extra_info/
	   union
	      map
	         integer*4	extra_info(n_extra_info_lws)
	      end map
	      map
	         ! bytes 0..7
	         integer*4	ert(2)		! earth receive time 0..7
	         ! bytes 8..15
	         integer*4	major		! ert major frame 8..11
	         integer*4	minor		! ert minor frame 12..15
	         ! bytes 16..23
                 integer*4	scet(2)		! spacecraft event time 16..23
	         ! bytes 24..31
	         integer*4	scet1000	! fractional scet to 1/1000 sec
	         integer*4	dpu_major_ert	! DPU major# in 1st rec of evnt
	         ! bytes 32..39
	         byte		dpu_version	! dpu version of flight software
	         byte		fft_version	! fft flight software version
	         byte		tds_version	! tds flight software version
	         byte		dummy		! 35..35
	         integer*4	psni		! pointer to same name item
	         ! bytes 40..47
	         byte		sp_test_number	! Script Player 40..40
	         byte		sp_step_number  ! Script Player 41..41
	         byte		sc_mode		! cdhf mode codes 42..42
	         byte		bit_rate	! 0=slow, 1=fast  43..43
	         integer*4	ert1000		! fractional ert to 1/1000 sec
	         ! bytes 48..55
	         real*8		ur8_scet	! event data scet in ur8 format
	         ! bytes 56..63
	         real*8		ur8_ert		! boe, scet 1st frame, 1st pkt
	         ! bytes 64..71
	         real*8		ur8_eoe		! eoe, scet last frame, last pkt
	         ! bytes 72..79
	         real*8		ur8_position	! last w_channel_position()
	         ! bytes 80..87
	         real*8		ur8_context	! one of: ur8_[scet,position]
	         ! bytes 88..91
	         integer*4	wind_lib_nver	! numeric wind_lib version #
	         integer*4	dummy2		! 92..95
	         ! bytes 96..103
	         real*8		stream_boi	! beginning of information
	         real*8		stream_eoi	! end       of information
	         real*8		stream_bow	! beginning of window
	         real*8		stream_eow	! end       of window
	         real*8		stream_bof	! beginning of file
	         real*8		stream_eof	! end       of file
	         real*8		stream_bod	! beginning of day
	         real*8		stream_eod	! end       of day
	         real*8		stream_eoy	! end       of yesterday
	         real*8		stream_bot	! beginning of tomorrow
	         ! bytes 176...
	      end map
	   end union
	end structure
	record 	/wind_extra_info/ exi(max_channels)
	common	/wind_user_blk8/ exi
