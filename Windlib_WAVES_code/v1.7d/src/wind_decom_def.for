! wind_decom_def.for - WIND/WAVES Decom Record Definitions
!
!xxxx need a FILL type
!
! decommmutation
!

!	parameter	min_packet_id=0
!	parameter	max_packet_id=15
	integer*4	hk_idx
	parameter	(hk_idx=16)
	integer*4	xr_idx
	parameter	(xr_idx=17)

	integer*4	max_xref_buffer_size
	parameter	(max_xref_buffer_size=1880)

	! the following record definition defines the first record of all
	! decom files (master cross reference, instrument, and HK)
	structure /decom_universal_hdr_disk_rec/
	   union
	   map
	      integer*4	flags			! (1 byte for # mcr hdr recs)
	   end map
	   map
	      byte	n_mcr_hdr_recs, b1, b2, b3
	   end map
	   end union
	   integer*4	recl			! byte record length
	   integer*4	n_of_hdr_recs		! # header records
	   integer*4	n_of_data_recs		! # of data records
	   character*8	version			! decom program version #

!	   integer*4	atc_year_first
!	   integer*4	atc_day_first
!	   integer*4	atc_msec_first
!	   integer*4	atc_year_last
!	   integer*4	atc_day_last
!	   integer*4	atc_msec_last
	end structure

	structure /decom_instrument_disk_rec/
	   integer*4	xref_ptr
	   byte		ert_minor_frame
!	   byte		q1			! quality bits 0-7
!	   byte		q2			! quality bits 8-9
	   byte		pkt(0:size_of_packet)
	end structure

	structure /decom_xref_disk_rec/
	   union
	   map
	      integer*4	flags			! (1 byte for # mcr hdr recs)
	   end map
	   map
	      byte	n_mcr_hdr_recs, b1, b2, b3
	   end map
	   map
	      logical*1	g0, g1, g2, g3
	   end map
	   end union
	   integer*4	atc_year
	   integer*4	atc_day
	   integer*4	atc_msec
	   integer*4	dpu_major_frame
	   integer*4	ert_major_frame
	   byte		dpu_version
	   byte		fft_version
	   byte		tds_version
	   byte		spare
	end structure

	structure /decom_xref_buffer/
	   real*8 	scet
	   union
	   map
	      integer*4	flags			! (1 byte for # mcr hdr recs)
	   end map
	   map
	      byte	n_mcr_hdr_recs, b1, b2, b3
	   end map
	   end union
	   integer*4	dpu_major_frame
	   byte		dpu_version
	   byte		fft_version
	   byte		tds_version
	   byte		spare
	end structure

	structure /decom_hk_disk_rec/
	   integer*4	xref_ptr
	   byte		word(0:w_last_hk_idx)
	end structure

	structure /decom_file_info/
	   real*8	scet_initial
	   real*8	scet_final
	   integer*4	lun
	   integer*4	recno			! current record #
	   integer*4	first_rec		! rec # of first data record
	   integer*4	last_rec		! rec # of last data record
	   integer*4	byte_lrecl
	   integer*4	recno_correction
	   integer*4	k	! k3len of file
	   character*256 file
	   record /decom_universal_hdr_disk_rec/ duh
	   union
	   map
	      record /decom_instrument_disk_rec/ ddr_ins
	   end map
	   map
	      record /decom_xref_disk_rec/ ddr_xr
	   end map
	   map
	      record /decom_hk_disk_rec/ ddr_hk
	   end map
	   map
	      byte	byte_rec(512)
	   end map
	   end union
	end structure

	integer*4	estimated_ert_mjr,
	1		estimated_dpu_mjr,
	1		estimated_dpu_ver,
	1		estimated_fft_ver,
	1		estimated_tds_ver,
	1		estimated_scet,
	1		estimated_sc_mode,
	1		estimated_bit_rate,
	1		sc_mode_mask,
	1		bit_rate_mask,
	1		n_mcr_hdr_mask,
	1		estimates_resolved

	parameter	(n_mcr_hdr_mask       ='0000ff'x,	! byte 0

	1		sc_mode_mask          ='00000f'x,	! byte 1
	1		bit_rate_mask         ='000010'x,	! byte 1
	1		estimated_sc_mode     ='000020'x,	! byte 1
	1		estimated_bit_rate    ='000040'x,	! byte 1
	1		estimated_scet        ='000080'x,	! byte 1

	1		estimated_dpu_ver     ='000001'x,	! byte 2
	1		estimated_fft_ver     ='000002'x,	! byte 2
	1		estimated_tds_ver     ='000004'x,	! byte 2

	1		estimated_ert_mjr     ='000008'x,	! byte 2
	1		estimated_dpu_mjr     ='000010'x,	! byte 2
	1		estimates_resolved    ='000020'x)	! byte 2

	! universal header record bit fields
	integer*4	big_endian_encoding,
	1		header_rec_count_mask
	parameter	(header_rec_count_mask='0000ff'x,
	1		big_endian_encoding   ='000200'x)

