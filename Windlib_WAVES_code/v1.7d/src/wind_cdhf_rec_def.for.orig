! wind_cdhf_rec_def.for - data structures for using WIND/WAVES cdhf files,
! including:
!
!  - Level-Zero Data File Label Record Definition
!  - Level-Zero Data Record Header Definition
!  - Level-Zero Data Subrecord Formats

	integer*4	science_mode_1
	integer*4	science_mode_2
	integer*4	maneuver_mode_1
	integer*4	maneuver_mode_2
	integer*4	contingency_mode_1
	integer*4	contingency_mode_2

	parameter	(science_mode_1=1)
	parameter	(maneuver_mode_1=3)
	parameter	(contingency_mode_1=4)
	parameter	(science_mode_2=5)
	parameter	(maneuver_mode_2=7)
	parameter	(contingency_mode_2=8)

	integer*4	science_mode_lrecl
	integer*4	maneuver_mode_lrecl
	integer*4	max_lrecl
	integer*4	min_lrecl
	integer*4	max_i4_lrecl

	parameter	(science_mode_lrecl=11552)
	parameter	(maneuver_mode_lrecl=15552)
	parameter	(max_lrecl=max(maneuver_mode_lrecl,science_mode_lrecl))
	parameter	(min_lrecl=min(maneuver_mode_lrecl,science_mode_lrecl))
	parameter	(max_i4_lrecl=max_lrecl/4)

	structure /pb5_time/
	   union
	   map
	   integer*4	x
	   integer*4	y
	   end map
	   map
	   byte		b(8)
	   end map
	   end union
	end structure

	structure /edit_file/
	   character*44	edit_filename
	   character*24	edit_key
	   integer*4	edit_rerun_number
	   character*8	edit_prog_version
	   character*16	edit_run_time
	   character*4	edit_data_type
	   character*28	edit_message_key
	end structure

	structure /L0_DATA_FILE_LABEL_H/
	   integer*4	spacecraft_id
	   integer*4	instrument_number
	   character*4	instrument_name
	   integer*4	physical_record_number
	   integer*4	recs_per_major_frame
	   integer*4	max_recs_in_file
	   integer*4	major_frame_count_first
	   integer*4	major_frame_count_last
	   record /pb5_time/	sc_clock_first
	   record /pb5_time/	sc_clock_last
	   integer*4	atc_year_first
	   integer*4	atc_day_first
	   integer*4	atc_msec_first
	   integer*4	atc_micro_first
	   integer*4	atc_year_last
	   integer*4	atc_day_last
	   integer*4	atc_msec_last
	   integer*4	atc_micro_last
	   integer*4	n_major_expected
	   integer*4	n_major_in_file
	   integer*4	n_major_gaps
	   character*4	data_coverage_type
	   integer*4	decom_rerun_number
	   character*8	decom_prog_version
	   character*8	decom_db_version
	   character*16	decom_run_time
	   character*44	instrument_filename
	   integer*4	lrecl
	   character*20	spares
	   integer*4	merge_rerun_number
	   character*8	merge_prog_version
	   character*16	merge_run_time
	   integer*4	n_edit_files
	   record /edit_file/ ef(20)
	end structure

	structure /L0_DATA_FILE_LABEL/
	union
	map
	   byte		b(0:max_lrecl-1)
	end map
	map
	   record /l0_data_file_label_h/ fh
	end map
	end union
	end structure

	integer*4	size_l0_data_rec_header
	parameter	(size_l0_data_rec_header=300)

	structure /L0_DATA_HEADER/
	union
	map
	   byte		b(0:size_l0_data_rec_header-1)
	end map
	map
	   integer*4	instrument_number
	   integer*4	physical_record_number
	   integer*4	major_frame
	   record /pb5_time/ sc_clock
	   integer*4	atc_year
	   integer*4	atc_day
	   integer*4	atc_msec
	   integer*4	atc_micro
	   integer*4	n_mf_with_fill
	   integer*4	n_mf_with_synch_err
	   integer*4	tm_mode
	   byte		quality(0:249)
	end map
	end union
	end structure

! data record stuff

	integer*4	size_science_allocation
	parameter	(size_science_allocation=45)

	structure /science_tm_data/
	   byte		tm(0:size_science_allocation-1)
	end structure

	integer*4	size_maneuver_allocation
	parameter	(size_maneuver_allocation=61)

	structure /maneuver_tm_data/
	   byte		tm(0:size_maneuver_allocation-1)
	end structure

	structure /L0_DATA_RECORD/
	union
	map
	   byte		b(0:max_lrecl-1)
	end map
	map
	   record /l0_data_header/ h
	   union
	   map
	      record /science_tm_data/ s(0:249)
	   end map
	   map
	      record /maneuver_tm_data/ m(0:249)
	   end map
	   end union
	end map
	map
	   byte	science_rec(science_mode_lrecl)
	end map
	map
	   byte	maneuver_rec(maneuver_mode_lrecl)
	end map
	end union
	end structure
