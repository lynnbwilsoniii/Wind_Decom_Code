! wind_realtime_def.for - data structures for using wind/waves realtime tm


	structure /wind_realtime_stuff/
	   integer*4	ch
	   logical*4	wait_for_new_records
	   character*128 file			! recording file name
	   integer*4	tm_ptrs(0:sizeof_minor_frame-1)
	   integer*4	num_tm_ptrs
	   logical*1	is_a_valid_ptr(0:sizeof_minor_frame-1)
	   record	/wind_record/	rec
	end structure

	record /wind_realtime_stuff/ rltm(max_channels)

	common /wind_user_blk5/ rltm
