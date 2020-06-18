! wind_decom_blk_def.for - common block definition used by wind_decom_lib.for

	include	'wind_decom_def.for'

	structure /decom_channels/
	   record /decom_file_info/ dfi(0:xr_idx)
	   record /decom_xref_buffer/ xrbuf(max_xref_buffer_size)
	   integer*4	first_major
	   integer*4	last_major
	   integer*4	first_minor
	   integer*4	last_minor
	end structure
	record /decom_channels/ dc(max_channels)

	common /wind_user_blk7/ dc
