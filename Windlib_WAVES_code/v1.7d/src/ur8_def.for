! ur8_def.for - Ulysses REAL*8 time format constants for WIND/WAVES processing

	real*8		ur8_day, ur8_hr, ur8_min, ur8_sec, ur8_msec
	parameter	(ur8_day=1.0,
	1		ur8_hr=ur8_day/24.0,
	1		ur8_min=ur8_hr/60.0,
	1		ur8_sec=ur8_min/60.0,
	1		ur8_msec=ur8_sec/1000.0)

	real*8		ur8_1x_mf_diff
	parameter	(ur8_1x_mf_diff=ur8_sec*92.0/250.0)
	real*8		ur8_2x_mf_diff
	parameter	(ur8_2x_mf_diff=ur8_sec*46.0/250.0)
