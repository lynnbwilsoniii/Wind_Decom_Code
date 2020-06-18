! wind_cdhf_alloc_def.for - wind/waves telemetry words, values are minor frame
! word numbers (0...255) 
! Note: file wind_cdhf_rec_def.for must be included prior to including this
! file.

	integer*4	fixed_col_science_alloc( size_science_allocation)
	1		/17, 18, 24, 28, 32, 36, 40, 44, 48, 52, 56,
	1		 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 100, 104,
	1		108, 112, 116, 120, 124, 128, 132, 136, 140, 144,
	1		148, 152,
	1		156, 160, 164, 168, 172, 176, 180, 184, 188, 192/

	integer*4	fixed_col_maneuver_alloc(size_maneuver_allocation)
	1		/17, 18, 32, 33, 36, 37, 40, 41, 44, 45, 48, 49, 52,
	1		 53, 56, 57, 60, 61, 64, 65, 68, 69, 72, 73, 76, 77,
	1		 80, 81, 84, 85, 88, 89, 92, 93, 96, 97, 100, 101,
	1		104, 108, 112, 116, 120, 124, 128, 132, 136, 140, 144,
	1		148,
	1		152, 156, 160, 164, 168, 172, 176, 180, 184, 188, 192/

