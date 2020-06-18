! t_def.for - data declarations for t.for
! Note:  scr_manager_def.for must be "included" prior to this file.

	include		'wdir:termvideo_def.for/nolist'
	include		'wdir:scr_manager_def.for/nolist'

	parameter	wind_last_file_used='WIND_LAST_FILE_USED'
	parameter	max_fields=20
	parameter	max_on_keys=20
	parameter	max_screen_objects=20
	parameter	ur8_fmt='(f16.10)'

	record /scr_mgr_object/ g(max_screen_objects)
	integer*4	n_g

	record /tv_field/ f(max_fields)
	integer*4	n_f

	record /tv_on_key/ on(max_on_keys)

	character	w_fs3_screen*4096
	integer*4	w_fs3_screen_size

	character*256	result
	integer*4	chooser_start_line /18/
	integer*4	chooser_bottom_line /24/
	integer*4	chooser_row_height /6/

	integer*4	default_year
	real*8		ur8a, ur8b
	real*8		prev_ur8a, prev_ur8b

	common /fs3_blk0/ 
	1		ur8a, ur8b,
	1		prev_ur8a, prev_ur8b,
	1		g, n_g, f, n_f,
	1		on,
	1		w_fs3_screen, w_fs3_screen_size,
	1		result,
	1		chooser_start_line,
	1		chooser_bottom_line,
	1		chooser_row_height,
	1		default_year
