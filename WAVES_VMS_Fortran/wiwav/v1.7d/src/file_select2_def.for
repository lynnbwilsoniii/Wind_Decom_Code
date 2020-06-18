! file_select2_def.for - data declarations for file_select2.for
! Note:  scr_manager_def.for must be "included" prior to this file.

	parameter	wind_last_file_used='WIND_LAST_FILE_USED'
	parameter	max_fields=4
	parameter	max_on_keys=32
	parameter	max_scrolls=3
	parameter	max_screen_objects=4

	record /scr_mgr_object/ g(max_screen_objects)
	integer*4	n_g

	record /tv_field/ f(max_fields)
	integer*4	n_f

	record /tv_scroll/ s(max_scrolls)
	integer*4	n_s

	record /tv_on_key/ on(max_on_keys)

	character	my_screen*4096
	integer*4	my_screen_size

	! day #2 data is stored in slot #2 even if day#1 doesn't exist
	structure /virtual_month/
	   integer*4	i
	   integer*4	n
	   character*2	dayver(32)
	end structure
	!
	! mon #2 data is stored in slot #2 even if no data for mon #1
	structure /virtual_year/
	   character*4	year
	   integer*4	i
	   record /virtual_month/ vm(12)
	end structure
	!
	! year #3 data is stored in slot #3 even if no data for year #2
	parameter	max_years_of_mission=10
	structure /virtual_file_list/
	   integer*4	i
	   record /virtual_year/ vy(max_years_of_mission)
	end structure
	record /virtual_file_list/ vfl

	parameter	max_files=2048
	character*96	files(max_files)
	parameter	max_dirs=128
	character*96	dirs(max_dirs)
	character*256	current
	character*256	last_file_used
	character*256	result_file
	integer*4	show_file_size
	integer*4	have_shown_file_list_once
	integer*4	have_shown_dir_list_once
	integer*4	newly_including_sizes
	integer*4	dir_search_depth
	parameter	max_search_list_entries=16
	integer*4	n_search_list_entries
	logical*4	got_search_list
	character*256	search_list(max_search_list_entries)
	logical*4	use_other_chooser
	integer*4	state

	parameter	default_dir='WIND_DATA'
	parameter	default_mask='*.*'

	parameter	i_lf=1
	parameter	i_sdir=1
	parameter	i_sfile=2

	common /file_select2_blk0/ g, n_g, f, n_f,
	1		s, n_s,
	1		on,
	1		my_screen, my_screen_size,
	1		files, dirs, current, last_file_used,
	1		result_file, show_file_size,
	1		have_shown_file_list_once, newly_including_sizes,
	1		dir_search_depth, n_search_list_entries,
	1		got_search_list, search_list,
	1		have_shown_dir_list_once,
	1		vfl, use_other_chooser, state

cdec$ psect /file_select2_blk0/ noshr


	structure /file_display_line/
	   union
	   map
	   character*78	s
	   end map
	   map
	   character*27	file_name
	   character*1	space2
	   character*3	month
	   character*1	space3
	   character*2	day_of_month
	   character*1	space4
	   character*7	block_size
	   character*1	space5
	   character*9	owner
	   character*1	space6
	   character*19	protection
	   end map
	   end union
	end structure

	parameter	nrt_id_label='Near Realtime'
	parameter	ok_and_exit_now=3
