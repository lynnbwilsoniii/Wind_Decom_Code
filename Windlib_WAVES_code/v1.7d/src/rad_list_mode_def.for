! rad_list_mode_def.for
!

	! this structure must be identical to another found in
	! source code item_functions.c
	structure /tm_list_parms/
	  integer*4	xlate_table	! selected translation table
	  integer*4	freq_table	! selected freq table
	  integer*4	steps		! number of steps
	  integer*4	sum_loop	! sum loop count
	  integer*4	group_loop	! group loop count
	  integer*4	group_size	! group size
	  integer*4	xlat_mask	! translation flag and mask
	  integer*4	auto_mask	! sum toggle mode mask
	  integer*4	sum_flag	! initial sum/sep state of event
	  integer*4	radio		! 1=rad1, 2=rad2
	  integer*4	list_type	! channels, freqs, or toggle states
	end structure

!	character*128	rad1_ffile_name
!	character*128	rad1_pfile_name

$IF ABSOFT_FORTRAN
	integer*4 n_rad1_plists
	integer*4 n_p_per_line_rad1
	integer*4 size_rad1_plist
	parameter (n_rad1_plists        = 12)	! number of rad1 pointer lists
	parameter (n_p_per_line_rad1    = 16)	! count of numbers in input line
	parameter (size_rad1_plist      = 64)	! count of pointers in rad1 list

!	character*128	rad2_ffile_name
!	character*128	rad2_pfile_name

	integer*4 n_rad2_plists
	integer*4 n_p_per_line_rad2
	integer*4 size_rad2_plist
	integer*4 n_flists
	integer*4 size_flist
	integer*4 last_fdex
	integer*4 rad_ch_list
	integer*4 rad_hz_list
	integer*4 rad_tgl_list
	parameter (n_rad2_plists        = 14)	! number of rad2 pointer lists
	parameter (n_p_per_line_rad2    = 12)	! count of numbers in input line
	parameter (size_rad2_plist      = 48)	! count of pointers in rad2 list

	parameter (n_flists 	  = 16)	!CURRENT MAX NUMBER OF FREQUENCY LISTS
	parameter (size_flist      = 16)	!CURRENT MAX NUMBER OF FREQUENCIES/LIST
	parameter (last_fdex	  = (n_flists*size_flist)-1)

	parameter	(rad_ch_list=18)
	parameter	(rad_hz_list=19)
	parameter	(rad_tgl_list=20)
$ELSE
	parameter n_rad1_plists        = 12	! number of rad1 pointer lists
	parameter n_p_per_line_rad1    = 16	! count of numbers in input line
	parameter size_rad1_plist      = 64	! count of pointers in rad1 list

!	character*128	rad2_ffile_name
!	character*128	rad2_pfile_name

	parameter n_rad2_plists        = 14	! number of rad2 pointer lists
	parameter n_p_per_line_rad2    = 12	! count of numbers in input line
	parameter size_rad2_plist      = 48	! count of pointers in rad2 list

	parameter n_flists 	  = 16	!CURRENT MAX NUMBER OF FREQUENCY LISTS
	parameter size_flist      = 16	!CURRENT MAX NUMBER OF FREQUENCIES/LIST
	parameter last_fdex	  = (n_flists*size_flist)-1

	parameter	rad_ch_list=18
	parameter	rad_hz_list=19
	parameter	rad_tgl_list=20
$ENDIF
