! wind_print_def.for -- data structures/definitions for wind_print_lib.for.

	parameter	max_printer_channels=2
	parameter	first_printer_channel=377
	parameter	last_printer_channel=first_printer_channel +
	1			max_printer_channels - 1
	integer*4	lines_per_landscape_page
	integer*4	lines_per_portrait_page
	integer*4	chars_per_landscape_line
	integer*4	chars_per_portrait_line
	integer*4	number_of_header_lines
	integer*4	number_of_footer_lines
	integer*4	max_pages_without_printing
	integer*4	entry_count_threshold
	character*96	wind_print_filename
	character*32	wind_print_queue_name
	character*32	landscape_print_command
	character*32	landshift_print_command
	character*32	portrait_print_command
	character*32	portshift_print_command
	parameter	default_parm_file='wind_share:wind_print_def.nml'
	parameter	logical_parm_file='wind_print_parameters'

	structure /wind_printer_user/
	   integer*4	channel			! channel number (multi stream)
	   integer*4	page_number		! page number since channel open
	   integer*4	total_page_number	! page number since startup
	   integer*4	line_number		! line number on current page
	   integer*4	lines_per_page		! number lines on user's page
	   integer*4	chars_per_line		! number of characters per line
	   integer*4	last_page_printed	! pg number of last page printed
	   integer*4	lun			! FORTRAN logical unit number
	   character*16	orientation		! 'landscape' or 'portrait'
	   character*96 filename		! output file name
	   integer*4	version			! VMS version number
	   integer*4	number_print_jobs	! total number of print commands
	   integer*4	wait_until_end_to_print	! hardcopy timing flag
	end structure
	record /wind_printer_user/
	1		wpu(first_printer_channel:last_printer_channel)

	common /wind_printer_blk0/ wpu,
	1	lines_per_landscape_page,
	1	lines_per_portrait_page,
	1	chars_per_landscape_line,
	1	chars_per_portrait_line,
	1	number_of_header_lines,
	1	number_of_footer_lines,
	1	max_pages_without_printing,
	1	entry_count_threshold,
	1	wind_print_filename,
	1	wind_print_queue_name,
	1	landscape_print_command,
	1	landshift_print_command,
	1	portrait_print_command,
	1	portshift_print_command

