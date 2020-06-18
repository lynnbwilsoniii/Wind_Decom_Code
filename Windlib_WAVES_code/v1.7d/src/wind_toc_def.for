! wind_toc_def.for - for table of contents files for reading .wnd files

!
! table of contents
!
	structure /toc_file_info/	! table_of_contents
	   integer*4	lun
	   logical*4	exists
	   integer*4	recnum
	   integer*4	packet_id
	   integer*2	is_first
	   integer*2	is_last
	   integer*4	pointer
	   integer*4	major
	   integer*4	minor
	   integer*4	ncalls		! stats
	   integer*4	coarse_hits
	   integer*4	fine_hits
	   integer*4	nfine_reads
	end structure

	structure /wind_toc_disk_record/
	   integer*4	ptr
	   integer*4	major
	   byte		minor
	   byte		id
	   byte		is_first
	   byte		is_last
	end structure
	record /wind_toc_disk_record/ toc_rec


	   record	/toc_file_info/ toc
