	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_output_device()
!	implicit	none
	include		'apmplot_def.for'
	integer*4	ok
	integer*4	i,j,k
	integer*4	ios
	character*1	c
	integer*4	initialize_mongo_xwindows

 10	write(6,'(a,$)') ' Enter output device [H=hardcopy (default), X=xwindow]: '
	read(5,'(q,a)',iostat=ios,err=10) i, c
	i = 0				!may 1999
	if (i.eq.0 .or. c .eq. 'h' .or. c.eq.'H') then
	   ok = 1
	   terminal_output = 0
	else if (c .eq. 'x' .or. c .eq. 'X') then
	   terminal_output = 1
	   ok = initialize_mongo_xwindows()
	else
	   type *, 'No output device selected.'
	   ok = 0
	end if

	get_output_device = ok
	return
	end


