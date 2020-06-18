! vax_only.for - stub routines for compiling wind_lib on VAX/VMS platform
! The routines listed here should never be called from VMS and are only
! included here to satisfy linker references.


!------------------------------------------------------------------------------
! Simple file name solicitation routine.
!---------------------------------------------------------------------------
	integer*4	function	w_get_sun_filename(f)
	implicit	none
	character*(*)	f
	integer*4	ios
	integer*4	i

	w_get_sun_filename = 0

 100	write(6,'(1x,a,$)') 'Enter (.wnd) filename: '
	read(5,'(q,a)', iostat=ios) i, f
	if (i.le. 0) then
	   write(6,*) char(7), '...try again...'
	   goto 100
	else if (ios .eq. -1) then
	   return
	else if (ios .ne. 0) then
	   write(6,*) 'Error reading filename, again please...'
	   goto 100
	end if

	w_get_sun_filename = 1

	return
	end

