! wind_version.for -
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_version(version_string)
! Returns current wind_lib software version string.
! See file wind_lib.history for specific version information.
!
	implicit	none
	integer*4	iver /0/
	integer*4	sz_ver
	parameter	(sz_ver=16)
	character*(sz_ver) ver /'1.7d VMS-SunOS'/
	include		'low_byte_def.for'
	byte		b(*)
	integer*4	sz_b
	integer*4	i,j,k,o
	integer*4	v1	! 1st version #:  "1" in 1.4j.3
	integer*4	v2	! 2nd version #:  "4" in 1.4j.3
	integer*4	v3	! 3rd version #:  "j" in 1.4j.3
	record /low_byte/ x
	integer*4	wind_lib_version_c	! an entry point
	integer*4	w_version		! an entry point
	integer*4	w_version_c		! an entry point
	integer*4	w_version_i4		! an entry point
	integer*4	i4
	logical*4	first_time /.true./
	logical*4	wind_suppress_internal_msgs	! a function

	!----------------------------------------------------------------------
	entry	w_version(version_string)

	character*(*)	version_string
	version_string  = ver
	wind_tm_version = 1
	return

	!----------------------------------------------------------------------
	! this entry point is C callable with char data type for b
	entry	w_version_c(b, sz_b)
	entry	wind_lib_version_c(b,sz_b)

	i = min(sz_ver, sz_b)
	do j=1,i
	   x.i4val = ichar(ver(j:j))
	   b(j) = x.b
	end do
	if (j .le. sz_b) b(j) = 0		! null terminate
	sz_b = i

	wind_lib_version_c = 1
	return

	!----------------------------------------------------------------------
	! This version returns a coded numeric version number based on the
	! (i,'.',i,a1) formated string version number
	entry	w_version_i4(i4)
	w_version_i4 = 0
	i4 = 0

	if (first_time) then
	   first_time = .false.
	   ! get the major version number
	   i = index(ver,'.') - 1
	   if (i .lt. 1) goto 20
	   read(ver(1:i),'(i)',iostat=o,err=20) v1
	   v1 = v1 * 1 00 00
	   ! get the minor version number
	   i = i + 2
	   j = i + 1
	   do while(j.lt.len(ver) .and. ver(j:j).ge.'0' .and. ver(j:j).le.'9')
	      j = j + 1
	   end do
	   j = j - 1
	   read(ver(i:j),'(i)',iostat=o,err=20) v2
	   v2 = v2 * 100
	   ! get the subversion letter
	   k = j + 1
	   v3 = 0
	   if (ver(k:k) .ne. ' ') v3 = ichar(ver(k:k))
	   if (v3 .ge. ichar('A') .and. v3 .le. ichar('Z')) then
	      v3 = v3 - ichar('A') + 1
	   else if (v3 .ge. ichar('a') .and. v3 .le. ichar('z')) then
	      v3 = v3 - ichar('a') + 1
	   else
	      if (ver(k:k) .ne. ' ') v3 = 99
	   end if
	   iver = v1 + v2 + v3
	end if

	i4 = iver

	w_version_i4 = 1
	return
  1	format(1x,'W_VERSION_I4: ', a)
 20	continue
	if (wind_suppress_internal_msgs()) return
	write(6,1,iostat=o) 'cannot determine numeric version number.'
	return
	end
