! master_cylinder.for 

	integer*4	function	w_master_cylinder
	1				(aevent, ch, item, buf, buf_sz, ret_sz)
	implicit	none
	character*(*)	aevent		! r, "true" event name
	integer*4	ch		! r, wind_lib channel number
	character*(*)	item		! r, item name
	real*4		buf(*)		! w, caller's buffer
	integer*4	buf_sz		! r, size of caller's buffer
	integer*4	ret_sz		! w, number of values put in buffer

	integer*4	w_physical_tnr_r4	! a function
	integer*4	w_physical_fft_r4	! a function
	integer*4	w_physical_rad1_r4	! a function
	integer*4	w_physical_rad2_r4	! a function
	integer*4	ok
	integer*4	ios
	character*32	event

	w_master_cylinder = 0

	event = aevent
	if (event(1:3) .eq. 'TNR') then
	   ok = w_physical_tnr_r4(ch,item,buf,buf_sz,ret_sz)
	else if (event(1:4) .eq. 'RAD2') then
	   ok = w_physical_rad2_r4(ch,item,buf,buf_sz,ret_sz)
	else if (event(1:4) .eq. 'RAD1') then
	   ok = w_physical_rad1_r4(ch,item,buf,buf_sz,ret_sz)
	else if (event(1:3) .eq. 'FFT') then
	   ok = w_physical_fft_r4(ch,item,buf,buf_sz,ret_sz)
	else
	   goto 60
	end if

	w_master_cylinder = ok

	return
  1	format(1x,'W_MASTER_CYLINDER: ', a)
  60	continue
$IF ABSOFT_FORTRAN
	write(6,1,iostat=ios)
	1    'event '//event//' has no physical units routine.'
$ELSE
	write(6,1,iostat=ios) 'event '//event//' has no physical units routine.'
$ENDIF
	return
	end
