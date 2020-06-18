! ur8d.for
	program ur8c
	implicit	none
	real*8		u,v
	integer*4	i,j,k,o
	integer*4	ok
	integer*4	dbms_to_ur8
	integer*4	ur8_to_dbms
	integer*4	ur8_to_wnd
	integer*4	wnd_to_ur8
	integer*4	wnd_to_dbms
	integer*4	dbms_to_wnd
	integer*4	wnd(2)
	integer*4	w_ur8_to_filename
	character*60	f


  1	format(1x,a,$)
  3	format(1x,a, f15.10, :, i10, i8, i5)
  4	format(1x,a, 2x, z8.8, 2x, z8.8)
  5	format(1x,a,         i10, i8, i5)
 10	write(6,1) 'Enter a ur8 value: '
	read(5,*,end=999) u
	v = u

	ok = w_ur8_to_filename(u,f)
	type *, 'File= ', f

	ok = ur8_to_dbms(u, i, j, k)
	write(6,3,iostat=o) ' ur8 to dbms: ', u, i, j, k

	ok = dbms_to_ur8(i, j, k, u)
	write(6,3,iostat=o) ' dbms to ur8 : ', u, i, j, k

	ok = dbms_to_wnd(i,j,k, wnd)
	write(6,4,iostat=o) ' dbms to wnd: ', wnd(1), wnd(2)

	ok = wnd_to_dbms(wnd, i,j,k)
	write(6,5,iostat=o) '  wnd to dbms: ',  i, j, k

	ok = wnd_to_ur8(wnd, u)
	write(6,3,iostat=o) '  wnd to ur8: ', u

	ok = ur8_to_wnd(v, wnd)
	write(6,4,iostat=o) ' v ur8 to wnd: ', wnd(1), wnd(2)

	type *, ' '

	goto 10

 999	continue
	stop
	end
