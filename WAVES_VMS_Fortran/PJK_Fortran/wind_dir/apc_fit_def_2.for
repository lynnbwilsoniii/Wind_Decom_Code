! apc_fit_def.for - data definitions for apc_fit.

	parameter	sz_x_dim=120
	integer*4	x_peak(sz_x_dim)
	integer*4	y_peak(sz_x_dim)
	integer*4	x_dc(sz_x_dim)
	integer*4	y_dc(sz_x_dim)
	integer*4	z_dc(sz_x_dim)
	integer*4	apc_dac(sz_x_dim)
	integer*4	x_resistor(sz_x_dim)
	integer*4	y_resistor(sz_x_dim)
	integer*4	x_apc(sz_x_dim)
	integer*4	y_apc(sz_x_dim)
	integer*4	z_apc(sz_x_dim)
	real*4		x(sz_x_dim)
	real*4		y(sz_x_dim)
	real*4		av(5,3,40)  ! 1st arg = res, 2nd = ant, 3rd = samp 
	real*4		pk(5,3,40)  !		"
	real*4		drive(5,3,40)	!	"
	real*4		wtav(5,3,40)	!	"
	real*4		wtpk(5,3,40)	!	"
	integer*4	count(5,3)	! 1st arg = res, 2nd = ant
	real*4		resis(5,3)	!	"
	integer*4	major1
	integer*4	major2
	character*128	file
	integer*4	terminal_output
	integer*4	rassign


	common /apc_blk_0/ x_peak, y_peak, x_dc, y_dc, z_dc,
	1		apc_dac, x_resistor, y_resistor,
	1		x_apc, y_apc, z_apc,
	1		terminal_output, major1, major2,
	1		av,pk,wtav,wtpk,count,drive,resis,
	1		x, y, file


