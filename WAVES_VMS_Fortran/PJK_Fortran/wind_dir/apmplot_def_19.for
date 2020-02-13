! apc_fit_def.for - data definitions for apc_fit.

	parameter	sz_x_dim=4000
	integer*4	x_peak
	integer*4	y_peak
	integer*4	x_dc
	integer*4	y_dc
	integer*4	z_dc
	integer*4	apc_dac(sz_x_dim)
	integer*4	x_resistor
	integer*4	y_resistor
	integer*4	z_resistor
	integer*4	x_apc(sz_x_dim)
	integer*4	y_apc(sz_x_dim)
	integer*4	z_apc(sz_x_dim)
	real*4		xtime(sz_x_dim)
	real*4		drive(sz_x_dim)	
	real*4		onoff(sz_x_dim)	
	real*4		av(3,sz_x_dim)  ! 1st arg = ant, 2nd = samp 
	real*4		pk(3,sz_x_dim)  !		"
	integer*4	resistor(3,sz_x_dim)
	integer*4	count(3)
	character*128	file
	integer*4	terminal_output
	integer*4	rassign
	integer*4 	iangle


	common /apc_blk_0/ x_peak, y_peak, x_dc, y_dc, z_dc,
	1		apc_dac, x_resistor, y_resistor,z_resistor,
	1		x_apc, y_apc, z_apc,resistor,
	1		terminal_output, onoff,
	1		av,pk,count,drive,xtime,file,iangle

