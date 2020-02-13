	!----------------------------------------------------------------------
	entry adjust_gxy_of_mgoxwin()
	a = gy2 - gy1
	b = a * 0.01
	gy2 = gy2 + b
	b = a * 0.08
	gy1 = gy1 + b
	!
	c = gx2 - gx1
	d = c * 0.12
	gx1 = gx1 + d
	d = c * 0.02
	gx2 = gx2 - d
	return

	!----------------------------------------------------------------------
	entry adjust_gxy_of_hardcopy()
	a = gy2 - gy1
!	b = a * 0.01
!	gy2 = gy2 + b
	b = a * 0.10
	gy1 = gy1 - b
	!
!	c = gx2 - gx1
!	d = c * 0.08
!	gx1 = gx1 + d
!	d = c * 0.04
!	gx2 = gx2 - d
	return


