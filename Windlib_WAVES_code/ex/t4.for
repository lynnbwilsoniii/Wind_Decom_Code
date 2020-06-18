	integer*4	i,j
	logical*4	x

	i = '0000f0f0'x
	j = '12345678'x
	x = ( i .and. j) .gt. 0
	type *, x

	stop
	end
