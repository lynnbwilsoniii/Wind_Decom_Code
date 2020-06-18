	program t
	integer*4	i,j,k
	byte		b0, b1, b2

	i = 0
	i = ibset(i,8)
	write(6,'(1x,i,1x,z8.8)') i, i

	stop
	end
