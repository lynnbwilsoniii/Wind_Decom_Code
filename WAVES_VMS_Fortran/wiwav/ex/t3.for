	program t3
	structure /lb/
	   union
	   map
	      integer*4	i4val
	   end map
	   map
	      byte 	b3,b2,b1,b
	   end map
	   end union
	end structure
	record /lb/ lb
	integer*4	i,j,k
	logical*4	lx

	k = '000001ff'x
	lb.b3 = '33'x
	lb.b2 = '22'x
	lb.b1 = '11'x
	lb.b  = 'ab'x

  1	format(1x,a,z8.8)
	type 1, 'i4val = ', lb.i4val
	j = lb.i4val

	lx = '000001FF'x .and. j
	type 1, ' lx = ', lx

	i = and('000001ff'x, j)
	type 1, ' i = ', i

	i = lb.i4val .and. k
	type 1, ' i = ', i

	i = lb.i4val .or. '00ff00FF'x
	type 1, ' i = ', i

	i = .not. lb.i4val
	type 1, ' i = ', i

	i = '00f0'x
	type 1, ' i = 00f0x = ', i
	i = 'f0'x
	type 1, ' i = f0x = ', i
	stop
	end
