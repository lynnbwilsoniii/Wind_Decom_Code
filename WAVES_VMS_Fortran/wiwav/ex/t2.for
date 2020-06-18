! t2.for - tests zext function
	integer*4	i,j,k
	byte		b
	logical*1	lx

	b = '30'x
	lx= '80'x
	i = zext(b)
	j = zext(lx)
	type 1, 'b,i,...lx,j=', b, i, lx, j

	i = i .and. 'f0'x
	j = j .and. 'f0'x
	type 1, 'b,i,...lx,j=', b, i, lx, j

	i = 'f0f0f0ff'x
	j = i .and. 'ffff'x
	type 2, 'i,j=',i,j
	type *, ' '

	k = '01ff'x
	type *, 'k = ', k
	
	stop
  1	format(1x,a, 1x,z8.8, 1x,z8.8, 1x,'... ', z8.8, 1x, z8.8)
  2	format(1x,a, 1x,z8.8, 1x,z8.8)
	end
