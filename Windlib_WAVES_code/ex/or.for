! or.for - tests bit functions

	program or
	integer*4       i,j,k

	i = '12345678'x
	j = 'F00001FF'X
	j = '0777'o
	type 1, i,j,k
	k = and(i,j)
	type 1, i,j,k

	k = ior(i,j)
	type 1, i,j,k

	k = i .or. j
	type 1, i,j,k

  1	format(1x,'i=',z8.8, '  j=',z8.8, '  k=',z8.8)
	stop
	end
