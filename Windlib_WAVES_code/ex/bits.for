! bits.for - 
	program bits
	integer*4	i
	integer*4	p
	parameter	(p='0000ffff'x)

  1	format(1x,a,1x,z8.8)
	type 1, ' p = ', p
	type 1, ' -1 = ', 'ffffffff'x
	type 1, ' max int = ', '7fffffff'x
	type 1, ' -1 = ', -1

	i = 'FFFFFFFF'x
	type 1, 'FFFFFFFFx ', i

	i = '00ff00ff'x
	type 1, '00ff00ffx ', i

	i = '1000ffff'x
	type 1, '1000ffffx ', i

	i = 'f000ffff'x
	type 1, 'f000ffffx ', i

	i = '000cffff'x
	type 1, '000cffffx ', i

	i = '0001ffff'x
	type 1, '0001ffffx ', i

	i = '00012345'x
	type 1, '00012345x ', i

	i = '1005ffff'x
	type 1, '1005ffffx ', i

	i = '0120abcd'x
	type 1, '0120abcdx ', i

	stop
	end
