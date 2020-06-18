! car.for - tests character array passing between C and fortran

	program car
	implicit	none
	character*4	c4(8)
	external	c_str !$pragma C (c_str)

	c4(1) = 'abcd'
	c4(2) = 'efgh'
	c4(3) = 'ijkl'
	c4(4) = 'mnoq'

	call c_str(c4)

	type *, '...fortran...'
	type *, c4(1), '.'
	type *, c4(2), '.'
	type *, c4(3), '.'
	type *, c4(4), '.'

	stop
	end
