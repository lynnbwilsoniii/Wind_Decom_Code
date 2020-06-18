
! msks.for - tests some bit masking and shifting stuff
	programs msk
	integer*4	i,j,k,val

	i = 'FFFFFFFF'X
	i = '37777777777'o
	i = -1
	j = ishft(i, 16)
	k = j .and. 67489
	val = 1952 .or. k

	type 1, i,j,k,val
  1	format(1x,'i=',z8.8, ' j=',z8.8, ' k=',z8.8, ' val=',z8.8)

	stop
	end
