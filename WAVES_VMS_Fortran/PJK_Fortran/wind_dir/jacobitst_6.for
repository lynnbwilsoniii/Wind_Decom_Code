	program jacobitst
c
c	what's a row and what's a column?
c	this shows that, in evect(i,j) i is component, j is vector #
c
	real aa(3,3),evect(3,3),eval(3)
	data aa /9*0./
c
	aa(1,1) = 1.
	aa(2,2) = 2.
	aa(3,3) = 1.
	aa(1,2) = .5
	aa(2,1) = aa(1,2) 
	call jacobi(aa,3,3,eval,evect,nrot)
	print*,eval
	do j = 1,3
	  print*,evect(1,j),evect(2,j),evect(3,j)
	enddo
	call eigsrt(eval,evect,3,3)
	print*,eval
	do j = 1,3
	  print*,evect(1,j),evect(2,j),evect(3,j)
	enddo
	stop
	end	
