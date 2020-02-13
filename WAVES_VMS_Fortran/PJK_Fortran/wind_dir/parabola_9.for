	program parabola
c
c	debyel is actually sqrt(3 kT / m)
c		for kt = 12 eV, debyel = 8.4 e-3
	debyel = 8.4 e-3
	fact = 45.
	npt = 100
	do n = 0,npt
	  waven = fact*(2.*n - npt)/npt
	  f1 = sqrt(1. + 3.*(debyel*waven)**2)
	  f2 = sqrt(1. + waven**2)
	  write(45,*) debyel*waven,f1,f2
	  print*, debyel*waven,f1,f2
	enddo
	stop
	end
