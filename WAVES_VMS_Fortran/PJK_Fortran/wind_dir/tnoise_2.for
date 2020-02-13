	program tnoise
	do n = 1,100
	  x = .05*n
	  y = x*exp(-x**2)
	  z = 10.*alog10(x)
	  write(67,*) x,z,y
	enddo
	stop
	end

