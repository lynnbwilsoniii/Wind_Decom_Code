        WRITE (7,350) 
  999	format(5(f16.8,x))
c
c
c
c	..attempt to calculate density fluctuations from the continuity 
c		equation	11/23/92
c
c
c	..fixed up on 6/18/93
c
c
	emag = sqrt(e(1)**2. + e(2)**2. + e(3)**2. + e(4)**2. + e(5)**2.
	1		+ e(6)**2.) 
c
c	print*,'emag = ',emag		!this should be 1
c
	del_nr = 0
	del_ni = 0
c
c********************
c	ALL, N = 1,NC
c
c	..first calculate the complex matrix product
c
c
	DO N = 1,NC
	   do jj=1,3
		del_nr(N) = del_nr(N) + 			!real part
     1		XKX*(dieli(N,1,jj)*E(jj) + dielr(N,1,jj)*E(jj+3)) 
     1		+ XKZ*(dieli(N,3,jj)*E(jj)+ dielr(N,3,jj)*E(jj+3))
c
		del_ni(N) = del_ni(N) +				!imaginary part 
     1		XKX*(dielr(N,1,jj)*E(jj) - dieli(N,1,jj)*E(jj+3))
     1		+ XKZ*(dielr(N,3,jj)*E(jj) - dielr(N,3,jj)*E(jj+3))
c
	   end do
c
c	..then include the real scalars
c
c
	del_nr(N) = del_nr(N)*ratiom(N)/(dens(N)*abs(ratiom(N))
     1		*(WR**2.))
	del_ni(N) = del_ni(N)*ratiom(N)/(dens(N)*abs(ratiom(N))
     1		*( WR**2.))
	ENDDO
c
c****************

