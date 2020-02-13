	program realftest
c
c
c
	common /winblk/ window(1024)
	dimension data(2050),pwr(513)
	complex cgain,cdata(2050)

	data twopi /6.2831853/
c
	cycles = 10.			! number of cycles in full sample
	period = 1024./cycles			! period in samples
	amp = 1000.
c
c	construct a sine wave
c
	do i = 1,1024
		data(i) = amp*sin(twopi*(i-1)*cycles/1024. + .125*TWOPI)
c		data(i) = amp*sin(twopi*(i-1)*cycles/1024.)
	   write(22,*) i,data(i)
	enddo
c
C	call fwindw(1)
C	do i = 1,1024
C		data(i) = window(i)*data(i)
C	enddo
	call realft(data,512,1)
	do i = 1,512
		data(i) = data(i)/512.
		data(i+512) = data(i+512)/512.
		pwr(i) = sqrt(data(2*i-1)**2 + data(2*i)**2)/512.
	   write(23,*) i,pwr(i)
	enddo
	print*,'data, fourier coefficients'
	print*,(data(j),j=1,25)
	xph = cosd(45.)
	cgain = cmplx(xph,xph)            ! gain with +45 deg. phase shift
	do j = 1,513
	  cdata(j) = cmplx(data(2*j-1),data(2*j))
	  cdata(j) = cdata(j)/cgain	
        enddo
	print*,'cdata'
	print*,(cdata(j),j=1,13)
c
c	test inverse transform
c
	call realft(data,512,-1)
	   write(24,*) (i,data(i),i=1,1024)
c
C	DO J = 1,1025
C	  WRITE(67,*) J,DATA(J),WINDOW(J)
C	ENDDO
	stop
	end
