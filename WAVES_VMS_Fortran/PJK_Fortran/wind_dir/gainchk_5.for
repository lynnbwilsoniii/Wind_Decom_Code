	program gainchk
c
C	common /gainblk/ phase,gain
	complex gain,PREAMP,FFTAMP
c
	do i = 1,200
	  f = .1*i
	  ich = 3	
	  IPA = 4		! EXDC
	  GAIN = PREAMP(IPA,F)
	  GAIN = GAIN*FFTAMP(ICH,F)
	  WRITE(23,*) f,CABS(GAIN)
	  print*,f,CABS(GAIN)
	enddo
	stop
	end
