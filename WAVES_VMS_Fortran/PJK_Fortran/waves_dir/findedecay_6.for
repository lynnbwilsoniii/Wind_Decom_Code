
 	PROGRAM EDECAY
C
C	ELECTROSTATIC DECAY OF LANGMUIR WAVE
C
	REAL K0,K1,K2,K1P,K1M,K2P,K2M
	DATA TWOPI /6.2831865/
C
	FP = 22500.
	EB = 5.			! KEV
   	TE = 10.					! EV		
   	TI = 10.					! EV		
C	Bardwell and Goldman, Ap.J. 209,912, (1976)
C	Weatherall, Goldman and Nicholson, Ap.J.  (1981)
	FP = 90.E6
	EB = 27.5			! KEV
   	TE = 140.					! EV		
   	TI = 140.					! EV		
   	VB = SQRT(2.*EB/.511E3)*3.E8			! M/SEC
   	VS = SQRT((TE + 3.*TI)/.511E6/1836.)*3.E8       ! M/SEC
   	VT2 = TE*(3.E8)**2/.511E6
C
  	K0 = TWOPI*FP/VB				! m^-1
  	W02 = (TWOPI*FP)**2 + 3.*VT2*K0**2
   	W0 = SQRT(W02)					! radians/sec
   	print*,'k0,w0',k0,w0
C
C	QUADRATIC FOR K1 = DAUGHTER LANGMUIR WAVE, ONE IS ALWAYS SAME AS K0
C
   	AA = 3.*VT2 - VS**2
   	BB = -2.*VS*(W0 - K0*VS)
  	CC = (TWOPI*FP)**2 - (W0-K0*VS)**2
   	DISC = BB**2 - 4.*AA*CC
   	K1P = .5*(-BB+SQRT(DISC))/AA	
   	K1M = .5*(-BB-SQRT(DISC))/AA	
C
   	PRINT*,AA,BB,CC,DISC
   	PRINT*,'k1+, k1-',K1P,K1M
   	K2M = K0 - K1M
   	K2P = K0 - K1P
	WPL = VS*K2P
	WMN = VS*K2M
   	PRINT*,'k2+,w+,k2-,w-',K2P,WPL,K2M,WMN
   	STOP
   	END
