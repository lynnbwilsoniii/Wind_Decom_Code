	PROGRAM BWTRY
C
	DATA F0,BW /15000., 100./
	REAL SIG(500)
C
	NPT = 100
	MID = NPT/2
	DO N = 1,NPT
	  F = F0 + .1*(N-MID)*BW
C	  SIGT = 1./((F-F0)**2 + BW**2)
	  SIGT = BW**2/((F-F0)**2 + BW**2)
	  SIGT = EXP(-1./SQRT(SIGT))
	  PRINT*,F,SIGT
	  SIG(N) = 10.*ALOG10(SIGT)
	  WRITE(49,*) F,SIGT,SIG(N)
	ENDDO
	STOP
	END 
