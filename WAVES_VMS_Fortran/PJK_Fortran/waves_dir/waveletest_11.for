	PROGRAM WAVELETEST
C
	REAL REALP(100),IMAGP(100)
	DATA TWOPI /6.28318531/
C
C	GENERATE A WAVELET
C
	NUMBER = 50
	OMEGA = TWOPI
	GAMMA = 2.*(OMEGA/5.336)**2
	print*,'w,gamma',omega,gamma
	DT = .1
	CALL WVLTGEN(OMEGA,GAMMA,NUMBER,DT,REALP,IMAGP)
	DO N = 1,NUMBER+1
	  PRINT*,N,OMEGA*N*DT,REALP(N),IMAGP(N)
	  WRITE(33,*) N,OMEGA*N*DT,REALP(N),IMAGP(N)
	ENDDO
C
C	GENERATE SINE WAVES FOR TESTING
C
	STOP
	END
	SUBROUTINE WVLTGEN(OMEGA,GAMMA,NUMBER,DT,REALP,IMAGP)
C
C	GENERATES REAL AND IMAGINARY PARTS OF A MORLET WAVELET.  I USE
C		THE NOTATION OF TEOLIS "COMPUTATIONAL SIGNAL PROCESSING
C		WITH WAVELETS, EXCEPT THAT HIS GAMMAb IS MY GAMMA AND
C		HIS GAMMAc IS MY OMEGA
C	HERE DT IS THE TIME BETWEEN SAMPLES, AND NUMBER IS THE TOTAL
C	NUMBER OF POINTS (TRUNCATION),  NUMBER IS TURNED INTO THE
C		NEXT LARGEST ODD NUMBER
C
	REAL REALP(100),IMAGP(100)
	DATA TWOPI /6.28318531/
C	
	NO = 2*(NUMBER/2) + 1
	N2 =  (NO-1)/2	
	N1 = -N2
	DO N = N1,N2
	  GAUSSF = EXP(-(N*DT)**2/GAMMA)/SQRT(.5*TWOPI*GAMMA)
	  ARG = OMEGA*N*DT
	  REALP(N-N1+1) = GAUSSF*COS(ARG)
	  IMAGP(N-N1+1) = GAUSSF*SIN(ARG) 
	ENDDO
	RETURN
	END