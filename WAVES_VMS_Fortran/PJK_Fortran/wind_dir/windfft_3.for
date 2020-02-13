
	SUBROUTINE WINDFFT(IFFTCH)
C
C	THIS WINDFFT DOES FFT AS IN FLIGHT SOFTWARE
C
	COMMON /WINBLK/ WINDOW(1024)
	common /fftblk/ nexpt(1024),mantissa(1024),ipwrw(1024),data(1024)
c
	REAL PWRW(1024),PWRNW(1024),DATA,AR(1024),OFFS(4,10)
	DATA TWOPI /6.2831853/
	DATA WINDOW /1024*1./
C
C	OFFSETS IN FLIGHT PROMS, ACCORDING TO RED NOTEBOOK
C
	DATA OFFS /2048. ,2050. ,2046. ,2048.,   ! CH 1 GN STP 0-3
     2             2048. ,2050. ,2046. ,2051.,   ! CH 2 GN STP 0-3
     3             2048. ,2082. ,2041. ,2043.,   ! etc.
     4             2048. ,2078. ,2042. ,2046.,
     5             2048. ,2048. ,2048. ,2047.,
     6             2048. ,2075. ,2043. ,2045.,
     7             2048. ,2048. ,2048. ,2037.,
     8             2048. ,2048. ,2048. ,2040.,
     9             2048. ,2048. ,2048. ,2036.,
     X             2048. ,2048. ,2287. ,2048./
C
	NPROS = 1024
	NFRQ = NPROS/2
C
	IWINDW = 1
	CALL FWINDW(IWINDW)
C
	datamax = 0.
	DO IK = 1,1024
	  NTEMP = MANTISSA(IK).AND.4095
	  NEX = NEXPT(IK).AND.3
	  TEMP = -(NTEMP-OFFS(NEX+1,IFFTCH))
	  DATA(IK) = TEMP*(-16.3)**NEX
	  datamax = amax1(abs(data(ik)),datamax)
	ENDDO
C	print*,'max data',datamax
C
	DO N = 1,NPROS
	   AR(N) = WINDOW(N)*DATA(N)
	ENDDO
        CALL REALFT(AR,NFRQ,1)
C?	  FFTAVR = AR(1)/1024.
C?	  AVR = AR(1)
	  AVR = AR(1)
C	
	IF(AVR.GT.0.) THEN
	    IPWRW(1) = 40.*ALOG10(AVR) - 124.
	ELSE
	    IPWRW(1) = 0.
	ENDIF
C
C	CALCULATE POWER 
C
c	114.4 is 20.*log10(1024**2/2), to make fasfor normalization agree
c	with the flight norm which subtracts 124.
C
	DO N = 2,NFRQ+1
	  PWRW(N) = AR(2*N-1)**2 + AR(2*N)**2
	  IF(PWRW(N).GT.0.) THEN
	    IPWRW(N) = 20.*ALOG10(PWRW(N)) - 124.   ! .5 db steps,fl bias
	  ELSE
	    IPWRW(N) = 0.
	  ENDIF
	ENDDO
	RETURN
	END
	SUBROUTINE FWINDW(IWINDW)
C
	COMMON /WINBLK/ WINDOW(1024)
	DATA TWOPI /6.2831853/
C
	GO TO (100,200,300) IWINDW+1
	PRINT*,'REQUESTED WINDOW DOES NOT EXIST'
 100	CONTINUE
	PRINT*,'IWINDW =',IWINDW,'  NO WINDOWING'
	DO N = 1,1024
	  WINDOW(N) = 1.
	ENDDO
	RETURN
 200	CONTINUE
C	PRINT*,'IWINDW =',IWINDW,'  HAMMING'
	ALPHA = .54
	GO TO 310
 300	CONTINUE
	ALPHA = .5
	PRINT*,'IWINDW =',IWINDW,'  HANNING'
 310	CONTINUE
	SUMSQ = 0.
	DO N = 1,1024
	  WINDOW(N) = ALPHA + (ALPHA-1.)*COS(TWOPI*(N-1)/1024.)
	  SUMSQ = SUMSQ + WINDOW(N)**2 
	ENDDO
	RMS = SQRT(SUMSQ/1024.)
	DO N = 1,1024
	  WINDOW(N) = WINDOW(N)/RMS
	ENDDO
	RETURN
	END
