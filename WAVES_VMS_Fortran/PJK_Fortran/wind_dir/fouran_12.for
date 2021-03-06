	PROGRAM FOURAN
C
C	FOURIER ANALYSES SIGNAL PRODUCED BY ANTPHOT3
C	FIRST I MADE A COPY OF ANTPHOT3 AND CHANGED IT TO
C	CALCULATE 512 POINTS PER REV, AND TO PRINT OUT
C	F(1) - F(3), THE DIFFERENCE POTENTIAL.
C	THEN I EDITED THE LOG FILE TO PRODUCE ANTPHOT.DAT,
C	A DATA FILE OF THE OUTPUT
C
	DIMENSION AR(520),AI(520),PWR(250)
	DATA SPINP /3./
C
	OPEN(UNIT=12,FILE='ANTPHOT.DAT',STATUS='OLD')
 101  FORMAT(F8.2,10E12.3)
	PWRT = 0.
	J=0
 100	READ(12,101,END=200) ANG,A,B,C,D,E,F,G,DELV
	J = J+1
	PWRT = PWRT + DELV**2
	AR(J) = DELV
	GO TO 100
 200	CONTINUE
	PWRT = PWRT/512.
	PRINT*,'TIME INTEGRAL',PWRT
	PWRT = 0.
	F0 = 1./SPINP
	CALL FASFOR(9,AR,AI,PWR)
	DO IFR = 1,256
	FREQ = IFR*F0
C
C	CORRECT FOR PREAMP LOW FREQUENCY RESPONSE
	FCRNR = 106.
	PWR(IFR) = PWR(IFR)/(1.+(FCRNR/FREQ)**2)
	FCRNR = 40.
	PWR(IFR) = PWR(IFR)/(1.+(FCRNR/FREQ)**2)
C
	PWRT = PWRT + PWR(IFR)
C	CONVERT TO V**2/HZ
	PWR(IFR) = PWR(IFR)/F0
	AMP = SQRT(PWR(IFR))
C	WRITE A FILE TO BE PLOTTED BY SPECT.MGO
	WRITE(13,101) FREQ,PWR(IFR),AMP
	ENDDO
	PRINT*,'SPECTRUM INTEGRAL',PWRT
	STOP
	END
