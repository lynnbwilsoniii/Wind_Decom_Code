
	PROGRAM BPREAMP
C
C	THIS PROGRAM CALCULATES THE SEARCH COIL RESPONSE, 
C	IT WRITES A FILE FOR011.DAT TO BE PLOTTED BY MONGO
C
	COMPLEX VIN,VAC,BXPA,BYPA,BZPA
	DATA TWOPI /6.2831853/
	DATA FHZMIN,FHZMAX,NF /.3,5.E3,120/
C
	FRAT = ALOG(FHZMAX/FHZMIN)/(NF-1)
	FRAT = EXP(FRAT)
	FHZ = FHZMIN
	DO 100 NN = 1,NF
	  W = TWOPI*FHZ
	  VAC = BXPA(FHZ)
C
C	WRITE OUTPUT FILE
C
 	  VR = VAC
	  VI = AIMAG(VAC)
	  GAIN = CABS(VAC)
	  PHAC = 57.29578*ATAN2(VI,VR)
C
	  WRITE(11,1001) FHZ,VAC,GAIN,PHAC
 1001	  FORMAT(E13.4,3E12.3,F7.1)
	  PRINT*,FHZ,VAC,GAIN,PHAC
	  FHZ = FHZ*FRAT
 100	CONTINUE
	STOP
	END
	SUBROUTINE MGO
C	THIS IS  PREAMP.MGO
C	STORED HERE FOR REFERENCE--TO BE RUN INTERACTIVELY
C
C	DATA FOR011.DAT
C	FORMAT (E13.4,3F7.3,F6.1,2X,3F7.3,F6.1)
C	XCOLUMN 1
C	XLOGARITHM 1
C	YCOLUMN 4                 ! GAIN AC
C	LIMITS  -0.5  6.   0.  4.
C	CONNECT
C	BOX
C	XCOLUMN 1
C	XLOGARITHM 1
C	YCOLUMN 8                 ! GAIN DC
C	LIMITS 0.  6.  0.  5.
C	CONNECT
C	XLABEL LOG FREQUENCY (HZ)
C	YLABEL GAIN
C	ID
	RETURN
	END
