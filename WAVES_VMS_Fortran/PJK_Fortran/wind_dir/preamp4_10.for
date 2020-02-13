

	PROGRAM PREAMP4
C
C	THIS PROGRAM CALCULATES THE ELECTRIC PREAMP RESPONSE, 
C	IT WRITES A FILE FOR011.DAT TO BE PLOTTED BY MONGO
C	DIFFERS FROM PREVIOUS VERSIONS IN USING TDS_PACKAGE
C
	COMPLEX Z1,Z2,Z3,Z4,Y1,Y2,VIN,V1,V2,V3,CGAIN,gainac,gaindc
	CHARACTER*50 CH(20)
	COMMON /GAINBLK/ PHASE,CGAIN
C
	DATA TWOPI /6.2831853/
	DATA FHZMIN,FHZMAX,NF /.3,200.E3,130/
C
C	IPA = 1 IS EX, 2 IS EY, 3 IS EZ
C
	IACDC = 0     !  0 IS AC, 1 IS DC
	IPA = 0
C
	DO IPA = 1,3
C
C	  WRITE(11,*) 'PA=',IPA
	  FRAT = ALOG(FHZMAX/FHZMIN)/(NF-1)
	  FRAT = EXP(FRAT)
	  FHZ = FHZMIN
	  VIN = 1.
	  DO 100 NN = 1,NF
	  W = TWOPI*FHZ
C
	  IF(IPA.EQ.1) THEN
C	    VDC = EXDCPA(FHZ)*FFTAMP(1,FHZ)
	    VDC = EXDCPA(FHZ)
	    GAINDC = CGAIN
	    PHDC = 57.29578*PHASE
C	    VAC = EXACPA(FHZ)*FFTAMP(2,FHZ)*FFT_FILTER(2,FHZ)
C	    VAC = EXACPA(FHZ)*FFTAMP(1,FHZ)
	    VAC = EXACPA(FHZ)
	    GAINAC = CABS(CGAIN)
	    PHAC = 57.29578*PHASE
	  ENDIF	
C
	  IF(IPA.EQ.2) THEN
C	    VDC = YPADC(FHZ)*FFTAMP(7,FHZ)*FFT_FILTER(7,FHZ)
C	    VDC = EYDCPA(FHZ)*FFTAMP(7,FHZ)
	    VDC = EYDCPA(FHZ)
	    GAINDC = CGAIN
	    PHDC = 57.29578*PHASE
C	    VAC = YPAAC(FHZ)*FFTAMP(2,FHZ)*FFT_FILTER(2,FHZ)
C	    VAC = EYACPA(FHZ)*FFTAMP(2,FHZ)
	    VAC = EYACPA(FHZ)
	    GAINAC = CGAIN
	    PHAC = 57.29578*PHASE
	  ENDIF	
C
	  IF(IPA.EQ.3) THEN
C	    VDC = ZPADC(FHZ)*FFTAMP(7,FHZ)*FFT_FILTER(7,FHZ)
C	    VDC = EZDCPA(FHZ)*FFTAMP(10,FHZ)*16.3
	    VDC = EZDCPA(FHZ)
	    GAINDC = CGAIN
	    PHDC = 57.29578*PHASE
C	    VAC = ZPAAC(FHZ)*FFTAMP(2,FHZ)*FFT_FILTER(2,FHZ)
C	    VAC = EZACPA(FHZ)*FFTAMP(2,FHZ)
	    VAC = EZACPA(FHZ)
	    GAINAC = CGAIN
	    PHAC = 57.29578*PHASE
	  ENDIF	
C
C
C	  IF(FHZ.LT.1.2E4)WRITE(11,1001)FHZ,VAC,GAINAC,PHAC,VDC,GAINDC,PHDC
	  WRITE(11,1001)FHZ,VAC,GAINAC,PHAC,VDC,GAINDC,PHDC
 1001	  FORMAT(E13.4,3F7.3,F7.1,2X,3F7.3,F7.1)
C	  IF(FHZ.LT.120.)PRINT*,FHZ,VAC,PHAC,VDC,PHDC
	  FHZ = FHZ*FRAT
 100	  CONTINUE
	  CLOSE(UNIT=11)
	  ICH = 1
	  CH(1) = 'PRINTER 2'
C	  CH(1) = 'terminal 3'
	  CH(2) = 'ERASE'
	  CH(3) = 'DATA FOR011.DAT'
	  CH(4) = 'TICKSIZE -1  0  .5 1.'
	  IF(IACDC.EQ.1) CH(4) = 'TICKSIZE -1  0  .02  .1'
	  CH(5) = 'XCOLUMN 1'
	  CH(6) = 'XLOGARITHM 1'
	  CH(7) = 'YCOLUMN 2'
C	  CH(8) = 'LIMITS -1. 3.301  0.  1.265'     ! DC P/A'S LOW END
	  CH(8) = 'LIMITS'
	  CH(9) = 'CONNECT'
	  CH(10) = 'GRID 0'
	  CH(11) = 'EXPAND 1.2'
	  ICH = 12
	  CH(ICH) = 'BOX'
	  ICH = ICH+1
	  CH(ICH) = 'XLABEL FREQUENCY'
	  ICH = ICH+1
	  CH(ICH) = 'YLABEL P/A GAIN'
	  ICH = ICH+3
	  CH(ICH) = 'HARDCOPY'
	  ICH = ICH+1
	  CH(ICH) = 'ID'     		!  18
	  ICH = ICH+1
	  CH(ICH) = 'END'     		!  19
	  ICHT = ICH
C
C	  PLOT AC
C
	  IF(IPA.EQ.1) THEN
	    ICH = 15
	    CH(ICH) = 'RELOCATE .25 1.8'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EX AC'
	  ENDIF	
C
	  IF(IPA.EQ.2) THEN
	    ICH = 15
	    CH(ICH) = 'RELOCATE .25 5.5'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EY AC'
	  ENDIF	
C
	  IF(IPA.EQ.3) THEN
	    ICH = 15
	    CH(ICH) = 'RELOCATE .25 5.'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EZ AC'
	  ENDIF	
C
	  IF(IACDC.EQ.0) CALL MONGO(ICHT,CH,500,2,FRGN)
C
C	  PLOT DC
C
	  CH(7) = 'YCOLUMN 6'
C
	  IF(IPA.EQ.1) THEN
	    ICH = 15
	    CH(ICH) = 'RELOCATE .25 .35'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EX DC'
	  ENDIF	
C
	  IF(IPA.EQ.2) THEN
	    ICH = 15
	    CH(ICH) = 'RELOCATE .25 .25'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EY DC'
	  ENDIF	
C
	  IF(IPA.EQ.3) THEN
	    ICH = 15
	    CH(ICH) = 'RELOCATE .25 .65'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EZ DC'
	  ENDIF	
C
	  PRINT*,'MONGO CALLED FOR',IPA
	  IF(IACDC.EQ.1) CALL MONGO(ICHT,CH,500,2,FRGN)

C
	ENDDO
C
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
