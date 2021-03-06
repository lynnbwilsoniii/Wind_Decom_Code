

	PROGRAM PREAMP5
C
C	THIS PROGRAM CALCULATES THE TDS OR FFT RESPONSE, 
C	IT WRITES A FILE FOR011.DAT TO BE PLOTTED BY MONGO
C	DIFFERS FROM PREVIOUS VERSIONS IN USING TDS_PACKAGE
C	DIFFERS FROM PREAMP4 IN USING NEW COMPLEX PREAMP GAINS
C
	COMPLEX CGAIN,PREAMP
	CHARACTER*50 CH(20)
	REAL FRGN(2,500)
C	COMMON /GAINBLK/ PHASE,CGAIN
C
	DATA TWOPI /6.2831853/
	DATA FHZMIN,FHZMAX,NF /.3,200.E3,130/
C
C	IPA = 1 IS EX AC, 2 IS EY, 3 IS EZ
C
	ISYS = 0     !  0 IS TDS, 1 IS FFT
	IPA = 0
C
	DO IPA = 1,9
C
	  TMONGO = SECNDS(0.)         
 987	  CONTINUE         
	  DELTA = SECNDS(TMONGO)         
	  IF(DELTA.LT.2.) GO TO 987         
C         
	  FRAT = ALOG(FHZMAX/FHZMIN)/(NF-1)
	  FRAT = EXP(FRAT)
	  FHZ = FHZMIN
	  VIN = 1.
	  DO 100 NN = 1,NF
	  W = TWOPI*FHZ
C
	  CGAIN = PREAMP(IPA,FHZ)
	  IF(ISYS.EQ.0) CGAIN = CGAIN*TDSDET(IPA,FHZ)
	  IF(ISYS.EQ.1) CGAIN = CGAIN*FFTAMP(IPA,FHZ)
	  GAINDC = CABS(CGAIN)
 	  GNIP = AIMAG(CGAIN)
	  GNRP = CGAIN
	  PHDC = ATAN2D(GNIP,GNRP)
C
C	  IF(FHZ.LT.25.)PRINT*,FHZ,CGAIN,GAINDC,PHDC
C	  IF(FHZ.GT.25.) STOP
	  WRITE(11,1001) FHZ,GAINDC,PHDC
 1001	  FORMAT(E13.4,F7.3,F7.1)
	  FHZ = FHZ*FRAT
 100	  CONTINUE
	  CLOSE(UNIT=11)
	  ICH = 1
	  CH(1) = 'PRINTER 2'
C	  CH(1) = 'terminal 3'
	  CH(2) = 'ERASE'
	  CH(3) = 'DATA FOR011.DAT'
	  CH(4) = 'TICKSIZE -1  0  .2 1.'
	  IF(IPA.GT.2) CH(4) = 'TICKSIZE -1  0  .1 .5'
	  IF(IPA.GT.6) CH(4) = 'TICKSIZE -1  0  .2 1.'
C	  IF(IACDC.EQ.1) CH(4) = 'TICKSIZE -1  0  .02  .1'
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
C
	  IF(IPA.EQ.1) THEN
	    ICH = ICH+1
	    CH(ICH) = 'RELOCATE .25 .35'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EX AC'
	  ENDIF	
C
	  IF(IPA.EQ.2) THEN
	    ICH = ICH+1
	    CH(ICH) = 'RELOCATE .25 .25'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EY AC'
	  ENDIF	
C
	  IF(IPA.EQ.3) THEN
	    ICH = ICH+1
	    CH(ICH) = 'RELOCATE .25 .65'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EZ AC'
	  ENDIF	
C
	  IF(IPA.EQ.4) THEN
	    ICH = ICH+1
	    CH(ICH) = 'RELOCATE .25 .35'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EX DC'
	  ENDIF	
C
	  IF(IPA.EQ.5) THEN
	    ICH = ICH+1
	    CH(ICH) = 'RELOCATE .25 .25'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EY DC'
	  ENDIF	
C
	  IF(IPA.EQ.6) THEN
	    ICH = ICH+1
	    CH(ICH) = 'RELOCATE .25 .65'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL EZ DC'
	  ENDIF
C
	  IF(IPA.EQ.7) THEN
	    ICH = ICH + 1
	    CH(ICH) = 'RELOCATE .25 .35'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL BX '
	  ENDIF	
C
	  IF(IPA.EQ.8) THEN
	    ICH = ICH + 1
	    CH(ICH) = 'RELOCATE .25 .25'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL BY '
	  ENDIF	
C
	  IF(IPA.EQ.9) THEN
	    ICH = ICH + 1
	    CH(ICH) = 'RELOCATE .25 .65'
	    ICH = ICH+1
	    CH(ICH) = 'LABEL BZ '
	  ENDIF	
C
	  ICH = ICH+1
	  IF(ISYS.EQ.0) CH(ICH) = 'LABEL , TDS'
	  IF(ISYS.EQ.1) CH(ICH) = 'LABEL , FFT'
C
	  ICH = ICH+1
	  CH(ICH) = 'HARDCOPY'
	  ICH = ICH+1
	  CH(ICH) = 'ID'     		!  18?
	  ICH = ICH+1
	  CH(ICH) = 'END'     		!  19?
	  ICHT = ICH
C
	  CALL MONGO(ICHT,CH,500,2,FRGN)
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
