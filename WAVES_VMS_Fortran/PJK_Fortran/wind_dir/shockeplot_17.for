	PROGRAM  SHOCKEPLOT
C
C	PLOTS THINGS DERIVED FROM SHOCK DATA  AS IN  FILE 'SHOCKE.DATA'
C
	CHARACTER*80 JUNK
	REAL  MA,MS, PDATA(500,3)
	INTEGER*4  NEVT,YYMMDD,HHMMSS
C
	OPEN(UNIT=72,FILE='SHOCKE.DATA',STATUS='OLD',READONLY)
	
C
 	READ(72,1001,END=200) JUNK
	PRINT*,JUNK
	IN = 0
 1001	FORMAT(A80)
  100	READ(72,*,END=200) NEVT,YYMMDD,HHMMSS,BDOTN,MA,MS,DENS,
     1		VDOTB,VEL,CH1AV,CH1PK,CH2AV,CH2PK
 	PRINT*, NEVT,YYMMDD,HHMMSS,BDOTN,MA,MS,DENS,
     1		VDOTB,VEL,CH1AV,CH1PK,CH2AV,CH2PK
	IN = IN+1
	PDATA(IN,1) = SQRT(CH1PK**2 + CH2PK**2)
	PDATA(IN,2) = SQRT(CH1AV**2 + CH2AV**2)
C1	PDATA(IN,3) = BDOTN
C2	PDATA(IN,3) = MA
C3	PDATA(IN,3) = MS
C4	PDATA(IN,3) = ABS(VDOTB)
	PDATA(IN,3) = DENS
	GO TO 100
  200	CONTINUE
	CALL MGPLOT(1,IN,PDATA)
	STOP
	END
	SUBROUTINE MGPLOT(IHARD,NL,PDATA)
C
	DIMENSION PDATA(500,3)
	CHARACTER*20 CH(20)
C
	CH(1) = 'TERMINAL 3'
	IF(IHARD.EQ.1) CH(1) = 'PRINTER 2'
	CH(2) = 'LINES 1 499'
	CH(3) = 'XCOLUMN 3'
	CH(4) = 'YCOLUMN 2'
	CH(5) = 'LIMITS'
	CH(6) = 'PTYPE 6 0'
	CH(7) = 'POINTS'
	CH(8) = 'BOX'
C1	CH(9) = 'XLABEL B dot N'
C2	CH(9) = 'XLABEL M alfven'
C3	CH(9) = 'XLABEL M acoustic'
C4	CH(9) = 'XLABEL V dot B'
	CH(9) = 'XLABEL DENSITY'
	CH(10) = 'YLABEL RMS E (V/M)'
	CH(11) = 'ID [WIND]SHOCKEPLOT  SHOCKE.DATA'
	CH(12) = 'END'
      IF(IHARD.EQ.1) THEN
	CH(12) = 'HARDCOPY'
	CH(13) = 'END'
      ENDIF
	CALL MONGO(12+IHARD,CH,500,3,PDATA)
C
	CH(4) = 'YCOLUMN 1'
	CH(10) = 'YLABEL PEAK E (V/M)'
	CALL MONGO(12+IHARD,CH,500,3,PDATA)
      RETURN
      END
