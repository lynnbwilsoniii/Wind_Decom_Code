	PROGRAM PROCESS90
C
C	PROCESSES FOR090.DAT = RESULTS OF ION ACOUSTIC MISSION_SCAN
C		WITH MAKEFILE5
C
	INTEGER*4 SCETI4(2)
	CHARACTER*120 JUNK
	REAL ANGHIST(100),HIST(100)
	DATA YXRATIO /11.8/
	DATA DANG /2./
C	DATA YXRATIO /8./
	DATA HIST /100*0./
	DATA IHISTMAX /0/
C
	IPT = 0
	NPWR = 0
	RATIOMAX = 0.
	PXTOT = 0.
	PYTOT = 0.
	PMIN = 10000.
	PMAX = 0.
	YXSQ = YXRATIO**2
C	OPEN(UNIT=90,FILE='FOR090.DAT',TYPE='OLD',READONLY)
	OPEN(UNIT=90,FILE='MAKEFILE5.RESULTS',TYPE='OLD',READONLY)
	READ(90,1022) JUNK
	READ(90,1022) JUNK
	READ(90,1022) JUNK
	READ(90,1022) JUNK
	READ(90,1022) JUNK
	READ(90,1022) JUNK
	DO N = 1,50
	  ANGHIST(N) = DANG*(N-1)
	ENDDO
 100	CONTINUE
C
	READ(90,1011,END=200) sceti4,NO_EVT,FREQKHZ,PX,PY,SLOPE,ANGTOB,
     1	     AZMAG,MAGEL,FP,FCE,BETA,XCOUNT,YCOUNT
 1011	format(I10,I7,I10,F7.2,2E10.3,F8.3,2F7.1,I5,F7.3,F7.4,F8.4,2F7.0)
 1022	FORMAT(A120)
	IF(IPT.EQ.0) PRINT*,NO_EVT,PX,PY,ANGTOB
	IPT = IPT+1
	IHIST = ABS(ANGTOB)/DANG + 1.
	IHIST = MIN0(IHIST,100)
	IHISTMAX = MAX0(IHIST,IHISTMAX)
C	REAL ANGHIST(50),HIST(50)
	HIST(IHIST) = HIST(IHIST) + 1.	
	RATIO = PY/PX
	RATIOMAX = AMAX1(RATIOMAX,RATIO)
	PMIN = AMIN1(PX+YXSQ*PY,PMIN)
	PMAX = AMAX1(PX+YXSQ*PY,PMAX)
	PWR = PX + YXSQ*PY
C	IF(RATIO.GT..1) THEN
	IF(PWR.LT.1.5E-4) THEN
	  RATIOMAX = AMAX1(RATIOMAX,RATIO)
	  PRINT*,SCETI4,RATIO
	ELSE
	  NPWR = NPWR+1
	  WT = PX + YXSQ*PY
	  PXTOT = PXTOT+PX/WT
	  PYTOT = PYTOT+PY/WT
	ENDIF
	GO TO 100
C
 200	CONTINUE
	RATIO = PXTOT/PYTOT
	PRINT*,'TOT,RAT',PXTOT,PYTOT,RATIO
	PRINT*,'MAX RATIO',RATIOMAX
	PRINT*,'N,PMIN,PMAX',NPWR,PMIN,PMAX
	PRINT*,'IPT',IPT
	DO N = 1,IHISTMAX
	  WRITE(45,*) ANGHIST(N),HIST(N)
	ENDDO 
	STOP
	END
