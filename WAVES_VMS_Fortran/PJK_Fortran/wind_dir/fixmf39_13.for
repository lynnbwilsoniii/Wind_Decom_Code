	program fixmf39
C
C	READS IN THE VOLUMINOUS RESULTS OF MAKEFILE39 AND CHOOSES SOME OF 
C	TO WRITE TO ANOTHER FILE.
C
	INTEGER  SCETI4(2),ERTDAY,NO_EVT,MAXCH1,MAXCH2
	CHARACTER*2  EVENT
	CHARACTER*120 JUNK
C
	OPEN(UNIT=90,FILE='MAKEFILE39.RESULTS',STATUS='OLD')
	READ(90,1001) JUNK
	WRITE(80,1001) JUNK
	READ(90,1001) JUNK
	WRITE(80,1001) JUNK
 1001	FORMAT(A)
 100	CONTINUE
C        READ(90,1054,END=200) SCETI4,ERTDAY,EVENT,NO_EVT,MAXCH1,MAXCH2,
C     1          E1MAX,E2MAX,EEIG1,EEIG2,FREQKHZ,FP,FCE 
        READ(90,1055,END=200) SCETI4,NO_EVT,ERTDAY,EVENT,MAXCH1,MAXCH2,
     1          E1MAX,E2MAX,EEIG1,EEIG2,FREQKHZ,FP,FCE 
 1054   FORMAT(I10,I7,I3,A2,I10,2I4,4E11.3,3F8.2)
 1055   FORMAT(I10,I7,I10,I3,A2,2I4,4E11.3,3F8.2)  
 	
C
C	CRITERIA, LARGE SIGNAL AND F LT .5*FCE  THIS WAS FOR FIRST SET
C
	IWRITE = 0
	IF(E1MAX.GT..2.OR.E2MAX.GT..2) IWRITE = 1
	IF(FREQKHZ.LT..5*FCE) IWRITE = IWRITE+1
C	IF(IWRITE.LE.1) GO TO 100
	IF(IWRITE.GT.1) GO TO 100
C        IF(IWRITE.GT.1) write(80,1055)SCETI4,NO_EVT,ERTDAY,EVENT,MAXCH1,
C     1          MAXCH2,E1MAX,E2MAX,EEIG1,EEIG2,FREQKHZ,FP,FCE
	IWRITE = 0
	IF(E1MAX.GT..1.OR.E2MAX.GT..1) IWRITE = 1
	IF(FREQKHZ.LT..5*FCE) IWRITE = IWRITE+1
        IF(IWRITE.GT.1) write(80,1055)SCETI4,NO_EVT,ERTDAY,EVENT,MAXCH1,
     1          MAXCH2,E1MAX,E2MAX,EEIG1,EEIG2,FREQKHZ,FP,FCE
	GO TO 100
 200	CONTINUE 
C	RELEVANT LINE FROM TDSVIS
C        READ(44,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR
C 1114   FORMAT(I10,I7,I10,I3,A2,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1  
	STOP
	END

