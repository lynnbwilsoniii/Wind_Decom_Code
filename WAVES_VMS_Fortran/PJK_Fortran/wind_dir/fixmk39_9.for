	program fixmk39
C
C	TO DO A MORE NARROW SELECTION ON THE RESULTS OF A WHISTLER SEARCH,
C		MAKEFILE39.RESULTS
C
	CHARACTER*2 	EVT
	CHARACTER*120	JUNK
	INTEGER*4	SCETI4(2),ERTDAY
C
	OPEN(UNIT=90,FILE='MAKEFILE39.RESULTS',STATUS='OLD')
	NCOUNT = 0
	READ(90,1001) JUNK
	READ(90,1001) JUNK
 1001	FORMAT(A)
C
 100	CONTINUE
        READ(90,1054,END=200) SCETI4,NO_EVT,ERTDAY,EVT,MAXCH1,MAXCH2,
     1    E1MAX,E2MAX,EEIG1,EEIG2,FREQKHZ,FP,FCE
 1054     FORMAT(I10,I7,I10,I3,A2,2I4,4E11.3,3F8.2)
C
C	CRITERIA
C
	IF(FREQKHZ.GT.FCE) GO TO 100
	IF(FCE.LT.1.4) GO TO 100
	IF(E1MAX.LT..05) GO TO 100
	IF(E2MAX.LT..05) GO TO 100
        WRITE(91,1054) SCETI4,NO_EVT,ERTDAY,EVT,MAXCH1,MAXCH2,
     1          E1MAX,E2MAX,EEIG1,EEIG2,FREQKHZ,FP,FCE
	NCOUNT = NCOUNT+1
	GO TO 100
 200	CONTINUE 
	PRINT*,'TOTAL NUMBER ',NCOUNT
	stop
	end
