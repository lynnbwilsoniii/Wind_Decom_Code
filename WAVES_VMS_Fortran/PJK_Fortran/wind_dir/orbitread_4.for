	PROGRAM ORBITREAD
C
C	READS ORBIT TABLE AND DOES WHATEVER WITH IT
C
	INTEGER YYYYMMDD,HR
C
	OPEN(UNIT=78,FILE='ORBIT.TABLE',STATUS='OLD')
100	READ (78,*) DAYNO,YYYYMMDD,HR,X,Y,Z,R
	IF(R.LE.15.) WRITE (51,*) DAYNO,YYYYMMDD,X,R
	IF(DAYNO.LT.7650.) GO TO 100
	STOP
	END
