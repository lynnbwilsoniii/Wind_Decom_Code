	PROGRAM SPITZHARM
C
C
	OPEN(UNIT=89,FILE='SPITZHARM.DAT',STATUS='OLD',READONLY)
 100	READ(89,*,END=200) X,TABLE1,TABLE2
	FMAXW = EXP(-X**2)
	F1 = TABLE1*FMAXW
	F2 = TABLE2*FMAXW
	WRITE(98,*) X,TABLE1,TABLE2,F1,F2
	GO TO 100
 200	CONTINUE
	STOP
	END
