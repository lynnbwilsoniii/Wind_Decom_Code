	program read090
C
	CHARACTER*1 EVENT
	INTEGER SCETI4(2)
C
	OPEN(UNIT=90,FILE='FOR090.DAT',STATUS='OLD',READONLY)
 100	CONTINUE
          READ(90,1011,END=200,IOSTAT=I90ERR) sceti4,NO_EVT,ERTDAY,
     1     EVENT,IGL,
     1     OBSPWR5,PX,R21,R31,COSKB,NFMAX,FREQSP,BW,FREQR,REVS,
     1     FP,FCE,BETA
 1011   format(I10,I7,I10,I3,A2,I5,2E11.3,F6.3,F6.3,F7.3,I5,F6.1,F6.1,
     1    F6.1,F7.1,F6.1,F6.3,F7.3)
C 
	  PRINT*,SCETI4,PX,FREQSP,I90ERR
C
	IF(FREQSP.GT.900..AND.PX.GT..1) THEN
          WRITE(89,1011) sceti4,NO_EVT,ERTDAY,EVENT,IGL,
     1     OBSPWR5,PX,R21,R31,COSKB,NFMAX,FREQSP,BW,FREQR,REVS,
     1     FP,FCE,BETA
	  PRINT*,SCETI4,NO_EVT,ERTDAY
	ENDIF
	GO TO 100
 200	CONTINUE
	CLOSE(UNIT=89)
	CLOSE(UNIT=90)
	STOP
	end
