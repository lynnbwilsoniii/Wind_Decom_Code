C*******************
C
C	FIND ZERO CROSSING
C
	IZCNT = 0
	IL = 1
	IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL),NDATA(IL+1)
	DO IL = 2,2047
	  IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL-1),
     1   NDATA(IL),NDATA(IL+1)
C		COUNT ALL CROSSINGS, POS TO NEG AND NEG TO POS
C	  IF(NDATA(IL)*NDATA(IL+1).LE.0) THEN
C	        IZCNT = IZCNT+1
C		S1 = NDATA(IL)
C		S2 = NDATA(IL+1)
C		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
C	  ENDIF
C		COUNT ONLY POS TO NEG
	  IF(NDATA(IL).GT.0.AND.NDATA(IL+1).LE.0) THEN
	        IZCNT = IZCNT+1
		IF(IPROCESS.EQ.0) THEN
		  S1 = NDATA(IL)
		  S2 = NDATA(IL+1)
		ELSE
		  S1 = DATA(IL)
		  S2 = DATA(IL+1)
		ENDIF
		ZCROSS(IZCNT) = IL + S1/(S1 - S2)
	  ENDIF
	ENDDO
	DO N = 1,IZCNT-1
	  ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
	  IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
	  IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
	ENDDO
	print*,'zero crossings found',izcnt
	print*,'first 5',(zcross(kk),kk=1,5)
C

