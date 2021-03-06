	SUBROUTINE MULTIHISTO(ILOAD,X,NARR,NBIN,NCH,XMIN,XMAX,TOTAL)
C
C	THIS IS A SPECIAL VERSION TO CALCULATE LEVEL AT A CERTAIN PERCENTILE
C
C	TO INITIALIZE, CALL WITH ILOAD = 2, ACCUMULATE WITH ILOAD = 1,
C		PRINT OUT WITH ILOAD = 0, PCTILE GT .5
C
	INTEGER*4 NARR(NBIN,NCH)
C	DATA NARR /256*0/
C
	IF(ILOAD.GT.1) THEN
	  DO N = 1,256
	    NARR(N) = 0
	  ENDDO
	  TOTAL = 0.
	  RETURN
	ENDIF
C
	IF(ILOAD.EQ.1) THEN
	  XT = AMAX1(X,XMIN)
	  XT = AMIN1(XT,XMAX)
	  IBIN = (NBIN-1)*(XT-XMIN)/(XMAX-XMIN) + 1
	  NARR(IBIN) = NARR(IBIN)+1
	  TOTAL = TOTAL + 1.
	  RETURN
	ENDIF
C
	IF(ILOAD.EQ.0 AND.PCTILE.GT..5) THEN
C	  PRINT OUT RESULTS
    	  PRINT*,' TOTAL NUMBER, NBIN',TOTAL,NBIN
	  PRINT*,'XMIN,XMAX',XMIN,XMAX
	  PRINT*,'HISTOGRAM'
	  PRINT 1111, (NARR(J),J=1,NBIN)
 1111	  FORMAT(10I7)
	ENDIF
C
C	NOW CALCULATE VALUE OF X CORRESPONDING TO PCTILE PERCENTILE
C
C	PCTILE = .2
	SET = PCTILE*TOTAL
	SUM = 0.
	N = 0
  100	N = N+1
	  SUM = SUM + NARR(N)
	  IF(SUM.LT.SET) GO TO 100
C
C	NOW THE DESIRED VALUE LIES IN BIN N
C
	SUMLOW = SUM - NARR(N)
	XN = N
	RETLOW = XMIN + ((XN-1.)/(NBIN-1.))*(XMAX-XMIN)
	RETHI = XMIN +       (XN/(NBIN-1.))*(XMAX-XMIN)
	RET = .5*(RETLOW+RETHI)
	IF(NARR(N).NE.0) THEN
	  RET = RETLOW + (SET-SUMLOW)*(RETHI-RETLOW)/NARR(N)
	ELSE
	  PRINT*,'ERROR IN HISTO, N, NARR(N) = ',N,NARR(N)
	ENDIF
C	PRINT*,'N,SET,SUMLOW,LOW,RET',N,SET,SUMLOW,RET
	RETURN
	END
