	SUBROUTINE FIXBADTDS
C
	COMMON /FIXUPBLK/ NBAD,NBAD1,NBAD2,ITDS
C
C	THIS SUBROUTINE LOOKS FOR CERTAIN VALUES IN A TDS EVENT OUTPUT
C	WHICH ARE KNOWN TO BE SUSPICIOUS, AND PUTS IN NEW VALUES BASED
C	ON THE ADJACENT VALUES.
C
	ITDS = 3
C
	DO N = 2,2045
C
C	VALUES OF -58 AND -63 AND PERHAPS IN BETWEEN, IN CHANNEL 1
C	AND SOMETIMES CHANNEL 2 WHICH FOLLOW VALUES WHICH ARE ABOUT THE
C	SAME
C
	  IF(IABS(NDATA(N)+60).LE.3) THEN
	    IF(IABS(NDATA(N+1)+60).LE.3) THEN
	      IF(NDATA(N-1).GT.-56.AND.NDATA(N+2).LT.-62) THEN
		CALL FIXUP(N+1)
	      ENDIF
	    ENDIF
	  ENDIF
C
C	BAD VALUES WHICH ARE DUE TO THE SWITCHOVER FROM HIGH GAIN TO
C	LOW GAIN A/DB CONVERTER.  THESE DEPEND ON CHANNEL.  THEY HAVE
C	THE VALUES +-71 IN CHANNELS 1 AND 2
C
	ENDDO
C
	RETURN
	END
	SUBROUTINE FIXUP(N)
	  PRINT*,'FIXUP',N,NDATA(N),NDATA(N+1)
	RETURN
	END
       SUBROUTINE SINFIT(X,SUMSQ)
C
C	THIS ROUTINE FITS A SINE WAVE TO DATA.  X(1) IS SUPPOSED TO
C	BE VARIED BY HUNTMN, WHILE Y(1) TO Y(3) ARE DETERMINED BY
C	LEAST SQUARES FIT IN CLOSED FORM.  DCSV IS THE ARRAY OF NPT
C	DATA POINTS.  
C
C	X(1) IS FREQUENCY IN CYCLES PER SAMPLE  (A SMALL NUMBER)
C
C	Y(1) IS AVERAGE (OFFSET)
C	Y(2) IS COEFF OF COSINE
C	Y(3) IS COEFF OF SINE
C
       COMMON /FITBLK/ DCSV(3000),ANPL(3000),WT(3000),ERR(3000),NPT
       DOUBLE PRECISION COS1,SIN1,COSN,SINN,THT0
       DIMENSION X(25),C(3,3),Y(3)
       DATA TWOPI /6.2831853/
C
	DO I = 1,3
	  Y(I)= 0.
	  DO J = 1,3
	    C(I,J) = 0.
	  ENDDO
	ENDDO
C
	THT0 = TWOPI*X(1)
	COS1 = DCOS(THT0)
	SIN1 = DSIN(THT0)
	SINN = SIN1
	COSN = COS1
	DO N = 1,NPT
C	  ANPL(N) = (N-1)*360.*X(1)
	  Y(1) = Y(1) + WT(N)*DCSV(N)
	  Y(2) = Y(2) + WT(N)*DCSV(N)*COSN
	  Y(3) = Y(3) + WT(N)*DCSV(N)*SINN
	  C(1,1) = C(1,1) + WT(N)
	  C(1,2) = C(1,2) + WT(N)*COSN
	  C(1,3) = C(1,3) + WT(N)*SINN
C	  C(2,1) = C(2,1) + WT(N)*
	  C(2,2) = C(2,2) + WT(N)*COSN**2
	  C(2,3) = C(2,3) + WT(N)*COSN*SINN
C	  C(3,1) = C(3,1) + WT(N)*
C	  C(3,2) = C(3,2) + WT(N)*
	  C(3,3) = C(3,3) + WT(N)*SINN**2
C          ERR(N) =  DCSV(N) - X(3)*COSN - X(4)*SINN - X(2)
C	  SUMSQ = SUMSQ + WT(N)*ERR(N)**2
C	  SUMN = SUMN + WT(N)
	  SINN = COSN*SIN1 + SINN*COS1
	  COSN = COS1*COSN - SIN1*SINN
	ENDDO
	C(3,1)= C(1,3)
	C(3,2) = C(2,3)
	C(2,1) = C(1,2)
	CALL GAUSSJ(C,3,3,Y,1,1)
C
	SUMSQ = 0.
	SUMN = 1.E-8
	THT0 = TWOPI*X(1)
	COS1 = DCOS(THT0)
	SIN1 = DSIN(THT0)
	SINN = SIN1
	COSN = COS1
	DO N = 1,NPT
          ERR(N) =  DCSV(N) - X(3)*COSN - X(4)*SINN - X(2)
	  SUMSQ = SUMSQ + WT(N)*ERR(N)**2
	  SUMN = SUMN + WT(N)
	  SINN = COSN*SIN1 + SINN*COS1
	  COSN = COS1*COSN - SIN1*SINN
	ENDDO
	SUMSQ = SUMSQ/SUMN
C	PRINT*,'EFIT,SINN,COSN',SINN,COSN
       RETURN
       END
