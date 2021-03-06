	SUBROUTINE FANDBW2(FREQ,DBX,DBY,NPEAK,FAVR,FBW,SKEW)
C
	DATA SPS /120000./
	REAL AMP(2),DBX(1025),DBY(1025),FREQ(1025)
	REAL FCOUNT(2),FAVR(2),FSTD(2),F3MOM(2),FBW(2),SKEW(2)
	DATA NFLLIMP,NFUPLIMP /4, 1024/
C
	CONST1 = (1./41.1)**2		! DO IN (VOLTS/M)**2
	CONST2 = (1./3.79)**2
C
C	CALCULATE BANDWIDTH, ETC.
	NFLLIM = NPEAK/2
	NFLLIM = MAX0(NFLLIM,NFLLIMP+1)
	NFUPLIM = (3*NPEAK)/2
	NFUPLIM = MIN0(NFUPLIM,NFUPLIMP)
C*********
C	THIS CHOICE DID NOT WORK WELL.  IT GAVE NARROW BANDWIDTHS
C	FOR SOME CASES WHEN THE BANDWIDTH WAS WIDE.  FOR EXAMPLE, IT
C	GAVE A BANDWIDTH OF 63.7 HZ FOR 19960421 # 13042040
C	NFLLIM = NPEAK-2
C	NFUPLIM = NPEAK+2
C*********
	AMP(1) = DBX(NPEAK)
	AMP(2) = DBY(NPEAK)
C
	DO NC = 1,2
	  FCOUNT(NC) = 1.E-20
	  FAVR(NC) = 0.
	  FSTD(NC) = 0.
	  F3MOM(NC) = 0.
	  DO N = NFLLIM,NFUPLIM
	    IF(NC.EQ.1) THEN
		VLT = CONST1*10.**(.1*DBX(N))
	    ELSE
		VLT = CONST2*10.**(.1*DBY(N))
	    ENDIF
	    FCOUNT(NC) = FCOUNT(NC) + VLT
	    FAVR(NC) = FAVR(NC) + VLT*FREQ(N)
	    FSTD(NC) = FSTD(NC) + VLT*FREQ(N)**2
	    F3MOM(NC) = F3MOM(NC) + VLT*FREQ(N)**3
	  ENDDO
	  FAVR(NC) = FAVR(NC)/FCOUNT(NC)
	  FSTD(NC) = FSTD(NC)/FCOUNT(NC) - FAVR(NC)**2
	  FBW(NC) = SQRT(AMAX1(FSTD(NC),0.))
	  SKEW(NC) = F3MOM(NC)/FCOUNT(NC) - 3.*FAVR(NC)*FSTD(NC)
     1		 - FAVR(NC)**3
	ENDDO
	PRINT*,'FANDBW2,F,BW',FAVR(1),FBW(1),FAVR(2),FBW(2)
C
	RETURN
	END

