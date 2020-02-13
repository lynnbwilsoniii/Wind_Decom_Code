	PROGRAM PROCMK20
C
C	MAKES for033, to make HISTOGRAM OF ANGLES TO B, FROM MAKEFILE20
C
	INTEGER*4 	SCETI4(2)
	INTEGER   	ERTDAY
	CHARACTER*1 	EVENT
	REAL		BANG(2000)
	INTEGER		NHIST(101)
	CHARACTER*120	JUNK
C
	OPEN(UNIT=80,FILE='[KELLOGG.WIND.PAPERS]MAKEFILE20.80RESULTS',
     C	STATUS='OLD')
	NPT = 0
	NOUT = 0
	NTOT = 0
	ANGAVR = 0.
	COUNT = 1.E-10
	ANGSTD = 0.
	READ(80,1001) JUNK
	READ(80,1001) JUNK
 1001	FORMAT(A)
C
 100	CONTINUE
C
          READ(80,1080,ERR=100,END=200) sceti4,NO_EVT,ERTDAY,EVENT,
     1    FAVR1,XRMS,FBW1,FAVR2,YRMS,FBW2,FP,
     2    ANGTOB,AXRATIO,THETAB,DDIFF,XRE,YRE,ZRE
c	  print*,sceti4,no_evt,ERTDAY,EVENT
c	  PRINT*,NO_EVT,ANGTOB,AXRATIO,XRE,YRE
c	if(1) stop
c
	  NTOT = NTOT+1
C	  IF(AXRATIO.GT..04) GO TO 100
c	  IF(AXRATIO.GT..20) GO TO 100
C	  FBW IN BANDWIDTH IN KHZ IN THIS PROGRAM
	  FRBW1 = FBW1/FAVR1
	  FRBW2 = FBW2/FAVR2
	  IF(FBW1.GT.1.) GO TO 100
	  IF(FBW2.GT.1.) GO TO 100
	  NPT = NPT+1
	  BANG(NPT) = ANGTOB
	  IHIST = ANGTOB + 90.5
	  IHIST = MAX0(IHIST,1)
	  IHIST = MIN0(IHIST,181)
	  NHIST(IHIST) = NHIST(IHIST)+1
	  IF(ABS(ANGTOB).LE.18.) THEN
	    COUNT = COUNT + 1.
	    ANGAVR = ANGAVR + ANGTOB
	    ANGSTD = ANGSTD + ANGTOB**2
c	  ELSE
c	    WRITE(82,1080) sceti4,NO_EVT,ERTDAY,EVENT,
c     1    FAVR1,XRMS,FBW1,FAVR2,YRMS,FBW2,FP,
c     2    ANGTOB,AXRATIO,THETAB,DDIFF,XRE,YRE,ZRE 
c	  NOUT = NOUT+1
	  ENDIF
	GO TO 100
C
 1080   format(I10,I7,I10,I3,A2,F6.2,E9.2,F6.3,F6.2,E9.2,F6.3,F6.2,F7.1,
     1  F6.2,F6.1,4F7.1)
C
 200	CONTINUE
	print*,'npt = ',npt
	PRINT*,'OUTLIERS',NOUT
	PRINT*,'TOTAL',NTOT
	DO N = 1,101
	  write(33,*) n-90,nhist(n)
          PRINT*,N-90,NHIST(N)
        ENDDO
	ANGAVR = ANGAVR/COUNT
	ANGSTD = ANGSTD/COUNT - ANGAVR**2
	ANGRMS = SQRT(ANGSTD)
	PRINT*,'COUNT,AVR,STD',COUNT,ANGAVR,ANGRMS
	STOP
	END