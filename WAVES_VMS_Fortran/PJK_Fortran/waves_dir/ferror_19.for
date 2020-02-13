	PROGRAM FERROR
C
C	THIS PROGRAM CALCULATES THE ERROR AND THE CHANGEOVER POINT BETWEEN
C	THE POWER SERIES AND THE ASYMPTOTIC SERIES FOR SUBROUTINE F
C	EVALUATE ERROR IN POWER SERIES
	EPS = 1.E-7                 !  SINGLE PRECISION ROUNDOFF ERROR
C
	DO N = 1,20
	  ALPHA = 2.1 + .1*(N-1)
	  A2 = ALPHA**2
C	  ERR = .5*EPS*SQRT(1./(2.*A2))*(A2-.5)**A2*EXP(A2+.5)
	  ERR = .5*EPS*SQRT(1./(2.*A2-1.))*((A2-.5)**(A2-1.))*EXP(A2)
	  TERMN = A2-1.
	  ERRLOG = ALOG10(ERR)
C	  PRINT*,ALPHA,TERMN,ERR,ERRLOG
C	  WRITE(7,*) ALPHA,TERMN,ERR,ERRLOG
	ENDDO
C	IT IS ALL WRONG FROM HERE
C
C	EVALUATE ERROR IN ASYMPTOTIC SERIES
C
	DO N = 1,20
	  ALPHA = 1.7 + .05*(N-1)
	  A2 = ALPHA**2
	  TERMN = A2+.5
	  ERR = 2.*SQRT(2.*A2-1.)/(TERMN-1.)**(TERMN-.5)/EXP(TERMN)
	  ERRLOG = ALOG10(ERR)
C	  PRINT*,ALPHA,TERMN,ERR,ERRLOG
C	  WRITE(7,*) ALPHA,TERMN,ERR,ERRLOG
	ENDDO
C
C	CALCULATE LIMITS
C
	DO N = 1,60
	  ALPHA = 1.7 + .05*(N-1)
	  A2 = ALPHA**2
	  EPSN = 2.*EXP(-2.*A2)
	  FRERR = 1.414*EXP(-A2)
	  PRINT*,ALPHA,EPSN,FRERR
	  WRITE(7,*) ALPHA,EPSN,FRERR
	ENDDO
	STOP
	END
