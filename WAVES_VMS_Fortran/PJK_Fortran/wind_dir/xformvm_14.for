	SUBROUTINE XFORMVM(N1,N2,NSYS,XINP,XFDATA)
C
C	THIS TRANSFORMS THE ELECTRIC FIELD, XINP,INTO THE VARIANCE MATRIX
C	EIGENSYSTEM, 1 = LARGEST EIGENVALUE.  XFDATA ARE THE TRANS-
C	FORMED FIELDS
C
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
C	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
C     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS
C	common /headblk/ major,minor,s_scet,nsys
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	INTEGER*4 SUNCLOCK
	REAL XINP(2050,4), XTEMP(2050,4),XFDATA(2050,4)
C
C	EVECT(I,J) IS THE Ith COMPONENT OF THE Jth EIGENVECTOR
C
C	VMATRIX CALCULATES THE VARIANCE MATRIX OF XGSE, AND RETURNS
C	EIGENVALUES IN COMMON /VARMATX/EVAL, AND EIGENVECTORS IN EVECT
C
C	MAKE A COPY OF THE INPUT MATRIX TO WORK ON
C
	DO J = 1,4
 	  DO N = 1,2048
		XTEMP(N,J) = XINP(N,J)
	  ENDDO
	ENDDO	
C
	IF(N2-N1.LT.90) THEN
	    CALL VMATRIX(XTEMP,N1,N2)
	ELSE
	    CALL VMATRIX(XTEMP,N1,N2)
	ENDIF
	WRITE(79,*) ' '
	WRITE(79,*) 'XFORMVM CALLED--PUT FIELD DATA INTO VAR MX SYSTEM, NSYS=2'
C
C	NOW EVECT ARE EIGENVECTORS IN THE XINP SYSTEM
C	XFDATA(N,1) CORRESPONDS TO LARGEST EIGENVALUE,N=COMPONENT
C
	DO N = 1,2048
	  XFDATA(N,1) = XTEMP(N,1)*EVECT(1,1) + XTEMP(N,2)*EVECT(2,1) + 
     1		XTEMP(N,3)*EVECT(3,1)
	  XFDATA(N,2) = XTEMP(N,1)*EVECT(1,2) + XTEMP(N,2)*EVECT(2,2) + 
     1		XTEMP(N,3)*EVECT(3,2)
	  XFDATA(N,3) = XTEMP(N,1)*EVECT(1,3) + XTEMP(N,2)*EVECT(2,3) + 
     1		XTEMP(N,3)*EVECT(3,3)
	ENDDO
C
C	CHECK TRANSFORMATION.  IN XTEMP, COMPONENT IS 2ND INDEX, IN
C	EVECT THE FIRST INDEX IS COMPONENT.  T1,T2,T3 ARE THE
C	COMPONENTS IN THE EIGENVECTOR SYSTEM, N IS EIGENVECTOR NO. 
C
C	DO N = 1,3
C	  T1 = EVECT(1,N)*EVECT(1,1) + EVECT(2,N)*EVECT(2,1) + 
C     1		EVECT(3,N)*EVECT(3,1)
C	  T2 = EVECT(1,N)*EVECT(1,2) + EVECT(2,N)*EVECT(2,2) + 
C     1		EVECT(3,N)*EVECT(3,2)
C	  T3 = EVECT(1,N)*EVECT(1,3) + EVECT(2,N)*EVECT(2,3) + 
C     1		EVECT(3,N)*EVECT(3,3)
C	  PRINT*,'CHECK',N,T1,T2,T3
C	ENDDO
C
C	DO N = 1,3
C	  T1 = EVECT(1,N)*EVECT(1,1) + EVECT(2,N)*EVECT(2,1) + 
C     1		EVECT(3,N)*EVECT(3,1)
C	  T2 = EVECT(1,N)*EVECT(1,2) + EVECT(2,N)*EVECT(2,2) + 
C     1		EVECT(3,N)*EVECT(3,2)
C	  T3 = EVECT(1,N)*EVECT(1,3) + EVECT(2,N)*EVECT(2,3) + 
C     1		EVECT(3,N)*EVECT(3,3)
C	  PRINT*,'CHECK',N,T1,T2,T3
C	ENDDO
C
C	WRITE(79,*) 'IN SYSTEM,',NSYS,' EIGENVALUES:'
C	WRITE(79,*)  EVAL
C	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
C	WRITE(79,*)(EVECT(1,I),I=1,3)
C	WRITE(79,*)(EVECT(2,I),I=1,3)
C	WRITE(79,*)(EVECT(3,I),I=1,3)
C
	NSYS = 2
C
	RETURN
	END
	SUBROUTINE XFGSE(XDATA,XGSE,NMEAS,EANGLE,DANG)
