	SUBROUTINE HUNTMN( N, X, DX, Y, FN, FF) 
C
C     THIS IS MK V, APRIL 1988 
C
	DIMENSION X(25), Y(25), DX(25), DXB(25)
	COMMON /HNTBLK/ NHUNT(25) 
	DATA NHUNT/25*1/
C
C
	DO 3 I = 1,25
    3	Y(I) = X(I) 
	DO 1 I = 1,N
	   DXB(I) = 0.  
	   Y(I) = X(I)   
 1	CONTINUE
	CALL FN(Y,F0)  
	FM = F0 
	DO 2 I = 1,N   
	   IF(NHUNT(I).EQ.0) GO TO 2  
	   XB = X(I)
	   Y(I) = X(I) + DX(I)   
	   CALL FN(Y, F1)
	   IF(F1.GE.FM) GO TO 4
	   FM = F1 
	   XB = Y(I)
 4	   Y(I) = X(I) - DX(I)   
	   CALL FN(Y,F2) 
	   IF(F2.GE.FM) GO TO 8
	   FM = F2 
	   XB = Y(I)
 8	   DEN = F1 + F2 - 2.*F0   
	   IF(DEN.EQ.0.) GO TO 2 
C	   DEN IS SECOND DERIV DIVIDED BY 2.*DX**2.  IF ITS NEGATIVE
C	   THEN THE EXTREMUM IS A MAXIMUM.  IN THAT CASE, GO ONE STEP
C	   BEYOND THE LOWEST POINT.
	   IF(DEN.GT.0.) THEN
	     DEX = -.5*(F1 - F2)/DEN  
	   ELSE
	     DEX = SIGN(2.,(F2-F1))
	   ENDIF
	   Y(I) = X(I) + DEX*DX(I) 
	   CALL FN(Y,FF) 
	   IF(FF.LT.FM) GO TO 18 
C		NEW VALUE IS NOT BETTER
	   FF = FM 
	   Y(I) = XB
	   IF((F1+F2).LT.3.*F0) GO TO 18
C		DX IS TOO BIG, ADJUST TO GIVE 50 % INCREASE IN F
	   DX(I) = SQRT(ABS(F0/DEN))*DX(I)
	   GO TO 19
 18	   CONTINUE
	   DX(I) = .5*(ABS(DX(I)) + ABS(X(I) - Y(I)))
 19	   DXB(I) = Y(I) - X(I)
	   X(I) = Y(I)  
	   F0 = FF  
	   FM = FF 
 2	CONTINUE
C 
C     DO ONE MORE STEP IN DIRECTION OF BEST CHANGE  
C
	DO 30 I = 1,N 
	   Y(I) = X(I) + DXB(I)   
 30	CONTINUE
	CALL FN(Y, F1) 
	FM = AMIN1(FM,F1) 
	DO 31 I = 1,N 
	   Y(I) = X(I) - DXB(I)   
 31	CONTINUE
	CALL FN(Y, F2) 
	FM = AMIN1(F2,FM) 
	DEN = F1 + F2 - 2.*FF   
	IF(DEN.EQ.0.) RETURN
	DEX = -.5*(F1 - F2)/DEN 
	DO 32 I = 1,N 
	   Y(I) = X(I) + DEX*DXB(I)   
 32	CONTINUE
	CALL FN(Y, FFP)
	IF(FFP.GT.FM) GO TO 34  
	FF = FFP 
	DO 33 I = 1,N 
	   X(I) = Y(I) 
 33	CONTINUE
	RETURN
C
C     ENTRY HERE MEANS THAT F1, F2, OR FF IS SMALLEST 
C
 34	IF(FF.LE.F1.AND.FF.LE.F2) RETURN
	DEX = 1.  
	IF(F2.LT.F1) DEX = -1. 
	DO 35 I = 1,N  
	   Y(I) = X(I) + DEX*DXB(I)
	   X(I) = Y(I) 
 35	CONTINUE
	FF = AMIN1(F1,F2) 
	RETURN
C
C
	END
