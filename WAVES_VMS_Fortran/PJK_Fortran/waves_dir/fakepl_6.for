	FUNCTION FLILO(X,B) 
C 
C 
C       FLILO MEANS LINEAR FUNCTION FOR SMALL ARGUMENT
C       AND LOGARITHM FOR LARGE ARGUMENT
C       SIGN IS PRESERVED 
C       FOR /X/.LT.B   FLILO=X
C       FOR /X/.GT.B   FLILO=A*LOG(X)+C IN SUCH A WAY 
C       THAT FLILO AND ITS DERIVATIVE ARE CONTINUOUS AT /X/=/B/ 
C 
        XM=ABS(X) 
        IF(XM.GT.B) GO TO 1 
	 FLILO=X
        RETURN
    1   FT=B*ALOG(XM/B)+B 
        FLILO=SIGN(FT,X)
        RETURN          
	END 
         FUNCTION AFLILO(X,B) 
C 
C       INVERSE OF FLILO
C 
        XM=ABS(X) 
        IF(XM.GT.B) GO TO 1 
        AFLILO=X
        RETURN
    1   FT=B*EXP(XM/B-1.) 
        AFLILO=SIGN(FT,X) 
        RETURN
        END 
	SUBROUTINE PLTPRT(STRING)
	BYTE STRING(132) 
	WRITE (7,100) STRING 
  100 FORMAT(' '132A1) 
	DO 10 N = 1,132
   10 STRING(N) = ' ' 
	RETURN 
	END
	SUBROUTINE PLTFST(N) 
	RETURN 
	END
	SUBROUTINE CLOSEF(N)
	RETURN 
	END
	SUBROUTINE PLOTF(N) 
	RETURN 
	END 
	SUBROUTINE SCOPST(N) 
	RETURN 
	END
	SUBROUTINE SCOPLT(N)
	RETURN 
	END
	SUBROUTINE SCOPE(N) 
	RETURN 
	END 
