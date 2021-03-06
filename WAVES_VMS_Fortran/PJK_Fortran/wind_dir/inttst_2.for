	PROGRAM INTTST
C
	REAL*8 YY(5)
	REAL*4 XX(5)
	DATA XX /0.,1.,2.,3.,4./
	DATA YY /1.D00,2.D00,3.D00,4.D00,5.D00/
C
	XT = -1.5
	CALL DINTERP(XX,YY,5,XT,YINT,ERR)
	PRINT*,'XT,YINT,ERR',XT,YINT,ERR
	XT = 1.5
	CALL DINTERP(XX,YY,5,XT,YINT,ERR)
	PRINT*,'XT,YINT,ERR',XT,YINT,ERR
	XT = 5.5
	CALL DINTERP(XX,YY,5,XT,YINT,ERR)
	PRINT*,'XT,YINT,ERR',XT,YINT,ERR
	STOP	
	END
	SUBROUTINE DINTERP(XX,YY,NN,X,Y,ERR)
C
C	INTERPOLATE ON X TO FIND Y, GIVEN TABLES XX AND YY, CONTAINING
C	NN ELEMENTS, TABLE XX MUST BE IN INCREASING ORDER
C
	DIMENSION  XX(1)
	REAL*8	   YY(1)
C
	Y = YY(1)
	ERR = -1.
	IF(X.LT.XX(1)) RETURN
	Y = YY(NN)
	ERR = 1.
	IF(X.GT.XX(NN)) RETURN
	ERR = 0.
	NNX = (NN+1)/2
	NDEL = (NNX+1)/2
  25	IF(NDEL.LE.1) GO TO 29
	IF(X.GE.XX(NNX)) GO TO 20
	NNX = NNX - NDEL
	NDEL = (NDEL+1)/2
	GO TO 25
  20	NNX = NNX + NDEL
	NDEL = (NDEL+1)/2
	GO TO 25
  29	LLIM = MAX0(NNX-1,2)
	DO 10 I = LLIM,NN
	IF((X-XX(I)).GT.0.) GO TO 10
	Y = YY(I-1) + (YY(I)-YY(I-1))*(X-XX(I-1))/(XX(I)-XX(I-1))
	RETURN
  10	CONTINUE
	RETURN
	END
