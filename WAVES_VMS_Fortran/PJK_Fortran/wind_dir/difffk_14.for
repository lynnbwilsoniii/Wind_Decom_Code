	SUBROUTINE DIFFFK(XN,YN,ZN,BX,BY,BZ,D,XT,YT,ZT,XI,YI,ZI,IN)
C
	IMPLICIT REAL*4 (A-H)
	IMPLICIT INTEGER*4 (I-N)
	IMPLICIT REAL*4 (O-Z)
C
C	CALCULATES DIFF TO BOW SHOCK, ALGEBRA IN WIND VOL 4 P 89-90
C		D IS DIFF, XT,YT,ZT ARE COORDINATES OF TANGENT POINT
C		XI,YI,ZI ARE COORDINATES OF INTERSECION WITH SHOCK
C		INPUT XN, ETC ARE IN EARTH RADII, B IN nT
C		IN = 1, INSIDE SHOCK, 2 FORESHOCK, 3 FREE SW
C		positive d means field intersects bow shock
C
	DATA XNOSE,RS /14.6,25.6/
C
	IF(BX.EQ.0.) THEN
	  D = -(XN-XNOSE)
	  RETURN
	ENDIF
C
	RS2 = RS**2
	BX2 = BX**2
	BY2 = BY**2
	BZ2 = BZ**2
C	print*,'in diff,X,B',xn,bx,by,bz
C
	a = -(BY2 + BZ2)/BX2/RS2
C	print*,'a=',a
	ZXN = ZN - XN*BZ/BX
	YXN = YN - XN*BY/BX
	b0 = 1./XNOSE +2.*(BZ*ZXN/BX + BY*YXN/BX)/RS2
	b0 = -b0
	b1 = -2.*a
	c0 = 1. - (YXN**2 + ZXN**2)/RS2
	c1 = 2.*(BZ*ZXN/BX + BY*YXN/BX)/RS2
	c2 = a
	BB = 2.*b0*b1 - 4.*a*c1
	CC = b0**2 - 4.*a*c0
	D = 0.
	IF(BB.NE.0.) D = -CC/BB
C
	IF(D.GE.0.) THEN
C
C	  CALCULATE TANGENT POINT IF DIFF IS POSITIVE
C
	  AT = a
	  BT = b0 + b1*d
	  CT = c0 + c1*d + c2*d**2
	  XT = -.5*BT/AT
	  YT = YN + BY*(XT - XN - D)/BX
	  ZT = ZN + BZ*(XT - XN - D)/BX
C
C	  CALCULATE INTERSECTION IF DIFF IS POSITIVE
C	    THERE ARE TWO SOLUTIONS, XI1 AND XI2, OF 
C	    AI X**2 + BI X + CI = 0
C
	  AI = a
	  BI = b0
	  CI = c0
	  TEST = BI**2-4.*AI*CI
	  IF(TEST.GE.0.) THEN
	    XI1 = .5*(-BI + SQRT(BI**2-4.*AI*CI))/AI
	    YI1 = YN + BY*(XI1-XN)/BX
	    ZI1 = ZN + BZ*(XI1-XN)/BX
	    XI2 = .5*(-BI - SQRT(BI**2-4.*AI*CI))/AI
	    YI2 = YN + BY*(XI2-XN)/BX
	    ZI2 = ZN + BZ*(XI2-XN)/BX
	    D1SQ = (XN-XI1)**2 + (YN-YI1)**2 + (ZN-ZI1)**2
	    D2SQ = (XN-XI2)**2 + (YN-YI2)**2 + (ZN-ZI2)**2
	    IF(D1SQ.LT.D2SQ) THEN
	      XI = XI1
	      YI = YI1
	      ZI = ZI1
	    ELSE
	      XI = XI2
	      YI = YI2
	      ZI = ZI2
	    ENDIF
	  ELSE
	    IN = 2
	  ENDIF
	  IF(XN.LE.XI1.AND.XN.GE.XI2) IN = 1
	  IF(XN.LE.XI2.AND.XN.GE.XI1) IN = 1
C
	ELSE
	  XT = 0.
	  YT = 0.
	  ZT = 0.
	  XI = 0.
	  YI = 0.
	  ZI = 0.
	  IN = 3
	ENDIF
C
	RETURN
	END
	

