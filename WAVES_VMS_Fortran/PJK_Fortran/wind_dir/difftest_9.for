	PROGRAM DIFFTEST
C
c	so far, this test has failed
c
	IMPLICIT REAL (A-H)
	IMPLICIT INTEGER*4 (I-N)
	IMPLICIT REAL (O-Z)
C
	DATA YS,ZS /12.,1./
	DATA BX,BY,BZ /5.,-2.,0./
C
	DATA YS,ZS /40.405,-4.283/
	DATA BX,BY,BZ /-1.887,-2.126,1.843/
C
C	CALCULATE X ON THE SHOCK FOR THIS Y AND Z
C
	X = 14.6*(1. - (YS**2+ZS**2)/25.6/25.6)
	T = 5.
	T = 0.
	XN = X + T*BX
c
c	xn = 28.378
	YN = YS + T*BY
	ZN = ZS + T*BZ
	PRINT*,'S/C AT',XN,YN,ZN
	PRINT*,'B ',BX,BY,BZ
	CALL DIFFFK1(XN,YN,ZN,BX,BY,BZ,D0,XT,YT,ZT) 
	CALL DIFFFK(XN,YN,ZN,BX,BY,BZ,DK,XT,YT,ZT) 
	PRINT*,'DIFF',D0,DK
	  print*,'tangent',XT,YT,ZT
	STOP
	END
	SUBROUTINE DIFF(XN,YN,ZN,BX,BY,BZ,D,XT,YT,ZT)
C
	IMPLICIT REAL*4 (A-H)
	IMPLICIT INTEGER*4 (I-N)
	IMPLICIT REAL*4 (O-Z)
C
C	CALCULATES DIFF TO BOW SHOCK, ALGEBRA IN WIND VOL 4 P 89-90
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
	print*,'in diff',xn,bx,by,bz
C
	a = -(BY2 + BZ2)/BX2/RS2
	print*,'a=',a
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
	D = -CC/BB
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
c		not done yet
C
C
	ELSE
	  XT = 0.
	  YT = 0.
	  ZT = 0.
	ENDIF
C
	RETURN
	END
	