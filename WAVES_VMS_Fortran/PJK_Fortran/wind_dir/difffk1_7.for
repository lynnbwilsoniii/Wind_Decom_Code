	SUBROUTINE DIFFFK1(XN,YN,ZN,BX,BY,BZ,D,XT,YT,ZT)
C
	IMPLICIT REAL*4 (A-H)
	IMPLICIT INTEGER*4 (I-N)
	IMPLICIT REAL*4 (O-Z)
C
C	CALCULATES DIFF TO BOW SHOCK, ALGEBRA IN WIND VOL 4 P 89-90
C		XN,YN,ZN ARE POSITION OF SATELLITE, D IS DIFF, POSITIVE
C		IF SATELLITE IS DOWNSTREAM FROM TANGENT LINE.  
C		XT,YT,ZT ARE TANGENT POINT
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
	BMAG = SQRT(BX2+BY2+BZ2)
	print*,'in diff',xn,bx,by,bz
C
C	THE FIELD LINE THROUGH THE SATELLITE IS GIVEN BY X = XN + T*BX
C		ETC.  SUBSTITUTING THIS IS THE EQUATION FOR THE SHOCK,
C		
C		X = XNOSE*(1. - (Y**2 + Z**2)/RS**2
C
C		GIVE A QUADRATIC FOR T.  THE FIELD LINE IS TANGENT WHEN
C		THE DISCRIMINANT IS ZERO.
C		a, b, and c ARE THE USUAL PARTS OF THE DISCRIMINANT
C		OF THE QUADRATIC FOR T
C		NOW CHANGE XN INTO X, X IS THE VALUE OF THE X COORDINATE
C		WHICH MAKES THE DISCRIMINANT ZERO
C
	a = XNOSE*(BY2 + BZ2)/RS2
	b = BX + 2.*XNOSE*(YN*BY + ZN*BZ)/RS2
	c = XN - XNOSE + XNOSE*(YN**2 + ZN**2)/RS2
	X = .25*b**2/a + XNOSE*(1. - (YN**2 + ZN**2)/RS2)
C
	D = X - XN
C
	XT = 0.
	YT = 0.
	ZT = 0.
c
	IF(D.GE.0.) THEN
C
C	  CALCULATE TANGENT POINT IF DIFF IS POSITIVE
C
	T = -b/2./a
	XT = X + T*BX
	YT = YN + T*BY
	ZT = ZN + T*BZ
C
C	CHECK, XT,YT,ZT OUGHT TO LIE ON THE SHOCK
	XCHECK = XNOSE*(1. - (YT**2 + ZT**2)/RS2)
	print*,'t,b,a,',t,b,a
	print*,'xt,xcheck,diff',xt,xcheck,xt-xcheck
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
	

