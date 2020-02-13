	SUBROUTINE RCUBIC(A2,A1,A0,Z1,Z2,Z3)
C
C	THIS ROUTINE RETURNS Z1,Z2,Z3 = THREE ROOTS OF THE CUBIC
C
C		Z**3 + A2*Z**2 + A1*Z + A0 = 0
C
	COMPLEX Z1,Z2,Z3
	DATA COEF /1.73205081/
	DATA PI /3.14159265/
C
C
C	TRIGONOMETRIC SOLUTION FROM "MATHEMATICAL HANDBOOK FOR
C	SCIENTISTS AND ENGINEERS" KORN AND KORN, P23 SECT 1.8-4
C
C	NOTATION    HERE QLC2 = q/2  in Korn and Korn
C                          P3 = p/3  in Korn and Korn
C			   Q  = same, Q =P3**3 + QLC2**2 = (p/3)**3 + (q/2)**2
C 
	IF(A0.EQ.0) THEN
	  print*,'case a0 = 0'
	  Z1 = 0.
	  DISC = A2**2 - 4.*A1
	  Z2 = .5*(-A2 + CSQRT(CMPLX(DISC,0.)))
	  Z3 = .5*(-A2 - CSQRT(CMPLX(DISC,0.)))
	print*,'cube roots',z1,z2,z3
	print*,'check sum,product',z1+z2+z3,z1*z2*z3
C	  RETURN
	ENDIF
	P3 =  -(A2**2)/9. + A1/3. 
	QLC2 =  (A2**3)/27. - A1*A2/6. + .5*A0
	Q = P3**3 + QLC2**2
	print*,'p3,qlc2,Q',p3,qlc2,q
	IF(Q.LT.0.) THEN
C	case (a)    certainly p3 is negative
	  print*,'case a'
	  COSALF = -QLC2/SQRT(-P3**3)
	  print*,'cosalf',cosalf
	  ALPHA = ACOS(COSALF)
	  print*,'alpha',alpha,180.*alpha/pi
	  AL3 = ALPHA/3.
	  SQRTP3 = SQRT(-P3)
	  Z1 = 2.*SQRTP3*COS(AL3) - A2/3.
	  Z2 = -2.*SQRTP3*COS(AL3 + PI/3.) - A2/3.
	  Z3 = -2.*SQRTP3*COS(AL3 - PI/3.) - A2/3.
	print*,'cube roots',z1,z2,z3
	print*,'check sum,product',z1+z2+z3,z1*z2*z3
	ELSE
	    IF(P3.GT.0.) THEN
C	    case (b)
	  print*,'case b'
	       TANBET = SQRT(P3**3)/QLC2
	       BETA2 = .5*ATAN(TANBET)
	       IF(TANBET.GE.0.) THEN
	         TANALF =   (TAN(BETA2))**(1./3.)
	       ELSE
	         TANALF = -(-TAN(BETA2))**(1./3.)
	       ENDIF
	       ALPHA = ATAN(TANALF)
	       SQRTP3 = SQRT(P3)
	       Z1 = -2.*SQRTP3/TAN(2.*ALPHA) - A2/3.
	       Z2 = SQRTP3*CMPLX(1./TAN(2.*ALPHA),COEF/SIN(2.*ALPHA)) - A2/3.
	       Z3 = CONJG(Z2)
	    ELSE
C	    case (c)
	  print*,'case c'
	       SINBET = SQRT(-P3**3)/QLC2
	       BETA2 = .5*ASIN(SINBET)
	       IF(SINBET.GE.0.) THEN
	         TANALF =   (TAN(BETA2))**(1./3.)
	       ELSE
	         TANALF = -(-TAN(BETA2))**(1./3.)
	       ENDIF
	       ALPHA = ATAN(TANALF)
	       SQRTP3 = SQRT(-P3)
	       Z1 = -2.*SQRTP3/SIN(2.*ALPHA)
	       Z2 = SQRTP3*CMPLX( 1./SIN(2.*ALPHA) , COEF/TAN(2.*ALPHA) )
	       Z3 = CONJG(Z2)
	    ENDIF
	ENDIF
	print*,'cube roots',z1,z2,z3
	print*,'check sum,product',z1+z2+z3,z1*z2*z3
	RETURN
	END
