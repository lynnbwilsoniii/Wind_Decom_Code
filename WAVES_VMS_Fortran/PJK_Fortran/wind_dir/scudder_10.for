	SUBROUTINE SCUDDER(V,CURDENS)
C
C	CALCULATES PHOTOEMISSION IN AMP/ M**2 ACCORDING TO SCUDDER,CAO
C		AND MOZER
C
C	DATA AA,TPHI1,CC,TPHI2/ 405., 1.45, 1.75, 8.4/  ! joint prob
C	DATA AA,TPHI1,CC,TPHI2/ 154., 1.45, 1.75, 8.4/  !error corrected
	DATA AA,TPHI1,CC,TPHI2/ 152., 1.70, 0.86, 9.5/  ! april 2000
C	DATA AA,TPHI1,CC,TPHI2/ 102., 1.46, 1.37, 8.42/  ! joint prob*
C
	CURDENS = 1.E-6*(AA*EXP(-V/TPHI1) + CC*EXP(-V/TPHI2))  !genuine
C	CURDENS = 1.E-7*(AA*EXP(-V/TPHI1) + CC*EXP(-V/TPHI2))  ! fake
	RETURN
	END 
