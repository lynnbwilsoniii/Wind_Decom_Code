	PROGRAM FORSLUNDVC
C
C	CALCULATES VC FOR FORSLUND INSTABILITY
C
	REAL IONMASS
C
	DATA TEC/10./		!IN eV
	DATA TI /10./
	DATA CKMS /3.E5/
	DATA IONMASS /1836./
C
	TRATIO = TEC/TI
	EVIMASS = 935.E6		! ion mass in eV
C	
	DO NVC = 1,10
	  VC = 10.*NVC
	  VCC = VC/CKMS
C		APPROXIMATE
	  V = VCC/((TRATIO**1.5*EXP(-EVIMASS*VCC**2/TI) + 1.)
	  SUMF = 1. + 
