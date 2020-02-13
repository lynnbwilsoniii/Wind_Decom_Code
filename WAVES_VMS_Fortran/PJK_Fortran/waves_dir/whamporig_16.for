	PROGRAM WHAMP
	COMPLEX X,XO,XVO,XX(6),XP(6),DX,OME,FPX,DIR,DIX,DIZ,DIP,
     1   EPS(6,4),DOX,DOZ,DOP,CX,EFL(3),BFL(3),RI
	DIMENSION REN(6),T(6),ST(6),ISP(6),ITID(7)
	CHARACTER*3 SPE(5)
	COMMON /XPZ/ XX,PP(6),ZZ(6),A(6),B(6),D(6),ASS(6),VD(6),
     1     DN(6),TA(6),XP,CV,PM(3),ZM(3),XOI,XC,PZL
	COMMON /COUT/ X,P,Z,EFL,BFL,DIR,DIX,DIZ,DIP,EPS,VG(2),SG(2),RI
	DATA DN/6.0E6,0.0E6,0.E6,0.,0.,0./,
     1    TA/6*.001/,
     1     D/6*1.0/,			! = 1. FOR NO LOSS CONE 
     1     A/6*1./,			! RATIO TPAR/TPERP
     1     B/6*0.10/,			! LOSS CONE TEMP
     1    ASS/6*0./,
     1    VD/6*0./,
     1    XC/3.30/
	DATA SPE/'e  ','H+ ','HE+','O+ ','   '/
C
	NPL=0
    1   DEN=0.
	RED=0.
	DO 2 J = 1,6
	REN(J) = 1836.1*ASS(J)
	IF(REN(J).EQ.0.) REN(J)=1.
	T(J)=TA(J)/TA(1)
	ISP(J)=SQRT(ASS(J))
	IF(ISP(J).LT.4.) ISP(J)=ISP(J)+1
	IF(DN(J).LE.0.) GOTO 2
	JMA=J
	RED=RED+DN(J)/REN(J)
	IF(ISP(J).EQ.1) DEN=DEN+DN(J)
    2   CONTINUE

C
	RN = REN(1)
C                **** normalized temperatures and velocities. ****
	DO 3 J=1,JMA
	REN(J)=REN(J)/RN
	T(J)=T(J)*REN(J)
    3   ST(J)=SQRT(T(J))
C
	DEK=12405.
	PFQ=RED/DEK
	PX=SQRT(PFQ)
	XA=XC/RN
	TR=TA(1)/RN
	CV=TR*(1022.+TR)/(511.+TR)**2
	CV=1./SQRT(CV)
	DEK=DEK*RN
C		**** PRINT PLASMA PARAMETERS. ****
	PRINT 101,PX,XC,DEN
 101    FORMAT(' PLASMA FREQ.:',F8.3,'KHZ GYRO FREQ.:',F8.3,'KHZ  ',
     1 	'ELECTRON DENSITY:'E9.3,'m-3')
C******
	print*,'isp',isp
	print*,'pfq',pfq
	print*,'px',px
	print*,'rn',rn
	print*,'cv',cv
	DO 4 J=1,JMA
 102    FORMAT(1x,A3,' DN=',E9.3,' T=',F7.4,' D=',F4.2,' A=',
     1	  F4.2,' B=',F4.2,' VD=',F6.2)
    4   PRINT 102,SPE(ISP(J)),DN(J),TA(J),D(J),A(J),B(J),VD(J)
c*********  this is PJK input *****
	p = 0.
	z = 10.
	f = 24000.
	STOP
	END
	SUBROUTINE DIFU(KOL,JMA,IERR)
	COMPLEX X,XX,E(6,4),XSI(6,4),XP,DF,U1,U2,U3,U12,U13,U32,
     1   A,B,C,DA,DB,DC,D,DX,DZ,DP,EFL(3),BFL(3)
	COMMON /XPZ/ XX(6),PP(6),ZZ(6),AA(6,2),DD(6),ASS(6),VD(6),
     1     DN(6),TA(6),XP(6),CV
	COMMON /COUT/ X,P,Z,EFL,BFL,D,DX,DZ,DP,E
C
C		******* FORM DIELECTRIC TENSOR **********
	DO 1 K = 1,4
	DO 1 I = 1,6
    1	E(I,K) = (0.,0.)
	E(1,1) = 1.
	E(4,1) = 1.
	E(6,1) = 1.
	DO 5 J=1,JMA

    5   CONTINUE
	RETURN
	END
