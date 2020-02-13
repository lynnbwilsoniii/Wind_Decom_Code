	PROGRAM EMCOPL
C	SOLVES COLD PLASMA DISPERSION EQUATION, NOT ELECTROSTATIC
C	IN STIX'S NOTATION ER IS R,EL IS L,EP IS P,EPERP IS S.
C	EPSL IS A.
C	THEN PLOTS THE RESULTS
C
	COMPLEX CNU,DN,FN,BN,EPSL,EN2P
	COMPLEX EN2M,XKP,XKM,ERN,ELN,EPN
	COMPLEX ER,EL,EP,EPERP,EPERPN,LAMBDA,ZPL,SQL,E(3),B(3)
 	REAL NU 
	COMMON /LIMITS/ BOTTOM,TOP
C
	DIMENSION WP(10),WC(10),NU(10),A(10),DENS(10),WP2(10)
	DIMENSION IPL(32)
	DATA NC /2/
C	DATA NU /1.E-5,1.E-7,8*0./
	DATA NU /10*0./
	DATA A /1.,16.,16.,7*1./
C	DATA DENS /1.,.95,.05,7*1./
	DATA DENS /1.,1.,.05,7*1./
C	WCYCL IS RATIO OF ELECTRON CYCLOTRON FREQ TO PLASMA FREQ
C	DATA WCYCL /.007/
	DATA WCYCL /.4/
	DATA TWOPI /6.28318531/
	DATA RSH /13.5/
	DATA ALT /250./
	DATA FP /3.4E6/
	DATA COSTK /1.0/
C	DATA COSTK /0.00873/  ! 89.5 DEGREES
C	DATA BOTTOM,TOP/50.,1100./             ! CIE PRINTER
	DATA BOTTOM,TOP/50.,896./              ! GENICOM 4440
C	FOR DETERMINING THE LOGARATHMIC SCALE OF THE Y AXIS
C	THE AXIS SWITCHES FROM LINEAR TO LOG AT Y = BBY AND THIS
C	IS A LENGTH OF YSCLR DOTS (FULL HT. IN DOTS IS TOP, ABOVE)
C	DATA BBY,YSCLR /.001,75./
	DATA BBY,YSCLR /1.,250./
C	DATA FHIGH,FLOW /20.2E3,19.8E3/
C	DATA FHIGH,FLOW /300.,.001/
	DATA FHIGH,FLOW /1.5E6,1.E5/
	DATA XMS,XPS /0.,0./
C	IQ = 1 IS PLOT K VS W, 2 IS PLOT B/E VS W
C	IQ = 3 IS PLOT N**2 VS W
	DATA IQ /2/
C
C	W,WC,WP2 ARE RATIO TO PLASMA FREQ. OF COMP. 1
C
C	NU(1) = 700.*EXP(-ALT/313.)
C	NU(2) = 70.*EXP(-ALT/87.)
	PRINT 701, ALT,FP,NU(1),NU(2)
 701	FORMAT('0ALT=',F8.1,' KM, FP=',E10.3,' HZ, E,I COLL F',2E10.3)
	PRINT 703, COSTK,(A(N),N=2,NC)
 703	FORMAT(' COS THETA ='F7.4,3X,'ION AT.WTS.',10F6.1)
	PRINT 704,(DENS(N),N=2,NC)
 704	FORMAT(22X,'ION REL.DENS.',10F6.3)
	PRINT 702
 702	FORMAT('0')
  	A(1) = -1./1836.
	DO 11 N = 1,NC
	WP2(N) = DENS(N)/1836./ABS(A(N))
	WC(N) = WCYCL/1836./A(N)
 11	CONTINUE
C
C	SET UP PLOT AND Y AXIS
C
C	LAST ARGUMENT IS FRACTION OF PAGE TO BE USED
	CALL BPLTBGN(1,1024,' ',' ',' ',1.)
	CALL SPLTERM
	IF(IQ.EQ.1)
     1	CALL BITPLTBGN('COLD PLASMA','KC/WP^    F->')
	IF(IQ.EQ.2)
     1	CALL BITPLTBGN('COLD PLASMA','B/E^    <-F')
	IF(IQ.EQ.3)
     1	CALL BITPLTBGN('COLD PLASMA','N**2^    F->')
	NPTS = 500
	NTOP = TOP-BOTTOM
	WRAT = ALOG(FHIGH/FLOW)/NPTS
	IF(IQ.EQ.2) WRAT = EXP(-WRAT)
	IF(IQ.NE.2) WRAT = EXP( WRAT)
	YSCL = YSCLR/BBY
	OFFSET = BOTTOM
	IF(IQ.EQ.3) OFFSET = OFFSET + .5*(TOP-BOTTOM)
	IPL(1) = OFFSET
	NM = 1
	DO 12 N = 1,5
	NM = NM+1
	Y = .2*N*BBY
	IPL(NM) = YSCL*FLILO(Y,BBY) + OFFSET
	IF(IQ.NE.3) GO TO 12
	NM = NM+1
	IPL(NM) = -YSCL*FLILO(Y,BBY) + OFFSET
 12	CONTINUE
	DO 13 N = 6,15
	NM = NM+1
	Y = 10.*Y
	IPL(NM) = YSCL*FLILO(Y,BBY) + OFFSET
	IF(IPL(N).GT.NTOP) GO TO 14
	IF(IQ.NE.3) GO TO 13
	NM = NM+1
	IPL(NM) = -YSCL*FLILO(Y,BBY) + OFFSET
 13	CONTINUE
 14	DO 16 IDOT = 1,10
	DO 15 N = 1,NM
	CALL SETDOT(IPL(N))
 15	CONTINUE
	CALL OVERLAY
	CALL BUFLINE
 16	CONTINUE
	CALL BLACKL
	CALL LABELX(7,'<-F(HZ)')
C
	W = FHIGH/WRAT/FP
	IF(IQ.NE.2) W = FLOW/WRAT/FP
	WS = W
	DO 1 NIT = 1,NPTS
	W=W*WRAT
	FHZ = FP*W
C     CALCULATE EPSILON 
      ER = 1. 
      EL = 1. 
      EP = 1. 
      EMAG = 1. 
      DO 10 N = 1,NC  
C     REPLACE M BY M*(1. - I*NU/W)
      CNU = CMPLX(0.,NU(N))/FP/TWOPI
      ER = ER - WP2(N)/W/(W-CNU+WC(N))
      EL = EL - WP2(N)/W/(W-CNU-WC(N))
      EP = EP - WP2(N)/W/(W-CNU)
      EMAG = AMAX1(EMAG,CABS(ER),CABS(EL),CABS(EP)) 
 10   CONTINUE
	ERN = ER/EMAG
	ELN = EL/EMAG
	EPN = EP/EMAG
      EPERPN = .5*(ERN + ELN)
      EPERP = .5*(ER + EL)
	DN = .5*(ERN - ELN)
	SINTK2 = 1. - COSTK**2
	FN = CSQRT(((ERN*ELN-EPN*EPERPN)*SINTK2)**2
     1    + 4.*(EPN*DN*COSTK)**2)
	BN = ERN*ELN*SINTK2 + EPN*EPERPN*(1. + COSTK**2)
	EPSL = EPERP*SINTK2 + EP*COSTK**2
	EN2P = .5*EMAG**2*(BN + FN)/EPSL
	XKP = W*CSQRT(EN2P)
	CALL EBPOL(EN2P,ER,EL,EP,COSTK,E,B)
C	IF(MOD(NIT,10).EQ.0) PRINT 100,E
C	IF(MOD(NIT,10).EQ.0) PRINT 100,B
	EN2M = .5*EMAG*(BN - FN)/(EPSL/EMAG)
	XKM = W*CSQRT(EN2M)
C	
	IF(IQ.NE.1) GO TO 31
	XKR = XKP
	XKR = YSCL*FLILO(XKR,BBY) + OFFSET
	IPL(1) = LIMITD(XKR)
	CALL SETDOT(IPL(1))
	XKR = XKM
	XKR = YSCL*FLILO(XKR,BBY) + OFFSET
	IPL(2) = LIMITD(XKR)
	CALL SETDOT(IPL(2))
  31	IF(IQ.NE.2) GO TO 32
	BERAT = SQRT((CABS(B(1))**2 + CABS(B(2))**2 + CABS(B(3))**2)/
     1    (CABS(E(1))**2 + CABS(E(2))**2 + CABS(E(3))**2))
	BER = YSCLR*FLILO(BERAT,1.)
	IPL(3) = LIMITD(BER)
C	IF(IQ.EQ.2.AND.XKR.GT.0.) CALL SETDOT(IPL(3))
	EN2PT = EN2P
	IF(IQ.EQ.2.AND.EN2PT.GT.0.) CALL SETDOT(IPL(3))
	CALL EBPOL(EN2M,ER,EL,EP,COSTK,E,B)
	IF(MOD(NIT,10).EQ.0) PRINT 100,E
	IF(MOD(NIT,10).EQ.0) PRINT 100,B
	XKR = XKM
	XKR = YSCL*FLILO(XKR,BBY)
	IPL(2) = LIMITD(XKR)
	IF(IQ.EQ.1) CALL SETDOT(IPL(2))
	BERAT = SQRT((CABS(B(1))**2 + CABS(B(2))**2 + CABS(B(3))**2)/
     1    (CABS(E(1))**2 + CABS(E(2))**2 + CABS(E(3))**2))
	BER = YSCLR*FLILO(BERAT,1.)
	IPL(4) = LIMITD(BER)
	EN2MR = EN2M
	IF(IQ.EQ.2.AND.EN2MR.GT.0.) CALL SETDOT(IPL(4))
   32	IF(IQ.NE.3) GO TO 34
C	PRINT*,'IQ=3',FHZ,W,EN2P,EN2M
	EN2R = EN2P
	IPL(1) = YSCL*FLILO(EN2R,BBY) + OFFSET
	CALL SETDOT(IPL(1))
	EN2R = EN2M
	IPL(2) = YSCL*FLILO(EN2R,BBY) + OFFSET
	CALL SETDOT(IPL(2))
C	PRINT 700,FHZ,XKP,XKM,EN2P,EN2M
   34	CONTINUE
C
C	PRINT 700,FHZ,EP,EPERP,ER,EL,EMAG
C	PRINT 700,FHZ,EPN,EPERPN,ERN,ELN
	IF(MOD(NIT,10).EQ.0)PRINT 700,FHZ,EN2P,XKP,EN2M,XKM
C
C     PUT TIC MARKS AT POWERS OF 10.
C
      WT = FHZ
      ITIC = 1
      DO 2 N = 0,8
C      TEST = BBX*10.**N 
      TEST =1000.*10.**(-N)
      IF(WS.GE.TEST.AND.WT.LT.TEST) ITIC = 0
      TEST = - TEST 
      IF(WS.GE.TEST.AND.WT.LT.TEST) ITIC = 0
    2 CONTINUE
      TEST = 0. 
      IF(WS.GE.TEST.AND.WT.LT.TEST) ITIC = 0
      WS = WT 
	IF(ITIC.EQ.0) CALL BIGTIC
	CALL OVERLAY
	CALL BUFLINE
 1	CONTINUE
C	
C	PUT FINAL AXIS AND Y TICS	
C
	CALL BLACKL
	DO 22 N = 1,11
	Y = .1*(N-1)*BBY
	IPL(N) = YSCL*FLILO(Y,BBY) + OFFSET
 22	CONTINUE
	NTOP = TOP-BOTTOM
	DO 23 N = 22,20
	Y = 10.*Y
	IPL(N) = YSCL*FLILO(Y,BBY) + OFFSET
	IF(IPL(N).GT.NTOP) GO TO 24
	NM = N
 23	CONTINUE
 24	DO 26 IDOT = 1,10
	DO 25 N = 1,NM
	CALL SETDOT(IPL(N))
 25	CONTINUE
	CALL OVERLAY
	CALL BUFLINE
 26	CONTINUE
C	MOVE END OF PLOT AWAY
	IPL(1) = 1
	IPL(2) = TOP
	DO 27 N = 1,25
 27	CALL BUFLINE
	CALL BPLTERM
C     PRINT 100,W,X(1),EPSR,EPS0,EP 
  100 FORMAT(' COPLAS' 8E12.4)  
  700 FORMAT(' 'F12.4,4(E13.4,E12.4),E12.4)
C      RETURN
	STOP
      END 
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
	FUNCTION LIMITD(X)
	COMMON /LIMITS/ BOTTOM,TOP
	X1 = AMAX1(X,1.)
	LIMITD = AMIN1(X1,TOP-BOTTOM) + BOTTOM
	RETURN
	END
	SUBROUTINE BLACKL
C
C	PUTS A VERTICAL BLACK LINE FROM BOTTOM TO TOP
C
	COMMON /LIMITS/ BOTTOM,TOP
C
	NTOP = TOP - BOTTOM
	DO 1 N = 1,NTOP
	IPL = N+BOTTOM
	CALL SETDOT(IPL)
  1	CONTINUE
	CALL OVERLAY
	CALL BUFLINE
	RETURN
	END
	SUBROUTINE EBPOL(EN2,ER,EL,EP,COSTK,E,B)
C
C	CALCULATES E (NORMALIZED TO 1.) AND B IN THE WAVES
C	
	COMPLEX EN2,ER,EL,EP,EPERP,D,XI,DEN,EN,E(3),B(3)
	DATA XI /(0.,1.)/
C
	SINTK2 = AMAX1(1.-COSTK**2,0.)
	SINTK = SQRT(SINTK2)
	D = .5*(ER-EL)
	EPERP = .5*(ER+EL)
	ENORM = AMAX1(CABS(ER),CABS(EL),CABS(EP))
C	ASSUME EX IS NOT ZERO AND DO RATIOS TO EX
	ERR = 1.E-7*ENORM
	DEN = EN2*SINTK2 - EP
	IF(CABS(DEN).LT.ERR) GO TO 100
	E(3) = EN2*SINTK*COSTK/DEN
   5	DEN = EN2 - EPERP
	IF(CABS(DEN).LT.ERR) GO TO 120
	E(2) = XI*D/DEN
	E(1) = 1.
	GO TO 200
C
 100	CONTINUE
C	EX IS 0. UNLESS N**2*COSTK*SINTK IS 0.
	IF(CABS(EN2)*COSTK*SINTK.GT.ERR) GO TO 110
	E(3) = 0.
	GO TO 5
 110	CONTINUE
C	EX IS 0.
	E(1) = 0.
	E(3) = 1.
	E(2) = -XI*EN2*COSTK*SINTK*E(3)/D
	GO TO 200
 120	CONTINUE
C	A STRANGE CASE. EN2 - EPERP = 0., SO EX = 0.
	E(2) = 1.
	E(1) = 0.
	E(3) = 0.
C	IF(CABS(EP-EN2*SINTK2).GT.ERR) E(3) = EN2*SINTK*COSTK
	DEN = EN2*COSTK*SINTK
	IF(CABS(DEN).GT.ERR) E(3) = XI*D*E(2)/DEN
C
C	NORMALIZE E TO LARGEST COMPONENT = (1.,0.)
C
 200	E1 = CABS(E(1))
	E2 = CABS(E(2))
	E3 = CABS(E(3))
	DEN = E(1)
	IF(E2.GT.E1.AND.E2.GT.E3) DEN = E(2)
	IF(E3.GT.E1.AND.E3.GT.E2) DEN = E(3)
	E(1) = E(1)/DEN
	E(2) = E(2)/DEN
	E(3) = E(3)/DEN
C
C	CALCULATE B
C
	EN = CSQRT(EN2)
	B(3) =  EN*SINTK*E(2)
	B(1) = -EN*COSTK*E(2)
	B(2) = EN*(COSTK*E(1) - SINTK*E(3))
	RETURN
	END
