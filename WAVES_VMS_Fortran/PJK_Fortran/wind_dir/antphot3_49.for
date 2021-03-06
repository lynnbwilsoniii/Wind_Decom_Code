      PROGRAM ANTPHOT3
C
C	IN MINN, ANTPHOT3 DOES
C	THE DIFFERENCE BETWEEN TWO ANTENNAS WHICH ARE NOT QUITE COLINEAR.
C	ANTPHOT4 INCLUDES THE  EFFECT OF RC COUPLING IN THE PREAMP.
C
C	THIS PROGRAM IS INTENDED TO CALCULATE THE POTENTIAL OF AN ANTENNA
C	IN THE SOLAR WIND, AS A FUNCTION OF ROTATION ANGLE.  THE ANTENNA
C	IS ASSUMED TO HAVE HATS OF AREA HATA, ON ITS ENDS.  
C
	COMMON /PARAMS/ RSC,HSC,HANT,ANTR,ANTL,HATA,DANG,PI,PARES
	COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR,ESAT
      COMMON /VARBLK/ CAP,TILT,SPINP,VOIDL,CURDI,SATI,ANTA,PHISC,SUPPLY
	COMMON /ICHECK/ CURC(4)
      EXTERNAL FDERIV,OUTPUT
      DIMENSION F(10),DF(10),PRMT(20),WT(10),AUX(8,10)
	DIMENSION PDATA(500,3),PARLIST(10)
	DATA PI /3.14159265/              
	DATA PARLIST /3.E7,5.E7,7.E7,1.E8,2.E8,3.E8,5.E8,7.E8,1.E9,2.E9/
CT	DATA DENSW /5./                          !SOLAR WIND DENS IN /CM**3
	DATA DENSW /8./                          !SOLAR WIND DENS IN /CM**3
	DATA TESW /15./                          !SOLAR WIND TE IN EV
	DATA TISW /10./                          !SW TI IN EV
	DATA VELSW /400./                        !SW VELOCITY IN KM/SEC
	DATA TPHE1 /2./                          !PHOTO EL TEMP IN EV 
	DATA PHCURD /4.E-9/              !STD PHOTO EM. CURRENT, AMPS/CM**2
	DATA RSC /122./                          !SPACECRAFT RADIUS IN CM
	DATA HSC /183./                          !SPACECRAFT HEIGHT IN CM
	DATA HANT /91.5/                         !ANT HT. FROM BOTTOM IN CM
	DATA SUPPLY /0./                        !CURRENT SUPPLY VOLTAGE
	DATA PARES /5.E7/                        !CURRENT SUPPLY RESISTANCE
	DATA ANTR,ANTL /.019,4500./              !ANTENNA DIM. IN CM.
C	DATA ANTR,ANTL /.019,750./              !ANTENNA DIM. IN CM.
C	DATA ANTR,ANTL /.635,750./              !ANTENNA DIM. IN CM., .5 INCH
C	DATA ANTR,ANTL /1.27,750./              !ANTENNA DIM. IN CM., 1. INCH
	DATA DANG /1./
C   HELIOS
C	DATA ANTR,ANTL /.317,1600./             !ANTENNA DIM. IN CM., HELIOS
C      DATA TILT /.25/                             !SUN-SPACECRAFT AXIS ANGLE
C      DATA SPINP /1./                            !SPIN PERIOD IN SEC.
C	DATA RSC /86./                          !SPACECRAFT RADIUS IN CM
C
C	DATA HATA /171./                         !HAT AREA IN CM**2, LONG WIND
	DATA HATA /3./                         !HAT AREA IN CM**2
C
C	HIGH SPEED, LOW DENS STREAM
	DATA DENSW /3./                          !SOLAR WIND DENS IN /CM**3
	DATA TESW /12./                          !SOLAR WIND TE IN EV
	DATA TISW /20./                          !SW TI IN EV
	DATA VELSW /700./                        !SW VELOCITY IN KM/SEC
C
C	LOW SPEED, HIGH DENS STREAM
	DATA DENSW /20./                         !SOLAR WIND DENS IN /CM**3
	DATA TESW /12./                          !SOLAR WIND TE IN EV
	DATA TISW /3./                           !SW TI IN EV
	DATA VELSW /320./                        !SW VELOCITY IN KM/SEC
C
C	AVERAGE SOLAR WIND
	DATA DENSW /7./                          !SOLAR WIND DENS IN /CM**3
	DATA TESW /12./                          !SOLAR WIND TE IN EV
	DATA TISW /10./                          !SW TI IN EV
	DATA VELSW /450./                        !SW VELOCITY IN KM/SEC
C
      DATA NPL /0/
      DATA TWOPI /6.2831853/
      DATA WT /10*.1/
      DATA F /10*0./
      DATA NPR /0/
      DATA CAP /220.E-12/
      DATA TILT /.25/                             !SUN-SPACECRAFT AXIS ANGLE
      DATA SPINP /3./                            !SPIN PERIOD IN SEC.
C
	PRINT*,'WITH SHADOW'
	PRINT*,'WITH IONS'
	PRINT*,'ANTENNA MISALIGNMENT',DANG,'  DEG.'
C	SHDW = 57.29578*ATAN(RSC/ANTL)
	CAP = 2.415E-13*ANTL/(ALOG10(ANTL/ANTR) - .4)
C	PRINT*,'ANT CAP',CAP
C	ION THERMAL VELOCITY (1 DIM.) IN KM/SEC
	VTI = 3.E5*SQRT(TISW/935.E6)
C	VOIDL IS THE LENGTH OF THE ION VOID
	VOIDL = RSC*VELSW/VTI
	CURDI = DENSW*VELSW*1.602E-19
C
C	CALCULATE SPACECRAFT FLOATING POTENTIAL
C
      ASC = TWOPI*(RSC*HSC + RSC**2)                 !AREA FOR EL. PICKUP
      VTEL = SQRT(8.*TESW/PI/.51E6)*3.E10
      ESAT = .25*DENSW*VTEL*ASC*1.602E-19
      ASC = 2.*RSC*HSC                               !AREA FOR MAX PHOTOEM.
      ANTA = ASC
      HATASV = HATA
      HATA = 0.
      PHCUR = PHCURD/(1.-REFL(90.))                  ! TIMES 2 FOR REFL
      CALL INIT(F,DF)
      PHISC = F(1)
      HATA = HATASV
      PRINT*,'SPACECRAFT RADIUS,HEIGHT',RSC,HSC
      PRINT*,'SPACECRAFT FLOATING POTENTIAL',PHISC
      VMIN = 10000.
      VMAX = -10000.
      RMAX = 0.
      RMIN = 1.E15
C
C	CALCULATE CURRENTS FOR ANTENNA
C
      AANT = TWOPI*ANTR*ANTL + 2.*HATA               !AREA FOR EL. PICKUP
      VTEL = SQRT(8.*TESW/PI/.51E6)*3.E10
      ESAT = .25*DENSW*VTEL*AANT*1.602E-19
      VTIA = SQRT(8.*TISW/PI/935.E6)*3.E10
      SATI = .25*DENSW*VTIA*1.602E-19
      AANT = 2.*ANTR*ANTL                            !AREA FOR MAX PHOTOEM.
      PHCUR = PHCURD/(1.-REFL(90.))                  !CORRECT FOR REFL
      ANTA = AANT
C
	PRINT 105,ANTL,ANTR,HATA
 105	FORMAT(' ANT.LENGTH,CM'F7.0'  RADIUS'F7.4,5X,'HAT AREA'F7.1)
	PRINT*,'SUPPLY VOLTAGE',SUPPLY,'  RESISTANCE',PARES
	PRINT 106,DENSW,TESW
 106	FORMAT(' PLASMA DENSITY'F7.1'  TEMPERATURE(EV)'F7.1)
	PRINT*,'PHOTOELECTRON TEMPERATURES',TPHE1,TPHE2,' EV'
      PHCUR = PHCUR*AANT
	PRINT*,'MAXIMUM PHOTOCURRENT,ANT AT 90 DEG',PHCUR,' AMP'
      PHCUR = PHCURD/(1.-REFL(90.))                  !CORRECT FOR REFL
	PRINT*,'MAXIMUM PHOTOCURRENT/CM**2',PHCUR,' AMP'
	PRINT*,'SATURATED PLASMA ELECTRON CUR. TO ANT',ESAT
C
C      WRITE(6,700)
C  700 FORMAT(' TYPE MACH NO. (F8.3), RETURN FOR NO CHANGE')
C      READ(5,*) CAPT
C      IF(CAPT.NE.0.) CAP = CAPT
C
C	CHECK REFLECTION COEFFICIENT
C
	PRINT*,' ANGLE,REFLECTION COEFFICIENT'
	DO N = 0,9
	  ANG = N*10.
	  PRINT*,ANG,REFL(ANG)
	ENDDO
      WRITE(6,703)
  703 FORMAT(' OK, START INTEGRATION')
C
 104  FORMAT(' CAP'E12.3'  TILT'F6.2' DEG.,   FLOATING POTENTIAL',F8.2)
C
C     SET UP INITIAL CONDITIONS.
      CALL INIT(F,DF)
C      WRITE(8,104) CAP,TILT,F(1)
      PRINT 104,CAP,TILT,F(1)
      PRMT(2) = 0.
C      WRITE(8,101) PRMT(2),F
    1 CONTINUE
C     DO INTEGRATION around 1. revolutions
C
      PRINT 108
 108  FORMAT(/,'   ANGLE',5X,'VOLTS',5X,'TOT CUR',7X,'PHCUR',7X
     1 ,'ELCUR',7X,'IONCUR',5X,'PA CURR',6X,'RESIS')
      DO 100 NHALF = 1,2
      PRMT(3) = 1.1
      PRMT(4) = 1.E-5
C      DO 10 N = 1,16
C      PRMT(1) = PRMT(2)
C      PRMT(2) = PRMT(1) + SPINP*(5./360.)
C      DO K = 1,2
C        DF(K) = WT(K)
C      ENDDO
C      PHISV = F(1)
C      CALL RKGS(PRMT,F,DF,4,IHLF,FDERIV,OUTPUT,AUX)
C      IF(NPR.NE.0)       WRITE(8,101) PRMT(2),F
C	IYPL(1) = 100.*F(1) + 512.5
C	IYPL(2) = 100.*F(2) + 512.5
C	IYPL(3) =      F(5) + 512.5
C	IYPL(4) =      F(6) + 512.5
C	IYPL(5) =  10.*F(7) + 512.5
C	IYPL(6) =  10.*F(8) + 512.5
C      CALL BPLOT(IYPL)
 101  FORMAT(F8.2,10E12.3)
C	CALL CURRENT(F(1),F(2),CUR)
C	CALL CURRENT(F(1)+.01,F(2),CURD)
C	RES = .01/(CUR-CURD)
C	PRINT 101,F(2),F(1),CUR,CURC,RES
C      		COSA = COSD(F(2))*COSD(TILT)
C		XP = COSA
C		YP = SIND(F(2))
C		ZP = COSD(F(2))*SIND(TILT)
C     		CALL SHADOW(XP,YP,ZP,SUNLIT,HATL)
C		PRINT*,'XP,YP,SUNLIT,HATL',XP,YP,SUNLIT,HATL
C      IF(PHI.GT.PHIP) GO TO 1
C
  10  CONTINUE
C      DO 20 N = 1,25
      DO 20 N = 1,180
      PRMT(1) = PRMT(2)
      PRMT(2) = PRMT(1) + SPINP*(1./360.)
      DO K = 1,2
        DF(K) = WT(K)
      ENDDO
      CALL RKGS(PRMT,F,DF,4,IHLF,FDERIV,OUTPUT,AUX)
      CALL CURRENT(F(1),F(2),CUR)
      CALL CURRENT(F(1)+.01,F(2),CURD)
	VMAX = AMAX1(VMAX,F(1)-F(3))
	VMIN = AMIN1(VMIN,F(1)-F(3))
	RES = .01/(CUR-CURD)
	RMAX = AMAX1(RMAX,RES)
	RMIN = AMIN1(RMIN,RES)
C	PRINT 101,F(2),F(1),CUR,CURC,RES,F(3)
	NPL = NPL+1
	PDATA(NPL,1) = F(2)-360.
	PDATA(NPL,2) = F(1)-F(3)
   20  CONTINUE
C      DO 30 N = 1,15
C      PRMT(1) = PRMT(2)
C      PRMT(2) = PRMT(1) + SPINP*(5./360.)
C      DO K = 1,2
C        DF(K) = WT(K)
C      ENDDO
C      CALL RKGS(PRMT,F,DF,4,IHLF,FDERIV,OUTPUT,AUX)
C      CALL CURRENT(F(1),F(2),CUR)
C      CALL CURRENT(F(1)+.01,F(2),CURD)
C	RES = .01/(CUR-CURD)
C	PRINT 101,F(2),F(1),CUR,CURC,RES
   30  CONTINUE
 100  CONTINUE
C
      PRINT*,'MAX AND MIN POT. DIFF.,+ DIFF',VMAX,VMIN,VMAX-VMIN
      PRINT*,'MAX AND MIN EFFECTIVE RESISTANCE ',RMAX,RMIN
	RMAXT = RMAX/(1. - RMAX/PARES)
	RMINT = RMIN/(1. - RMIN/PARES)
      PRINT*,'MAX AND MIN RESISTANCE ',RMAXT,RMINT
	CALL MGPLOT(1,NPL,PDATA)
      STOP
      END
      SUBROUTINE INIT(F,DF)
	COMMON /PARAMS/ RSC,HSC,HANT,ANTR,ANTL,HATA,DANG,PI,PARES
	COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR,ESAT
        COMMON /ICHECK/ CURC(4)
      DIMENSION F(10),DF(10)
C
C     THIS ROUTINE SETS THE INITIAL CONDITIONS.  THE ANTENNA IS ASSUMED TO
C	BE AT 90 DEG. TO THE SUN, AND THE POTENTIAL AT THAT POINT IS 
C	ASSUMED TO BE POSITIVE
C
      CALL CURRENT(0.,90.,CURS)
      PHIS = -2.
      DO 1 N = 1,100
      PHI = N*.2 - 2.
      CALL CURRENT(PHI,90.,CUR)
C      PRINT 101, PHIS,PHI,CURS,CUR,PHIST
C      PRINT*,'CURR COMPS',CURC
      IF(CUR*CURS.LE.0.) GO TO 2
      CURS = CUR
      PHIS = PHI
   1  CONTINUE
   2  PHIST = PHIS - (PHIS-PHI)*CURS/(CURS-CUR)
C      WRITE(8,101) PHIS,PHI,CURS,CUR,PHIST
      PRINT 101, PHIS,PHI,CURS,CUR,PHIST
 101  FORMAT(' INIT',5E13.4)
      F(1) = PHIST
      F(3) = PHIST
      F(2) = 90.
      F(4) = 270. + DANG
      RETURN
      END
      SUBROUTINE FDERIV(T,F,DF)
C
C     THIS ROUTINE CALCULATES THE DERIVATIVE dV/dt
C
      COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR,ESAT
      COMMON /VARBLK/ CAP,TILT,SPINP,VOIDL,CURDI,SATI,ANTA,PHISC,SUPPLY
      DIMENSION F(10),DF(10)
C
      PHI = F(1)
      ANG = F(2)
      CALL CURRENT(PHI,ANG,CUR)
      DF(1) = CUR/CAP
      DF(2) = 360.*(1./SPINP)                 !SPIN RATE
      PHI = F(3)
      ANG = F(4)
      CALL CURRENT(PHI,ANG,CUR)
      DF(3) = CUR/CAP
      DF(4) = 360.*(1./SPINP)                 !SPIN RATE
C	PRINT 101,T,F
C	PRINT 101,T,DF
      RETURN
      END
      SUBROUTINE CURRENT(PHI,RANG,CUR)
C
      COMMON /PARAMS/ RSC,HSC,HANT,ANTR,ANTL,HATA,DANG,PI,PARES
      COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR,ESAT
      COMMON /VARBLK/ CAP,TILT,SPINP,VOIDL,CURDI,SATI,ANTA,PHISC,SUPPLY
      COMMON /ICHECK/ CURC(4)
      DATA COEF /1.27/
C
C	THIS RETURNS THE CURRENT TO THE ANTENNA, WHICH CONSISTS OF 4
C	COMPONENTS--(1) PHOTOELECTRONS LEAVING THE ANTENNA (POSITIVE
C	CURRENT)   (2) PLASMA ELECTRON PICKUP  (NEGATIVE)
C	(3) SOLAR WIND ION PICKUP  (POS.)  (4) CURRENT FROM PREAMP
C	CIRCUIT
C	THE EFFECT OF THE ION VOID ON THE ION CURRENT IS TAKEN INTO
C	ACCOUNT ONLY ROUGHLY.  IF THE ANTENNA IS LONGER THAN THE ION
C	VOID, THE VOID IS NEGLECTED.  IF SHORTER, THE VOID IS TREATED
C	AS THE SAME AS THE SUN SHADOW.
C
      COSA = COSD(RANG)*COSD(TILT)
      ANG = ACOSD(COSA)
      R = REFL(ANG)
      RANGT = ABS(RANG)
      RANGT = AMOD(RANGT,360.)
      IF(RANGT.GT.90..AND.RANGT.LT.270.) THEN
	XP = COSA
	YP = SIND(RANG)
	ZP = COSD(RANG)*SIND(TILT)
	CALL SHADOW(XP,YP,ZP,SUNLIT,HATL)
      ELSE                            ! NO SHADOW ON ANTENNA
	SUNLIT = ANTA
	HATL = HATA
      ENDIF
C	PRINT*,'RANG,SUNLIT,HATL',RANG,SUNLIT,HATL
      IF(PHI.LT.0.) THEN
        AR = ABS(SIND(ANG))*SUNLIT
	CURC(1) = PHCUR*AR*(1.-R)                   ! SATURATED PHOTOEMISSION
	R = REFL(90.-ANG)
	AR = ABS(COSD(ANG))*HATL
	CURC(1) = CURC(1) + PHCUR*AR*(1.-R)         ! DITTO FROM HATS
	CURC(2) = - ESAT*EXP(PHI/TESW)              ! REPELLED PLASMA ELS.
	ANTAI = SUNLIT
	IF(ANTL.GT.VOIDL) ANTAI = ANTA
        CURI1 = CURDI*ABS(SIND(ANG))*ANTAI
	CURI2 = PI*ANTAI*SATI*SQRT(1.-COEF*PHI/TISW)     ! ATTRACTED IONS
      ELSE
        AR = ABS(SIND(ANG))*SUNLIT
	CURC(1) = PHCUR*(1.-R)*AR*EXP(-PHI/TPHE1)   ! PHOTOEMISSION IS NOT SAT.
	R = REFL(90.-ANG)
	AR = ABS(COSD(ANG))*HATL
	CURC(1) = CURC(1) + PHCUR*AR*(1.-R)*EXP(-PHI/TPHE1)    !DITTO FROM HATS
	CURC(2) = - ESAT*SQRT(1.+COEF*PHI/TESW)          ! ATTRACTED PLASMA ELS.
	ANTAI = SUNLIT
	IF(ANTL.GT.VOIDL) ANTAI = ANTA
        CURI1 = CURDI*ABS(SIND(ANG))*ANTAI
	CURI2 = PI*ANTAI*SATI*EXP(-PHI/TISW)              ! REPELLED IONS
      ENDIF
      CURC(3) = SQRT(CURI1**2 + CURI2**2)
      CURC(4) = 0.
      IF(PARES.LT.1.E11) CURC(4) = (PHISC + SUPPLY - PHI)/PARES
      CANG = 180. - ANG
C
      IF(CANG.LT.2.) THEN
	CANG = SQRT((180.-RANG)**2 + TILT**2)
      ENDIF
C
      CUR = CURC(1) + CURC(2) + CURC(3) + CURC(4)
      RETURN
      END
      FUNCTION REFL(ANG)
      ANGM = ABS(ANG)
      ANGM = AMOD(ANGM,180.)
      IF(ANGM.GT.90.) ANGM = 180. - ANGM
      REFL = .5
      IF(ANGM.GT.40.) RETURN
      REFL = .5 + .5*((40.-ANGM)/40.)**2
      RETURN
      END
      SUBROUTINE SHADOW(XP,YP,ZP,SUNLIT,HATL)
C
	COMMON /PARAMS/ RSC,HSC,HANT,ANTR,ANTL,HATA,DANG,PI,PARES
C
C	GIVEN XP,YP,ZP = COMPONENTS OF A UNIT VECTOR ALONG THE 
C	ANTENNA, RETURNS SUNLIT = SUNLIT AREA OF WIRE AND HATL =
C	SUNLIT AREA OF HAT 
C
C	COMPUTE INTERSECTIONS XS,YS,ZS WITH SIDE OF SHADOW
C
      SUNLIT = 2.*ANTR*ANTL
      HATL = HATA
      IF(XP.GT.0.) RETURN
      YS = RSC
      SLENS = 10.*(ANTL+RSC)
      IF(YP.EQ.0.) GO TO 10
	ZS = (ZP/YP)*YS
	XS = (XP/YP)*YS
	SLENS = SQRT(XS**2 + YS**2 + ZS**2)
C
C	COMPUTE INTERSECTION WITH BOTTOM (OR TOP) OF SHADOW
C
 10   ZB = -HANT
      SLENB = 10.*(ANTL+RSC)
      IF(ZP.EQ.0.) GO TO 20
	YB = (YP/ZP)*ZB
	XB = (XP/ZP)*ZB
	SLENB = SQRT(XB**2 + YB**2 + ZB**2)
C
C	COMPUTE SHADOWED LENGTH
C
 20   SLEN = AMIN1(SLENS,SLENB)
      HATL = 0.
      SUNLIT = 0.
      IF(SLEN.GT.(RSC+ANTL)) RETURN
C
C	ANTENNA IS NOT ENTIRELY SHADOWED
C
      HATL = HATA
      SUNLIT = (ANTL + RSC - SLEN)*2.*ANTR
      RETURN
      END
      SUBROUTINE OUTPUT(T,F,DF,IHLF,NDIM,PRMT)
      COMMON /VARBLK/ CAP,TILT,SPINP,VOIDL,CURDI,SATI,ANTA,PHISC,SUPPLY
      DIMENSION F(10),DF(10),PRMT(20)
      IF(IHLF.EQ.12) RETURN
      IF(IHLF.GE.10) PRINT 101,IHLF,F(1),F(2),DF(1),DF(2)
  101 FORMAT(' IN OUTPUT, IHLF=',I3,' F,DF=',10E11.3)
C      IF(IHLF.GE.10) WRITE(6,101) IHLF,F
      RETURN
      END
      SUBROUTINE RKGS(PRMT,Y,DERY,NDIM,IHLF,FCT,OUTP,AUX)
C
C
      DIMENSION Y(1),DERY(1),A(4),B(4),C(4),PRMT(1),AUX(8,NDIM)
      DO 1 I=1,NDIM
    1 AUX(8,I)=.06666667*DERY(I)
      X=PRMT(1)
      XEND=PRMT(2)
      H=PRMT(3)
      PRMT(5)=0.
      CALL FCT(X,Y,DERY)
C
C     ERROR TEST
      IF(H*(XEND-X))38,37,2
C
C     PREPARATIONS FOR RUNGE-KUTTA METHOD
    2 A(1)=.5
      A(2)=.2928932
      A(3)=1.707107
      A(4)=.1666667
      B(1)=2.
      B(2)=1.
      B(3)=1.
      B(4)=2.
      C(1)=.5
      C(2)=.2928932
      C(3)=1.707107
      C(4)=.5
C
C     PREPARATIONS OF FIRST RUNGE-KUTTA STEP
      DO 3 I=1,NDIM
      AUX(1,I)=Y(I)
      AUX(2,I)=DERY(I)
      AUX(3,I)=0.
    3 AUX(6,I)=0.
      IREC=0
      H=H+H
      IHLF=-1
      ISTEP=0
      IEND=0
C
C
C     START OF A RUNGE-KUTTA STEP
    4 IF((X+H-XEND)*H)7,6,5
    5 H=XEND-X
    6 IEND=1
C
C     RECORDING OF INITIAL VALUES OF THIS STEP
    7 CALL OUTP(X,Y,DERY,IREC,NDIM,PRMT)
      IF(PRMT(5))40,8,40
    8 ITEST=0
    9 ISTEP=ISTEP+1
C
C
C     START OF INNERMOST RUNGE-KUTTA LOOP
      J=1
   10 AJ=A(J)
      BJ=B(J)
      CJ=C(J)
      DO 11 I=1,NDIM
      R1=H*DERY(I)
      R2=AJ*(R1-BJ*AUX(6,I))
      Y(I)=Y(I)+R2
      R2=R2+R2+R2
   11 AUX(6,I)=AUX(6,I)+R2-CJ*R1
      IF(J-4)12,15,15
   12 J=J+1
      IF(J-3)13,14,13
   13 X=X+.5*H
   14 CALL FCT(X,Y,DERY)
      GOTO 10
C     END OF INNERMOST RUNGE-KUTTA LOOP
C
C
C     TEST OF ACCURACY
   15 IF(ITEST)16,16,20
C
C     IN CASE ITEST=0 THERE IS NO POSSIBILITY FOR TESTING OF ACCURACY
   16 DO 17 I=1,NDIM
   17 AUX(4,I)=Y(I)
      ITEST=1
      ISTEP=ISTEP+ISTEP-2
   18 IHLF=IHLF+1
      X=X-H
      H=.5*H
      DO 19 I=1,NDIM
      Y(I)=AUX(1,I)
      DERY(I)=AUX(2,I)
   19 AUX(6,I)=AUX(3,I)
      GOTO 9
C
C     IN CASE ITEST=1 TESTING OF ACCURACY IS POSSIBLE
   20 IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD)21,23,21
   21 CALL FCT(X,Y,DERY)
      DO 22 I=1,NDIM
      AUX(5,I)=Y(I)
   22 AUX(7,I)=DERY(I)
      GOTO 9
C
C     COMPUTATION OF TEST VALUE DELT
   23 DELT=0.
      DO 24 I=1,NDIM
   24 DELT=DELT+AUX(8,I)*ABS(AUX(4,I)-Y(I))
      IF(DELT-PRMT(4))28,28,25
C
C     ERROR IS TOO GREAT
   25 IF(IHLF-10)26,36,36
   26 DO 27 I=1,NDIM
   27 AUX(4,I)=AUX(5,I)
      ISTEP=ISTEP+ISTEP-4
      X=X-H
      IEND=0
      GOTO 18
C
C     RESULT VALUES ARE GOOD
   28 CALL FCT(X,Y,DERY)
      DO 29 I=1,NDIM
      AUX(1,I)=Y(I)
      AUX(2,I)=DERY(I)
      AUX(3,I)=AUX(6,I)
      Y(I)=AUX(5,I)
   29 DERY(I)=AUX(7,I)
      CALL OUTP(X-H,Y,DERY,IHLF,NDIM,PRMT)
      IF(PRMT(5))40,30,40
   30 DO 31 I=1,NDIM
      Y(I)=AUX(1,I)
   31 DERY(I)=AUX(2,I)
      IREC=IHLF
      IF(IEND)32,32,39
C
C     INCREMENT GETS DOUBLED
   32 IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      IF(IHLF)4,33,33
   33 IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD)4,34,4
   34 IF(DELT-.02*PRMT(4))35,35,4
   35 IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      GOTO 4
C
C
C     RETURNS TO CALLING PROGRAM
   36 IHLF=11
      CALL FCT(X,Y,DERY)
      GOTO 39
   37 IHLF=12
      GOTO 39
   38 IHLF=13
   39 CALL OUTP(X,Y,DERY,IHLF,NDIM,PRMT)
   40 RETURN
      END
	SUBROUTINE MGPLOT(IHARD,NL,PDATA)
C
	DIMENSION PDATA(500,3)
	CHARACTER*20 CH(20)
C
	CH(1) = 'TERMINAL 3'
	IF(IHARD.EQ.1) CH(1) = 'PRINTER 6'
	CH(2) = 'XCOLUMN 1'
	CH(3) = 'YCOLUMN 2'
	CH(4) = 'LINES 1 359'
	CH(5) = 'LIMITS'
	CH(6) = 'CONNECT'
	CH(7) = 'BOX'
	CH(8) = 'XLABEL ANGLE TO SUN'
	CH(9) = 'ID'
	CH(10) = 'END'
      IF(IHARD.EQ.1) THEN
	CH(10) = 'HARDCOPY'
	CH(11) = 'END'
      ENDIF
	CALL MONGO(10+IHARD,CH,500,3,PDATA)
      RETURN
      END
