	PROGRAM ANTPHOT
C
C	THIS PROGRAM IS INTENDED TO CALCULATE THE POTENTIAL OF AN ANTENNA
C	IN THE SOLAR WIND, AS A FUNCTION OF ROTATION ANGLE.  THE ANTENNA
C	IS ASSUMED TO HAVE HATS OF SOME KIND, OF AREA HATA, ON ITS ENDS.  
C
	COMMON /PARAMS/ RSC,ANTR,ANTL,HATA,PI,THETA,PHI,PHISUN,SHDW,CLN
	COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR
	DIMENSION XX(730),YY(730)
	DATA PI /3.14159265/
	DATA DENSW /5./
	DATA TESW /15./                          !SOLAR WIND TE IN EV
	DATA TISW /10./                          !SW TI IN EV
	DATA VELSW /400./                        !SW VELOCITY IN KM/SEC
	DATA TPHE1 /2./                          !PHOTO EL TEMP IN EV 
	DATA PHCUR /4.E-9/              !STD PHOTO EM. CURRENT, AMPS/CM**2
	DATA RSC /100./                          !SPACECRAFT RADIUS IN CM
	DATA ANTR,ANTL /.019,4500./              !ANTENNA DIM. IN CM.
	DATA HATA /171./                         !HAT AREA IN CM**2
C	DATA HATA /1./                         !HAT AREA IN CM**2
C
	SHDW = 57.29578*ATAN(RSC/ANTL)
   20	NPHI = 0
	DO 10 IPHI = 0,720
	PHI = .5*IPHI
	NPHI = NPHI+1
	PHISUN = SUNANG()
C
C	CALCULATE ILLUMINATED AREA AND PHOTOEMISSION
C
	PHOTOEM = AREA()*PHCUR
C
C	CALCULATE POTENTIAL SUCH THAT PHOTOEMISSION IS COMPENSATED BY 
C	PLASMA ELECTRON AND ION PICKUP
C
	ANTPOT = POTL(PHOTOEM)
	XX(NPHI) = PHI
	YY(NPHI) = ANTPOT
C
  10	CONTINUE
C
C	PLOT ANTENNA POTENTIAL VS. ROTATION ANGLE
C
	CALL PLTPOT(XX,YY,NPHI)
	READ (5,*) HATA
	CALL JCHTRM(.TRUE.)
	IF(HATA.GT.0.) GO TO 20
C
C	FOURIER ANALYSE POTENTIAL VS. ANGLE  AND PLOT SPECTRUM
C
	STOP
	END
	FUNCTION SUNANG
C
	COMMON /PARAMS/ RSC,ANTR,ANTL,HATA,PI,THETA,PHI,PHISUN,SHDW,CLN
	DATA RADDEG /.01745329/
C
C	SUNANG IS ANGLE BETWEEN ANTENNA AXIS AND SUN CENTER
C
	SUNANG = PHI
	RETURN
	END
	FUNCTION AREA
C
	COMMON /PARAMS/ RSC,ANTR,ANTL,HATA,PI,THETA,PHI,PHISUN,SHDW,CLN
	COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR
C
	UMBANG = SHDW - .25
	PENANG = SHDW + .25
C	PRINT*,' IN AREA,PHISUN,SHDW,CLN',PHISUN
	PHISUN = AMOD(PHISUN+720.,360.)
	IF(PHISUN.LE.90..OR.PHISUN.GE.270.) THEN
C	  ANTENNA ON SUNWARD SIDE, SHADED ONLY BY HAT
	  AREA = ANTL*2.*ANTR*ABS(SIND(PHISUN)) + HATA*ABS(COSD(PHISUN))
	ELSE
C	ANTENNA ON SHADY SIDE
C
	  COMPHI = ABS(PHISUN - 180.)
	  IF(COMPHI.GT.PENANG) THEN
C	    AT LEAST PART OF ANTENNA IS IN FULL SUNLIGHT, SO EFFECT OF
C	    PENUMBRA AVERAGES TO SHARP SHADOW
	    AREA = ((ANTL+RSC)*ABS(SIND(PHISUN))-RSC)*2.*ANTR 
     1             +HATA*ABS(COSD(PHISUN))
C
          ELSE
            AREA = .005*(2.*PI*ANTL*ANTR + HATA)
          ENDIF
	ENDIF
	AREA = ABS(AREA)
	RETURN
	END
	FUNCTION POTL(PHOTOEM)
C
	COMMON /PARAMS/ RSC,ANTR,ANTL,HATA,PI,THETA,PHI,PHISUN,SHDW,CLN
	COMMON /PLASMA/ DENSW,TESW,TISW,VELSW,TPHE1,TPHE2,PHCUR
C
C	
	AREAEL = 2.*PI*ANTR*ANTL + 2.*HATA
	AREAIN = 2.*ANTR*ANTL*SIND(PHISUN)
	VTE = SQRT(8.*TESW/PI/.511E6)*2.9979E10
	ESATCR = .25*DENSW*VTE*1.602E-19*AREAEL
	POTL = -TPHE1*ALOG(ESATCR/PHOTOEM)
C	PRINT 100,AREAEL,VTE,PHOTOEM,ESATCR,POTL
 100	FORMAT(' IN POTL,A,VTE,PH,ISAT,POTL',5E11.3)
	RETURN
	END
	SUBROUTINE PLTPOT(XX,YY,NPHI)
C
	DIMENSION XX(1),YY(1),SPACE(2000),VPARY(4),WNARY(4)
	DATA ISIZE /2000/
C
	CALL JCHINI(.TRUE.,1)
	CALL JVSPAC(-1.,1.,-.75,.75)
	CALL JCHART(SPACE,ISIZE)
C
	IPAR = 1
	CALL JGRAPH(SPACE,ISIZE,IPAR)
	CALL JCHEXT(SPACE,ISIZE,0, 1000., 750.)
	CALL JCHEXT(SPACE,ISIZE,1, 1000., 750.)
C
	CALL JXTEXT(SPACE,ISIZE,1,1,0.,1.4,9)
	CALL JXLINE(SPACE,ISIZE, 1, 9, 16383, 0, 16383)
C
!		DEFINE PLOT LINE CHARACTERISTICS.

	CALL JTXBOX (SPACE, ISIZE, -1, -1, 1)
	CALL JCHATR (SPACE, ISIZE,  1, -1,-1)

!		DEFINE PLOT COORDINATES AND AXES 

	CALL JHAXIS (SPACE,ISIZE,IPAR,  1,.0,  360., '$$')
	CALL JVAXIS (SPACE,ISIZE,IPAR,  2,-10.0, 6.0, '$$')
C
C	FOR JAXATR ARG3-7=CHRTID,AXISID FROM JH OR VAXIS,LINDEX FROM
C	JXLINE, SCALEF,ITYPE	
C	ITYPE = 1, LINEAR SCALE; 2 IS LOG 10, 3 IS Ln e
C
	CALL JAXATR (SPACE, ISIZE, IPAR,1,1,0,1)
	CALL JAXATR (SPACE, ISIZE, IPAR,2,1,0,1)
	CALL JTIC (SPACE, ISIZE,  1, 1,1,10.,10.,1.)
	CALL JTIC (SPACE, ISIZE,  1, 2,2,-10.,6.,2.)
C
C	DISABLE DISPLAY OF X AXIS
C
	CALL JCAXIS (SPACE, ISIZE, 1,1,0,0,0,0,1)	
C
	XMOVE = 149.
	CALL JAXPOS (SPACE,ISIZE, IPAR,2,XMOVE,0.,0.)
C

!		SHOW CHART.
C
	CALL JCHSHW (SPACE, ISIZE, -1.0, 1.0 , -.75, .75)
C
C	TRY SOME DISPLAYS
C
C	CALL JADNOT(SPACE, ISIZE, CHRTID,1,1,' ADD NOTE')
C	CALL JPONOT(SPACE, ISIZE, CHRTID,1,0.,0.)
C
C	INQUIRE VIEWPORT AND WINDOW INFORMATION
C
	CALL JWNDTA (SPACE, ISIZE, 1, VPARY, WNARY )
C	PRINT*,'VP',VPARY
	VPARY(1) = -.76
	VPARY(2) = 1.
C	WNARY(1) = 0.
C	WNARY(2) = 12.
	CALL JWINDO (WNARY(1),WNARY(2),WNARY(3),WNARY(4))
	CALL JVPORT (VPARY(1),VPARY(2),VPARY(3),VPARY(4))
C
C	CALL JWCLIP(.TRUE.)
	CALL JOPEN
	CALL JCMARK (1)
	CALL JCOLOR (9)
C
!		PASS CALCULATED VALUES TO GRAFMAKER

	CALL JPMARK (XX, YY, NPHI)
C
	CALL JCLOSE
C
!		PAUSE FOR VIEWING

	CALL JPAUSE (1)
C
	CALL JCHCLR (1)
	RETURN
	END
