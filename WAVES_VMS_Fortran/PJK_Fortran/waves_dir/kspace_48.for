	PROGRAM KSPACE
C
C	MAKES A TABLE FOR CONTOUR PLOTS IN K SPACE
C
	REAL WMATRIX(16,62)		! (KX VALUES,KZ VALUES)
	REAL XX(20),ZZ(62)
	REAL RATIOM(10),DENS(10),BETAZ(10),TPAR(10),TPERP(10)
	REAL ZLEVEL(20),TEMP(100)
	CHARACTER*20 FILENAME
	CHARACTER*10 TITLE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	INORM = 0		! NORMALIZE K TO WP/C
	FNORM = 1.
C	INORM = 1		! NORMALIZE K TO RLi
C
	CMAX = -1.E6
	XMIN = 1.E6
	XMAX = -1.E6
	ZMIN = 1.E6
	ZMAX = -1.E6
	ITERM = -1
C
	NCOLS = 16			! NUMBER OF VALUES OF KZ
	DO N = 1,NCOLS
C	DO N = 10,14
	  WRITE(FILENAME,777) N
 777	  FORMAT('FOR008.DAT;',I2)
	  PRINT*,FILENAME
	  OPEN(UNIT=8,FILE=FILENAME,STATUS='OLD',READONLY)
C	  READ FIRST LINE
	  READ(8,1005) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
 1005	  FORMAT(8E12.5,3I3)
C	  READ PLASMA PARAMETERS
 	  READ(8,1011)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
	  PRINT*,NC,RATIOM(NC),DENS(NC),BETAZ(NC)
 1011	  FORMAT(5E12.4)
C
	  IF(INORM.EQ.1) FNORM = SQRT(2.*TPERP(3))*RATIOM(3)/WCYCL	  
	  PRINT*,'INORM,FNORM',INORM,FNORM
	  DO K = 1,KIT
	    READ(8,1361) WR,WI,DML,DTR,DTI,XKXT,XKZT
 1361 FORMAT (1X,E12.6 ',' E10.3,1X,F8.3,F8.3,',',F6.3,E12.4 ',' E11.4)
C	     TO PLOT WR CONTOURS
C	    WMATRIX(N,K) = WR
C	     TO PLOT WI CONTOURS
	    WMATRIX(N,K) = WI
	    IF(WMATRIX(N,K).GT.CMAX) THEN
	      CMAX = WMATRIX(N,K)
	      XKMAX = XKXT
	      XZMAX = XKZT
	    ENDIF
	    XX(N) = XKXT
	    ZZ(K) = XKZT
	    XMIN = AMIN1(XMIN,XKXT)
	    XMAX = AMAX1(XMAX,XKXT)
	    ZMIN = AMIN1(ZMIN,XKZT)
	    ZMAX = AMAX1(ZMAX,XKZT)
	  ENDDO	  
	  PRINT*,XX(10),ZZ(1)
	ENDDO
	IF(INORM.NE.0) THEN
	  DO N = 1,NCOLS
	    XX(N) = XX(N)*FNORM
	  ENDDO
	  DO K = 1,KIT
	    ZZ(K) = ZZ(K)*FNORM
	  ENDDO
	ENDIF
C
C	IF KZ IS NEGATIVE, REVERSE THE MATRIX SO THAT TOP IS NEAREST TO
C		KZ = 0
C
	IF(ZZ(1).LT.0.) THEN
	  PRINT*,'MATRIX FLIPPED'
	  DO N = 1,NCOLS
	    DO K = 1,KIT
	       TEMP(K) = WMATRIX(N,KIT+1-K)
	    ENDDO
	    DO K = 1,KIT
	       WMATRIX(N,K) = TEMP(K)
	    ENDDO
	  ENDDO
	  DO K = 1,KIT
	     TEMP(K) = ZZ(KIT+1-K)
  	  ENDDO
	  DO K = 1,KIT
	     ZZ(K) = TEMP(K)
	  ENDDO
	ENDIF
C
C	NOW WMATRIX IS LOADED, PLOT CONTOURS 
C
        CALL MGOINIT
        CALL MGOSETUP(ITERM)                       !TERMINAL
        CALL MGOERASE
	LEVELS = 5
	DO L = 1,LEVELS
	  ZLEVEL(L) = (L-1)*CMAX/(LEVELS-1)
	ENDDO
	ZLEVEL(LEVELS) = .999*ZLEVEL(LEVELS)
C
	print*,'min,max',xmin,xmax,zmin,zmax
	print*,'WI max',CMAX
	XRANGE = ABS(XMAX-XMIN)
	ZRANGE = ABS(ZMAX-ZMIN)
	XRT = XRANGE
	ZRT = ZZ(KIT) - ZZ(1)
C	POWER = 1.
C 40	XRT = XRANGE
C	IF(XRT.GT.FLOAT(NCOLS)) GO TO 41
C	POWER = 10.*POWER
C	XRT = 10.*XRT
C	GO TO 40
C 41	CONTINUE
C	NXRT = XRT
C	XRT = NXRT/POWER
	BXTIC = XRT/(NCOLS-1)
	SXTIC = .5*BXTIC
	CALL MGOSETLIM(XMIN,ZZ(1),XMAX,ZZ(KIT))
C	CALL MGOTICKSIZE(-1., -1., 0., 0.)
C	CALL MGOTICKSIZE(SXTIC, BXTIC, 0., 0.)
	XTITLE = XMIN
	ZTITLE = ZZ(1) - .01*ZRT
	CALL MGOBOX(0,0)
	CALL MGOCONTOUR(WMATRIX,NCOLS,62,ZLEVEL,LEVELS)
	CALL MGOSETEXPAND(.8)
	NSPACE = 1
	IF(ITERM.EQ.-1.AND.NCOLS.GT.10) NSPACE = 2
	DO N = 1,NCOLS,NSPACE
	  CALL MGORELOCATE(XTITLE,ZTITLE)
	  IF(XX(N).LT.1.) THEN
	    WRITE(TITLE,1055) XX(N)
	    LL = 5
	  ENDIF
 1055	  FORMAT(F5.4)
	  IF(XX(N).GE.1.) THEN
	    WRITE(TITLE,1056) XX(N)
	    LL = 5
	  ENDIF
 1056	  FORMAT(F5.3)
	  CALL MGOPUTLABEL(6,TITLE,2) 
	print*,n,xtitle,ztitle,xx(n),title
	  XTITLE = XTITLE + NSPACE*BXTIC
	ENDDO
C
	XTITLE = XMIN - .005*XRANGE
	ZTITLE = ZZ(1)
	ZRT = ZZ(KIT) - ZZ(1)
	BZTIC =10.*ZRT/(KIT-1)
	DO N = 1,KIT,10
	  CALL MGORELOCATE(XTITLE,ZTITLE)
	  WRITE(TITLE,1057) ZZ(N)
 1057	  FORMAT(F7.5)
	  CALL MGOPUTLABEL(7,TITLE,4) 
	  ZTITLE = ZTITLE + BZTIC
	ENDDO
	CALL MGOSETEXPAND(1.1)
	IF(INORM.EQ.0) THEN
	  CALL MGOXLABEL(11,'Kx c/\gw\dp')
	  CALL MGOYLABEL(11,'Kz c/\gw\dp')
	ENDIF
	IF(INORM.EQ.1) THEN
	  CALL MGOXLABEL(10,'Kx R\dL\di')
	  CALL MGOYLABEL(10,'Kz R\dL\di')
	ENDIF
	CALL MGOSETEXPAND(1.)
	CALL MGOPLOTID('[.WAVES]','KSPACE')
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	ENDIF
C
	STOP
	END
