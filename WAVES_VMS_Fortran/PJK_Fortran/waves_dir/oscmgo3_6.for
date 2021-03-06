	PROGRAM OSCMGO
C
C	PLOT RESULTS OF OSCARS RUNS WHICH HAVE BEEN STORED IN FOR008
C	SAME AS OSCPLT EXCEPT USES MONGO
C
	COMMON /PPAK/ XMIN,XMAX,DELX,YMIN,YMAX,DELY,FILFAC
	COMMON /PLTBLK/ PLT(200,10)
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PXDEF,PYDEFF,COFFMGO,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PIMGO,USERVAR(10),AUTODOT
C
	CHARACTER*30 CH(30)
	DIMENSION RATIOM(10),DENS(10),BETAZ(10),TPAR(10),TPERP(10)
	DIMENSION XX(900),YY(900),ZZ(900),SYM(900)
C
	NPLOT = 0
 666	NPLOT = NPLOT+1
	IF(NPLOT.EQ.1) 
     1	OPEN(UNIT=8,FILE='FOR008.DAT;1',TYPE='OLD',READONLY)
	IF(NPLOT.EQ.2) 
     1	OPEN(UNIT=8,FILE='FOR008.DAT;2',TYPE='OLD',READONLY)
	IF(NPLOT.EQ.3) 
     1	OPEN(UNIT=8,FILE='FOR008.DAT;3',TYPE='OLD',READONLY)
	XMIN = 0.
	XMAX = 0.
	YMIN = 0.
	YMAX = 0.
	N = 0
	READ (8,105) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
	READ (8,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
	WCI = WCYCL/RATIOM(2)
C
	PRINT*,'MODE=',MODE
	IF(MODE.GT.6) GO TO 50
	IF(MODE.LE.2) GO TO 30
C
C	READ IN THE DATA FROM A FILE FOR008.DAT
C
  10	READ (8,361,END=20) WR,WI,DML,DTR,DTI,XKX,XKZ
C  	print 361, WR,WI,DML,DTR,DTI,XKX,XKZ
	N = N+1
C	THE FOLLOWING SET IS FOR RADIUS PROP. TO. WR, DIRECTION OF K
C	I.E.
	XK = SQRT(XKX**2 + XKZ**2)
	PLT(N,5) = WR*XKX/XK
	PLT(N,6) = WR*XKZ/XK
C	THE FOLLOWING SET IS FOR RADIUS PROP. TO. WI, DIRECTION OF K
	PLT(N,7) = -WI*XKX/XK
	PLT(N,8) = -WI*XKZ/XK
C
C	WRITE A SPECIAL FILE
C
	WRITE(9,777) WR,WI,XKX,XKZ,WR*XKX/XK,WR*XKZ/XK,-WI*XKX/XK
     1		,-WI*XKZ/XK
 777	FORMAT(8E12.4)
C	THE FOLLOWING IS FOR KX VS KZ
C	XX(N) =XKX
C	YY(N) = XKZ
C	SYM(N) = DTR
C	YMIN = AMIN1(YMIN,YY(N))
C	YMAX = AMAX1(YMAX,YY(N))
C	XMIN = AMIN1(XX(N),XMIN)
C	XMAX = AMAX1(XX(N),XMAX)
	GO TO 10
C
  20	CONTINUE
	CALL OSCAMG(5)
C	CALL OSCAMG(6)
	
	IF(NPLOT.EQ.3) call oscamg(10)
	IF(NPLOT.EQ.3) STOP
	GO TO 666
C
C	PLOT MODE = 1 OR 2, W VS MAG(K)
C
C
C	READ IN THE DATA FROM A FILE FOR008.DAT
C
  30	READ (8,361,END=40) WR,WI,DML,DTR,DTI,XKX,XKZ
C  	print 361, WR,WI,DML,DTR,DTI,XKX,XKZ
	N = N+1
C	THE FOLLOWING SET IS FOR RADIUS PROP. TO. WR, DIRECTION OF K
C	I.E.
	XK = SQRT(XKX**2 + XKZ**2)
	PLT(N,5) = WR
	PLT(N,6) = WI
	PLT(N,7) = XK
	NPLT = N
	TEMP = ABS(YMIN)
	NTEMP = TEMP
	TEMP = NTEMP+1
	GO TO 30
 40	PRINT*,'AT 40,NPLT',NPLT
c	XMAX = AMAX1(ABS(YMAX),ABS(YMIN),ABS(XMAX))
C	YMIN = -YMAX
c	YMIN = 0.
c	DELX = .1E-6
c	xmin = 0.
c	xmax = .9e-6
c	ymax = xmax
c	xmax = 1.143*xmax
c	FILFAC = 1.
c	DELY = DELX
C	PRINT*,(XX(I),YY(I),I=1,NPLT)
C
C	FILL IN SO MONGO DOESNT PLOT ZEROS
C
	DO N = NPLT+1,200
	  PLT(N,5) = PLT(N-1,5)
	  PLT(N,6) = PLT(N-1,6)
	  PLT(N,7) = PLT(N-1,7)
	ENDDO
	NCH = 1
	CH(NCH) = 'PRINTER 1'
	NCH = NCH+1
	CH(NCH) = 'ERASE'
	NCH = NCH+1
	CH(NCH) = 'WINDOW 1 2 1'
	NCH = NCH+1
	CH(NCH) = 'XCOLUMN 7'
	NCH = NCH+1
	CH(NCH) = 'YCOLUMN 5'
	NCH = NCH+1
	CH(NCH) = 'LIMITS'
	NCH = NCH+1
	CH(NCH) = 'CONNECT'
	NCH = NCH+1
	CH(NCH) = 'BOX'
	NCH = NCH+1
	CH(NCH) = 'XLABEL k c /wpe'
	NCH = NCH+1
	CH(NCH) = 'YLABEL Re(w)/wpe'
	NCH = NCH+1
	CH(NCH) = 'WINDOW 1 2 2'
	NCH = NCH+1
	CH(NCH) = 'XCOLUMN 7'
	NCH = NCH+1
	CH(NCH) = 'YCOLUMN 6'
	NCH = NCH+1
	CH(NCH) = 'LIMITS'
	NCH = NCH+1
	CH(NCH) = 'CONNECT'
	NCH = NCH+1
	CH(NCH) = 'BOX'
	NCH = NCH+1
	CH(NCH) = 'XLABEL k c /wpe'
	NCH = NCH+1
	CH(NCH) = 'yLABEL Im(w)/wpe'
	NCH = NCH+1
	CH(NCH) = 'ID'
	NCH = NCH+1
	CH(NCH) = 'HARDCOPY'
	NCH = NCH+1
	CH(NCH) = 'END'
C	PRINT*,(CH(N),N=1,NCH)
	CALL MONGO(NCH,CH,200,10,PLT)
 105	FORMAT(8E12.5,3I3)
 106	FORMAT(5F10.5,3F5.3,3I2)
 101	FORMAT(5E10.4)
  361 FORMAT (1X,E12.6 ',' E10.3,1X,F8.3,F8.3,',',F7.3,E12.4 ',' E11.4)
	STOP
C
C	READ IN THE DATA FROM A FILE FOR008.DAT--MODE GT 6
C
  50	READ (8,361,END=60) WR,WI,DML,DTR,DTI,XKX,XKZ
	N = N+1
	PRINT*,N,WR,WI,XKX,XKZ
	XX(N) = XKX
	YY(N) = XKZ
	ZZ(N) = WI
	XK = SQRT(XKX**2 + XKZ**2)
	PLT(N,3) = XKX
	PLT(N,4) = XKZ
	PLT(N,5) = WR*XKX/XK
	PLT(N,6) = WR*XKZ/XK
C	THE FOLLOWING SET IS FOR RADIUS PROP. TO. WI, DIRECTION OF K
	PLT(N,7) = -WI*XKX/XK
	PLT(N,8) = -WI*XKZ/XK
C	YMAX = AMAX1(YMAX,YY(N))
C	XMAX = AMAX1(XX(N),XMAX)
	GO TO 50
C
  60	CONTINUE
C	NOW PLOT THE RESULTS WHICH HAVE BEEN READ IN
C
	FILFAC = 1.
	NPLT = N
C********
c	NPLT = 71
C
C	PLOT CONSTANT W CONTOUR IN K SPACE
C
	XMAX = 0.
	YMAX = 0.
	DO 62 N = 1,NPLT
	YMAX = AMAX1(YMAX,YY(N))
	XMAX = AMAX1(XX(N),XMAX)
	SYM(N) = 1.
  62	CONTINUE
	PRINT*,'NPLT',NPLT,' X,YMAX',XMAX,YMAX
c	YMAX = AMAX1(ABS(YMAX),ABS(XMAX))
c	XMAX = AMAX1(ABS(YMAX),ABS(XMAX))
	XMAX = 1.2*XMAX
	YMAX = 1.2*YMAX
C
C	LOAD REST OF ARRAY TO AVOID ZEROS IN PLOT
C
	DO  N = NPLT+1,200
	  DO J = 1,10
	    PLT(N,J) = PLT(N-1,J)
	  ENDDO
	ENDDO
C	DELX = .1*XMAX
C	NXMAX = (ALOG10(DELX) + 50.)
C	DELX = 10.**(NXMAX-50)
C	DELY = DELX
C
	PRINT*,'NPLT',NPLT,' X,YMAX',XMAX,YMAX
C	YMAX = AMAX1(ABS(YMAX),ABS(XMAX))
C	XMAX = AMAX1(ABS(YMAX),ABS(XMAX))
C	XMAX = 1.2*XMAX
C	YMAX = 1.2*YMAX
C	XMAX = 1.140*XMAX
C	DELX = .01
C	DELY = DELX
C
C	PLOT CONSTANT W CONTOUR IN K SPACE
C
	X1 = 0.
	Y1 = 0.
	x2 = xmax
	y2 = ymax
c	X2 = AMAX1(XMAX,YMAX)
c	Y2 = X2
	PRINT*,'MGO2 USER LIMITS',X1,X2,Y1,Y2
	CALL OSCAMG(2)
C
C	PLOT IMAGINARY PART OF FREQUENCY
C
	XMAX = 0.
	YMAX = 0.
	DO 61 N = 1,NPLT
	  XK = SQRT(XX(N)**2 + YY(N)**2)
	  IF(XK.EQ.0.) XK = 1.
	  WI = ZZ(N)/WCI
 	  YY(N) = -WI*YY(N)/XK
	  XX(N) = -WI*XX(N)/XK
	  PLT(N,7) = XX(N)
	  PLT(N,8) = YY(N)
	  YMAX = AMAX1(YMAX,YY(N))
	  XMAX = AMAX1(XX(N),XMAX)
	  SYM(N) = 1.
  61	CONTINUE
	X1 = 0.
	Y1 = 0.
	x2 = xmax
	y2 = ymax
	X2 = AMAX1(XMAX,YMAX)
	Y2 = X2
C 
	CALL OSCAMG(3)
C
	STOP
	END
	SUBROUTINE OSCAMG(IMRP)
C
	CHARACTER*50 COM(20)
	COMMON /PLTBLK/ PLT(200,10)
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PXDEF,PYDEFF,COFFMGO,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PIMGO,USERVAR(10),AUTODOT
C
C	ITERM = 3 FOR TERMINAL, -1 or -2 FOR PRINTER 
	DATA ITERM /-1/
C	DATA ITERM /3/
C
c	DO N = 1,10
c	PRINT*,(PLT(N,J),J=1,4)
c	ENDDO
C
	IF(ITERM.GT.0) THEN
	  COM(1) = 'TERMINAL 3'
	  NCOM = 1
	ELSE
C	  COM(1) = 'PRINTER 1'
	  COM(1) = 'PRINTER 2'
	  NCOM = 1
	ENDIF
	NCOM = NCOM+1
	  COM(NCOM) = 'LIMITS 0. 1.E-6  0. .85E-6'
	IF(IMRP.EQ.0) COM(NCOM) = 'INPUT SHZMGO.MGO'
	IF(IMRP.EQ.1) COM(NCOM) = 'INPUT SHZMGO.IMG'
	IF(IMRP.EQ.2) COM(NCOM) = 'INPUT OSCPLT.MGO2'
	IF(IMRP.EQ.3) COM(NCOM) = 'INPUT OSCPLT.MGO3'
	IF(IMRP.EQ.5) COM(NCOM) = 'INPUT SHZMGO.MGO4'
	IF(IMRP.EQ.10) then
	  IF(ITERM.LT.0) THEN
	   NCOM = NCOM+1
	   COM(NCOM) = 'HARDCOPY'
	  ENDIF
	  NCOM = NCOM+1
	  COM(NCOM) = 'END'
	  CALL MONGO(NCOM,COM,200,10,PLT)
	endif
	PRINT*,'END OSCAMG',IMRP
C  120	STOP
	END
