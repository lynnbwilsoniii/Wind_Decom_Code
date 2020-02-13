	PROGRAM OSCPUB3
C
C	PLOT RESULTS OF TRIADS OF OSCARS RUNS WHICH HAVE BEEN STORED IN FOR008
C	WILL PLOT THE RESULTS FROM UP TO 6 FILES.  LASTFILE = NUMBER OF 
C	FILES TO BE PLOTTED
C	I COULDNT MAKE OSCPUB RUN SO REWROTE THIS TO PLOT WITH A SUBROUTINE
C
C	DERIVED FROM OSCPUB2.FOR
C
C	REQUIRES  FOR008.DAT FILES, FOR008.DAT;0, FOR008.DAT;-1, 
C	FOR008.DAT;-2, ETC., WITH  MODE = 4 OR 5 RESULTS FROM WHISTLER, ION-
C	CYCLO-ALFVEN, AND ION ACOUSTIC. 
C
	COMMON /PPAK/ XMIN,XMAX,DELX,YMIN,YMAX,DELY,IFIG,RAD
	COMMON /PLTBLK/ PLT(750,10)
	COMMON /PLTR/ WRXMIN(10),WRXMAX(10),WRYMIN(10),WRYMAX(10)
	COMMON /PLTI/ WIXMIN(10),WIXMAX(10),WIYMIN(10),WIYMAX(10)
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PXDEF,PYDEFF,COFFMGO,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PIMGO,USERVAR(10),AUTODOT
C
	CHARACTER*1  JUNK
	CHARACTER*10  RLABEL
	DIMENSION RATIOM(10),DENS(10),BETAZ(10),TPAR(10),TPERP(10)
	DIMENSION XX(900),YY(900),ZZ(900),SYM(900),BETAPERP(10)
	REAL BETAPAR(10)
C
	ITERM = 3
C	ITERM = -1
	LASTFILE = 4
	LASTFILE = 3
	LASTFILE = 2
	LASTFILE = 6
	IF(LASTFILE.GT.2.AND.ITERM.LT.0) ITERM = -2
	IFIG = 3
C	IFIG = 0
C
C	DETERMINE SCALING, I.E. MAXIMA
C
	DO N = 1,10
	    WRXMAX(N) = 0.
	    WIXMAX(N) = 0.
	    WRYMAX(N) = 0.
	    WIYMAX(N) = 0.
	ENDDO
C
	DO NFILE = 1,LASTFILE
C       
	  LASTM = MOD(NFILE-1,2) 
C
	  IF(NFILE.GT.1) CLOSE(UNIT=8)
C
	    IF(IFIG.EQ.0) THEN			! ORDINARY RUN
	      IF(NFILE.EQ.1) 
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;0',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.2)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-1',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.3)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-2',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.4)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-3',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.5)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-4',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.6)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-5',TYPE='OLD',READONLY)
	    ENDIF
C
	    IF(IFIG.EQ.1) THEN				! FIG 0 FOR NICE
	      IF(NFILE.EQ.1)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;0',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.2)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-1',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.3)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-2',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.4)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-3',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.5)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-4',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.6)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-5',TYPE='OLD',READONLY)
	    ENDIF
C
	    IF(IFIG.EQ.4) THEN
	      IF(NFILE.EQ.1)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;0',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.2)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-1',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.3)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-2',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.4)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-3',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.5)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-4',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.6)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-5',TYPE='OLD',READONLY)
	    ENDIF
C	  ENDIF
C
	  print*,'plot file no.',NFILE
	  write(39,*) 'plot file no.',nfile
	  XMIN = 0.
	  XMAX = 0.
	  YMIN = 0.
	  YMAX = 0.
	  N = 0
	  READ (8,105) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
C	  write(39,*) wr,wi,wcycl,xkx,xkz
C	  write(39,*) nc,kit,mode
C	  PRINT*,'READ IN WR,WI,KIT',WR,WI,KIT
	  READ (8,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
	  WCI = WCYCL/RATIOM(NC)
	  XKMAG = SQRT(XKX**2+XKZ**2)
	  RAD = XKZ*1836.*SQRT(2.*TPERP(2))/WCYCL
	  WRITE(39,*) 'W RLH =',RAD
	  VS2 = 0.
	  TOTBETA = 0.
	  VAOC = WCYCL/SQRT(RATIOM(NC))
C	  PRINT*,'ALFVEN SPEED/C',VAOC
	  SUMMASS = 0.
	  DO I = 1,NC
	    SUMMASS = SUMMASS + DENS(I)*ABS(RATIOM(I))
	    IF(RATIOM(I).GT.0.) THEN
	      VAOC = WCYCL/SQRT(RATIOM(I))
C	      PRINT*,'ALFVEN SPEED/C',VAOC
	      VS2 = VS2 + DENS(I)*(2.*TPERP(I) + TPAR(I))
	      BETAPERP(I) = DENS(I)*TPERP(I)/VAOC**2 
	      BETAPAR(I) = DENS(I)*TPAR(I)/VAOC**2 
	      TOTBETA = TOTBETA + BETAPERP(I) + BETAPAR(I)
C	      PRINT*,'I,BETAPAR,PERP',I,BETAPAR(I),BETAPERP(I)
	    ELSE
	      VS2 = VS2 + DENS(I)*(2.*TPERP(I) + TPAR(I))/(3.*RATIOM(NC))
	      BETAPERP(I) = DENS(I)*TPERP(I)/RATIOM(NC)/VAOC**2 
	      BETAPAR(I) = DENS(I)*TPAR(I)/RATIOM(NC)/VAOC**2 
	      TOTBETA = TOTBETA + BETAPERP(I) + BETAPAR(I)
C	      PRINT*,'I,BETAPAR,PERP',I,BETAPAR(I),BETAPERP(I)
	    ENDIF
	  ENDDO
C
	  VAOC = WCYCL/SQRT(SUMMASS)
	  WRITE(39,*)'SUM ALFVEN SPEED/C',VAOC
	  VSOC = SQRT(VS2)
C	  PRINT*,'SOUND SPEED/C',VSOC
	  WRITE(39,*) 'SOUND SPEED/C',VSOC
C	  PRINT*,'TOTAL BETA',TOTBETA
	  WRITE(39,*) 'TOTAL BETA',TOTBETA
C	  PRINT*,'MODE=',MODE
C
C	ENTRY FOR MODE = 3,4 OR 5, CIRCLES AT CONSTANT /k/
C
C	  READ IN THE DATA FROM A FILE FOR008.DAT
C
  10	  READ (8,361,END=20,ERR=20) WR,WI,DML,DTR,DTI,XKX,XKZ
C  	  print 361, WR,WI,DML,DTR,DTI,XKX,XKZ
	  N = N+1
	  NPLT = N
C	  THE FOLLOWING SET IS FOR RADIUS PROP. TO. WR, DIRECTION OF K
C	  I.E.
	  XK = SQRT(XKX**2 + XKZ**2)
	  PLT(N,5) = WR*XKX/XK**2/VAOC
	  WRXMAX(NFILE) = AMAX1(WRXMAX(NFILE),PLT(N,5))
	  PLT(N,6) = WR*XKZ/XK**2/VAOC
	  WRYMAX(NFILE) = AMAX1(WRYMAX(NFILE),PLT(N,6))
C	  THE FOLLOWING SET IS FOR RADIUS PROP. TO. WI, DIRECTION OF K
C	  PLT(N,7) = -WI*XKX/XK/WR
	  PLT(N,7) = -WI*XKX/XK**2/VAOC
	  WIXMAX(NFILE) = AMAX1(WIXMAX(NFILE),PLT(N,7))
C	  PLT(N,8) = -WI*XKZ/XK/WR
	  PLT(N,8) = -WI*XKZ/XK**2/VAOC
	  WIYMAX(NFILE) = AMAX1(WIYMAX(NFILE),PLT(N,8))
C         WRXMAXSV = AMAX1(WRXMAXSV,WRXMAX)
C          WRYMAXSV = AMAX1(WRYMAXSV,WRYMAX)
C          WIXMAXSV = AMAX1(WIXMAXSV,WIXMAX)
C          WIYMAXSV = AMAX1(WIYMAXSV,WIYMAX)
  	  GO TO 10
C
  20	  CONTINUE	
	ENDDO
C
	IPLOT=0
	CALL PLOT2(NLINES,IPLOT,NFILE,LASTFILE,ITERM)
C
C	READ AGAIN, FOR PLOT
C
C 	  PRINT*,'AT 20,N,WR,WI,K',N,WR,WI,XKX,XKZ
C	  PRINT*,'MAXS RX,RY,IX,IY',WRXMAX,WRYMAX,WIXMAX,WIYMAX
C
	DO NFILE=1,LASTFILE
C
	  CLOSE(UNIT=8)
	    IF(IFIG.EQ.0) THEN
	      IF(NFILE.EQ.1)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;0',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.2)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-1',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.3)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-2',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.4)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-3',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.5)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-4',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.6)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-5',TYPE='OLD',READONLY)
	    ENDIF
C
	    IF(IFIG.EQ.3) THEN
	      IF(NFILE.EQ.1)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;0',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.2)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-1',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.3)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-2',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.4)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-3',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.5)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-4',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.6)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-5',TYPE='OLD',READONLY)
	    ENDIF
C
	    IF(IFIG.EQ.4) THEN
	      IF(NFILE.EQ.1)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;0',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.2)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-1',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.3)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-2',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.4)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-3',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.5)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-4',TYPE='OLD',READONLY)
	      IF(NFILE.EQ.6)
     1	      OPEN(UNIT=8,FILE='FOR008.DAT;-5',TYPE='OLD',READONLY)
	    ENDIF
C	  ENDIF
C
	  write(39,*) 'read to plot file no.',NFILE
	  READ (8,105) WR,WI,WCYCL,XKX,XKZ,RW,RKX,RKZ,NC,KIT,MODE
	  write(39,*) wr,wi,wcycl,xkx,xkz
	  write(39,*) '2nd head',nc,kit,mode
C	  PRINT*,'READ IN WR,WI,KIT',WR,WI,KIT
	  READ (8,101)(RATIOM(I),DENS(I),BETAZ(I),TPAR(I),TPERP(I),I=1,NC)
	  WCI = WCYCL/RATIOM(NC)
	  N = 0
  30	  READ (8,361,END=40,ERR=40) WR,WI,DML,DTR,DTI,XKX,XKZ
C  	  print 361, WR,WI,DML,DTR,DTI,XKX,XKZ
	  N = N+1
C	
	  XK = SQRT(XKX**2 + XKZ**2)
	  PLT(N,5) = WR*XKX/XK**2/VAOC
	  PLT(N,6) = WR*XKZ/XK**2/VAOC
	  PLT(N,7) = -WI*XKX/XK**2/VAOC
	  PLT(N,8) = -WI*XKZ/XK**2/VAOC
	  NPLT = N
	  TEMP = ABS(YMIN)
	  NTEMP = TEMP
	  TEMP = NTEMP+1
	  GO TO 30
 40	  CONTINUE	
C	  PRINT*,'AT 40,NPLT,WR,WI,K',NPLT,WR,WI,XKX,XKZ
c	  XMAX = AMAX1(ABS(YMAX),ABS(YMIN),ABS(XMAX))
C	  YMIN = -YMAX
c	  YMIN = 0.
c	  DELX = .1E-6
c	  xmin = 0.
c	  xmax = .9e-6
c	  ymax = xmax
c	  xmax = 1.143*xmax
c	  FILFAC = 1.
c	  DELY = DELX
C	  PRINT*,(XX(I),YY(I),I=1,NPLT)
C	  PRINT*,'TO CHECK FILL IN',N,NPLT
C	  PRINT*,'TO CHECK FILL IN',N,plt(nplt,5),plt(nplt,6),plt(nplt,7)
	  IPLOT=1
	write(39,*) ' '
	write(39,*) 'call plot,iplot,nfile=',iplot,nfile
	  CALL PLOT2(NPLT,IPLOT,NFILE,LASTFILE,ITERM)
	ENDDO
	IPLOT=2
	NFILE = MIN0(NFILE,LASTFILE)
	CALL PLOT2(NPLT,IPLOT,NFILE,LASTFILE,ITERM)
 105	FORMAT(8E12.5,3I3)
 106	FORMAT(5F10.5,3F5.3,3I2)
 101	FORMAT(5E12.4)
  361 FORMAT (1X,E12.6 ',' E10.3,1X,F8.3,F8.3,',',F7.3,E12.4 ',' E11.4)
C
	STOP
	END
	SUBROUTINE PLOT2(NLINES,IPLOT,NFILE,LASTFILE,ITERM)
C
C	THIS PLOTS LASTFILE BOXES.  BUT THE BOXEX ARE NOT NUMBERED BY
C	NFILE.  REAL PARTS FOR NFILE = 1 AND 2 GO IN BOX 2, IMAG IN BOX 1
C	REAL PARTS FOR NFILE = 4 AND 4 GO IN BOX 4, IMAG IN BOX 3,ETC.
C 
	COMMON /PPAK/ XMIN,XMAX,DELX,YMIN,YMAX,DELY,IFIG,RAD
	COMMON /PLTBLK/ PLT(750,10)
	COMMON /PLTR/ WRXMIN(10),WRXMAX(10),WRYMIN(10),WRYMAX(10)
	COMMON /PLTI/ WIXMIN(10),WIXMAX(10),WIYMIN(10),WIYMAX(10)
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PXDEF,PYDEFF,COFFMGO,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PIMGO,USERVAR(10),AUTODOT
C
	CHARACTER*1  JUNK
	CHARACTER*10 RLABEL
	REAL*4 XX(750),YY(750),GX1USE(10),GX2USE(10),GY1USE(10),GY2USE(10)
	DATA RATIO /1.624/
C
	write(39,*) 'plot2 called',iplot,nfile,lastfile,iterm,nlines
	IF(IPLOT.EQ.0) THEN
C
C	  SET UP MONGO
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	  IF(ITERM.GT.0) THEN
	    IF(LASTFILE.LE.2) THEN
		RATIO = 160./98.5	   ! RATIO X TO Y WITH 2 WINDOWS
C		for terminal, order seems to be x1,y1,x2,y2
	        CALL MGOSETLOC(100.,100.,900.,700.)
	    ELSE
		RATIO = 160./98.5	   ! RATIO X TO Y WITH 2 WINDOWS
	        CALL MGOSETLOC(100.,150.,900.,600.)
	    ENDIF
	  ELSE
	    RATIO = 1.
	    IF(LASTFILE.LE.2) RATIO = 160./98.5	   ! RATIO X TO Y WITH 2 WINDOWS
	  ENDIF
	  NWX = LASTFILE/2
C
C	GX AND GY ARE INDEXED BY BOX NUMBER, WHICH ARE THE SAME AS 
C	MGOWINDOWS NUMBERS, IMAG IN BOX 1,2,3,REAL IN BOX 1+NWX,2+NWX, ETC.
C
	write(39,*) 'page,gx1,gx2',gx1,gx2,gy1,gy2
	  LASTBOX = LASTFILE
	  DELGX = (GX2-GX1)/NWX
	  DELGY = (GY2-GY1)/2.
	  XSPACE = .03*(GX2-GX1)
	  YSPACE = .03*(GY2-GY1)
	  DO N = 1,LASTBOX+1
	    NWX = LASTBOX/2
	    NBOXI = (N+1)/2		!  JPLOT = 1,2,3,ETC.
	    NBOXM = MOD(N-1,NWX)
	    NFILEY = (N-1)/NWX      !  NFILEM = 0 OR 1, 0 = SETUP
	    GX1USE(N) = GX1 + (NBOXM)*DELGX + XSPACE	    	    
	    GX2USE(N) = GX1 + (NBOXM+1)*DELGX - XSPACE	    
	    GY1USE(N) = GY1 + (NFILEY)*DELGY + YSPACE	    
	    GY2USE(N) = GY1 + (NFILEY+1)*DELGY - YSPACE	    
	  ENDDO
	write(39,*) 'check g'
	write(39,*) gx1use
	write(39,*) gx2use
	write(39,*) gy1use
	write(39,*) gy2use
C
	  DO N = 1,NWX
	write(39,*) 'true maxs'
	write(39,*) n,wrxmax(n),wrymax(n),wixmax(n),wiymax(n)
	n1 = n+nwx
	write(39,*) n1,wrxmax(n1),wrymax(n+nwx),wixmax(n+nwx),wiymax(n+nwx)
	  ENDDO
C
	  RETURN
	ENDIF
C
C	ENTRY FOR STORING PLOTS
C
	IF(IPLOT.EQ.1) THEN
	  NWX = LASTFILE/2
	  NBOXI = (NFILE+1)/2		!  NBOXI = 1,2,3,ETC.
	  NBOXR = NBOXI + NWX
	  NFILEM = MOD((NFILE-1),2)       !  NFILEM = 0 OR 1, 0 = SETUP
C 
	  write(39,*) 'nfile,jplot,nfilem',NFILE,NBOXI,nfilem
	  IF(NFILEM.EQ.0) THEN
	    WRXMAXSV = AMAX1(WRXMAX(NFILE),WRXMAX(NFILE+1))
	    WRYMAXSV = AMAX1(WRYMAX(NFILE),WRYMAX(NFILE+1))
	    XMAX = AMAX1(RATIO*WRYMAXSV,WRXMAXSV)
	    XMAX = 1.1*XMAX
	    YMAX = XMAX/RATIO
	    AXLR = YMAX	
	    NBX=NBOXR
c	    write(39,*) 'nfile,nboxr,nbox,nbx',nfile,nboxr,nboxi,nbx
	    CALL MGOSETLOC(GX1USE(NBX),GY1USE(NBX),GX2USE(NBX),GY2USE(NBX))
c	    write(39,*) 'gx1,gx2,',gx1,gx2,gy1,gy2
	    CALL MGOSETLIM(0., 0., AXLR*RATIO,AXLR)
	    CALL MGOBOX(1,2)
C	    CALL MGOXLABEL(19,'\Gw\dr/kV\da sin\gq')
	    EXPSV = EXPAND
	    CALL MGOSETEXPAND(.8*EXPSV)
	    IF(NBOXI.EQ.1) CALL MGOYLABEL(19,'\Gw\dr/kV\da cos\gq')
	    EXPAND = EXPSV
	    CALL MGOGRELOCATE(.20*GX1+.80*GX2,.25*GY1+.75*GY2)
	    IF(IFIG.EQ.3) CALL MGOPUTLABEL(6,'S>V\DA',5)
	    IF(IFIG.EQ.4) CALL MGOPUTLABEL(6,'V\DA>S',5)
C
	    WIXMAXSV = AMAX1(WIXMAX(NFILE),WIXMAX(NFILE+1))
	    WIYMAXSV = AMAX1(WIYMAX(NFILE),WIYMAX(NFILE+1))
	    XMAX = AMAX1(RATIO*WIYMAXSV,WIXMAXSV)
	    XMAX = 1.1*XMAX
	    YMAX = XMAX/RATIO
	    AXLI = YMAX
	    NBX = NBOXI
c	    write(39,*) 'nfile,nboxr,nboxi,nbx',nfile,nboxr,nboxi,nbx
	    CALL MGOSETLOC(GX1USE(NBX),GY1USE(NBX),GX2USE(NBX),GY2USE(NBX))
c	    write(39,*) 'gx1,gx2,',gx1,gx2,gy1,gy2
	    CALL MGOSETLIM(0., 0., AXLI*RATIO,AXLI)
	    CALL MGOBOX(1,2)
	    EXPSV = EXPAND
	    CALL MGOSETEXPAND(.8*EXPSV)
	    CALL MGOXLABEL(20,'-\Gw\di/kV\da sin\gq')
	    IF(NBOXI.EQ.1) CALL MGOYLABEL(20,'-\Gw\di/kV\da cos\gq')
	    CALL MGOSETEXPAND(EXPSV)
	    CALL MGOGRELOCATE(.20*GX1+.80*GX2,.25*GY1+.75*GY2)
	    EXPSV = EXPAND
	    CALL MGOSETEXPAND(.8*EXPSV)
	WRITE(39,*) 'PUTLABEL KRLH'
	    CALL MGOPUTLABEL(9,'kR\DL\DH=',5)
	    WRITE(RLABEL,123) RAD
 123	    FORMAT(F7.3)
	write(39,*) '123'
	    CALL MGOLABEL(7,RLABEL)
	    EXPAND = EXPSV
C
	  ENDIF
c	if(1) return
C
C	END OF SET UP WINDOWS
C
	  NBOXI = (NFILE+1)/2		!  NBOXI = 1,2,3,ETC.
	  NBOXR = NBOXI + NWX
	  NFILEM = MOD((NFILE-1),2)       !  NFILEM = 0 OR 1, 0 = SETUP
	  NBX=NBOXR
	  CALL MGOSETLOC(GX1USE(NBX),GY1USE(NBX),GX2USE(NBX),GY2USE(NBX))
	  CALL MGOSETLIM(0., 0., AXLR*RATIO,AXLR)
	  DO NN = 1,NLINES
	    XX(NN) = PLT(NN,5)
	    YY(NN) = PLT(NN,6)
	  ENDDO     
	  IF(NFILE.EQ.2) CALL MGOSETLWEIGHT(2)
	  CALL MGOCONNECT(XX,YY,NLINES)
	  CALL MGOSETLWEIGHT(1)
C
	  NBX=NBOXI
	  CALL MGOSETLOC(GX1USE(NBX),GY1USE(NBX),GX2USE(NBX),GY2USE(NBX))
	  CALL MGOSETLIM(0., 0., AXLI*RATIO,AXLI)
	  IF(NFILE.EQ.2) CALL MGOSETLWEIGHT(2)
	  DO NN = 1,NLINES
	    XX(NN) = PLT(NN,7)
	    YY(NN) = PLT(NN,8)
	  ENDDO
	  CALL MGOCONNECT(XX,YY,NLINES)
	  CALL MGOSETLWEIGHT(1)
	  RETURN
	ENDIF
C
	IF(IPLOT.EQ.2) THEN
	  CALL MGOSETEXPAND(.8)
	  NBX = LASTFILE
	  CALL MGOSETLOC(GX1USE(NBX),GY1USE(NBX),GX2USE(NBX),GY2USE(NBX))
	  CALL MGOPLOTID('[.waves]','oscpub2')
	ENDIF
C
	RETURN
	END