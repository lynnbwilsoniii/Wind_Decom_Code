	SUBROUTINE VIEW(CH,X0,Y0,Z0,ALPHA,BETA,GAMMA,EXPANS)
C
C	MAKE A TOP VIEW AND A SIDE VIEW OF THE SHOCK
C
	COMMON /MODEL/ A,B,C 
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
	INTEGER*4 CH,OK,SCETI4(2),RETURN_SIZE
	CHARACTER*32 ITEM
	CHARACTER*32 STR
C
	DIMENSION YY(2048),PP(2048)
	DATA ITERM /-1/
	DATA IDONE /0/
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,300.,2150.,3000.)
	ENDIF
C
C	SET UP SIZE OF PLOTS
C
	IF(IDONE.EQ.0) THEN
	  YZ0 = AMAX1(ABS(Y0),ABS(Z0))
	  IF(X0.GT.0.) THEN
	    XULIM = AMAX1(1.1*X0,15.)
	    XLLIM = -10.
	    YLIM = .343*(XULIM - XLLIM)
	    IF(ABS(YZ0).GT.YLIM) THEN
		YLIM = 1.1*ABS(YZ0)
		XULIM = 1.46*YLIM
		XLLIM = -1.46*YLIM
	    ENDIF
	  ELSE
	    XULIM = 15.
 	    XLLIM = 1.1*X0
	    YLIM = 1.1*ABS(YZ0)
	    IF(YLIM.GT.ABS(XULIM-XLLIM)) THEN
		XULIM = 1.46*YLIM
		XLLIM = -1.46*YLIM
	    ENDIF
	  ENDIF
	  XRANGE = XULIM - XLLIM
	  EFFY = 2.92*YLIM			! MULT BY SIDES RATIO
	  IF(XRANGE.GT.EFFY) THEN
	    YLIM = .343*XRANGE
	  ELSE
	    XTRA = .5*(EFFY-XRANGE)
	    XULIM = XULIM + XTRA
	    XLLIM = XLLIM - XTRA
	  ENDIF
	  CALL MGOSETLIM(XLLIM,-YLIM,XULIM,YLIM)
	  IDONE = 1
	ENDIF
C
C	END OF CALCULATION OF LENGTHS OF AXES
C
C
C	CALCULATE INTERSECTION OF FIELD LINE WITH SHOCK, IF THERE IS ONE.
C		QA,QB AND QC ARE USUAL QUADRATIC COEFFICIENTS
C
	QA = GAMMA**2/A + BETA**2/B
	QB = 2.*GAMMA*Z0/A + 2.*BETA*Y0/B - ALPHA
	QC = Z0**2/A + Y0**2/B + C - X0
	DISC = QB**2 - 4.*QA*QC
	PRINT*,'IN VIEW DISC=',DISC
	IF(DISC.GE.0.) THEN
	   T1 = .5*(-QB + SQRT(DISC))/QA
	   T2 = .5*(-QB - SQRT(DISC))/QA
	   T = T1
	   IF(ABS(T2).LT.ABS(T1)) T = T2
	   XI = X0 + ALPHA*T		! NEAREST INTERSECTION
	   YI = Y0 + BETA*T
	   ZI = Z0 + GAMMA*T
	ENDIF
	 PRINT*,'IN VIEW, XI,T=',XI,YI,ZI,T
C
	CALL MGOWINDOW( 1,2,1)
C
C	PUT ON A CIRCLE FOR EARTH
C
	CALL MGORELOCATE(0.,0)
	EXPSAVE = EXPAND
	CALL MGOSETEXPAND(2.)
	CALL MGOPOINT(10,3)
	CALL MGOSETEXPAND(EXPSAVE)
C
C	PUT A SQUARE AT SPACECRAFT POSITION
C
	CALL MGORELOCATE(X0,Y0)
	CALL MGOPOINT(4,0)
C
C	DRAW A LINE FOR MAGNETIC FIELD  (A) TO INTERSECTION WITH SHOCK
C		IF DISC IS POSITIVE, (B) TO EDGE OF GRAPH OTHERWISE
C
	IF(DISC.LE.0.) THEN           ! NO INTERSECTION
	  XP = XLLIM
	  YP = 0.
	  IF(ALPHA.NE.0.) YP = XP*BETA/ALPHA
	  IF(ABS(YP).GT.YLIM) THEN
	    YP = SIGN(YLIM,YP)
	    XP = X0
	    IF(BETA.NE.0.) XP = YP*ALPHA/BETA
	  ENDIF
	  CALL MGORELOCATE(X0,Y0)
	  CALL MGODRAW(XP,YP)
	  print*,'no intsect',xp,yp
	ELSE				! DRAW LINE TO INTERSECTION
	  CALL MGORELOCATE(X0,Y0)
	  CALL MGODRAW(XI,YI)
	  print*,' intsect',xi,yi
	ENDIF
C
C	DRAW AVERAGE SHOCK
C
	CALL MGORELOCATE(C,0.)
	YP = 0.
	DY = YLIM/25.
	DOWHILE (YP.LT.YLIM)
	  YP = YP + DY
	  XP = C + (YP**2)/A
	  CALL MGODRAW(XP,YP) 
	ENDDO 
	CALL MGORELOCATE(C,0.)
	YP = 0.
	DOWHILE (YP.GT.-YLIM)
	  YP = YP - DY
	  XP = C + (YP**2)/A
	  CALL MGODRAW(XP,YP) 
	ENDDO 
C	CALL MGOSETEXPAND(.8)
C	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOBOX(1,2)
	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(17,'X GSE earth radii')
	CALL MGOYLABEL(17,'Y GSE earth radii')
C	CALL MGOSETEXPAND(.8)
C
	CALL MGOWINDOW( 1,2,2)
C
C	PUT ON A CIRCLE FOR EARTH
C
	CALL MGORELOCATE(0.,0)
	CALL MGOPOINT(10,3)
C
C	PUT A SQUARE AT SPACECRAFT POSITION
C
	CALL MGORELOCATE(X0,Z0)
	CALL MGOPOINT(4,0)
C
C	DRAW A LINE FOR MAGNETIC FIELD
C
	IF(DISC.LE.0.) THEN 
	  XP = XLLIM
	  YP = 0.
	  IF(ALPHA.NE.0.) YP = XP*GAMMA/ALPHA
	  IF(ABS(YP).GT.YLIM) THEN
	    YP = SIGN(YLIM,YP)
	    XP = X0
	    IF(GAMMA.NE.0.) XP = YP*ALPHA/GAMMA  
	  ENDIF
	  CALL MGORELOCATE(X0,Z0)
	  CALL MGODRAW(XP,YP)
	ELSE
	  CALL MGORELOCATE(X0,Z0)
	  CALL MGODRAW(XI,ZI)
	ENDIF
C
C	DRAW AVERAGE SHOCK
C
	CALL MGORELOCATE(C,0.)
	YP = 0.
	DY = YLIM/25.
	DOWHILE (YP.LT.YLIM)
	  YP = YP + DY
	  XP = C + (YP**2)/B
	  CALL MGODRAW(XP,YP) 
	ENDDO 
	CALL MGORELOCATE(C,0.)
	YP = 0.
	DOWHILE (YP.GT.-YLIM)
	  YP = YP - DY
	  XP = C + (YP**2)/B
	  CALL MGODRAW(XP,YP) 
	ENDDO 
C	CALL MGOSETEXPAND(.8)
C	CALL MGOTICKSIZE(0.,0.,0.,0.)  
	CALL MGOBOX(1,2)
C	CALL MGOSETEXPAND(.7)
	CALL MGOXLABEL(17,'X GSE earth radii')
	CALL MGOYLABEL(17,'Z GSE earth radii')
C
	ITEM = 'EVENT_SCET'
	ok = w_item_i4(ch, item, SCETI4, 2, return_size)
	TOPLABEL = GY2 + .02*(GY2-GY1)
	CALL MGOGRELOCATE(GX1,TOPLABEL)
	WRITE(STR,1001) SCETI4(1),SCETI4(2)
 1001	FORMAT(2I9)
	CALL MGOLABEL(18,STR)
C	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]SHOCKDIFF,VIEW')
	CALL MGOSETEXPAND(1.)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END

