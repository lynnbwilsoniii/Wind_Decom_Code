	PROGRAM PLOTMK18
C
C	MAKES PLOTS OF DATA WRITTEN TO FOR056.DAT BY MAKEFILE18, SEARCH FOR
C		LANGMUIR WAVES IN THE EARTH'S TAIL
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	INTEGER*4 SCETI4(2)
	REAL*4 XX(500),YY(500),ZZ(500)
C
	ITERM = -1
	ITERM = -3
c	ITERM = 3
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	CALL MGOSETLWEIGHT(2)
	NP = 0
	NOR = 0
	CALL MGOSETEXPAND(1.)
	XMAX = -100.
	XMIN = 0.
C	YMAX = -70.
	YMAX = -31.
	YMIN = -YMAX
	CALL MGOWINDOW(1, 2, 1)
	CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(5,'X(RE)')
	CALL MGOYLABEL(5,'Y(RE)')
	CALL MGOWINDOW(1, 2, 2)
	CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	CALL MGOBOX(1,2)
	CALL MGOXLABEL(5,'X(RE)')
	CALL MGOYLABEL(5,'Z(RE)')
c	go to 200		! OK at this point
C
C	PLOT ORBITS
C
C	OPEN(UNIT=79,FILE='FOR079.DAT;7',STATUS='OLD',READONLY)
	OPEN(UNIT=79,FILE='ORBIT.TABLE',STATUS='OLD',READONLY)
C	OPEN(UNIT=79,FILE='GSM.TABLE',STATUS='OLD',READONLY)
C
	OPEN(UNIT=56,FILE='MAKEFILE18SAVE.RESULTS',STATUS='OLD',READONLY)
C
 150	CONTINUE
	READ (79,*,END=100,ERR=150) SCET,SCETI4,XRE,YRE,ZRE,RRE
C	write(44,*) nor,scet,sceti4
C	IF(SCET.EQ.0) THEN
	IF(XRE.GT.-10.) THEN
	  CALL MGOWINDOW(1, 2, 1)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(XX,YY,NOR)
c  	CALL MGOSETEXPAND(1.)
	  CALL MGOWINDOW(1, 2, 2)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(XX,ZZ,NOR)
	  NOR = 0
	ELSE
	  NOR = NOR+1
	  XX(NOR) = XRE
	  YY(NOR) = YRE
	  ZZ(NOR) = ZRE
	ENDIF
c	go to 200		! OK at this point
	GO TO 150
C
C	PLOT LANGMUIR AND BERNSTEIN WAVES FOUND
C
 100	CONTINUE
C	write(44,*) '1',x1,x2,y1,y2
	READ (56,*,END=200,ERR=100) NEVT,SCETI4,ISPS,IZCNT,
     1		F1,F2,F3,ZCBW,FRBW,FP_3DP,FP_SWE,XMAX,XRE,YRE,ZRE
	NP = NP+1
	XX(NP) = XRE
	YY(NP) = YRE
	ZZ(NP) = ZRE
	GO TO 100
 200	CONTINUE
C
C	write(44,*) 'GOT TO 200,NP=',NP
	CALL MGOSETEXPAND(1.)
	CALL MGOWINDOW(1, 2, 1)
	CALL MGOPOINTS(80.,1,XX,YY,NP)
	CALL MGOBOX(1,2)
C
c	write(44,*) '3',x1,x2,y1,y2
	CALL MGOWINDOW(1, 2, 2)
	CALL MGOPOINTS(80.,1,XX,ZZ,NP)
	CALL MGOBOX(1,2)
C
C	write(44,*) 'GOT TO PLOTID, NP=',NP
	CALL MGOPLOTID('[.WIND]PLOTMK18','FROM MAKEFILE18')
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
C	  CALL MGOTCLOSE
	ENDIF
	CLOSE(UNIT=79)
	CLOSE(UNIT=56)
C
	STOP
	END	