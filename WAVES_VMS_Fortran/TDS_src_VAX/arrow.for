	SUBROUTINE ARROW(X,Y,DX,DY,SIZE)
C
C	makes an arrowhead, near x,y in user coords, size as fraction
c	of graph size, direction dx,dy  from  x,y
C	MONGO MUST BE OPENED AND WORKING
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
C	CALCULATE DXP,DYP PERPENDICULAR TO DX,DY
C		DOESNT WORK WELL IF GRAPH IS NOT SQUARE?
C
	WDX = ABS(X2-X1)	! WIDTH OF GRAPH IN USER COORDS
	WDY = ABS(Y2-Y1)
C	PRINT*,'ARROW,X1',X1,X2,Y1,Y2
C	PRINT*,'ARROW,X,DX',X,Y,DX,DY
	WRITE(76,*)'ARROW,X1,X2,Y1,Y2',X1,X2,Y1,Y2
	DXN = SQRT(DX**2 + DY**2)
	DXL = DX*SIZE*WDX/DXN
	DYL = DY*SIZE*WDY/DXN
C	PRINT*,'ARROW,DXN,DXL,DYL',DXN,DXL,DYL
	WRITE(76,*)'ARROW,DXN,DXL,DYL',DXN,DXL,DYL
	write(76,*) 'arrow,DX,DY,SIZE',dx,dy,size
	DXP = .2*DYL		! PERPENDICULAR TO LINE
	DYP = -.2*DXL
C
	CALL MGORELOCATE(X+.5*DXL,Y+.5*DYL)
	CALL MGODRAW(X-.5*DXL+DXP,Y-.5*DYL+DYP)
	CALL MGODRAW(X,Y)
C	CALL MGORELOCATE(X+.5*DXL,Y+.5*DYL)
C	CALL MGODRAW(X-.5*DXL+.5*DXP,Y-.5*DYL+.5*DYP)
C	CALL MGODRAW(X,Y)
	CALL MGORELOCATE(X+.5*DXL,Y+.5*DYL)
	CALL MGODRAW(X-.5*DXL-DXP,Y-.5*DYL-DYP)
	CALL MGODRAW(X,Y)
C	CALL MGORELOCATE(X+.5*DXL,Y+.5*DYL)
C	CALL MGODRAW(X-.5*DXL-.5*DXP,Y-.5*DYL-.5*DYP)
C
	RETURN
	END
