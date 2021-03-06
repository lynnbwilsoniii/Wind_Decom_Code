	SUBROUTINE PLOTZTABLE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
C
	COMMON /FUDGE/ FFACT	
C
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	CHARACTER*50 COM(20)
	INTEGER  YYYYMMDD(200)
	REAL RX(1),RY(1),FLUX(1),DENS(1),STDRX(1)
	REAL STDRT(1),STDDDENS(1),TE(1)
	REAL ZTAB(22,200)
C
	  ITERM = 3
	  ICOM = 0
C
	OPEN(UNIT=89,FILE='ZTABLE.DAT',STATUS='OLD',READONLY)
	READ(89,1000) JUNK
	print*,junk
	READ(89,1000) JUNK
	print*,junk
 1000 	FORMAT(A)
	IS = 0
C
 100	CONTINUE
C	READ(89,*,END=200,ERR=201) YYYYMMDD,TS,TEND,XDC,XPK,YDC,YPK,SXDC,SXPK,
C     1   	SYDC,SYPK,ANGLE,DENS,SDENS,TE,STE,TI,STI,FLUX,
C     2		RBASE,RX,RY
c
	IS = IS+1
	READ(89,*,END=200,ERR=201) YYYYMMDD(IS),(ZTAB(I,IS),I=2,22)
 201	PRINT*,'ERROR IN SUBROUTINE PLOTZTABLE READIN AT IS=',IS
	GO TO 100
C
 200	CONTINUE	
C
	  ICOM = ICOM+1
	  COM(ICOM) = 'TERMINAL 3'
	  IF(ITERM.LT.0) COM(ICOM) = 'PRINTER 1'
C
C	  WRITE(COM(2),1001) NW
C 1001	  FORMAT('WINDOW 2 2 ',I2)
	  ICOM = ICOM+1
	  COM(ICOM) = 'WINDOW 1 3 3'
	  COM(3) = 'RELOCATE .05 .95'
 	  WRITE(COM(4),1020) ISCET,NUMEVENT,ICH,IWRONG
 1020	  FORMAT('LABEL',I10,I10,I10,'  CH',I4,I6)
	  ICOM = ICOM+1
	  COM(ICOM) = 'xcolumn 19'
	  ICOM = ICOM+1
	  COM(ICOM) = 'ycolumn 21'
	  ICOM = ICOM+1
	  COM(ICOM) = 'limits  0.  30.   0.  500.'
	  ICOM = ICOM+1
	  COM(ICOM) = 'ptype 5 0'
	  ICOM = ICOM+1
	  COM(ICOM) = 'points'
	  ICOM = ICOM+1
	  COM(ICOM) = 'box'
	  ICOM = ICOM+1
	  COM(ICOM) = 'xlabel electron flux'
	  ICOM = ICOM+1
	  COM(ICOM) = 'ylabel RX M\gW'
	  ICOM = ICOM+1
	  COM(ICOM) = 'id [.WIND]PROCZTABLE'
	  IF(ITERM.LT.0) THEN
	    ICOM = ICOM+1
	    COM(ICOM) = 'HARDCOPY'
	  ENDIF

	  CALL MONGO(ICOM,COM,256,2,ZTABLE)

Cdata ztable.dat
Clines 3 2000
Cwindow 1 3 3
Cxcolumn 19
Cycolumn 21
Climits  0.  30.   0.  500.  
C ptype 5 0
C points
Cbox
Cxlabel electron flux
Cylabel RX M\gW
Cid
DATA FOR067.DAT
lines 1 2000
XCOLUMN 2
YCOLUMN 4
CONNECT
DATA FOR067.DAT;-1
lines 1 2000
XCOLUMN 2
YCOLUMN 4
CONNECT
window 1 3 2
data ztable.dat
lines 3 2000
xcolumn 19
ycolumn 22
limits  0.  30.   0.  5000.  
ptype 5 0
points
box
xlabel electron flux
ylabel RY M\gW
window 1 3 1
xcolumn 19
ycolumn 20
limits  0.  30.  0.  500.
ptype 5 0
points
box
xlabel electron flux
ylabel Rbase M\gW
	RETURN
	END

