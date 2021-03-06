	PROGRAM SHOCKSPACE
C
C	MAKES A PICTURE OF THE EARTH'S BOWSHOCK, THE SHOCK CROSSING POINT,
C	ORBIT, MAGNETIC FIELD, ETC.
C	i decided that my first approach was all wrong, and saved the 
C	version done up to 14 Sep 2005 as SHOCKSPACE.OLD
C
	COMMON /PLOTLIMITS/ XMLIM,XPLIM,YPLIM
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	INTEGER YYYYMMDD,HHMMSS,YYYY,DD,YMD
	integer*4 ch,ok,OK1,OK2,SCETI4(2)	
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_CHANNEL_OPEN,W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	REAL*8 SCET8,RGSE,SCET,XGSE,YGSE,ZGSE
	REAL B(3),VSW(3),ORBIT(3),SHKNORMAL(3)
	REAL XX(500),YY(500)
	character*20 str
	character*32 ITEM
	character*4 event
	character*80 stream
	CHARACTER*1 DISPOSE
	DATA RGSE /6378.D00/
	DATA RNOSE0,RFLANK0 /14.6,25.6/
C
	ITERM = 3
C
	write(6,*) 'type shock time,YYYYMMDD,HHMMSS e.g. 20010315,080000'  
	read(5,*) YYYYMMDD,HHMMSS
	type*,YYYYMMDD,HHMMSS  
	tstart = (HHMMSS/10000) + mod(HHMMSS,10000)/60. + mod(HHMMSS,100)
	print*,'tstart,days ',tstart
	YYYY = YYYYMMDD/10000
	MON = MOD(YYYYMMDD,10000)/100
	DD = MOD(YYYYMMDD,100) 
	HH = HHMMSS/10000
	MM = MOD(HHMMSS,10000)/60
	SS = MOD(HHMMSS,100)  
	ms = 0
	print*,'yr',yyyy,mon,dd
	print*,'time ',hh,mm,ss      
C	OPEN(UNIT=90,FILE='MAKEFILE5.RESULTS',STATUS='OLD',ACCESS='APPEND')
 100	continue
c	call w_ur8_from_ymd(scet8,yyyy,mon,dd,hh,mm,ss,ms)
	WRITE(stream,30) YYYYMMDD
 30	FORMAT('wi_lz_wav_',I8.8,'_v*.dat')
	PRINT*,'in get_stream_name, file= ',STREAM
C
C	call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
C
	ok = w_channel_open(ichannel,stream)
C
C	print*,'open, TMCHANNEL,ok=',ICHANNEL,ok
	if (ok.ne.1) stop 'Cannot open t/m channel'
c
	scet = 0.
	call w_channel_position(ichannel,scet)
	print*,'t/m file starts at scet',scet
	idds = scet
	FDAY = HH/24.E00 + MM/1440.D00 + SS/86400.E00
	SCET8 = IDDS + FDAY
	call w_channel_position(ichannel,scet8)
	print*,'t/m file position set to ',scet8
C
	OK = W_EVENT(ICHANNEL,'CDF') 
	item = 'WIND_ORBIT_X(GSE)_R8'
	ok = w_item_r8(ICHANNEL, item, XGSE, 1, return_size)
	orbit(1) = xgse/rgse
	item = 'WIND_ORBIT_Y(GSE)_R8'
	ok = w_item_r8(ICHANNEL, item, YGSE, 1, return_size)
	orbit(2) = ygse/rgse
	item = 'WIND_ORBIT_Z(GSE)_R8'
	ok = w_item_r8(ICHANNEL, item, ZGSE, 1, return_size)
	orbit(3) = zgse/rgse
C
	XPLIM = 35.
	XMLIM = 0.
	IF(ORBIT(1).LT.5.) XMLIM = ORBIT(1)-5.
	XPRANGE = XPLIM-XMLIM
	YPLIM = .888*XPRANGE

C
C			CALCULATE SCALE
C
	SCALE = .5*(ORBIT(1)/RNOSE0 + SQRT((ORBIT(1)/RNOSE0)**2
     1	     + 4.*(ORBIT(2)**2+ORBIT(3)**2)/RFLANK0**2))
	RNOSE = RNOSE0*SCALE
	RFLANK = RFLANK0*SCALE
C
C	DETERMINE SHOCK NORMAL
C
	SHKNORMAL(1) = 1./RNOSE
	SHKNORMAL(2) = 2.*ORBIT(2)/RFLANK**2 
	SHKNORMAL(3) = 2.*ORBIT(3)/RFLANK**2
	SMAG = SQRT(SHKNORMAL(1)**2+SHKNORMAL(2)**2+SHKNORMAL(3)**2)
	SHKNORMAL(1) = SHKNORMAL(1)/SMAG
	SHKNORMAL(2) = SHKNORMAL(2)/SMAG
	SHKNORMAL(3) = SHKNORMAL(3)/SMAG
C
C	DETERMINE INOUT
C
	item = 'WIND_ORBIT_VX(GSE)_R8'
	ok = w_item_r8(ICHANNEL, item, VXGSE, 1, isize)
	item = 'WIND_ORBIT_VY(GSE)_R8'
	ok = w_item_r8(ICHANNEL, item, VYGSE, 1, isize)
	item = 'WIND_ORBIT_VZ(GSE)_R8'
	ok = w_item_r8(ICHANNEL, item, VZGSE, 1, isize)
	VNORM = VXGSE*SHKNORMAL(1)+VYGSE*SHKNORMAL(2)+VZGSE*SHKNORMAL(3)
C	0 IS OUT, 1 IS IN
	INOUT = 0
	IF(VNORM.LT.0.) INOUT = 1
	WRITE(67,*) 'NORMAL S/C VEL',VNORM,INOUT
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	DO NWDW = 1,2
	  IF(ITERM.LT.0) CALL MGOWINDOW(1,2,NWDW)
	  IF(ITERM.GE.0) CALL MGOWINDOW(2,1,NWDW)
C
	  WRITE(STR,1001) YYYY,MON,DD
	  CALL MGOSETLIM(XMLIM,-YPLIM,XPLIM,YPLIM)
	  CALL MGOBOX(1,2)
	  CALL MGOXLABEL(11,'X(GSE),R\DE')
	  IF(NWDW.EQ.1) THEN
	    CALL MGOYLABEL(11,'Y(GSE),R\DE')
	    CALL MGORELOCATE(.4*XPRANGE,.9*YPLIM)
	    CALL MGOLABEL(11,STR)
	    CALL MGORELOCATE(.5*XPRANGE,.8*YPLIM)
	    CALL MGOLABEL(8,'TOP VIEW')
	  ELSE 	
	    CALL MGOYLABEL(11,'Z(GSE),R\DE')
	    CALL MGORELOCATE(.5*XPRANGE,.8*YPLIM)
	    CALL MGOLABEL(9,'SIDE VIEW')
	  ENDIF
 1001	  FORMAT(I5.4,'/',I2.2,'/',I2.2)
c	  write(67,*) 'lims ',nwdw,xmlim,xplim,yplim
C
C		PLOT SHOCK
C
	  NP=1
	  YY(NP) = 0.
	  XX(NP) = RNOSE
	  DO WHILE (XX(NP).GT.XMLIM)
	    NP = NP+1
	    YY(NP) = YY(NP-1) - 1.
	    XX(NP) = RNOSE*(1. - (YY(NP)/RFLANK)**2)
	  ENDDO
	  write(67,*) 'shock points ',np
	  CALL MGOCONNECT(XX,YY,NP)
	  NP=1
	  YY(NP) = 0.
	  XX(NP) = RNOSE
	  DO WHILE (XX(NP).GT.XMLIM)
	    NP = NP+1
	    YY(NP) = YY(NP-1) + 1.
	    XX(NP) = RNOSE*(1. - (YY(NP)/RFLANK)**2)
	  ENDDO
	  write(67,*) 'nwdw,shock points ',nwdw,np
	  CALL MGOCONNECT(XX,YY,NP)
	ENDDO
C
	OK = W_MESSAGES_OFF(ICHANNEL)
C
	CALL PICTURE(ICHANNEL,SCET8,B,VSW,ORBIT,SCALE,INOUT)
C
C
	CALL MGOSETEXPAND(1.)
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  READ(5,1023) DISPOSE
 1023	  FORMAT(A1)
	  CALL MGOTCLOSE
	ENDIF
C
	STOP
	END
	SUBROUTINE PICTURE(ICH,SCET8,B,VSW,ORBIT,SCALE,INOUT)
C
	COMMON /PLOTLIMITS/ XMLIM,XPLIM,YPLIM
C	REAL B(1),VSW(1),ORBIT(3),XRE(200),YRE(200),ZRE(200)
	REAL B(1),VSW(1),ORBIT(3)
	REAL*8 SCET8,SCETT,XGSE,YGSE,ZGSE,RGSE
	INTEGER OK,OK1,OK2,W_EVENT,W_ITER_R4,W_ITEM_R8,SCETI4(2)
	CHARACTER*32 ITEM
	CHARACTER*4 EVENT
	DATA RGSE /6378.D00/
	DATA RNOSE0,RFLANK0 /14.6,25.6/
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
	ITERM = 3
	RNOSE = RNOSE0*SCALE
	RFLANK = RFLANK0*SCALE
C
 100	CONTINUE
  	OK = W_EVENT(ICH,'CDF')
C
	IF(OK.NE.1) THEN
	  IF(OK.EQ.82) RETURN
	  NERROR = NERROR+1
	  IF(NERROR.GT.10) STOP 'CANNOT GET NEXT EVENT'
	  GO TO 100
	ELSE
	  NERROR=0
	ENDIF 
C
	item = 'EVENT_SCET'
	ok = w_item_I4(ICH, item, SCETI4, 2, isize)
	ITEM = 'WIND_MFI_BX(GSE)_R4'
	ok = w_item_r4(ICH, item, BX, 1, return_size)
	B(1) = BX
	  ITEM = 'WIND_MFI_BY(GSE)_R4'
	  ok = w_item_r4(ICH, item, BY, 1, return_size)
	  B(2) = BY
	  ITEM = 'WIND_MFI_BZ(GSE)_R4'
	  ok = w_item_r4(ICH, item, BZ, 1, return_size)
	  B(3) = BZ
	  WRITE(67,*) 'TIME,B',SCETI4
C	ENDDO	  
	WRITE(67,*) 'B',B
C
C
	DO NWDW = 1,2
	  IF(ITERM.LT.0) CALL MGOWINDOW(1,2,NWDW)
	  IF(ITERM.GE.0) CALL MGOWINDOW(2,1,NWDW)
C
	  CALL MGOSETLIM(XMLIM,-YPLIM,XPLIM,YPLIM)
C
C		PLOT ORBIT
C
C	  write(67,*) 'weight ',lweight
	  CALL MGOSETLWEIGHT(2)
	  NP = 0
	    item = 'WIND_ORBIT_X(GSE)_R8'
	    ok = w_item_r8(ICH, item, XGSE, 1, isize)
C	    IF(OK.EQ.1) THEN
	      XRE = XGSE/RGSE
	      item = 'WIND_ORBIT_Y(GSE)_R8'
	      ok = w_item_r8(ICH, item, YGSE, 1, isize)
	      YRE = YGSE/RGSE
	      item = 'WIND_ORBIT_Z(GSE)_R8'
	      ok = w_item_r8(ICH, item, ZGSE, 1, isize)
	      ZRE = ZGSE/RGSE
C	    ENDIF
C
	  ITEM = 'WIND_3DP_ION_VX(GSE)_R4'
	  ok = w_item_r4(ICH, item, VSW(1), 1, return_size)
	  ITEM = 'WIND_3DP_ION_VY(GSE)_R4'
	  ok = w_item_r4(ICH, item, VSW(2), 1, return_size)
	  ITEM = 'WIND_3DP_ION_VZ(GSE)_R4'
	  ok = w_item_r4(ICH, item, VSW(3), 1, return_size)
	  ITEM = 'WIND_3DP_ION_DENSITY_R4'
	  ok1 = w_item_r4(ICH, item, DENS3DP, 1, return_size)
	  ITEM = 'WIND_SWE_DENSITY_R4'
	  ok2 = w_item_r4(ICH, item, DENSSWE, 1, return_size)
C	  IF(OK1+OK2.LE.0) DENSITY = 7.
	  IF(DENSSWE.LE.0.) DENSITY = DENS3DP
	  IF(DENS3DP.LE.0.) DENSITY = DENSSWE
	  IF(DENS3DP.GT.0..AND.DENSSWE.GT.0.) DENSITY = .5*(DENSSWE+DENS3DP)
C
C			CALCULATE SCALE
C
	  VLIM = -2000.
	  IF(VSW(1).LT.VLIM.OR.VSW(2).LT.VLIM.OR.VSW(3).LT.VLIM) GO TO 100
	  SCALE = 9.5*((VSW(1)**2+VSW(2)**2+VSW(3)**2)*DENSITY)**(-1./6.)
	  write(67,*) 'scale ',DENS3DP,DENSSWE,VSW(1),scale
C
C		PLOT MAGNETIC FIELD LINES (not done)
C
C		PLOT B TANGENT TO SHOCK (not done)
C
	CALL DIFFFK(XRE,YRE,ZRE,BX,BY,BZ,D,XT,YT,ZT,XI,YI,ZI,IN,SCALE)
	  WRITE(67,*) 'DIFF',D,XI,YI,ZI
C
	IF(D.GT.0..AND.IN.GT.1) THEN
C
C
C	DRAW LINE OF B TO FOOTPRINT
C
	  IF(NWDW.EQ.1) THEN 
	    CALL MGOSETLWEIGHT(2)
	    CALL MGORELOCATE(ORBIT(1),ORBIT(2))
	    CALL MGODRAW(XI,YI)
	    CALL MGOSETLWEIGHT(1)
	    CALL MGORELOCATE(ORBIT(1),ORBIT(2))
	    CALL MGODRAW(XI,YI)
	  ELSE
	    CALL MGOSETLWEIGHT(2)
	    CALL MGORELOCATE(ORBIT(1),ORBIT(3))
	    CALL MGODRAW(XRE,ZRE)
	    CALL MGOSETLWEIGHT(1)
	    CALL MGORELOCATE(ORBIT(1),ORBIT(3))
	    CALL MGODRAW(XI,ZI)
	  ENDIF
	  ORBIT(1) = XRE
	  ORBIT(2) = YRE
	  ORBIT(3) = ZRE
C	DETERMINE SHOCK NORMAL
C
C	SHKNORMAL(1) = 1./RNOSE
C	SHKNORMAL(2) = 2.*ORBIT(2)/RFLANK**2 
C	SHKNORMAL(3) = 2.*ORBIT(3)/RFLANK**2
C	SMAG = SQRT(SHKNORMAL(1)**2+SHKNORMAL(2)**2+SHKNORMAL(3)**2)
C	SHKNORMAL(1) = SHKNORMAL(1)/SMAG
C	SHKNORMAL(2) = SHKNORMAL(2)/SMAG
C	SHKNORMAL(3) = SHKNORMAL(3)/SMAG
C
C	DETERMINE B.N AT FOOTPRINT
	BMAG = SQRT(B(1)**2 + B(2)**2 + B(3)**2)
C	BDOTN =  BUP(1)*SHKNORMAL(1)+BUP(2)*SHKNORMAL(2)+BUP(3)*SHKNORMAL(3)
C	BDOTN = BDOTN/BMAG
C	ABDOTN = ACOSD(ABS(BDOTN))
C	WRITE(67,*) 'B.N',BDOTN,ABDOTN
C
	ENDIF
C
C		PLOT ION FORESHOCK BOUNDARY (not done)
	ENDDO
	GO TO 100
C
C	RETURN
	END
	SUBROUTINE DIFFFK(XN,YN,ZN,BX,BY,BZ,D,XT,YT,ZT,XI,YI,ZI,IN,SCALE)
C
	IMPLICIT REAL*4 (A-H)
	IMPLICIT INTEGER*4 (I-N)
	IMPLICIT REAL*4 (O-Z)
C
C	CALCULATES DIFF TO BOW SHOCK, ALGEBRA IN WIND VOL 4 P 89-90
C		D IS DIFF, XT,YT,ZT ARE COORDINATES OF TANGENT POINT
C		XI,YI,ZI ARE COORDINATES OF INTERSECION WITH SHOCK
C		INPUT XN, ETC ARE IN EARTH RADII, B IN nT
C		IN = 1, INSIDE SHOCK, 2 FORESHOCK, 3 FREE SW
C		positive d means field intersects bow shock
C
	DATA XNOSE0,RS0 /14.6,25.6/
C
	XNOSE = XNOSE0*SCALE
	RS = RS0*SCALE
	IF(BX.EQ.0.) THEN
	  D = -(XN-XNOSE)
	  RETURN
	ENDIF
C
	RS2 = RS**2
	BX2 = BX**2
	BY2 = BY**2
	BZ2 = BZ**2
C	print*,'in diff,X,B',xn,bx,by,bz
C
	a = -(BY2 + BZ2)/BX2/RS2
C	print*,'a=',a
	ZXN = ZN - XN*BZ/BX
	YXN = YN - XN*BY/BX
	b0 = 1./XNOSE +2.*(BZ*ZXN/BX + BY*YXN/BX)/RS2
	b0 = -b0
	b1 = -2.*a
	c0 = 1. - (YXN**2 + ZXN**2)/RS2
	c1 = 2.*(BZ*ZXN/BX + BY*YXN/BX)/RS2
	c2 = a
	BB = 2.*b0*b1 - 4.*a*c1
	CC = b0**2 - 4.*a*c0
	D = 0.
	IF(BB.NE.0.) D = -CC/BB
C
	IF(D.GE.0.) THEN
C
C	  CALCULATE TANGENT POINT IF DIFF IS POSITIVE
C
	  AT = a
	  BT = b0 + b1*d
	  CT = c0 + c1*d + c2*d**2
	  XT = -.5*BT/AT
	  YT = YN + BY*(XT - XN - D)/BX
	  ZT = ZN + BZ*(XT - XN - D)/BX
C
C	  CALCULATE INTERSECTION IF DIFF IS POSITIVE
C	    THERE ARE TWO SOLUTIONS, XI1 AND XI2, OF 
C	    AI X**2 + BI X + CI = 0
C
	  AI = a
	  BI = b0
	  CI = c0
	  XI1 = .5*(-BI + SQRT(BI**2-4.*AI*CI))/AI
	  YI1 = YN + BY*(XI1-XN)/BX
	  ZI1 = ZN + BZ*(XI1-XN)/BX
	  XI2 = .5*(-BI - SQRT(BI**2-4.*AI*CI))/AI
	  YI2 = YN + BY*(XI2-XN)/BX
	  ZI2 = ZN + BZ*(XI2-XN)/BX
	  D1SQ = (XN-XI1)**2 + (YN-YI1)**2 + (ZN-ZI1)**2
	  D2SQ = (XN-XI2)**2 + (YN-YI2)**2 + (ZN-ZI2)**2
	  IF(D1SQ.LT.D2SQ) THEN
	    XI = XI1
	    YI = YI1
	    ZI = ZI1
	  ELSE
	    XI = XI2
	    YI = YI2
	    ZI = ZI2
	  ENDIF
	  IN = 2
	  IF(XN.LE.XI1.AND.XN.GE.XI2) IN = 1
	  IF(XN.LE.XI2.AND.XN.GE.XI1) IN = 1
C
	ELSE
	  XT = 0.
	  YT = 0.
	  ZT = 0.
	  XI = 0.
	  YI = 0.
	  ZI = 0.
	  IN = 3
	ENDIF
C
	RETURN
	END
