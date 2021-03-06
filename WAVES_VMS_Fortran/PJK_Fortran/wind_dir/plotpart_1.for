	SUBROUTINE PLOTPART(ITERM)
C
C	PLOTS ONE COMPONENT AGAINST ANOTHER FOR A FRACTION OF AN
C		EVENT TO LOOK FOR POLARIZATION CHANGES
C
	CHARACTER*12 title(20)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*8 LABEL(4)
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS
	COMMON /HEADBL/ TITLE,EVENT,NUMEVENT,FILE
	COMMON /FIXUPBLK/ NBAD3,NBAD1,NBAD2,IFXB
	common /headblk/ major,minor,s_scet,nsys
	COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
	COMMON /PARTBLK/ XDATA(2050,4),XRE,YRE,ZRE,SUNCLOCK,SPINRATE
	COMMON /XYBLOCK/ XSPECT,YSPECT,DPHASE
	COMMON /ANGLEBLK/ BANGLE,IAANGLE
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
	character*80	file
	INTEGER*4 SUNCLOCK
	DIMENSION YY(2048),XX(2048),XD(2048),YD(2048),PP(2048)
	REAL XSPECT(1025),YSPECT(1025)

	DATA LABEL /'EX mV/m','EY mV/m','EZ mV/m','B nT'/
	DATA TWOPI /6.2831853/
C
		N1 = 1
		N2 = 2
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,450.,2000.,3050.)
  	  ENDIF
C
c	  XTITLE = GX2 +.05*(GX2-GX1)
	  XTITLE = GX2 +.1*(GX2-GX1)              ! 3 dec 1996
	  YTITLE = GY2
	  TRANGE = GY2-GY1
	  TINC = .03*TRANGE
	  CALL MGOSETEXPAND(.6)
	  TITLE(6) = ' '
	  TITLE(7) = ' '
	  DO N = 1,18
	    YTITLE = YTITLE - TINC
	    IF(N.EQ.4) YTITLE = YTITLE - TINC
	    IF(N.EQ.6) YTITLE = YTITLE - TINC
	    IF(N.EQ.19) YTITLE = YTITLE - TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(12,TITLE(N))
	  ENDDO
C
C	  PLOT TDS DATA 
	  YMAX = 0.
	  EFFLENX = 41.1				! X ANTENNA  23-SEP-96
	  EFFLENY = 3.79  				! Y ANTENNA   "
	  EFFLENZ = 2.17  				! Z ANTENNA   "
	  ACORR = 1000.
	  IF(IRX.GE.7) THEN			! SEARCH COILS
		ACORR=1.
		EFFLEN = 1.
	  ENDIF
C
C	CHANGE TO mV/meter
	  DO N = 1,2048
	    XD(N) = ACORR*XDATA(N,N1)/EFFLENX
	    YD(N) = ACORR*XDATA(N,N2)/EFFLENY	
	    YMAX = AMAX1(YY(N),YMAX)
	    YMAX = AMAX1(-YY(N),YMAX)
	    XMAX = AMAX1(XX(N),XMAX)
	    XMAX = AMAX1(-XX(N),XMAX)
	  ENDDO
C
C************WRITE DATA TO A FILE FOR THE PSSLW PAPER
	iwrt = 0
	if(iwrt.eq.1) then
	  WRITE(32,*) FILE,S_SCET,NUMEVENT,SUNCLOCK,BANGLE
	  WRITE(33,*) FILE,S_SCET,NUMEVENT,SUNCLOCK,BANGLE
	  DO N = 1,2048
  	    PP(N) = 1000.*(N-1)/SPS
	    PS    = N*SPS/2048.
ct	    WRITE(32,*) PP(N),XD(N),YD(N)
	  ENDDO
C
	  DO N = 1,1024
	    PS    = N*SPS/2048.
ct	    WRITE(33,*) PS,XSPECT(N),YSPECT(N)
	  ENDDO
	  CALL INT_E_DT(PP,XD,YD,EDTX,EDTY)
	  open(unit=47,name='potential.results',type='old',access='append')
	  write(47,*) s_scet,numevent,edtx,edty
	  close(unit=47)
	endif
C************
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
c	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	  NXW = 3
	  NYW = 5
	  NW = NXW*NYW
	  DO IW = 1,NW
	    CALL MGOWINDOW(NXW,NYW,IW)
	    NPTST = (IW-1)*2048/NW + 1
	    NPTND = (IW)*2048/NW
	    NPT = 0	
	    XMAX = 0.
	    YMAX = 0.
	    NPTMAX = NPTST
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
	      XX(NPT) = XD(N)
	      YY(NPT) = YD(N)
	      IF(ABS(XX(NPT)).GT.XMAX) NPTMAX = NPT
	      XMAX = AMAX1(XMAX,ABS(XX(NPT)))
	      YMAX = AMAX1(YMAX,ABS(YY(NPT)))
	    ENDDO
	    XMAX = 1.1*AMAX1(XMAX,YMAX)
	    YMAX = XMAX
	    CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(XX,YY,NPT)
C	    PUT ON ARROW
	    SIZE = .01*SQRT(XMAX**2 + YMAX**2)
	    DX = XX(NPTMAX+1) - XX(NPTMAX)
	    DY = YY(NPTMAX+1) - YY(NPTMAX)
	    CALL ARROW(XX(NPTMAX),YY(NPTMAX),DX,DY,SIZE)
C	if(n2.eq.2) then
c	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
C	  print*,nptmax,xx(nptmax),yy(nptmax)
c	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
C	ENDIF
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(7,LABEL(N1))
	    CALL MGOYLABEL(7,LABEL(N2))
	    CALL MGORELOCATE(-.95*XMAX,.90*YMAX)
	    CALL MGOSETEXPAND(.4)
	    WRITE(STR,1017)  NPTST,NPTND
 1017	    FORMAT('SAMPLES',I5,' TO',I5)
	    CALL MGOLABEL(20,STR)
C	    CALL MGOSETEXPAND(.8)
	    CALL MGOSETEXPAND(1.)
C
C	PUT ON B VECTOR
C
	END_ANGLE =  -360.*(SUNCLOCK-14)/4096. - 45.   ! ANGLE SUN TO +EX AT END
	DANG = SPINRATE*360./SPS/TWOPI
	CTR_ANGLE = END_ANGLE + 2048.*DANG  !ANGLE SUN TO +EX AT CENTER,nov 1999
	PANGLE = CTR_ANGLE + BANGLE
	PANGLE = 360. - PANGLE
	XBVEC = .2*XMAX*COSD(PANGLE)
	YBVEC = .2*XMAX*SIND(PANGLE)
	XX1 = -.7*XMAX
	YY1 = .7*XMAX
c	print*,sunclock,ctr_angle,bangle,pangle
	CALL MGORELOCATE(XX1,YY1)
	CALL MGODRAW(XX1+XBVEC,YY1+YBVEC)
	CALL MGOSETEXPAND(.5)
	CALL MGOLABEL(1,'B')
	CALL MGOSETEXPAND(.8)
C		SPSKHZ = .001*SPS
C
	  ENDDO
C
	  IF(IPROCESS.GE.3) THEN
	    YTITLE = YTITLE-2.*TINC
	    CALL MGOSETEXPAND(.5)
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(8,' BAD TDS')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(9,'CORRECTED')
	    YTITLE = YTITLE-TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    WRITE(STR,1024) IPROCESS
 1024	    FORMAT(' LEVEL',I2)
	    CALL MGOLABEL(8,STR)
C	    WRITE(STR,1025) NBAD3
 1025	    FORMAT(I5,' PTS')
C	    YTITLE = YTITLE-TINC
C	    CALL MGOGRELOCATE(XTITLE,YTITLE)
C	    CALL MGOLABEL(9,STR)
	    CALL MGOSETEXPAND(.8)
	  ENDIF
C
	  CALL MGOSETEXPAND(.6)
 	  CALL MGOPLOTID(' ','[.WIND]TDSEXEY,PLOTPART')
	  CALL MGOSETEXPAND(.8)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
C
	END	

