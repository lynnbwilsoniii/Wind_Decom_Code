	SUBROUTINE SUMMPLOT(ITERM,CH,NPTST,NPTND)
C
C	LEFT HAND SECTION PLOTS MEASURED FIELDS, AND BZ RAW, RIGHT
C	PLOTS ONE COMPONENT AGAINST ANOTHER FOR A SELECTED FRACTION
C		 OF AN EVENT
C
	CHARACTER*12 title(25)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	CHARACTER*6 LABELY(10)
	CHARACTER*4 LABEL1(4)
	CHARACTER*12 LABELT
	character*32 ITEM
	INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS,DOY,ERT(2),OK
	INTEGER*4 SUNCLOCK,CH
	REAL*8 SCET8
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /FIXUPBLK/ ISTART
	common /nrblk/ nrem,NHRMN,IFASTSLOW,ANGEVTOB(3)
	common /headblk/ major,minor,s_scet,nsys
	COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
	COMMON /PARTBLK/ XDATA(2050,4),XFDATA(2050,4),XGSE(2050,4),
     1		XRE,YRE,ZRE,SUNCLOCK,SPINRATE,SPSS,IANT(4),AVRFREQ
	COMMON /EXTRA/ NBXBY,NDATA(2048,4),DBSPEC(1025,4),AVRB,STDB
	COMMON /FRLIMITS/ FREQMIN,TOPFREQ
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
	DIMENSION YY(2048),XX(2048),PP(2048)
	REAL AVRB(1025,4),STDB(1025,4)
	DATA TWOPI /6.2831853/
C	DATA LABELB /'BX(nT)','BY(nT)','BZ(nT)','EX(V)','BZ raw'/
	DATA LABELY /'EX(V)','EY(V)','EZ(V)','EX(V)','EY(V)','EZ(V)',
     1	'BX(nT)','BY(nT)','BZ(nT)','raw'/
	DATA LABEL1 /'E1','E2','E3','B'/
	DATA DOY /0/
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	  NSYS = 0			! POSSIBLY TEMPORARY
c	print*,'at summ, iant=',iant
C*************
C	DO N = NPTST,NPTND
C	  WRITE(67,*) N,XFDATA(N,1),XFDATA(N,2),XFDATA(N,3)
C	ENDDO
C
C	  PUT LABELS ON RIGHT HAND SIDE
C
C 1012	  FORMAT(I10)
 1009	  FORMAT(F8.1)
	  TITLE(12) = 'SCET'
c	  ITEM = 'EVENT_SCET'
c	  ok = W_ITEM_I4(ch, item, S_SCET, 2, ret_size)
	  WRITE(STR,1013) s_scet(1)
 1013	  FORMAT(I8)
	   WRITE(TITLE(13),1016) str(1:4),str(5:6),str(7:8)
	   ITEM = 'EVENT_SCET_R8'
	   ok = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	   call w_ur8_to_ydoy(scet8,yyyy,doy,msday)
	   WRITE(TITLE(14),1014) DOY
	   WRITE(TITLE(15),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016	   format(A4,'/',A2,'/',A2)
 1014	   FORMAT(' DOY ',I4)
 1018	   FORMAT(I6.4,I3.2)
	  WRITE(title(18),*) 'RECEIVED AT' 
	  ITEM = 'EVENT_TM_SCET_I4'
	  ok = w_item_I4(ch, item, ERT, 2, return_size)
	  WRITE(STR,1013) ERT(1)
	  WRITE(TITLE(19),1016) str(1:4),str(5:6),str(7:8)
	  WRITE(title(20),1018) ERT(2)/100,mod(ERT(2),100)
 1017	  format(I10)
	  WRITE(title(21),1021) '  Xre',XRE
	  WRITE(title(22),1021) '  Yre',YRE
	  WRITE(title(23),1021) '  Zre',ZRE
 1021	  format(a5,f5.1)
C
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,320.,3050.,2170.)
	  ELSE
	    CALL MGOSETLOC(339.,50.,661.,750.)
 	  ENDIF
C
	  XTITLE = GX2 -.1*(GX2-GX1)              ! 3 dec 1996
	  YTITLE = GY2
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  TRANGE = GY2-GY1
	  TINC = .03*TRANGE
	  CALL MGOSETEXPAND(.6)
	  TITLE(6) = 'SAMPLES'
	  WRITE(STR,1007) NPTST,NPTND
 1007	  FORMAT(I4,' TO',I5)	
	  TITLE(7) = STR(1:12)
	  TITLE(16) = 'FFT BANDPASS'
	  WRITE(TITLE(17),1019) FREQMIN,TOPFREQ
 1019	  FORMAT(F5.1,'-',F5.0)
	  YTITLE = YTITLE + TINC
	  DO N = 4,23
	    YTITLE = YTITLE - TINC
	    IF(N.EQ.6) YTITLE = YTITLE - TINC
	    IF(N.EQ.16) YTITLE = YTITLE - TINC
	    IF(N.EQ.18) YTITLE = YTITLE - TINC
	    IF(N.EQ.21) YTITLE = YTITLE - TINC
	    CALL MGOGRELOCATE(XTITLE,YTITLE)
	    CALL MGOLABEL(12,TITLE(N))
	  ENDDO
C
C	NEW STUFF IN  SPACE
C
   	  XTITLE = GX2 -.3*(GX2-GX1)         
	  YTITLE = GY2
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(4,'TDSS')
	  YTITLE = YTITLE-2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  TITLE(24) = ' AVR.FREQ.'
	  WRITE(TITLE(25),1025) AVRFREQ
 1025	  FORMAT(F8.2,' kHZ')
	  CALL MGOLABEL(12,TITLE(24))
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(25))	
C
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(15,'ANGLE, SMALLEST')	
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,'EIGENVECT TO B')
	  YTITLE = YTITLE - TINC  
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(LABELT,1022) ANGEVTOB(3)
 1022	  FORMAT(F5.0,' deg')
	  CALL MGOLABEL(10,LABELT)
C
	  YTITLE = YTITLE - 2.*TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(15,'ANGLE, LARGEST')	
	  YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(14,'EIGENVECT TO B')
	  YTITLE = YTITLE - TINC  
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  WRITE(LABELT,1022) ANGEVTOB(1)
	  CALL MGOLABEL(10,LABELT)
C
C	  PLOT TDS DATA 
C
C
C	  IF(ITERM.LT.0) THEN
C	    CALL MGOSETLOC(500.,350.,2700.,2200.)
C	  ELSE
C	    CALL MGOSETLOC(339.,50.,661.,750.)
C 	  ENDIF
C
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	XMAX = 0.
	YMAX = 0.
	EANGLE =  -360.*SUNCLOCK/4096. - 45.       ! ANGLE SUN TO +EX AT END
	DANG = SPINRATE*360./SPS/TWOPI		  ! CHANGE PER SAMPLE
	THANT = EANGLE + 2049.*DANG + 360.	  ! ANGLE SUN TO +EX AT MIDDLE??
c
c	PRINT*,'IN PLOTSEL, XFDATA=',(XFDATA(1,N),N=1,4)
C
C	PLOT MEASURED FIELDS ON LEFT SIDE
C
	  NXW = 3
	  NYW = 5
	  NW = 15
C	the plots on the left side have nxw = 1, nyw = 1-5, iw = 1,4,7,10,13
C	       and n2 = 1,2,3,4,5.  n2 = 1,2,3 are X,Y,Z, of whichever field
C	       is main field, 4 is the other, and 5 is raw data	
	  DO IW = 1,NW,3
	    CALL MGOWINDOW(NXW,NYW,IW)
	    N2 = (IW+2)/3
C
	    NPT = 0	
	    YMAX = -1000.
	    YMIN = 1000.
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
  	      PP(NPT) = (N-1)/SPSS
	      IF(N2.GT.4) THEN				! BZ RAW
	        YY(NPT) = NDATA(N,3)
  	        PP(NPT) = THANT - N*DANG 
	      ELSE
	        YY(NPT) = XDATA(N,N2)
	      ENDIF
	      YMAX = AMAX1(YY(NPT),YMAX)
	      YMIN = AMIN1(YY(NPT),YMIN)
	    ENDDO
C	print*,'y',iw,n2,ymin,ymax,iant(n2)
	    CALL MGOSETLIM(PP(1),YMIN,PP(NPT),YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(PP,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    IF(IW.EQ.1) CALL MGOXLABEL(3,'SEC')
	    IF(N2.GT.4) CALL MGOXLABEL(17,'ANGLE,SUN TO +EX')
	    IF(N2.LE.4) NLAB = IANT(N2)
	    IF(N2.GT.4) NLAB = 10
c	    PRINT*,'NLAB',NLAB
c	    PRINT*,'YLABEL CHECK ',NLAB,N2,LABELY(NLAB)
	    CALL MGOYLABEL(6,LABELY(NLAB))
	    CALL MGOSETEXPAND(1.)
C	if(n2.eq.2) then
c	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
C	  print*,nptmax,xx(nptmax),yy(nptmax)
c	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
C	endif
C
	  ENDDO
C
c	  IF(IPROCESS.GE.3) THEN
c	    YTITLE = YTITLE-2.*TINC
c	    CALL MGOSETEXPAND(.5)
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    CALL MGOLABEL(8,' BAD TDS')
c	    YTITLE = YTITLE-TINC
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    CALL MGOLABEL(9,'CORRECTED')
c	    YTITLE = YTITLE-TINC
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    WRITE(STR,1024) IPROCESS
 1024	    FORMAT(' LEVEL',I2)
c	    CALL MGOLABEL(8,STR)
c	    WRITE(STR,1026) ISTART
 1026	    FORMAT(I5,' PTS')
c	    YTITLE = YTITLE-TINC
c	    CALL MGOGRELOCATE(XTITLE,YTITLE)
c	    CALL MGOLABEL(9,STR)
c	    CALL MGOSETEXPAND(.8)
c	  ENDIF
C
	  CALL MGOSETEXPAND(.6)
C
C	PLOT MINIMUM VARIANCE FIELDS ON RIGHT SIDE
C
	  XMAX = -1000.
	  YMAX = -1000.
    	  DO N = NPTST,NPTND
	     IF(ABS(XFDATA(N,1)).GT.XMAX) THEN
		XMAX = ABS(XFDATA(N,1))
		NXMAX = N
	     ENDIF
	     IF(ABS(XFDATA(N,2)).GT.YMAX.OR.XFDATA(N,3).GT.YMAX) THEN
	        YMAX = AMAX1(ABS(XFDATA(N,3)),ABS(XFDATA(N,2)))
		NYMAX = N
	     ENDIF
	  ENDDO
	  IF(1.364*YMAX.GT.XMAX) THEN
		NMAX = NYMAX
	  ELSE
		NMAX = NXMAX
	  ENDIF
c	PRINT*,'SUMMPLOT,iw,XMAX,YMAX',IW,XMAX,YMAX
c	PRINT*,'SUMMPLOT,NMAX,NXMAX,NYMAX',NMAX,NXMAX,NYMAX
C	  XMAX = 1000.*XMAX
C	  YMAX = 1000.*YMAX
	  XMAX = 1.1*AMAX1(XMAX,1.364*YMAX)
	  YMAX = XMAX/1.364
C
	  NXW = 3
	  NYW = 3
	  NW = 9
	  DO IW = 2,NW,3
	    IW1 = IW
	    IF(IW.EQ.8) IW1 = 3
	    CALL MGOWINDOW(NXW,NYW,IW1)
	    N1 = IW/2
	    N2 = N1+1
	    IF(IW.EQ.8) THEN
		N1 = 1
		N2 = 3
	    ENDIF
C
	    NPT = 0	
    	    DO N = NPTST,NPTND
	      NPT = NPT+1
	      XX(NPT) = XFDATA(N,N1)
	      YY(NPT) = XFDATA(N,N2)
	    ENDDO
	    CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
C	    CALL MGOTICKSIZE(0.,0.,0.,0.)  
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(2,LABEL1(N1))
	    CALL MGOYLABEL(2,LABEL1(N2))
	    CALL MGOSETEXPAND(.8)
	    CALL MGOSETEXPAND(1.)
C	    PUT ON ARROW
	    NPTMAX = NMAX - NPTST + 1
	    SIZE = .01*SQRT(XMAX**2 + YMAX**2)
	    IF(ITERM.GT.0) SIZE = 2.*SIZE
	    XARR = .5*(XX(NPTMAX+1) + XX(NPTMAX))
	    YARR = .5*(YY(NPTMAX+1) + YY(NPTMAX))
	    DX = XX(NPTMAX+1) - XX(NPTMAX)
	    DY = YY(NPTMAX+1) - YY(NPTMAX)
c	print*,'summp',iw,nptst,nptnd
c	print*,'nptmax',nptmax,xx(nptmax+1),xx(nptmax)
c	print*,'summp',dx,dy,xarr,yarr,xmax
	write*39,*) 'summp',dx,dy,xarr,yarr,size
	IF(DX**2+DY**2.NE.0.) CALL ARROW(XARR,YARR,DX,DY,SIZE)
C	if(n2.eq.2) then
c	  print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
C	  print*,nptmax,xx(nptmax),yy(nptmax)
c	  print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
C	endif
C
	  ENDDO
C
	  CALL MGOSETEXPAND(.8)
C
C	PLOT A SPECTRUM
C
	  NXW = 3
	  NYW = 3
	  IW = 8
	  YMIN = 1000.
	  YMAX = -1000.
	  CALL MGOWINDOW(NXW,NYW,IW)
	    NPT = 0	
    	    DO N = 2,1024
	      NPT = NPT+1
	      XX(NPT) = ALOG10(SPS*(N-1)/2048.)
	      YY(NPT) = DBSPEC(N,NBXBY)
	      YMAX = AMAX1(YY(NPT),YMAX)
	      YMIN = AMIN1(YY(NPT),YMIN)
	    ENDDO
	    XMAX = ALOG10(SPS/2.)
	    XMIN = ALOG10(SPS/2048.)
	    CALL MGOTICKSIZE(-1.,-1.,0.,0.)  
	    CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOCONNECT(XX,YY,NPT)
	    NPT = 0
	    IF(NBXBY.EQ.3) NBXBY = 2
    	    DO N = 2,1024
	      NPT = NPT+1
	      YY(NPT) = AVRB(N,NBXBY)
	    ENDDO
	    CALL MGOSETLTYPE(1)
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETLTYPE(0)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,2)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOXLABEL(9,'FREQ (Hz)')
	    IF(NBXBY.EQ.1) CALL MGOYLABEL(14,'BX dB nT\u2/Hz')
	    IF(NBXBY.EQ.2) CALL MGOYLABEL(14,'BY dB nT\u2/Hz')
C
C	  CALL MGOSETEXPAND(.6)
C	print*,'in SUMMPLOT,nsys=',nsys
 	  IF(NSYS.EQ.0)CALL MGOPLOTID('S/C','[.WIND]TDSXYZ,SUMMPLOT')
 	  IF(NSYS.EQ.1)CALL MGOPLOTID('SN,CL','[.WIND]TDSXYZ,SUMMPLOT')
 	  IF(NSYS.EQ.2)CALL MGOPLOTID('VAR MX','[.WIND]TDSXYZ,SUMMPLOT')
 	  IF(NSYS.EQ.3)CALL MGOPLOTID('GSE','[.WIND]TDSXYZ,SUMMPLOT')
	    CALL MGOSETEXPAND(.8)
 
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
c	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
C
	END	

