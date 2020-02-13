	SUBROUTINE TDPLOT(ICH)         
C         
C	PLOTS THE TIME DOMAIN DATA, AND THE GAIN STEP         
C		note that ich is the stream channel, not fft_channel         
C         
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)         
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY         
	common /headblk/ major,minor,IFFTCH,isrc,scet,titlex,scet8         
	COMMON /RAWVOLTS/ RDATA(1026),RAWAVR,SLOPE
	common /angles/iantang,antang,ezang,bzantang         
	character*32 scet,item         
	character*10	titlex(15)         
	integer*4 return_size,ok,w_item_r4         
	real*8 scet8         
	real*4 gain(1024)         
c         
	COMMON /MONGOPAR/         
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,         
     1  GX,GY,CX,CY,         
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,         
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,         
     1  TERMOUT,XYSWAPPED,NUMDEV,         
     1  PI,USERVAR(10),AUTODOT         
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV         
C         
	DIMENSION PTIME(1024),YY(1024)         
	CHARACTER*8 TITLE(4)         
	CHARACTER*120 STRG(5)         
	CHARACTER*120 STR         
	DATA TWOPI /6.2831853/         
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/         
C         
	IFCORR = 1			! CORRECTED FOR FREQ. RESPONSE         
C         
C	ITERM = 3         
	ITERM = -2         
	ITERM = -4         
C         
	PRINT*,' IN tdplot, stream chann,fft chan,source=',ich,ifftch,isrc    
	    print*,'tdplot, headblk=',major,minor,ifftch,isrc         
	FFUND = 20.3477         
	IF(IFFTCH.GT.2) FFUND = .25*FFUND         
	IF(IFFTCH.GT.6) FFUND = .333557         
	DO JF = 1,1024         
	  PTIME(JF) = (JF-1)/1.024/FFUND		!XAXIS IN mSEC         
	ENDDO         
	XRANGE = PTIME(1024) - PTIME(1)         
	XMIN = PTIME(1) - .022*XRANGE         
	XMAX = PTIME(1024) + .022*XRANGE         
C         
	  CALL MGOINIT         
	  CALL MGOSETUP(ITERM)         
	  CALL MGOERASE         
	  GYRANGE = GY2-GY1         
	IF(ITERM.LT.0) THEN         
	  PRINT*,'START tdplot',GX1,GX2,GY1,GY2         
C	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-200.)         
	  CALL MGOSETLOC(GX1,600.,GX2,GY2-200.)         
	ELSE         
	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-.08*GYRANGE)         
	ENDIF         
C         
	  CALL MGOSETEXPAND(.8)         
	  TOP = LY2         
cold	  CALL MGOGRELOCATE(GX1,TOP-50.)          
C	  CALL MGOGRELOCATE(GX1,TOP-100.)                             
	  CALL MGOGRELOCATE(GX1,TOP)
	  CALL MGOPUTLABEL(6,'SCET   ',3)         
	  CALL MGOPUTLABEL(79,SCET,3)         
	  WRITE(STR,801) MAJOR,MINOR         
 801	  FORMAT('MF.mf ',I7,'.',I3.3)         
 802	  FORMAT(2I6)         
	  CALL MGOPUTLABEL(17,STR,3)         
	  CALL MGOPUTLABEL(12,'  R8 time ',3)         
	  WRITE(STR,803) SCET8         
 803	  FORMAT(F14.8)         
	  CALL MGOPUTLABEL(14,STR,3)         
	  CALL MGOGRELOCATE(GX1,GY1+.2*(GY2-GY1))                 
	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3)         
	  WRITE(STR,802) IFFTCH         
	  CALL MGOPUTLABEL(8,STR,3)         
	  CALL MGOPUTLABEL(9,'  SOURCE ',3)         
	  IW = IFFTCH         
	  IF(IFFTCH.GT.2) IW = IFFTCH-2         
	  IF(IFFTCH.GT.6) IW = IFFTCH-6         
	  CALL MGOPUTLABEL(10,TITLEX(IW),3)         ! SOURCE IN CHARACTERS
	  CALL MGOPUTLABEL(12,' MAX,MIN,AVR',3)
	  GXSAVE = GX
	  GYSAVE = GY
C	  IF(ITERM.GT.0) THEN         
C	    CALL MGOGRELOCATE(10.,0.)                             
C	  ELSE         
C	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy         
C	  ENDIF         
	  CALL MGOSETEXPAND(1.)         
C         
	  IF(ISRC.LE.6) THEN         
	   item = 'E_VOLTS/METER'         
	  ELSE         
	   item ='B_NT2/HZ'         
	  ENDIF         
c	  ok = w_item_R4(ICH, item, FDATA, 1024, return_size)         
c	  print*,'in tdplot, item,return_size',item,return_size         
C	  if(ok.eq.0) then         
C	     print*,'ok = 0 in tdplot, no raw data, no item ',item         
C	     return         
C	  endif         
c	  YMIN = FDATA(1)         
	  YMIN = 1.E6         
	  YMAX = -YMIN         
	  JP = 1         
	IF(IFCORR.NE.1) THEN    
	  AVR = 0.     
	  DO J = 1,1024         
	    JP = J         
C	    YY(JP) = DATA(J)         
c		rdata, called data in fft_phys, is just t/m numbers         
c			multiplied by (volts per step/eff length)         
	    YY(JP) = RDATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)
	    AVR = AVR+YY(JP)         
	  ENDDO         
	ELSE         
	  DO J = 1,1024         
	    JP = J         
c		data, called vdata in fft_phys, is volts/meter         
c			corrected for frequency response         
	    YY(JP) = DATA(J)         
	    YMAX = AMAX1(YY(JP),YMAX)         
	    YMIN = AMIN1(YY(JP),YMIN)         
	    AVR = AVR+YY(JP)         
	  ENDDO         
	ENDIF         
	AVR = AVR/1024.
C         
	  RANGE = ABS(YMAX-YMIN)         
	  YMAX = YMAX + .05*RANGE         
	  YMIN = YMIN - .05*RANGE	         
	  IF(IFFTCH.LE.2) THEN         
	    CALL MGOTICKSIZE(5.,20.,0.,0.)         
	  ELSEIF(IFFTCH.LE.6) THEN         
	    CALL MGOTICKSIZE(10.,50.,0.,0.)         
	  ELSE         
	    CALL MGOTICKSIZE(100.,500.,0.,0.)         
	  ENDIF         
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)         
	  CALL MGOCONNECT(PTIME,YY,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOXLABEL(4,'mSEC')         
	  CALL MGOYLABEL(18,'V/M OR nT')         
	  XTITLE = .5*(XMAX+XMIN)         
	  YTITLE = YMIN + .9*(YMAX-YMIN)         
	  CALL MGORELOCATE(XTITLE,YTITLE)         
	  IF(IFCORR.EQ.1) THEN         
	    CALL MGOLABEL(28,'CORRECTED FOR FREQ. RESPONSE')         
	  ELSE         
	    CALL MGOLABEL(32,'NOT CORRECTED FOR FREQ. RESPONSE')         
	  ENDIF         
	  CALL MGOSETEXPAND(1.)         
C         
	CALL MGOSETEXPAND(.7)         
	CALL MGOPLOTID('[kellogg.wind]FFTPRO','TPLOT')         
	CALL MGOSETEXPAND(1.)         
C         
C	PLOT GAIN STEP         
C         
c	IF(IFFTCH.GT.6.AND.ISRC.EQ.4) THEN         
	  ITEM = 'WIND_SPIN_RATE_R4'         
	  OK = W_ITEM_R4(ICH,ITEM,SPINRATE,1,ISIZE)         
	  SAMP_PER_SEC = 1024.*FFUND         
	  DANG = SPINRATE/SAMP_PER_SEC			! IN RADIANS         
	  DANGD = DANG*360./TWOPI         
	  SAMP_PER_SPIN = TWOPI/DANG         
	  NSPPS = SAMP_PER_SPIN + .5			! ROUNDOFF         
c	  CALL FFTLOANGLE(ICH,DATA,SUNCLOCK,ANTANG)         
c	  PRINT*,'FFTLOANGLE CALLED FROM tdplot'         
c	ENDIF         
 
	item = 'EXPONENT'         
	ok = w_item_r4(ich, item, gain, 1024, return_size)         
	PRINT*,'START gain',GX1,GX2,GY1,GY2         
	IF(IANTANG.NE.0) THEN         
	  DO JF = 1,1024         
	    PTIME(JF) = ANTANG + (1024-JF)*DANGD         
	  ENDDO         
	ELSE         
	  DO JF = 1,1024         
	    PTIME(JF) = JF         
	  ENDDO         
	ENDIF         
	  PRINT*,'ANGLE CHECK',IANTANG,ANTANG,PTIME(1),PTIME(1024)         
	  XRANGE = ABS(PTIME(1024) - PTIME(1))         
C	  IF(PTIME(1024).GT.PTIME(1)) THEN         
	    XMIN = PTIME(1) - .022*XRANGE         
	    XMAX = PTIME(1024) + .022*XRANGE         
C	  ELSE         
C	    XMAX = PTIME(1) + .022*XRANGE         
C	    XMIN = PTIME(1024) - .022*XRANGE         
C	  ENDIF	         
	  IF(ITERM.LT.0) THEN         
c	    TOP = GY1 - .01*GYRANGE         
	    TOP = GY1 - .07*GYRANGE         
	    BOTT = 250.          
	    CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
	  ELSE         
	    CALL MGOSETLOC(GX1,BOTT,GX2,TOP)         
	  ENDIF         
C         
	  CALL MGOSETLIM(XMIN,-.5,XMAX,3.5)         
	IF(IANTANG.NE.0) THEN         
	  CALL MGOXLABEL(17,'ANGLE, SUN TO +EX')         
	  CALL MGOTICKSIZE(10.,90.,1.,1.)         
	ELSE         
	  CALL MGOXLABEL(10,'SAMPLE NO.')         
	  CALL MGOTICKSIZE(20.,200.,1.,1.)         
	ENDIF         
	  CALL MGOCONNECT(PTIME,GAIN,JP)         
	  CALL MGOSETEXPAND(.8)         
	  CALL MGOBOX(1,2)         
	  CALL MGOYLABEL(18,'GAIN STEP')         
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
C         
	END         
