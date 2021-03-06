	SUBROUTINE MAKEFILE(CH,CDFCH)
C
C	THIS IS MAKEFILE24
C	A PROGRAM TO SEARCH FOR UPSTREAM OR SOLAR WIND SPIKES
C	THE FITTING PROGRAM HAS BEEN MOVED TO DATA_A:[KELLOGG]SPIKEFIT
	COMMON /XFER/ SPHASE(1025)
	COMMON /FRLIMITS/ FFTMIN,FFTMAX
	COMMON/FITDATA/N1,N2,V1DATA(2048),V2DATA(2048),IPRNT,NLAG1,NLAG2,
     1	  NVAR
	integer*4 ch,cdfch,ok,okt,OK1,SCETI4(2),N1DATA(2048),N2DATA(2048)
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	INTEGER*4 TDS_CHANNEL,ISRC,SUNCLOCK,ERT(2),ERTDAY,MAJOR,MINOR
	REAL FCOUNT(2),FAVR(2),FSTD(2),F3MOM(2),FBW(2)
	REAL*8 SCET8,XGSE,YGSE,ZGSE,RGSE
	REAL DBSPEC1(1025),V1DATA,PHDIFF(1025),X(25),DX(25),Y(25)
	REAL DBSPEC2(1025),V2DATA,DELB(3),CROSS(3),DIRCUR(3),BDATA(20,3)
	REAL EVAL(2),EVEC(2,2),EVLT(2),EVCT(2,2),LEFFX,LEFFY,BAVR(3)
C	REAL F(10),AMP(10),BW(10),SKEW(10),SKEWT(2),XYANG(10),ELLIP(10)
	REAL FREQ(1025),WT(100)
	character*32 ITEM
	character*4 event
	DATA TWOPI /6.2831853/
	DATA YXRATIO /11.8/
C	ABOVE BASED ON LEFFX = 41.1, LEFFY = 3.79
	DATA LEFFX,LEFFY/ 41.1,3.79/
	DATA RE /6.378E3/
	DATA ANGTOB,TDIST /0.,0./
	DATA WT /100*1./
C	data event /'TDSF'/
	data event /'FILL'/
C
	CONST1 = (1./LEFFX)**2		! DO IN (VOLTS/M)**2
	CONST2 = (1./LEFFY)**2
	YXSQ = YXRATIO**2
	IFOUND = 0
C
C	CALCULATE FREQUENCIES FOR SPECTRUM
C
	SPS = 120000.
	DO N = 1,1025
	  FREQ(N) = (N-1)*SPS/2048.    ! FREQ(N) GOES WITH DBSPECT(N)
C	  DBSPECT(2) IS LOWEST NON-ZERO FREQUENCY, DBSPECT(1) IS DC
	ENDDO
C
C	OPEN(UNIT=70,FILE='MAKEFILE24.RESULTS',STATUS='OLD',ACCESS='APPEND')
 100	ok = w_event(ch,event)
C
C	CHECK FOR END OF RECORD
C
	if (ok.ne.1) then
		if(ok.eq.82) then
		   ok = w_channel_close(ch)
		   ok = w_channel_close(cdfch)
C		   CLOSE(UNIT=70)
		   return
	        endif
		write(6,*) 'cannot open ',event, ', ok=', ok
	endif
C
C	END OF END OF RECORD CHECK
C
	OKT = W_MESSAGES_OFF(ch)
C
c	find peak, assuming that it is near center
c	record, magnitude and sign of peak, magnitude and sign of peak in
c	opposite direction, widths of both = number of samples above half
c	maximum, number of other peaks above half maximum, major and minor 
c	frame, sunclock, dpuclock, raw and actual angle,
C
	ITEM = 'FAST_RX_SPEED'
	ok = W_ITEM_I4(ch, item, ISPS, 1, ret_size)
	IF(ISPS.NE.0) GO TO 100
C
	ITEM = 'CHANNEL'
	ok = W_ITEM_I4(ch, item, TDS_CHANNEL, 1, ret_size)
C
	IPROS = 4
	IF(TDS_CHANNEL.EQ.1) THEN
C	  ITEM = 'DATA'
C	  ok1 = W_ITEM_I4(ch, item, N1DATA, 2048, ret_size)
	  CALL TDS_PHYS(CH,IPROS,N1DATA,V1DATA,DBSPEC1)
	  ITEM = 'EVENT_NUMBER'
	  ok = W_ITEM_I4(ch, item, NO_EVT1, 1, ret_size)
	  MAXCH1 = 0
	  DO N = 1,2048
	    MAXCH1 = MAX0(MAXCH1,IABS(N1DATA(N)-128)) 
	  ENDDO
	  DO N = 1,1025
	    PHDIFF(N) = SPHASE(N)
	  ENDDO
C	  GO TO 100
	ELSEIF(TDS_CHANNEL.EQ.2) THEN
C	  ITEM = 'DATA'
	  CALL TDS_PHYS(CH,IPROS,N2DATA,V2DATA,DBSPEC2)
	  ITEM = 'EVENT_NUMBER'
	  ok = W_ITEM_I4(ch, item, NO_EVT, 1, ret_size)
C	  IF(NO_EVT.NE.NO_EVT1) GO TO 100
	  MAXCH2 = 0
	  DO N = 1,2048
	    MAXCH2 = MAX0(MAXCH2,IABS(N2DATA(N)-128)) 
	  ENDDO
	  DO N = 1,1025
	    PHDIFF(N) = PHDIFF(N) - SPHASE(N)
	  ENDDO
	ELSE
	  GO TO 100
	ENDIF
C
CDIAG	PRINT*,'******START EVENT',NO_EVT1,NO_EVT,' *****'
C
C	IF(TDS_CHANNEL.EQ.1) GO TO 100		! GO GET CHANNEL 2
C	IF(TDS_CHANNEL.NE.2) GO TO 100
C
C	SET STORAGES
C
C	DO N = 1,10
C	  BW(N) = 0.
C	  F(N) = 0.
C	  AMP(N) = 0.
C	  ELLIP(N) = 0.
C	  XYANG(N) = 0.
C	ENDDO
C
C	FIND LARGEST POSITIVE AND NEGATIVE PEAKS AND WIDTHS
C
	N1 = 1024 - 100			! LOWEST FREQ 177 HZ
	N2 = 1024 + 100
	IF(TDS_CHANNEL.EQ.1) THEN
C
C	NPK1,APK1 ARE SAMPLE NO AND AMPLITUDE OF POSITIVE PEAK, 2 IS NEG.
C
	  CALL FINDPEAK(N1,N2,V1DATA,NPK1,APK1,NPK2,APK2)
	  APK1P = 1000.*APK1/LEFFX
	  APK1N = 1000.*APK2/LEFFX
	  NLAG1 = NPK2-NPK1
	  MID = .5*(NPK1+NPK2)			! HALFWAY BETWEEN PEAKS
	  N1 = MID - (5*IABS(NPK1-NPK2))/4
	  N2 = MID + (5*IABS(NPK1-NPK2))/4
	  CALL PEAKWIDTH(N1,N2,V1DATA,NPK1,WPK1P,NPK2,WPK1N,IGB1)
	  NWPK1P = WPK1P + .01
	  NWPK1N = WPK1N + .01
	ELSE
	  CALL FINDPEAK(N1,N2,V2DATA,NPK1,APK1,NPK2,APK2)
	  APK2P = 1000.*APK1/LEFFY
	  APK2N = 1000.*APK2/LEFFY
	  NLAG2 = NPK2-NPK1
	  NPKP = NPK1
	  NPKN = NPK2
	  CALL PEAKWIDTH(N1,N2,V2DATA,NPK1,WPK2P,NPK2,WPK2N,IGB2)
	  NWPK2P = WPK2P + .01
	  NWPK2N = WPK2N + .01
	ENDIF
C
	IF(TDS_CHANNEL.EQ.1) GO TO 100		! GO GET CHANNEL 2
	IF(TDS_CHANNEL.NE.2) GO TO 100
	IF(IGB1.NE.0.AND.IGB2.NE.0) GO TO 100
C
 200	CONTINUE
C
C	ITEM = 'WIND_ORBIT_X(GSE)_R8'
C	ok = W_ITEM_R8(ch, item, XGSE, 1, ret_size)
C	XRE = XGSE/RE
C	ITEM = 'WIND_ORBIT_Y(GSE)_R8'
C	ok = W_ITEM_R8(ch, item, YGSE, 1, ret_size)
C	YRE = YGSE/RE
C	ITEM = 'WIND_ORBIT_Z(GSE)_R8'
C	ok = W_ITEM_R8(ch, item, ZGSE, 1, ret_size)
C	ZRE = ZGSE/RE
C	RRE = SQRT(XRE**2+YRE**2+ZRE**2)
C
C	IF(RGSE.LT.6.4D05) THEN			! LIMIT 100 RE
C	    ADVANCE BY ONE HOUR AND GET ANOTHER EVENT
C	    ITEM = 'EVENT_SCET_R8'
C	    OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
C	    ITEM = 'EVENT_SCET'
C	    ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
C	    PRINT*,RGSE/6.38E3,'INSIDE 100 RE',SCETI4
C	    SCET8 = SCET8 + 1.D00/24.D00
C	    CALL W_CHANNEL_POSITION(CH,SCET8)
C	    GO TO 100
C	  ENDIF
C
C	ITEM = 'SOURCE'
C	ok = W_ITEM_I4(ch, item, ISRC, 1, ret_size)
	ITEM = 'EVENT_NUMBER'
	ok = W_ITEM_I4(ch, item, NO_EVT, 1, ret_size)
	ITEM = 'EVENT_SCET'
	ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
C
	ITEM = 'WIND_MFI_BPHI(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BANGLE, 1, ret_size)
	ITEM = 'SUN_ANGLE'
	ok = W_ITEM_I4(ch, item, SUNCLOCK, 1, ret_size)
	item = 'DPU_CLOCK'
        ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
	ITEM = 'EVENT_TM_SCET_I4'
	ok = W_ITEM_I4(ch, item, ERT, 2, ret_size)
	ERTDAY = MOD(ERT(1),100)
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major,1,ret_size)
	item = 'DPU_MINOR_FRAME'
	ok = w_item_i4(ch, item, minor,1,ret_size)
	ITEM = 'WIND_3DP_E_TEMP_R4'
	ok = W_ITEM_R4(ch, item, TEMPE, 1, ret_size)
	ITEM = 'WIND_3DP_ION_TEMP_R4'
	ok = W_ITEM_R4(ch, item, TEMPI, 1, ret_size)
	ITEM = 'WIND_3DP_ION_DENSITY_R4'
	ok = W_ITEM_R4(ch, item, DENS, 1, ret_size)
C
	ITEM = 'WIND_3DP_ION_VX(GSE)_R4'
	ok = W_ITEM_R4(ch, item, VX, 1, ret_size)
C	ITEM = 'WIND_3DP_ION_VY(GSE)_R4'
C	ok = W_ITEM_R4(ch, item, VY, 1, ret_size)
	ITEM = 'WIND_MFI_BMAG_R4'
	ok = W_ITEM_R4(ch, item, BMAG, 1, ret_size)
	ITEM = 'WIND_SPIN_RATE_R4'
	ok = W_ITEM_R4(ch, item, SPINRATE, 1, ret_size)
	if(ok.ne.1) then
	  spinrate = 2.
	endif
C
	CALL EEVAL(N1,N2,V1DATA,V2DATA,EVLT,EVCT,EPT,XANG)
C
C	DETERMINE ANGLE TO B
C
	END_ANGLE =  -360.*(SUNCLOCK-14.)/4096. - 45. ! ANGLE SUN TO +EX AT END
	IF(END_ANGLE.LT.-180.) END_ANGLE = END_ANGLE + 360.
	IF(END_ANGLE.GT.180.)  END_ANGLE = END_ANGLE - 360.
	DANG = SPINRATE*360./SPS/TWOPI
	ST_ANGLE = END_ANGLE + 3072.*DANG   ! ANGLE SUN TO +EX AT START 16nov99
	END_ANGLE = END_ANGLE + 1024.*DANG
c	ST_ANGLE = END_ANGLE + 2048.*DANG	  ! ANGLE SUN TO +EX AT START
C
	NAVR = 1024
	CTR_ANGLE = ST_ANGLE - NAVR*DANG  !ANGLE SUN TO +EX AT CENTER,nov 1999
C	
C
C	ZANGLE IS THE ANGLE BETWEEN E AND THE DIRECTION TO THE SUN
C
	ZANGLE = CTR_ANGLE - XANG
	IF(ZANGLE.GT.90.)  ZANGLE = ZANGLE - 180.
	IF(ZANGLE.LT.-90.) ZANGLE = ZANGLE + 180.
	IF(ZANGLE.GT.90.)  ZANGLE = ZANGLE - 180.
	IF(ZANGLE.LT.-90.) ZANGLE = ZANGLE + 180.
	IF(ZANGLE.GT.90.)  ZANGLE = ZANGLE - 180.
	IF(ZANGLE.LT.-90.) ZANGLE = ZANGLE + 180.
	IF(NLAG1.LT.0) THEN
	  IF(ZANGLE.GT.0.) ZANGLE = ZANGLE - 180.
	ELSE
	  IF(ZANGLE.LE.0.) ZANGLE = ZANGLE + 180.
	ENDIF
C
C	ITEM = 'CAL_PA'
C	ok = W_ITEM_I4(ch, item, ICAL, 1, ret_size)
C	IF(ICAL.NE.0) GO TO 100
C	HAVE TO EXCLUDE CALS BY TIMING.  
c		to exclude cals
	  if(sceti4(2).ge.010000.and.sceti4(2).lt.011000) THEN
		PRINT*,'CALIBRATE, NO_EVT=',SCETI4,NO_EVT
		GO TO 100
	  endif
C
C	CALL DIFFFK(XRE,YRE,ZRE,BX,BY,BZ,DDIFF,XT,YT,ZT,XI,YI,ZI,IN)
C
C	TDIST = (XRE-XT)**2 + (YRE-YT)**2 + (ZRE-ZT)**2
C	TDIST = SQRT(TDIST)
C
C	CRITERIA
C
cdiag	print*,'start criteria, diff=',ddiff
c	IF(DDIFF.GT.0.) GO TO 100
C
	FP = 9.E3*SQRT(DENS)			  ! PLASMA FREQ IN KHZ
	FCE = .028*BMAG
	BETA = .402*(TEMPE+TEMPI)/BMAG**2
C
c	IF(FAVR(1).GT..8*FP) THEN
C	  PRINT*,'EVENT NO.',NO_EVT,' FREQ', F(1),' MAX',
C     1		MAXCH1,MAXCH2
c	  IFOUND = 0
c	ENDIF
C
cdiag	print*,'start special criteria'
C	require narrow peaks
C	if(no_evt.ne.13356589) go to 100
	print*,'crit',no_evt,nwpk1p,nwpk1n,nwpk2p,nwpk2n
	if(nwpk1p.gt.50.or.nwpk1p.eq.1) go to 100
	if(nwpk1n.gt.50.or.nwpk1n.eq.1) go to 100
	if(nwpk2p.gt.50.or.nwpk2p.eq.1) go to 100
	if(nwpk2n.gt.50.or.nwpk2n.eq.1) go to 100
	nsamp = 7
	call clayer(cdfch,scet8,nsamp,NLAYER,BDATA,DELB,BAVR)
C	DO N = 1,NSAMP
C	  WRITE(54,*) BDATA(N,1),BDATA(N,2),BDATA(N,3)
C	ENDDO	
C
	BAVANG = ATAN2D(BAVR(2),BAVR(1))
	PANGLE = CTR_ANGLE - BAVANG - XANG
	IF(PANGLE.GT.90.)  PANGLE = PANGLE - 180.
	IF(PANGLE.LT.-90.) PANGLE = PANGLE + 180.
	IF(PANGLE.GT.90.)  PANGLE = PANGLE - 180.
	IF(PANGLE.LT.-90.) PANGLE = PANGLE + 180.
	IF(PANGLE.GT.90.)  PANGLE = PANGLE - 180.
	IF(PANGLE.LT.-90.) PANGLE = PANGLE + 180.
	IF(PANGLE.GT.90.)  PANGLE = PANGLE - 180.
	IF(PANGLE.LT.-90.) PANGLE = PANGLE + 180.
	IF(NLAG1.LT.0) THEN
	  IF(PANGLE.GT.0.) PANGLE = PANGLE - 180.
	ELSE
	  IF(PANGLE.LE.0.) PANGLE = PANGLE + 180.
	ENDIF
C	XBVEC = .2*XMAX*COSD(PANGLE)
C	YBVEC = .2*XMAX*SIND(PANGLE)
C
C	PANGLE IS THE ANGLE BETWEEN THE MAJOR AXIS OF E, AND B
C	CHANGED FROM CDF VALUE TO BAVR ON 23 JUNE 2001
C	print*,'NO_EVT,bangle,ctr_angle',NO_EVT,bangle,ctr_angle
c	print*,'slope,angle to x,angle to b',slope,XANG,PANGLE
C
	XNORMBD = SQRT(DELB(1)**2 + DELB(2)**2)
	COSJE = (DELB(1)*EVCT(1,1) + DELB(2)*EVCT(1,2))/XNORMBD
	XNORMBA = SQRT(BAVR(1)**2 + BAVR(2)**2)
	COSBE = (BAVR(1)*EVCT(1,1) + BAVR(2)*EVCT(1,2))/XNORMBA
C
C	THE CROSS PRODUCT A X B IS 1/2 (A-B) X (A+B)
C	IF A IS DELB AND B IS BAVR
C	IT IS, OF COURSE, PERPENDICULAR TO DELB.  BOTH ARE PERPENDICULAR
C		TO THE CURRENT SHEET.  HENCE THEIR CROSS PRODUCT OUGHT TO
C		BE PARALLEL TO THE CURRENT.
C
	CROSS(1) = DELB(2)*BAVR(3) - DELB(3)*BAVR(2)
	CROSS(2) = DELB(3)*BAVR(1) - DELB(1)*BAVR(3)
	CROSS(3) = DELB(1)*BAVR(2) - DELB(2)*BAVR(1)
	CNORM = SQRT(CROSS(1)**2 + CROSS(2)**2)
	COSCE = (CROSS(1)*EVCT(1,1) + CROSS(2)*EVCT(1,2))/CNORM
C
	XNORMBF = SQRT(DELB(1)**2 + DELB(2)**2 + DELB(3)**2)
	CNORM = SQRT(CROSS(1)**2 + CROSS(2)**2 + CROSS(3)**2)
C
C	PRINT*,'CROSS',CROSS,CNORM
C	PRINT*,'DELB',DELB,XNORMBF
	print*,'event no',no_evt
C
	DIRCUR(1) = CROSS(2)*DELB(3) - CROSS(3)*DELB(2)
	DIRCUR(2) = CROSS(3)*DELB(1) - CROSS(1)*DELB(3)
	DIRCUR(3) = CROSS(1)*DELB(2) - CROSS(2)*DELB(1)
C	DIRNORM = SQRT(DIRCUR(1)**2 + DIRCUR(2)**2 + DIRCUR(3)**2)
	DIRNORM = SQRT(DIRCUR(1)**2 + DIRCUR(2)**2)
	COSCC = (DIRCUR(1)*EVCT(1,1) + DIRCUR(2)*EVCT(1,2))/DIRNORM
C
C	CALCULATE CHANGE OF DIRECTION OF B
C		Bbefore . Bafter = BAVR**2 - (1/4) DELB**2
C
	BAVR2 = BAVR(1)**2 + BAVR(2)**2  + BAVR(3)**2
	DELB2 = DELB(1)**2 + DELB(2)**2  + DELB(3)**2
	BDOTDB = BAVR(1)*DELB(1) + BAVR(2)*DELB(2) + BAVR(3)*DELB(3)
	DEN1 = SQRT(BAVR2 - BDOTDB + .25*DELB2)
	DEN2 = SQRT(BAVR2 + BDOTDB + .25*DELB2)
	COSDD = (BAVR2 - .25*DELB2)/DEN1/DEN2
	COSDD = AMIN1(COSDD,1.)
	COSDD = AMAX1(COSDD,-1.)
	ANGDD = ACOSD(COSDD)
	FCHANGE = SQRT(DELB2/BAVR2)
C
	WRITE(72,1013) sceti4,NO_EVT,ERTDAY,EVENT(1:1),XNORMBD,XNORMBA
     1		,COSCE
 1013	format(I10,I7,I10,I3,A2,2F7.2,F6.2)
C
C	WRITE EQUAL POSITIVE AND NEG PEAKS IN 70, ONE SIDED IN 71
C	NO, I DECIDED THAT TWO-SIDED WAS A BAD CRITERION
C	JUST ELIMINATE BLOBS AND PUT THEM IN 71
C
C	IF(ABS(APK1P+APK1N).LT..6*APK1P.AND.
C     1		ABS(APK2P+APK2N).LT..6*APK2P) THEN
C
	CALL BLOBOUT(N1DATA,V1DATA,IBLOB1)
	CALL BLOBOUT(N2DATA,V2DATA,IBLOB2)
	IF(IBLOB1.EQ.0.AND.IBLOB2.EQ.0) THEN
C
C	COSBE IS COSINE OF ANGLE BAVR AND E (EVCT), BUT NOW PANGLE DOES 
C		THIS BETTER
C
C	FIT SHAPE IF ISHFIT = 1
C
	ISHFIT = 1
	ISHFIT = 0
C	PHIE = X(1)		! ANGLE FROM X ANTENNA TO E, GSE, NOT S/C
C	VEL = X(2)
C	WIDTH = X(3)
C	AMP = X(4)
C	SHIM = X(5)
C
	IF(ISHFIT.EQ.1) THEN
	  X(5) = (MID-1024)/SPS
	  MID = .5*(NPKP+NPKN)			! HALFWAY BETWEEN Y PEAKS
	  T0 = (MID-1024)/SPS
	  X(6) = (MID-1024)/SPS
	  XLAG1 = NLAG1
	  XLAG2 = NLAG2
	  X(1) = ATAN2D(XLAG2*LEFFX,XLAG1*LEFFY)
	  X(2) = VX
	  X(3) = 707.*IABS(NPKP-NPKN)*ABS(VX)/SPS
	  X(4) = .5*(APK2P - APK2N) 
C	  x(3) = 2.*x(3)
	print*,'x',(x(i),i=1,6)
C	  DO N = 1,5
C	    X(N+5) = X(N)
C	  ENDDO
	  CALL SHAPERR(X,SUMSQ)
	  CALL SPIKEFIT(X,SUMSQ)
	ENDIF
C
	WRITE(70,1011) sceti4,NO_EVT,ERTDAY,EVENT(1:1),APK1P,NWPK1P,
     1  APK1N,NWPK1N,APK2P,NWPK2P,APK2N,NWPK2N,NLAG1,NLAG2,SUNCLOCK,
     1  PANGLE,ZANGLE,FCHANGE,ANGDD,EPT
C	at present 1011 is 127 characters
 1011	format(I10,I7,I10,I3,A2,F7.2,I4,F8.2,I4,F9.2,I4,F8.2,I4,I6,I5,
     1   I5,F6.0,F6.0,F6.2,F6.1,F6.3)
C
	ELSE
C
	WRITE(71,1012) sceti4,NO_EVT,ERTDAY,EVENT(1:1),APK1P,NWPK1P,
     1  APK1N,NWPK1N,APK2P,NWPK2P,APK2N,NWPK2N,SUNCLOCK,
     1  MAJOR,MINOR,DPUCLK
 1012	format(I10,I7,I10,I3,A2,F7.2,I4,F8.2,I4,F9.2,I4,F8.2,I4,I7,
     1   I8,I5,F11.0)
C
	ENDIF

C
C
	IF(OK.NE.82) GO TO 100
	CLOSE(UNIT=90)
	return
	end
	SUBROUTINE FINDPEAK(N1,N2,VDATA,NPK1,APK1,NPK2,APK2)
C
C	NPK1,APK1 ARE SAMPLE NO AND AMPLITUDE OF POSITIVE PEAK, 2 IS NEG.
C
	REAL VDATA(2048)
	REAL X(25)
	DATA SPS /120000./
C
C	FINDS AMPLITUDE OF LARGEST PEAKS BETWEEN N1 AND N2
C
C	  FIND MAXIMA
C
	SMAX = VDATA(N1)
	SMIN = VDATA(N1)
	NPK1 = N1
	DO N = N1+1,N2
	    IF(VDATA(N).GT.SMAX) THEN
	        SMAX = VDATA(N)
	        NPK1 = N
C	       PRINT*,'in findpeak MAX:N,F,SMAX',NPK,(NPK-1)*SPS/2048.,SMAX
	    ENDIF
	ENDDO
	APK1 = SMAX
C
	SMAX = VDATA(N1)
	SMIN = VDATA(N1)
	NPK2 = N1
	DO N = N1+1,N2
	    IF(VDATA(N).LT.SMIN) THEN
	        SMIN = VDATA(N)
	        NPK2 = N
C	       PRINT*,'in findpeak MAX:N,F,SMAX',NPK,(NPK-1)*SPS/2048.,SMAX
	    ENDIF
	ENDDO
	APK2 = SMIN
C
	RETURN
	END
	SUBROUTINE PEAKWIDTH(N1,N2,VDATA,NPK1,WPK1,NPK2,WPK2,IGB)
C
	REAL VDATA(2048)
	REAL X(25)
	DATA SPS /120000./
C
	IGBP = 0
	IGBN = 0
	APK1 = VDATA(NPK1)  
	NWU = NPK1
	DO N = NPK1+1,N2
	  IF(VDATA(N).LT..5*APK1) GO TO 20
	  NWU = N
	ENDDO	  
C	CHECK THAT IT IS REALLY AN ISOLATED PEAK, RETURN IGBP=1 IF IT'S NOT
 20	DO N = NWU+1,N2+5
	  IF(VDATA(N).GE..5*APK1) THEN
	     IGBP = 1
	     GO TO 25
	  ENDIF
	ENDDO	
C
 25 	NWL = NPK1
	DO N =NPK1-1,N1,-1
	  IF(VDATA(N).LT..5*APK1) GO TO 30
	  NWL = N
	ENDDO	  
C	CHECK THAT IT IS REALLY AN ISOLATED PEAK
 30	DO N = NWL-1,N1-5,-1
	  IF(VDATA(N).GE..5*APK1) THEN
		IGBP = 1
		GO TO 35
	  ENDIF
	ENDDO
 35 	WPK1 = NWU-NWL+1
C
C	DO NEGATIVE PEAK
C
	APK2 = VDATA(NPK2)  
	NWU = NPK2
	DO N = NPK2+1,N2
	  IF(VDATA(N).GT..5*APK2) GO TO 40
	  NWU = N
	ENDDO	  
C	CHECK
 40	DO N = NWU+1,N2+5
	  IF(VDATA(N).LE..5*APK2) THEN
		IGBN = 1
		GO TO 45
	  ENDIF
	ENDDO
 45	NWL = NPK2
	  DO N =NPK2-1,N1,-1
	    IF(VDATA(N).GT..5*APK2) GO TO 50 
	    NWL = N
	  ENDDO	  
 50	DO N = NWL+1,N2+5
	  IF(VDATA(N).LE..5*APK2) THEN
		IGBN = 1
	 	GO TO 55
	  ENDIF
	ENDDO
 55	WPK2 = NWU-NWL+1
	IGB = 1
 	IF(IGBP.EQ.0.OR.IGBN.EQ.0) IGB = 0
C	print*,'IGB',IGBP,IGBN 
C	print*,'widths',wpk1,wpk2  
	RETURN
	END
	SUBROUTINE EEVAL(N1,N2,V1IN,V2IN,EVAL,EVEC,ELLIP,XYANG)
C
C
	REAL EVAL(2),EVEC(2,2),LEFFX,LEFFY,V1IN(1),V2IN(1)
	REAL V1DATA(2048),V2DATA(2048)
	DATA LEFFX,LEFFY/ 41.1,3.79/
C
C	ALGEBRA IN WIND N.B. VOL 4 P 101
C
	IF(N1.GT.N2) THEN
	   N2T = N1
	   N1T = N2
	ELSE
	   N1T = N1
	   N2T = N2
	ENDIF
C
	SXX = 0.
	SYY = 0.
	SXY = 0.
	SXY2 = 0.
C
C	REMOVE AVERAGES
C
	V1AVR = 0.
	V2AV2 = 0.
	COUNT = 1.E-10
	DO N = N1T,N2T
	  V1AVR = V1AVR + V1IN(N)	
	  V2AVR = V2AVR + V2IN(N)	
	  COUNT = COUNT + 1.
	ENDDO
	V1AVR = V1AVR/COUNT
	V2AVR = V2AVR/COUNT
C
	DO N = N1T,N2T
	  V1DATA(N) = (V1IN(N) - V1AVR)/LEFFX 
	  V2DATA(N) = (V2IN(N) - V2AVR)/LEFFY 
	  SXX = SXX + V1DATA(N)**2
	  SYY = SYY + V2DATA(N)**2
	  SXY = SXY + V1DATA(N)*V2DATA(N)
	ENDDO
	IF(SXX.EQ.0..OR.SYY.EQ.0.) RETURN	! MISSING DATA
	BB = (SXX + SYY)
	DISC = BB**2 - 4.*(SXX*SYY - SXY**2)
	EVAL(1) = .5*(BB + SQRT(DISC))
	EVAL(2) = .5*(BB - SQRT(DISC))
C	FIRST INDEX IS 1 FOR EVAL(1), 2 FOR EVAL(2)
	EVEC(1,1) = 1.
	EVEC(1,2) = -(SXX-EVAL(1))/SXY
	VNORM = EVEC(1,1)**2 + EVEC(1,2)**2
	SNORM = SQRT(VNORM)
	EVEC(1,1) = EVEC(1,1)/SNORM
	EVEC(1,2) = EVEC(1,2)/SNORM
C
	EVEC(2,1) = 1.
	EVEC(2,2) = -(SXX-EVAL(2))/SXY
	VNORM = EVEC(2,1)**2 + EVEC(2,2)**2
	SNORM = SQRT(VNORM)
	EVEC(2,1) = EVEC(2,1)/SNORM
	EVEC(2,2) = EVEC(2,2)/SNORM
C
	ELLIP = SQRT(EVAL(2)/EVAL(1))
C
C	ANGLE BETWEEN E AND X ANTENNA
C
	XYANG = ATAN2D(EVEC(1,2),EVEC(1,1))
	IF(XYANG.GT.90.) XYANG = XYANG - 180.
	IF(XYANG.LE.-90.) XYANG = XYANG + 180.
	RETURN
	END
	SUBROUTINE CLAYER(CH,SCET,NSAMP,NLAYER,BDATA,DELB,BAVR)
C
C	GETS NSAMP VALUES OF B AROUND THE SPIKE AND CALCULATES THE 
C	CHANGE IN B (DELB)  AND BAVR
C	AT PRESENT, USES THE TWO VALUES OF B ON EACH SIDE OF THE SPIKE.
C
	REAL*8 SCET,SCETB(21),SCETSET
	REAL*4 BX(21),BY(21),BZ(21),DEL(3),BAVR(3),DELB(3),BDATA(20,3)
	CHARACTER*32 ITEM
	CHARACTER*4 EVENT
	INTEGER*4 CH,OK
	DATA IOPEN /0/
	DATA EVENT /'CDF'/	
C
 	ok = w_event(ch,EVENT)
	N2 = NSAMP/2
	SCETSET = SCET - N2*.77D00/1440.D00
	CALL W_CHANNEL_POSITION(CH,SCETSET)
	ITEM = 'WIND_MFI_BX(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BX, NSAMP, ret_size)
	ITEM = 'WIND_MFI_BY(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BY, NSAMP, ret_size)
	ITEM = 'WIND_MFI_BZ(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BZ, NSAMP, ret_size)
	ITEM = 'WIND_MFI_SCET_R8'
	ok = W_ITEM_R8(ch, item, SCETB, NSAMP, ret_size)
C
	DO N = 1,NSAMP
	  BDATA(N,1) = BX(N)
	  BDATA(N,2) = BY(N)
	  BDATA(N,3) = BZ(N)
	ENDDO
C
c	DO N = 1,NSAMP
c	  WRITE(54,*) SCETB(N),BX(N),BY(N),BZ(N)
c	ENDDO	
C
	DO N = 1,NSAMP-1
	  IF(SCETB(N+1).GT.SCET.AND.SCETB(N).LE.SCET) THEN
	    DELB(1) = BX(N+1) - BX(N)
	    BAVR(1) = .5*(BX(N+1) + BX(N))
	    DELB(2) = BY(N+1) - BY(N)
	    BAVR(2) = .5*(BY(N+1) + BY(N))
	    DELB(3) = BZ(N+1) - BZ(N)
	    BAVR(3) = .5*(BZ(N+1) + BZ(N))
	    NLAYER = N
	    RETURN
	  ENDIF
	ENDDO
	RETURN
	END
	SUBROUTINE SPIKEFIT(X,SUMSQ)
C
C	FITS "SHAPE" TO DATA.  It varies the angle of approach, the width,
C		amplitude and offset and the velocity to fit the data
C
	EXTERNAL SHAPERR
        COMMON /HNTBLK/ NHUNT(25)
	COMMON/FITDATA/N1,N2,V1DATA(2048),V2DATA(2048),IPRNT,NLAG1,NLAG2,
     1		NVAR
	REAL X(25),Y(25),DX(25),DF(25),D2F(12,12),DXS(25)
C
C	PHIE = X(1)		! ANGLE FROM X ANTENNA TO E, GSE, NOT S/C
C	VEL = X(2)
C	WIDTH = X(3)
C	AMP = X(4)
C	SHIM = X(5)
C	SHIM2 = X(6)
C
	IPRNT = 0
C
C	DETERMINE X(4) = AMP, ROUGHLY ONLY
C
	AMPX = 0.
	AMPY = 0.
	DO NT = N1,N2
	  AMPX = AMAX1(ABS(V1DATA(NT)),AMPX)
	  AMPY = AMAX1(ABS(V2DATA(NT)),AMPY)
	ENDDO
	X(4) = AMAX1(AMPX,AMPY)
C
	NVA = 6
	DO N = 1,NVA
	  DX(N) = .1*X(N)
	ENDDO
	DX(1) = 15.
C
C	ADJUST AMPLITUDE = X(4)
C
	NHUNT(1) = 0
	NHUNT(2) = 0
	NHUNT(3) = 0
	NHUNT(5) = 0
	NHUNT(6) = 0
	CALL HUNTMN( NVA, X, DX, Y, SHAPERR, SUMSQ)
	print*,'amp adjust, sumsq=',sumsq
	CALL HUNTMN( NVA, Y, DX, X, SHAPERR, SUMSQ)
	print*,'amp adjust, sumsq=',sumsq
C
	IMINIM = 1		! 0 IS USE HUNTMN, FINMIN, 1 IS USE AMOEBA
C
	IF(IMINIM.EQ.0) THEN
C
C	  ADJUST AMPLITUDE AND ANGLE TOGETHER
C
	  NHUNT(1) = 1
	  CALL FINMIN( NVA, X, DX, Y, SHAPERR, SUMSQ)
	  PRINT 101,(Y(I),I=1,5), SUMSQ 
 101	  FORMAT(2F7.1,4E13.4,F9.4)
	  CALL FINMIN( NVA, Y, DX, X, SHAPERR, SUMSQ) 
	  PRINT 101,(X(I),I=1,5), SUMSQ 
C
C	  DO ALL
C
 	  NHUNT(2) = 1
	  NHUNT(3) = 1
	  NHUNT(5) = 1
	  NHUNT(6) = 1
	  SUMSQSV = SUMSQ
	  DO IT = 1,15
C	    CALL FINMIN( NVA, X, DX, Y, SHAPERR, SUMSQ)
C	    PRINT 101,(X(I),I=1,5), SUMSQ 
C	    CALL FINMIN( NVA, Y, DX, X, SHAPERR, SUMSQ) 
C	    PRINT 101,(X(I),I=1,5), SUMSQ 
	    CALL HUNTMN( NVA, X, DX, Y, SHAPERR, SUMSQ)
	    print*,'ALL adjust, sumsq=',sumsq
	    CALL HUNTMN( NVA, Y, DX, X, SHAPERR, SUMSQ)
	    print*,'ALL adjust, sumsq=',sumsq
	    PRINT 101,(X(I),I=1,5), SUMSQ 
	    IF(SUMSQ.GT..999*SUMSQSV) THEN
	      IPRNT = 1
	      CALL SHAPERR(X,SUMSQ)
	      RETURN
	    ENDIF
	    SUMSQSV = SUMSQ
	  ENDDO
	ELSE
	  NDIM = 6
	  DO M = 1,NDIM
	      D2F(1,M) = X(M)
	      DX(M) = .2*X(M)
	      D2F(1,M+NDIM) = X(M)
	      DX(M+NDIM) = .2*X(M)
	  ENDDO
	  PRINT 101,(X(I),I=1,6)
	  Y(1) = ASHAPE(X)
C
C	DETERMINE RELATIVE AMPLITUDES FOR LARGE WIDTH APPROX
C
	IF(NLAG1.LT.0) AMPX = -AMPX
	IF(NLAG2.LT.0) AMPY = -AMPY
	D2F(2,1) = ATAN2D(AMPY,AMPX)
	print*,'large width angle',d2f(2,1)
	D2F(2,2) = 1.3*X(2)
	D2F(2,3) = 1.5*X(3)
	D2F(2,4) = X(4)
	D2F(2,5) = X(5)
	D2F(2,6) = X(6)
	DO N = 1,NDIM
	  DF(N) = D2F(2,N)
	ENDDO
	Y(2) = ASHAPE(DF)
C
C	QUICK AND DIRTY for rest
	  DO N = 2,NDIM
	    DO M = 1,NDIM
	      D2F(N+1,M) = X(M)
	    ENDDO
	    D2F(N+1,N) = D2F(N+1,N) + DX(N)
	    DO M = 1,NDIM
	      DF(M) = D2F(N+1,M)
	    ENDDO
	    Y(N+1) = ASHAPE(DF)
	  ENDDO
C
	  FRTOL = 1.E-6
C
	  PRINT*,'INITIAL FOR AMOEBA'
	  DO N = 1,NDIM+1
	    PRINT 101,(D2F(N,M),M=1,NDIM),Y(N)
	  ENDDO
C
	  NVAR = 6
	  CALL AMOEBA(D2F,Y,12,12,NVAR,FRTOL,ASHAPE,ITER)
	  DO N = 1,NDIM+1
	    PRINT 101,(D2F(N,M),M=1,NDIM),Y(N)
	  ENDDO
	  PRINT*,'NVAR =',NVAR,' ITERATIONS',ITER
C
C	FIND THE BEST OF THE FIRST FUNCTION FIT
C
	  BESTERR = Y(1)
	  NBEST = 1
	  DO N = 1,NVAR+1
		print*,'check',n,y(n)
	   IF(Y(N).LT.BESTERR) THEN
	     BESTERR = Y(N)
	     NBEST = N
	   ENDIF
	  ENDDO	  
	  DO N = 1,NVAR
	    X(N) = D2F(NBEST,N)
	    X(N+NVAR) = D2F(NBEST,N)
	  ENDDO
	  X(9) = .1*X(4)
C
	  NDIM = NVAR
C
	  NDIM = NVAR
	  CALL AMOEBA(D2F,Y,12,12,NVAR,FRTOL,ASHAPE,ITER)
	  DO N = 1,NVAR+1
	    PRINT 101,(D2F(N,M),M=1,NVAR),Y(N)
	  ENDDO
	  PRINT*,'NVAR =',NVAR,' ITERATIONS',ITER
C
C	REDO WITH RANDOM CHANGES
C
	  ISEED = 759391
	  DO N = 1,NVAR
	    DO M = 1,NVAR
	      D2F(N+1,M) = (.5 + RAN(ISEED))*D2F(N+1,M)
	    ENDDO
	  ENDDO
C
C	  DO N = 1,NVAR+1
	  DO N = 1,NVAR
	    DO M = 1,NVAR
	      DF(M) = D2F(N+1,M)
	    ENDDO
	    Y(N+1) = ASHAPE(DF)
	print*,'random',n,y(n),n+1,y(n+1)
	  ENDDO
C
	  CALL AMOEBA(D2F,Y,12,12,NVAR,FRTOL,ASHAPE,ITER)
C
	DO N = 1,NVAR
C	FIND THE BEST
	  BESTERR = Y(1)
	  NBEST = 1
	print*,'check',y(n)
	   IF(Y(N).LT.BESTERR) THEN
	     BESTERR = Y(N)
	     NBEST = N
	   ENDIF
	ENDDO	  
	  DO N = 1,NVAR
	    X(N) = D2F(NBEST,N)
	  ENDDO
	  IPRNT = 1
	  PRINT*,'FINAL'
	  PRINT 101,(x(n),n=1,nvar)
	  CALL SHAPERR(X,SUMSQ)
	  IPRNT = 0
	ENDIF
	  PRINT*,'RANDOM, NVAR =',NVAR,' ITERATIONS',ITER
C
	RETURN
	END
	SUBROUTINE SPIKEFITOLD(X,SUMSQ)
C
C	FITS "SHAPE" TO DATA.  It varies the angle of approach, the width,
C		amplitude and offset and the velocity to fit the data
C
	EXTERNAL SHAPERR
        COMMON /HNTBLK/ NHUNT(25)
	COMMON/FITDATA/N1,N2,V1DATA(2048),V2DATA(2048),IPRNT,NLAG1,NLAG2,
     1		NVAR
	REAL X(25),Y(25),DX(25),DF(25),D2F(12,12),DXS(25)
C
C	PHIE = X(1)		! ANGLE FROM X ANTENNA TO E, S/C SYSTEM
C	VEL = X(2)
C	WIDTH = X(3)
C	AMP = X(4)
C	SHIM = X(5)
C
	IPRNT = 0
C
C	DETERMINE X(4) = AMP, ROUGHLY ONLY
C
	AMPX = 0.
	AMPY = 0.
	DO NT = N1,N2
	  AMPX = AMAX1(ABS(V1DATA(NT)),AMPX)
	  AMPY = AMAX1(ABS(V2DATA(NT)),AMPY)
	ENDDO
	X(4) = AMAX1(AMPX,AMPY)
C
	NVA = 5
	DO N = 1,NVA
	  DX(N) = .1*X(N)
	ENDDO
	DX(1) = 15.
C
C	ADJUST AMPLITUDE = X(4)
C
	NHUNT(1) = 0
	NHUNT(2) = 0
	NHUNT(3) = 0
	NHUNT(5) = 0
	NHUNT(6) = 0
	CALL HUNTMN( NVA, X, DX, Y, SHAPERR, SUMSQ)
	print*,'amp adjust, sumsq=',sumsq
	CALL HUNTMN( NVA, Y, DX, X, SHAPERR, SUMSQ)
	print*,'amp adjust, sumsq=',sumsq
C
	IMINIM = 1		! 0 IS USE HUNTMN, FINMIN, 1 IS USE AMOEBA
C
	IF(IMINIM.EQ.0) THEN
C
C	  ADJUST AMPLITUDE AND ANGLE TOGETHER
C
	  NHUNT(1) = 1
	  CALL FINMIN( NVA, X, DX, Y, SHAPERR, SUMSQ)
	  PRINT 101,(Y(I),I=1,5), SUMSQ 
 101	  FORMAT(2F7.1,3E13.4,F9.4)
	  CALL FINMIN( NVA, Y, DX, X, SHAPERR, SUMSQ) 
	  PRINT 101,(X(I),I=1,5), SUMSQ 
C
C	  DO ALL
C
 	  NHUNT(2) = 1
	  NHUNT(3) = 1
	  NHUNT(5) = 1
	  SUMSQSV = SUMSQ
	  DO IT = 1,15
C	    CALL FINMIN( NVA, X, DX, Y, SHAPERR, SUMSQ)
C	    PRINT 101,(X(I),I=1,5), SUMSQ 
C	    CALL FINMIN( NVA, Y, DX, X, SHAPERR, SUMSQ) 
C	    PRINT 101,(X(I),I=1,5), SUMSQ 
	    CALL HUNTMN( NVA, X, DX, Y, SHAPERR, SUMSQ)
	    print*,'ALL adjust, sumsq=',sumsq
	    CALL HUNTMN( NVA, Y, DX, X, SHAPERR, SUMSQ)
	    print*,'ALL adjust, sumsq=',sumsq
	    PRINT 101,(X(I),I=1,5), SUMSQ 
	    IF(SUMSQ.GT..999*SUMSQSV) THEN
	      IPRNT = 1
	      CALL SHAPERR(X,SUMSQ)
	      RETURN
	    ENDIF
	    SUMSQSV = SUMSQ
	  ENDDO
	ELSE
	  NDIM = 5
	  DO M = 1,NDIM
	      D2F(1,M) = X(M)
	      DX(M) = .2*X(M)
	      D2F(1,M+5) = X(M)
	      DX(M+5) = .2*X(M)
	  ENDDO
	  PRINT 101,(X(I),I=1,5)
	  Y(1) = ASHAPE(X)
C
C	DETERMINE RELATIVE AMPLITUDES FOR LARGE WIDTH APPROX
C
	IF(NLAG1.LT.0) AMPX = -AMPX
	IF(NLAG2.LT.0) AMPY = -AMPY
	D2F(2,1) = ATAN2D(AMPY,AMPX)
	print*,'large width angle',d2f(2,1)
	D2F(2,2) = 1.3*X(2)
	D2F(2,3) = 1.5*X(3)
	D2F(2,4) = X(4)
	D2F(2,5) = X(5)
	DO N = 1,NDIM
	  DF(N) = D2F(2,N)
	ENDDO
	Y(2) = ASHAPE(DF)
C
C	QUICK AND DIRTY for rest
	  DO N = 2,NDIM
	    DO M = 1,NDIM
	      D2F(N+1,M) = X(M)
	    ENDDO
	    D2F(N+1,N) = D2F(N+1,N) + DX(N)
	    DO M = 1,NDIM
	      DF(M) = D2F(N+1,M)
	    ENDDO
	    Y(N+1) = ASHAPE(DF)
	  ENDDO
C
	  FRTOL = 1.E-6
C
	  PRINT*,'INITIAL FOR AMOEBA'
	  DO N = 1,NDIM+1
	    PRINT 101,(D2F(N,M),M=1,NDIM),Y(N)
	  ENDDO
C
	  NVAR = 5
	  CALL AMOEBA(D2F,Y,12,12,NVAR,FRTOL,ASHAPE,ITER)
	  DO N = 1,NDIM+1
	    PRINT 101,(D2F(N,M),M=1,NDIM),Y(N)
	  ENDDO
	  PRINT*,'NVAR = 5 ITERATIONS',ITER
C
C	  INCLUDE SECOND FUNCTION IN SHAPE
C
C	FIND THE BEST OF THE FIRST FUNCTION FIT
	  BESTERR = Y(1)
	  NBEST = 1
	  DO N = 1,NVAR+1
	print*,'check',n,y(n)
	   IF(Y(N).LT.BESTERR) THEN
	     BESTERR = Y(N)
	     NBEST = N
	   ENDIF
	  ENDDO	  
	  DO N = 1,NVAR
	    X(N) = D2F(NBEST,N)
	    X(N+NVAR) = D2F(NBEST,N)
	  ENDDO
	  X(9) = .1*X(4)
C
	  NDIM = NVAR
	  NVAR = 10
C
C	THINK OF DF AS RECTANGLE.  N IS HORIZONTAL, M VERTICAL, 
C	UPPER LEFT IS FILLED IN ABOVE.  
C	FILL IN LOWER LEFT  (N = 1,NVAR, M = NVAR+,NDIM)
C
	  DO N = 1,NDIM
	    DO M = NDIM+1,NVAR
	      D2F(N+1,M) = X(M)
	    ENDDO
	  ENDDO
C	FILL IN RIGHT HALF
	  DO N = NDIM+1,NVAR+1
	    DO M = 1,NVAR
	      D2F(N+1,M) = X(M)
	    ENDDO
	    DX(N) = .2*X(N)
	    IF(N.EQ.9) DX(N) = -2.*X(N)
C	print*,'check dx',n,x(n),dx(n)
	    D2F(N+1,N) = D2F(N+1,N) + DX(N)
	  ENDDO
C
C	PUT IN BIG T0
C
	  TT = 2.*X(3)/(1000.*X(2))
	print*,'add time',tt
	  D2F(7,10) = D2F(7,10) + TT
	  D2F(8,10) = D2F(7,10) + 1.5*TT
	  D2F(9,10) = D2F(9,10) - TT
	  D2F(10,10) = D2F(10,10) - 1.5*TT
C
C	PUT IN SOME BIG DL WIDTHS
C
C	  D2F(3,8) = 10.*D2F(7,3) 
C	  D2F(7,8) = 10.*D2F(7,3) 
C	  D2F(8,8) = 10.*D2F(7,3) 
C
	  D2F(3,8) = 200.
	  D2F(4,8) = 300.
	  D2F(7,8) = 250.
	  D2F(8,8) = 400.
C
	  DO N = NDIM+1,NVAR+1
	    DO M = 1,NVAR
	      DF(M) = D2F(N+1,M)
	    ENDDO
	print*,'calling ashape n =',n
	    Y(N+1) = ASHAPE(DF)
c	print*,(df(m),m=1,nvar),y(n+1)
	print*,y(n+1)
	  ENDDO
C
	  NDIM = NVAR
	  CALL AMOEBA(D2F,Y,12,12,NVAR,FRTOL,ASHAPE,ITER)
	  DO N = 1,NVAR+1
	    PRINT 101,(D2F(N,M),M=1,NVAR),Y(N)
	  ENDDO
	  PRINT*,'NVAR = 10 ITERATIONS',ITER
C
C	REDO WITH RANDOM CHANGES
C
	  ISEED = 759391
	  DO N = 1,NVAR
	    DO M = 1,NVAR
	      D2F(N+1,M) = (.5 + RAN(ISEED))*D2F(N+1,M)
	    ENDDO
	  ENDDO
C
	  DO N = 1,NVAR+1
	    DO M = 1,NVAR
	      DF(M) = D2F(N+1,M)
	    ENDDO
	    Y(N+1) = ASHAPE(DF)
	print*,'random',n,y(n),n+1,y(n+1)
	  ENDDO
C
	  CALL AMOEBA(D2F,Y,12,12,NVAR,FRTOL,ASHAPE,ITER)
C
	DO N = 1,NVAR
C	FIND THE BEST
	  BESTERR = Y(1)
	  NBEST = 1
	print*,'check',y(n)
	   IF(Y(N).LT.BESTERR) THEN
	     BESTERR = Y(N)
	     NBEST = N
	   ENDIF
	  ENDDO	  
	  DO N = 1,NVAR
	    X(N) = D2F(NBEST,N)
	  ENDDO
	  IPRNT = 1
	  PRINT*,'FINAL'
	  PRINT 101,(x(n),n=1,nvar)
	  CALL SHAPERR(X,SUMSQ)
	  IPRNT = 0
	ENDIF
	  PRINT*,'RANDOM, NVAR = 10 ITERATIONS',ITER
C
	RETURN
	END
	FUNCTION ASHAPE(X)
C
	DIMENSION X(25)
C
	CALL SHAPERR(X,SUMSQ)
	ASHAPE = SUMSQ
	RETURN
	END
	SUBROUTINE SHAPERR(X,SUMSQ)
C
C	CALCULATES POTENTIAL WAVEFORM =  AMP * EXP (-(X/WIDTH)**2)
C		WIDTH IS IN METERS.
C	
	COMMON/FITDATA/N1,N2,V1DATA(2048),V2DATA(2048),IPRNT,NLAG1,NLAG2,
     1	  NVAR
	REAL X(25),R(8),RSC(8),V1E(2048),V2E(2048)
	REAL LEFFX,LEFFY,LPHYSX,LPHYSY
C
	DATA LEFFX,LEFFY/ 41.1,3.79/
	DATA SCDIA,LPHYSX,LPHYSY/ 2.8, 50., 7.5/
C
C	VARIABLES ARE PHI, VELOCITY, WIDTH, AMPLITUDE, AND 
C		SHIM ON INITIAL POSITION. 
C	        SHIM IS T0, MEASURED FROM SAMPLE 1024
C
	SPS = 120000.
C
C	FIT TDS POINTS FROM N1 TO N2
C
	SUMSQ = 0.
	PHIE = X(1)
	VEL = X(2)		! KM/SEC
	WIDTH = X(3)		! METERS
	AMP = X(4)
C	T0 = X(5)		! SEC
C	PHIDL = X(6)
C	VELDL = X(7)
C	WIDTHDL = X(8)
C	AMPDL = X(9)
C	TDL = X(10)
	if(nvar.eq.10) print*,'dl',(x(i),i=1,10)
C
C	R'S AND RSC'S ARE THE X COORDINATES OF THE ANTENNA ENDS
C	R(1) = TIP OF X+,  R(2) = BASE OF X+
C	R(3) = TIP OF X-,  R(4) = BASE OF X-
C	R(5) = TIP OF Y+, ETC.
C
C
C	DETERMINE RELATIVE AMPLITUDES FOR WEIGHTING
C
	AMPX = 0.
	AMPY = 0.
	DO NT = N1,N2
	  V1E(NT) = V1DATA(NT)*LPHYSX/LEFFX
	  V2E(NT) = V2DATA(NT)*LPHYSY/LEFFY
	  AMPX = AMAX1(ABS(V1E(NT)),AMPX)
	  AMPY = AMAX1(ABS(V2E(NT)),AMPY)
	ENDDO
	DO NT = N1,N2
C
C	CALCULATE POSITIONS OF THE 8 ENDS OF THE ANTENNAS IN SYSTEM WITH
C		ORIGIN AT S/C CENTER BUT X PARALLEL TO E OF SPIKE SYSTEM
C
	  WIDTH = X(3)		! METERS
	  R(1) = (.5*SCDIA + LPHYSX)*COSD(PHIE)		!  X ANTENNA
	  R(2) = (.5*SCDIA)*COSD(PHIE)
	  R(3) = -R(1)
	  R(4) = -R(2) 
	  R(5) = (.5*SCDIA + LPHYSY)*SIND(PHIE)		!  Y ANTENNA
	  R(6) = (.5*SCDIA)*SIND(PHIE)
	  R(7) = -R(5)
	  R(8) = -R(6)
C
C	CALCULATE POSITIONS OF THE 8 ENDS IN THE SPIKE SYSTEM AT TIME T.  
C		INITIAL T0 IS HALFWAY BETWEEN PEAKS OF Y SIGNAL AND IS
C		MEASURED FROM POINT 1024
C
C	  DIST = 1000.*VEL*((NT - 1024)/SPS - T0)
	  DIST = 1000.*VEL*((NT - 1024)/SPS - X(5))
	  DO NA = 1,4
	    R(NA) = R(NA) + DIST
	  ENDDO
	  DIST = 1000.*VEL*((NT - 1024)/SPS - X(6))
	  DO NA = 5,8
	    R(NA) = R(NA) + DIST
	  ENDDO
	  OFFSET = 0.
C
C	  CALCULATE THE FOUR ANTENNA POTENTIALS
C
	  OFFSET = X(5)
	  XPLUS = -AMP*(HSHAPE(R(1),WIDTH,OFFSET)-HSHAPE(R(2),WIDTH,OFFSET))
	  XPLUS = XPLUS/(R(1)-R(2))
	  XMINUS = -AMP*(HSHAPE(R(3),WIDTH,OFFSET)-HSHAPE(R(4),WIDTH,OFFSET))
	  XMINUS = XMINUS/(R(3)-R(4))
	  OFFSET = X(6)
	  YPLUS = -AMP*(HSHAPE(R(5),WIDTH,OFFSET)-HSHAPE(R(6),WIDTH,OFFSET))
	  YPLUS = YPLUS/(R(5)-R(6))
	  YMINUS = -AMP*(HSHAPE(R(7),WIDTH,OFFSET)-HSHAPE(R(8),WIDTH,OFFSET))
	  YMINUS = YMINUS/(R(7)-R(8))
C
C	  CALCULATE SUM OF ERRORS SQUARED
C
	  SUMSQ = SUMSQ + ((V1E(NT)-XPLUS+XMINUS)/AMPX)**2
	  SUMSQ = SUMSQ + ((V2E(NT)-YPLUS+YMINUS)/AMPY)**2
	  IF(IPRNT.NE.0) 
     1	  write(65,*) nt,v1E(nt),xplus-xminus,v2E(nt),yplus-yminus
	ENDDO
	SUMSQ = SUMSQ/(N2-N1+1)
C	print*,'sumsq',sumsq
	RETURN
	END
	FUNCTION HSHAPE(X,WIDTH,OFFSET)
C
C	THE WAVEFORM IS TAKEN TO BE THE DERIVATIVE OF THE GAUSSIAN
C		EXP(-(X/WIDTH)**2)	I.E. -2.*X/WIDTH**2 * EXP
C	THE PEAKS ARE THEN AT ( Y = X/WIDTH)  Y**2 = .5
C	THE POTENTIAL IS THE INTEGRAL OF THE WAVEFORM, HENCE THE GAUSSIAN
C
	Y = (X-OFFSET)/WIDTH
C	HSHAPE = EXP(-Y**2) 
C
C	THE POTENTIAL IS TAKEN FROM WANG AND SONNERUP, PHYS.FL 27, 1460, 
C	(1984), EQ 28.  BUT NOW I ASSUME THAT THE INPUT VOLTAGE IS THE
C	AVERAGE POTENTIAL, AND THE INTEGRAL OF 1/COSH**2 =SECH*2 IS TANH
C
C	HSHAPE = 1./COSH(Y)**2
	HSHAPE = TANH(Y)*WIDTH
C	
	RETURN
	END
	FUNCTION DLSHAPE(X,WIDTH,OFFSET)
C
C	THE WAVEFORM IS THE DERIVATIVE OF THE POTENTIAL.
C	THE POTENTIAL IS THE INTEGRAL OF THE WAVEFORM, AND IS TANH
C
	DLSHAPE = TANH(X/WIDTH)
C
	RETURN
	END
	SUBROUTINE XFORMVM(N1,N2,NSYS,XINP,XFDATA)
C
C	THIS TRANSFORMS THE ELECTRIC FIELD, XINP,INTO THE VARIANCE MATRIX
C	EIGENSYSTEM, 1 = LARGEST EIGENVALUE.  XFDATA ARE THE TRANS-
C	FORMED FIELDS
C
	INTEGER*4 MAJOR,MINOR,S_SCET(2),NSYS
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	INTEGER*4 SUNCLOCK
	REAL XINP(200,4), XTEMP(200,4),XFDATA(200,4)
C
C	EVECT(I,J) IS THE Ith COMPONENT OF THE Jth EIGENVECTOR
C
C	VMATRIX CALCULATES THE VARIANCE MATRIX OF XGSE, AND RETURNS
C	EIGENVALUES IN COMMON /VARMATX/EVAL, AND EIGENVECTORS IN EVECT
C
C	MAKE A COPY OF THE INPUT MATRIX TO WORK ON
C
	DO J = 1,4
 	  DO N = N1,N2
		XTEMP(N,J) = XINP(N,J)
	  ENDDO
	ENDDO	
C
	CALL VMATRIX(XTEMP,N1,N2)
C	WRITE(79,*) ' '
C	WRITE(79,*) 'XFORMVM CALLED--PUT FIELD DATA INTO VAR MX SYSTEM, NSYS=2'
C
C	NOW EVECT ARE EIGENVECTORS IN THE XINP SYSTEM
C	XFDATA(N,1) CORRESPONDS TO LARGEST EIGENVALUE,N=COMPONENT
C
	DO N = N1,N2
	  XFDATA(N,1) = XTEMP(N,1)*EVECT(1,1) + XTEMP(N,2)*EVECT(2,1) + 
     1		XTEMP(N,3)*EVECT(3,1)
	  XFDATA(N,2) = XTEMP(N,1)*EVECT(1,2) + XTEMP(N,2)*EVECT(2,2) + 
     1		XTEMP(N,3)*EVECT(3,2)
	  XFDATA(N,3) = XTEMP(N,1)*EVECT(1,3) + XTEMP(N,2)*EVECT(2,3) + 
     1		XTEMP(N,3)*EVECT(3,3)
	ENDDO
C
C	CHECK TRANSFORMATION.  IN XTEMP, COMPONENT IS 2ND INDEX, IN
C	EVECT THE FIRST INDEX IS COMPONENT.  T1,T2,T3 ARE THE
C	COMPONENTS IN THE EIGENVECTOR SYSTEM, N IS EIGENVECTOR NO. 
C
C	DO N = 1,3
C	  T1 = EVECT(1,N)*EVECT(1,1) + EVECT(2,N)*EVECT(2,1) + 
C     1		EVECT(3,N)*EVECT(3,1)
C	  T2 = EVECT(1,N)*EVECT(1,2) + EVECT(2,N)*EVECT(2,2) + 
C     1		EVECT(3,N)*EVECT(3,2)
C	  T3 = EVECT(1,N)*EVECT(1,3) + EVECT(2,N)*EVECT(2,3) + 
C     1		EVECT(3,N)*EVECT(3,3)
C	  PRINT*,'CHECK',N,T1,T2,T3
C	ENDDO
C
C	DO N = 1,3
C	  T1 = EVECT(1,N)*EVECT(1,1) + EVECT(2,N)*EVECT(2,1) + 
C     1		EVECT(3,N)*EVECT(3,1)
C	  T2 = EVECT(1,N)*EVECT(1,2) + EVECT(2,N)*EVECT(2,2) + 
C     1		EVECT(3,N)*EVECT(3,2)
C	  T3 = EVECT(1,N)*EVECT(1,3) + EVECT(2,N)*EVECT(2,3) + 
C     1		EVECT(3,N)*EVECT(3,3)
C	  PRINT*,'CHECK',N,T1,T2,T3
C	ENDDO
C
C	WRITE(79,*) 'IN SYSTEM,',NSYS,' EIGENVALUES:'
C	WRITE(79,*)  EVAL
C	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
C	WRITE(79,*)(EVECT(1,I),I=1,3)
C	WRITE(79,*)(EVECT(2,I),I=1,3)
C	WRITE(79,*)(EVECT(3,I),I=1,3)
C
	NSYS = 2
C
	RETURN
	END
	SUBROUTINE VMATRIX(DATA,N1,N2)
C
C	CALCULATES VARIANCE MATRIX AND EIGENVALUES
C
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
	REAL DATA(200,4),DATAS(200,3),VMAT(3,3),WT(200)
	REAL SN(3),CL(3),V3(3),AV(3),STD(3)
	INTEGER*4 SUNCLOCK
	DATA WT /200*1./
	DATA NSYS /-1/
C
C	THIS PROGRAM EXCLUDES GLITCH DATA BY USING WT
C
C	subtract averages
C
C	WRITE(79,*) ' '	
C	WRITE(79,*) 'VARIANCE MATRIX,NPT=',NPT	
C
	DO M = 1,3
	  AV(M) = 0.
	  COUNT = 0.
	  DO N = N1,N2
		AV(M) = AV(M) + WT(N)*DATA(N,M)
		COUNT = COUNT+WT(N)
	  ENDDO
C	PRINT*,'M,AV,COUNT',M,AV(M),COUNT
	  AV(M) = AV(M)/COUNT
	ENDDO
	DO M = 1,3	
	  DO N = N1,N2
		DATAS(N,M) = WT(N)*(DATA(N,M) - AV(M))
	  ENDDO
	ENDDO
C
C	PRINT*,' WITH AVERAGES REMOVED'
C
C	FORM VARIANCE MATRIX
C	
	DO K = 1,3
	  DO M = 1,3
		VMAT(K,M) = 0.
	  ENDDO
	ENDDO
C	
	DO N = N1,N2
	  DO K = 1,3
	    DO M = K,3
		VMAT(K,M) = VMAT(K,M) + DATAS(N,K)*DATAS(N,M)
	    ENDDO
	  ENDDO
	ENDDO	
c	print*,'diag elems',vmat(1,1),vmat(2,2),vmat(3,3),
c     1	' trace',vmat(1,1)+vmat(2,2)+vmat(3,3)
C
C	CALCULATE EIGENVALUES AND MATRIX OF EIGENVECTORS
C
	CALL JACOBI(VMAT,3,3,EVAL,EVECT,NROT)
C	PRINT*,'EIGENVALUES',EVAL
C	PRINT*,'EIGENVECTOR'
C	PRINT*,(EVECT(1,I),I=1,3)
C	PRINT*,(EVECT(2,I),I=1,3)
C	PRINT*,(EVECT(3,I),I=1,3)
C
C	SORT IN DESCENDING ORDER OF EIGENVALUES
C
	CALL EIGSRT(EVAL,EVECT,3,3)
C
C
C 	ENFORCE A RIGHT HANDED SYSTEM BY REVERSING THE SMALLEST
C	EIGENVECTOR IF NECESSARY
C
	ADOTBXC = EVECT(1,1)*EVECT(2,2)*EVECT(3,3)
     1	 - EVECT(1,1)*EVECT(3,2)*EVECT(2,3)
     1	 + EVECT(2,1)*EVECT(1,3)*EVECT(3,2)
     1	 - EVECT(2,1)*EVECT(1,2)*EVECT(3,3)
     1	 + EVECT(3,1)*EVECT(1,2)*EVECT(2,3)
     1	 - EVECT(3,1)*EVECT(2,2)*EVECT(1,3)
C	PRINT*,'DETERMINANT OF EIGENVECTORS',ADOTBXC
	IF(ADOTBXC.LT.0.) THEN        ! REVERSE SMALLEST EVECT FOR RH SYSTEM
	  EVECT(1,3) = -EVECT(1,3)
	  EVECT(2,3) = -EVECT(2,3)
	  EVECT(3,3) = -EVECT(3,3)
C	  PRINT*,'CHANGE TO RIGHT HANDED SYSTEM'
	ENDIF
C
C	PRINT*,'IN SYSTEM',NSYS,'  EIGENVALUES:'
C	PRINT*,EVAL,'eig sum',eval(1)+eval(2)+eval(3)
C	PRINT*,'EIGENVECTORS'
C	PRINT*,(EVECT(1,I),I=1,3)
C	PRINT*,(EVECT(2,I),I=1,3)
C	PRINT*,(EVECT(3,I),I=1,3)
C	WRITE(79,*) 'IN SYSTEM',NSYS,'  EIGENVALUES:'
C	WRITE(79,*)  EVAL
C	WRITE(79,*) 'EIGENVECTORS IN COLUMNS BELOW EIGENVALUES'
C	WRITE(79,*)(EVECT(1,I),I=1,3)
C	WRITE(79,*)(EVECT(2,I),I=1,3)
C	WRITE(79,*)(EVECT(3,I),I=1,3)
	NSYS = 2
	RETURN
	END	
