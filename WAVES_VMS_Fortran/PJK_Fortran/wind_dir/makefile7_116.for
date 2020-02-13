	subroutine makefile(ch)
c
C	A PROGRAM TO SEARCH FOR LION ROARS IN FFTL
c
C	WRITES OUTPUT TO FFTL.REC,FOR037,FOR080,FOR090,WHISTLER.POWER
C		ONLY WHISTLER.POWER IS ACCESS=APPEND
C
	integer*4 ch,ok,okt,SCETI4(2),NDATA(2048,4),SCETST(2)
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	INTEGER*4 FFT_CHANNEL,ISRC,SUNCLOCK
	INTEGER*4 ISOURCE(10),LSOURCE(4)
	REAL*8 SCET8,XGSE,YGSE,ZGSE,ERAD
	REAL Y(3),ERR(1024),GSEK(3)
	REAL TDATA(1026),TSPEC(1026),ZCROSS(1024),ZINT(1024),FREQZC(4)
	REAL DBSPEC(513,4),VDATA(2050,4),GDATA(2050,4),VMDATA(2050,4)
	real avspec(513),stdspec(513),spcount(513)
	REAL FREQ(513),PWR(4),SPPWR(4),FREQHZ(4),BW(4),RDATA(1024,4)
	REAL VPWR(4),WT(2050)
	REAL THRESH(513),BACKGND(513)
	complex preamp
	character*32 ITEM
	character*4 event
	COMMON /WPOWER/ TPOWER(513),TCOUNT(513)
	COMMON /VARMATX/ EVAL(3),EVECT(3,3)
C	COMMON /WEIGHT/ WT(2050)
	common /rawvolts/ DATA(1026)
	common /telem/ tmspec(1024)
	integer*4 tmspec
	DATA TWOPI /6.2831853/
	DATA FUNDFR /.33356/
	DATA ERAD /6378.D00/
	DATA PWR /4*0./
	DATA PWRLIM /.6E-5/
	DATA IBACKRD/0/
	DATA VPWRLIM /.7/
	data event /'FFTL'/
	DATA NCOUNT /0/
	DATA IPOWER /0/
	DATA TPOWER /513*0./
	DATA TCOUNT /513*1.E-8/
C
C	CALCULATE FREQUENCIES FOR DBSPEC
C
	FREQ(1) = 0.
	DO N = 2,513
	  FREQ(N) = (N-1)*FUNDFR	    ! FREQ(N) GOES WITH DBSPECT(N)
C	  TH = -29. - 19.*ALOG10(FREQ(N))
C	  TL = -2. - 52.*ALOG10(FREQ(N))
C	  THRESH(N) = AMAX1(TL,TH)
C	  BACKGND(N) = THRESH(N) - 5.
	ENDDO
C	THRESH(228) = THRESH(228) + 5.
C	THRESH(229) = THRESH(229) + 5.
	NUMEVTS = 0
C
	IF(IPOWER.EQ.0) THEN
	  IPOWER = 1
C
C	  READ IN ACCUMULATED TOTAL POWER
C
	  OPEN(UNIT=46,FILE='WHISTLER.POWER',STATUS='OLD')
	  DO N = 2,513
	    READ(46,*) FF,TCOUNT(N),TPOWER(N)
	  ENDDO
	  CLOSE(UNIT=46)	
	ENDIF
C
	IF(IBACKRD.EQ.0) THEN
	  IBACKRD = 1
	  OPEN(UNIT=45,FILE='FFTL.BCKG',STATUS='OLD',READONLY)
	  DO N = 1,512
	    READ(45,*) NL,CNT,BACKGNDT,STD2T
	    BACKGND(NL) = BACKGNDT
	    STD = SQRT(STD2T)
	    THRESH(NL) = BACKGND(NL) + 1.5*STD
	  ENDDO
	  CLOSE(UNIT=45)
	  PRINT*,NL,CNT,BACKGND(NL),STD2T,THRESH(NL)
	ENDIF
	IFOUND = 0
 100	ok = w_event(ch,event)
C
C	CHECK FOR END OF RECORD
C
	if (ok.ne.1) then
		if(ok.eq.82) then
		   ok = w_channel_close(ch)
		   open(unit=45,file='fftl.rec',status='old',
     1			access='append')
		   write(45,*) sceti4,numevts
		   close(unit=45)
c
C	do n = 1,512
C	  avspec(n) = avspec(n)/spcount(n)
C	  stdspec(n) = stdspec(n)/spcount(n) - avspec(n)**2
C	  np = n
C	  write(72,*) np,spcount(n),avspec(n),stdspec(n),sqrt(stdspec(n))
C	enddo	  
c
		   return
	        endif
		write(6,*) 'cannot open ',event, ', ok=', ok
	endif
C
C	END OF END OF RECORD CHECK
C
	OKT = W_MESSAGES_OFF(ch)
C
C
	ITEM = 'CHANNEL_NUMBER'
	ok = W_ITEM_I4(ch, item, FFT_CHANNEL, 1, ret_size)
	IF(FFT_CHANNEL.LE.6) GO TO 100      		! NOT FFTL
C
	ITEM = 'EVENT_SCET'
	ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
C
C	ITEM = 'CAL_PA'
C	ok = W_ITEM_I4(ch, item, ICAL, 1, ret_size)
C	IF(ICAL.NE.0) THEN
C	  PRINT*,'CALIBRATION'
C	  GO TO 100
C	ENDIF
C
	ITEM = 'SOURCE'
	ok = W_ITEM_I4(ch, item, ISOURCE(FFT_CHANNEL), 1, ret_size)
C
	IF(FFT_CHANNEL.EQ.7) THEN		! START OF A NEW SET
	  ITEM = 'EVENT_SCET'
	  ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
	  IF(ISOURCE(FFT_CHANNEL).GT.6) THEN
	    LD = ISOURCE(FFT_CHANNEL) - 6
	  ELSE
	    LD = 4
	  ENDIF
C
	  BFREQ = 3.
	  TFREQ = 140.
	  CALL FFT_PHYS_BPF(CH,IRAW,TDATA,TSPEC,BFREQ,TFREQ)
	  DO N = 1,1024
	    VDATA(N,LD) = TDATA(N)
	    RDATA(N,LD) = DATA(N)
	  ENDDO
	  DO N = 1,513
	    DBSPEC(N,LD) = TSPEC(N)
	  ENDDO
C	print*,'load channel,source',fft_channel,isource(fft_channel),
C     1		'  to index',ld
C
C	PRINT 722,SCETST,FFT_CHANNEL,ISOURCE(FFT_CHANNEL),LD,IRAW
 722	FORMAT(' EVENT AT',I10,I8,'  CH',I3,'  P/A',I3,'   TO INDEX',
     1		I3,'  IRAW=',I2)
C
	  IF(IRAW.NE.1) THEN
C	    ADVANCE BY ONE HOUR AND RETURN
	    ITEM = 'EVENT_SCET_R8'
	    OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
C	    PRINT*,'NOT RAW DATA',SCETI4
	    SCET8 = SCET8 + 1.D00/24.D00
	    CALL W_CHANNEL_POSITION(CH,SCET8)
	    GO TO 100
	  ENDIF
C
	  DO N = 8,10
		ISOURCE(N) = 0
	  ENDDO
	  DO N = 1,4
	 	LSOURCE(N) = 0
	  ENDDO
	  DO N = 1,2
		SCETST(N) = SCETI4(N)
	  ENDDO
	  LSOURCE(LD) = ISOURCE(FFT_CHANNEL)
	  IFOUND = 1
	  GO TO 100
	ENDIF			! end of setup in first channel
C
	IF(ISOURCE(FFT_CHANNEL).GT.6) THEN
	  LD = ISOURCE(FFT_CHANNEL) - 6
	ELSE
	  LD = 4
	ENDIF
	LSOURCE(LD) = ISOURCE(FFT_CHANNEL)
C
	IF(FFT_CHANNEL.GT.7.AND.IFOUND.EQ.1) THEN
C
	  CALL FFT_PHYS_BPF(CH,IRAW,TDATA,TSPEC,BFREQ,TFREQ)
	  DO N = 1,1024
	    VDATA(N,LD) = TDATA(N)
	    RDATA(N,LD) = DATA(N)
	  ENDDO
	  DO N = 1,513
	    DBSPEC(N,LD) = TSPEC(N)
	  ENDDO
C
C	print*,'load channel,source',fft_channel,isource(fft_channel),
C     1		'  to index',ld
C	  PRINT*,'LD,SAMPLE DATA',LD,VDATA(1,LD),VDATA(2,LD),VDATA(3,LD)
C
	  IF(FFT_CHANNEL.NE.10) GO TO 100
	ENDIF
	NUMEVTS = NUMEVTS+1
C
C	********	LOOK FOR EZ, BZ, TOGETHER
c	IEZBZ = 0
C	DO I = 7,10
c	  IF(ISOURCE(I).EQ.9) IEZBZ = IEZBZ + 1
c	  IF(ISOURCE(I).EQ.6) IEZBZ = IEZBZ + 1
c	ENDDO
c	IF(IEZBZ.GE.2) WRITE(77,*) 'EZBZ AT',SCETST,(isource(i),i=7,10)
C	********
C	  print*,'load check,FFT_CH,SOURCE,INDEX',I,ISOURCE(I),LD
C	ENDDO
C
C	CHECK FOR 3 B CHANNELS
C
	I3E3B = 0
	DO N = 7,10
C	  print*,'3B check, FFT_CHANNEL',n,' FROM SOURCE',ISOURCE(n)
	  IF(ISOURCE(N).GT.6) I3E3B = I3E3B + 1 
	ENDDO
	IF(I3E3B.LT.3) THEN
C		PRINT*,'NOT 3 Bs MODE',SCETST
		GO TO 100
	ENDIF
C
C	CHECK THAT ALL CHANNELS ARE FULL 
C
	DO N = 7,10
	  IF(ISOURCE(N).EQ.0) GO TO 100
	ENDDO
C
c	DO N = 1,1024
c	  WRITE(35,*) N,VDATA(N,1),VDATA(N,2),VDATA(N,3)
c	ENDDO
c	DO N = 1,1024
c	  WRITE(34,*) N,rDATA(N,1),rDATA(N,2),rDATA(N,3)
c	ENDDO
c	IF(1) STOP
C
C	THE FOLLOWING IS SUPPOSED TO PREVENT STARTING AT SOME
C		CHANNEL OTHER THAN THE FIRST
C
	IFOUND = 0
C
C	CALCULATE POWERS FROM SPECTRUM
C
C	DO N = 1,513
C	  WRITE(37,*) N,dbspec(N,1),dbspec(N,2),dbspec(N,3)
C	ENDDO
C
C	FIND ZERO CROSSINGS, AFTER REMOVING AVERAGE
C
	DO LC = 1,2
	  AVR = 0.
	  COUNT = 0.
	  DO IL = 1,1024
	    AVR = AVR+RDATA(IL,LC)
	    COUNT = COUNT+1.
 	  ENDDO
	  IF(COUNT.NE.0.) AVR = AVR/COUNT
	  DO IL = 1,1024
	    RDATA(IL,LC) = RDATA(IL,LC) - AVR
 	  ENDDO
	  IZCNT = 0
	  IL = 1
	  IZ = IL
C	  IF(RDATA(IL,LC).EQ.0) PRINT*,'ZERO DATA',
C     1		IL,RDATA(IL,LC),RDATA(IL+1,LC)
	  DO IL = 2,1023
	    IZ = IL
CT	    IF(RDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,RDATA(IL-1),
CT     1    RDATA(IL),RDATA(IL+1)
C
C		COUNT ONLY POS TO NEG
	    IF(RDATA(IL,LC).GT.0.AND.RDATA(IL+1,LC).LE.0) THEN
	        IZCNT = IZCNT+1
		S1 = RDATA(IL,LC)
		S2 = RDATA(IL+1,LC)
	        DELZ = .5
		IF((S1-S2).NE.0.) DELZ = S1/(S1 - S2)
		IF(DELZ.LE.1.AND.DELZ.GE.0.) THEN
			ZCROSS(IZCNT) = IL + DELZ
		ELSE
			ZCROSS(IZCNT) = IL + .5
		ENDIF
	    ENDIF
	  ENDDO
	  DO N = 1,IZCNT-1
	    ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
	    IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
	    IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
	  ENDDO
	  FREQZC(LC) = FUNDFR*IZCNT
C	  print*,'zero crossings found',izcnt
C	  print*,'first 5',(zcross(kk),kk=1,5)
	ENDDO
	NCOUNT = NCOUNT+1
C	WRITE(37,*) NCOUNT,FREQZC(1),FREQZC(2)
C
C	LIMLO = 9
C	LIMUP = 512
C	DO NC = 1,4
C	  SPPWR(NC) = 0.
C	  AVRF = 0.
C	  SQF = 0.
C	  DO NF = LIMLO,LIMUP
C	    VLT2 =  10.**(.1*DBSPEC(NF,NC))
C	    SPPWR(NC) = SPPWR(NC) + VLT2
C	    AVRF = AVRF + FREQ(NF)*VLT2
C	    SQF = SQF + VLT2*FREQ(NF)**2
C	  ENDDO
C	  FREQHZ(NC) = AVRF/SPPWR(NC)
C	  BW(NC) = SQF/SPPWR(NC) - FREQHZ(NC)**2
C	  BW(NC) = SQRT(AMAX1(BW(NC),0.))
C	  SPPWR(NC) = .5*SPPWR(NC)
C	ENDDO
CTEMP	IF(PWRTOT.LT.PWRLIM) GO TO 100
C
C	HANDLE BZ GLITCH
C
C
	FFUND = .333557
	ITEM = 'WIND_SPIN_RATE_R4'
	OK = W_ITEM_R4(CH,ITEM,SPINRATE,1,ISIZE)
	SAMP_PER_SEC = 1024.*FFUND
	SPSS = SAMP_PER_SEC
	DANG = SPINRATE/SAMP_PER_SEC			! IN RADIANS
	DANGD = DANG*360./TWOPI
	DANGR = DANG/TWOPI				! IN CYCLES PER SAMP.
	SAMP_PER_SPIN = TWOPI/DANG
	NSPPS = SAMP_PER_SPIN + .5			! ROUNDOFF
C
	  bzmax = 0.
	  bzmin = 0.
	  do i = 1,1024
	    if(rdata(i,3).gt.bzmax) then
		bzmax = rdata(i,3)
		ibzmax = i
	    endif
	    if(rdata(i,3).lt.bzmin) then
		bzmin = rdata(i,3)
		ibzmin = i 
	    endif
	  enddo
c	the following is based on solving:
c	  bzmxang = antang + dangd*(1024-ibzmax)
c	  bzmnang = antang + dangd*(1024-ibzmin)
c	for antang, given bzmxang = 34. deg.
c
c	antangb = 79. - dangd*(1024-ibzmax)	! this is s/c angle
	antangb = 34. - dangd*(1024-ibzmax)
	if(antangb.lt.-180.) antangb = antangb + 360.
C
	  DO N = 1,1024
		WT(N) = 1.
	  ENDDO
C
C	CHECK THAT THERE WAS A BZ GLITCH IN THE DATA.  IF NOT, USE
C		EX DATA
C
	IF(BZMAX.GT..3) THEN
C
C	  ON 19960528, BZMAX WAS .391 ALL DAY EXCEPT FOR NO GLITCH 
C	  FOR MAY 28 AND 29, BZMIN WAS SLIGHTLY VARIABLE, USE -.219
C
	  ANTANG = ANTANGB
	  IAANG = 0
C
C	  ELIMINATE BZ GLITCH
C
	  N1 = IBZMIN - 38
	  IF(N1.LT.1) N1 = N1 + 1024
	  N2 = IBZMAX + 44
	  IF(N2.GT.1024) N2 = N2 - 1024
C	  PRINT*,'ELIM GIVES N1,N2=',N1,N2
C
C	  NOW ELIMINATE GLITCH DATA, WHICH LIES BETWEEN N1 AND N2
C		IF N2.GT.N1, AND FROM 1 TO N2 AND N1 TO 1024 OTHERWISE
C
C	  IF(IABS(N2-N1).LT.88) THEN
	  IF(N2.GT.N1) THEN
	    DO N = N1,N2
c	      VDATA(N,3) = 0.
c	      RDATA(N,3) = 0.
	      WT(N) = 0.
	    ENDDO
	  ELSE
	    DO N = 1,N2
c	      VDATA(N,3) = 0.
c	      RDATA(N,3) = 0.
	      WT(N) = 0.
	    ENDDO
	    DO N = N1,1024
c	      VDATA(N,3) = 0.
c	      RDATA(N,3) = 0.
	      WT(N) = 0.
	    ENDDO
	  ENDIF
C
	ELSEIF(BZMIN.LT.-.219) THEN
	  antangb = 34. - dangd*(1024-IBZMIN-4)
	  if(antangb.lt.-180.) antangb = antangb + 360.
C	  print*,'b angle from min',antangb
	  IAANG = 2
	ELSE
cold	  CALL FFTLOANGLE(CH,VDATA(1,4),SUNANGLE,ANTANG)
	  CALL FFTLOANGLE(CH,RDATA(1,4),SUNANGLE,ANTANG)
	  IF(ANTANG.GT.180.) ANTANG = ANTANG - 360.
C	  PRINT*,'RETURN FROM FFTLOANGLE, SUN,ANT',SUNANGLE,ANTANG
	  IAANG = 1
	  N1 = 1
	  N2 = 1
	ENDIF
c
C	  write(65,*) 'bzmin,max',scetst,ibzmin,bzmin,ibzmax,bzmax		
C	  write(65,*) 'bzmin,max angles',bzmnang,bzmxang	
c        endif
C	CALCULATE POWERS FROM RAW VOLTAGE SAMPLES

c	print*,'vdata',vdata(1,1),vdata(1,2),vdata(1,3)
C
	DO NC = 1,4
	  PWR(NC) = 0.
	  AVR = 0.
C
C	  REMOVE AVERAGE
C	  DO NF = 1,1024
C	    AVR = AVR + RDATA(NF,NC)
C	  ENDDO
C	  AVR = AVR/1024.
C	  DO NF = 1,1024
C	    PWR(NC) = PWR(NC) + (RDATA(NF,NC)-AVR)**2
C	  ENDDO
C	  PWR(NC) = PWR(NC)/1024.
C
C	  REMOVE SINFIT
C	  IF(NC.NE.3) THEN
C	    CALL SINFIT(DANGR,RDATA(1,NC),ERR,Y,SUMSQ)
C	    DO NF = 1,1024
C	      PWR(NC) = PWR(NC) + ERR(NF)**2
C	      RDATA(NF,NC) = ERR(NF)
C	    ENDDO
C	  ELSE
C	    DO NF = 1,1024
C	      PWR(NC) = PWR(NC) + RDATA(NF,NC)**2
C	    ENDDO
C	  ENDIF
C	  PWR(NC) = PWR(NC)/1024.
C	      WRITE(67,1067) SCETST,NC,Y,SUMSQ
 1067	      FORMAT(I10,I8,I4,3E12.3,E11.4)
C
	ENDDO
C
C	IF(IRAW.EQ.1) THEN
		CALL FFTLOANGLE(CH,VDATA(1,4),SUNANGLE,ANTANGX)
		IF(ANTANGX.GT.180.) ANTANGX = ANTANGX - 360.
C		PRINT*,'RETURN FROM FFTLOANGLE, SUN,ANT',SUNANGLE,ANTANGX
C	ENDIF
C
c	WRITE(66,1066) SCETST,(FREQHZ(NC),NC=1,2),(SPPWR(NC),NC=1,2),
c     1	(BW(NC),NC=1,2),(FREQZC(NC),NC=1,2),BZMIN,BZMAX,ANTANGB,
c     2  ANTANGX,IAANG
C	WRITE(67,1068) DBSPEC(2,1),DBSPEC(2,2),DBSPEC(3,1),DBSPEC(3,2),
C     1	DBSPEC(4,1),DBSPEC(4,2),DBSPEC(5,1),DBSPEC(5,2),DBSPEC(6,1),
C     1  DBSPEC(6,2)
 1066   FORMAT(I10,I8,2F6.2,4E11.3,2F6.1,2E12.3,2F7.1,I2)
 1068   FORMAT(10F7.1)
C	PWRTOT = PWR(1) + PWR(2) 		! ELIM BZ GLITCH
C
C	CALCULATE POWERS FROM FREQ CORRECTED VOLTAGE SAMPLES
C
c	DO N = 1,1024
c	  WRITE(38,*) N,VDATA(N,1),VDATA(N,2),VDATA(N,3)
c	ENDDO
C
	DO NC = 1,4
	  VPWR(NC) = 0.
	  AVR = 0.
	  IF(NC.NE.3) THEN
C	    REMOVE SINFIT
CS	    CALL SINFIT(DANGR,VDATA(1,NC),ERR,Y,SUMSQ)
CSS	    DO NF = 1,1024
CS	      VPWR(NC) = VPWR(NC) + ERR(NF)**2
CS	      VDATA(NF,NC) = ERR(NF)
CS	    ENDDO
CS	    VPWR(NC) = VPWR(NC)/1024.
	  ELSE
C
C	    REMOVE AVERAGE
	    DO NF = 1,1024
	      AVR = AVR + VDATA(NF,NC)
	    ENDDO
	    AVR = AVR/1024.
	    Y(1) = AVR
	    DO NF = 1,1024
	      VPWR(NC) = VPWR(NC) + (VDATA(NF,NC)-AVR)**2
	    ENDDO
	    VPWR(NC) = VPWR(NC)/945.
	  ENDIF
C	      WRITE(68,1067) SCETST,NC,Y,SUMSQ
C
	ENDDO
c	DO N = 1,1024
c	  WRITE(39,*) N,VDATA(N,1),VDATA(N,2),VDATA(N,3)
c	ENDDO
c	IF(1) STOP
C
	PWRTOT = PWR(1) + PWR(2) 		! AVOID BZ GLITCH
C	WRITE(81,777) SCETST,PWR(1),PWR(2),PWR(3),PWRTOT
 777	FORMAT(I10,I8,2X,4E12.4)
C	IF(PWRTOT.LT.PWRLIM.AND.VPWRTOT.LT.VPWRLIM) GO TO 100
C	RECALCULATE BZ POWER
C
C	NC = 3
C	  PWR(NC) = 0.
C	  AVRF = 0.
C	  SQF = 0.
C	  DO NF = LIMLO,LIMUP
C	    VLT2 =  10.**(.1*DBSPEC(NF,NC))
C	    PWR(NC) = PWR(NC) + VLT2
C	    AVRF = AVRF + FREQ(NF)*VLT2
C	    SQF = SQF + VLT2*FREQ(NF)**2
C	  ENDDO
C	  FREQHZ(NC) = AVRF/PWR(NC)
C	  BW(NC) = SQF/PWR(NC) - FREQHZ(NC)**2
C	  BW(NC) = SQRT(AMAX1(BW(NC),0.))
C	PWRTOT = PWRTOT + PWR(NC) 	
C
C	TRANSFORM TO GSE SYSTEM
C
C	PRINT*,'AT XFGSE,ANTANG,ANTANGB,DANGD=',ANTANG,ANTANGB,DANGD
C
	CALL XFGSE(CH,NSYS,1024,SPINRATE,SPSS,ANTANG,VDATA,GDATA)
C	DO N = 1,1024
C	  WRITE(39,*) N,gdata(N,1),gdata(N,2),gdata(N,3)
C	ENDDO
C	IF(1) STOP
C
C	print*,' VDATa1', VDATa(1,1), VDATa(2,1), VDATa(3,1)
C	print*,' VDATa4', VDATa(1,4), VDATa(2,4), VDATa(3,4)
C	print*,'GDATa1',GData(1,1),GData(2,1),GData(3,1)
C	print*,'GData2',GData(1,2),GData(2,2),GData(3,2)
C
C	CALCULATE VARIANCE MATRIX, EIGENVALUES, AND EIGENVECTORS
C
	NPT = 1024
C
C	CALL TO VMATRIX TO FIND THIRD EIGENVECTOR, FOR DIRECTION WRT B
C
	CALL VMATRIX(GDATA,WT,NPT)
C	print*,'evals',eval
	DO N = 1,3
	  GSEK(N) = EVECT(N,3)
	ENDDO
C
C	TRANSFORM TO VARIANCE MATRIX EIGENSYSTEM
C
	CALL XFORMVM(CH,NPT,NSYS,GDATA,VMDATA,WT)
c	DO N = 1,1024
c	  WRITE(37,*) N,VMDATA(N,1),VMDATA(N,2),VMDATA(N,3)
c	ENDDO
c	if(1) stop
C	print*,' gdata1', gdata(1,1), gdata(2,1), gdata(3,1)
C	print*,'VMdata1',VMdata(1,1),VMdata(2,1),VMdata(3,1)
C	print*,'xfdata2',xfdata(1,2),xfdata(2,2),xfdata(3,2)
C	
C	IF(1) GO TO 100
C
C	TEST, IS IT A KEEPER?
C
C	TEST FOR SEVERAL CONSEQUETIVE LARGE SIGNALS
C
	NBIGXSIG = 0
	NBIGYSIG = 0
	DO NF = 9,512
	  IF(DBSPEC(NF,1).GT.THRESH(NF)) THEN
	    NBIGXSIG = NBIGXSIG + 1
	    NFXB = NF
	print*,'big sig,nf',nbigxsig,nf,thresh(nf),dbspec(nf,1)
	    IF(NBIGXSIG.GE.3) GO TO 205
	  ELSE
	    NBIGXSIG = 0
	  ENDIF
 205	  CONTINUE
	  IF(DBSPEC(NF,2).GT.THRESH(NF)) THEN
	    NBIGYSIG = NBIGYSIG + 1
	    NFYB = NF
	    IF(NBIGYSIG.GE.3) GO TO 200
	  ELSE
	    NBIGYSIG = 0
	  ENDIF
	ENDDO
 200	ITP = 0
	IF(NBIGXSIG.GE.3.OR.NBIGYSIG.GE.3) then
		ITP = 1
C
C	TAKE THE WIDEST RANGE
C
	NFLOW = MIN0(NFXB,NFYB) - 2
	IF(NBIGXSIG.LT.3) NFLOW = NFYB - 2
	NFLOW = MAX0(NFLOW,2)
C
C	else
C	  do n=1,512
C	    avspec(n) = avspec(n) + dbspec(n,1) + dbspec(nf,2)	  
C	    stdspec(n) = stdspec(n)+dbspec(n,1)**2+dbspec(nf,2)**2	  
C	    spcount(n) = spcount(n) + 2.
C	  enddo
	endif
C	
c	print*,'itp,x,y',itp,nbigxsig,nbigysig
	WTCOUNT = 0.
	DO N = 1,1024
	  WTCOUNT = WTCOUNT + WT(N)
	ENDDO
C
c	print*,'wtcount',n1,n2,wtcount

	RATIO3RD = EVAL(3)/(EVAL(1)+EVAL(2))
	write(37,*) sceti4,ratio3rd
	ITR = 0
	IF(RATIO3RD.LT..15) ITR = 1
C
	print*,'ratio',ratio3rd
	IF(ITP+ITR.EQ.0) GO TO 100
C
	PRINT*,'******START EVENT',SCETST(1),SCETST(2),' *****'
C
	STOKESRQ = (EVAL(1)-EVAL(2))/(EVAL(1)+EVAL(2))
	STOKESI = (EVAL(1)+EVAL(2))/WTCOUNT

C
	IF(ITP.NE.0) THEN
C	  FIND UPPER EDGE OF PEAK
	  NBIGXSIG = 0
	  NBIGYSIG = 0
	  NFXBU = 2
	  NFYBU = 2
	  DO NF = 512,9,-1
	    IF(DBSPEC(NF,1).GT.THRESH(NF)) THEN
	      NBIGXSIG = NBIGXSIG + 1
	      NFXBU = NF
	      IF(NBIGXSIG.GE.3) GO TO 210
	    ELSE
	      NBIGXSIG = 0
	    ENDIF
 210	    CONTINUE
	    IF(DBSPEC(NF,2).GT.THRESH(NF)) THEN
	      NBIGYSIG = NBIGYSIG + 1
	      NFYBU = NF
	      IF(NBIGYSIG.GE.3) GO TO 220
	    ELSE
	      NBIGYSIG = 0
	    ENDIF
	  ENDDO
 220	  CONTINUE
C
	  NFHI  = MAX0(NFXBU,NFYBU) + 2	
	  IF(NBIGYSIG.LT.3) NFHI = NFXBU + 2
	  print*,'nflow,nfhi',nflow,nfhi
	  IF(NFHI.LT.NFLOW) NFHI = NFLOW + 2
C
	  DO NC = 1,3
	    SPPWR(NC) = 0.
	    AVRF = 0.
	    SQF = 0.
	    IF(ITP.NE.0) THEN
	      DO NF = NFLOW,NFHI
	        VLT2 =  10.**(.1*DBSPEC(NF,NC)) - 10.**(.1*BACKGND(NF))
	        SPPWR(NC) = SPPWR(NC) + VLT2
	        AVRF = AVRF + FREQ(NF)*VLT2
	        SQF = SQF + VLT2*FREQ(NF)**2
	  	TPOWER(NF) = TPOWER(NF) + VLT2
		TCOUNT(NF) = TCOUNT(NF) + 1./3.
	      ENDDO
	    ENDIF
	    FREQHZ(NC) = AVRF/SPPWR(NC)
	    BW(NC) = SQF/SPPWR(NC) - FREQHZ(NC)**2
	    BW(NC) = SQRT(AMAX1(BW(NC),0.))
	    SPPWR(NC) = .5*SPPWR(NC)		! CONVERT TO RMS
	  ENDDO
	  PWRTOT=SPPWR(1)+SPPWR(2)+SPPWR(3)
	ENDIF
C
	ITEM = 'WIND_ORBIT_X(GSE)_R8'
	ok = W_ITEM_R8(ch, item, XGSE, 1, ret_size)
	XGSE = XGSE/ERAD
	ITEM = 'WIND_ORBIT_Y(GSE)_R8'
	ok = W_ITEM_R8(ch, item, YGSE, 1, ret_size)
	YGSE = YGSE/ERAD
	ITEM = 'WIND_ORBIT_Z(GSE)_R8'
	ok = W_ITEM_R8(ch, item, ZGSE, 1, ret_size)
	ZGSE = ZGSE/ERAD
	ITEM = 'WIND_MFI_BPHI(GSE)_R4'
	ok = W_ITEM_R4(ch, item, AZMAG, 1, ret_size)
	ITEM = 'WIND_MFI_BTHETA(GSE)_R4'
	ok = W_ITEM_R4(ch, item, ELMAG, 1, ret_size)
	ITEM = 'WIND_3DP_E_TEMP_R4'
	ok = W_ITEM_R4(ch, item, TEMPE, 1, ret_size)
	ITEM = 'WIND_3DP_ION_TEMP_R4'
	ok = W_ITEM_R4(ch, item, TEMPI, 1, ret_size)
	ITEM = 'WIND_3DP_ION_DENSITY_R4'
	ok = W_ITEM_R4(ch, item, DENS, 1, ret_size)
	ITEM = 'WIND_3DP_ION_VX(GSE)_R4'
	ok = W_ITEM_R4(ch, item, VX, 1, ret_size)
	ITEM = 'WIND_3DP_ION_VY(GSE)_R4'
	ok = W_ITEM_R4(ch, item, VY, 1, ret_size)
	ITEM = 'WIND_MFI_BX(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BX, 1, ret_size)
	ITEM = 'WIND_MFI_BY(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BY, 1, ret_size)
	ITEM = 'WIND_MFI_BZ(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BZ, 1, ret_size)
	ITEM = 'WIND_MFI_BMAG_R4'
	ok = W_ITEM_R4(ch, item, BMAG, 1, ret_size)
	ITEM = 'WIND_SPIN_RATE_R4'
	ok = W_ITEM_R4(ch, item, SPINRATE, 1, ret_size)
C****
C	WRITE(37,*), SCETST(2),XMAX,YMAX,XCOUNT,YCOUNT
C**** 
C
	FP = 9.*SQRT(DENS)			  ! PLASMA FREQ IN KHZ
	FCE = .028*BMAG
	BETA = .402*DENS*(TEMPE+TEMPI)/BMAG**2
C
	AXMAJOR = EVAL(1)/WTCOUNT
	AXMINOR = EVAL(2)/WTCOUNT
	COSKB = (BX*GSEK(1)+BY*GSEK(2)+BZ*GSEK(3))/BMAG
C
C	CALCULATE NUMBER OF TURNS THE VECTOR MAKES IN THE VM SYSTEM
C
C	N1T = MIN0(N1,N2)
C	N2T = MAX0(N1,N2)
C	N1 = N1T
C	N2 = N2T
C	IF(IABS(N2-N1).LT.90) THEN
C	    CALL COUNT_REV(VMDATA(1,1),VMDATA(1,2),1,N1,REVS1)
C	    CALL COUNT_REV(VMDATA(1,1),VMDATA(1,2),N2,1024,REVS2)
C	    COUNT = N1 + (1024-N2+1)
C	    REVS = REVS1 + REVS2
C	ELSE
C	    CALL COUNT_REV(VMDATA(1,1),VMDATA(1,2),N1,N2,REVS)
C	    COUNT = N2-N1+1
C	ENDIF
C
C	DO N = 1,1024			! CHECK COUNT_REV
C	  WRITE(37,*) N,VMDATA(N,1),VMDATA(N,2),VMDATA(N,3)
C	ENDDO
C	IF(1) STOP
C
	CALL COUNT_REV(VMDATA(1,1),VMDATA(1,2),WT,REVS)
	FREQTRN = ABS(REVS)*FUNDFR*1024./WTCOUNT
C
c	WRITE(69,169) SCETST,VPWR(1),VPWR(2),VPWR(3),VPWR(1)+VPWR(2),
c     1	  RATIO3RD,COSKB
	VPWRTOT = VPWR(1)+VPWR(2)
 169	FORMAT(I10,I8,5E11.4,F6.3)
C	PRINT*,'REL STOKES Q,TURNS',STOKESI,STOKESRQ,REVS,FREQTRN
C	IF(ANGTOB.GT.90.) ANGTOB = ANGTOB - 180.
C	IF(ANGTOB.GT.90.) ANGTOB = ANGTOB - 180.
C	IF(ANGTOB.LT.-90.) ANGTOB = ANGTOB + 180.
C	IF(ANGTOB.LT.-90.) ANGTOB = ANGTOB + 180.
C
C	PLASMA PARAMETERS, ETC.

C	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
C
c	print date, time, event no., freq, Px, Py, Vy/Vx, angles of
c		antenna,B, phiB, mag B or fce, fp
c
	  WRITE(90,1090) scetst,XGSE,YGSE,ZGSE,
     1	     AZMAG,ELMAG,FP,FCE,BETA
 1090	format(I10,I7,3F8.2,F8.3,2F7.1,F7.3,F7.4,F8.4,2F7.0)
C
	EVAL1 = EVAL(1)/WTCOUNT	
	EVAL2 = EVAL(2)/WTCOUNT	
	EVAL3 = EVAL(3)/WTCOUNT
	FREQAV = .5*(FREQHZ(1) + FREQHZ(2))	
	BWAV = .5*(BW(1) + BW(2))	
	IF(COSKB.LT.0.) THEN
	  REVS = -REVS
	  COSKB = -COSKB
	ENDIF
	WRITE(80,1011) scetST,IRAW,ITP,ITR,nflow,nfhi,FREQTRN,FREQAV,
     1    BWAV,EVAL1,EVAL2,EVAL3,RATIO3RD,PWRTOT,SPPWR(3),STOKESI,
     2 	  STOKESRQ,COSKB,REVS
 1011	format(I10,I7,3I2,2I4,F6.1,2F6.1,3E11.3,F6.3,2F7.3,F7.3,
     1		F6.2,2F6.2)
C
	IF(OK.NE.82) GO TO 100
	RETURN
	END
	SUBROUTINE COUNT_REV(FX,FY,WT,REVS)
C
C	COUNTS NUMBER OF REVOLUTIONS OF THE VECTOR FX,FY
C	WHICH HAS NPT SAMPLES
C
	REAL FX(1),FY(1),WT(1)
C
C	PRINT*,'COUNT_REV CALLED,N1,N2',N1,N2
C	PRINT*,'FIRST SAMPLES',FX(1),FY(1)
C	PRINT*,'LAST SAMPLES',FX(1024),FY(1024)
	ANGTOT = 0.
	ANGLESV = 0.
	WTCOUNT = 0.
	IF(FY(1)*FX(1).NE.0.) ANGLESV = ATAN2D(FY(1),FX(1))
	YSAVE = FY(1)
	DO N = 1,1024
	  IF(WT(N).LT..1) GO TO 100
	  IF(FY(1)*FX(1).NE.0..AND.WTCOUNT.EQ.0.) 
     1		ANGLESV = ATAN2D(FY(N),FX(N))
	  ANG = ANGLESV
	  IF(FY(N).NE.0..OR.FX(N).NE.0.) ANG = ATAN2D(FY(N),FX(N))
	  ANGTOT = ANGTOT + ANG - ANGLESV
	  WTCOUNT = WTCOUNT + 1.
	  SFY = YSAVE*FY(N)
	  IF(FX(N).LT.0..AND.SFY.LT.0.) THEN
	    IF(YSAVE.GT.0.) THEN 
	      ANGTOT = ANGTOT + 360.
	    ELSE
	      ANGTOT = ANGTOT - 360.
	    ENDIF
	  ENDIF
C
C	print*,'cnt',fx(n),fy(n),ang,anglesv,angtot
C
	  ANGLESV = ANG
	  YSAVE = FY(N)
 100	  CONTINUE
	ENDDO
	REVS = ANGTOT/360.
C
	RETURN
	END
	SUBROUTINE COUNT_REVOLD(FX,FY,N1,N2,REVS)
C
C	COUNTS NUMBER OF REVOLUTIONS OF THE VECTOR FX,FY
C	WHICH HAS NPT SAMPLES
C
	REAL FX(1),FY(1)
C
C	PRINT*,'COUNT_REV CALLED,N1,N2',N1,N2
C	PRINT*,'FIRST SAMPLES',FX(1),FY(1)
C	PRINT*,'LAST SAMPLES',FX(1024),FY(1024)
	ANGTOT = 0.
	ANGLESV = 0.
	IF(FY(1)*FX(1).NE.0.) ANGLESV = ATAN2D(FY(1),FX(1))
	YSAVE = FY(1)
	DO N = N1+1,N2
	  ANG = ANGLESV
	  IF(FY(N).NE.0..OR.FX(N).NE.0.) ANG = ATAN2D(FY(N),FX(N))
	  ANGTOT = ANGTOT + ANG - ANGLESV
	  SFY = YSAVE*FY(N)
	  IF(FX(N).LT.0..AND.SFY.LT.0.) THEN
	    IF(YSAVE.GT.0.) THEN 
	      ANGTOT = ANGTOT + 360.
	    ELSE
	      ANGTOT = ANGTOT - 360.
	    ENDIF
	  ENDIF
C
C	print*,'cnt',fx(n),fy(n),ang,anglesv,angtot
C
	  ANGLESV = ANG
	  YSAVE = FY(N)
	ENDDO
	REVS = ANGTOT/360.
C
	RETURN
	END
       SUBROUTINE SINFIT(X,FDATA,ERR,Y,SUMSQ)
C
C	THIS ROUTINE FITS A SINE WAVE TO DATA.  X IS SUPPOSED TO
C	BE VARIED BY HUNTMN, WHILE Y(1) TO Y(3) ARE DETERMINED BY
C	LEAST SQUARES FIT IN CLOSED FORM.  
C
C	X IS FREQUENCY IN CYCLES PER SAMPLE  (A SMALL NUMBER)
C
C	Y(1) IS AVERAGE (OFFSET)
C	Y(2) IS COEFF OF COSINE
C	Y(3) IS COEFF OF SINE
C
        DOUBLE PRECISION COS1,SIN1,COSN,SINN,SINNT,THT0
        REAL X,C(3,3),Y(3),ERR(1024),WT(1024),FDATA(1024)
        DATA TWOPI /6.2831853/
	DATA WT /1024*1./
C
	DO I = 1,3
	  Y(I)= 0.
	  DO J = 1,3
	    C(I,J) = 0.
	  ENDDO
	ENDDO
C
	THT0 = TWOPI*X
	COS1 = DCOS(THT0)
	SIN1 = DSIN(THT0)
	SINN = SIN1
	COSN = COS1
	DO N = 1,1024
	  Y(1) = Y(1) + WT(N)*FDATA(N)
	  Y(2) = Y(2) + WT(N)*FDATA(N)*COSN
	  Y(3) = Y(3) + WT(N)*FDATA(N)*SINN
	  C(1,1) = C(1,1) + WT(N)
	  C(1,2) = C(1,2) + WT(N)*COSN
	  C(1,3) = C(1,3) + WT(N)*SINN
	  C(2,2) = C(2,2) + WT(N)*COSN**2
	  C(2,3) = C(2,3) + WT(N)*COSN*SINN
	  C(3,3) = C(3,3) + WT(N)*SINN**2
C
	  SINNT = COSN*SIN1 + SINN*COS1
	  COSN = COS1*COSN - SIN1*SINN
	  SINN = SINNT
	ENDDO
	C(3,1)= C(1,3)
	C(3,2) = C(2,3)
	C(2,1) = C(1,2)
	CALL GAUSSJ(C,3,3,Y,1,1)
C
	SUMSQ = 0.
	SUMN = 1.E-8
	THT0 = TWOPI*X
	COS1 = DCOS(THT0)
	SIN1 = DSIN(THT0)
	SINN = SIN1
	COSN = COS1
	DO N = 1,1024
          ERR(N) =  FDATA(N) - Y(2)*COSN - Y(3)*SINN - Y(1)
	  SUMSQ = SUMSQ + WT(N)*ERR(N)**2
	  SUMN = SUMN + WT(N)
	  SINNT = COSN*SIN1 + SINN*COS1
	  COSN = COS1*COSN - SIN1*SINN
	  SINN = SINNT
	ENDDO
	SUMSQ = SUMSQ/SUMN
C	FIXED_V1 = Y(1) + Y(2)*COS(3.*THT0) + Y(3)*SIN(3.*THT0)
	FIXED_V = FDATA(3) - ERR(3)
C	PRINT*,'SINFIT EXIT,X,ERR,FDATA,SUMSQ',X,ERR(3),FDATA(3),SUMSQ
       RETURN
       END