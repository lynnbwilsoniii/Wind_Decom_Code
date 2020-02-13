	subroutine makefile(ch)
c
C	MAKEFILE10, A PROGRAM TO SEARCH FOR REFLECTING LANGMUIR WAVES.  
C
C	OBVIOUSLY NOT DONE YET
C	ONE CRITERION WOULD BE TO LOOK FOR DOUBLE PEAKED EVENTS,
C	PEAKS QUITE CLOSE TOGETHER (DIFFERENT DOPPLER SHIFTS ONLY)
C	AND CLOSER TOGETHER AT ONE END THAN THE OTHER (K GOING TO
C	ZERO)  E.G. FOR A 5 KEV BEAM AND n = 7, VSW = 450 KM/S
C	THE DOPPLER SHIFT IS +- 250 HZ.
C	OR LOOK FOR EVENTS VERY CLOSE TOGETHER IN TIME WHICH
C	SATISFY THIS
C	I DECIDED THAT THE CHANCES OF SEEING REFLECTION IN ONE EVENT
C	IS TOO SMALL --  THERE IS NOT LIKELY TO BE MUCH CHANGE IN
C	DENSITY IN 17 MSEC.  SO LOOK FOR DOUBLE PEAKED EVENTS WHICH
C	ARE LESS THAN 2 SEC APART
C
C	include		'wind_examples:wind_tm_routine_def.for'
C	include		'wind_examples:wind_return_code_def.for'
C
	COMMON /PARTBLK/ X4DATA(2050,4),XRE,YRE,ZRE,SUNCLOCK,SPINRATE
	COMMON /XYBLOCK/ XSPECT,YSPECT,DPHASE
	COMMON /PLTBLK/ TDSHI(2000),FRQHI(2000),HRHI(2000),DPUHI(2000),
     1		SPSHI(2000),NPTH,NPTL,NPTB,NSIM,HRSIM(200),
     1		EVNO(2000,3)
	REAL 	XSPECT(1025),YSPECT(1025),DPHASE(1025)
	integer*4 ch,ok,okt,OK1,OK2,SCETI4(2),NDATA(2048),SUNCLOCK
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	INTEGER*4 ITDSCH
	REAL*8 SCET8,SCETMFI,SCET3DP
	REAL FREQ(1024),DATA(2048),SPECT(1025)
	character*32 ITEM
	character*4 event
	DATA TWOPI /6.2831853/
	DATA YXRATIO /11.8/
C	ABOVE BASED ON LEFFX = 41.1, LEFFY = 3.79
C	data event /'TDSF'/
	data event /'FILL'/
	DATA XLEN,YLEN,ZLEN /.0411, .00379, .00217/             ! KM, FOR mV
C	DATA D_MID_END /100./       ! this value excluded 1996/12/29 4701840
C	DATA D_MID_END /80./  	    ! used for 1996 0415 to 0419
	DATA D_MID_END /50./
	DATA IOPEN /0/
	DATA IDIAGN /0/
C
	IF(IOPEN.EQ.0) THEN
	OPEN(UNIT=91,FILE='REFLECTION.DATA',STATUS='OLD',ACCESS='APPEND')
	IOPEN = 1
	ENDIF
C
C
C
C
C	IN THIS SECTION, THE PROGRAM WORKS THROUGH TDSF AND FILL, CHECKS
C	EVENT FOR TWO NARROW PEAKS, AND PUTS "GOOD" EVENTS IN A FILE.
C	IN THE NEXT SECTION, THE EVENTS WILL BE PUT IN ORDER AND CHECKED
C	FOR CLOSENESS IN TIME. 
C
	DO 200 LOCATE = 1,2
C
	  NEVENT=0
	  event = 'FILL'
	  if(locate.eq.2) event='TDSF'
	  SCET = 0.
	  call w_channel_position(TDSCH,SCET)
	  print*,' file starts at scet',SCET
	  scetstr = scet
	  dd = SCET
C	  SCET = float(dd) + hh/24. + mm/1440.
C	  print*,'set channel position to',SCET
C	  call w_channel_position(TDSCH,SCET)
C	  print*,'channel position set to',SCET

C	ok = wind_tm_get_mfmf(TDSCH,major,minor)
C	if (.not. ok) stop 'cannot get Mfmf stream position'
C	print*,' after mfmf, at Mfmf=',major,minor
c
	get_tm_stream = 1
c
        ok = w_event(TDSCH,EVENT)
	print*,'for first event,OK=',ok
c
	  IPROCESS = 4
	  CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
C
	  DO N = 1,2048
	      X4DATA(N,ITDSCH) = DATA(N)
	  ENDDO
C
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
	   IF(TDS_CHANNEL.LE.2) THEN
	      ISPS = IFSPS
	      WRITE(PTITLE(2),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(5),1008) FFILTER(IFILF+1)
	   ELSE
	      ISPS = ISSPS
	      WRITE(PTITLE(8),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	   ENDIF
	   call w_ur8_to_ydoy(scet,yyyy,doy,msday)
 1012	   FORMAT(I10)
C
C	   ipa = ichpa(tds_channel)
C	   WRITE(PTITLE(7),1007) pa(tds_channel,ipa+1)
 1004	   FORMAT(F7.2)
 1008	   FORMAT(F7.0)
C
	type*,'going to get first desired event'
	go to 110
c
C
C	GET NEXT EVENT
C
 100    continue

!	 this is the main program loop

C	if(wind_tm_eof(TDSCH,major,minor)) then
C	  type*,'call pltmax at eof, just after 100'
C	  call pltmax(-2)
C	  stop
C	endif
C
C	IF(NEVENT.GT.20) GO TO 200

	ok = w_event(TDSCH,EVENT)
	NEVENT = NEVENT+1
	   if (ok.ne.1) then
	      if(ok.eq.82) go to 200
	      ok = wind_tm_get_mfmf(TDSCH,major,minor)
	      type *, char(7), '***** not OK missing packet in event ****'
c	      type *, 'Cannot get event at MF.mf: ', major, minor
C	      if (wind_tm_realtime(ch)) then
C	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
C	         type *, '-reset to latest position: ', major, minor
C	      else
C	         call wind_tm_increment_packet(major,minor)
C	         type *, '-incrementing packet...'
C	      end if
	   end if
	  ok = wind_tm_get_mfmf(TDSCH,major,minor)
	  if (.not. ok) stop 'cannot get Mfmf stream position'
C
 110	  CONTINUE


C
C	ITEM = 'DATA'
C	OK = WIND_TM_GET_ITEM(TDSCH,ITEM,NDATA,SIZE,RETURN_SIZE)
	  IPROCESS = 4
	  CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
C
	  DO N = 1,2048
	      X4DATA(N,ITDSCH) = DATA(N)
	  ENDDO
C
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
C	PRINT*,'CHANNEL',TDS_CHANNEL
C	  PRINT*,'DATA, SIZE=',RETURN_SIZE
C	  PRINT 222, (NDATA(J),J=1,2048)
 222	FORMAT(10I6)
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(TDSCH,ITEM,SCET,1,RETURN_SIZE)
	item = 'DPU_CLOCK'
	OK = W_ITEM_I4(TDSCH,ITEM,DPUCLK,1,RETURN_SIZE)
c******		temporary fix for bad first event
	if(dd.eq.0) dd = scet
c
	   IF(TDS_CHANNEL.LE.2) THEN
	      ISPS = IFSPS
	      WRITE(PTITLE(2),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(5),1008) FFILTER(IFILF+1)
	   ELSE
	      ISPS = ISSPS
	      WRITE(PTITLE(8),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	   ENDIF
C
	DO IK = 1,2048
	  NDATA(IK) = NDATA(IK)-128
	ENDDO

 20	CONTINUE




C*********
	     item = 'EVENT_SCET'
	     ok = w_item_i4(TDSCH, item,scetI4, 2, return_size)
C	     type*,'scet',s_scetI4 
	     SS = MOD(SCETI4(2),100)
	     MM = SCETI4(2)/100
	     MM = MOD(MM,100)
	     HH = SCETI4(2)/10000
C	     SCET = DFLOAT(DD) + HH/24. + MM/1440. + SS/86400.
C	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
C	     call w_ur8_to_string(scet,PTITLE(16),PTITLE(18))
C	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
C	     call w_ur8_to_ydoy(scet,yyyy,doy,msday)
C	     ihrmn = 100*hh+mm
C	     TYPE 1005,itemp,YYYY,MON,DD,IHRMN
C		write(26,*) itemp,YYYY,MON,DD,IHRMN
c 1016	   format(I5,'/',I2,'/',I2)
C
C	print*,'loc,ch,ichpa',locate,tds_channel,ichpa(tds_channel)
	IF(LOCATE.EQ.1) THEN			!FILL
	  IF(TDS_CHANNEL.EQ.1.AND.ICHPA(1).EQ.0) THEN      ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
c            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.2.AND.ICHPA(2).EQ.0) THEN       ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
c            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	ENDIF	
C
	IF(LOCATE.EQ.2) THEN			! TDSF
	  IF(TDS_CHANNEL.EQ.1.AND.ICHPA(1).EQ.0) THEN      ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
C            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.2.AND.ICHPA(2).EQ.0) THEN       ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
 	    EVNO(NPTH,1) = ITEMP
C            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	ENDIF
C
	ENDIF	
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	if(npth.lt.1900) go to 100
	if(LOCATE.EQ.3.AND.nptL.lt.1900) go to 100
C
C	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
C
	PRINT*,NEVENT,' ',EVENT,' EVENTS FOUND'
 200	CONTINUE          ! END OF ILOCAT LOOP
C
C
C
C
C
C
C
 100	continue
C
C	GET NEXT EVENT
C
	IF(IDIAGN.NE.0) type*,'going to get next ',EVENT,' event'
c
	ok = w_event(ch,event)
C
C	CHECK FOR END OF RECORD
C
	if (ok.ne.1) then
		if(ok.eq.82) then
		   ok = w_channel_close(ch)
C		   WRITE(66,*) SCETI4(1),FFT_COUNT,RAW_COUNT,E_COUNT
C     1			, B_COUNT
		   return
	        endif
		write(6,*) 'cannot open ',event, ', ok=', ok
	endif
C
	OKT = W_MESSAGES_OFF(ch)
C

	item = 'CHANNEL'
	ok = w_item_i4(ch, item, ITDSCH, 1, return_size)
	item = 'EVENT_NUMBER'
	ok = w_item_i4(ch, item, numevt, 1, return_size)
	IF(ITDSCH.EQ.1) LASTEVT = NUMEVT 
	IF(ITDSCH.NE.1.AND.NUMEVT.NE.LASTEVT) GO TO 100
	item = 'EVENT_SCET'
	ok = w_item_i4(ch, item, SCETI4, 2, return_size)
	IF(IDIAGN.NE.0) print*,'CHANNEL,event_number,las evn,time',
     1		ITDSCH,numevt,lastevt,sceti4
	ss = mod(SCETI4(2),100)
	mm = SCETI4(2)/100
	mm = mod(mm,100)
	hh = SCETI4(2)/10000
	item = 'EVENT_SCET_R8'
	ok = w_item_R8(ch, item, scet8, 1, return_size)
	item = 'FAST_RX_SPEED_R4'
	ok = w_item_R4(ch, item, SPS, 1, return_size)
C
c		PRINT*,'CALL TDS_PHYS'
C
	  IPROCESS = 4
	  CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
C
	  DO N = 1,2048
	      X4DATA(N,ITDSCH) = DATA(N)
	  ENDDO
	  IF(ITDSCH.EQ.1) THEN
	    DO N = 1,1025
	      XSPECT(N) = SPECT(N)
	    ENDDO
	  ELSE
	    DO N = 1,1025
	      YSPECT(N) = SPECT(N)
	    ENDDO
	  ENDIF
	IF(IDIAGN.NE.0)PRINT*,'GET ANOTHER IF CH NE 2, CHANN=',ITDSCH
	IF(ITDSCH.NE.2) GO TO 100
C
C	IF(ITDSCH.EQ.1)
C
C	FIND MAXIMUM IN SPECTRUM
C
	  SMAX = -1000.
	  DO N = 50,1022
	    IF(SPECT(N).GT.SMAX) THEN
	      SMAX = SPECT(N)
	      NSPMAX = N
	    ENDIF
	  ENDDO
	  IF(IDIAGN.NE.0) 
     1    PRINT*,' SPECTRUM MAX:N,F,SMAX',NSPMAX,(NSPMAX-1)*SPS/2048.,SMAX
	  FREQMAX = (NSPMAX-1)*SPS/2048.
	  IF(FREQMAX.LT.1.E4) GO TO 100 		! limit to gt 10 kHz
	  F1 = .001*FREQMAX
C	ENDIF
C
C	FIND SPECTRAL BANDWIDTH
C
	  DO N = NSPMAX,1022
	     NUPPER = N
	     IF(SPECT(N+1).GT.SPECT(N)) GO TO 201
	  ENDDO
 201	  CONTINUE
	  DO N = NSPMAX,50,-1
	     NLOWER = N
	     IF(SPECT(N-1).GT.SPECT(N)) GO TO 202
	  ENDDO
 202	  CONTINUE
C
C	  NOW THE SIGNAL IS MONOTONICALLY FALLING ON EACH SIDE OF THE
C		PEAK, OUT TO NLOWER AND NUPPER
C
C	LOOK FOR OTHER LARGE SIGNALS IN THE SPECTRUM
C
	  NOUTSIDE = 0
	  DO N = NUPPER+1,1022
	    IF(SPECT(N).GT.(SMAX-20.)) NOUTSIDE = NOUTSIDE + 1
C	    PRINT*,'OTHER LARGE SIGS:N,F,SMAX',N,(N-1)*SPS/2048.,SPECT(N)
	  ENDDO
	  DO N = NLOWER-1,50,-1
	    IF(SPECT(N).GT.(SMAX-20.)) NOUTSIDE = NOUTSIDE + 1
C	    PRINT*,'OTHER LARGE SIGS:N,F,SMAX',N,(N-1)*SPS/2048.,SPECT(N)
	  ENDDO
	  IF(NOUTSIDE.NE.0) GO TO 100
C
C	COMPARE MIDDLE AND ENDS
C
	PWRLEFT = 0.
	DO N = 1,170 
	  PWRLEFT = PWRLEFT + DATA(N)**2
	ENDDO
	PWRLEFT = PWRLEFT/170.
C
	PWRRIGHT = 0.
	DO N = 1878,2047 
	  PWRRIGHT = PWRRIGHT + DATA(N)**2
	ENDDO
	PWRRIGHT = PWRRIGHT/170.
C
	PWRMID = 0.
	DO N = 940,1109 
	  PWRMID = PWRMID + DATA(N)**2
	ENDDO
	PWRMID = PWRMID/170.
	PWREND = D_MID_END*(PWRRIGHT + PWRLEFT)
C
	IF(IDIAGN.NE.0) PRINT*,'PWRMID,END',PWRMID,PWREND
	IF(PWRMID.LT.PWREND) GO TO 100
C
	SPBW = (NUPPER-NLOWER)*SPS/2048.
C
C	GET SUPPORTING DATA
C
	item = 'WIND_3DP_ION_VX(GSE)_R4'
	ok = w_item_r4(ch, item, VX, 1, return_size)
	item = 'WIND_3DP_ION_VY(GSE)_R4'
	ok = w_item_r4(ch, item, VY, 1, return_size)
	item = 'WIND_3DP_ION_VZ(GSE)_R4'
	ok = w_item_r4(ch, item, VZ, 1, return_size)
	VSW = SQRT(VX**2 + VY**2 + VZ**2)
	VSWANG = ATAN2D(-VY,-VX)
	item = 'WIND_3DP_SCET_R8'
	ok = w_item_r8(ch, item, SCET3DP, 1, return_size)
	IF(IDIAGN.NE.0)	print*,'scet3dp,vx,vy,ang',scet3dp,vx,vy,vswang
	item = 'WIND_MFI_BPHI(GSE)_R4'
	ok = w_item_r4(ch, item, PHI_B, 1, return_size)
C
	item = 'WIND_MFI_BX(GSE)_R4'
	ok = w_item_r4(ch, item, BX, 1, return_size)
	item = 'WIND_MFI_BY(GSE)_R4'
	ok = w_item_r4(ch, item, BY, 1, return_size)
C
	ABSW = VSWANG - PHI_B
	IF(IDIAGN.NE.0)print*,'phi_b,absw',phi_b,absw
	IF(ABSW.GT.90.)  ABSW = ABSW - 90. 
	IF(ABSW.LT.-90.) ABSW = ABSW + 90. 
	ABSW = ABS(ABSW)
	item = 'WIND_MFI_BTHETA(GSE)_R4'
	ok = w_item_r4(ch, item, THETA_B, 1, return_size)
	item = 'WIND_MFI_SCET_R8'
	ok = w_item_r8(ch, item, SCETMFI, 1, return_size)
	item = 'WIND_3DP_E_TEMP_R4'
	ok = w_item_r4(ch, item, TE, 1, return_size)
	item = 'WIND_3DP_ION_TEMP_R4'
	ok = w_item_r4(ch, item, TI, 1, return_size)
	ITEM = 'SUN_ANGLE'
	ok = W_ITEM_I4(ch, item, SUNCLOCK, 1, ret_size)
	ITEM = 'WIND_SPIN_RATE_R4'
	ok = W_ITEM_R4(ch, item, SPINRATE, 1, ret_size)
	ANGLE =  -360.*SUNCLOCK/4096. - 45.       ! ANGLE SUN TO +EX AT END
	DANG = SPINRATE*360./SPS/TWOPI		  ! CHANGE PER SAMPLE
	THANT = ANGLE + 1024.*DANG + 360.	  ! ANGLE SUN TO +EX AT MIDDLE
	IF(IDIAGN.NE.0) PRINT*,'ANTENNA ANGLE',THANT
C
	write(66,*) ' '
	write(66,*) scetI4,numevt 
	CENTER = 1024.
C
	ANGTOSW = EANG - VSWANG
	IF(ANGTOSW.GT.90.) ANGTOSW = ANGTOSW - 180.
	IF(ANGTOSW.LT.-90.) ANGTOSW = ANGTOSW + 180.
C
C	CALCULATE AMPLITUDE BY AVERAGING SIGNAL IN THE MIDDLE
C	AND MULTIPLYING BY 2/PI
C
	AMPX1 = 0.
	AMPY1 = 0.
	COUNT = 0.
	DO N = 924,1124		! +- 100 SAMPLES AT MIDDLE
	  COUNT = COUNT + 1.
	  AMPX1 = AMPX1 + ABS(X4DATA(N,1))
	  AMPY1 = AMPY1 + ABS(X4DATA(N,2))
	ENDDO
	AMPX1 = (TWOPI/4.)*AMPX1/COUNT/XLEN
	AMPY1 = (TWOPI/4.)*AMPY1/COUNT/YLEN
C
	IF(IDIAGN.NE.0) PRINT*,'DIRECT,AMPX1,AMPY1',AMPX1,AMPY1
	RATIO = 0.
	IF(PWREND.NE.0.) RATIO = PWRMID/PWREND
	NBW = NUPPER-NLOWER
	IF(IDIAGN.NE.0) print*,'mid,end,ratio',pwrmid,pwrend,ratio
C
	WRITE(91,1011) scetI4,NUMEVT,EVENT(1:1),F1,RATIO,
     1	   AMPX,AMPY,AMPT,AMPX1,AMPY1,WDTH,VSW,ABSW,ANGTOB,THETA_B,
     1     ANGTOSW,TE,TI,FRMS
	WRITE(89,1012) SCETI4,NUMEVT,F1,NBW,SPBW,WDTH,DPHS
C
 1011	format(I9,I7,I10,A2,F7.3,F5.1,5F6.1,F6.2,F6.0,
     1	   F6.0,F6.0,F5.0,F5.0,F5.1,F5.1,E9.2)  ! 
c
c 1011	format(I9,I7,I10,A2,F7.3,F6.1,5F6.1,F6.2,F6.0,
c     1	   F6.0,F5.0,F5.0,F5.0,F5.1,F5.1,E9.2)  ! 
C
C	if(ok.ne.-1) stop
	IF(OK.NE.82) GO TO 100
	return
	end
	SUBROUTINE FANDBW(SPS,ICHANN,DBSPECT,NPEAK)
C
	REAL DBSPECT(1025,2)
	INTEGER HPEAK
C
C	FINDS FREQUENCY OF HIGHEST PEAK AND ITS BANDWIDTH
C
C	FIRST FIND THE MAXIMUM IN THE SPECTRUM. 
C	THEN TRACE EACH SIDE DOWN TO:
C		(1) HALFWAY TO BACKG
C		(2) SIGNAL STARTS TO GO UP AGAIN (REQUIRE 3 UP POINTS)
C	THEN FIT A/((W-W0)**2 + DW**2) TO THESE POINTS IF THERE
C		ARE AT LEAST 3 POINTS
C	THEN LOOK FOR POSSIBLE SECOND PEAK
C
C	  FIND MAXIMUM IN SPECTRUM
C
	SMAX = -1000.
	DO N = 10,1022
	    IF(DBSPECT(N,1).GT.SMAX) THEN
	      SMAX = DBSPECT(N,1)
	      NSPMAX1 = N
	    ENDIF
	ENDDO
	PRINT*,'X SPEC. MAX:N,F,SMAX',NSPMAX1,(NSPMAX1-1)*SPS/2048.,SMAX
	FREQMAX1 = (NSPMAX1-1)*SPS/2048.
C
C	GO DOWN LOW SIDE
C
	SLIM = .5*(SMAX - 80.)
	LCOUNT = 0
	SSAVE = SMAX
	LPEAK = 0
	DO N = NSPMAX1-1,10,-1
	    NSVL = N
	    IF(DBSPECT(N,1).GT.SSAVE) THEN
	      LCOUNT = LCOUNT+1
	      IF(LCOUNT.GE.3) THEN
		LPEAK = 1
		GO TO 20
	      ENDIF
	    ENDIF
	    IF(DBSPECT(N,1).LT.SLIM) GO TO 20
	    SSAVE = DBSPECT(N,1)
	ENDDO
C
C	GO DOWN HIGH SIDE
C
 20	CONTINUE
	LCOUNT = 0
	SSAVE = SMAX
	HPEAK = 0
	DO N = NSPMAX1+1,1024
	    NSVH = N
	    IF(DBSPECT(N,1).GT.SSAVE) THEN
	      LCOUNT = LCOUNT+1
	      IF(LCOUNT.GE.3) THEN
		HPEAK = 1
		GO TO 30
	      ENDIF
	    ENDIF
	    IF(DBSPECT(N,1).LT.SLIM) GO TO 30
	    SSAVE = DBSPECT(N,1)
	ENDDO
C
 30	CONTINUE
C
C	DETERMINE SECOND AND MAYBE THIRD PEAK
C
	IF(LPEAK.NE.0) THEN
	  SMAX = -1000.
	  DO N = NSVL+2,10,-1
	    IF(DBSPECT(N,1).GT.SMAX) THEN
	      SMAX = DBSPECT(N,1)
	      NSPMAX3 = N
	    ENDIF
	  ENDDO
	  FREQMAX3 = (NSPMAX3-1)*SPS/2048.
	  PRINT*,'2ND PK. MAX:N,F,SMAX',NSPMAX3,FREQMAX3,SMAX
	ENDIF
C
	IF(HPEAK.NE.0) THEN
	  SMAX = -1000.
	  DO N = NSVH-2,1024
	    IF(DBSPECT(N,1).GT.SMAX) THEN
	      SMAX = DBSPECT(N,1)
	      NSPMAX4 = N
	    ENDIF
	  ENDDO
	  FREQMAX4 = (NSPMAX4-1)*SPS/2048.
	  PRINT*,'2ND PK. MAX:N,F,SMAX',NSPMAX4,FREQMAX4,SMAX
	ENDIF
C
	RETURN
	END
