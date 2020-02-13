	PROGRAM SINCAL
C
C	FITS A SINE WAVE TO CALIBRATE TDS A/D CONVERTER
c	this is the version that fits positive and negative signals
c		together, with different offsets only
c		in my notebook, p 119, it is called TDSSINCAL.OLD2
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	CHARACTER*10	event
	integer*4	i,j,k,n,itemp,n_event
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6)
	character*80	stream
	character*4	pa(6,4)
	parameter	size=2048
	integer*4	return_size
	integer*4	tds_channel,ifilf,ifils,ifsps,issps,rate
	integer*4	temp_waves,maxtm
	character*32	s
	character*32	s_scet
	integer*4	scet(2)
	integer*4	wchannel
	integer*4	major, minor, dpumaj,dpumin
	character*80	file
	character*80	title
	character*32	item
	integer*4	ios
	integer*4	NREM,NHRMN,IHRMN
	real ffilter,sfilter,fsps,ssps
!
	common /headblk/ major,minor,tds_channel,rate,n_event,scet

C
	CHARACTER*120 JUNK(10)
	CHARACTER*24 PTITLE(10)
	INTEGER*4 TDSCH,hkch
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	COMMON /HEADBL/ JUNK
	COMMON /FITBLK/ NPT,COUNT,RMS,VOLTIN(2048),TM(2048)
	COMMON /ZBLK/ NZER,AMPT,ZCROSS(128)
	COMMON /PLOTDT/ ITERM,PTITLE
	COMMON /HNTBLK/ NHUNT(25)
	COMMON /TCALDATA/ AA(6),BB(6),POFFS(6),XNOFFS(6)
	EXTERNAL FITDATA,FITZERO,FITFRPH
	DIMENSION IZCROSS(4,128),IZCNTS(128)
	DIMENSION X(25),DX(25),Y(25)
	DIMENSION FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
C	DATA ITERM /3/
C	DATA ITERM /-6/		! laser_476 landscape
	DATA ITERM /-2/		! LASER_370 LANDSCAPE
	DATA TWOPI /6.2831853/
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.875/
	DATA SSPS /7500.,1875.,469.,117./
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
	DATA LOLIM /78/			! TM NO. BOUNDARY FOR LO,HI FIT
C	DATA LOLIM /71/			! TM NO. BOUNDARY FOR LO,HI FIT
C	INSERT, FREQ,AMPL,SPS,P/A,MODEL,CHANNEL,FILTER
C	DATA FREQ /1000./
        DATA FREQ /7./
C	DATA FREQ /95./
C	DATA AMPL /4.0/				! AMPLITUDE IN V p-p
C	DATA AMPL /.4/				! AMPLITUDE IN V p-p
	DATA AMPL /2./				! AMPLITUDE IN V p-p
	DATA WCHANNEL /4/
	DATA SPS /7500./			! SAMPLE RATE, PER SEC.
C	DATA BB /.08078,.08066,.08286,.08325,.08268/
C	DATA BB /.08270,.08291,.08286,.08325,.08293,.08293/
	DATA BB /.08270,.08291,.08286,.08325,.0834,.0834/
	DATA AA /4*1.E-3,2*2.E-4/
	DATA POFFS /6*0./
	DATA XNOFFS /6*0./
C
	TYPE *,'TYPE INPUT FREQUENCY IN HZ'	
	READ(5,*) FREQ
	TYPE *,'TYPE AMPLITUDE IN V P-P'	
	READ(5,*) AMPL
	TYPE *,'TYPE CHANNEL NUMBER'	
	READ(5,*) WCHANNEL
	TYPE *,'OK, HERE GOES,FREQ,AMPL,CHAN=',FREQ,AMPL,WCHANNEL	
	WRITE(42,*) 'FOR OFFSETS, LOLIM=',LOLIM
C
	EVENT = 'TDSF'
	IF(WCHANNEL.GE.3) EVENT = 'TDSS'
C	PTITLE(1) = 'ENG. MODEL'
	PTITLE(1) = 'FLIGHT MODEL'
	WRITE(PTITLE(5),1005) FREQ
 1005	FORMAT(F7.2,' HZ')
	WRITE(PTITLE(6),1006) AMPL
	AMPLSV = AMPL
 1006	FORMAT(F5.2,' V P-P')
	PRINT*,'LOLIM=',LOLIM
	PRINT*,' '
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
C	GET STARTED
C
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'

        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min,sec to start, e.g. 041200'
	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  write(6,4)
	  read(5,3,err=10,end=20) iq, NREM
	  type*,nrem
	endif

  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)

	ok = wind_tm_open_channel(tdsch,stream)
	if (.not. ok) stop 'Cannot open tds channel'

c	ok = wind_tm_open_channel(hkch,stream)           !pjk
c	if (.not. ok) stop 'Cannot open hk channel'      !pjk

	ok = wind_tm_get_filename(tdsch,file)

	ok = wind_tm_get_mfmf(tdsch,major,minor)
	if (.not. ok) stop 'cannot get stream position'
c
!           ok = wind_tm_get_next_event(tdsch,major,minor,'HK')
!	   item = 'TEMP_WAVES2'
!	   ok = wind_tm_get_item(tdsch, item, temp_waves, 1, return_size)
C
C	IF OFFLINE, FIND DESIRED TIME
C
        if(stream.ne.'realtime') then
  120    if( wind_tm_eof(tdsch,major,minor)) stop
           ok = wind_tm_get_next_event(tdsch,major,minor,event)
	   item = 'EVENT_SCET'
	   ok = wind_tm_get_item(tdsch, item, scet, 2, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(tdsch, item, itemp, 1, return_size)
	   item = 'CHANNEL'
	   ok = wind_tm_get_item(tdsch, item, tds_channel, 1, return_size)
	   item = 'DPU_MAJOR_FRAME'
	   ok = wind_tm_get_item(tdsch, item, dpumaj, 1, return_size)
	   item = 'DPU_MINOR_FRAME'
	   ok = wind_tm_get_item(tdsch, item, dpumin, 1, return_size)
c	   type 1011,scet,major,minor,tds_channel,dpumaj,dpumin
 1011	   FORMAT(' SCET',I10,I7.6' GSE MF,mf',I9,I4,'  CHANNEL',I3,
     1		'  DPU MF,mf',I8,I4)
	   ihrmn = scet(2)
	   if(ihrmn.lt.nhrmn) then
	     ok = 1
C	     ok = wind_tm_increment_mfmf(major,minor)
C	     if ( .not. ok) type *,' cannot find time'
c	     type*,' s/c time' ,ihrmn
	     if( ok ) go to 120
	   endif
c		write(26,*) 'data collection at ',scet
	   go to 115
	endif
c
	! this is the main program loop

	type*,'going to get next tds event'
C
C	GET NEXT EVENT
C
  110    if( wind_tm_eof(tdsch,major,minor)) stop
c  110	continue
	ok = wind_tm_get_next_event(tdsch,major,minor,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
	      call wind_tm_increment_packet(major,minor)
	      goto 110
	   end if


 115       item = 'CHANNEL'
	   ok = wind_tm_get_item(tdsch, item, tds_channel, 1, return_size)
	   TYPE*,'CHANNEL',tds_channel
	   IF(TDS_CHANNEL.NE.WCHANNEL) GO TO 110
	   lbp = 71
	   if(tds_channel.ge.3) lbp = 81
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(tdsch, item, n_event, 1, return_size)
	   item = 'EVENT_SCET'
	   ok = wind_tm_get_item(tdsch, item, scet, 2, return_size)
	   item = 'DPU_MAJOR_FRAME'
	   ok = wind_tm_get_item(tdsch, item, dpumaj, 1, return_size)
	   item = 'DPU_MINOR_FRAME'
	   ok = wind_tm_get_item(tdsch, item, dpumin, 1, return_size)
	   WRITE(PTITLE(8),1018)  N_EVENT
 1018	   FORMAT('EVENT NO.',I8)
	   WRITE(PTITLE(9),1010) dpumaj,dpumin
 1010	   FORMAT('DPU MFmf ',i6,'.',i3)
C	   type*,'event number',itemp
	   item = 'SLOW_RX_SPEED'
	   ok = wind_tm_get_item(tdsch, item, issps, 1, return_size)
C	   type*,'slow rx speed',issps
	   item = 'FAST_RX_SPEED'
	   ok = wind_tm_get_item(tdsch, item, ifsps, 1, return_size)
C	   type*,'fast rx speed',ifsps
	   item = 'SLOW_RX_FILTER'
	   ok = wind_tm_get_item(tdsch, item, ifils, 1, return_size)
C	   type*,'slow rx filter',ifils
	   item = 'FAST_RX_FILTER'
	   ok = wind_tm_get_item(tdsch, item, ifilf, 1, return_size)
C	   type*,'fast rx filter',ifilf
	   item = 'SOURCE_CHAN_1'
	   ok = wind_tm_get_item(tdsch, item, ichpa(1), 1, return_size)
	   item = 'SOURCE_CHAN_2'
	   ok = wind_tm_get_item(tdsch, item, ichpa(2), 1, return_size)
	   item = 'SOURCE_CHAN_3'
	   ok = wind_tm_get_item(tdsch, item, ichpa(3), 1, return_size)
	   item = 'SOURCE_CHAN_4'
	   ok = wind_tm_get_item(tdsch, item, ichpa(4), 1, return_size)
	   item = 'SOURCE_CHAN_5'
	   ok = wind_tm_get_item(tdsch, item, ichpa(5), 1, return_size)
	   item = 'SOURCE_CHAN_6'
	   ok = wind_tm_get_item(tdsch, item, ichpa(6), 1, return_size)
C
	   WRITE(PTITLE(2),1009)  TDS_CHANNEL
	   ipa = ichpa(tds_channel)
	   WRITE(PTITLE(3),1007) pa(tds_channel,ipa+1)
 1007	   FORMAT('P/A ',A4)
	   IF(TDS_CHANNEL.LE.2) THEN
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(7),1008) FFILTER(IFILF+1)
	   ELSE
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(7),1008) SFILTER(IFILS+1)
	   ENDIF
	      WRITE(PTITLE(4),1004) SPS
 1004	   FORMAT('SPS',F8.0)
 1008	   FORMAT('FILTER',F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

C	   if (ok) type *, 'waves2 temperature', temp_waves2
CW		write(16,*) 'temp in t/m counts',temp_waves
C		write(26,*) 'temp in t/m counts',temp_waves
c	   ok = wind_tm_xlate_item(ch, 'HK', item, temp_waves, title)
c		type*,'temp in degrees?',title
c	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c
C
	ITEM = 'DATA'
	OK = WIND_TM_GET_ITEM(TDSCH,ITEM,NDATA,SIZE,RETURN_SIZE)
	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
C	  PRINT*,'DATA'
C	  PRINT 222, (NDATA(J),J=1,2048)
 222	FORMAT(10I6)
C
C	REMOVE DATA BIAS
C
	MAXTM = 0.
	DO IK = 1,2048
	  NDATA(IK) = NDATA(IK)-128
	  MAXTM = MAX0(MAXTM,NDATA(IK))
	ENDDO
C
C	CALCULATE NOMINAL WAVE PERIOD IN SAMPLES
C
	WPER = SPS/FREQ
C***********
C	FOR FIXED WAVE PERIOD IN SAMPLES, 8 HZ AT HIGHEST SAMPLE RATE, SLOW
	IF(FREQ.LT.80.) WPER = 935.
	PRINT*,'WPER',WPER
	AMPL = AMPLSV
C	IF(MAXTM.LT.60) AMPL = .4
	WRITE(PTITLE(6),1006) AMPL
C
C	FIND FIRST ZERO CROSSING
C
	  IF(NDATA(1).EQ.0) PRINT*,'ZERO DATA AT I = 1'
	DO IL = 2,2047
	  IZ = IL
	  IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL-1),
     1   NDATA(IL),NDATA(IL+1)
	  IF(NDATA(IL)*NDATA(IL+1).LE.0) GO TO 20
	ENDDO
 20	CONTINUE
C	PRINT*,'FIRST ZERO CROSSING AT IZ=',IZ
C
C
C	  CALCULATE ZERO CROSSINGS IN GROUPS FOR UP TO 4 GROUPS
C
	  IZCROSS(1,1) = IZ
	  IZCNT = 1
	  IL1 = IZ+1
	  IL2 = IL1 + WPER/8.
	  IL2 = MIN0(IL2,2047)
	  DO IGP = 1,4
 	    DO IL = IL1,IL2
	      IF(NDATA(IL)*NDATA(IL+1).LE.0) THEN
	        IZCNT = MIN0(IZCNT+1,128)
	        IZCROSS(IGP,IZCNT) = IL
	      ENDIF
	    ENDDO
	    IL1 = IZCROSS(IGP,1) + 3.*WPER/8.
	    IL2 = IL1 + WPER/4.
	    IL2 = MIN0(IL2,2047)
	    IZCNTS(IGP) = IZCNT
	    IZCNT = 0
  	  ENDDO
C
C	  CALCULATE AVERAGES FOR MULTIPLE ZERO CROSSINGS
C
	  IZCNTT = 0
	  DO IGP = 1,4
	    IZCNT = IZCNTS(IGP)
	    PRINT*,'ZEROS',IGP,(IZCROSS(IGP,I),I=1,IZCNT)
	    ZCROSS(IGP) = 0.
	    DO I = 1,IZCNT
	      ZCROSS(IGP) = ZCROSS(IGP) + IZCROSS(IGP,I)
	    ENDDO
	    IF(IZCNT.NE.0) THEN
		ZCROSS(IGP) = ZCROSS(IGP)/IZCNT + .5
		IZCNTT = IZCNTT + 1
	    ENDIF
	  ENDDO
	  PRINT*,'Z CROSSINGS',(ZCROSS(J),J=1,4)
	  IF(IZCNTT.EQ.4) THEN
	    WPER1 = .5*(ZCROSS(3) - ZCROSS(1) + ZCROSS(4) - ZCROSS(2))
	  ELSE
	    WPER1 = ZCROSS(3) - ZCROSS(1) 
	  ENDIF
	  PRINT*,'NEW WPER',WPER1
	  PHI0 = .25*(ZCROSS(1)+ZCROSS(2)+ZCROSS(3)+ZCROSS(4)-WPER1)
	  PRINT*,'PHI0',PHI0
	  NPHI0 = PHI0 + .5
	  IZCNT = IZCNTT
C
C	IF FREQUENCY IS HIGH, THERE WILL BE A LOT OF ZERO CROSSINGS AND
C	ALL WILL BE SINGLE, SO FIND SOME MORE
C
	IF(WPER.LT.820.) THEN
	    PRINT*,'FIND SOME MORE ZEROS'
	    IZCNT = MIN0(IZCNT,4)
	  DO JT = 1,127
	    IL1 = ZCROSS(IZCNT) + 3.*WPER/8.
	    IL2 = IL1 + WPER/4.
	    IL2 = MIN0(IL2,2047)
	    IF(IL1.GT.2047) GO TO 25
C	    PRINT*,'IL1,IL2',IL1,IL2
 	      DO IL = IL1,IL2
	        IF(NDATA(IL)*NDATA(IL+1).LE.0) THEN
		  IZCNT = MIN0(IZCNT+1,128)
	          ZCROSS(IZCNT) = IL+.5
	          IL1 = IL
		  GO TO 22
	        ENDIF
	      ENDDO
 22	      CONTINUE
  	  ENDDO
	ENDIF
C
C	  END OF FURTHER ZERO CROSSINGS FOR HIGH FREQUENCIES
C
 25	  CONTINUE
C
	  PRINT*,IZCNT,' ZEROS FOUND'
	  PRINT*,(ZCROSS(J),J=1,IZCNT)
	  PRINT*,'INTERVALS'
	  IF(IZCNT.GT.1)PRINT*,((ZCROSS(J+1)-ZCROSS(J)),J=1,IZCNT-1)
	  IF(IZCNT.GT.4.AND.ZCROSS(IZCNT).GT.ZCROSS(IZCNT-1)) THEN
            WPER1 = 2.*(ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	    PRINT*,'NEW WPER',WPER1
	  ENDIF
	  X(1) = ZCROSS(1)
	  X(2) = .5*WPER1
	  DX(1) = .4
	  DX(2) = .0005*X(2)
	  NZER = IZCNT
	  NHUNT(1) = 1
	  NHUNT(2) = 1
	  CALL HUNTMN(2,X,DX,Y,FITZERO,SUMSQ)
	  WPER1 = 2.*X(2)
	  PHI0 = X(1) - .25*WPER1	
	  IF(PHI0.LT.0.) PHI0 = X(1) + .25*WPER1	
	  IF(PHI0.LT.0.) PHI0 = X(1) + .75*WPER1	
	  PRINT*,'NEW WPER,PHI0=',WPER1,PHI0
	  PRINT*,'EST. ZEROS',X(1),X(1)+.5*WPER1,X(1)+WPER1
     1     ,X(1)+1.5*WPER1
	  NPHI0 = PHI0 + .5
C
C	ALLOW FOR POSSIBILITY OF NEGATIVE PEAK
C
 	AMPT = AMPL
	IF(NDATA(NPHI0).LT.0) AMPT = -AMPL
	print*,'calc volts',ampt,phi0,wper1
	DO J = 1,2048
	  PHI = (J-PHI0)*TWOPI/WPER1
	  VOLTS(J) = .5*AMPT*COS(PHI)
	ENDDO
C	WRITE(27,1003) (J,VOLTS(J), NDATA(J),J=1,2048)
 1003	FORMAT(3(I9,E11.3,I5))
C
CP	CALL CALPLOT
	IF(ITERM.GT.0) CALL TPLOT
C
C	NOW A SINE WAVE HAS BEEN FITTED THROUGH THE ZERO CROSSINGS AND
C	THE KNOWN AMPLITUDE, SO VOLTAGE INPUT FOR EACH DATA POINT HAS
C	BEEN CALCULATED
C
C	FIT UPPER PART OF CALIBRATION CURVE, WITH ZERO OFFSET
C
	  IF(TDS_CHANNEL.LE.2) THEN
		RATE =  IFSPS
	  ELSE
		RATE = ISSPS
	  ENDIF
	INDEX = 2*TDS_CHANNEL + MIN0(RATE,1) - 1
	INDEX = MIN0(INDEX,6)
	X(1) = 28.
	X(1) = 2.302585/BB(INDEX)
	X(2) = -81.26            	! exdc at 2 hz
	X(2) = -89.23            	! ezdc at 2 hz
	X(2) = -98.60            	! ezdc at 8 hz
C	IF(MAXTM.LT.LOLIM) GO TO 50
C********
C	IF(MAXTM.GT.0) GO TO 50
	NPT = 0
	PRINT*,'LOW BRANCH POINT',LBP
	DO J = 1,2048
C	  IF(IABS(NDATA(J)).GT.LBP.AND.NDATA(J).GT.LOLIM) THEN
C	  IF(IABS(NDATA(J)).GT.LBP) THEN
	  IF(IABS(NDATA(J)).LT.LOLIM) THEN
	    NPT = NPT+1
	    VOLTIN(NPT) = VOLTS(J)
	    TM(NPT) = NDATA(J)
	  ENDIF
	ENDDO
C
	NHUNT(1) = 0
	NHUNT(2) = 0
	NHUNT(3) = 1
	NHUNT(4) = 1
	X(1) = 28.
	X(1) = 2.302585/BB(INDEX)
	X(3) = 0.
	X(2) = -99.
	X(2) = -81.26            	! exdc at 2 hz
	X(2) = -89.23            	! ezdc at 2 hz
	X(2) = -98.60            	! ezdc at 8 hz
	X(3) = 0.
	X(4) = 0.
C
	DX(1) = .02*X(1)
	DX(2) = .02*x(2)
	DX(3) = .001
	DX(4) = .001
	IF(NPT.LE.4) GO TO 30
	CALL FITDATA(X,SUMSQS)
	PRINT*,' '
	PRINT*,' '
	PRINT*,' FIRST TRY AT HIGH FIT, ',NPT,'  POINTS, SUMSQ='
     1  ,SUMSQS
	DO IT = 1,20
	  CALL HUNTMN(4,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),X(4),SUMSQ
	  IF(SUMSQ.GT..9995*SUMSQS) GO TO 30
	  SUMSQS = SUMSQ
	ENDDO
	PRINT*,'NOT ENOUGH ITERATIONS, POOR HIGH FIT'
	  X(1) = PHI0 
	  IF(X(1).LT.0.) X(1) = PHI0 + .25*WPER1	
	  X(2) = WPER1
	  DX(1) = .4
	  DX(2) = .0005*X(2)
	  NZER = IZCNT
	  NHUNT(1) = 1
	  NHUNT(2) = 1
	  CALL FINMIN(2,X,DX,Y,FITFRPH,SUMSQ)
	  CALL FINMIN(2,Y,DX,X,FITFRPH,SUMSQ)
	  WPER1 = X(2)
	  PHI0 = X(1)
	  IF(PHI0.LT.0.) PHI0 = X(1) + .5*WPER1	
	  PRINT*,'NEW WPER,PHI0=',WPER1,PHI0
	  X(1) = 2.302585/BB(INDEX)
	  X(2) = -81.26            	! exdc at 2 hz
	  X(2) = -89.23            	! ezdc at 2 hz
	  X(2) = -98.60            	! ezdc at 8 hz
	  NHUNT(1) = 0
	  NHUNT(2) = 0
 30	CONTINUE
	IF(X(1).NE.0.) VZERO = 10.**(X(2)/X(1))
	IF(NPT.GT.3) THEN
	  RMS = SQRT(SUMSQ/(NPT-2))
	  PRINT*,' VZERO = OLD X(3) =',VZERO,'  RMS ERR',RMS,'  TM UNITS'
	  DO IT = 1,5
	    IF(DX(3).EQ.0.) DX(3) = 1.E-5
	    CALL FINMIN(4,X,DX,Y,FITDATA,SUMSQF)
	    CALL FINMIN(4,Y,DX,X,FITDATA,SUMSQF)
	    TYPE*,' FINMIN GIVES',(X(N),N=1,2),SUMSQF
	  ENDDO
C	  V71 = 10.**((LBP+X(3))/X(1)) - X(2)
C	  PRINT*,'VOLTS TO GIVE TM = BP', V71
	  VZERO = 10.**(X(2)/X(1)) 
	  PRINT*,'VOLTS TO GIVE TM = 0', VZERO
	ELSE
	  PRINT*,'NOT ENOUGH DATA FOR HIGH FIT'
	ENDIF
	  CALL HUNTMN(4,X,DX,Y,FITDATA,SUMSQ)
	  RMS = SQRT(SUMSQ/(NPT-2))
	  PRINT*,'HUNTMN CHECK',X(1),X(2),X(3),X(4),SUMSQ
	GO TO 60
C	
C
C	FIT LOWER PART OF CALIBRATION CURVE, WITH NONZERO OFFSET,SAME SLOPE
C
 50	NPT = 0
	DO J = 1,2048
C	  IF(IABS(NDATA(J)).LE.71.AND.NDATA(J).GT.LOLIM) THEN
C	  IF(IABS(NDATA(J)).LT.LBP) THEN
	  IF(IABS(NDATA(J)).LT.LOLIM) THEN
	    NPT = NPT+1
	    VOLTIN(NPT) = VOLTS(J)
	    TM(NPT) = NDATA(J)
	  ENDIF
	ENDDO
	NHUNT(3) = 1
	NHUNT(4) = 1
	NHUNT(2) = 1
	NHUNT(1) = 0
C	X(2) = 2.3E-2
	X(1) = 2.302585/BB(INDEX)
C**************
	IF(INDEX.EQ.4) X(1) = 28.19
	DX(1) = .05*X(1)
	DX(2) = .02*X(2)
	DX(3) = .01
	DX(4) = .01
	CALL FITDATA(X,SUMSQS)
	PRINT*,' '
	PRINT*,' '
	PRINT*,' FIRST TRY AT LOW FIT,',NPT,'  POINTS, SUMSQ='
     1  ,SUMSQS
	DO IT = 1,20
	  CALL HUNTMN(4,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),X(4),SUMSQ
	  IF(SUMSQ.GT..9995*SUMSQS) GO TO 40
	  SUMSQS = SUMSQ
	ENDDO
	PRINT*,'NOT ENOUGH ITERATIONS, POOR FULL FIT'
 40	CONTINUE
	IF(NPT.GE.4) RMS = SQRT(SUMSQ/(NPT-3))
	PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	IF(X(1).NE.0.) AA(INDEX) = 10.**(X(2)/X(1))
	PRINT*,'INDEX',INDEX
	PRINT*,'AA',AA(INDEX),BB(INDEX)
	POFFS(INDEX) = X(3)
	XNOFFS(INDEX) = X(4)
	X2SAVE = X(2)
	SUMSQS = SUMSQ
	  X(1) = PHI0
	  IF(X(1).LT.0.) X(1) = PHI0 + .25*WPER1
	  X(2) = WPER1
	  DX(1) = .4
	  DX(2) = .0005*X(2)
	  NHUNT(1) = 1
	  NHUNT(2) = 1
	  CALL FINMIN(2,X,DX,Y,FITFRPH,SUMSQ)
	  CALL FINMIN(2,Y,DX,X,FITFRPH,SUMSQ)
	  WPER1 = X(2)
	  PHI0 = X(1) 
	  IF(PHI0.LT.0.) PHI0 = X(1) + .5*WPER1	
	  PRINT*,'NEW WPER,PHI0=',WPER1,PHI0
	  NHUNT(1) = 0
	  X(1) = 2.302585/BB(INDEX)
	  X(2) = X2SAVE
	  DO IT = 1,10
	    CALL FINMIN(4,X,DX,Y,FITDATA,SUMSQF)
	    CALL FINMIN(4,Y,DX,X,FITDATA,SUMSQF)
	    PRINT*,' FINMIN GIVES',(X(N),N=1,4),SUMSQF
	    IF(SUMSQF.GT..9999*SUMSQS) GO TO 47
	    SUMSQS = SUMSQF
	  ENDDO
  47	  V71 = 10.**((LBP+X(2))/X(1)) 
	  IF(X(1).NE.0.) VZERO = 10.**(X(2)/X(1))
	  PRINT*,' '
	  PRINT*,' FINMIN GIVES',(X(N),N=1,4),SUMSQF
	  PRINT*,'VOLTS TO GIVE TM = BP', V71
	  PRINT*,'VOLTS TO GIVE TM = 0', VZERO
	  CALL HUNTMN(4,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN CHECK',X(1),X(2),X(3),X(4),SUMSQ
C
C
 60	NREM = NREM-1
c	CALL CALPLOT
	  DFREQ = SPS/WPER1
	WRITE(42,1042) SCET,DPUMAJ,DPUMIN,TDS_CHANNEL,RATE,
     1   pa(tds_channel,ipa+1),DFREQ,MAXTM,
     1		(X(I),I=1,4),VZERO,RMS
 1042	FORMAT(I9,I7.6,I8,I4,2I3,1X,A4,F6.1,I4,5E12.4,F6.3)
	IF(NREM.GT.0) THEN
C	     ok = wind_tm_increment_mfmf(major,minor)
C	     if ( .not. ok) type *,' cannot find next time'
	     GO TO 110
	ENDIF
	STOP
	END
	SUBROUTINE CALPLOT
C
	CHARACTER*24 TITLE(10)
	CHARACTER*120 JUNK(10)
	CHARACTER*120 STR
	COMMON /HEADBL/ JUNK
	common /headblk/ major,minor,tds_channel,rate,n_event,scet
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	COMMON /PLOTDT/ ITERM,TITLE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	INTEGER*4 MAJOR,MINOR,TDS_CHANNEL,SCET(2)
C
	DIMENSION YY(500),PP(500)
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,330.,3000.,2300.)
	ENDIF
C
C	  WRITE(STR,703) NYR,NDOY,NHRMIN,NFCLKS
C 703	  FORMAT('\\t',I5,' DAY',I4,1X,I4.4,'  S/C CLK=',I12)
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
c	  WRITE(STR,704) SAA()
c 704	  FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c	  CALL MGOPUTLABEL(55,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	CALL MGOTICKSIZE(-1.,0.,2.,10.)  
	YLOLIM = -9.
	CALL MGOSETLIM(-5.,YLOLIM-1.,1.,130.)
	CALL MGOGRID(0)
	CALL MGOSETLTYPE(1)
	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	  DO I = 1,2048
	    IF(VOLTS(I).GT.0.) THEN
	      VOLTL = ALOG10(VOLTS(I))
	      DATPL = NDATA(I)
	      CALL MGORELOCATE(VOLTL,DATPL)
	      CALL MGOSETANGLE(45.)
	      CALL MGOPOINT(4,1)
	      CALL MGOSETANGLE(0.)
	    ELSE
	      VOLTL = ALOG10(-VOLTS(I))
	      DATPL = -NDATA(I)
	      DATPL = AMAX1(DATPL,YLOLIM)
	      CALL MGORELOCATE(VOLTL,DATPL)
	      CALL MGOPOINT(4,1)
	    ENDIF
	  ENDDO
	CALL MGOSETEXPAND(1.)
	  CALL MGOBOX(1,2)
C	  WRITE(STR,704) JHP,PFRFREQ(JHP+1)
C 704	  FORMAT('\\tFREQ',I3,F6.2,'\\t kHz')
	CALL MGOSETEXPAND(.8)
	  CALL MGOXLABEL(5,'VOLTS')
	  CALL MGOYLABEL(14,'T/M NUMBER-128')
	  CALL MGOSETEXPAND(.55)
	  IF(ITERM.GT.0) CALL MGOSETEXPAND(1.)
	  CALL MGORELOCATE(-4.9,129.)
	  CALL MGOLABEL(120,JUNK(2))
	  CALL MGORELOCATE(-4.9,119.)
	  CALL MGOLABEL(18,TITLE(1))
	  CALL MGORELOCATE(-4.5,113.)
	  CALL MGOLABEL(18,TITLE(2))
	  CALL MGORELOCATE(-4.5,107.)
	  CALL MGOLABEL(18,TITLE(3))
	  CALL MGORELOCATE(-4.5,101.)
	  CALL MGOLABEL(18,TITLE(4))
	  CALL MGORELOCATE(-4.5,95.)
	  CALL MGOLABEL(18,TITLE(5))
	  CALL MGORELOCATE(-4.5, 89.)
	  CALL MGOLABEL(18,TITLE(6))
	  CALL MGORELOCATE(-4.5, 81.)
	  CALL MGOLABEL(18,TITLE(7))
	  CALL MGORELOCATE(-4.5, 75.)
	  CALL MGOLABEL(19,TITLE(8))
	  CALL MGORELOCATE(-4.5, 69.)
	  CALL MGOLABEL(19,TITLE(9))
	  CALL MGORELOCATE(-4.6, 63.)
	  WRITE(STR,1001) SCET
 1001     FORMAT('SCET ',I9,I7.6)
	  CALL MGOLABEL(21,STR)
	  CALL MGORELOCATE(-4.6, 57.)
	  CALL MGOLABEL(12,'INPUT POS. ')
	  CALL MGOSETANGLE(45.)
	  CALL MGOPOINT(4,1)
	  CALL MGOSETANGLE(0.)
	  CALL MGORELOCATE(-4.6, 51.)
	  CALL MGOLABEL(12,'INPUT NEG. ')
	  CALL MGOPOINT(4,1)
	CALL MGOSETEXPAND(1.)
C
C
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(JUNK(1),'TDSSINCAL')
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
C	  CALL MGOTIDLE
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  NPLOTS = NPLOTS + 1
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END
	SUBROUTINE TPLOT
C
	CHARACTER*24 TITLE(10)
	CHARACTER*120 JUNK(10)
	CHARACTER*120 STR
	COMMON /HEADBL/ JUNK
	common /headblk/ major,minor,tds_channel,rate,n_event,scet
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	COMMON /PLOTDT/ ITERM,TITLE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	INTEGER*4 MAJOR,MINOR,TDS_CHANNEL,SCET(2)
C
	DIMENSION YY(2100),PP(2100)
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(400.,330.,3000.,2300.)
	ENDIF
C
C	  WRITE(STR,703) NYR,NDOY,NHRMIN,NFCLKS
C 703	  FORMAT('\\t',I5,' DAY',I4,1X,I4.4,'  S/C CLK=',I12)
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
C	  CALL MGOPUTLABEL(53,STR,9)
c	  WRITE(STR,704) SAA()
c 704	  FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c	  CALL MGOPUTLABEL(55,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	CALL MGOTICKSIZE(0.,0.,2.,10.)  
	CALL MGOSETLIM(-5.,-130.,2052.,130.)
	CALL MGOGRID(0)
	CALL MGOSETLTYPE(1)
	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	DO I = 1,2048
	    PP(I) = I-1
	    YY(I) = NDATA(I)
	ENDDO
	CALL MGOCONNECT(PP,YY,2048)
C	  DO I = 1,2048
C	    IF(VOLTS(I).GT.0.) THEN
C	      VOLTL = ALOG10(VOLTS(I))
C	      DATPL = NDATA(I)
C	      CALL MGORELOCATE(VOLTL,DATPL)
C	      CALL MGOSETANGLE(45.)
C	      CALL MGOPOINT(4,1)
C	      CALL MGOSETANGLE(0.)
C	    ELSE
C	      VOLTL = ALOG10(-VOLTS(I))
C	      DATPL = -NDATA(I)
C	      DATPL = AMAX1(DATPL,YLOLIM)
C	      CALL MGORELOCATE(VOLTL,DATPL)
C	      CALL MGOPOINT(4,1)
C	    ENDIF
C	  ENDDO
	CALL MGOSETEXPAND(1.)
	  CALL MGOBOX(1,2)
C	  WRITE(STR,704) JHP,PFRFREQ(JHP+1)
C 704	  FORMAT('\\tFREQ',I3,F6.2,'\\t kHz')
	CALL MGOSETEXPAND(.8)
	  CALL MGOXLABEL(10,'SAMPLE NO.')
	  CALL MGOYLABEL(14,'T/M NUMBER-128')
	  CALL MGOSETEXPAND(.55)
	  IF(ITERM.GT.0) CALL MGOSETEXPAND(1.)
	  CALL MGORELOCATE(-4.9,129.)
	  CALL MGOLABEL(120,JUNK(2))
	  CALL MGORELOCATE(-4.9,119.)
	  CALL MGOLABEL(18,TITLE(1))
	  CALL MGORELOCATE(-4.5,113.)
	  CALL MGOLABEL(18,TITLE(2))
	  CALL MGORELOCATE(-4.5,107.)
	  CALL MGOLABEL(18,TITLE(3))
	  CALL MGORELOCATE(-4.5,101.)
	  CALL MGOLABEL(18,TITLE(4))
	  CALL MGORELOCATE(-4.5,95.)
	  CALL MGOLABEL(18,TITLE(5))
	  CALL MGORELOCATE(-4.5, 89.)
	  CALL MGOLABEL(18,TITLE(6))
	  CALL MGORELOCATE(-4.5, 81.)
	  CALL MGOLABEL(18,TITLE(7))
	  CALL MGORELOCATE(-4.5, 75.)
	  CALL MGOLABEL(19,TITLE(8))
	  CALL MGORELOCATE(-4.5, 69.)
	  CALL MGOLABEL(19,TITLE(9))
	  CALL MGORELOCATE(-4.6, 63.)
	  WRITE(STR,1001) SCET
 1001     FORMAT('SCET ',I9,I7.6)
	  CALL MGOLABEL(21,STR)
C	  CALL MGORELOCATE(-4.6, 57.)
C	  CALL MGOLABEL(12,'INPUT POS. ')
C	  CALL MGOSETANGLE(45.)
C	  CALL MGOPOINT(4,1)
C	  CALL MGOSETANGLE(0.)
C	  CALL MGORELOCATE(-4.6, 51.)
C	  CALL MGOLABEL(12,'INPUT NEG. ')
C	  CALL MGOPOINT(4,1)
	CALL MGOSETEXPAND(1.)
C
C
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(JUNK(1),'TDSSINCAL')
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
C	  CALL MGOTIDLE
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  NPLOTS = NPLOTS + 1
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END
	SUBROUTINE FITDATA(X,SUMSQ)
C
C	FITS T/M NO. = X(1)*ALOG10( VOLTS + VOFFS)/X(2))
C		WITH VOFFS = X(3) FOR + TM AND X(4) FOR - TM
C		ACTUALLY X(2) HERE IS THE -LOG OF X(2) ABOVE
C
	COMMON /FITBLK/ NPT,COUNT,RMS,VOLTS(2048),TM(2048)
	DIMENSION X(25),WT(2048)
	DATA WT /2048*1./
C
	SUMSQ = 0.
	COUNT = 1.E-9
c	X(3) = AMAX1(X(3),1.E-6)
C	X(2) = AMAX1(X(2),0.)
	DO N = 1,NPT
	  TMA = ABS(TM(N))
	  VT = ABS(VOLTS(N))
	  IF(TM(N).GT.0) THEN
	    IF((VT+X(3)).GT.0.) THEN
C	      SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10((VT + X(3))/X(2)))**2
	      SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10(VT + X(3)) + X(2))**2
	      COUNT = COUNT + WT(N)
	    ELSEIF((VT+X(3)).LT.0.) THEN
	      ARG = -(VT+X(3))
	      SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10(ARG) + X(2))**2
	      COUNT = COUNT + WT(N)
	    ELSE
	      SUMSQ = SUMSQ*(1.+2./NPT)
	    ENDIF
	  ELSE
	    IF((VT+X(4)).GT.0.) THEN
	      SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10(VT + X(4)) + X(2))**2
	      COUNT = COUNT + WT(N)
	    ELSEIF((VT+X(4)).LT.0.) THEN
	      ARG = -(VT+X(4))
	      SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10(ARG) + X(2))**2
	      COUNT = COUNT + WT(N)
	    ELSE
	      SUMSQ = SUMSQ*(1.+2./NPT)
	    ENDIF
	  ENDIF
	ENDDO
	RMS = SQRT(SUMSQ/COUNT)
	RETURN
	END
	SUBROUTINE FITZERO(X,SUMSQ)
C
	COMMON /ZBLK/ NPT,AMPT,ZCROSS(128)
	DIMENSION X(25),WT(2048)
	DATA WT /2048*1./
C
	SUMSQ = 0.
	DO N = 1,NPT
	  SUMSQ = SUMSQ + (ZCROSS(N) - X(1) - X(2)*(N-1))**2
	ENDDO
C	PRINT*,'FITZERO',X(1),X(2),SUMSQ
	RETURN
	END
	SUBROUTINE FITFRPH(X,SUMSQ)
C
	COMMON /ZBLK/ NPT,AMPT,ZCROSS(128)
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	common /headblk/ major,minor,tds_channel,rate,n_event,scet
	INTEGER*4  TDS_CHANNEL,RATE,SCET(2)
	DIMENSION X(25),WT(2048)
	DATA TWOPI /6.2831853/
	DATA WT /2048*1./
C
C	X(1) IS THE PHASE AT WHICH THE COS=1, AND X(2) IS THE WAVE
C	PERIOD, IN SAMPLES
C
	WPER = X(2)
	PHI0 = X(1)
	DO J = 1,2048
	  PHI = (J-PHI0)*TWOPI/WPER
	  VOLTS(J) = .5*AMPT*COS(PHI)
	ENDDO
C
C	PRINT*,'VOLTS',(VOLTS(K),K=1,4)
C	PRINT*,'NDATA',(NDATA(K),K=1,4)
C	PRINT*,'TDSCL',TDSCAL(TDS_CHANNEL,RATE,NDATA(1))
C     1  ,TDSCAL(TDS_CHANNEL,RATE,NDATA(2))
	SUMSQ = 0.
	DO J = 1,2048
	  SUMSQ = SUMSQ + (VOLTS(J) - TDSCAL(TDS_CHANNEL,RATE,NDATA(J)))**2
	ENDDO
C	PRINT*,'FITfrph',X(1),X(2),SUMSQ
	RETURN
	END
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	integer*4	iq

  6	format(1x,'Enter TM stream type [O=offline(default), R=realtime ]: ',$)
  5	format(q,a)
  3	format(q,i10)

 10	write(6,6)
	read(5,5,err=10,end=20) iq, stream

	if (iq .lt. 1) then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else
	   ! assume the user entered the name of an offline file
	end if

	get_stream_name = 1

 20	return
	end
	FUNCTION TDSCAL(CHANNEL,SPEED,TM)
C
	COMMON /TCALDATA/ A(6),B(6),POFFS(6),NOFFS(6)
	INTEGER*4 CHANNEL,SPEED,TM,INDEX
	REAL TDSCAL,A,B,POFFS,NOFFS
C	DATA A /.08,.08,.08,.08,.08,.08/
C	DATA B /6*1.E-3/
C	DATA POFFS /6*0./
C	DATA NOFFS /6*0./
C
	INDEX = 2*CHANNEL + MIN0(SPEED,1) - 1
	INDEX = MIN0(INDEX,6)
	kk = index
c	PRINT*,' tdscal',kk
c	PRINT*,A(kk),B(kk),POFFS(kk),NOFFS(kk)
c	PRINT*,A(6)

C
C	INDEX = 1 IS CHANNEL 1, FASTEST, 2 IS CHANNEL 1, SLOWER, 3 IS
C	CHANNEL 2 FASTEST, 4 IS CHANNEL 2 SLOWER, 5 IS CHANNELS 3-6
C
c*************
c	TMNB = TM - 128
	TMNB = TM 
	IF(TMNB.GT.0.) THEN
	  TDSCAL = A(INDEX)*EXP(B(INDEX)*TMNB) - POFFS(INDEX)
	ELSE
	  TDSCAL = -(A(INDEX)*EXP(-B(INDEX)*TMNB) - NOFFS(INDEX))
	ENDIF
	RETURN
	END

