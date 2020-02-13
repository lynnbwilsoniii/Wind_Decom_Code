	PROGRAM SINCAL
C
C	FITS A SINE WAVE TO CALIBRATE TDS A/D CONVERTER
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	parameter	event='TDSF'
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6)
	character*80	stream
	character*4	pa(6,4)
	parameter	size=2048
	integer*4	return_size
	integer*4	tds_channel,ifilf,ifils,ifsps,issps
	integer*4	temp_waves
	character*32	s
	character*32	s_scet
	integer*4	scet(2)
	integer*4	major, minor
	character*80	file
	character*80	title
	character*32	item
	integer*4	ios
	integer*4	NREM,NHRMN,IHRMN
	real ffilter,sfilter,fsps,ssps
!
	common /nrblk/ nrem,NHRMN
	common /headblk/ major,minor,tds_channel,s_scet

C
	CHARACTER*120 JUNK(10)
	CHARACTER*18 PTITLE(10)
	INTEGER*4 TDSCH,hkch
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	COMMON /HEADBL/ JUNK
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /ZBLK/ NZER,ZCROSS(128)
	COMMON /PLOTDT/ ITERM,PTITLE
	COMMON /HNTBLK/ NHUNT(25)
	EXTERNAL FITDATA,FITZERO
	DIMENSION IZCROSS(4,128),IZCNTS(128)
	DIMENSION X(25),DX(25),Y(25)
	DIMENSION FFILTER(4),SFILTER(4),FSPS(4),SSPS(4)
C	DATA ITERM /3/
	DATA ITERM /-6/
	DATA TWOPI /6.2831853/
	DATA FFILTER /50000.,12500.,3125.,781./
	DATA SFILTER /3125.,781.,195.,49./
	DATA FSPS /120.,30.,7.5,1.8/
	DATA SSPS /7500.,1875.,469.,117./
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
	DATA LOLIM /20/
C	INSERT, FREQ,AMPL,SPS,P/A,MODEL,CHANNEL,FILTER
C	DATA FREQ /1000./
	DATA FREQ /95./
C	DATA AMPL /4.0/				! AMPLITUDE IN V p-p
C	DATA AMPL /.4/				! AMPLITUDE IN V p-p
	DATA AMPL /2./				! AMPLITUDE IN V p-p
C	DATA SPS /1875./			! SAMPLE RATE, PER SEC.
	DATA SPS /7500./			! SAMPLE RATE, PER SEC.
C
	PTITLE(3) = 'P/A EXDC'
C	PTITLE(1) = 'ENG. MODEL'
	PTITLE(1) = 'FLIGHT MODEL'
C	PTITLE(2) = 'TDS CH 3'
	WRITE(PTITLE(5),1005) FREQ
 1005	FORMAT(F5.0,' HZ')
	WRITE(PTITLE(6),1006) AMPL
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

	ok = wind_tm_open_channel(tdsch,stream)
	if (.not. ok) stop 'Cannot open tds channel'
	if( ok ) print*,'opened tds channel',tdsch

c	ok = wind_tm_open_channel(hkch,stream)           !pjk
c	if (.not. ok) stop 'Cannot open hk channel'      !pjk

	ok = wind_tm_get_filename(tdsch,file)
	type*, 'file',file

	ok = wind_tm_get_mfmf(tdsch,major,minor)
	if (.not. ok) stop 'cannot get stream position'
	if( ok ) print*,'frame',major,minor
c
	get_tm_stream = 1
c
C           ok = wind_tm_get_next_event(tdsch,major,minor,'HK')
C	   item = 'TEMP_WAVES2'
C	   ok = wind_tm_get_item(tdsch, item, temp_waves, 1, return_size)
C
C	IF OFFLINE, FIND DESIRED TIME
C
        if(stream.eq.'offline') then
 120           ok = wind_tm_get_next_event(tdsch,major,minor,'TDSF')
	   item = 'EVENT_SCET'
	   TYPE*,'AT EVENT_SCET'
	   ok = wind_tm_get_item(tdsch, item, scet, 2, return_size)
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(tdsch, item, itemp, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	TYPE*,'SCET',SCET,' event no', itemp
	   ihrmn = scet(2)/100
	   if(ihrmn.lt.nhrmn) then
	     ok = wind_tm_increment_mfmf(major,minor)
	     if ( .not. ok) type *,' cannot find time'
c	     type*,' s/c time' ,ihrmn
c	     if( ok ) go to 110
	     go to 120
	   endif
c		write(26,*) 'data collection at ',scet
	endif
c
	! this is the main program loop

	type*,'going to get next tds event'
C
C	GET NEXT EVENT
C
c 110    if( wind_tm_eof(tdsch,major,minor)) stop
 110    continue

	ok = wind_tm_get_next_event(tdsch,major,minor,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
C	      if (wind_tm_realtime(ch)) then
C	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
C	         type *, '-reset to latest position: ', major, minor
C	      else
C	         call wind_tm_increment_packet(major,minor)
C	         type *, '-incrementing packet...'
C	      end if
	   end if


	   item = 'CHANNEL'
	   ok = wind_tm_get_item(tdsch, item, tds_channel, 1, return_size)
	TYPE*,'CHANNEL',tds_channel
	   item = 'EVENT_NUMBER'
	   ok = wind_tm_get_item(tdsch, item, itemp, 1, return_size)
	   type*,'event number',itemp
	   item = 'SLOW_RX_SPEED'
	   ok = wind_tm_get_item(tdsch, item, issps, 1, return_size)
	   type*,'slow rx speed',issps
	   item = 'FAST_RX_SPEED'
	   ok = wind_tm_get_item(tdsch, item, ifsps, 1, return_size)
	   type*,'fast rx speed',ifsps
	   item = 'SLOW_RX_FILTER'
	   ok = wind_tm_get_item(tdsch, item, ifils, 1, return_size)
	   type*,'slow rx filter',ifils
	   item = 'FAST_RX_FILTER'
	   ok = wind_tm_get_item(tdsch, item, ifilf, 1, return_size)
	   type*,'fast rx filter',ifilf
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
	      WRITE(PTITLE(4),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(7),1008) FFILTER(IFILF+1)
	   ELSE
	      WRITE(PTITLE(4),1004) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(7),1008) SFILTER(IFILS+1)
	   ENDIF
 1004	   FORMAT('SPS',F8.1)
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
c	  PRINT*,'DATA'
c	  PRINT 222, (NDATA(J),J=1,2048)
 222	FORMAT(10I6)
	DO IK = 1,2048
	  NDATA(IK) = NDATA(IK)-128
	ENDDO
C
C	CALCULATE NOMINAL WAVE PERIOD IN SAMPLES
C
	WPER = SPS/FREQ
	PRINT*,'WPER',WPER
C
C	FIND FIRST ZERO CROSSING
C
	DO IL = 1,2047
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
	  DO IGP = 1,4
	    IZCNT = IZCNTS(IGP)
	    PRINT*,'ZEROS',IGP,(IZCROSS(IGP,I),I=1,IZCNT)
	    ZCROSS(IGP) = 0.
	    DO I = 1,IZCNT
	      ZCROSS(IGP) = ZCROSS(IGP) + IZCROSS(IGP,I)
	    ENDDO
	    IF(IZCNT.NE.0) ZCROSS(IGP) = ZCROSS(IGP)/IZCNT + .5
	  ENDDO
	  PRINT*,'Z CROSSINGS',(ZCROSS(J),J=1,4)
	  WPER1 = .5*(ZCROSS(3) - ZCROSS(1) + ZCROSS(4) - ZCROSS(2))
	  PRINT*,'NEW WPER',WPER1
	  PHI0 = .25*(ZCROSS(1)+ZCROSS(2)+ZCROSS(3)+ZCROSS(4)-2.*WPER1)
	  PRINT*,'PHI0',PHI0
	  NPHI0 = PHI0 + .5
C
C	IF FREQUENCY IS HIGH, THERE WILL BE A LOT OF ZERO CROSSINGS AND
C	ALL WILL BE SINGLE, SO FIND SOME MORE
C
	IF(WPER.LT.820.) THEN
	    PRINT*,'FIND SOME MORE ZEROS'
	  DO JT = 1,127
	    IL1 = ZCROSS(IZCNT) + 3.*WPER/8.
	    IL2 = IL1 + WPER/4.
	    IL2 = MIN0(IL2,2047)
	    IF(IL1.GT.2047) GO TO 25
C	    PRINT*,'IL1,IL2',IL1,IL2
 	      DO IL = IL1,IL2
	        IF(NDATA(IL)*NDATA(IL+1).LE.0) THEN
		  IZCNT = IZCNT+1
	          ZCROSS(IZCNT) = IL+.5
	          IL1 = IL
		  GO TO 22
	        ENDIF
	      ENDDO
 22	      CONTINUE
  	  ENDDO
C
 25	  CONTINUE
C
	  PRINT*,IZCNT,' ZEROS FOUND'
	  PRINT*,(ZCROSS(J),J=1,IZCNT)
	  PRINT*,'INTERVALS'
	  PRINT*,((ZCROSS(J+1)-ZCROSS(J)),J=1,IZCNT-1)
	  WPER1 = 2.*(ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
	  PRINT*,'NEW WPER',WPER1
	  X(1) = ZCROSS(1)
	  X(2) = 2./WPER1
	  DX(1) = .5
	  DX(2) = .05*X(2)
	  CALL HUNTMN(2,X,DX,Y,FITZERO,SUMSQ)
	  WPER1 = 2./X(2)
	  PHI0 = X(1) + .25*WPER1	
	  PRINT*,'NEW WPER,PHI0=',WPER1,PHI0
	  NPHI0 = PHI0 + .5
C
C	  END OF FURTHER ZERO CROSSINGS FOR HIGH FREQUENCIES
C
	ENDIF
C
C
C	ALLOW FOR POSSIBILITY OF NEGATIVE PEAK
C
 	AMPT = AMPL
	IF(NDATA(NPHI0).LT.0) AMPT = -AMPL
	DO J = 1,2048
	  PHI = (J-PHI0)*TWOPI/WPER1
	  VOLTS(J) = .5*AMPT*COS(PHI)
	ENDDO
c	WRITE(27,1003) (J,VOLTS(J), NDATA(J),J=1,2048)
 1003	FORMAT(3(I9,E11.3,I5))
C
C	NOW A SINE WAVE HAS BEEN FITTED THROUGH THE ZERO CROSSINGS AND
C	THE KNOWN AMPLITUDE, SO VOLTAGE INPUT FOR EACH DATA POINT HAS
C	BEEN CALCULATED
C
C	FIT UPPER PART OF CALIBRATION CURVE, WITH ZERO OFFSET
C
	NPT = 0
	DO J = 1,2048
	  IF(IABS(NDATA(J)).GT.71.AND.NDATA(J).GT.LOLIM) THEN
	    NPT = NPT+1
	    VOLTIN(NPT) = ABS(VOLTS(J))
	    TM(NPT) = NDATA(J)
	  ENDIF
	ENDDO
C
	NHUNT(2) = 0
C	X(1) =20./.7238
	X(1) = 31.
	X(2) = 0.
	X(3) = 2.8E-3
	X(3) = X(1)*ALOG10(X(3))
C
	DX(1) = .05*X(1)
	DX(2) = 5.E-3
	DX(3) = .1*X(3)
	IF(NPT.LE.3) GO TO 30
	CALL FITDATA(X,SUMSQS)
	PRINT*,' '
	PRINT*,' '
	PRINT*,' FIRST TRY AT HIGH FIT, +DATA,',NPT,'  POINTS, SUMSQ='
     1  ,SUMSQS
	DO IT = 1,20
	  CALL HUNTMN(3,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	  IF(SUMSQ.GT..9995*SUMSQS) GO TO 30
	  SUMSQS = SUMSQ
	ENDDO
	PRINT*,'NOT ENOUGH ITERATIONS, POOR HIGH FIT'
 30	CONTINUE
	IF(X(1).NE.0.) VZERO = 10.**(X(3)/X(1))
	IF(NPT.GT.3) THEN
	  RMS = SQRT(SUMSQ/(NPT-2))
	  PRINT*,' VZERO = OLD X(3) =',VZERO,'  RMS ERR',RMS,'  TM UNITS'
	  DO IT = 1,5
	    IF(DX(3).EQ.0.) DX(3) = 1.E-5
	    CALL FINMIN(3,X,DX,Y,FITDATA,SUMSQF)
	    CALL FINMIN(3,Y,DX,X,FITDATA,SUMSQF)
	    PRINT*,' FINMIN GIVES',(X(N),N=1,3),SUMSQF
	  ENDDO
	  V71 = 10.**((71.+X(3))/X(1)) - X(2)
	  PRINT*,'VOLTS TO GIVE TM = 71', V71
	ELSE
	  PRINT*,'NOT ENOUGH DATA FOR HIGH, +DATA FIT'
	ENDIF
C
	NPT = 0
	DO J = 1,2048
	  IF(IABS(NDATA(J)).GT.71.AND.NDATA(J).LT.-LOLIM) THEN
	    NPT = NPT+1
	    VOLTIN(NPT) = ABS(VOLTS(J))
	    TM(NPT) = -NDATA(J)
	  ENDIF
	ENDDO
C
	NHUNT(2) = 0
	X(2) = 0.
C
	DX(1) = .05*X(1)
	DX(2) = 5.E-3
	DX(3) = .1*X(3)
	IF(NPT.LE.3) GO TO 35
	CALL FITDATA(X,SUMSQS)
	PRINT*,' '
	PRINT*,' '
	PRINT*,'/FIRST TRY AT HIGH FIT, -DATA,',NPT,'  POINTS, SUMSQ='
     1  ,SUMSQS
	DO IT = 1,20
	  CALL HUNTMN(3,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	  IF(SUMSQ.GT..9995*SUMSQS) GO TO 35
	  SUMSQS = SUMSQ
	ENDDO
	PRINT*,'NOT ENOUGH ITERATIONS, POOR HIGH FIT'
 35	CONTINUE
	VZERO = 10.**(X(3)/X(1))
	IF(NPT.GT.3) THEN
	  RMS = SQRT(SUMSQ/(NPT-2))
	  PRINT*,' VZERO = OLD X(3) =',VZERO,'  RMS ERR',RMS,'  TM UNITS'
	  DO IT = 1,5
	    IF(DX(3).EQ.0.) DX(3) = 1.E-5
	    CALL FINMIN(3,X,DX,Y,FITDATA,SUMSQF)
	    CALL FINMIN(3,Y,DX,X,FITDATA,SUMSQF)
	    PRINT*,' FINMIN GIVES',(X(N),N=1,3),SUMSQF
	  ENDDO
	  V71 = 10.**((71.+X(3))/X(1)) - X(2)
	  PRINT*,'VOLTS TO GIVE TM = 71', V71
	ELSE
	  PRINT*,'NOT ENOUGH DATA FOR HIGH, -DATA FIT'
	ENDIF
C
C
C	FIT LOWER PART OF CALIBRATION CURVE, WITH NONZERO OFFSET
C
	NPT = 0
	DO J = 1,2048
	  IF(IABS(NDATA(J)).LE.71.AND.NDATA(J).GT.LOLIM) THEN
	    NPT = NPT+1
	    VOLTIN(NPT) = ABS(VOLTS(J))
	    TM(NPT) = NDATA(J)
	  ENDIF
	ENDDO
	NHUNT(2) = 1
	X(2) = 2.3E-2
	DX(1) = .05*X(1)
	DX(2) = .1*X(2)
	DX(3) = 1.E-5
	CALL FITDATA(X,SUMSQS)
	PRINT*,' '
	PRINT*,' '
	PRINT*,' FIRST TRY AT FULL FIT, +DATA,',NPT,'  POINTS, SUMSQ='
     1  ,SUMSQS
	DO IT = 1,20
	  CALL HUNTMN(3,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	  IF(SUMSQ.GT..9995*SUMSQS) GO TO 40
	  SUMSQS = SUMSQ
	ENDDO
	PRINT*,'NOT ENOUGH ITERATIONS, POOR FULL FIT'
 40	CONTINUE
	VZERO = 10.**(X(3)/X(1))
	IF(NPT.GE.4) RMS = SQRT(SUMSQ/(NPT-3))
	PRINT*,' VZERO = OLD X(3) =',VZERO,'  RMS ERR',RMS,'  TM UNITS'
	PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	SUMSQS = SUMSQ
	  DO IT = 1,10
	    CALL FINMIN(3,X,DX,Y,FITDATA,SUMSQF)
	    CALL FINMIN(3,Y,DX,X,FITDATA,SUMSQF)
	    PRINT*,' FINMIN GIVES',(X(N),N=1,3),SUMSQF
	    IF(SUMSQF.GT..9999*SUMSQS) GO TO 47
	    SUMSQS = SUMSQF
	  ENDDO
  47	  V71 = 10.**((71.+X(3))/X(1)) - X(2)
	  PRINT*,' '
	  PRINT*,' FINMIN GIVES',(X(N),N=1,3),SUMSQF
	  PRINT*,'VOLTS TO GIVE TM = 71', V71
C
	NPT = 0
	DO J = 1,2048
	  IF(IABS(NDATA(J)).LE.71.AND.NDATA(J).LT.-LOLIM) THEN
	    NPT = NPT+1
	    VOLTIN(NPT) = ABS(VOLTS(J))
	    TM(NPT) = -NDATA(J)
	  ENDIF
	ENDDO
	NHUNT(2) = 1
	DX(1) = .05*X(1)
	DX(2) = .1*X(2)
	DX(3) = 1.E-5
	CALL FITDATA(X,SUMSQS)
	PRINT*,' '
	PRINT*,' '
	PRINT*,' FIRST TRY AT FULL FIT, -DATA,',NPT,'  POINTS, SUMSQ='
     1  ,SUMSQS
	DO IT = 1,20
	  CALL HUNTMN(3,X,DX,Y,FITDATA,SUMSQ)
	  PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	  IF(SUMSQ.GT..9995*SUMSQS) GO TO 45
	  SUMSQS = SUMSQ
	ENDDO
	PRINT*,'NOT ENOUGH ITERATIONS, POOR FULL FIT'
 45	CONTINUE
	VZERO = 10.**(X(3)/X(1))
	IF(NPT.GE.4) RMS = SQRT(SUMSQ/(NPT-3))
	PRINT*,' VZERO = OLD X(3) =',VZERO,'  RMS ERR',RMS,'  TM UNITS'
	PRINT*,'HUNTMN OUTPUT',X(1),X(2),X(3),SUMSQ
	SUMSQS = SUMSQ
	  DO IT = 1,10
	    CALL FINMIN(3,X,DX,Y,FITDATA,SUMSQF)
	    CALL FINMIN(3,Y,DX,X,FITDATA,SUMSQF)
	    PRINT*,' FINMIN GIVES',(X(N),N=1,3),SUMSQF
	    IF(SUMSQF.GT..9999*SUMSQS) GO TO 48
	    SUMSQS = SUMSQF
	  ENDDO
  48	  PRINT*,' '
	  PRINT*,' FINMIN GIVES',(X(N),N=1,3),SUMSQF
	  V71 = 10.**((71.+X(3))/X(1)) - X(2)
	  PRINT*,'VOLTS TO GIVE TM = 71', V71
	  PRINT*,' '
	CALL CALPLOT
	STOP
	END
	SUBROUTINE CALPLOT
C
	CHARACTER*18 TITLE(10)
	CHARACTER*120 JUNK(10)
	CHARACTER*120 STR
	COMMON /HEADBL/ JUNK
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
	  CALL MGOYLABEL(10,'T/M NUMBER')
	  CALL MGOSETEXPAND(.55)
	  CALL MGORELOCATE(-4.9,129.)
	  CALL MGOLABEL(120,JUNK(2))
	  CALL MGORELOCATE(-4.9,119.)
	  CALL MGOLABEL(120,JUNK(1))
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
	  CALL MGORELOCATE(-4.6, 75.)
	  CALL MGOLABEL(12,'INPUT POS. ')
	  CALL MGOSETANGLE(45.)
	  CALL MGOPOINT(4,1)
	  CALL MGOSETANGLE(0.)
	  CALL MGORELOCATE(-4.6, 71.)
	  CALL MGOLABEL(12,'INPUT NEG. ')
	  CALL MGOPOINT(4,1)
	CALL MGOSETEXPAND(1.)
C
C
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(TITLE(8),'SINCAL')
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
C	FITS T/M NO. = X(1)*ALOG10( VOLTS + VOFFS)/X(3))
C		WITH VOFFS = X(2)
C
	COMMON /FITBLK/ NPT,VOLTS(2048),TM(2048)
	DIMENSION X(25),WT(2048)
	DATA WT /2048*1./
C
	SUMSQ = 0.
c	X(3) = AMAX1(X(3),1.E-6)
C	X(2) = AMAX1(X(2),0.)
	DO N = 1,NPT
	  TMA = ABS(TM(N))
	  VT = ABS(VOLTS(N))
	  IF((VT+X(2)).GT.0.) THEN
C	    SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10((VT + X(2))/X(3)))**2
	    SUMSQ = SUMSQ + WT(N)*(TMA - X(1)*ALOG10(VT + X(2)) + X(3))**2
	  ELSE
	    SUMSQ = SUMSQ*(1.+2./NPT)
	  ENDIF
	ENDDO
	RETURN
	END
	SUBROUTINE FITZERO(X,SUMSQ)
C
	COMMON /ZBLK/ NPT,ZCROSS(128)
	DIMENSION X(25),WT(2048)
	DATA WT /2048*1./
C
	SUMSQ = 0.
	DO N = 1,NPT
	  SUMSQ = SUMSQ + (ZCROSS(N) - X(1) - X(2)*(N-1))**2
	ENDDO
	RETURN
	END
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	common /nrblk/ nrem,NHRMN
	integer*4	iq,NREM,NHRMN

  6	format(1x,'Enter TM stream type [O=offline(default), R=realtime ]: ',$)
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
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

        if(stream.eq.'offline') then
	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,3,err=10,end=20) iq, NHRMN
	  type*,NHRMN
C	  write(6,4)
C	  read(5,3,err=10,end=20) iq, NREM
C	  type*,nrem
	endif

 20	return
	end
	SUBROUTINE OLDATA
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	CHARACTER*120 JUNK(10)
	DO IL = 1,6
	  READ(19,1002) JUNK(IL)
	  PRINT *,IL,JUNK(IL)
	ENDDO
C
	DO IL = 1,103
	  IK2 = 20*IL
	  IK1 = IK2-19
	  READ(19,1001,END=200) LINNO,(NDATA(IK),IK=IK1,IK2)
C	  PRINT*,'DATA',LINNO,NDATA(IK1),NDATA(IK2)
	ENDDO
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
  200	RETURN
	END
