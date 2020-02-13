!  sample wind/waves tds data collection program

	program		get_tds
	implicit	integer*4 (a-z)

	ok = 1
	if (ok) ok = get_tm_stream()
	TYPE*,'GOING TO WAIT FOR AN EVENT'
	if (ok) ok = wait_for_events()


	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_tm_stream()
	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	parameter	event='TDS'
	integer*4	i,j,k,n
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*16	stream
	parameter	size=1024
	integer*4	return_size
	integer*4	raw(size)
	integer*4	mantissa(size)
	integer*4	gain(size)
	integer*4	tds_channel
	integer*4	temp_waves
	character*32	s
	character*32	s_scet
	integer*4	scet(2)
	integer*4	ch, chhk, major, minor
	character*80	file
	character*80	title
	character*32	item
	integer*4	ios
	integer*4	NREM,NHRMN,IHRMN
!

	common /nrblk/ nrem,NHRMN
	common /fftblk/ gain,mantissa
	common /headblk/ major,minor,tds_channel,s_scet
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'

	ok = wind_tm_open_channel(ch,stream)
	if (.not. ok) stop 'cannot open tm channel'

c	ok = wind_tm_open_channel(chhk,stream)           !pjk
c	if (.not. ok) stop 'cannot open tm channel'      !pjk

	ok = wind_tm_get_filename(ch,file)

	ok = wind_tm_get_mfmf(ch,major,minor)
	if (.not. ok) stop 'cannot get stream position'

	get_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events()

 110      ok = wind_tm_get_next_event(ch,major,minor,'HK')

	   item = 'EVENT_SCET'
	   ok = wind_tm_get_item(ch, item, scet, 2, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   ihrmn = scet(2)/100
	   if(ihrmn.lt.nhrmn) then
	     ok = wind_tm_increment_mfmf(major,minor)
	     if ( .not. ok) type *,' cannot find time'
	     type*,' s/c time' ,ihrmn
	     if( ok ) go to 110
	   endif
		write(26,*) 'data collection at ',scet

	   item = 'TEMP_WAVES2'
	   ok = wind_tm_get_item(ch, item, temp_waves, 1, return_size)
	   if (ok) type *, 'waves2 temperature', temp_waves
CW		write(16,*) 'temp in t/m counts',temp_waves
		write(26,*) 'temp in t/m counts',temp_waves
c	   ok = wind_tm_xlate_item(ch, 'HK', item, temp_waves, title)
c		type*,'temp in degrees?',title
c	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c
	! this is the main program loop

	do while(.not. wind_tm_eof(ch,major,minor))
 100	   continue

           ok = wind_tm_get_next_event(ch,major,minor,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
	      if (wind_tm_realtime(ch)) then
	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
	         type *, '-reset to latest position: ', major, minor
	      else
	         call wind_tm_increment_packet(major,minor)
	         type *, '-incrementing packet...'
	      end if
	      err_count = err_count + 1
	      if (err_count .lt. 2) goto 100
	      if (err_count .ge. 2) goto 200
	   end if

	   ! now get the items

	   item = 'CHANNEL_NUMBER'
	   ok = wind_tm_get_item(ch, item, tds_channel, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'POWER_SPECTRUM'
	   ok = wind_tm_get_item(ch, item, raw, size, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'MANTISSA'
	   ok = wind_tm_get_item(ch, item, mantissa, size, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'EXPONENT'
	   ok = wind_tm_get_item(ch, item, gain, size, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   item = 'EVENT_SCET'
	   ok = wind_tm_get_item(ch, item, scet, 2, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok

	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

	   ! type the report
C	   type *, '--------------------------------------------------'
C	   type *, 'TDS Ch ', tds_channel
	   type *, 'SCET ', s_scet
C	   type *, 'Offline file: ', file
C	   do i=1,size
C	      type *, i, raw(i), mantissa(i), gain(i)
C	   end do
CW	  WRITE(16,*) ' '
CW	  WRITE(16,*) ' '
CW	  WRITE(16,*) 'CALLING WINDFFT'
CW	   PRINT*,' '
CW	   PRINT*,' '
CW	   PRINT*,'CALLING WINDFFT'
CW	   CALL WINDFFT
	   CALL WINDFFT
CO	   CALL WINDOFFS(0)
	   NREM = NREM-1
CO	   IF(NREM.LE.0) CALL WINDOFFS(1)
	   IF(NREM.LE.0) STOP
c	   IF(1) STOP

	end do

 200	continue
CO	      IF(NREM.GT.0) CALL WINDOFFS(1)
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	common /nrblk/ nrem,NHRMN
	integer*4	iq,NREM,NHRMN

  6	format(1x,'Enter TM stream type [O=offline, R=realtime (default)]: ',$)
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)

 10	write(6,6)
	read(5,5,err=10,end=20) iq, stream

	if (iq .lt. 1) then
	   stream = 'realtime'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else
	   ! assume the user entered the name of an offline file
	end if

	get_stream_name = 1

	write(6,*)  'type hr,min to start, e.g. 0412'
	read(5,3,err=10,end=20) iq, NHRMN
	type*,NHRMN
	write(6,4)
	read(5,3,err=10,end=20) iq, NREM
	type*,nrem

 20	return
	end
	SUBROUTINE RAWPRINT
C
	common /fftblk/ nexpt(1024),ndata(1024)
	common /headblk/ major,minor,ich,scet
	character*32 scet
C
	write(36,*) 'fft data collection at ',scet,' channel',ich
C
	DO I = 1,103
	  I2 = 10*I
	  I1 = I2 - 9
	  I2 = MIN0(I2,1024)
	  WRITE(36,1006) I1-1,(NEXPT(J),NDATA(J),J=I1,I2)
	ENDDO
C
	RETURN
 1006	FORMAT(I5,'.',(10(I3,I5)))
	END
	SUBROUTINE TPLOT
C
C	PLOTS THE TIME DOMAIN DATA
C
	COMMON /SPBLK/ PWRW(512),PWRNW(512),FDATA(1024),DATA(1024)
	COMMON /PLOTDT/ ITERM,FFTAVR,NDRV,AMP,IWINDW,STRG
	common /headblk/ major,minor,ifftch,scet
	character*32 scet
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
	DIMENSION PTIME(1024),YY(1024)
	CHARACTER*8 TITLE(4)
	CHARACTER*120 STRG(5)
	CHARACTER*120 STR
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/
C
C	PRINT*,' IN TPLOT',NPT
	DO JF = 1,1024
	  PTIME(JF) = JF
	ENDDO
C
c	ITERM = 3
	ITERM = -6
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  PRINT*,'START TPLOT',GX1,GX2,GY1,GY2
C	  CALL MGOSETLOC(500.,330.,GY2-200.,3000.)
	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-200.)
	ENDIF
C
	  CALL MGOSETEXPAND(.8)
	  TOP = LY2
	  CALL MGOGRELOCATE(GX1,TOP-50.)                    
	  CALL MGOPUTLABEL(6,'SCET   ',3)
	  CALL MGOPUTLABEL(79,SCET,3)
	  CALL MGOPUTLABEL(20,'  s/c major,minor frame ',3)
	  WRITE(STR,801) MAJOR,MINOR
 801	  FORMAT(I6,'.',I3.3)
 802	  FORMAT(2I6)
	  CALL MGOPUTLABEL(14,STR,3)
	  CALL MGOGRELOCATE(GX1,TOP-90.)        
	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3)
	  WRITE(STR,802) IFFTCH
	  CALL MGOPUTLABEL(8,STR,3)
C	  IF(ITERM.GT.0) THEN
C	    CALL MGOGRELOCATE(10.,0.)                    
C	  ELSE
C	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy
C	  ENDIF
	  CALL MGOSETEXPAND(1.)
C
	  XMIN = -20.
	  XMAX = 1045.
C
	  YMIN = DATA(1)
	  YMAX = YMIN
	  JP = 1
	  DO J = 1,1024
	    JP = J
	    YY(JP) = DATA(J)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOTICKSIZE(20.,100.,0.,0.)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PTIME,YY,JP)
	  CALL MGOSETEXPAND(.8)
	  CALL MGOBOX(1,2)
	  CALL MGOXLABEL(10,'SAMPLE NO.')
	  CALL MGOYLABEL(18,'T/M DATA WITH GAIN')
	  CALL MGOSETEXPAND(1.)
C
	CALL MGOSETEXPAND(.7)
	CALL MGOPLOTID('WINDFFT','TPLOT')
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
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
	PROGRAM SINCAL
C
C	FITS A SINE WAVE TO CALIBRATE TDS A/D CONVERTER
C
	CHARACTER*120 JUNK(10)
	CHARACTER*14 TITLE(10)
	COMMON /CALBLK/ VOLTS(2048),NDATA(2060)
	COMMON /HEADBL/ JUNK
	COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
	COMMON /ZBLK/ NZER,ZCROSS(128)
	COMMON /PLOTDT/ ITERM,TITLE
	COMMON /HNTBLK/ NHUNT(25)
	EXTERNAL FITDATA,FITZERO
	DIMENSION IZCROSS(4,128),IZCNTS(128)
	DIMENSION X(25),DX(25),Y(25)
C	DATA ITERM /3/
	DATA ITERM /-6/
	DATA TWOPI /6.2831853/
	DATA LOLIM /20/
C	INSERT, FREQ,AMPL,SPS,P/A,MODEL,CHANNEL,FILTER
C	DATA FREQ /1000./
	DATA FREQ /7./
C	DATA AMPL /4.0/				! AMPLITUDE IN V p-p
C	DATA AMPL /.4/				! AMPLITUDE IN V p-p
	DATA AMPL /.5/				! AMPLITUDE IN V p-p
C	DATA SPS /1875./			! SAMPLE RATE, PER SEC.
	DATA SPS /7500./			! SAMPLE RATE, PER SEC.
C
	TITLE(3) = 'P/A EXDC'
C	TITLE(1) = 'ENG. MODEL'
	TITLE(1) = 'FLIGHT MODEL'
C	TITLE(2) = 'TDS CH 3'
C	WRITE(TITLE(4),1004) SPS
 1004	FORMAT('SPS',F8.0)
	WRITE(TITLE(5),1005) FREQ
 1005	FORMAT(F5.0,' HZ')
	WRITE(TITLE(6),1006) AMPL
 1006	FORMAT(F5.2,' V P-P')
	TITLE(7) = 'FILTER 3.125 KHZ'
	TITLE(8) = 'SINCAL57.DAT'
	PRINT*,'LOLIM=',LOLIM
	PRINT*,' '
	PRINT*,TITLE(8)
C
c	OPEN(UNIT=19,NAME=TITLE(8),TYPE='OLD',READONLY)
C	OPEN(UNIT=19,NAME='NOISE2.DAT',TYPE='OLD')
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
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
200	CONTINUE
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
	    ZCROSS(IGP) = ZCROSS(IGP)/IZCNT + .5
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
	WRITE(27,1003) (J,VOLTS(J), NDATA(J),J=1,2048)
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
	CHARACTER*14 TITLE(10)
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
C	  CALL MGORELOCATE(-4.5,113.)
C	  CALL MGOLABEL(12,TITLE(2))
C	  CALL MGORELOCATE(-4.5,107.)
C	  CALL MGOLABEL(10,TITLE(3))
	  CALL MGORELOCATE(-4.5,101.)
	  CALL MGOLABEL(10,TITLE(4))
	  CALL MGORELOCATE(-4.5,95.)
	  CALL MGOLABEL(10,TITLE(5))
	  CALL MGORELOCATE(-4.5, 89.)
	  CALL MGOLABEL(12,TITLE(6))
	  CALL MGORELOCATE(-4.5, 81.)
	  CALL MGOLABEL(14,TITLE(7))
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

