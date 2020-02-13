!  sample wind/waves fft data collection program

	program		get_fft
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
	parameter	event='FFTH'
	integer*4	i,j,k,n
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*80	stream
	parameter	size=1024
	integer*4	return_size
	integer*4	raw(size)
	integer*4	mantissa(size)
	integer*4	gain(size)
	integer*4	fft_channel
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
	common /headblk/ major,minor,fft_channel,s_scet
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
	   ok = wind_tm_get_item(ch, item, fft_channel, 1, return_size)
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
C	   type *, 'FFT Ch ', fft_channel
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
	SUBROUTINE WINDFFT
C
C	PLOTS WIND 'STRAIGHT THROUGH' FFT DATA AND DOES FFT--LINK WITH FASFOR
C
	CHARACTER*14 TITLE(10)
	CHARACTER*128 STR(7)
C	COMMON /SHBLK/ PTIME(64),PWRL(64,511)
	COMMON /SPBLK/ PWRW(512),PWRNW(512),FDATA(1024),DATA(1024)
	COMMON /PLOTDT/ ITERM,FFTAVR,NDRV,AMP,IWINDW,STR
	COMMON /WINBLK/ WINDOW(1024)
c
	common /fftblk/ nexpt(1024),ndata(1024)
	common /headblk/ major,minor,ifftch,scet
	character*32 scet
c
	DOUBLE PRECISION GAVR(4),GSTD(4)
	DIMENSION AR(1024),AI(1024),NGHIST(4),OFFS(4,10)
	DATA TWOPI /6.2831853/
	DATA VPSTEP/ .6E-6/              ! VERY ROUGH
	DATA WINDOW /1024*1./
c	DATA OFFS /2048. ,2048. ,2044. ,2051.,   ! CH 1 GN STP 0-3
c     2             2048. ,2048. ,2046. ,2039.,   ! CH 2 GN STP 0-3
c     3             2048. ,2048. ,2038. ,2047.,   ! etc.
c     4             2048. ,2048. ,2038. ,2048.,
c     5             2048. ,2048. ,2040. ,2043.,
c     6             2048. ,2048. ,2041. ,2063.,
c     7             2032. ,2032. ,2032. ,2032.,
c     8             2035. ,2035. ,2035. ,2035.,
c     9             2033. ,2033. ,2033. ,2033.,
c     X             2250. ,2250. ,2250. ,2250./
	DATA OFFS /40*2048./
C	DATA GAIN(CHANNEL,INPUT)   !  INPUTS ARE ORDERED XYZ,AC, DC, B
C				   !  NO CHANNEL HAS MORE THAN 3 POSS. INPUTS
C
	NP = 10
	NSIZE = 64                        ! FIRST DIMENSION OF PWRL
	NPROS = 2**NP
	NFRQ = NPROS/2
C
	IWINDW = 1
	CALL FWINDW(IWINDW)

C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
 1003	FORMAT(' scet',A,'  s/c maj,min frame',I6,'.',I3.3)
C 1003	FORMAT(' S/C MAJOR,MINOR FRAME ',I6,'.',I3.3)
 1004	FORMAT(A15,4F12.3)
	IL = 1
C	PRINT 1003,MAJOR,MINOR
	PRINT *,' FFT CHANNEL',IFFTCH
	PRINT *,' scet ',SCET
	print*,'1st,last data',ndata(1),ndata(1024)
	print*,'1st,last expt',nexpt(1),nexpt(1024)
C
	WRITE(16,1003) SCET,major,minor
	WRITE(16,*)' FFT CHANNEL',IFFTCH
	WRITE(16,*) '1st,last expt,data',nexpt(1),ndata(1),
     1   nexpt(1024),ndata(1024)
C
C	COUNT NUMBER OF OCCURENCES OF EACH GAIN STEP
C
	DO NEX = 1,4
	  GAVR(NEX) = 0.D00
	  GSTD(NEX) = 0.D00
	  NGHIST(NEX) = 0
	ENDDO
	DO IK = 1,1024
	  NEX = NEXPT(IK) + 1
	  NGHIST(NEX) = NGHIST(NEX)+1
	  GAVR(NEX) = GAVR(NEX) + NDATA(IK)
	  GSTD(NEX) = GSTD(NEX) + NDATA(IK)**2
	ENDDO
	PRINT*,'GAIN HISTOGRAM',NGHIST
	DO NEX = 1,4
	  IF(NGHIST(NEX).NE.0) GAVR(NEX) = GAVR(NEX)/NGHIST(NEX)
	  IF(NGHIST(NEX).GT.1) THEN
	    GSTD(NEX) = GSTD(NEX)/DFLOAT(NGHIST(NEX)) - GAVR(NEX)**2
	    GSTD(NEX) = DMAX1(GSTD(NEX),0.D00)
	    GSTD(NEX) = DSQRT(GSTD(NEX)/(NGHIST(NEX)-1))
	  ENDIF
	ENDDO
	WRITE(17,5676) IFFTCH,NGHIST,(GAVR(JJ),GSTD(JJ),JJ=1,4)
 5676	FORMAT(I3,4I5,4(F7.1,'+-',F5.2))
	WRITE(16,*) 'GAIN HISTOGRAM',NGHIST
	WRITE(16,1004) 'GAIN AVRS',GAVR
	WRITE(16,1004)'GAIN STDS',GSTD
C
C	16 BIT WORDS.  12 LSB'S ARE 0-4095 MANTISSA, THEN 2 BIT EXPONENT
C	THEN 2 BLANK BITS
C
	DAMAX = -1.E7
	DAMIN =  1.E7
	DO IK = 1,1024
	  NTEMP = NDATA(IK).AND.4095
C	  NEXPT(IK) = (NDATA(IK)/2048).AND.3
	  NEX = NEXPT(IK).AND.3
	  TEMP = -(NTEMP-OFFS(NEX+1,IFFTCH))
C	  TEMP = -(NTEMP-2048)
	  DATA(IK) = TEMP*(-16.)**NEX
	  FDATA(IK) = ABS(TEMP)+2048.*NEX
	  IF(DATA(IK).LT.0.) FDATA(IK) = -FDATA(IK)
	  IF(FDATA(IK).GT.DAMAX) THEN
	    MAXEXP = NEXPT(IK)
	    MAXDAT = NDATA(IK)
	    DAMAX = FDATA(IK)
	  ENDIF
	  IF(FDATA(IK).LT.DAMIN) THEN
	    MINEXP = NEXPT(IK)
	    MINDAT = NDATA(IK)
	    DAMIN = FDATA(IK)
	  ENDIF
	ENDDO
	AVR = .5*(MAXDAT + MINDAT)
	DIFF = .5*(MAXDAT - MINDAT)
C
	WRITE(16,*) 'min,max data,expt',mindat,minexp,maxdat,maxexp
	WRITE(16,*) 'average,pk sig',avr,diff
C	WRITE(19,*) ' '
C	WRITE(19,*) 'ch,average,pk sig',ifftch,avr,diff
C
C	CALL RAWPRINT
C
	  DO N = 0,NPROS-1
	    AR(N+1) = VPSTEP*DATA(N+1)
	    AI(N+1) = 0.
	  ENDDO
	  CALL FASFOR(NP,AR,AI,PWRNW)
	  PRINT*,'NO WINDOW,AR1,AI1',AR(1),AI(1)
	  WRITE(16,*) 'NO WINDOW,AR1,AI1',AR(1),AI(1)
	  FFTAVR = AR(1)/1024.
	  PRINT*, 'AVERAGE FROM OFFSET',FFTAVR
	  WRITE(16,*) 'AVERAGE FROM OFFSET',FFTAVR
C
	  DO N = 0,NPROS-1
	    AR(N+1) = WINDOW(N+1)*DATA(N+1)
	    AI(N+1) = 0.
	  ENDDO
	  CALL FASFOR(NP,AR,AI,PWRW)
	  PRINT*,'   WINDOW,AR1,AI1',AR(1),AI(1)
	  WRITE(16,*) '   WINDOW,AR1,AI1',AR(1),AI(1)
	  AVR = AR(1)/512.
	  PRINT*, '2*AVERAGE FROM OFFSET',AVR
	  WRITE(16,*) '2*AVERAGE FROM OFFSET',AVR
C	    CALL SHPLOT
	    CALL TABLE
C	    CALL SPPLOT
	    iterm = -5
	    CALL TPLOT
C
C	PWR(NHARM) WILL BE AMP**2/2.
C
	RETURN
	END
	SUBROUTINE WINDOFFS(IPRINT)
C
C	CALCULATES OFFSETS FROM A SINE WAVE TEST
C
	CHARACTER*128 STR(7)
	COMMON /SPBLK/ PWRW(512),PWRNW(512),FDATA(1024),DATA(1024)
	COMMON /PLOTDT/ ITERM,FFTAVR,NDRV,AMP,IWINDW,STR
c
	common /fftblk/ nexpt(1024),ndata(1024)
	common /headblk/ major,minor,ifftch,scet
	character*32 scet
c
C	10 CHANNELS, 4 GAINS, 9 EXAMPLES OF EACH
	DIMENSION OFFSTR(10,4,13),NCOUNT(10,4)            ! HOLDS UP TO 9 VALUES
	DATA NCOUNT /40*0/
	DATA TWOPI /6.2831853/
C
	IF(IPRINT.NE.0) THEN
	  DO ICH = 1,6 
	    DO NE = 1,4
	      NP = NCOUNT(ICH,NE) 
	      PRINT 1005,ICH,NE-1,(OFFSTR(ICH,NE,N),N=1,NP)
	      WRITE(26,1005) ICH,NE-1,(OFFSTR(ICH,NE,N),N=1,NP)
 1005	      FORMAT(I4,I3,13F8.1)
	    ENDDO
	  ENDDO
	  NE = 4
	  DO ICH = 7,9
	    NP = NCOUNT(ICH,NE) 
	    PRINT 1005,ICH,NE-1,(OFFSTR(ICH,NE,N),N=1,NP)
	    WRITE(26,1005) ICH,NE-1,(OFFSTR(ICH,NE,N),N=1,NP)
	  ENDDO
	  NE = 3
	  ICH = 10
	  NP = NCOUNT(ICH,NE) 
	  PRINT 1005,ICH,NE-1,(OFFSTR(ICH,NE,N),N=1,NP)
	  WRITE(26,1005) ICH,NE-1,(OFFSTR(ICH,NE,N),N=1,NP)
	ENDIF
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
 1003	FORMAT(' scet',a32,'  s/c maj,min frame',I6,'.',I3.3)
 1004	FORMAT(A15,4F12.3)
	PRINT 1003,MAJOR,MINOR
	PRINT *,' FFT CHANNEL',IFFTCH
	PRINT *,' scet ',SCET
	print*,'1st,last data',ndata(1),ndata(1024)
	print*,'1st,last expt',nexpt(1),nexpt(1024)
C
C	WRITE(16, 1003) MAJOR,MINOR
C	WRITE(16,*)' FFT CHANNEL',IFFTCH
C	WRITE(16,*) ' scet ',SCET
C	WRITE(16,*) '1st,last data',ndata(1),ndata(1024)
C	WRITE(16,*)'1st,last expt',nexpt(1),nexpt(1024)
C
C
	DAMAX = -1.E7
	DAMIN =  1.E7
	DO IK = 1,1024
	  NTEMP = NDATA(IK).AND.4095
	  TEMP = -(NTEMP-2048)
	  DATA(IK) = TEMP*(-16.)**NEXPT(IK)
	  FDATA(IK) = ABS(TEMP)+2048.*NEXPT(IK)
	  IF(DATA(IK).LT.0.) FDATA(IK) = -FDATA(IK)
	  IF(FDATA(IK).GT.DAMAX) THEN
	    MAXEXP = NEXPT(IK)
	    MAXDAT = NDATA(IK)
	    DAMAX = FDATA(IK)
	  ENDIF
	  IF(FDATA(IK).LT.DAMIN) THEN
	    MINEXP = NEXPT(IK)
	    MINDAT = NDATA(IK)
	    DAMIN = FDATA(IK)
	  ENDIF
	ENDDO
C
	IF(MAXEXP.EQ.MINEXP) THEN
	  NP = NCOUNT(IFFTCH,MINEXP+1) + 1
	  OFFSTR(IFFTCH,MINEXP+1,NP) = .5*(MAXDAT+MINDAT)
	  NCOUNT(IFFTCH,MINEXP+1)  = NP
	ENDIF
C
C	WRITE(16,*) 'min,max data,expt',mindat,minexp,maxdat,maxexp
C
C
	RETURN
	END
	SUBROUTINE FWINDW(IWINDW)
C
	COMMON /WINBLK/ WINDOW(1024)
	DATA TWOPI /6.2831853/
C
	GO TO (100,200,300) IWINDW+1
	PRINT*,'REQUESTED WINDOW DOES NOT EXIST'
 100	CONTINUE
	PRINT*,'IWINDW =',IWINDW,'  NO WINDOWING'
	DO N = 1,1024
	  WINDOW(N) = 1.
	ENDDO
	RETURN
 200	CONTINUE
	PRINT*,'IWINDW =',IWINDW,'  HAMMING'
	ALPHA = .54
	GO TO 310
 300	CONTINUE
	ALPHA = .5
	PRINT*,'IWINDW =',IWINDW,'  HANNING'
 310	CONTINUE
	DO N = 1,1024
	  WINDOW(N) = ALPHA + (ALPHA-1.)*COS(TWOPI*(N-1)/1024.)
	ENDDO
	RETURN
	END
	SUBROUTINE SHPLOT
C
	character*32 scet
	COMMON /SHBLK/ PTIME(64),PWRL(64,511)
	COMMON /PLOTDT/ ITERM,FFTAVR,NDRV,AMP,IWINDW,STRG
	common /headblk/ major,minor,ifftch,scet
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
	CHARACTER*6 TITLE(4)
	CHARACTER*120 STR
C
C
	IF(NPT.LT.1) NPT = 1             	  ! protection
	PRINT*,' IN SHPLOT,NF1,NF2',NF1,NF2
	PRINT*,' IN SHPLOT,FREQS',PTIME(1),PTIME(NPT),NPT
C	CALCULATE BOX SIZE
	PIXSIZ = .1
	IF(NPT.GT.1) PIXSIZ = (PTIME(NF2) - PTIME(NF1))/(NPT-1)
	HSTART = PTIME(1) - .5*PIXSIZ
	HEND =   PTIME(NPT) + .5*PIXSIZ
	PRINT*,' IN SHPLOT',HSTART,HEND,NPT
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
	IF(NF1.EQ.1) THEN
	  YMIN = PWRL(1,1)
	  YMAX = YMIN
	  DO N = 1,NPT
	    DO J = 1,511
	        YMIN = AMIN1(YMIN,PWRL(N,J))
	        YMAX = AMAX1(YMAX,PWRL(N,J))
	    ENDDO
	  ENDDO
	ENDIF
C
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,280.,XEND,1587.)
	ENDIF
	PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C	RAISING YMIN MAKES THE BACKGROUND LIGHTER
C	LOWERING YMAX MAKES THE SIGNAL DARKER
	YMIN = -6.
	PRINT*,'YMIN,MAX SET TO',YMIN,YMAX
	CALL MGOHALFTONE(PWRL,NPT,511,YMIN,YMAX,1.E7)
	CALL MGOSETEXPAND(.7)
	CALL MGOSETLIM(FSTART,1.,FEND,511.)
	IF(NF2.EQ.NSIZE) THEN
          CALL MGOXLABEL(15,'INPUT HARMONIC')	
	  CALL MGOYLABEL(15,'OUTPUT HARMONIC')	
	  CALL MGOBOX(1,2)
	  CALL MGOSETEXPAND(1.)
	  CALL MGOPLOTID('FFTWIN','SHPLOT')
	  CALL MGOSETEXPAND(1.)
C
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	ENDIF
C
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
	END
	SUBROUTINE TABLE
C
	character*32 scet
	COMMON /SPBLK/ PWRW(512),PWRNW(512),FDATA(1024),DATA(1024)
	common /fftblk/ nexpt(1024),ndata(1024)
	common /headblk/ major,minor,ifftch,scet
	DIMENSION YY(512),FREQ(512)
	DIMENSION AR(1024),AI(1024)
	DATA NCALL /0/
C
	IF(NCALL.EQ.0) THEN
	  write(19,*) ' data collection at ',scet
	  write(19,*) ' '
	  write(19,*) ' CH    MIN      MAX     AVR    PK SIG  NOWNDW',
     1  ' PK     FREQ     AMP PK     FFT A1'
	  write(19,*) ' '
	  NCALL = NCALL+1
	ENDIF
C
	DAMAX = -1.E7
	DAMIN =  1.E7
	DO IK = 1,1024
C	  NTEMP = NDATA(IK).AND.4095
C	  NEXPT(IK) = (NDATA(IK)/2048).AND.3
C	  NEX = NEXPT(IK).AND.3
C	  TEMP = -(NTEMP-OFFS(NEX+1,IFFTCH))
C	  TEMP = -(NTEMP-2048)
C	  DATA(IK) = TEMP*(-16.)**NEX
C	  FDATA(IK) = ABS(TEMP)+2048.*NEX
C	  IF(DATA(IK).LT.0.) FDATA(IK) = -FDATA(IK)
	  IF(FDATA(IK).GT.DAMAX) THEN
	    MAXEXP = NEXPT(IK)
	    MAXDAT = NDATA(IK)
	    DAMAX = FDATA(IK)
	  ENDIF
	  IF(FDATA(IK).LT.DAMIN) THEN
	    MINEXP = NEXPT(IK)
	    MINDAT = NDATA(IK)
	    DAMIN = FDATA(IK)
	  ENDIF
	ENDDO
	AVR = .5*(MAXDAT + MINDAT)
	DIFF = .5*(MAXDAT - MINDAT)
C
	FFUND = 20.3477
	IF(IFFTCH.GT.2) FFUND = .25*FFUND
	IF(IFFTCH.GT.6) FFUND = .333557
	DO JF = 1,512
	  FREQ(JF) = JF*FFUND
	ENDDO
C
	  DO N = 0,1023
	    AR(N+1) = DATA(N+1)
	    AI(N+1) = 0.
	  ENDDO
	  CALL FASFOR(10,AR,AI,PWRNW)
	  FFTAVR = AR(1)/1024.
C
C	FIND PEAK DATA AND PEAK POWER
C
	  YMIN = PWRNW(1)
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
	  DO J = 1,511
	    JP = J
	    YY(JP) = PWRNW(J)
	    PWRTOT = PWRTOT + PWRNW(J)
	    IF(YY(JP).GT.YMAX) THEN
	      YMAX = AMAX1(YY(JP),YMAX)
	      JMAX = JP
	    ENDIF
	    YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
C	PRINT*,'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
C	WRITE(19,*)'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  PKPWR = PWRNW(JMAX)
	  IF((JMAX-1).GT.0) PKPWR = PKPWR + PWRNW(JMAX-1)
	  IF((JMAX+1).LT.512) PKPWR = PKPWR + PWRNW(JMAX+1)
	  AMPP = SQRT(2.*PKPWR)
C	  WRITE(19,803) PKPWR,FREQ(JMAX),AMPP
C	  PRINT 803,PKPWR,FREQ(JMAX),AMPP
	  WRITE(19,*) ' '
	  WRITE(19,801) IFFTCH,MINEXP,MINDAT,MAXEXP,MAXDAT,AVR,DIFF,
     1   PKPWR,FREQ(JMAX),AMPP,FFTAVR
 801	FORMAT(I4,I4,I5,I4,I5,F8.1,F7.1,E12.4,F10.2,E12.4,F10.1)
 803	FORMAT(' 3PK PWR',E12.4,' AT',F8.2,' HZ, AMP PK=',E12.4)
	RETURN
	END
	SUBROUTINE SPPLOT
C
C	PLOTS THE SPECTRUM
C
	character*32 scet
	COMMON /SPBLK/ PWRW(512),PWRNW(512),FDATA(1024),DATA(1024)
	COMMON /PLOTDT/ ITERM,FFTAVR,NDRV,AMP,IWINDW,STRG
	common /headblk/ major,minor,ifftch,scet
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
	DIMENSION XX(1024),YY(512),FREQ(512)
	CHARACTER*8 TITLE(4)
	CHARACTER*120 STRG(5)
	CHARACTER*120 STR
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/
C
	PRINT*,' IN SPPLOT',NPT
	FFUND = 20.3477
	IF(IFFTCH.GT.2) FFUND = .25*FFUND
	IF(IFFTCH.GT.6) FFUND = .333557
	DO JF = 1,512
	  FREQ(JF) = JF*FFUND
	ENDDO
C
C	ITERM = -5
	ITERM = 3
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C	PRINT*,'START',GX1,GX2,GY1,GY2
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,330.,2200.,3000.)
	ENDIF
C
	  CALL MGOSETEXPAND(.6)
	  TOP = LY2
	  CALL MGOGRELOCATE(GX1,TOP)                    
	  CALL MGOPUTLABEL(6,'SCET   ',3)
	  CALL MGOPUTLABEL(79,SCET,3)
	  CALL MGOPUTLABEL(20,'   major,minor frame ',3)
	  WRITE(STR,801) MAJOR,MINOR
 801	  FORMAT(I6,'.',I3.3)
	  CALL MGOPUTLABEL(14,STR,3)
	  CALL MGOGRELOCATE(GX1,TOP-50.)        
	  CALL MGOPUTLABEL(12,' FFT CHANNEL',3)
	  WRITE(STR,802) IFFTCH
 802	  FORMAT(I6)
	  CALL MGOPUTLABEL(8,STR,3)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                    
	  ELSE
	    CALL MGOGRELOCATE(GX1,GY2-20.)                      ! hardcopy
	  ENDIF
	  CALL MGOSETEXPAND(1.)
C
C	DO JW = 1,4
C	  CALL MGOWINDOW(1,4,JW)                          ! 
C	  PRINT*,'WINDOW',GX1,GX2,GY1,GY2
C	  IF(JW.EQ.4) GYTOP = GY2
C	ENDDO
C
	  XMIN = ALOG10(FREQ(1))
	  XMAX = ALOG10(FREQ(511))
	  RANGE = ABS(XMAX-XMIN)
	  XMAX = XMAX + .02*RANGE
	  XMIN = XMIN - .02*RANGE	
C
	  CALL MGOWINDOW(1,2,1)                          ! NO WINDOW
	  YMIN = ALOG10(PWRNW(1))
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
	  DO J = 1,511
	    JP = J
	    YY(JP) = ALOG10(PWRNW(J))
	    XX(JP) = ALOG10(FREQ(J))
	    PWRTOT = PWRTOT + PWRNW(J)
	    IF(YY(JP).GT.YMAX) THEN
	      YMAX = AMAX1(YY(JP),YMAX)
	      JMAX = JP
	    ENDIF
	    YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
	PRINT*,'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	WRITE(16,*)'UNWINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  PKPWR = PWRNW(JMAX)
	  IF((JMAX-1).GT.0) PKPWR = PKPWR + PWRNW(JMAX-1)
	  IF((JMAX+1).LT.512) PKPWR = PKPWR + PWRNW(JMAX+1)
	  AMP = SQRT(PKPWR)
	  WRITE(16,803) PKPWR,FREQ(JMAX),AMP
C	  WRITE(19,803) PKPWR,FREQ(JMAX),AMP
	  PRINT 803,PKPWR,FREQ(JMAX),AMP
 803	FORMAT(' 3PK PWR',E12.4,' AT',F8.2,' HZ, AMP=',E12.4)
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOTICKSIZE(-1.,0.,-1.,0.)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(XX,YY,JP)
	  CALL MGOBOX(1,2)
	  CALL MGOXLABEL(9,'FREQ (HZ)')
	  CALL MGOYLABEL(12,'V**2/CHANNEL')
	  CALL MGOSETEXPAND(.6)
	  CALL MGOPLOTID('WINDOW','RECT.')
	  CALL MGOSETEXPAND(1.)
C
	  CALL MGOWINDOW(1,2,2)                          ! WITH WINDOW
	  YMIN = ALOG10(PWRW(1))
	  YMAX = YMIN
	  JP = 1
	  JMAX = 1
	  PWRTOT = 0.
	  DO J = 1,511
	    JP = J
	    YY(JP) = ALOG10(PWRW(J))
	    XX(JP) = ALOG10(FREQ(J))
	    PWRTOT = PWRTOT + PWRW(J)
	    IF(YY(JP).GT.YMAX) THEN
	      YMAX = AMAX1(YY(JP),YMAX)
	      JMAX = JP
	    ENDIF
	    YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  PRINT*,'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  WRITE(16,*)'WINDOWED FFT, PWR MAX,MIN',YMAX,YMIN,'  TOTAL',PWRTOT
	  PKPWRW = PWRW(JMAX)
	  IF((JMAX-1).GT.0) PKPWRW = PKPWRW + PWRW(JMAX-1)
	  IF((JMAX+1).LT.512) PKPWRW = PKPWRW + PWRW(JMAX+1)
	  AMPW = SQRT(PKPWRW)
	  WRITE(16,803) PKPWRW,FREQ(JMAX),AMPW
	  PRINT 803,PKPWR,FREQ(JMAX),AMPW
C
	  CALL MGOTICKSIZE(-1.,0.,-1.,0.)
C
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(XX,YY,JP)
	  CALL MGOBOX(1,2)
	  CALL MGOYLABEL(12,'V**2/CHANNEL')
C	  WRITE(TITLE(1),705) IWINDW
C 705	  FORMAT(I2)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOPLOTID('WINDOW',TITLE(IWINDW+1))
	  CALL MGOSETEXPAND(1.)
C
C	  CALL MGOWINDOW(1,4,3)                          ! FAKE DATA
C	  CALL MGOSETLOC(GX1,GY1,GX2,GYTOP)
C	  CALL MGOTICKSIZE(0.,0.,128.,2048.)
C	  CALL MGOSETLIM(-10.,-8192.,1035.,8192.)
C	  EXPSV = EXPAND
C	  CALL MGOSETEXPAND(.4)
C	  DO N = 1,1024
C	    XX(N) = N
C	    CALL MGORELOCATE(XX(N),FDATA(N))
C	    CALL MGOPOINT(4,1)
C	  ENDDO
C	  CALL MGOSETEXPAND(EXPSV)
C	  CALL MGOBOX(1,2)
C	  CALL MGOSETLTYPE(2)
C	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(0)
C	  CALL MGOXLABEL(10,'SAMPLE NO.')
C	  CALL MGOYLABEL(16,'DATA & GAIN STEP')
C
	CALL MGOSETEXPAND(.8)
C	CALL MGOPLOTID('WINDFFT','SPPLOT')
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
