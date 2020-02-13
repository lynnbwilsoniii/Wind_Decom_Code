	PROGRAM rad2
C
C	PLOTS rad2.  In linear sweep mode, it makes a gray scale plot
C	In list mode, it plots individual frequencies as a function of time.
c	this is not finished yet
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok,ok2
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	integer*4	ichpa(6)
	character*80	stream
	character*4	event
	character*4	pa(6,4)
	parameter	size=2048
	integer*4	return_size,FLIST_SIZE,rad2_size
	integer*4	rad2_spectrum(512)
	integer*4	temp_waves
	integer*4	count(512),NUMSAMP
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scetrad2,scetsrt
	real*8		timelist(512)
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss
	integer*4	channel(1024),SANGLE,status
	real 		signal(5000),WATT_S(512),volt6_2(512)
	REAL 		one_spectrum(256,1500),PTIME(256,1500)
	REAL 		one_list(16,24000),LISTIME(16,24000)
	REAL		FREQ_LIST(512)
	EQUIVALENCE (ONE_LIST(1,1),ONE_SPECTRUM(1,1))
	EQUIVALENCE (PTIME(1,1),LISTIME(1,1))
!
	common /nrblk/ nrem,NHRMN

C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CH
	COMMON /PLTBLK/ COUNT,PTIME,one_spectrum
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
	DATA TWOPI /6.2831853/
	DATA NPLOTS /0/
	DATA STATUS /0/
C
	ITERM = -1
C	ITERM = 3
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = ' RAD 2'
	PTITLE(3) = 'RCVR'
	PTITLE(4) = 'EVENT NO.'
	PRINT*,' '
C
 1001	FORMAT(I4,1X,20I5)
 1002	FORMAT(A)
C
C
C	Rad2 in linear sweep mode has 256 frequencies from 1.075
C	to 13.825 Mhz, spacing 50 kHz.  On 1995 sep 10, spectra were
C	separated by 16 sec (high bit rate?), so 5400 spectra per day
C	In linear sweep, one sample is made at each frequency, lowest
C	to highest
C
C	In list mode, the number of frequencies, etc. can be larger
C	than 256.  288 is a number that I have found.  Consecutive
C	samples are at different frequencies, unlike RAD1
C
C	Frequency in MHz is given by 1.075 + (channel_number)*.05
C
C	GET STARTED
C
!
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
C
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
c		nrem = 225 for one hour of data
	  write(6,4)
	  read(5,3,err=10,end=20) iq, NREM
	  type*,nrem
	endif
c
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)
c

	ok = w_channel_open(CH,stream)
	if (.not. ok) stop 'Cannot open channel'
	scet = 0.
	call w_channel_position(CH,scet)
	print*,'file starts at scet',scet
	dd = scet
	call w_ur8_to_ydoy(scet,yyyy,idoy,msec)
	scetsrt = float(dd) + hh/24. + mm/1440.
	print*,'set channel position to',scetsrt
	call w_channel_position(CH,scetsrt)
	print*,'channel position set to',scetsrt


        ok = w_event(CH,'rad2')
	if(ok.eq.82) then
		stop 'end of file on CH'
	endif
	if (.not. ok) stop 'cannot get event'
	item = 'EVENT_SCET_R8'
	  ok = w_item_r8(CH, item, scet, 1, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(CH, item, s_scet, 2, return_size)
	print*,'initial CH time',s_scet
c	  scetfill = s_scet(2)
c
	ok = w_channel_filename(CH,file)
	print*,file
        item = 'EVENT_STATE'
	ok = w_item_i4(ch, item, status, 1, return_size)
	TYPE*,'STATUS,0=ERR0R,1=FIX TUNE,2=LINEAR SWEEP,3=LIST',STATUS
c
	get_tm_stream = 1
C
C	GET NEXT EVENT
C
 110    continue
C
	! this is the main program loop


C	type*,'going to get next event'

	event = 'rad2'

	ok = w_event(ch,event)
	   if(ok.eq.82) then
		IF(STATUS.EQ.3) CALL SHPLOT
		stop 'end of file on CH'
	   endif
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	   end if
C
	     item = 'EVENT_STATE'
	     ok = w_item_i4(ch, item, status, 1, return_size)
	     item = 'EVENT_SCET'
	     ok = w_item_i4(ch, item, s_scet, 2, return_size)
	     ss = mod(s_scet(2),100)
	     type*,'s_scet',s_scet
		mm = s_scet(2)/100
		mm = mod(mm,100)
		hh = s_scet(2)/10000
		scett = float(dd) + hh/24. + mm/1440. + ss/86400.
	   HRDAY = HH + MM/60. + SS/3600.


	   ihrmn = 100*hh+mm
	   WRITE(PTITLE(17),1017) s_scet(1)
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1017	   FORMAT(I9)
C 1017	   FORMAT(I10.6)
c 1017	   FORMAT('doy ',I3,I5.4)
 1018	   FORMAT(I6.4,I3.2)
c	TYPE*,'GOING TO GET FREQ COV'
c	   item = 'FREQUENCY_COVERAGE_HZ_R4'
c	   ok = w_item_r4(ch,item,FREQ_LIST,512,FLIST_SIZE)
c	   TYPE*,'FREQ_COVERAGE, RETURN SIZE',FLIST_SIZE
c	TYPE*,(FREQ_LIST(J),J=1,FLIST_SIZE)
	   item = 'S_SCET_R8'
	   ok = w_item_r8(ch,item,timeLIST,512,FLIST_SIZE)
C	   TYPE*,'timelist, RETURN SIZE',FLIST_SIZE
C	TYPE*,(timeLIST(J),J=1,FLIST_SIZE)
c	   item = 'CHANNEL_COVERAGE'
c	   ok = w_item_I4(ch,item,rad2_SPECTRUM,1024,FLIST_SIZE)
c	   TYPE*,'CHANNEL_COVERAGE, RETURN SIZE',FLIST_SIZE
c	TYPE*,(rad2_SPECTRUM(J),J=1,FLIST_SIZE)
	   item = 'CHANNEL_NUMBERS'
	   ok = w_item_I4(ch,item,channel,1024,FLIST_SIZE)
c	   TYPE*,'CHANNEL_NUMBERS, RETURN SIZE',FLIST_SIZE
c	TYPE*,(channel(J),J=1,FLIST_SIZE)
	   item = 'SUN_ANGLE'
	   ok = w_item_I4(ch,item,SANGLE,1,FLIST_SIZE)
	TYPE*,'SUN ANGLE',SANGLE
C	   item = 'POINTER_LIST'
C	   ok = w_item_I4(ch,item,rad2_SPECTRUM,1024,FLIST_SIZE)
C	   TYPE*,'POINTER_LIST, RETURN SIZE',FLIST_SIZE
C	TYPE*,(rad2_SPECTRUM(J),J=1,FLIST_SIZE)
	   item = 'FREQUENCIES_HZ_R4'
	   ok = w_item_r4(ch,item,FREQ_LIST,512,FLIST_SIZE)
c	   TYPE*,'FREQ_LIST, RETURN SIZE',FLIST_SIZE
c	TYPE*,(FREQ_LIST(J),J=1,FLIST_SIZE)
c	   item = 'S_WATTS_R4'
C	   ok = w_item_r4(ch,item,watt_S,512,NUMSAMP)
c	   TYPE*,'WATT_S, RETURN SIZE',NUMSAMP
c	TYPE*,(WATT_S(J),J=1,FLIST_SIZE)
	   NPT = RETURN_SIZE
	   item = 'S_MICROVOLTS_R4'
	   ok2 = w_item_r4(ch,item,volt6_2,512,NUMSAMP)
	   TYPE*,'MUVOLT_2, RETURN SIZE',NUMSAMP
C	   DHR = 2./3600.
c	HRDAY = HRDAY + DHR
C
C
c	write(65,1019) HRDAY,FLIST_SIZE,FREQ_LIST(1),
c     1      (SIGNAL(jj),jj=1,3)
	   if(ok) go to 200
 1019	   FORMAT(F12.4,2I6,4e12.3)
	   if( .not. ok) then
		nerror = nerror + 1
		if(nerror.lt.11) go to 110
	   else
	   	nerror = 0
	   endif
 200	   continue
c	   item = 'BAND_THIS_SPEC'
c	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
c	   type*,'BAND_THIS_SPEC',itemp
C 	   item = 'EVENT_BAND'
C	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
c	   type*,'EVENT_BAND',itemp
C
C	SEPARATE SPECTRA
C
	  DO N = 1,256
	    NFR = CHANNEL(N)+1
	    NSPCTR = COUNT(NFR) + 1
	    IF(NSPCTR.GE.NREM) THEN
c		the 2nd arg. is freq no., 0 = 1.075 MHz, 255 = 
		CALL TPLOT(ITERM,0)
c		CALL TPLOT(ITERM,8)
		CALL TPLOT(ITERM,248)
		CALL TPLOT(ITERM,176)
		STOP
	    ENDIF
C	    ONE_SPECTRUM(NFR,NSPCTR) = WATT_S(N)
	    ONE_SPECTRUM(NFR,NSPCTR) = VOLT6_2(N)
	    PTIME(NFR,NSPCTR) = TIMELIST(N) - DD
	    COUNT(NFR) = NSPCTR
	  ENDDO
	  GO TO 110
C	if(nspctr.gt.0) stop
C	    if(1) stop
C
 1004	   FORMAT(F7.2,' MHz')
 1008	   FORMAT(F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

C
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	

c	CALL TPLOT(ITERM,0)
c	CALL TPLOT(ITERM,8)
	CALL TPLOT(ITERM,248)
		CALL TPLOT(ITERM,176)
C	IF(NPLOTS.LT.1) CALL TPLOT(-2,1)
	NPLOTS=NPLOTS+1
	IF(NPLOTS.LT.NREM) GO TO 110
	PRINT*,'REACHED SECOND SHPLOT'
	IF(STATUS.EQ.3) CALL SHPLOT
	CALL SHPLOT
	STOP
	END
	SUBROUTINE TPLOT(ITERM,NCHANS)
C
C	PLOT 4 WINDOWS, FREQ CHANNELS STARTING WITH NCHANS
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*32 S,SCET
	CHARACTER*4 EVENT
	INTEGER*4 NDATA(1024),S_SCET(2),IDOY
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
	CHARACTER*12 PTITLE(20)
	COMMON /PLTBLK/ COUNT,PTIME,one_spectrum
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
	REAL 	one_spectrum(256,1500),PTIME(256,1500)
	INTEGER*4 COUNT(512),STORE(260)
	DIMENSION YY(2048),PP(2048)
C
	NUMFREQS = 0
	  IF(ITERM.GT.0) THEN			!TERMINAL
		YLOCTOP = 750.
		YLOCBOT = 80.
		XST = 100.
		XEND = 1000.
	  ELSE
		YLOCTOP = 3200.
		YLOCBOT =  300.
		XST = 300.
		XEND = 2250.
	  ENDIF
	TSTART = 1.E6
	TEND  = -1.E6	
	NST = 0
	DO N = 1,256
	  IF(COUNT(N).NE.0) THEN
		NST=NST+1
		STORE(NST) = N
		FMHz = 1.075 + (N-1)*.05
c		PRINT *, N,FMHz,COUNT(N)
		NPT = COUNT(N)
		TSTART = AMIN1(PTIME(N,1),TSTART)
		TEND   = AMAX1(PTIME(N,NPT),TEND)
		NUMFREQS = NUMFREQS+1
	  ENDIF
	ENDDO
	TYPE*,'NUMBER OF FREQUENCIES',NUMFREQS
c	print*,'store'
c	print*,store
	TSTART = 24.*TSTART
	TEND  =  24.*TEND
 1022	FORMAT(10I8)
C
	PRINT*,'IN TPLOT,TSTART,TEND',TSTART,TEND
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
	NPLOTS = 8
	IF(ITERM.GT.0) NPLOTS=2
C
C	PLOT DATA
C
	YINC = (YLOCTOP-YLOCBOT)/NPLOTS
	DO NPLOT=1,NPLOTS
c	  print*,'nplot',nplot
	  YPBOT = (NPLOT-1)*YINC + YLOCBOT
	  YPTOP = YPBOT + YINC
	  CALL MGOSETLOC(XST,YPBOT,XEND,YPTOP)
C
C
 100	  NCHANN = STORE(NPLOT+NCHANS)
	  IF(NCHANN.EQ.0) GO TO 300
	  NPT = COUNT(NCHANN)
	  IF(NPT.EQ.0) GO TO 300
 200	  CONTINUE
	  TYPE*,'PLOT CHANNEL NO.',NCHANN,'  NPT=',NPT
	  YMIN = 1.E18
	  YMAX = -1.E18
	  DO N = 1,NPT
	    PP(N) = 24.*PTIME(NCHANN,N)
	    YY(N) = ONE_SPECTRUM(NCHANN,N)
	    YMAX = AMAX1(YMAX,YY(N))
	    YMIN = AMIN1(YMIN,YY(N))
C	    PRINT*,N,YY(N)
	  ENDDO
	  PRINT*,'YMIN,YMAX',YMIN,YMAX
	  YRANGE = YMAX-YMIN
	  YMAX = YMAX + .05*YRANGE
	  YMIN = YMIN - .05*YRANGE
C
C	  CALL MGOTICKSIZE(0.,0.,5.6,28.)  
C	  CALL MGOSETLIM(-2.,0.,400.,256.)
	  CALL MGOSETLIM(TSTART,YMIN,TEND,YMAX)
c	  CALL MGOGRID(0)
C	  CALL MGOSETLTYPE(1)
c	  CALL MGOGRID(1)
C	  CALL MGOSETLTYPE(0)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOBOX(1,2)
	  CALL MGOCONNECT(PP,YY,NPT)
	  CALL MGOSETEXPAND(.6)
	  FMHz = 1.075 + (NCHANN-1)*.05
	  WRITE(STR,1020) FMHz
	  CALL MGOYLABEL(10,STR)
	  IF(NPLOT.EQ.1) THEN
	     write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
	     scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
             WRITE(STR,1001) SCET(1:10),IDOY
 1001	     FORMAT('HOURS OF ',A10,' DOY ',I3)
	     CALL MGOSETEXPAND(.8)
	     CALL MGOXLABEL(27,STR)
	  ENDIF
 1020	  FORMAT(F6.3,' MHz')
	  CALL MGOSETEXPAND(.8)
	  IF(NPLOT.EQ.NPLOTS) CALL MGOPLOTID('[.WIND]RAD2','TPLOT')
	  CALL MGOSETEXPAND(1.)
C
C
	ENDDO					! END NPLOT LOOP
 300	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
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

10	  write(6,6)
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
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end
	SUBROUTINE SHPLOT
C
	character*32 s,SCET
	integer*4	s_scet(2),IDOY
	integer*2 npwrl
	integer*4 rad2_spectrum(1024)
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR
	COMMON /HEADBL/ PTITLE,EVENT,S_SCET,IDOY
	CHARACTER*12 PTITLE(20)
	COMMON /PLTBLK/ COUNT,PTIME,one_spectrum
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
	CHARACTER*10    TITLE(4)
	CHARACTER*120   STR
	REAL 		one_spectrum(256,1500),PTIME(256,1500)
	INTEGER*4	COUNT(512)
	DIMENSION       PWRL(256000)
C
C
	ITERM = -2		! LANDSCAPE
	XSTART = 400.
	XEND = 3000.
	IW = 1
	NSPCTR = COUNT(1)
	PRINT*,' IN SHPLOT,NSPCTR',nspctr
	PRINT*,' IN SHPLOT,TIMES',PTIME(1,1),PTIME(256,NSPCTR),NSPCTR
C	PRINT*,'PTIME',PTIME(1),PTIME(2),PTIME(3),PTIME(5)
C	CALCULATE BOX SIZE
	PIXSIZ = .1
	IF(NSPCTR.GT.1) 
     1		PIXSIZ = (PTIME(256,NSPCTR) - PTIME(1,1))/(NSPCTR-1)
	HSTART = PTIME(1,1) - .5*PIXSIZ
	HEND =   PTIME(256,NSPCTR) + .5*PIXSIZ
	PRINT*,' IN SHPLOT',HSTART,HEND,NSPCTR
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,280.,XEND,2300.)
	ENDIF
C	
	  YMIN = 1.E6
	  YMAX = -YMIN
	  SMIN = 1.E-18
	  SMAX = 5.E-16
	  CALL HISTOG(2,TJUNK,256,SMIN,SMAX,.01,TOTAL,RET)     ! CLEAR AND INIT
	  DO N = 1,NSPCTR
	  DO M = 1,256
	    NM = N + (M-1)*NSPCTR
	    PWRL(NM) = ONE_SPECTRUM(M,N)
	    CALL HISTOG(1,PWRL(NM),256,SMIN,SMAX,.5,TOTAL,RET)   !LOAD ARRAY
C
	    YMIN = AMIN1(YMIN,PWRL(NM))
	    YMAX = AMAX1(YMAX,PWRL(NM))
	  ENDDO
	  ENDDO
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C
	  CALL HISTOG(0,TJUNK,256,SMIN,SMAX,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE
	  CALL HISTOG(0,TJUNK,256,SMIN,SMAX,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER
C	  LOWERING YMAX MAKES THE SIGNAL DARKER

	  YAMIN = YHMIN
	  YAMAX = YHMAX
	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX
c	  arguments are: array(m,n),m,n,white,black,linearity
c	  m is the x direction
	  CALL MGOTICKSIZE(0.,0.,0.,0.)  
	  CALL MGOHALFTONE(PWRL,NSPCTR,256,YAMIN,YAMAX,1.E7)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOSETLIM(HSTART,1.05,HEND,13.1)
	    CALL MGOYLABEL(14,'FREQUENCY, MHz')	
	  CALL MGOBOX(1,2)
	  CALL MGOGRELOCATE(GX1,GY2)
	  CALL MGOPUTLABEL(10,TITLE(IW),9)
	print*,'title in shplot  ',title(iw)
C
	  WRITE(STR,704) YAMIN,YAMAX
 704	  FORMAT('WHITE,BLACK ',2E10.2,' W/M2/Hz ')
	  CALL MGOSETANGLE(90.)
	  CALL MGOSETEXPAND(.5)
	  XPR = GX2 + .005*(GX2-GX1)
	  YPR = .5*(GY1+GY2)
	  CALL MGOGRELOCATE(XPR,YPR)
	  CALL MGOPUTLABEL(38,STR,2)                  
	  CALL MGOSETANGLE(0.)
	  CALL MGOSETEXPAND(1.)
	  IF(IW.EQ.1) THEN
	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
	   scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
            WRITE(STR,1001) SCET(1:10),IDOY
 1001	    FORMAT('HOURS OF ',A10,' DOY ',I3)
c            WRITE(STR,1001) s_SCET(1)
c 1001	    FORMAT('HOURS OF ',i10)
	    CALL MGOSETEXPAND(.8)
            CALL MGOXLABEL(28,STR)	
	    CALL MGOSETEXPAND(.5)
C	    IF(ITMCORR.EQ.0) THEN
C		CALL MGOPUTLABEL(23,'    consecutive spectra',6)
C	    ELSE
C		CALL MGOPUTLABEL(18,'    corrected time',6)
C	    ENDIF
	    CALL MGOSETEXPAND(1.)
	  ENDIF
C
	    CALL MGOSETEXPAND(.8)
	    CALL MGOPLOTID('WIND-WAVES-rad2','SHPLOT')
	    CALL MGOSETEXPAND(1.)
C
	  CALL MGOSETEXPAND(1.)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
C
	RETURN
	END
	SUBROUTINE HISTOG(ILOAD,X,NBIN,XMIN,XMAX,PCTILE,TOTAL,RET)
C
C	THIS IS A SPECIAL VERSION TO CALCULATE LEVEL AT A CERTAIN PERCENTILE
C
C	INTEGER*4 NARR
	DIMENSION NARR(256)
	DATA NARR /256*0/
C
	IF(ILOAD.GT.1) THEN
	  DO N = 1,256
	    NARR(N) = 0
	  ENDDO
	  TOTAL = 0.
	  RETURN
	ENDIF
C
	IF(ILOAD.EQ.1) THEN
	  XT = AMAX1(X,XMIN)
	  XT = AMIN1(XT,XMAX)
	  IBIN = (NBIN-1)*(XT-XMIN)/(XMAX-XMIN) + 1
	  NARR(IBIN) = NARR(IBIN)+1
	  TOTAL = TOTAL + 1.
	  RETURN
	ENDIF
C
	IF(PCTILE.GT..5) THEN
C	  PRINT OUT RESULTS
    	  PRINT*,' TOTAL NUMBER, NBIN',TOTAL,NBIN
	  PRINT*,'XMIN,XMAX',XMIN,XMAX
	  PRINT*,'HISTOGRAM'
	  PRINT 1111, (NARR(J),J=1,NBIN)
 1111	  FORMAT(10I7)
	ENDIF
C
C	NOW CALCULATE VALUE OF X CORRESPONDING TO PCTILE PERCENTILE
C
C	PCTILE = .2
	SET = PCTILE*TOTAL
	SUM = 0.
	N = 0
  100	N = N+1
	  SUM = SUM + NARR(N)
	  IF(SUM.LT.SET) GO TO 100
C
C	NOW THE DESIRED VALUE LIES IN BIN N
C
	SUMLOW = SUM - NARR(N)
	XN = N
	RETLOW = XMIN + ((XN-1.)/(NBIN-1.))*(XMAX-XMIN)
	RETHI = XMIN +       (XN/(NBIN-1.))*(XMAX-XMIN)
	RET = RETLOW + (SET-SUMLOW)*(RETHI-RETLOW)/NARR(N)
C	PRINT*,'N,SET,SUMLOW,LOW,RET',N,SET,SUMLOW,RET
	RETURN
	END
