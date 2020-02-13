	PROGRAM TNR
C
C	PLOTS TNR
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
	integer*4	return_size,agc_size,tnr_size
	integer*4	tnr_spectrum(1024),tnr_agc(50)
	integer*4	temp_waves
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scettnr,scetsrt
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss
	integer*4	minf,maxf,mode
	real		tpwr
	real 		signal(5000),volt6_1(500),volt6_2(500)
!
	common /nrblk/ nrem,NHRMN
	common /headblk/ major,minor,tds_channel,s_scet
C
C	The TNR has 5 bands of 32 freqs each, but there is overlap so
C	there are 96 different frequencies
C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CH
	COMMON /PLTBLK/ NPT,NSPCTR,TNR_SPECTRUM
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,MODE
	COMMON /SPECBLK/ VOLT6_1,VOLT6_2
	COMMON /HEADBL/ PTITLE,EVENT
	COMMON /SHBLK/ PTIME(15000)
	COMMON /PWRBLK/ TPWR(15000)
	DATA TWOPI /6.2831853/
	DATA NPLOTS /0/
	DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
C
	PTITLE(1) = 'WIND-WAVES'
	PTITLE(2) = ' THERMAL'
	PTITLE(3) = 'NOISE RCVR'
	PTITLE(4) = 'EVENT NO.'
	PTITLE(8) = 'SAMPLE RATE'
	PTITLE(10) = 'L.P.FILTER'
	PTITLE(12) = 'TRIGGERS'
	PTITLE(15) = 'SCET'
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
C
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
	  write(6,4)
	  read(5,3,err=11,end=20) iq, NREM
	  type*,nrem
	endif
 11	continue
c	  type*,'error in readin no. of events, got',nrem
c	go to 10
c
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)
c

	ok = w_channel_open(CH,stream)
	if (.not. ok) stop 'Cannot open channel'
	scet = 0.
	call w_channel_position(CH,scet)
	print*,'tds file starts at scettds',scet
	dd = scet
	call w_ur8_to_ydoy(scet,yyyy,idoy,msec)
	scetsrt = float(dd) + hh/24. + mm/1440.
	print*,'set channel position to',scetsrt
	call w_channel_position(CH,scetsrt)
	print*,'tds channel position set to',scetsrt


        ok = w_event(CH,'TNR')
	if(ok.eq.82) then
		stop 'end of file on CH'
	endif
	if (.not. ok) stop 'cannot get event'
	item = 'EVENT_MODE'
	  ok = w_item_i4(ch,item,mode,1,return_size)
	  type*,'initial mode',mode
c		mode = 0, TNR_A only, F=16
c		mode = 1, TNR_B only, F=16
c		mode = 2, TNR_A only, F=32
c		mode = 3, TNR_B only, F=32
c		mode = 4, TNR_A,TNR_B, F=16
c
	item = 'EVENT_SCET_R8'
	  ok = w_item_r8(CH, item, scet, 1, return_size)
	item = 'EVENT_SCET'
	  ok = w_item_i4(CH, item, s_scet, 2, return_size)
	print*,'initial CH time',s_scet
c	  scetfill = s_scet(2)
c
	ok = w_channel_filename(CH,file)
c	print*,file
c
	get_tm_stream = 1
C
	GO TO 110
C
C	GET NEXT EVENT
C
 100    continue
C
	! this is the main program loop


c	type*,'going to get next event'

	event = 'TNR'

	ok = w_event(ch,event)
	   if(ok.eq.82) then
		print*,'call pwrplot after eof'
		CALL PWRPLOT(MINF,MAXF)
		stop 'end of file on CH'
	   endif
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	   end if
	   if(ok) go to 200
 1019	   FORMAT(F12.4,2I6,4e12.3)
	   if( .not. ok) then
		nerror = nerror + 1
		if(nerror.lt.11) go to 100
	   else
	   	nerror = 0
	   endif
 200	   continue
C
	     item = 'EVENT_SCET'
	     ok = w_item_i4(ch, item, s_scet, 2, return_size)
 110    continue
	     ss = mod(s_scet(2),100)
		mm = s_scet(2)/100
		mm = mod(mm,100)
		hh = s_scet(2)/10000
		scett = float(dd) + hh/24. + mm/1440. + ss/86400.
	   HRDAY = HH + MM/60. + SS/3600.


	   ihrmn = 100*hh+mm
	   WRITE(PTITLE(17),1017) s_scet(1)
	   WRITE(PTITLE(18),1018) s_scet(2)/100, MOD(s_scet(2),100)
c	   WRITE(PTITLE(17),1017) ihrmm,ss
 1017	   FORMAT('YMD',I9)
C 1017	   FORMAT(I10.6)
c 1017	   FORMAT('doy ',I3,I5.4)
 1018	   FORMAT(I6.4,I3.2)
	   item = 'EVENT_MODE'
	   ok = w_item_i4(ch,item,mode,1,return_size)
	   item = 'AGC_1'
	   ok = w_item_i4(ch,item,tnr_agc,50,agc_size)
C	   TYPE*,'AGC, RETURN SIZE',agc_SIZE
	   item = 'SPECTRA_1_MICROVOLTS_R4'
C	   SPECTRA_1_MICROVOLTS_R4 IS microvolt per root hz
	   ok = w_item_r4(ch,item,volt6_1,500,return_size)
C	   TYPE*,'MUVOLT_1, RETURN SIZE',RETURN_SIZE
	   NPT = RETURN_SIZE
C	   item = 'SPECTRA_2_MICROVOLTS_R4'
C	   ok2 = w_item_r4(ch,item,volt6_2,500,return_size)
C	   TYPE*,'MUVOLT_2, RETURN SIZE',RETURN_SIZE
C	   DHR = 2./3600.
c	HRDAY = HRDAY + DHR
C
	IF(MODE.NE.4) THEN
	  IF(AGC_SIZE.EQ.10) THEN  		! SKIP B,D
C	  IF(AGC_SIZE.EQ.10.AND.NPT.EQ.320) THEN  		! SKIP B,D
C
	    DO NFR = 1,32
	      SIGNAL(NFR) = .5*(VOLT6_1(NFR) + VOLT6_1(NFR+160))
	    ENDDO
	    DO NFR = 33,64
	      SIGNAL(NFR) = .5*(VOLT6_1(NFR+32) + VOLT6_1(NFR+192))
	    ENDDO
	    DO NFR = 65,96
	      SIGNAL(NFR) = .5*(VOLT6_1(NFR+64) + VOLT6_1(NFR+224))
	    ENDDO
C
	  ELSE					! DO A,C,E	
C
	    DO NFR = 1,96
	      SIGNAL(NFR) = .25*(VOLT6_1(NFR) + VOLT6_1(NFR+96)
     1 		+ VOLT6_1(NFR+192) + VOLT6_1(NFR+288))
	    ENDDO
C
	  ENDIF
	ENDIF
	IF(MODE.EQ.4) THEN
C						! SKIP B,D
	    DO NFR = 1,16
	      SIGNAL(NFR) = VOLT6_1(NFR) 
	    ENDDO
	    DO NFR = 17,32
	      SIGNAL(NFR) = VOLT6_1(NFR+16)
	    ENDDO
	    DO NFR = 33,48
	      SIGNAL(NFR) = VOLT6_1(NFR+32)
	    ENDDO
	    DO NFR = 49,64
	      SIGNAL(NFR) = VOLT6_1(NFR+32) 
	    ENDDO
	    DO NFR = 65,80
	      SIGNAL(NFR) = VOLT6_1(NFR+48)
	    ENDDO
	    DO NFR = 81,96
	      SIGNAL(NFR) = VOLT6_1(NFR+64)
	    ENDDO
C
	ENDIF
c	write(65,1019) HRDAY,agc_size,tnr_agc(1),
c     1      (SIGNAL(jj),jj=1,3)
c	   item = 'BAND_THIS_SPEC'
c	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
c	   type*,'BAND_THIS_SPEC',itemp
C 	   item = 'EVENT_BAND'
C	   ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
c	   type*,'EVENT_BAND',itemp
C
C	SEPARATE SPECTRA
C
	  NSPCTR = NSPCTR + 1
	  PTIME(NSPCTR) = HRDAY
C
C	CALCULATE TOTAL POWER IN BAND MINF TO MAXF
C
	MINF = 1
	MAXF = 30
	minf = 36			! 19 khz
	maxf = 54			! 41.5 khz
	minf = 40			! 22.6 khz
	maxf = 50			! 34.9 khz
	TPWR(NSPCTR) = 0.
	DO NFR = MINF,MAXF
	  TPWR(NSPCTR) = TPWR(NSPCTR) + SIGNAL(NFR)**2
	ENDDO
C	    if(1) stop
C
 1004	   FORMAT(F7.2,' kHZ')
 1008	   FORMAT(F7.0,' HZ')
 1009	   FORMAT('TDS CHANNEL',I4)

C
 20	CONTINUE
C
 1003	FORMAT(3(I9,E11.3,I5))
C

C	CALL PLTDSFR(3)
c	IF(NPLOTS.LT.1) CALL PLTDSFR(-2)
C	IF(NPLOTS.LT.5) CALL PLTDSFR(-2)
	NPLOTS=NPLOTS+1
C	TYPE*,'NSPCTR,PTIME,NPLOTS,PWR',NSPCTR,PTIME(NSPCTR),NPLOTS,
C     1      TPWR(NSPCTR)
	IF(NPLOTS.LT.NREM) GO TO 100
	type*,'call pwrplot after go to 100'
	CALL PWRPLOT(MINF,MAXF)
	STOP
	END
	SUBROUTINE PWRPLOT(MINF,MAXF)
C
C	MAKES A FILE OF TOTAL POWER FROM MINF TO MAXF FOR PLOTTING
C		BY PWRPLOT.MGO
C
	COMMON /PLTBLK/ NPT,NSPCTR,NDATA
	COMMON /SHBLK/ PTIME(15000)
	COMMON /PWRBLK/ TPWR(15000)
	REAL TPWR
C
	DO N = 1,NSPCTR
	  WRITE(63,1063) PTIME(N),TPWR(N)
	ENDDO
 1063	FORMAT(F10.5,E12.3)
	RETURN
	END
	SUBROUTINE PLTDSFR(ITERM)
C
C	PLOT 
C
	CHARACTER*12 TITLE(20)
	CHARACTER*120 STR
	CHARACTER*4 EVENT
	INTEGER*4 NDATA(1024)
	COMMON /HEADBL/ TITLE,EVENT
	COMMON /PLTBLK/ NPT,NSPCTR,NDATA
	COMMON /SPECBLK/ VOLT6_1(500),VOLT6_2(500)
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
	DIMENSION YY(2048),PP(2048)
C
	CALL MGOINIT
	CALL MGOSETUP(ITERM)
	CALL MGOERASE
C
	TYPE*,'NO POINTS IN PLTDSFR',NPT
C
C	PLOT DATA
C
	XEND = 2750.
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(300.,1100.,XEND,2230.)
	ENDIF
C
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
	NPS = NPT
	YMAX = -1.E6
	YMIN = 1.E6
	XLIM = NPT + 2
	DO N = 1,NPT
	  PP(N) = N
C	  YY(N) = NDATA(N)
	  YY(N) = VOLT6_1(N)
	  YMAX = AMAX1(YY(N),YMAX)
	  YMIN = AMIN1(YY(N),YMIN)
C	  PRINT*,N,YY(N)
	ENDDO
C
C	CALL MGOTICKSIZE(0.,0.,5.6,28.)  
	CALL MGOSETLIM(-2.,0.,XLIM,YMAX)
c	CALL MGOGRID(0)
C	CALL MGOSETLTYPE(1)
c	CALL MGOGRID(1)
	CALL MGOSETLTYPE(0)
	CALL MGOSETEXPAND(.6)
	CALL MGOCONNECT(PP,YY,NPS)
	CALL MGOSETEXPAND(.8)
	  CALL MGOBOX(1,2)
	  CALL MGOYLABEL(10,'T/M NUMBER')
	  CALL MGOSETEXPAND(1.)
	TRANGE = GY2-GY1
	TINC = .08*TRANGE
	XTITLE = GX2 +.005*(GX2-GX1)
	YTITLE = GY2
	CALL MGOSETEXPAND(.8)
 1020	FORMAT(F8.2,' kHZ')
	DO N = 1,20
	  YTITLE = YTITLE - TINC
	  IF(N.EQ.4) YTITLE = YTITLE - TINC
	  IF(N.EQ.6) YTITLE = YTITLE - TINC
	  CALL MGOGRELOCATE(XTITLE,YTITLE)
	  CALL MGOLABEL(12,TITLE(N))
	ENDDO
	CALL MGOSETEXPAND(1.)
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID(EVENT,'[.WIND]TNR')
	CALL MGOSETEXPAND(1.)
C
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
C    	  PRINT*,' TOTAL NUMBER, NBIN',TOTAL,NBIN
C	  PRINT*,'XMIN,XMAX',XMIN,XMAX
c	  PRINT*,'HISTOGRAM'
c	  PRINT 1111, (NARR(J),J=1,NBIN)
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

