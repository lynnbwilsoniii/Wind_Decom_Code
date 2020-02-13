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
	real		tpwr,FREQLIST(500)
	real 		signal(5000),volt6_1(500),volt6_2(500)
	REAL 		one_spectrum(96,11000)
!
	common /nrblk/ nrem,NHRMN
	common /headblk/ major,minor,tds_channel,s_scet
C
C	The TNR has 5 bands of 32 freqs each, but there is overlap so
C	there are 96 different frequencies
C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 CH
	COMMON /PLTBLK/ NPT,NSPCTR,TNR_SPECTRUM,one_spectrum
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,MODE
	COMMON /SPECBLK/ VOLT6_1,VOLT6_2
	COMMON /HEADBL/ PTITLE,EVENT
	COMMON /SHBLK/ PTIME(15000)
	COMMON /PWRBLK/ TPWR(15000),FREQM(15000)
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
c	  read(5,3,err=10) iq, NHRMN,LAST
	  read(5,3) iq, NHRMN
	  type*,NHRMN
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
	  write(6,4)
c	  read(5,3,err=10,end=20) iq, NREM
	  read(5,*)  NREM
	  type*,' NREM',nrem
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
		CALL SHPLOT
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
	   ITEM = 'ANTENNA_1'
	   OK2 = W_ITEM_I4(CH,ITEM,IANT_1,1,RETURN_SIZE)
           ITEM = 'ANTENNA_2'
           OK2 = W_ITEM_I4(CH,ITEM,IANT_2,1,RETURN_SIZE)
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
	      FREQLIST(NFR) = 4. + (NFR-1)*.375  
	    ENDDO
	    DO NFR = 33,64
	      SIGNAL(NFR) = .5*(VOLT6_1(NFR+32) + VOLT6_1(NFR+192))
	      FREQLIST(NFR) = 16. + (NFR-33)*1.5  
	    ENDDO
	    DO NFR = 65,96
	      SIGNAL(NFR) = .5*(VOLT6_1(NFR+64) + VOLT6_1(NFR+224))
	      FREQLIST(NFR) = 64. + (NFR-65)*6.  
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
	      FREQLIST(NFR) = 4. + (NFR-1)*.375  
	    ENDDO
	    DO NFR = 17,32
	      SIGNAL(NFR) = VOLT6_1(NFR+16)
	      FREQLIST(NFR) = 4. + (NFR-1)*.375  
	    ENDDO
	    DO NFR = 33,48
	      SIGNAL(NFR) = VOLT6_1(NFR+32)
	      FREQLIST(NFR) = 16. + (NFR-17)*1.5  
	    ENDDO
	    DO NFR = 49,64
	      SIGNAL(NFR) = VOLT6_1(NFR+32) 
	      FREQLIST(NFR) = 16. + (NFR-17)*1.5  
	    ENDDO
	    DO NFR = 65,80
	      SIGNAL(NFR) = VOLT6_1(NFR+48)
	      FREQLIST(NFR) = 64. + (NFR-65)*6.  
	    ENDDO
	    DO NFR = 81,96
	      SIGNAL(NFR) = VOLT6_1(NFR+64)
	      FREQLIST(NFR) = 64. + (NFR-65)*6.  
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
	  DO NFR = 1,96
	    ONE_SPECTRUM(NFR,NSPCTR) = SIGNAL(NFR)
	  ENDDO
C
C	there are apparently 32 freqs per band, and freq nos are 1 to 160 
C	CALCULATE TOTAL POWER IN BAND MINF TO MAXF
C		BAND A  4 kHz TO 16 kHz   FREQ = 4 + (N-1)*.375  ?1 to 32
C		BAND B  8 kHz TO 32 kHz   FREQ = 8 + (N-1)*.75   33 to 64
C		BAND C 16 kHz TO 64 kHz   FREQ = 16 + (N-1)*1.5  65 to 96
C		BAND D 32 kHz TO 128 kHz  FREQ = 32 + (N-1)*3.   97 to 128
C		BAND E 64 kHz TO 256 kHz  FREQ = 64 + (N-1)*6.  129 to 160
C
C a special printout for the 8AIAC in Hawaii, 2009
C
	NPWR = 54			! 42 KHZ = 1.5 FP FOR 19980402
	write(61,1061) hrday,IANT_1,signal(npwr)**2,freqlist(npwr)
 1061	format(f10.5,I4,e13.4,f10.5)
C
	MINF = 1
	MAXF = 30
	minf = 36			! 19 khz
	maxf = 54			! 41.5 khz
	minf = 40			! 22.6 khz
C	maxf = 50			! 34.9 khz
	minf = 17			! 10 khz
	maxf = 96
C	maxf = 32			! 16 kHz
C*****  THIS IS A SPECIAL PART TO CALCULATE POWER AT MAXIMUM.  THE
C	IDEA IS THAT I WANT TO COMPARE WITH LANGMUIR WAVES, WHICH 
C	HAVE A NARROW BANDWIDTH, SO I WANT TO TAKE A NARROW PART
C	OF THE RADIO SPECTRUM, BUT TAKE IT WHERE THE SPECTRUM IS
C	AT MAXIMUM
C
	TPWR(NSPCTR) = 0.
COLD	DO NFR = MINF,MAXF
COLD	  TPWR(NSPCTR) = TPWR(NSPCTR) + SIGNAL(NFR)**2
COLD	ENDDO
	MAXSIG = 0
	DO NFR = MINF,MAXF
	  IF(SIGNAL(NFR)**2.GT.TPWR(NSPCTR)) THEN
	    TPWR(NSPCTR) = SIGNAL(NFR)**2
	    MAXSIG = NFR
	    FREQM(NSPCTR) = FREQLIST(NFR) 
	  ENDIF
	ENDDO
c
c	ok = w_item_r4(ch,'FREQUENCIES_HZ_R4',FREQLIST,500,return_size)
c	TYPE*,'MINF,MAXF,MAXSIG,ret',MINF,MAXF,MAXSIG,return_size
c	TYPE*,FREQLIST
c	IF(1) STOP
c	FREQM(NSPCTR) = FREQLIST(MAXSIG)
C
C
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
	CALL SHPLOT
	STOP
	END
	SUBROUTINE PWRPLOT(MINF,MAXF)
C
C	MAKES A FILE OF TOTAL POWER FROM MINF TO MAXF FOR PLOTTING
C		BY PWRPLOT.MGO
C
	COMMON /PLTBLK/ NPT,NSPCTR,NDATA,one_spectrum
	COMMON /SHBLK/ PTIME(15000)
	COMMON /PWRBLK/ TPWR(15000),FREQM(15000)
	REAL 		one_spectrum(96,11000)
	REAL TPWR
C
	DO N = 1,NSPCTR
	  WRITE(63,1063) PTIME(N),TPWR(N),FREQM(N)
	ENDDO
 1063	FORMAT(F10.5,E12.3,F10.5)
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
	COMMON /PLTBLK/ NPT,NSPCTR,NDATA,one_spectrum
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
	REAL 		one_spectrum(96,11000)
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
	SUBROUTINE SHPLOT
C
	character*32 s,SCET
	integer*4	s_scet(2)
	integer*2 npwrl
	integer*4 tnr_spectrum(1024)
	COMMON /SHBLK/ PTIME(15000)
	COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY,MODE
	COMMON /PLTBLK/ NPT,NSPCTR,TNR_SPECTRUM,one_spectrum
	common /headblk/ major,minor,ifftch,s_scet,title
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
	CHARACTER*10 TITLE(4)
	CHARACTER*120 STR
	REAL 		one_spectrum(96,11000)
	DIMENSION PWRL(1111000)
C
C
	ITERM = -2		! LANDSCAPE
C	ITERM = 3		! terminal
	XSTART = 400.
	XEND = 3000.
	IW = 1
	PRINT*,' IN SHPLOT,NSPCTR',nspctr
	PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NSPCTR),NSPCTR
	PRINT*,'PTIME',PTIME(1),PTIME(2),PTIME(3),PTIME(5)
C	CALCULATE BOX SIZE
c********
C	if(1)return
	PIXSIZ = .1
	IF(NPT.GT.1) PIXSIZ = (PTIME(NSPCTR) - PTIME(1))/(NSPCTR-1)
	HSTART = PTIME(1) - .5*PIXSIZ
	HEND =   PTIME(NSPCTR) + .5*PIXSIZ
	PRINT*,' IN SHPLOT',HSTART,HEND,NSPCTR
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
	NFREQS = 96
	IF(MODE.EQ.4) NFREQS = 48
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,280.,XEND,2300.)
	ENDIF
C	
	  YMIN = 1.E6
	  YMAX = -YMIN
	  CALL HISTOG(2,TJUNK,256,0.,255.,.01,TOTAL,RET)     ! CLEAR AND INIT
	  DO N = 1,NSPCTR
	  DO M = 1,96
c	    NM = N + (M-1)*NSPCTR      
	    NM = N + (M-1)*NSPCTR
	    PWRL(NM) = ONE_SPECTRUM(M,N)
	    CALL HISTOG(1,PWRL(NM),256,1.,255.,.5,TOTAL,RET)    ! LOAD ARRAY
C
	    YMIN = AMIN1(YMIN,PWRL(NM))
	    YMAX = AMAX1(YMAX,PWRL(NM))
	  ENDDO
	  ENDDO
	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C
	  CALL HISTOG(0,TJUNK,256,0.,255.,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE
	  CALL HISTOG(0,TJUNK,256,0.,255.,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER
C	  LOWERING YMAX MAKES THE SIGNAL DARKER

	  YAMIN = YHMIN
	  YAMAX = YHMAX
	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX
c	  arguments are: array(m,n),m,n,white,black,linearity
c	  m is the x direction
	  CALL MGOTICKSIZE(0.,0.,-1.,-1.)  
C	  CALL MGOHALFTONE(PWRL,96,NSPCTR,YAMIN,YAMAX,1.E7)
	  CALL MGOHALFTONE(PWRL,NSPCTR,NFREQS,YAMIN,YAMAX,1.E7)
	  CALL MGOSETEXPAND(.7)
	  CALL MGOSETLIM(HSTART,.593,HEND,2.417)
	    CALL MGOYLABEL(14,'FREQUENCY, kHz')	
	  CALL MGOBOX(1,2)
	  CALL MGOGRELOCATE(GX1,GY2)
	  CALL MGOPUTLABEL(10,TITLE(IW),9)
	print*,'title in shplot  ',title(iw)
C
	  WRITE(STR,704) YAMIN,YAMAX
 704	  FORMAT('WHITE,BLACK ',2F6.1,' \gmV\u2/Hz ')
	  CALL MGOSETANGLE(90.)
	  CALL MGOSETEXPAND(.5)
	  XPR = GX2 + .005*(GX2-GX1)
	  YPR = .5*(GY1+GY2)
	  CALL MGOGRELOCATE(XPR,YPR)
	  CALL MGOPUTLABEL(30,STR,2)                  
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
	    CALL MGOPLOTID('WIND-WAVES-TNR','SHPLOT')
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

