	subroutine makefile(ch)
C
C	THIS IS MAKEFILE22, WHICH COLLECTS AND AVERAGES A DESIGNATED SET OF 
C		SIGNALS FROM THE FFT, AS WELL AS SOME PLASMA DATA, ETC.  
c
c	carrington rotation 2203 began 19 nov 1994
c	carrington rotation 2204 began 16 dec 1994
c	carrington rotation 2205 began 12 Jan 1995
c	carrington rotation 2206 began 08 Feb 1995
c	carrington rotation 2207 began 07 Mar 1995
c	carrington rotation 2208 began 04 Apr 1995
c	carrington rotation 2209 began 01 May 1995
!							       X,Y  Z
!	implicit	integer*4 (a-z)
	include		'apmplot_def.for'
	integer*4	ok,ch,get_output_device,loop_and_gather
	character*80	stream
	data npt /0/
c
	IF(CH.EQ.0) THEN
	  CALL TPLOT(NPT,IRX,FFT_CHANNEL)
	  STOP
	ENDIF
C	
	ok = 1
	if (ok) ok = loop_and_gather(ch,npt)
c	TYPE*,'RETURN FROM LOOP_AND_GATHER at',sceti4(2)
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	loop_and_gather(ch,npt)
!	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	include		'apmplot_def.for'
	COMMON /PFILE/ PTIME(4000),SIG(4000,10),PEAK(4000,10),
     1		DENSI(4000),VSW(4000),BMAG(4000),TITLE(10)
	integer*4	ok,okt
	integer*4	ch
	integer*4	ret_size
	integer*4	s_scet(2)
	character*80	stream
	character*32	item
	character*4	year,EVENT
	character*40	com(15)
	character*20	title
	integer*4	error_count,hh,min,nday
	integer*4	ihrmn,yyyy,mon,dd,mm,ss,ms
	integer*4	ndaysdone
	integer*4	angle,NSSV(250)
	integer*4	major, last_major,cal_on,FFT_CHANNEL
	real*4		wt(1000),sigsv(250,10)
	real*4		tempel(4000),tempion(4000)
	real*4		COUNTT(15),SIGT(10)
	real		output(4,4)
	real*8		scet,scetst,timeint,scet_strt,scetstav
C		scet is current time, scetstav is start time of averaging
c		interval,scet_strt is start time of whole calculation
	real		nhist(100,10),vdata(1024),dbspec(513)
	data error_count /0/
	data ndaysdone /0/
	data nstart /0/
	data istart /0/
C	DATA EVENT /'FFTH'/
	DATA EVENT /'FFTM'/
c
	nstart = 0
c
c	write(37,*) 'return from stream name:',stream
!	ok = wind_tm_open_channel(ch,'realtime')
	call wind_tm_get_filename(ch,file)
c	type *,'file: ',file(:72)
c	type *,'file head ',file(:37)
c	type *,'year ',file(31:34)
c	type *,'month ',file(35:36)
c	type *,'day ',file(37:38)

	TIMEINT = 15.d00/1440.d00             ! 15 min. interval
C	TIMEINT = 2.d00/1440.d00              !  2 min. interval
C	TIMEINT = 0.			   !  do not skip data
C
	loop_and_gather = 1
	scet = 0.
	call w_channel_position(ch,scet)
c	type*,'channel, scet',ch,scet
	if(ndaysdone.eq.0) then
		scetst = 4747.
C		year = file(38:41)
		year = file(31:34)
c		print*,'check year,=',year
		if(year.eq.'1994') scetst = 4382.d00
		if(year.eq.'1995') scetst = 4747.d00
		if(year.eq.'1996') scetst = 5112.d00
		if(year.eq.'1997') scetst = 5478.d00
		if(year.eq.'1998') scetst = 5843.d00
		if(year.eq.'1999') scetst = 6208.d00
		if(year.eq.'2000') scetst = 6573.d00
		if(year.eq.'2001') scetst = 6939.d00
		if(year.eq.'2002') scetst = 7304.d00
		if(year.eq.'2003') scetst = 7669.d00
		if(year.eq.'2004') scetst = 8034.d00
	endif
	if(scet.eq.0.d00) scet = 4767.d00
	nday = scet
c	type*,'position channel pointer to',scet
c	ok = w_channel_position(ch,scet)
c	if ( ok.eq.1 ) then
c		type*,'channel pointer positioned to',scet
c	else
c		 type*,'cannot position pointer'
c	endif
	
 3001   ok = w_event(ch,EVENT)
	if ( ok.ne.1 ) then
		type *, 'cannot get event after', scet
		error_count = error_count + 1
		type*,'in getting started, error',error_count
		if(error_count.lt.100) go to 3001
		stop
	else
	 	if(error_count.ne.0) then
c		  reset initial scet
		  scet = 4767.d00
	 	  error_count=0
		endif
	endif
C
C	this is apparently used to eliminate duplication of major frames
C
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	last_major = major-1

	item = 'EVENT_SCET_R8'
	ok = w_item_r8(ch, item, scet, 1, ret_size)
c	scetst = scet
	scetstav = scet
	scet_strt = scet
C
	item = 'channel_number'
	ok = w_item_i4(ch, item, fft_channel, 1, return_size)
c
	FFUND = 20.3477
	IF(FFT_CHANNEL.GT.2) FFUND = .25*FFUND
	IF(FFT_CHANNEL.GT.6) FFUND = .333557
c
C
C	type*,'going to get first item=',item
C	type*,'channel no',ch
C	type*,'return size',ret_size
c
	call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
c
c	     ihrmn = 100*hh+mm
c	     TYPE 1005,YYYY,MON,DD,IHRMN
C		write(26,*) YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)

c	scet = scet + 6.d00/1440.d00			! advance six 
!						minutes to avoid cal


!	ok = wind_tm_set_messages_off(ch)
!	if (.not. ok) stop 'cannot turn messages off'

C	type *, 'Please wait, gathering data ...'
c
	  DENSIT = 0.
	  VSWT = 0.
	  BMAGT = 0.
	  NSV = 0
	  DO N = 1,10
	    SIGT(N) = 0.
	    PEAK(NPT+1,N) = -1.E10
	    COUNTT(N) = 1.E-10
	    COUNTT(N+5) = 1.E-10
	    WRITE(TITLE(N),1234) N*FFUND
	  ENDDO
	 item = 'channel_number'
	 ok = w_item_i4(ch, item, fft_channel, 1, return_size)
	 item = 'SOURCE'
	 ok = w_item_i4(ch, item, IRX, 1, return_size)
	 if(fft_channel.eq.4) go to 1010
c
 1000	   continue			! START NEW AVERAGING SERIES
c
c	countt(1) is densi
c	countt(2) is vsw
c	countt(3) is bmag
c	countt(5) is time
c	countt(6) to (15) is for signal
c
	  DENSIT = 0.
	  VSWT = 0.
	  BMAGT = 0.
	  NSV = 0
	  DO N = 1,10
	    SIGT(N) = 0.
	    PEAK(NPT+1,N) = -1.E10
	    COUNTT(N) = 1.E-10
	    COUNTT(N+5) = 1.E-10
	    WRITE(TITLE(N),1234) N*FFUND
	  ENDDO
	  WRITE(TITLE(5),1235) .255*FFUND
 1234	  FORMAT(F6.1,' Hz')
 1235	  FORMAT(F6.3,' kHz')
C
C	   if(TIMEINT.ne.0.) then
C	     scet = scet + TIMEINT
C	     scet_ret = scet
C 	     call w_channel_position(ch,scet_ret)
C	   endif
C
 1001	 continue			! COLLECT DATA
	 ok = w_event(ch,event)
	 item = 'channel_number'
	 okt = w_item_i4(ch, item, fft_channel, 1, return_size)
C	 if(fft_channel.ne.1.and.ok.ne.82) go to 1001
	 item = 'SOURCE'
	 okt = w_item_i4(ch, item, IRX, 1, return_size)
	 if(IRX.ne.4.and.ok.ne.82) go to 1001
c
 1010	 if(ok.eq.82) then
		ndaysdone = ndaysdone + 1
		type*,'finished at end of file',file
		type*,'DAYS DONE SO FAR',ndaysdone,' count',count(1)
		ok = wind_tm_close_channel(ch)
c		if(ndaysdone.lt.29) go to 3000
c		if(ndaysdone.lt.2) go to 3000
c		if(ndaysdone.lt.1) go to 3000
		go to 2000
c
	  endif
	if(ok.ne.1) go to 1001
c	
	call fft_phys(ch,iraw,vdata,dbspec)
C	print*,'dc term',dbspec(1)
C	write(22,*) scet,iraw
	item = 'EVENT_SCET'
	okt = w_item_i4(ch, item, s_scet, 2, ret_size)
        item = 'EVENT_SCET_R8'
	ok = w_item_r8(ch, item, scet, 1, ret_size)
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
C	if(major.eq.last_major) PRINT*,'MAJOR FRAME DID NOT ADVANCE',MAJOR
C	if(major.eq.last_major) go to 1000
	last_major = major
c
c
	hh = S_SCET(2)/10000
	min = mod(S_SCET(2),10000)/100
	SS = MOD(S_SCET(2),100)
c	scet = dfloat(nday) + hh/24. + min/1440. + SS/86400.
c
	NSV = NSV+1
	DO N = 1,4
	  COUNTT(N+5) = COUNTT(N+5) + 1.
C	  SIGT(N) = SIGT(N) + 10.**(.1*DBSPEC(N+1))
	  SIGT(N) = SIGT(N) + DBSPEC(N+1)
	  PEAK(NPT+1,N) = AMAX1(PEAK(NPT+1,N),DBSPEC(N+1))
	  SIGSV(NSV,N) = DBSPEC(N+1)
	ENDDO
c	print*,'npt,peak,dbspec',npt,peak(npt+1,1),dbspec(2)
	SIGT(5) = SIGT(5) + DBSPEC(256)
	COUNTT(10) = COUNTT(10) + 1.
	SIGSV(NSV,5) = DBSPEC(256)
	PEAK(NPT+1,5) = AMAX1(PEAK(NPT+1,5),DBSPEC(256))
C
	item = 'WIND_3DP_ION_DENSITY_R4'
	ok = w_item_r4(ch, item, densitt, 1, ret_size)
	if(ok.ne.1.) then
	     print*,'3dp data not available,',s_scet
c	     return
	else
C	     PRINT*,'DENS CHECK',DENSITT,DENSIT,COUNTT(1)
	     if(abs(densitt).lt.1000.) then
	       densit = densit + densitt
	       countt(1) = countt(1) + 1.
	     endif
c
C	     item = 'WIND_3DP_E_TEMP_R4'
C	     ok = w_item_r4(ch, item, densit, 1, ret_size)
c	     tempel(is) = densit
c	     item = 'WIND_3DP_ION_TEMP_R4'
c	     ok = w_item_r4(ch, item, densit, 1, ret_size)
c	     tempion(is) = densit
C
	     item = 'WIND_3DP_ION_VX(GSE)_R4'
	     ok = w_item_r4(ch, item, VSWTT, 1, ret_size)
	     IF(ABS(VSWTT).LT.2000.) THEN
	       VSWT = VSWT + VSWTT
	       COUNTT(2) = COUNTT(2) + 1.
	     ELSE
	       PRINT*,'VSW,OK=',VSWTT,OK
	     ENDIF
	endif
C
	item = 'WIND_MFI_BMAG_R4'
	ok = w_item_r4(ch, item, BMAGTT, 1, ret_size)
	IF(ABS(BMAGTT).LT.1.E4) THEN
	  BMAGT = BMAGT + BMAGTT
	  COUNTT(3) = COUNTT(3) + 1.
	ELSE
	  PRINT*,'B DATA,OK',BMAGTT,OK
	ENDIF
	IF((SCET-SCETSTAV).LT.TIMEINT) GO TO 1001
c
 1050	continue			! store values and go back
	npt = npt+1
	densi(npt) = densit/countt(1)
	VSW(npt) = VSWT/countt(2)
	BMAG(npt) = BMAGT/countt(3)
	SCOUNT = NSV
	PTIME(NPT) = .5E00*(SCET+SCETSTAV) - SCETST
	DO N = 1,5
C	  SIG(NPT,N) = 10.*ALOG10(SIGT(N)/COUNTT(N+5))
	  SIG(NPT,N) = SIGT(N)/COUNTT(N+5)
C
	  SIGSUM = 0.
	  SIGSQ = 0.
	  SIGMAX = -1000.
C	AT THIS POINT, SORT SIGSV
          DO M = 1,NSV
	    SIGSUM = SIGSUM + SIGSV(M,N)
	    SIGMAX = AMAX1(SIGSV(M,N),SIGMAX)
	    SIGSQ = SIGSQ + SIGSV(M,N)**2
	  ENDDO
	  SIGAV = SIGSUM/SCOUNT	  
	  SIGSTD = SIGSQ/SCOUNT - SIGAV**2
	  SIGSTD = SQRT(AMAX1(SIGSTD,0.))
c	  print 432, nsv,ptime(npt),sigav,sigmax
c     1     ,sigstd,(sigmax-sigav)/sigstd
 432	  format(i6,f10.4,7f8.1)
C	  print*,'n,sigav,sigstd,SIGSQ',sigav,sigstd,SIGSQ
	ENDDO
C	
C	REMOVE OUTLIERS
C
C	CALL SORTN(SIGSV(1,N),NSSV,NSV)
	WRITE(27,1027) PTIME(NPT),(SIG(NPT,N),N=1,4),DENSI(NPT),BMAG(NPT),
     1	  VSW(NPT)
C	PRINT 1027, PTIME(NPT),(SIG(NPT,N),N=1,4),(peak(NPT,N),N=1,4)
C	PRINT 1027, PTIME(NPT),(SIG(NPT,N),N=1,4),DENSI(NPT),BMAG(NPT),
C     1	  VSW(NPT),COUNTT(6)
 1027	FORMAT(F10.4,4F7.1,F6.1,F6.1,F8.1,F7.0)
	SCETSTAV = SCET
	GO TO 1000
C
 2000	continue			! finish up and return
c
c	close data storage file
c
	return
	end
      SUBROUTINE SORTN(XS,FOLLOW1,NS)
C
      REAL XS(1)
      INTEGER*4 FOLLOW1(1)
C
C     ORDER X COORDS IN INCREASING ORDER
C
      DO 11 N = 1,NS-1
      N1 = N + 1
      DO 12 M = N1,NS
      IF((XS(M)-XS(N)).GT.0.) GO TO 12
C     EXCHANGE
      XT = XS(N)
      XS(N) = XS(M)
      XS(M) = XT
      IXT = FOLLOW1(N)
      FOLLOW1(N) = FOLLOW1(M)
      FOLLOW1(M) = IXT
   12 CONTINUE
   11 CONTINUE
      RETURN
      END
	SUBROUTINE TPLOT(NPT,IRX,IFFTCH)
C
	COMMON /PFILE/ PTIME(4000),SIG(4000,10),PEAK(4000,10),
     1		DENSI(4000),VSW(4000),BMAG(4000),TITLE(10)
	REAL XX(4000),YYA(4000),YYP(4000)
	CHARACTER*80 	STR
	character*20	title
	character*4	pa(9)         
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',         
     1		'BX','BY','BZ'/         

C
	PRINT*,'TPLOT CALLED, NPT=',NPT
C
	ITERM = -1		! 370a, portrait
C	ITERM = 3
	XSTART = 500.
	XEND = 2300.
C
	NWNDOW = 8
	IWCH = 1
	IF(NPT.LT.1) NPT = 1             	  ! protection
	PRINT*,' IN TPLOT,TIMES',PTIME(1),PTIME(NPT),NPT
C	CALCULATE BOX SIZE
	PIXSIZ = .1
	IF(NPT.GT.1) PIXSIZ = (PTIME(NPT) - PTIME(1))/(NPT-1)
	HSTART = PTIME(1) - .5*PIXSIZ
	HEND =   PTIME(NPT) + .5*PIXSIZ
	PRINT*,' IN TPLOT',HSTART,HEND,NPT
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART-100.,280.,XEND,1500.)
	  print*,'in tplot, irx= ',irx
	  IF(IRX.GT.0.AND.IRX.LE.9) CALL MGOYLABEL(4,PA(IRX))
	  CALL MGOSETLOC(XSTART-100.,280.,XEND,2200.)
	  CALL MGOYLABEL(16,' dB (V/m)\u2/Hz')
	  CALL MGOSETLOC(XSTART,280.,XEND,3100.)
	ENDIF
C
	NWNDOW = 8
	DO IW = 1,NWNDOW
	  CALL MGOWINDOW(1,NWNDOW,IW)                          ! 
	  YMIN = 1.E6
	  YMAX = -YMIN
	  IF(IW.LE.5) THEN
	    YMAX = -1000.
	    YMIN =  1000.
	    DO N = 1,NPT	
	      XX(N) = PTIME(N)
	      YYA(N) = SIG(N,IW)
	      YMIN = AMIN1(YYA(N),YMIN)
	      YMAX = AMAX1(YYA(N),YMAX)	      
	      YYP(N) = PEAK(N,IW)
C	      YMAX = AMAX1(YYP(N),YMAX)	      
	    ENDDO
	    IF(IW.EQ.5) YMAX = AMIN1(YMAX,-140.)
	    IF(IW.EQ.5.AND.IFFTCH.GT.2) YMAX = AMIN1(YMAX,-120.)
	print*,'iw,ymin,ymax',iw,ymin,ymax
	    YRANGE = YMAX-YMIN
	    YMIN = YMIN - .05*ABS(YRANGE)
	    YMAX = YMAX + .05*ABS(YRANGE)
	    CALL MGOSETLIM(HSTART,YMIN,HEND,YMAX)
	    CALL MGOCONNECT(XX,YYA,NPT)
	    CALL MGOCONNECT(XX,YYP,NPT)
	    CALL MGOBOX(1,2)
	    CALL MGORELOCATE(HSTART+.05*(HEND-HSTART),YMIN + .8*(YMAX-YMIN))
	    CALL MGOLABEL(10,TITLE(IW))
	  ELSE   	
	    YMAX = -1000.
	    YMIN =  1000.
	    IF(IW.EQ.6) THEN
	      DO N = 1,NPT	
	        YYA(N) = DENSI(N)
	        YMIN = AMIN1(YYA(N),YMIN)
	        YMAX = AMAX1(YYA(N),YMAX)	      
	      ENDDO
	      STR = 'DENS'
	    ENDIF
	    IF(IW.EQ.7) THEN
	      DO N = 1,NPT	
	        YYA(N) = VSW(N)
	        YMIN = AMIN1(YYA(N),YMIN)
	        YMAX = AMAX1(YYA(N),YMAX)	      
	      ENDDO
	      YMIN = AMAX1(-900.,YMIN)
	      STR = ' VSW'
	    ENDIF
	    IF(IW.EQ.8) THEN
	      DO N = 1,NPT	
	        YYA(N) = BMAG(N)
		IF(BMAG(N).LT.200.) YMIN = AMIN1(YYA(N),YMIN)
	        IF(BMAG(N).LT.200.)YMAX = AMAX1(YYA(N),YMAX)	      
	      ENDDO
	      STR = ' |B|'
	      YMAX = AMIN1(YMAX,30.)  	  
	    ENDIF
	print*,'iw,ymin,ymax',iw,ymin,ymax
	    YRANGE = YMAX-YMIN
	    YMIN = YMIN - .05*ABS(YRANGE)
	    YMAX = YMAX + .05*ABS(YRANGE)
	    CALL MGOSETLIM(HSTART,YMIN,HEND,YMAX)
	    CALL MGOCONNECT(XX,YYA,NPT)
	    CALL MGOBOX(1,2)
	    CALL MGOYLABEL(4,STR)
	  ENDIF   			! END OF FFT SIGNAL PLOTS
	ENDDO
	CALL MGOPLOTID('MAKEFILE22','FFT AVS')
C
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	ELSE
C	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END
