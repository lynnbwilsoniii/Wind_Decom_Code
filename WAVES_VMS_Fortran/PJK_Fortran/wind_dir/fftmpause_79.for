! wind/waves fft data collection, mag field, and pk-avr program

	program		fft_mpause
	implicit	integer*4 (a-z)

	print*,'fftmpause.exe needs 5221 blocks'
	ok = 1
	if (ok) ok = get_set_tm_stream()
	TYPE*,'GOING TO COLLECT EVENT DATA'
	nchgp = 3
	ok = wait_for_events(nchgp)
	ITERM = -1
	TYPE*,' GOING TO TPLOT'
	CALL TPLOT(ITERM)
C
	end
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_set_tm_stream()
c	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	character*4	event,pa(9)
	integer*4	iq,i,j,k,n,iterm,nchgp,npt,np,nday
	integer*4	iw
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*80	stream
	parameter	size=1024
	integer*4	return_size
	integer*4	raw(size)
	integer*4	mantissa(size)
	integer*4	gain(size)
	integer*4	pwrdb(size)    ! actually, .5 db steps
	integer*4	fft_channel
	integer*4	temp_waves
	character*32	s
	real*8		scet8,scetsrt,scethk
	character*32	s_scet
	real*8		mfi_scet(2000),hk_scet(2000)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		bphi(2000),btheta(2000)
	real*4		exav(2000),expk(2000),ezav(2000)
	integer*4	scet(2)
	integer*4	ch, chhk, major, minor,itmcorr,iraw
	character*80	file
	character*10	title(4)
	character*32	item
	integer*4	ios,yyyy,idoy,msec
	integer*4	NHRMN,IPNT,LAST,NPTMX,N1,N2,NFILL,NPPL
	REAL		PTIME,DELT,DELTS
	REAL		tstart,eltime,evtime,tend,tintv
	real		nhist(100,10),vdata(1024),dbspec(1024)
	integer*2	pwrl
	integer*2	pwrlt(1000,511)
!

	common /fftblk/ gain,mantissa,pwrdb
	COMMON /PLTPAR/ NDAY,IPNT(4),ITMCORR,IDOY
	COMMON /SHBLK/ PTIME(1000),PWRL(1000,511,2)
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag,bphi,btheta
	common /pltblk5/ nhk,hk_scet,exav,expk,ezav
	common /headblk/ major,minor,fft_channel,s_scet,title
!
	DATA ITMCORR /1/             ! 1 = CORRECT FOR TIME, 0 = PLOT
C					CONSECUTIVE SPECTRA
	data nhk /0/
	DATA IPNT /4*0/
	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1		'BX','BY','BZ'/
	DATA JP /0/
C
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'

	ok = w_channel_open(ch,stream)
	if (.not. ok) stop 'cannot open tm channel'

	write(6,*) 'type hr,min to start,end, e.g. 0414,0800'
	read(5,*) nhrmn,last
	type*,nhrmn,last
	tstart = (nhrmn/100) + mod(nhrmn,100)/60.
	tlast = (last/100) + mod(last,100)/60.
 3	format(q,i10)
 4	format(1x,'enter 1 for hi channels, 2 for mid, 3 for low')
 5	format(q,a)
	do n = 1,4
	  title(n) = '          '
	enddo
 
	ok = w_channel_filename(ch,file)
	print*,'file',file

	scet8 = 0.
	call w_channel_position(ch,scet8)
	nday = scet8
	type*,'first channel position ',scet8,' ch',ch
	scet8 = dfloat(nday) + ((nhrmn/100) + mod(nhrmn,100)/60.)/24. 
	scetsrt = scet8
	type*,'set channel position to',scetsrt,' ch',ch

	get_set_tm_stream = 1
	return

	!----------------------------------------------------------------------
	entry	wait_for_events(nchgp)
c
	PRINT*,'START NCHGP=',NCHGP
C
	call w_channel_position(ch,scetsrt)
	type*,'channel position set to',scetsrt

	  event = 'FFT'
	  if(nchgp.eq.1) event = 'FFTH'
	  if(nchgp.eq.2) event = 'FFTM'
	  if(nchgp.eq.3) event = 'FFTL'
	  ok = w_event(ch,event)
	  if(ok.eq.82) then
		PRINT*,'END OF FILE'
C		TYPE*,'GO TO PLOT AT ',S_SCET,' previous'
	        GO TO 200
c		RETURN
	  endif

	  NEWOLD = 0
	  IF(NCHGP.EQ.0) THEN
		NCHGP = 3
	        NEWOLD = 1
	  ENDIF
	  print*,'collect events: ',event

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, scet, 2, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	   type*,'first event at',scet,'  evtime',evtime

C
	   item = 'EVENT_SCET_R8'
	   ok = w_item_r8(ch, item, scet8, 1, return_size)
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
c
	  go to 110

	! this is the main program loop

 100	   continue

c	  if( w_end_of_file(ch) ) then

           ok = w_event(ch,event)
	   if (.not. ok) then
	      type *, char(7), '******** missing packet in event ********'
	      type *, 'Cannot get event at MF.mf: ', major, minor
	      if( ok .eq. 82) then
		type*,'npt=',npt
		GO TO 200
c		return
	      endif
c	      if (wind_tm_realtime(ch)) then
c	         ok = wind_tm_get_latest_mfmf(ch,major,minor)
c	         type *, '-reset to latest position: ', major, minor
c	      else
c	         call wind_tm_increment_packet(major,minor)
c	         type *, '-incrementing packet...'
c	      end if
	      err_count = err_count + 1
	      if (err_count .lt. 2) goto 100
	      if (err_count .ge. 2) goto 200
	   end if

 110	  if(wind_tm_eof(ch,major,minor)) then
		PRINT*,'END OF FILE'
		TYPE*,'GO TO PLOT AT ',S_SCET
		RETURN
	  endif

	   ! now get the items

	   item = 'channel_number'
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   if(ok .eq. 82) type *,' end of file'
	   if(ok .eq. 82) return
C
C	PLOT ONLY EX AND EZ
C
	   item = 'SOURCE'
	   ok = w_item_i4(ch, item, IQ, 1, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
C
	   IF(IQ.NE.4.AND.IQ.NE.6) GO TO 100
C
	   IF(IQ.EQ.4) IW = 1
	   IF(IQ.EQ.6) IW = 2
	   IF(TITLE(iw).EQ.'          ') THEN
	     WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(IQ)
C	     TITLE(iw) = PA(IQ)
	   ENDIF
 1001	   FORMAT('CH',I2,'  ',A4)

C
	call fft_phys(ch,iraw,vdata,dbspec)
C	   item = 'SPECTRUM_DB'
C	   ok = w_item_R4(ch, item, DBSPEC, 1024, return_size)
C
C	CALCULATE TOTAL POWER
C
c	      TPOWER = 0.
c	      do n = 6,512
c		TPOWER = TPOWER + EXP(.23026*DBSPEC(N))
c	      enddo
c	      TPOWERDB = 10.*ALOG10(TPOWER)
c
c
C	IW is the index of the channels in pwrl, 
C

c	   endif

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, scet, 2, return_size)
	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
	   eltime = evtime - tstart
C
 1002	FORMAT(2I8,I4,F9.3,E12.3)
C	
	
	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)
	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

c
	   NPT = IPNT(IW) + 1
	   IPNT(IW) = NPT
	   PTIME(NPT) = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.
     1		+ mod(scet(2),100)/3600.
c	print*,'npt,iw,iraw',npt,iw,iraw
	   do i=1,511
c		if(dbspec(i+1).LE.-256.) 
c     1		TYPE*,'DB',I,PTIME(NPT),DBSPEC(I+1)
c		if(dbspec(i+1).GT.256.) 
c     1		TYPE*,'DB',I,PTIME(NPT),DBSPEC(I+1)
		if(dbspec(i+1).gt.-256..and.dbspec(i+1).lt.256.)
     1		PWRL(NPT,I,IW) = 2.*DBSPEC(I+1) + .5
	   end do

	   IF(NPT.GT.995) GO TO 200
	   IF(scet(2).LT.(100*LAST)) GO TO 100

 200	continue

	TYPE*,NPT,' SPECTRA FOUND, EACH CHANNEL'
	TYPE*,'GO TO HK AT ',S_SCET


	TEND = 0.
	NPTMX = 0
	  NW1 = 1
	  NW2 = 2
C
	DO I = NW1,NW2
	  NPT = IPNT(I)
	  NPTMX = MAX0(NPT,NPTMX)
	  IF(NPT.GT.0) TEND = AMAX1(TEND,PTIME(NPT))
	ENDDO
	print*,'nptmx,ipnt',nptmx,ipnt
C
	call w_channel_position(ch,scetsrt)
	type*,'channel position set to',scetsrt,' for mfi'
          ok = w_event(ch,'CDF')
	  item = 'WIND_MFI_SCET_R8'
	  ok = w_item_r8(ch, item, mfi_scet, 2000, return_size)
	  nmfi = return_size
	  print*,'initial mfi time',mfi_scet(1),' size',return_size
	  if(nmfi.gt.0) print*,'last mfi time',mfi_scet(return_size)
	  item = 'WIND_MFI_BX(GSE)_R4'
	  ok = w_item_r4(ch, item, BX, 2000, return_size)
	  item = 'WIND_MFI_BY(GSE)_R4'
	  ok = w_item_r4(ch, item, BY, 2000, return_size)
	  item = 'WIND_MFI_BZ(GSE)_R4'
	  ok = w_item_r4(ch, item, BZ, 2000, return_size)
	  item = 'WIND_MFI_BMAG_R4'
	  ok = w_item_r4(ch, item, BMAG, 2000, return_size)
	  item = 'WIND_MFI_BPHI(GSE)_R4'
	  ok = w_item_r4(ch, item, BPHI, 2000, return_size)
	  item = 'WIND_MFI_BTHETA(GSE)_R4'
	  ok = w_item_r4(ch, item, BTHETA, 2000, return_size)

C
	call w_channel_position(ch,scetsrt)
	type*,'channel position set to',scetsrt,' FOR HK'
	err_count = 0

 300	 continue
c
	   EVENT = 'HK'
	   ok = w_event(ch,event)
	   if( .not.ok ) then
		print*,'could not get HK event'
		IF(OK.EQ.82) RETURN
	        err_count = err_count + 1
	        if (err_count .lt. 4) goto 300
		PRINT*,'RETURN FROM NOT OK ,NMFI,NHK=',NMFI,NHK
	   	RETURN
	   endif
	   err_count = 0
	   nhk = nhk + 1
	   item = 'EVENT_SCET_R8'
	   ok = w_item_r8(ch, item, scethk, 1, ret_size)
	   hk_scet(nhk) = scethk
	   
	   item = 'APM_X_PEAK'		! word 63
	   ok = w_item_i4(ch, item, nx_peak, 1, ret_size)
		expk(nhk) = apmcal(1,nx_peak)

	   item = 'APM_X_DC'		! word 65
	   ok = w_item_i4(ch, item, nx_dc, 1, ret_size)
	   exav(nhk) = apmcal(3,nx_dc)
C	   type*,'hk,time,nexav,nexpk',NHK,scethk,nx_dc,nx_peak

	   item = 'APM_Z_DC'		! word 67
	   ok = w_item_i4(ch, item, nz_dc, 1, ret_size)
	   ezav(nhk) = apmcal(5,nz_dc)
c
	   IF(24.*(scetHK-NDAY).LT.TLAST) GO TO 300


	PRINT*,'RETURN FROM WAIT FOR EVENTS,NMFI,NHK=',NMFI,NHK
	return
	end
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	common /nrblk/ NHRMN
	integer*4	iq,NHRMN

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

 20	return
	end
	SUBROUTINE RAWPRINT
C
	common /fftblk/ nexpt(1024),ndata(1024),ipwrdb(1024)
	common /headblk/ major,minor,ich,scet,title
	character*32 scet
	character*10	title(4)
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
	SUBROUTINE TPLOT(ITERM)
C
C	PLOTS THE TIME DOMAIN DATA
C
	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,2)
	COMMON /PLTPAR/ NDAY,IPNT(4),ITMCORR,IDOY
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)
	common /headblk/ major,minor,ifftch,scet,titlex
	common /pltblk1/ nmfi,mfi_scet,bx,by,bz,bmag,bphi,btheta
	common /pltblk5/ nhk,hk_scet,exav,expk,ezav
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
	character*32 	scet
	integer*2 	npwrl
	character*10	titlex(4)
	REAL		FUNDF(3)
	real*8		mfi_scet(2000),hk_scet(2000)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		bphi(2000),btheta(2000)
	real*4		exav(2000),expk(2000),ezav(2000)
	DIMENSION PP(2048),YY(2048),ZZ(2048)
	CHARACTER*8 TITLE(4)
	CHARACTER*120 STRG(5)
	CHARACTER*120 STR
	DATA TITLE/'RECT.','HAMMING','HANNING',' '/
	DATA FUNDF /.021348,.005337, .33356/
C
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
C	  CALL MGOSETLOC(500.,330.,2300.,3000.)
	  PRINT*,'START TPLOT',GX1,GX2,GY1,GY2
C	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-100.)
	ENDIF
C
	IW = 1
	NPT = IPNT(IW)
	PRINT*,' IN TPLOT,npt,iterm',NPT,iterm
	IF(NPT.LT.1) THEN             	  ! protection
	  IW = 2
	  NPT = IPNT(IW)
          IF(NPT.LT.1) RETURN
	ENDIF
	  PRINT*,' IN TPLOT',NPT,' IW',IW
	PRINT*,' IN TPLOT,TIMES',PTIME(1),PTIME(NPT),NPT
C	CALCULATE BOX SIZE
	XRANGE = PTIME(NPT) - PTIME(1)
	XMIN = PTIME(1) - .01*XRANGE
	XMAX = PTIME(NPT) + .01*XRANGE
C
	CALL MGOSETANGLE(90.)
	CALL MGOGRELOCATE(100., .5*(GY1+GY2))
	CALL MGOPUTLABEL(15,'DB V\u2/M\u2-Hz rms\e',2)
	CALL MGOSETANGLE(0.)
          WRITE(STR,1001) SCET(1:10),IDOY
 1001	  FORMAT('HOURS OF ',A10,' DOY ',I3)
C	  YPR = GY1 + .005*(GY2-GY1)
	  YPR = 200.
	  XPR = .5*(GX1+GX2)
	  CALL MGOGRELOCATE(XPR,YPR)
	  CALL MGOSETEXPAND(.8)
	  CALL MGOPUTLABEL(28,STR,2)                  
C	  CALL MGOSETANGLE(0.)
	  CALL MGOSETEXPAND(.8)
	  TOP = LY2
	  CALL MGOGRELOCATE(GX1,TOP-100.)        
	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3)
c	  WRITE(STR,802) titlex(iw)
	  STR = TITLEX(IW)
	  CALL MGOPUTLABEL(8,STR(3:4),3)
	  CALL MGOPUTLABEL(10,'   SOURCE ',3)
	  CALL MGOPUTLABEL(10,TITLEX(IW),3)
C
	DO JW = 1,4
	  CALL MGOWINDOW(1,8,JW)
	  CALL MGOSETEXPAND(.6)
	  GY1S = GY1
	  GY1 = GY1 - .14*(GY2-GY1)
	  TOP = LY2
 802	  FORMAT(2I6)
	  CALL MGOSETEXPAND(1.)
C
	  YMAX = -1000.
	  YMIN = -YMAX
	  JP = 1
	  DO J = 1,NPT
	    JP = J
	    PP(JP) = PTIME(J)
	    YY(JP) = .5*NPWRL(J,JW+1,IW)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  print*,'jw,ymin,ymax',jw,ymin,ymax
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .1*RANGE
	  YMIN = YMIN - .1*RANGE	
C	  CALL MGOTICKSIZE(20.,100.,0.,0.)
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PTIME,YY,JP)
	  WRITE(STR,801) JW*FUNDF(3)
 801	  FORMAT(F6.2,' Hz')
	  CALL MGORELOCATE(XMIN + .05*XRANGE,YMAX-.2*RANGE)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOLABEL(9,STR)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOBOX(1,2)
	  IF(JW.EQ.1) CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  CALL MGOYLABEL(2,'DB')
	  CALL MGOSETEXPAND(1.)
	  GY1 = GY1S
	ENDDO
C
C	PLOT MAGNETIC FIELD PHI (DC)
C
	  CALL MGOWINDOW(1,8,5)
	  CALL MGOSETEXPAND(.6)
C
	  YMAX = -1000.
	  YMIN = -YMAX
	  JP = 1
	  DO J = 1,NMFI
	    JP = J
	    DAYF = MFI_SCET(J) - NDAY
	    PP(JP) = 24.*DAYF
	    YY(JP) = BPHI(J)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PP,YY,JP)
	  CALL MGORELOCATE(XMIN + .05*XRANGE,YMAX-.2*RANGE)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOBOX(1,2)
C	  IF(JW.EQ.1) CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  CALL MGOYLABEL(4,'Bphi')
	  CALL MGOSETEXPAND(.7)
C
C	PLOT MAGNETIC FIELD THETA (DC)
C
	  CALL MGOWINDOW(1,8,6)
	  CALL MGOSETEXPAND(.6)
C
	  YMAX = -1000.
	  YMIN = -YMAX
	  JP = 1
	  DO J = 1,NMFI
	    JP = J
	    DAYF = MFI_SCET(J) - NDAY
	    PP(JP) = 24.*DAYF
	    YY(JP) = BTHETA(J)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PP,YY,JP)
	  CALL MGORELOCATE(XMIN + .05*XRANGE,YMAX-.2*RANGE)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOBOX(1,2)
C	  IF(JW.EQ.1) CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  CALL MGOYLABEL(7,'Btheta')
	  CALL MGOSETEXPAND(.7)
C
C	PLOT MAGNETIC FIELD (DC)
C
	  CALL MGOWINDOW(1,8,7)
	  CALL MGOSETEXPAND(.6)
C
	  YMAX = -1000.
	  YMIN = -YMAX
	  JP = 1
	  DO J = 1,NMFI
	    JP = J
	    DAYF = MFI_SCET(J) - NDAY
	    PP(JP) = 24.*DAYF
	    YY(JP) = BMAG(J)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PP,YY,JP)
	  CALL MGORELOCATE(XMIN + .05*XRANGE,YMAX-.2*RANGE)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOBOX(1,2)
C	  IF(JW.EQ.1) CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  CALL MGOYLABEL(7,'Bmag nT')
	  CALL MGOSETEXPAND(.7)
C
C	PLOT EX PEAK AND AV (DC)
C
	  CALL MGOWINDOW(1,8,8)
	  CALL MGOSETEXPAND(.6)
C
	  YMAX = -1000.
	  YMIN = -YMAX
	  JP = 1
	  DO J = 1,NHK
	    JP = J
	    DAYF = HK_SCET(J) - NDAY
	    PP(JP) = 24.*DAYF
	    YY(JP) = EXAV(J)
	    ZZ(JP) = EXPK(J)
	      YMAX = AMAX1(YY(JP),YMAX)
	      YMIN = AMIN1(YY(JP),YMIN)
	      YMAX = AMAX1(ZZ(JP),YMAX)
	      YMIN = AMIN1(ZZ(JP),YMIN)
	  ENDDO
C
	  RANGE = ABS(YMAX-YMIN)
	  YMAX = YMAX + .05*RANGE
	  YMIN = YMIN - .05*RANGE	
	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	  CALL MGOCONNECT(PP,YY,JP)
	  CALL MGOSETLTYPE(1)
	  CALL MGOCONNECT(PP,ZZ,JP)
	  CALL MGOSETLTYPE(0)
	  CALL MGORELOCATE(XMIN + .05*XRANGE,YMAX-.2*RANGE)
	  CALL MGOSETEXPAND(.6)
	  CALL MGOBOX(1,2)
C	  IF(JW.EQ.1) CALL MGOXLABEL(16,'HOURS OF THE DAY')
	  CALL MGOYLABEL(8,'EX,PK,AV')
C
	CALL MGOSETEXPAND(.7)
	CALL MGOPLOTID('[.WIND]FFTMPAUSE','TPLOT')
	CALL MGOSETEXPAND(1.)
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
	REAL*4		function	APCCAL(NTM)
	implicit	none
	real		x
	integer*4 	ntm
	
!	preliminary calibration, needs replacement
C	apccal = .07843*ntm - 10.
C	WAS REPLACED 30 NOV 1994

	x = ntm
	apccal = -9.9874 + .07955*x - 5.0168E-5*x**2
     1	+ 9.7875E-7*x**3 - 8.4809E-9*x**4 + 3.3717E-11*x**5
     2	- 5.016E-14*x**6

	return
	end


