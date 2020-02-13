 	PROGRAM AUTO_CDF
C
C	GETS CDF DATA for resistance measurements
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	i,j,k,n,itemp
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*120	JUNK
	character*80	stream
	character*20	MFI_FILE
	character*4	event
	character*8	yymmdd
	parameter	size=2048
	integer*4	return_size
	integer*4	temp_waves,iend,n2,S_SCET(2)
	character*32	s
	real*8		SCET_SWE(2000)
	real*8		scet_wind(2000),windx(2000),windy(2000),windz(2000)
	real*4		bmag(2000),bx(2000),by(2000),bz(2000)
	real*4		vmag(2000),vx(2000),vy(2000),vz(2000),dens(2000)
	real*4		vmag3(2000),vx3(2000),vy3(2000),vz3(2000)
	real*4		etemp(2000),efq(2000),ef250(2000),ef1(2000)
	real*4		ef5(2000),ef20(2000),ef34(2000),ef90(2000)
	real*4		dens3dp(2000),densswe(2000),denstnr(2000)
	real*8		SCET_TNR(2000),scet_3dp(2000)
	real*8		scet,scet8
	integer*4	major, minor
	character*80	file
	character*32	item
	integer*4	ios,ms,doy,msday,dds
	integer*4	NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
!
	common /pltblk3/ n3dp,SCET_3DP,vx3,vy3,vz3,vmag3,dens3dp
     1	  ,etemp,efq,ef250,ef1,ef5,ef20,ef34,ef90
	common /pltblk4/ ntnr,SCET_TNR,denstnr
	common /pltblk6/ nWIND ,SCET_WIND,WINDX,WINDY,WINDZ

C
	CHARACTER*12 PTITLE(20)
	INTEGER*4 cdfch,hkch,fillch,ch
	COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
	COMMON /HEADBL/ NHRMN,LAST,PTITLE,EVENT,FILE
	DATA TWOPI /6.2831853/
C
C	GET STARTED
C
!
	open(unit=90,file='accrres.dat',status='old')
	open(unit=91,file='accrres.dat',status='new')
 110	stream = '          '
c	print*,'going to read, stream=',stream
	read(90,1001,end=200) yymmdd,nhrmn,last,rxsun,rxshd,
     1		rysun,ryshd,densr,etempr,efluxr
 1001	format(A9,2i5,7f8.2)
	print 1001, yymmdd,nhrmn,last,rxsun,rxshd,
     1		rysun,ryshd,densr,etempr,efluxr
	stream(1:8) = yymmdd
c	print*,'going to get_stream_name,stream=',stream
	ok = get_stream_name(stream)
c	print*,'return from get_stream_name,stream=',stream
	if (ok . ne . 1) stop 'no file found.'
c
c	'stream' is replaced by full file name
C
	  type*,NHRMN,LAST
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
c
	PRINT*,'DENSR,ETEMPR=',DENSR,ETEMPR
	if(densr.ne.0.0.or.etempr.ne.0.0) then
	  write(91,1001) yymmdd,nhrmn,last,rxsun,rxshd,
     1		rysun,ryshd,densr,etempr,efluxr
	  PRINT*,'READ ANOTHER'
	  go to 110
	else
	  PRINT*,'PROCESS THIS ONE, YYMMDD=',YYMMDD
	  go to 100
	endif 
c
  5	format(q,a)
  3	format(q,i10)
c

 100	ok = w_channel_open(cdfch,stream)
	if ( ok.ne.1 ) stop 'Cannot open cdhf channel'
	scet8 = 0.
	call w_channel_position(cdfch,scet8)
	print*,'cdhf file starts at scet8',scet8
	dds = scet8
	scet8 = float(dds) + hh/24. + mm/1440.
	print*,'set cdhf channel position to',scet8
	call w_channel_position(cdfch,scet8)
	print*,'cdhf channel position set to',scet8
c
	ok = w_channel_filename(cdfch,file)
C	write(87,*) file
	print*,file
c
	get_tm_stream = 1
c
	call w_ur8_to_ymd(scet8,yyyy,mon,dd,hh,mm,ss,ms)
	call w_ur8_to_ydoy(scet8,yyyy,doy,msday)

	  ok = w_event(cdfch,'CDF')
	if(ok.eq.82) then
	   scet8 = 10000.       	! artificially large
	   print*,'end of file'
	   if (.not. ok) stop 'end of WIND data file'
	else
	  item = 'EVENT_SCET'
	  ok = w_item_I4(cdfch, item, S_scet, 2, return_size)
c
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(cdfch, item, dens3dp, 2000, return_size)
	  n3dp = return_size
	  print*,'3dp dens(1)',dens3dp(1),'size',return_size
	  if(n3dp.eq.0) stop 'no 3dp data available'
	  item = 'WIND_3DP_ION_VX(GSE)_R4'
	  ok = w_item_r4(cdfch, item, VX3, 2000, return_size)
	  item = 'WIND_3DP_ION_VY(GSE)_R4'
	  ok = w_item_r4(cdfch, item, VY3, 2000, return_size)
	  item = 'WIND_3DP_ION_VZ(GSE)_R4'
	  ok = w_item_r4(cdfch, item, VZ3, 2000, return_size)
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(cdfch, item, ETEMP, 2000, return_size)
	  item = 'WIND_3DP_E_QDOTB_R4'
	  ok = w_item_r4(cdfch, item, EFQ, 2000, return_size)
	  item = 'WIND_3DP_EF_250EV_R4'
	  ok = w_item_r4(cdfch, item, EF250, 2000, return_size)
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(cdfch, item, SCET_3DP, 2000, return_size)
c
	  item = 'WIND_wav_ne_R4'
	  ok = w_item_r4(cdfch, item, denstnr, 2000, return_size)
	  item = 'WIND_wav_scet_R8'
	  ok = w_item_r8(cdfch, item, SCET_TNR, 2000, return_size)
	  print*,'tnr dens(1)',denstnr(1),'size',return_size
	  ntnr = return_size
c
	  item = 'WIND_ORBIT_X(GSE)_R8'
	  ok = w_item_r8(cdfch, item, WINDX, 2000, return_size)
	  nWIND = return_size
	  item = 'WIND_ORBIT_Y(GSE)_R8'
	  ok = w_item_r8(cdfch, item, WINDY, 2000, return_size)
	  item = 'WIND_ORBIT_Z(GSE)_R8'
	  ok = w_item_r8(cdfch, item, WINDZ, 2000, return_size)
	  item = 'WIND_ORBIT_scet_R8'
	  ok = w_item_r8(cdfch, item, SCET_WIND, 2000, return_size)
	  print*,'WIND size',return_size
C
	endif
c
c
	densav = 0.
	countd = 1.e-8
	etempav = 0.
	countt = 1.e-8
	efluxav = 0.
	countf = 1.e-8
	n = 0
	dowhile(ihrmn.lt.last)
	     n = n+1
	     scet = scet_3dp(n) 
	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
	     ihrmn = 100*hh+mm
C	     print*,'time',ihrmn
	     if(abs(dens3dp(n)).lt.1000.) then
		densav = densav + dens3dp(n)
		countd = countd + 1.
	     endif
	     if(abs(etemp(n)).lt.1000.) then
		etempav = etempav + etemp(n)
		countt = countt + 1.
	     endif
	     if(abs(dens3dp(n)).lt.1000..and.abs(etemp(n).lt.1000.)) then
		efluxav = efluxav + dens3dp(n)*sqrt(etemp(n))
		countf = countf + 1.
	     endif
	enddo
	densav = densav/countd
	etempav = etempav/countt
	efluxav = efluxav/countf
c	
c	   write(s,'(i8.8,i6.6)',iostat=ios) mfi_scet(1), mfi_scet(2)
C	   mfi_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
c
	  write(91,1001) yymmdd,nhrmn,last,rxsun,rxshd,
     1		rysun,ryshd,densav,etempav,efluxav
	go to 110
 200	CONTINUE
	close(unit=91)
	stop 'stop at 200'
	END
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	CHARACTER*80	YYYYMMDD
	integer*4	iq
	DATA IQ /1/
C
C10	  write(6,6)
C	  write(6,7)
C	  read(5,5,err=10,end=20) iq, stream
	print*,'in get_stream_name, initial stream=  ',stream
c
	   YYYYMMDD = STREAM(1:8)
	   WRITE(STREAM,30) YYYYMMDD
 30	   FORMAT('wi_lz_wav_',A8,'_v*.dat')
	   PRINT*,'in get_stream_name, file= ',STREAM
c	end if

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
  7	format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
	end
