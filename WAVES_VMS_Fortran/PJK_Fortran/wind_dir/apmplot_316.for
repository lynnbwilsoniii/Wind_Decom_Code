	program		apmplot
!
c	plots apm for a whole day--weeks would be better--but for now
c
! to build:
!	$ fortran apc_fit
!	$ link apc_fit,wind_lib/lib,mongo
!
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
	integer*4	ok,get_output_device,loop_and_gather
	integer*4	get_stream_name
	character*80	stream
c	
	ok = 1
c	if (ok) ok = get_output_device()
c	TYPE*,'RETURN FROM GET_OUTPUT_DEVICE,OK=',OK
c	write(37,*)'RETURN FROM GET_OUTPUT_DEVICE,OK=',OK
	terminal_output = 0
	if (ok) ok = loop_and_gather()
	TYPE*,'RETURN FROM LOOP_AND_GATHER'
	iterm = -3
	call plotall(iterm)
c	call plotall(3)
	stop
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_output_device()
!	implicit	none
	include		'apmplot_def.for'
	integer*4	ok
	integer*4	i,j,k
	integer*4	ios
	character*1	c
	integer*4	initialize_mongo_xwindows

 10	write(6,'(a,$)') ' Enter output device [H=hardcopy (default), X=xwindow]: '
	read(5,'(q,a)',iostat=ios,err=10) i, c
	if (i.eq.0 .or. c .eq. 'h' .or. c.eq.'H') then
	   ok = 1
	   terminal_output = 0
	else if (c .eq. 'x' .or. c .eq. 'X') then
	   terminal_output = 1
	   ok = initialize_mongo_xwindows()
	else
	   type *, 'No output device selected.'
	   ok = 0
	end if

	get_output_device = ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
!	implicit	none
	character*(*)	stream
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	integer*4	iq,NREM,NHRMN,IFASTSLOW

10	  write(6,6)
	  read(5,5,err=10,end=20) iq, stream
	write(37,*) 'read name',stream

 	if (iq .lt. 1) then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else
	   ! assume the user entered the name of an offline file
	end if
	write(37,*) 'decided on',stream

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	loop_and_gather()
!	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	include		'apmplot_def.for'
	integer*4	ok,okt
	integer*4	ch
	parameter	event='HK  '
	integer*4	ret_size
	integer*4	s_scet(2)
	character*80	stream
	character*16	item
	character*4	year
	character*40	com(15)
	real*4		acount(260),xascan(260),yascan(260),zascan(260)
	real*4		xpscan(260),ypscan(260)
	integer*4	error_count,hh,min,nday
	integer*4	ihrmn,yyyy,mon,dd,mm,ss,ms
	integer*4	iv		! drive telemetry number
	integer*4	ir,irtm		! resistor index
	integer*4	ia		! antenna index, 1=x,2=y,3=z
	integer*4	is		! sample number
	integer*4	IONOFF,nonoff	! DAC ON OR OFF
	integer*4	ndaysdone
	integer*4	major, last_major,cal_on
	real*4 		apccal,apmcal,drivev
	real*4		wt(1000)
	real		output(4,4)
	real		outmongo(200,7)
	real*8		scet,scetst,delt,scet_ret
	data is /0/
	data ntot /0/
	data error_count /0/
	data ndaysdone /0/
	data nstart /0/
	data istart /0/
	data acount /260*1.e-10/
	data xascan /260*0./
	data yascan /260*0./
	data zascan /260*0./
	data xpscan /260*0./
	data ypscan /260*0./
c
 3000	ok = get_stream_name(stream)
	type*,'return from stream name:',stream
	write(37,*) 'return from stream name:',stream
!	ok = wind_tm_open_channel(ch,'realtime')
	ok = wind_tm_open_channel(ch,stream)
	TYPE*,'RETURN FROM WIND_TM_OPEN_CHANNEL, OK=',OK
	if( ok.ne.1 ) stop 'cannot open tm channel'
	call wind_tm_get_filename(ch,file)
	type *,'file: ',file(:72)
c	type *,'file head ',file(:37)
c	type *,'year ',file(31:34)
c	type *,'month ',file(35:36)
c	type *,'day ',file(37:38)

	write(37,*) 'file: ',file(:72)
c	
C	delt = 15.d00/1440.d00             ! 15 min. interval
	delt = 2.d00/1440.d00              !  2 min. interval
	delt = 0.			   !  do not skip data
	loop_and_gather = 1
	scet = 0.
	call w_channel_position(ch,scet)
	type*,'channel, scet',ch,scet
	if(ndaysdone.eq.0) then
		scetst = 4747.
C		year = file(38:41)
		year = file(31:34)
	        print*,'check year, check=',year
		if(year.eq.'1994') scetst = 4382.d00
		if(year.eq.'1995') scetst = 4747.d00
		if(year.eq.'1996') scetst = 5112.d00
		if(year.eq.'1997') scetst = 5478.d00
		if(year.eq.'1998') scetst = 5843.d00
		if(year.eq.'1999') scetst = 6208.d00
		if(year.eq.'2000') scetst = 6573.d00
		if(year.eq.'2001') scetst = 6939.d00
	endif
	if(scet.eq.0.d00) scet = 4767.d00
	nday = scet
	type*,'position channel pointer to',scet
c	ok = w_channel_position(ch,scet)
c	if ( ok.eq.1 ) then
c		type*,'channel pointer positioned to',scet
c	else
c		 type*,'cannot position pointer'
c	endif
c	
	  iterm = -1
	  iterm = -3
c	  iterm = 3
c
 3001   ok = w_event(ch,'HK')
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

	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	last_major = major-1

	item = 'EVENT_SCET_R8'
C	type*,'going to get first item=',item
C	type*,'channel no',ch
c	ok = w_item_r8(ch, item, scet, 1, ret_size)
C	type*,'return size',ret_size
c	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
c	     ihrmn = 100*hh+mm
c	     TYPE 1005,YYYY,MON,DD,IHRMN
C		write(26,*) YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)

	scet = scet + 6.d00/1440.d00			! advance six 
!						minutes to avoid cal


!	ok = wind_tm_set_messages_off(ch)
!	if (.not. ok) stop 'cannot turn messages off'

	type *, 'Please wait, gathering data and making plots...'

	go to 1010

 1000	   continue

	   if(delt.ne.0.) then
	     scet = scet + delt
	     scet_ret = scet
  	     call w_channel_position(ch,scet_ret)
	   endif
 1001	   ok = w_event(ch,event)
	   item = 'EVENT_SCET'
	   okt = w_item_i4(ch, item, s_scet, 2, ret_size)
 1010	if(ok.eq.82) then		! END OF FILE
		ndaysdone = ndaysdone + 1
		type*,'finished at end of file',file
		type*,'DAYS DONE SO FAR',ndaysdone,' count',count(1)
		ok = wind_tm_close_channel(ch)
c		if(ndaysdone.lt.29) go to 3000
c		if(ndaysdone.lt.2) go to 3000
		if(ndaysdone.lt.1) go to 3000	! get another file
c		
		print*,'plotall and fitres called before ntot.gt.0'
		call plotall(iterm)
	  if(ntot.gt.0) then
		print*,'plotall and fitres called after ntot.gt.0'
		call plotall(iterm)
c		call plotdrive(-1)
	  open(unit=89,file='antres.dat',status='old',access='append')
		write(89,*) ' '
		write(89,*) file(:78)
		itend = s_scet(2)/100
		call fitres(ch,nstart,ntot,itstart,itend)
	 	return
	   endif
	   if(is.ge.sz_x_dim) then
c	   if(is.ge.20) then
		call plotall(iterm)
c		ix = 1
	 	return
	   endif
	   print*,'go to 2000 after is.ge.sz_x_dim'
	   go to 2000
	endif
c
	if (.not. ok) then
		type *, 'apmplot,cannot get event after', scet
		error_count = error_count + 1
		if(error_count.lt.10) go to 1001
		CALL plotall(iterm)
		TYPE*,'TOO MANY ERRORS'
		GO TO 3000
	else
		error_count=0
	endif

c	item = 'CAL_GENERATOR'
	item = 'CAL_ON'
	ok = w_item_i4(ch, item, cal_on, 1, ret_size)
c	type*,'cal_on',cal_on
	if(cal_on.eq.1) go to 1001

	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	if(major.eq.last_major) go to 1000
	last_major = major
c
c	event is ok, process it
c
	   is = MIN0(count(1)+1,sz_x_dim)

c 	    item = 'EVENT_SCET_R8'
c	    ok = w_item_r8(ch, item, scet, 1, ret_size)

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, s_scet, 2, ret_size)
c***********   for routine processing, started 10 May 1997, which stops
c		at 0300
c	   if(s_scet(2).gt.40000) go to 2000
	   if(s_scet(2).gt.235500) go to 2000
c*************
	   if(is.eq.1) type*, 'first sample, scet',
     1		s_scet
c	    print*,'scet',S_scet
c	    call w_ur8_to_string(scet,datestr,timestr)
c	    type*,datestr
c	    type*,timestr
	hh = S_SCET(2)/10000
	min = mod(S_SCET(2),10000)/100
	SS = MOD(S_SCET(2),100)
c	scet = dfloat(nday) + hh/24. + min/1440. + SS/86400.

	   item = 'APC_Z'		! word 74
C	   item = 'FFT_Z_APC_RELAY'		! word 74
	   if(is.gt.0) then
	     ok = w_item_i4(ch, item, z_apc(is), 1, ret_size)
	     if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok
c	     ONOFF(ix) = (.not. Z_APC(IS)) .and. 1
	   endif

c	   xtime(is) = (scet - scetst)
	   xtime(is) = dfloat(nday) + hh/24. + min/1440. + SS/86400.
     1        -scetst


	   item = 'APC_DAC'		! word 70
	   ok = w_item_I4(ch, item, idrive, 1, ret_size)
c	   item = 'APC_DAC_VOLTS_R4'		! word 70
c	   ok = w_item_R4(ch, item, drive(is), 1, ret_size)
	   if ( ok ) then
c		drivev = apccal(apc_dac(is))
		drivev = apccal(idrive)
	   else
		 write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif
	   drive(is) = drivev
c
	   item = 'APM_ANGLE'
	   ok = w_item_i4(ch, item, iangle, 1, ret_size)
c
C	   item = 'X_APC'		! word 69
	   item = 'APC_X'		! word 69
	   ok = w_item_i4(ch, item, x_apc(is), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok

C	   item = 'X_PEAK'		! word 63
	   item = 'APM_X_PEAK'		! word 63
	   ok = w_item_i4(ch, item, x_peak, 1, ret_size)
	   if (ok) then
		is = MIN0(count(1)+1,sz_x_dim)
		count(1) = is
		pk(1,is) = apmcal(1,x_peak)
c		drive(is) = drivev
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

C	   item = 'X_DC'		! word 65
	   item = 'APM_X_DC'		! word 65
	   ok = w_item_i4(ch, item, x_dc, 1, ret_size)
c	   av(1,is) = x_dc
	   if (ok) then
		av(1,is) = apmcal(3,x_dc)
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

C	   item = 'X_RESISTOR'		! word 69
	   item = 'APC_X_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, x_resistor, 1, ret_size)
	   if (ok) then
		irtm = x_resistor
		ir = rassign(irtm)
		resistor(1,is) = ir
c		resistor(3,is) = ir
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

C	   item = 'Y_APC'		! word 69
	   item = 'APC_Y'		! word 69
	   ok = w_item_I4(ch, item, y_apc(is), 1, ret_size)

C	   item = 'Y_PEAK'		! word 64
	   item = 'APM_Y_PEAK'		! word 64
	   ok = w_item_i4(ch, item, y_peak, 1, ret_size)
c	   pk(2,is) = y_peak
	   if (ok) then
		is = MIN0(count(2)+1,sz_x_dim)
		count(2) = is
c		pk(2,is) = apmcal(2,y_peak)
		pk(2,is) = apmcal(2,y_peak)
	   endif

C	   item = 'Y_DC'		! word 66
	   item = 'APM_Y_DC'		! word 66
	   ok = w_item_i4(ch, item, y_dc, 1, ret_size)
c	   av(2,is) = y_dc
	   if (ok) then
		av(2,is) = apmcal(4,y_dc)
	   endif

C	   item = 'Y_RESISTOR'		! word 69
	   item = 'APC_Y_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, y_resistor, 1, ret_size)
	   resistor(2,is) = y_resistor
	   if (ok) then
		irtm = y_resistor
		ir = rassign(irtm)
		resistor(2,is) = ir
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

C	   item = 'Z_DC'		! word 67
	   item = 'APM_Z_DC'		! word 67
	   ok = w_item_i4(ch, item, z_dc, 1, ret_size)
c	   av(3,is) = z_dc
	   if (ok) then
		is = MIN0(count(3)+1,sz_x_dim)
		count(3) = is
		av(3,is) = apmcal(5,z_dc)
c	write(77,*) is,av(3,is)
	   endif

c	   item = 'Z_RESISTOR'		! word 69
	   item = 'APC_Z_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, Z_resistor, 1, ret_size)
c	   resistor(3,is) = z_resistor
	   if (ok) then
		irtm = Z_resistor
		ir = rassign(irtm)
		resistor(3,is) = ir
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif
	   if(s_scet(2).ge.50000.and.s_scet(2).le.210000) then
	        acount(iangle+1) = acount(iangle+1) + 1.
		xascan(iangle+1) = xascan(iangle+1) + av(1,is)
		yascan(iangle+1) = yascan(iangle+1) + av(2,is)
		zascan(iangle+1) = zascan(iangle+1) + av(3,is)
		xpscan(iangle+1) = xpscan(iangle+1) + pk(1,is)
		ypscan(iangle+1) = ypscan(iangle+1) + pk(2,is)
	   endif
c***************
	if(x_apc(is).eq.0) then
	  write(88,1549) is,drivev,av(1,is),pk(1,is)
     1		,av(2,is),pk(2,is),av(3,is)
	  ntot = is
	  itend = s_scet(2)/100
	  if(nstart.eq.0) nstart = is
	  if(istart.eq.0) then
	  	itstart = s_scet(2)/100
	 	istart = 1
	  else
	    ist = is - nstart 
	    outmongo(ist,1) = is
	    outmongo(ist,2) = drivev
	    outmongo(ist,3) = av(1,is)
	    outmongo(ist,4) = pk(1,is)
	    outmongo(ist,5) = av(2,is)
	    outmongo(ist,6) = pk(2,is)
	    outmongo(ist,7) = av(3,is)
	  endif
	endif
 1549	format(i6,6f8.2)

 2001	GO TO 1000
C 	end do
 2000	continue

	type*,'write accumulated samples as function of angle'
	do i = 1,260
	  xascan(i) = xascan(i)/acount(i)
	  yascan(i) = yascan(i)/acount(i)
	  zascan(i) = zascan(i)/acount(i)
	  xpscan(i) = xpscan(i)/acount(i)
	  ypscan(i) = ypscan(i)/acount(i)
	enddo
c
	write(67,*) 'ang   no.   X_DC    Y_DC   Z_DC    X_PK    Y_PK  ',
     1		' XDIFF   YDIFF'
	write(67,*) ' '
	do i = 1,260
c	  ang = (i-1)*360./256.
	  if(i.gt.128) then
	    diffx = xascan(i) - xascan(i-128)
	    diffy = yascan(i) - yascan(i-128)
	  else
	    diffx = xascan(i) - xascan(i+128)
	    diffy = yascan(i) - yascan(i+128)
	  endif
	  if(acount(i).gt..5) write(67,67) i-1,acount(i),xascan(i),
     1	  yascan(i),zascan(i),xpscan(i),ypscan(i),diffx,diffy
	enddo
 67	format(i6,f6.0,7f8.3)
c
	type*,'generate plots at scet=',scet


	   write(37,*) 'last sample, scet',s_scet
c
	if(ntot.gt.0) then
	  call plotall(iterm)
c	  call plotdrive(-1)
	  open(unit=89,file='antres.dat',status='old',access='append')
		write(89,*) ' '
		write(89,*) file(:78)
c
c		itend = s_scet(2)/100
		call fitres(ch,nstart,ntot,itstart,itend)
c
  2	  format(1x,a,a,a,z8.8)
c
	  com(1) = 'printer 3'
	  write(com(2),1002) ntot-nstart
 1002	  format('lines 1 ',i5)
	  com(3) = 'ticksize .2  1. .2  1.'
	  com(4) = 'input xantres.mgo'
	  com(5) = 'limits 0. 1. 0. 1.'
	  com(6) = 'relocate .1 1.05'
c	  write(com(7),1003) file(31:39)
	  write(com(7),1003) file(31:34),file(35:36),file(37:38)
 1003	  format('label date',a5,'/',a2,'/',a2)
	  com(8) = 'hardcopy'
	  com(9) = 'end'
	  call mongo(9,com,200,7,outmongo)
c
	  com(1) = 'printer 1'
	  write(com(2),1002) ntot-nstart
	  com(3) = 'ticksize .2  1. .2  1.'
	  com(4) = 'input yantres.mgo'
	  com(5) = 'limits 0. 1. 0. 1.'
	  com(6) = 'relocate .1 1.05'
	  write(com(7),1003) file(31:34),file(35:36),file(37:38)
	  com(8) = 'hardcopy'
	  com(9) = 'end'
	  call mongo(9,com,200,7,outmongo)
c
	  com(1) = 'printer 3'
	  write(com(2),1002) ntot-nstart
	  com(3) = 'ticksize .2  1. 0.  0.'
	  com(4) = 'input zantres.mgo'
	  com(5) = 'limits 0. 1. 0. 1.'
	  com(6) = 'relocate .1 1.05'
	  write(com(7),1003) file(31:34),file(35:36),file(37:38)
	  com(8) = 'hardcopy'
	  com(9) = 'end'
	  call mongo(9,com,200,7,outmongo)
	endif
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	initialize_mongo_xwindows()
C	implicit	none
	integer*4	iq
	integer*4	i,j,k,n
	parameter	w1_default_title='Z Meas Plot'
	parameter	w1_default_icon='APC'
	parameter	w1_default_width=700
	parameter	w1_default_height=500
	parameter	w1_default_x_pos=300
	parameter	w1_default_y_pos=300

	initialize_mongo_xwindows = 1

        call mgoinit()

        call mgo_x_set_window_visibility_off(1)         ! and set current win
	call mgo_x_foreground_color('white')
	call mgo_x_background_color('black')
!       call mgo_x_foreground_color('green')
!       call mgo_x_background_color('cyan')
        call mgosetup(7)                                ! this creates win #1

	! first window
        call mgo_x_select_window(1)                     ! make win #1 current
        call mgo_x_title(w1_default_title)              ! title bar string
        call mgo_x_icon(w1_default_icon)                ! icon name
        call mgo_x_size(w1_default_width,w1_default_height)
        call mgo_x_position(w1_default_x_pos,w1_default_y_pos)
        call mgo_x_update_window()                      ! make changes stick

        call mgo_x_set_window_visibility_on(1)

	call adjust_gxy_of_mgoxwin()
!	call save_context_1()

	return
	end
	!----------------------------------------------------------------------
	subroutine adjust_gxy_of_mgoxwin()
	a = gy2 - gy1
	b = a * 0.01
	gy2 = gy2 + b
	b = a * 0.08
	gy1 = gy1 + b
	!
	c = gx2 - gx1
	d = c * 0.12
	gx1 = gx1 + d
	d = c * 0.02
	gx2 = gx2 - d
	return

	!----------------------------------------------------------------------
	entry adjust_gxy_of_hardcopy()
	a = gy2 - gy1
!	b = a * 0.01
!	gy2 = gy2 + b
	b = a * 0.10
	gy1 = gy1 - b
	!
!	c = gx2 - gx1
!	d = c * 0.08
!	gx1 = gx1 + d
!	d = c * 0.04
!	gx2 = gx2 - d
	return
	end
	options/extend_source
!------------------------------------------------------------------------------
	REAL*4		function	APMCAL(IANT,NTM)
	implicit	none
!	include		'apc_fit_def.for/nolist'
	real*4 		coeff(5),const(5),csq(5),corrf(5)
	integer*4 	iant,ntm
!
!	iant=1 is X_PK, 2 is Y_PK, 3 is X_DC, 4 is Y_DC, 5 is Z_DC
!
C	the following were used until Nov 1999
C	data coeff /.084848, .09678, .080956, .10424, .06669/
C	data const /13.315,  16.090,  13.069, 16.613, 9.0504/
C	data csq /-8.4573e-6, 2.2302e-5, 2.2793e-6, -9.197e-6, 5.1049e-6/	
!
	data coeff /.087818, .098714, .083789, .10633, .09728/
	data const /13.781,  16.412,  13.526, 16.954, 13.202/
	data csq /-8.7533e-6, 2.2748e-5, 2.3591e-6, -9.381e-6, 7.4466e-6/	
c
	apmcal = coeff(iant)*ntm - const(iant) + csq(iant)*ntm**2

	return
	end
	options/extend_source
!------------------------------------------------------------------------------
	REAL*4		function	APCCAL(NTM)
C	implicit	none
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
	SUBROUTINE   PLOTALL(ITERM)
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
C
	include		'apmplot_def.for'
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	CHARACTER*12 LABEL
	CHARACTER*2 ALABEL(3)
	CHARACTER*80  STR
	REAL*4 XX(sz_x_dim),YY(sz_x_dim)
	REAL*4 X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,GX,GY,CX,CY
	REAL*4 EXPAND,ANGLE,CHEIGHT,CWIDTH,CXDEF,CYDEF,PI
	REAL*4 PSDEF,PYDEF,COFF,TERMOUT,XYSWAPPED,USERVAR,AUTODOT
	REAL*4 YMAX,YMIN,CORRF,EXPSAV,SXTICK,BXTICK
	DATA ALABEL /'EX','EY','EZ'/
	INTEGER*4  ITERM,IRX,IANT,NPT,I,IAVPK,NVEC
	REAL YTOP,YBOT,YRANGE,YINC,XEND,XSTART
	REAL XMIN,XP,YP,A,B
C
C
	print*,'start plotall, iterm=',iterm
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	  IF(ITERM.LT.0) THEN
	    YBOT = 200.
	    XSTART = 500.
c	    YTOP = 3050.
	    YTOP = 3090.
	    XEND = 2200.
	  ENDIF
	YRANGE = YTOP - YBOT
c	YINC = .3*YRANGE
	YINC = .29*YRANGE
	NPT = count(1)
	    IF(NPT.EQ.0) NPT = 1
	TRANGE = XTIME(NPT) - XTIME(1)		! in days
	  BXTICK = 1./12.
	  SXTICK = .166667
	  SXTICK = sxtick/4.
	IF(24.*TRANGE.GT.10.) THEN
	  SXTICK = 1./24.
	  BXTICK = 1./6.
	ENDIF
	IF(24.*TRANGE.GT.20.) THEN
	  SXTICK = 1./24.
	  BXTICK = 1./6.
	ENDIF
	PRINT*,'PLOTALL CALLED, time',xtime(1),xtime(npt)
C
	DO IRX = 1,3
C		print*,'av',irx,(av(irx,j),j=1,4)
C		print*,'pk',irx,(pk(irx,j),j=1,4)
	    CALL MGOSETLOC(XSTART,YTOP-.6*YINC,XEND,YTOP)
	    CALL MGOSETEXPAND(.6)
	    YMAX =  6.
	    YMIN = -6.
		PRINT*,'IRX,COUNT',IRX,COUNT(IRX)
	    NPT = count(IRX)
	    NPT = MIN0(NPT,SZ_X_DIM)
	    IF(NPT.EQ.0) NPT = 1
	    XMIN = XTIME(1) - .01*TRANGE
	    XMAX = XTIME(NPT) + .01*TRANGE
	    CALL MGOTICKSIZE(SXTICK,BXTICK,0.,0.)
	    CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)
	    DO I = 1,NPT
	        YY(I) = pk(IRX,I)
		XX(I) = XTIME(I)
	write(39,*) i,irx,xtime(i),av(irx,i),pk(irx,i)
	    ENDDO	
	print*,'call mgoconnect,iterm,npt=',iterm,npt   
	    IF(IRX.NE.3) CALL MGOCONNECT(XX,YY,NPT)
	    DO I = 1,NPT
	       	   YY(I) = av(IRX,I)
	    ENDDO	   
	    CALL MGOCONNECT(XX,YY,NPT)
 704	    FORMAT(2X,A2,2x,A4,F10.0,5E12.3)
C
	print*,'got to 704,irx=',irx
	    CALL MGOYLABEL(2,ALABEL(IRX))
c	      call mgopoints(43.5,1,xx,yy,npt)		! points are 
	    CALL MGOTICKSIZE(SXTICK,BXTICK,0.,0.)
	    CALL MGOBOX(0,2)
	    if(irx.eq.1)  THEN
		CALL MGOPLOTID('[KELLOGG.WIND]','APMPLOT')
	        PX = .5*(GX1+GX2)
	        CALL MGOGRELOCATE(PX,GY2)
	        CALL MGOPUTLABEL(36,'WIND-WAVES ANTENNA POTENTIAL MONITOR'
     1		,2)
	    ENDIF
C
C	MAKE A PLOT OF RESISTOR
C
	print*,'got to make a plot of resistor'
	    CALL MGOSETLOC(XSTART,YTOP-.8*YINC,XEND,YTOP-.65*YINC)
	    CALL MGOSETLIM(XMIN,.5,XMAX,4.5)
	    CALL MGOYLABEL(3,'RES')
	    CALL MGOTICKSIZE(SXTICK,BXTICK,1.,4.)
C		print*,'res',irx,(resistor(irx,j),j=1,4)
	    DO I = 1,NPT
	       	   YY(I) = RESISTOR(IRX,I)
	    ENDDO	   
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOBOX(0,2)
C
C	PLOT ON-OFF
	print*,'got to plot on-off'
C
	    CALL MGOSETLOC(XSTART,YTOP-.9*YINC,XEND,YTOP-.85*YINC)
	    CALL MGOSETLIM(XMIN,-.5,XMAX,1.5)
	    CALL MGOTICKSIZE(SXTICK,BXTICK,1.,4.)
	    IF(IRX.EQ.1) THEN
	       DO I = 1,NPT
	       	   YY(I) = X_APC(I)
	       ENDDO	   
	    ENDIF
	    IF(IRX.EQ.2) THEN
	       DO I = 1,NPT
	       	   YY(I) = Y_APC(I)
	       ENDDO	   
	    ENDIF
	    IF(IRX.EQ.3) THEN
	       DO I = 1,NPT
	       	   YY(I) = Z_APC(I)
	       ENDDO	   
	    ENDIF
	    CALL MGOCONNECT(XX,YY,NPT)
	    CALL MGOSETEXPAND(.4)
	    CALL MGORELOCATE(XMIN,1.)
	    CALL MGOPUTLABEL(4,'OFF ',4)
	    CALL MGORELOCATE(XMIN,0.)
	    CALL MGOPUTLABEL(3,'ON ',4)
	    CALL MGOSETEXPAND(.6)
	    CALL MGOBOX(1,0)
C
	print*,'got to ytop'
	YTOP = YTOP-YINC
C
	ENDDO
C
C	PLOT DRIVE VOLTAGE
C
c	    CALL MGOSETLOC(XSTART,YTOP-.15*YINC,XEND,YTOP)
	    CALL MGOSETLOC(XSTART,YTOP-.19*YINC,XEND,YTOP)
	    CALL MGOSETLIM(XMIN,-10.5,XMAX,10.5)
	    CALL MGOTICKSIZE(SXTICK,BXTICK,5.,10.)
	    CALL MGOCONNECT(XX,DRIVE,NPT)
	    CALL MGOYLABEL(5,'DRIVE')
	    CALL MGOBOX(1,2)
	    WRITE(LABEL,701) FILE(31:34)
 701	    FORMAT(' DAY OF ',A4)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOXLABEL(12,LABEL)
	    CALL MGOSETEXPAND(.6)
C
C
	  EXPSAV = EXPAND
C	  IF(ITERM.LT.0) THEN
C	    CALL MGOSETLOC(500.,300.,3100.,2000.)
C	  ENDIF
	  GYRANGE = GY2-GY1
	type*,'gyrange,GY1,GY2',gyrange,GY1,GY2
	  CALL MGOGRELOCATE(GX2,GY1-.8*GYRANGE)
	  CALL MGOSETEXPAND(.4)
	  CALL MGOPUTLABEL(43,FILE,1)
C	  CALL MGOSETEXPAND(EXPSAV)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
	RETURN
	END
	integer*4	function	rassign(i)
	integer*4 i,ir,irtable(8)
	data irtable /1,3,1,4,2,3,2,4/

	  ir = i.and.7
	  rassign = irtable(i+1)

	return
	end
	SUBROUTINE   PLOTDRIVE(ITERM)
C
	include		'apmplot_def.for'
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
C
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	CHARACTER*12 LABEL
	CHARACTER*2 ALABEL(3)
	CHARACTER*80  STR
	REAL*4 XX(sz_x_dim),YY(sz_x_dim)
	REAL*4 X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,GX,GY,CX,CY
	REAL*4 EXPAND,ANGLE,CHEIGHT,CWIDTH,CXDEF,CYDEF,PI
C
	REAL*4 PSDEF,PYDEF,COFF,TERMOUT,XYSWAPPED,USERVAR,AUTODOT
	REAL*4 YMAX,YMIN,CORRF,EXPSAV,SXTICK,BXTICK
	DATA ALABEL /'EX','EY','EZ'/
	INTEGER*4  ITERM,IRX,IANT,NPT,I,IAVPK,NVEC
	REAL YTOP,YBOT,YRANGE,YINC,XEND,XSTART
	REAL XMIN,XP,YP,A,B
C
C
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	  IF(ITERM.LT.0) THEN
	    YBOT = 200.
	    XSTART = 500.
c	    YTOP = 3050.
	    YTOP = 3090.
	    XEND = 2200.
	  ENDIF
	YRANGE = YTOP - YBOT
c	YINC = .3*YRANGE
	YINC = .5*YRANGE
	NPT = count(1)
	    IF(NPT.EQ.0) NPT = 1
	TRANGE = XTIME(NPT) - XTIME(1)
	  BXTICK = 1.
	  SXTICK = .166667
	  SXTICK = sxtick/4.
	IF(TRANGE.GT.10.) THEN
	  SXTICK = 1.
	  BXTICK = 2.
	ENDIF
	IF(TRANGE.GT.20.) THEN
	  SXTICK = 1.
	  BXTICK = 5.
	ENDIF
	PRINT*,'PLOTDRIVE CALLED, time',xtime(1),xtime(npt)
	PRINT*,'PLOTDRIVE CALLED, drive',drive(1),drive(npt)
C
C	PLOT DRIVE VOLTAGE
C
c	    CALL MGOSETLOC(XSTART,YTOP-.15*YINC,XEND,YTOP)
	    CALL MGOSETLOC(XSTART,YTOP - YINC,XEND,YTOP)
	    XMIN = XTIME(1) - .01*TRANGE
	    XMAX = XTIME(NPT) + .01*TRANGE
	    CALL MGOSETLIM(XMIN,-10.5,XMAX,10.5)
	    CALL MGOTICKSIZE(SXTICK,BXTICK,1.,5.)
	    CALL MGOCONNECT(XTIME,DRIVE,NPT)
	    CALL MGOYLABEL(5,'DRIVE')
	    CALL MGOBOX(1,2)
	    WRITE(LABEL,701) FILE(31:34)
 701	    FORMAT(' DAY OF ',A4)
	    CALL MGOSETEXPAND(.7)
	    CALL MGOXLABEL(12,LABEL)
	    CALL MGOSETEXPAND(.6)
C
C
	  EXPSAV = EXPAND
C	  IF(ITERM.LT.0) THEN
C	    CALL MGOSETLOC(500.,300.,3100.,2000.)
C	  ENDIF
	  GYRANGE = GY2-GY1
	type*,'gyrange,GY1,GY2',gyrange,GY1,GY2
	  CALL MGOGRELOCATE(GX2,GY1-.2*GYRANGE)
	  CALL MGOSETEXPAND(.4)
	  CALL MGOPUTLABEL(45,FILE,1)
C	  CALL MGOSETEXPAND(EXPSAV)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
	RETURN
	END
	subroutine fitres(ch,nstart,ntot,itstart,itend)

c	on 21 Jan 2000, I altered this to only fit data from drive
c	voltages greater than drive_lim, because I felt that small
c	drive voltages give the resistance that I want.  This
c	wasnt quite right either -- it's small voltages on the
c	antenna that I want.  On July 14 2000 I changed it to limit
c	only for the shadow case.  drive_.lim = 2. was used for
c	the paper in July 2000

c
	include		'apmplot_def.for'
	real*4		wt(1000),yy(1000),xx(1000)
	integer*4	ch
c	character*80 file
	data wt /1000*1./
	data drive_lim /-6./
c
	call wind_tm_get_filename(ch,file)
c		write(89,*) file(:78)
	print*,'fitres',nstart,ntot
	if(nstart.eq.0.or.ntot.eq.0) return
	nl = 0
	do n = nstart,ntot
	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		xx(nl) = av(1,n)
		yy(nl) = drive(n)	  
c		print*,'n,drive,av',n,drive(n),av(1,n)
		write(44,*) n,drive(n),av(1,n)
	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A.ne.1.) res = 35.4/(A-1.)
		write(89,*) 'x av',A,B,res,RMS
		rxsun = res
c
	nl = 0
	do n = nstart,ntot
	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		xx(nl) = pk(1,n)
		yy(nl) = drive(n)	  
	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A.ne.1.) res = 35.4/(A-1.)
		write(89,*) 'x pk',A,B,res,RMS
		print*, 'x pk',A,B,res,RMS
		rxshd = res
c
	nl = 0
	do n = nstart,ntot
	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		xx(nl) = av(2,n)
		yy(nl) = drive(n)	  
	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A-1..ne.0.) res = 124.8/(A-1.)
		write(89,*) 'y av',A,B,res,RMS
		rysun = res
c
	nl = 0
	do n = nstart,ntot
	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		xx(nl) = pk(2,n)
		yy(nl) = drive(n)	  
	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A-1..ne.0.) res = 124.8/(A-1.)
		write(89,*) 'y pk',A,B,res,RMS
		ryshd = res
c
	nl = 0
	nfract = ntot - .14*(ntot-nstart+1) 
	print*,'for z, do points ',nfract,'  to end=',ntot
	do n = nstart,ntot
	     if(n.gt.nfract.or.drive(n).lt.-5.) then
		nl = nl+1
		xx(nl) = av(3,n)
		yy(nl) = drive(n)	
	     endif  
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A-1..ne.0.) res = 68./(A-1.)
		write(89,*) 'z av',A,B,res,RMS
		rz = res
c
	open(unit=90,file='accrres.dat',type='old',access='append')
	dens = 0.
	etemp = 0.
	eflux = 0.
	write(90,1001) file(31:38),itstart,itend,rxsun,rxshd,
     1		rysun,ryshd,rz,dens,etemp,eflux
 1001	format(A9,2i5,8f8.2)
	close(unit=90)
	return
	end
