
	program		apc_fit
! apc_fit.for - a wind/waves application
!
! to build:
!	$ fortran apc_fit
!	$ link apc_fit,wind_lib/lib,mongo
!
!							       X,Y  Z
!	HiD              10.6 Meg      	50 Meg                  A
!	Avr		  100 Meg      500 Meg			B
!	LoD		  350 Meg     1500 Meg			C
!	 Z	  	 1000 Meg    10000 Meg	    50 Meg      D   A

! There are also fixed resistors in the preamp circuit, of total resistance
! 24.8 M for X, 150 M for Y, and 50 M for Z, leading to totals:

!	name		     X           Y           Z         logical
!							       X,Y  Z
!	HiD              35.4 Meg      200 Meg                  A
!	Avr		124.8 Meg      650 Meg			B
!	LoD		  375 Meg     1650 Meg			C
!	 Z	  	 1025 Meg    10150 Meg	   100 Meg      D   A


!	options/extend_source
!	implicit	integer*4 (a-z)
	include		'apc_fit_def.for'
	integer*4	ok,get_output_device,loop_and_gather

	data resis/ 1.e9, 34.8, 124.8,  375.,  1025.,
	1	    1.e9, 200.,  650., 1650., 10150.,
	1	    1.e9, 100.,  100.,  100.,   100./

	ok = 1
	if (ok) ok = get_output_device()
	TYPE*,'RETURN FROM GET_OUTPUT_DEVICE,OK=',OK
	if (ok) ok = loop_and_gather()
	TYPE*,'RETURN FROM LOOP_AND_GATHER'
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_output_device()
	implicit	none
	include		'apc_fit_def.for'
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
	integer*4	function	loop_and_gather()
	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	include		'apc_fit_def.for'
	integer*4	ok
	integer*4	ch, major, minor
	parameter	event='HK  '
	integer*4	ret_size
	integer*4	s_scet(2)
	character*16	item
	integer*4	error_count,istart,ilast,hh,min,nday
	integer*4	ix,ihrmn,yyyy,mon,dd,mm,ss,ms
	integer*4	first_ok
	integer*4	first_major,last_major
	integer*4	iv		! drive telemetry number
	integer*4	ir,irtm		! resistor index
	integer*4	ia		! antenna index, 1=x,2=y,3=z
	integer*4	is		! sample number
	integer*4	IONOFF		! DAC ON OR OFF
	character*14	timestr,datestr
	real*4 		apccal,apmcal,drivev
	real*8		scet,stoptime

!	ok = wind_tm_open_channel(ch,'realtime')
	ok = wind_tm_open_channel(ch,'offline')
		TYPE*,'RETURN FROM WIND_TM_OPEN_CHANNEL, OK=',OK
	if (.not. ok) stop 'cannot open tm channel'
c	
	scet = 0.
	call w_channel_position(ch,scet)
	type*,' scet',scet
	nday = scet
	TYPE*,'type start time, e.g. 0314, (approximate)'
	read(5,*) istart
	hh = istart/100
	min = mod(istart,100)
	scet = dfloat(nday) + hh/24. + min/1440.
c	hh = ilast/100
c	min = mod(ilast,100)
c	stoptime = dfloat(nday) + hh/24. + min/1440.
	type*,'position channel pointer to',scet
	ok = w_channel_position(ch,scet)
	if (.not. ok) type*,'cannot position pointer'
	type*,'channel pointer positioned to',scet
	

 	TYPE*,'type first and last major frames to be processed'
!	read(5,'(q,a)',iostat=ios,err=10) i, c
	read(5,*) first_major,last_major

	   dowhile(major.lt.first_major)
c             ok = wind_tm_get_next_event(ch,major,minor,'HK')
             ok = w_event(ch,'HK')
	ok = wind_tm_get_mfmf(ch,major,minor)
c	     item = 'EVENT_SCET_R8'
c	     ok = w_item_r8(ch, item, scet, 2, ret_size)
c	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
c	     ihrmn = 100*hh+mm
c	     TYPE 1005,YYYY,MON,DD,IHRMN
C		write(26,*) YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)

	   item = 'FFT_Z_APC_RELAY'		! word 74
	   ok = wind_tm_get_item(ch, item, z_apc(1), 1, ret_size)
	   IONOFF = (.not. Z_APC(1)) .and. 1
	     type*,'major',major,ionoff
	  enddo


	call wind_tm_get_filename(ch,file)
	type *,'file: ',file(:72)
	write(37,*) 'file: ',file(:72)

!	ok = wind_tm_set_messages_off(ch)
!	if (.not. ok) stop 'cannot turn messages off'

	ok = wind_tm_get_mfmf(ch,major,minor)
c	if (.not. ok) stop 'cannot get stream position'
	if (.not. ok) type*, 'cannot get stream major,minor'
	if ( ok ) then
	  type*,'start at major frame',major
	else
	  stop 'cannot get stream position'
	endif

	if (wind_tm_realtime(ch)) then
	   major = major - 1
	else
	   major = major + 1
	end if

	type *, 'Please wait, gathering data and making plots...'

	first_ok = 1
	do while(.not. wind_tm_eof(ch,major,minor))
 1000	   continue
	   ok = w_event(ch,event)
	ok = wind_tm_get_mfmf(ch,major,minor)
	   if (ok) then
	      if (first_ok) then
	         first_ok = 0
	         major1 = major
!	         last_major = major + sz_x_dim - 1
	      end if
	      major2 = major
!	      type *, 'got HK event at major', major
	   else if (ok .eq. w_end_of_file) then
	      type *, 'End of file detected.'
	   else if (.not. ok) then
!	      error_count = error_count + 1
	      type *, 'cannot get event at MF', major, ', incrementing MF...'
	      major = major + 1
	      if (error_count .lt. 255) goto 1000
!	      stop 'STOP, too many errors.'
	   end if

	   if (major .gt. last_major) then
	      ! generate new plot
	      first_ok = 1
	      major2 = major2 - 1
	      call generate_plots()
	      major1 = major
	      major2 = major
	      ix = 1
	   else
	      ix = major - major1 + 1
	   end if

	   if(ix.ge.sz_x_dim) then
		call generate_plots
		ix = 1
	   endif

	   if (ok .eq. w_end_of_file) goto 2000

	   IF(MAJOR.LT.FIRST_MAJOR) GOTO 2001
	   IF(MAJOR.GT.LAST_MAJOR)  GOTO 2000

c	    print*,'scet',scet
c 	    call w_channel_position(ch,scet)
c	    call w_ur8_to_string(scet,datestr,timestr)
c	    type*,datestr
c	    type*,timestr


	   item = 'EVENT_SCET'
	   ok = wind_tm_get_item(ch, item, s_scet, 2, ret_size)
	   if(ix.eq.1) write(37,*) 'first sample, frame,scet',
     1		first_major,s_scet

	   item = 'FFT_Z_APC_RELAY'		! word 74
	   ok = wind_tm_get_item(ch, item, z_apc(ix), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok
	   IONOFF = (.not. Z_APC(IX)) .and. 1
	type*,'gathering, ionoff=',ionoff
	   IF(IONOFF.EQ	.0) GOTO 2001 


	   item = 'APC_DAC'		! word 70
	   ok = wind_tm_get_item(ch, item, apc_dac(ix), 1, ret_size)
	   if ( ok ) then
		drivev = apccal(apc_dac(ix))
	   else
		 write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'X_RESISTOR'		! word 69
	   ok = wind_tm_get_item(ch, item, x_resistor(ix), 1, ret_size)
	   if (ok) then
		irtm = x_resistor(ix)
		ir = rassign(irtm)
	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'X_APC'		! word 69
	   ok = wind_tm_get_item(ch, item, x_apc(ix), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok

	   item = 'X_PEAK'		! word 63
	   ok = wind_tm_get_item(ch, item, x_peak(ix), 1, ret_size)
	   if (ok) then
		is = MIN0(count(ir,1)+1,40)
		count(ir,1) = is
		pk(ir,1,is) = apmcal(1,x_peak(ix))
		drive(ir,1,is) = drivev
	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'X_DC'		! word 65
	   ok = wind_tm_get_item(ch, item, x_dc(ix), 1, ret_size)
	   if (ok) then
		av(ir,1,is) = apmcal(3,x_dc(ix))
	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'Y_RESISTOR'		! word 69
	   ok = wind_tm_get_item(ch, item, y_resistor(ix), 1, ret_size)
	   if (ok) then
		irtm = y_resistor(ix)
		ir = rassign(irtm)
	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'Y_APC'		! word 69
	   ok = wind_tm_get_item(ch, item, y_apc(ix), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok

	   item = 'Y_PEAK'		! word 64
	   ok = wind_tm_get_item(ch, item, y_peak(ix), 1, ret_size)
	   if (ok) then
		is = MIN0(count(ir,2)+1,40)
		count(ir,2) = is
		pk(ir,2,is) = apmcal(2,y_peak(ix))
		drive(ir,2,is) = drivev
	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'Y_DC'		! word 66
	   ok = wind_tm_get_item(ch, item, y_dc(ix), 1, ret_size)
	   if (ok) then
		av(ir,2,is) = apmcal(4,y_dc(ix))

	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'Z_DC'		! word 67
	   ok = wind_tm_get_item(ch, item, z_dc(ix), 1, ret_size)
	   if (ok) then
		is = MIN0(count(ir,3)+1,40)
		count(ir,3) = is
		av(ir,3,is) = apmcal(5,z_dc(ix))
		drive(ir,3,is) = drivev
	   else
		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

 2001	continue
	   major = major + 1
 	end do
 2000	continue

	type*,'generate plots at major=',major


	   write(37,*) 'last sample, frame,scet',
     1		last_major,s_scet

	if (.not. first_ok) then
	   call generate_plots()
	end if

  2	format(1x,a,a,a,z8.8)
	end

!------------------------------------------------------------------------------
	integer*4	function	generate_plots()
	implicit	none
!	include		'apc_fit_def.for/nolist'
	include		'apc_fit_def.for'
	integer*4	plot_number

  2	format(1x,'Generating plot number ',i3,' for ', a1)

	plot_number = plot_number + 1

	type 2, plot_number, 'X'
	call mk_plot('X', x_peak, x_dc, x_resistor, x_apc)
	if (terminal_output) pause

	type 2, plot_number, 'Y'
	call mk_plot('Y', y_peak, y_dc, y_resistor, y_apc)
	if (terminal_output) pause

	type 2, plot_number, 'Z'
	call mk_plot('Z', x_peak, z_dc, x_resistor, z_apc)
	if (terminal_output) pause
	
	open(unit=8,file='zmeas.results',status='new')
	write(8,*) FILE
	write(8,*) ' '
        write(8,*),'             R ser       A           B        RMS ERR
     1R    R ant, Meg    V float'
	write(8,*),' '
	close(unit=8)

	call  plotv(1,-2)
	call  plotv(2,-2)
	call  plotv(3,-2)

	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	mk_plot(type,ay,by,cy,dy)
	implicit	none
!	include		'apc_fit_def.for/nolist'
	include		'apc_fit_def.for'
	character*1	type
	integer*4	ay(*)
	integer*4	by(*)
	integer*4	cy(*)
	integer*4	dy(*)
	real*4		a,b,c,d, r
	integer*4	i,j,k,m,n
	real*4		fmaj1, fmaj2
	integer*4	k2len
	character*12	label1, label2, label3, label4

	label1 = type//'_PEAK'
	label2 = type//'_DC'
	label3 = type//'_RESISTOR'
	label4 = type//'_APC'

	if (terminal_output) then
	   call mgo_x_select_window(1)
	   call mgoerase()
	else
	   call mgosetup(-6)
!	   call mgosetup(3)
	   call mgoprntinit()
	end if

	fmaj1 = float(major1)
	a = fmaj1 - 10.0				! ll x
	b = -10.0					! ll y
	c = fmaj1 + float(sz_x_dim) - 1.0 + 10.0	! ur x
	d = 260.0					! ur y

	n = major2 - major1 + 1
	r = fmaj1
	do j=1,n
	   x(j) = r
	   r = r + 1.0
	end do

	call initplt()

	! plot the peak
	if (type .eq. 'Z') goto 2000
	call setplt2(0.70,1.0)
	do j=1,n
	   y(j) = float(ay(j))
	end do
	! plot the data
	call mgoticksize(2.0, 20.0, 10.0, 50.0)
	call mgosetlim(a,b,c,d)
	call mgosetexpand(0.60)
	call mgobox(0,2)
	call mgosetexpand(01.10)
 	call mgopoints(43.5,1,x,y,n)		! points are 
	call mgoconnect(x,y,n)

	! now the labels
        call mgosetlim(0.0, 0.0, 1.0, 1.0)
	call mgosetexpand(0.990)
	call mgorelocate(-0.10, 0.90)
	k = k2len(label1)
	call mgoputlabel(k,label1,5)
	!=====================================

 2000	continue
	! plot the average
	call setplt2(0.40,0.70)
	do j=1,n
	   y(j) = float(by(j))
	end do
	! plot the data
	call mgoticksize(2.0, 20.0, 10.0, 50.0)
	b = -10.0				! ll y
	d = 260.0				! ur y
	call mgosetlim(a,b,c,d)
	call mgosetexpand(0.60)
	call mgobox(0,2)
	call mgosetexpand(01.10)
 	call mgopoints(43.5,1,x,y,n)		! points are
	call mgoconnect(x,y,n)

	! now the labels
        call mgosetlim(0.0, 0.0, 1.0, 1.0)
	call mgosetexpand(0.990)
	call mgorelocate(-0.10, 0.90)
	k = k2len(label2)
	call mgoputlabel(k,label2,5)
	!=====================================

	! now plot the apc dac
	call setplt2(0.25,0.40)
	do j=1,n
	   y(j) = float(apc_dac(j))
	end do
	call mgoticksize(40.0, 40.0, 250.0, 250.0)
	b = -10.0				! ll y
	d = 280.0				! ur y
	call mgosetlim(a,b,c,d)
	call mgosetexpand(0.60)
	call mgobox(0,0)
	call mgosetexpand(01.10)
	call mgoconnect(x,y,n)
	! draw a dashed line
	call mgosetltype(2)			! 2=short dash
	call mgorelocate(a,127.0)
	call mgodraw(c,127.0)
	call mgosetltype(0)			! 0=solid
	! now the labels
	call mgosetexpand(0.50)
	call mgosetlim(0.0,b,1.0,d)
	call mgorelocate(-0.03, 0.0)
	call mgoputlabel(4,'-10V',8)
	call mgorelocate(-0.03, 255.0)
	call mgoputlabel(4,'+10V',2)
	!
	call mgosetexpand(01.10)
        call mgosetlim(0.0, 0.0, 1.0, 1.0)
	call mgorelocate(-0.10, 0.60)
	call mgoputlabel(7,'APC DAC',5)

	if (type .eq. 'Z') goto 5000
	!=====================================

	! now plot the ?_resistor
	call setplt2(0.10,0.25)
	do j=1,n
	   i = cy(j)
	   if (i.eq.2) then
	      i = 1
	   else if (i.eq.6) then
	      i = 2
	   else if (i.eq.5) then
	      i = 3
	   else if (i.eq.7) then
	      i = 4
	   else
	      i = 0
	   end if
	   y(j) = float(i)
	end do
	call mgoticksize(40.0, 40.0, 1.0, 1.0)
	b = 0.0					! ll y
	d = 5.0					! ur y
	call mgosetlim(a,b,c,d)
	call mgosetexpand(0.50)
	call mgobox(0,2)
	call mgoconnect(x,y,n)
	! now the label
	call mgosetexpand(0.9910)
        call mgosetlim(0.0, 0.0, 1.0, 1.0)
	call mgorelocate(-0.10, 0.60)
	k = k2len(label3)
	call mgoputlabel(k,label3,5)
	!=====================================

 5000	continue
	! now plot the ?_apc bit
	call setplt2(0.00,0.10)
	do j=1,n
	   i = (.not. dy(j)) .and. 1
	   y(j) = float(i)
	end do
	call mgoticksize(2.0, 20.0, 1.0, 1.0)
	b = -1.0				! ll y
	d = 2.0					! ur y
	call mgosetlim(a,b,c,d)
	call mgosetexpand(0.60)
	call mgobox(1,0)
	call mgoconnect(x,y,n)
	! now the labels
	call mgosetexpand(0.50)
	call mgosetlim(0.0,b,1.0,d)
	call mgorelocate(-0.02, 0.0)
	call mgoputlabel(3,'OFF',5)
	call mgorelocate(-0.02, 1.0)
	call mgoputlabel(2,'ON',5)
	!
	call mgosetexpand(0.991)
        call mgosetlim(0.0, 0.0, 1.0, 1.0)
	call mgorelocate(-0.10, 0.60)
	k = k2len(label4)
	call mgoputlabel(k,label4,5)
	!=====================================


	! put the file name on the plot
	call setplt2(0.00,1.00)
        call mgosetlim(0.0, 0.0, 1.0, 1.0)
	call mgorelocate(0.0, -0.08)
	k = k2len(file)
	call mgosetexpand(0.8)
	call mgoputlabel(k,file,3)

	call mgoplotid('[kellogg.wind]','apc_fit')

	call pltreset()

	if (terminal_output) then
           call mgotidle()
	else
	   call mgoprntplot(i)
!	   call restore_context_1()
	end if

	mk_plot = 1
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	mymgoplt()
	implicit	none
	include		'mn_programs:mongopar_def.for/nolist'
!	include		'apc_fit_def.for/nolist'
	include		'apc_fit_def.for'
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	integer*4	setplt
	integer*4	setplt2
	integer*4	initplt
	integer*4	pltreset
	integer*4	select_panel
	integer*4	show_gcoord
	integer*4	adjust_gxy_of_mgoxwin
	integer*4	adjust_gxy_of_hardcopy
	integer*4	adjust_gxy_of_hc_panel
	integer*4	i_p			! which panel
	integer*4	n_p			! total number of panels
	integer*4	stripe, n_stripes
	real*4		ytopcoef,ybotcoef
	real*4		a,b,c,d
	real*4		sgx1, sgx2
	real*4		scale(256)
	integer*4	i,j,k
	real*4		y1pct, y2pct		! entry arguments

	!----------------------------------------------------------------------
	entry adjust_gxy_of_mgoxwin()
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

	!----------------------------------------------------------------------
	entry	adjust_gxy_of_hc_panel()
	gx1 = gx1 + 2
	gy1 = gy1 + 2
	return

	!----------------------------------------------------------------------
	entry show_gcoord()
	type *, 'gx1,gx2:', gx1,gx2, '   gy1,gy2:', gy1,gy2
	return

	!----------------------------------------------------------------------
	entry setplt(ytopcoef,ybotcoef)
	uservar(4) = uservar(3) * ybotcoef
	uservar(9) = uservar(3) * ytopcoef
	gy1     = uservar(4) - uservar(9)
	gy2     = uservar(4) + uservar(9)
	
	uservar(8) = uservar(7) * 0.06
	gx1     = uservar(8) + uservar(5)
	uservar(8) = uservar(7) * 0.86
	gx2     = uservar(8) + uservar(5)
	sgx1 = gx1
	sgx2 = gx2
	return

	!----------------------------------------------------------------------
	entry setplt2(y1pct,y2pct)
	gy1 = uservar(1) + (y1pct*uservar(3))
	gy2 = uservar(1) + (y2pct*uservar(3))

	sgx1 = gx1
	sgx2 = gx2
	return

	!----------------------------------------------------------------------
	entry initplt
	uservar(1) = gy1
	uservar(2) = gy2
	uservar(3) = gy2 - gy1
	uservar(5) = gx1
	uservar(6) = gx2
	uservar(7) = gx2 - gx1
	return

	!----------------------------------------------------------------------
	entry pltreset
	gy1 = uservar(1)
	gy2 = uservar(2)
	gx1 = uservar(5)
	gx2 = uservar(6)
	return

	!----------------------------------------------------------------------
	entry select_panel(i_p,n_p)
	c = uservar(3)			! gy2-gy1
	d = c/float(n_p)		! get height of one panel

	gy2 = uservar(2)-(float(i_p-1)*d)	! top of panel
	gy1 = gy2 - d			! bottom of pannel

!	gy1 = uservar(1)+(float(i_p-1)*d)	! top of panel
!	gy2 = gy1 + d			! bottom of pannel

!	gy2 = uservar(1)+(float(i_p)*d)	! top of panel
!	gy1 = gy2 - d			! bottom of pannel

	d = gy2 - gy1
	a = d * 0.12			! bottom margin
	b = d * 0.18			! top margin
	gy2 = gy2 - b			! apply top margin
	gy1 = gy1 + a			! apply bottom margin
	return

	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	initialize_mongo_xwindows()
	implicit	none
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
c	data coeff /.084848, .09678, .080956, .10424, .06669/
c	data const /13.315,  16.090,  13.069, 16.613, 9.0504/
c	data csq /-8.4573e-6, 2.2302e-5, 2.2793e-6, -9.197e-6, 5.1049e-6/	
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
	INTEGER*4   FUNCTION   PLOTV(IANT,ITERM)
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
	CHARACTER*10 LABEL(3)
	CHARACTER*2 ALABEL(3)
	CHARACTER*4 APLABEL(2)
	CHARACTER*80  STR
	REAL*4 XX(50),YY(50),WTFIT(50)
	include		'apc_fit_def.for'
	DATA LABEL /'X ANTENNA','Y ANTENNA','Z ANTENNA'/
	DATA APLABEL /'PEAK','D.C.'/
	DATA ALABEL /'EX','EY','EZ'/
C
C
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,300.,3100.,1900.)
	  ENDIF
	PRINT*,'PLOTV CALLED,iant=',iant
	open(unit=8,file='zmeas.results',type='old',access='append')
C
	NX = 4
	NY = 2
	DO IAVPK = 1,NY
	  DO IR = 1,NX
	    CALL MGOSETEXPAND(1.)
	    IW4 = IR + NX*(IAVPK-1)
	    YMAX =  10.
	    YMIN = -10.
	    NPT = COUNT(IR,IANT)
	    DO I = 1,NPT
		IF(IAVPK.EQ.1) THEN
	       	   YY(I) = PK(IR,IANT,I)
		ELSE
	       	   YY(I) = AV(IR,IANT,I)
		ENDIF
		XX(I) = DRIVE(IR,IANT,I)
		WTFIT(I) = 1.
	    ENDDO	   
C
C	MAKE LEAST SQUARES FIT  YY (= POTENTIAL) = A + B*DRIVE
C
	    CALL LSFIT(YY,XX,WTFIT,NPT,A,B,RMS)
 704	    FORMAT(2X,A2,2x,A4,F10.0,5E12.3)
	    RANT = RESIS(IR+1,IANT)*A/(1.-A)
	    VFLOAT = B/(1.-A)
	    WRITE(8,704) ALABEL(IANT),APLABEL(IAVPK),
     1 		 RESIS(IR+1,IANT),A,B,RMS,RANT,VFLOAT
C
	    XMIN = -5.
	    CALL MGOSETLIM(XMIN,YMIN,11.,YMAX)
	    CALL MGOWINDOW(NX,NY,IW4)
	    IF(IAVPK.EQ.1) THEN
	        CALL MGORELOCATE(4.,-12.)
		WRITE(STR,702) RESIS(IR+1,IANT)
	        CALL MGOPUTLABEL(21,STR,5)
 702		FORMAT('\\tAPC,',F8.0'  Mohm')
	    ENDIF
	      IF(IR.EQ.1) THEN
		IF(IAVPK.EQ.1) THEN
	       	   CALL MGOYLABEL(8,'APM,PEAK')
		ELSE
	       	   CALL MGOYLABEL(8,'APM,D.C.')
		ENDIF
	      ENDIF
	    IF(NPT.NE.0) THEN
	      call mgopoints(43.5,1,xx,yy,npt)		! points are 
	      XP = XMIN
	      YP = A*XP+ B
	      CALL MGORELOCATE(XP,YP)
	      XP = 10.
	      YP = A*XP+ B
	      CALL MGODRAW(XP,YP)
	      DO JJ = 1,NPT
		CURR = (XX(JJ) - YY(JJ))/RESIS(IR+1,IANT)
	        WRITE(37,*) XX(JJ),YY(JJ),CURR
	      ENDDO
	    ENDIF
	    CALL MGOTICKSIZE(1.,2.,0.,0.)
	    CALL MGOBOX(1,2)
	  ENDDO
	ENDDO
C
	  EXPSAV = EXPAND
	  IF(ITERM.LT.0) THEN
	    CALL MGOSETLOC(500.,300.,3100.,2000.)
	  ENDIF
	  CALL MGOGRELOCATE(GX1,GY2)
	  CALL MGOPUTLABEL(45,FILE,9)
	  CALL MGOSETEXPAND(2.*EXPSAV)
	  CALL MGOPLOTID(LABEL(IANT),'  [WIND Z MEAS]')
	  CALL MGOSETEXPAND(EXPSAV)
	  WRITE(8,*) ' '
	  close(unit=8)
	  IF(ITERM.LT.0) THEN
	    CALL MGOPRNTPLOT(NVEC)
	    PRINT*,' NO. VECTORS PLOTTED',NVEC
	    NPLOTS = NPLOTS + 1
	  ELSE
	    CALL MGOTCLOSE
	  ENDIF
	RETURN
	END
	integer*4	function	rassign(i)

	   if (i.eq.2) then
	      ir = 1
	   else if (i.eq.6) then
	      ir = 2
	   else if (i.eq.5) then
	      ir = 3
	   else if (i.eq.7) then
	      ir = 4
	   else
	      ir = 0
	   end if

	  rassign = ir

	return
	end
	SUBROUTINE LSFIT(Y,X,WT,N,A,B,RMS)
C
C	FIT Y = A*X + B
C
	DIMENSION Y(128),X(128),WT(128)
C
C
	IF(N.LE.0) THEN
		PRINT*,'NO POINTS IN LSFIT'
		RETURN
	ENDIF
C
	SYY=0.
	SXY=0.
	SXX=0.
	SX=0.
	SY=0.
	SC=0.
	DO 1 J = 1,N
	   SYY=SYY+WT(J)*Y(J)**2
	   SXY=SXY+WT(J)*X(J)*Y(J)
	   SXX=SXX+WT(J)*X(J)**2
	   SX=SX+WT(J)*X(J)
	   SY=SY+WT(J)*Y(J)
	   SC=SC+WT(J)
 1	CONTINUE
	A=0.
	B=0.
	DEN=SXX*SC-SX*SX
	IF(DEN.EQ.0.) TYPE 101
	IF(DEN.EQ.0.) RETURN
	A=(SXY*SC-SY*SX)/DEN
	B=(SXX*SY-SX*SXY)/DEN
	SUMSQ=SYY+SXX*A**2+SC*B**2-2.*SXY*A-2.*SY*B+2.*SX*A*B
	SM=SUMSQ/SC
	RMS=SQRT(AMAX1(SM,0.))
	RETURN
C
C
 101	FORMAT(' VANISHING DETERMINANT IN LSFIT')
	END

