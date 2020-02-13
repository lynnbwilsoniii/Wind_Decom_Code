	subroutine makefile(ch)
!	program		apmplot
!
c	derived from apmplot--but treats a long run of antenna resistance
c		measurements
c	the way this program works is to store all the data for a day
c		in, for example, av(irs,n) but to record the starting
c		and end values, nstart and ntot, corresponding to
c		measurements which are to be fitted.  
c	note also that 'apmplot_def.for' contains a lot of common stuff
c		and its dimensions
c	On 5 April, 2007, I commented the main output line with CT, and 
c	added stuff to write max and min of drive, max and min of X voltage
c	to file 66
c
! to build:
!	$ fortran makefile11
!	$ link mission_scan11,makefile11,lsfiter or lsfit,wind_lib/lib
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
	integer*4	ok,ch,get_output_device,loop_and_gather
	character*80	stream
c	
	ok = 1
	terminal_output = 0
	if (ok) ok = loop_and_gather(ch)
c	TYPE*,'RETURN FROM LOOP_AND_GATHER at',sceti4(2)
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	loop_and_gather(ch)
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
	character*32	item
	character*4	year
	character*40	com(15)
	integer*4	error_count,hh,min,nday
	integer*4	ihrmn,yyyy,mon,dd,mm,ss,ms
	integer*4	iv		! drive telemetry number
	integer*4	ir,irtm		! resistor index
	integer*4	ia		! antenna index, 1=x,2=y,3=z
	integer*4	is		! sample number
	integer*4	IONOFF,nonoff	! DAC ON OR OFF
	integer*4	ndaysdone
	integer*4	angle
	integer*4	major, last_major,cal_on
	real*4 		apccal,apmcal,drivev
	real*4		wt(1000)
	real*4		densi(4000),tempel(4000),tempion(4000)
	real		output(4,4)
	real*8		scet,scetst,delt,scet_ret
	common /results/ AA(5),BB(5),RESIS(5),FRMS(5)
	data error_count /0/
	data ndaysdone /0/
	data nstart /0/
	data istart /0/
c
	drivemax = -1000.
	drivemin = 1000.
	vxmax    = -1000.
	vxmin	 = 1000.
c
	is = 0
	nstart = 0
	istart = 0
c
 3000	continue
c	write(37,*) 'return from stream name:',stream
!	ok = wind_tm_open_channel(ch,'realtime')
	call wind_tm_get_filename(ch,file)
c	type *,'file: ',file(:72)
c	type *,'file head ',file(:37)
c	type *,'year ',file(31:34)
c	type *,'month ',file(35:36)
c	type *,'day ',file(37:38)

C	delt = 15.d00/1440.d00             ! 15 min. interval
	delt = 2.d00/1440.d00              !  2 min. interval
	delt = 0.			   !  do not skip data
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
C
C	this is apparently used to eliminate duplication of major frames
C
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	last_major = major-1

	item = 'EVENT_SCET_R8'
C	type*,'going to get first item=',item
C	type*,'channel no',ch
c	ok = w_item_r8(ch, item, scet, 1, ret_size)
C	type*,'return size',ret_size
c
	call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
c
c	     ihrmn = 100*hh+mm
c	     TYPE 1005,YYYY,MON,DD,IHRMN
C		write(26,*) YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)

	scet = scet + 6.d00/1440.d00			! advance six 
!						minutes to avoid cal


!	ok = wind_tm_set_messages_off(ch)
!	if (.not. ok) stop 'cannot turn messages off'

C	type *, 'Please wait, gathering data ...'

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
 1010	   if(ok.eq.82) then
		ndaysdone = ndaysdone + 1
		type*,'finished at end of file',file
		type*,'DAYS DONE SO FAR',ndaysdone,' count',count(1)
		ok = wind_tm_close_channel(ch)
c		if(ndaysdone.lt.29) go to 3000
c		if(ndaysdone.lt.2) go to 3000
c		if(ndaysdone.lt.1) go to 3000
	        call fitres(ch,nstart,ntot,itstart,itend)
		go to 2000
c		call plotall(-1)
c
c	note this is only for end of file--this should not happen so
c		it's not complete
c
c	if(nstart.gt.0) then
c	  open(unit=89,file='ztable.dat',status='old',access='append')
c	  call fitres(ch,nstart,ntot,itstart,itend)
c	  dens = 0.
c	  dsigma = 0.
c	  etemp = 0.
c	  tsigma = 0.
c	  eflux = 0.
c	  write(89,989),yyyy,mon,dd,itstart,itend,resis,frms,dens,dsigma,
c     1		etemp,tsigma,angle
c	  close(unit=89)
c	endif
c	 	return
	   endif

	   if(is.ge.sz_x_dim) then
c	   if(is.ge.20) then
c		call plotall(-1)
c		ix = 1
	      print*,'return for large is =',is
	 	return
	   endif

	if (.not. ok) then
		type *, 'apmplot,cannot get event after', scet
		error_count = error_count + 1
		if(error_count.lt.10) go to 1001
c		CALL PLOTALL(-1)
		TYPE*,'TOO MANY ERRORS'
		GO TO 3000
	else
		error_count=0
	endif

c	item = 'CAL_GENERATOR'
	item = 'CAL_ON'
	ok = w_item_i4(ch, item, cal_on, 1, ret_size)
	item = 'APM_ANGLE'
	ok = w_item_i4(ch, item, angle, 1, ret_size)
	if(cal_on.eq.1) go to 1001		! 1 means radio cal on.
	
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	if(major.eq.last_major) PRINT*,'MAJOR FRAME DID NOT ADVANCE',MAJOR
	if(major.eq.last_major) go to 1000
	last_major = major
c
c	event is ok, process it
c
c	   is = MIN0(count(1)+1,sz_x_dim)
	   is = MIN0(is+1,sz_x_dim)

c 	    item = 'EVENT_SCET_R8'
c	    ok = w_item_r8(ch, item, scet, 1, ret_size)

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, s_scet, 2, ret_size)
c
c***********   for routine processing, started 10 May 1997, which stops
c		at 0300, every second day except in magnetosphere
	   if(s_scet(2).gt.31000) then
c
	write(66,1066) S_SCET,scet,vxmax,vxmin,drivemax,drivemin
 1066	format(i10,i8,f10.4,4e12.3)
		go to 2000
	   endif
c*************
c	   if(is.eq.1) type*, 'first sample, scet',
c     1		s_scet
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
	   if ( ok ) then
c		drivev = apccal(apc_dac(is))
		drivev = apccal(idrive)
		drivemax = amax1(drivemax,drivev)
	        drivemin = amin1(drivemin,drivev)
	   else
		 write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif
	   drive(is) = drivev
C
	   item = 'APC_X'		! word 69
	   ok = w_item_i4(ch, item, x_apc(is), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok

	   item = 'APM_X_PEAK'		! word 63
	   ok = w_item_i4(ch, item, x_peak, 1, ret_size)
	   if (ok) then
c		is = MIN0(count(1)+1,sz_x_dim)
		count(1) = is
		pk(1,is) = apmcal(1,x_peak)
c		drive(is) = drivev
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

	   item = 'APM_X_DC'		! word 65
	   ok = w_item_i4(ch, item, x_dc, 1, ret_size)
c	   av(1,is) = x_dc
	   if (ok) then
		av(1,is) = apmcal(3,x_dc)
		vxmax = amax1(vxmax,av(1,is))
		vxmin = amin1(vxmin,av(1,is))
	   endif

	   item = 'APC_X_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, x_resistor, 1, ret_size)
	   if (ok) then
		irtm = x_resistor
		ir = rassign(irtm)
		resistor(1,is) = ir
c		resistor(3,is) = ir
	   endif
c
	   item = 'APC_Y'		! word 69
	   ok = w_item_I4(ch, item, y_apc(is), 1, ret_size)

	   item = 'APM_Y_PEAK'		! word 64
	   ok = w_item_i4(ch, item, y_peak, 1, ret_size)
	   if (ok) then
c		is = MIN0(count(2)+1,sz_x_dim)
		count(2) = is
		pk(2,is) = apmcal(2,y_peak)
	   endif

	   item = 'APM_Y_DC'		! word 66
	   ok = w_item_i4(ch, item, y_dc, 1, ret_size)
	   if (ok) then
		av(2,is) = apmcal(4,y_dc)
	   endif

	   item = 'APC_Y_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, y_resistor, 1, ret_size)
	   resistor(2,is) = y_resistor
	   if (ok) then
		irtm = y_resistor
		ir = rassign(irtm)
		resistor(2,is) = ir
	   endif

	   item = 'APM_Z_DC'		! word 67
	   ok = w_item_i4(ch, item, z_dc, 1, ret_size)
	   if (ok) then
c		is = MIN0(count(3)+1,sz_x_dim)
		count(3) = is
		av(3,is) = apmcal(5,z_dc)
	   endif

	   item = 'APC_Z_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, Z_resistor, 1, ret_size)
	   if (ok) then
		irtm = Z_resistor
		ir = rassign(irtm)
		resistor(3,is) = ir
	   endif

	if(1) go to 1001
	   item = 'WIND_3DP_ION_DENSITY_R4'
	   ok = w_item_r4(ch, item, densit, 1, ret_size)
	   if(ok.ne.1.) then
	     print*,'3dp data not available,',s_scet
	     return
	   endif
	   densi(is) = densit
	   item = 'WIND_3DP_E_TEMP_R4'
	   ok = w_item_r4(ch, item, densit, 1, ret_size)
	   tempel(is) = densit
	   item = 'WIND_3DP_ION_TEMP_R4'
	   ok = w_item_r4(ch, item, densit, 1, ret_size)
	   tempion(is) = densit
c
c************  THIS IS THE TEST FOR 'ON', BUT IT WONT WORK IF WE
c		GO TO BIAS ALWAYS ON.  IN THAT CASE I WILL HAVE TO
c		USE TIME
c
	if(x_apc(is).eq.0) then
C	  write(88,1549) is,drivev,av(1,is),pk(1,is)
C     1		,av(2,is),pk(2,is)
	  ntot = is
	  itend = s_scet(2)/100
	  if(nstart.eq.0) nstart = is
	  if(istart.eq.0) then
	  	itstart = s_scet(2)/100
	 	istart = 1
	  endif
	endif
 1549	format(i6,5f8.2)

 2001	GO TO 1000
C 	end do
 2000	continue


c	call plotall(-1)
c	call plotdrive(-1)
c
	if(nstart.gt.0) then
CT	  open(unit=89,file='ztable.dat',status='old',access='append')
c
	  dens = 0.
	  dsigma = 0.
	  cdfn = 0.
	  do n = nstart,ntot-1
	    if(abs(densi(n)).lt.1000.) then
		dens = dens + densi(n)
		cdfn = cdfn + 1.
		dsigma = dsigma + densi(n)**2
	    endif
	  enddo
	  dens = dens/cdfn
	  dsigma = dsigma/cdfn - dens**2
	  dsigma = sqrt(amax1(dsigma,0.))
c
	  etemp = 0.
	  tsigma = 0.
	  cdfn = 0.
	  do n = nstart,ntot-1
	    if(abs(tempel(n)).lt.1000.) then
	        te3dp = 1.97*tempel(n) - .41
		etemp = etemp + te3dp
		cdfn = cdfn + 1.
		tsigma = tsigma + te3dp**2
	    endif
	  enddo
	  etemp = etemp/cdfn
	  tsigma = tsigma/cdfn - etemp**2
	  tsigma = sqrt(amax1(tsigma,0.))
c
	  tempi = 0.
	  sigmait = 0.
	  cdfn = 0.
	  do n = nstart,ntot-1
	    if(abs(tempion(n)).lt.1000.) then
		tempi = tempi + tempion(n)
		cdfn = cdfn + 1.
		sigmait = sigmait + tempion(n)**2
	    endif
	  enddo
	  tempi = tempi/cdfn
	  sigmait = sigmait/cdfn - tempi**2
	  sigmait = sqrt(amax1(sigmait,0.))
c
	  eflux = dens*sqrt(etemp)
c
	  call fitres(ch,nstart,ntot,itstart,itend)
	    baseR = 0.
	    XR = 0.
	    YR = 0.
c
c	this calculation assumes equal X and Y base resistances,
c	  measured R = base R and ant R in parallel, and R prop. to 1/L
c
	  IF(RESIS(1).NE.0..AND.RESIS(3).NE.0.) THEN
	    YXT = 1./RESIS(1)		! x total admittance
	    YXT = YXT - 1./1000.	! subtract preamp admittance
	    YYT = 1./RESIS(3)		! y total admittance
	    YYT = YYT - 1./10000.	! subtract preamp admittance
C	    solve for base and antenna resistances
	    ALPHA = (YXT-YYT)/42.5	! 42.5 = 50 m - 7.5 m
	    YB = YYT - 7.5*ALPHA	! base admittance
	    IF(YB.NE.0.) baseR = 1./YB
	    XR = 1./(50.*ALPHA)
	    YR = 1./(7.5*ALPHA)
	  ENDIF
c
CT	 write(89,989),yyyy,mon,dd,itstart,itend,resis,frms,angle,
CT     1	   dens,dsigma,etemp,tsigma,tempi,sigmait,eflux,baseR,XR,YR
	print 898,yyyy,mon,dd,angle,baser,xr,yr,resis(5)
	  close(unit=89)
	endif
c
 898	format(i5,i2.2,i2.2,i5,4e12.3)
 989	format(i5,i2.2,i2.2,2i4,3f6.1,F6.0,f5.1,5f5.2,i4,f5.1,f5.2,
     1		f5.1,f5.2,f5.1,f5.2,f6.1,3f7.1)
c
  2	format(1x,a,a,a,z8.8)
c
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
	subroutine fitres(ch,nstart,ntot,itstart,itend)
c
c	on 21 Jan 2000, I altered this to only fit data from drive
c	voltages greater than drive_lim, because I felt that small
c	drive voltages give the resistance that I want.  The sequence
c	of drive voltages at present is 0., -.385, -.935, -1.41, -1.95
c		-2.9, -3.9, and -4.95.
c	I should change it to limit X and Y sun antenna
c		voltage to greater than 1., but if this is done the easy
c		way, it will bias the slope of the line, so not done yet.
c		it was not practical, or necessary, to limit the shade
c		data.
c	On 14 July 2000, a drive of abou -10 V was added to try to get
c		good measurements of Rz.  The analysis was changed
c		to omit a lot of intermediate voltages, which are
c		not useful in the analysis
c
	include		'apmplot_def.for'
	real*4		wt(1000),yy(1000),xx(1000)
	real*4		xresistor(9),yresistor(9),zresistor(9)
	integer*4	ch
c	character*80 file
	character*32 item
	common /results/ AA(5),BB(5),RESIS(5),FRMS(5)
	data wt /1000*1./
	data xresistor /34.8,375.,34.8,1025.,124.8,375.,124.8,2*1025./
	data yresistor /200.,1650.,200.,10150.,650.,1650.,650.,2*10150./
	data zresistor /8*68.,1.E6/
	data drive_lim /-3./
c
	call wind_tm_get_filename(ch,file)
c	print*,'fitres',nstart,ntot
	if(nstart.eq.0) return
c
c	do n = nstart,ntot
c	  write(66,*) n,av(1,n),pk(1,n),av(2,n),pk(2,n)
c	enddo
c
	nl = 0
	do n = nstart,ntot
c	    write(37,*) n,drive(n),av(1,n)
C	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		wt(nl) = 1.
		xx(nl) = av(1,n)
		yy(nl) = drive(n)	  
c		print*,'n,drive,av',n,drive(n),av(1,n)
C	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		item = 'APC_X_RESISTANCE'
		ok = w_item_i4(ch, item, itemp, 1, ret_size)
		print*,'x index',itemp
		rm = xresistor(itemp+1)
	      	print*,'x resistor',itemp,rm
	if(1) stop 'x resistor'
		rm = 34.8
		if(A.ne.1.) res = rm/(A-1.)
c		write(89,*) 'x av',A,B,res,RMS
c		print*,'x av:A,B,res,rms',A,B,res,RMS
C	if(1) stop
c
		AA(1) = A
		BB(1) = B
		resis(1) = res
		FRMS(1) = rms
c
	nl = 0
	do n = nstart,ntot
	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		wt(nl) = 1.
		xx(nl) = pk(1,n)
		yy(nl) = drive(n)	  
	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A.ne.1.) res = rm/(A-1.)
c		write(89,*) 'x pk',A,B,res,RMS
c		print*, 'x pk',A,B,res,RMS
		AA(2) = A
		BB(2) = B
		resis(2) = res
		FRMS(2) = rms
		rxshd = res
c
	nl = 0
	do n = nstart,ntot
c	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		wt(nl) = 1.
		xx(nl) = av(2,n)
		yy(nl) = drive(n)	  
c	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		item = 'APC_Y_RESISTANCE'
		ok = w_item_i4(ch, item, itemp, 1, ret_size)
c		print*,'y index',itemp
		rm = yresistor(itemp+1)
c	      	print*,'y resistor',itemp,rm
		rm = 200.
		if(A-1..ne.0.) res = rm/(A-1.)
c		write(89,*) 'y av',A,B,res,RMS
		rysun = res
		AA(3) = A
		BB(3) = B
		resis(3) = res
		FRMS(3) = rms
c
	nl = 0
	do n = nstart,ntot
	    if(drive(n).gt.drive_lim) then
		nl = nl+1
		wt(nl) = 1.
		xx(nl) = pk(2,n)
		yy(nl) = drive(n)	  
	    endif
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		if(A-1..ne.0.) res = 124.8/(A-1.)
c		write(89,*) 'y pk',A,B,res,RMS
		ryshd = res
		AA(4) = A
		BB(4) = B
		resis(4) = res
		FRMS(4) = rms
	nl = 0
	nfract = .74*(ntot-nstart)
C	print*,'nstart,ntot,nfract',nstart,ntot,nfract
	do n = nstart,ntot
c		use only data from drive near 0 and max neg. for Z
		nl = nl+1
		wt(nl) = 1.
		if(nl.lt.nfract) wt(nl) = 0.
		xx(nl) = av(3,n)
		yy(nl) = drive(n)	
C	print*,'z chk',nl,wt(nl),xx(nl),yy(nl)  
	enddo
		call lsfit(yy,xx,wt, nl, A, B, rms)
		res = 0.
		item = 'APC_Z_RESISTANCE'
		ok = w_item_i4(ch, item, itemp, 1, ret_size)
c		print*,'Z resistor index',itemp
c	      	print*,'windlib z resistor',itemp,rm
c		rm = zresistor(itemp+1)
c	      	print*,'z resistor',itemp,rm
		rm = 68.
c	      	print*,'actual z resistor',rm
		if(A-1..ne.0.) res = rm/(A-1.)
c		write(89,*) 'Z av',A,B,res,RMS
		rz = res
		AA(5) = A
		BB(5) = B
		resis(5) = res
		FRMS(5) = rms
c
	do n = 1,ntot-nstart+1
	  wt(n) = 1.
	enddo
c
c	open(unit=90,file='accrres.dat',type='old',access='append')
	dens = 0.
	etemp = 0.
	eflux = 0.
c	write(90,1001) file(31:38),itstart,itend,rxsun,rxshd,
c     1		rysun,ryshd,dens,etemp,	eflux
 1001	format(A9,2i5,7f8.2)
c	close(unit=90)
	return
	end
	integer*4	function	rassign(i)
	integer*4 i,ir,irtable(8)
	data irtable /1,3,1,4,2,3,2,4/

	  ir = i.and.7
	  rassign = irtable(i+1)

	return
	end
