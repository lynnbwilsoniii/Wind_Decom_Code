	subroutine makefile(ch)
!
c	derived from apmplot--but makes a file of floating potential
c		measurements and fluxes
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
	integer*4	etime(6)
	real*4 		apccal,apmcal,drivev
	real*4		wt(1000)
	real*4		densi(4000),tempel(4000),tempion(4000)
	real		output(4,4)
	real*8		scet,scetst,delt,scet_ret
	data error_count /0/
	data ndaysdone /0/
	data nstart /0/
	data istart /0/
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

	loop_and_gather = 1
	scet = 0.
	call w_channel_position(ch,scet)
c	type*,'channel, scet',ch,scet
	nday = scet
C
	DO IDO = 1,2
	  IF(IDO.EQ.1) SCET = DFLOAT(NDAY) + 117.D00/1440.D00    ! 0157
	  IF(IDO.EQ.2) SCET = DFLOAT(NDAY) + 181.D00/1440.D00    ! 0301

	  type*,'position channel pointer to',scet
	ok = w_channel_position(ch,scet)
	if ( ok.eq.1 ) then
		type*,'channel pointer positioned to',scet
	else
		 type*,'cannot position pointer'
	endif
	
 1010     ok = w_event(ch,'HK')
	  if ( ok.ne.1 ) then
		type *, 'cannot get event after', scet
		error_count = error_count + 1
		type*,'in getting started, error',error_count
		if(error_count.lt.100) go to 1010
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

!	ok = wind_tm_set_messages_off(ch)
!	if (.not. ok) stop 'cannot turn messages off'

C	type *, 'Please wait, gathering data ...'

 1000	   continue

	   item = 'EVENT_SCET'
	   okt = w_item_i4(ch, item, s_scet, 2, ret_size)
C
	   if(is.ge.sz_x_dim) then
	      print*,'return for large is =',is
	 	return
	   endif

	if (.not. ok) then
		type *, 'apmplot,cannot get event after', scet
		error_count = error_count + 1
		if(error_count.lt.10) go to 1010
		TYPE*,'TOO MANY ERRORS'
		GO TO 1010
	else
		error_count=0
	endif

	item = 'APM_ANGLE'
	ok = w_item_i4(ch, item, angle, 1, ret_size)
	
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	if(major.eq.last_major) PRINT*,'MAJOR FRAME DID NOT ADVANCE',MAJOR
	if(major.eq.last_major) go to 1000
	last_major = major
c
c	event is ok, process it
c
	   is = MIN0(is+1,sz_x_dim)

c
c***********   for routine processing, started 10 May 1997, which stops
c		at 0300, every second day except in magnetosphere
	   if(s_scet(2).gt.31000) go to 2000
c*************
	   hh = S_SCET(2)/10000
	   min = mod(S_SCET(2),10000)/100
	   SS = MOD(S_SCET(2),100)
c	   scet = dfloat(nday) + hh/24. + min/1440. + SS/86400.

	   item = 'APC_Z'		! word 74
	   if(is.gt.0) then
	     ok = w_item_i4(ch, item, z_apc(is), 1, ret_size)
	     if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok
c	     ONOFF(ix) = (.not. Z_APC(IS)) .and. 1
	   endif

	   item = 'APC_DAC'		! word 70
	   ok = w_item_I4(ch, item, idrive, 1, ret_size)
	   if ( ok ) then
c		drivev = apccal(apc_dac(is))
		drivev = apccal(idrive)
	   else
		 write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif
	   drive(is) = drivev
C
	   item = 'APC_X'		! word 69
	   ok = w_item_i4(ch, item, x_apc(is), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok


	   item = 'APM_X_DC'		! word 65
	   ok = w_item_i4(ch, item, x_dc, 1, ret_size)
c	   av(1,is) = x_dc
	   if (ok) then
		av(1,is) = apmcal(3,x_dc)
	   endif

	   item = 'APC_X_RESISTOR'		! word 69
	   ok = w_item_i4(ch, item, x_resistor, 1, ret_size)
	   if (ok) then
		irtm = x_resistor
		ir = rassign(irtm)
		resistor(1,is) = ir
c		resistor(3,is) = ir
	   endif

	   item = 'APC_Y'		! word 69
	   ok = w_item_I4(ch, item, y_apc(is), 1, ret_size)
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
c
c	   get electron flux
c
	   if(ido.eq.1) then
 7703	     continue
	     read(77,*,err=7703) (etime(j),j=1,6),eflux
	   else
c		read to end
 7701	     continue
	     read(77,*,end=7702,err=7701) (etime(j),j=1,6),eflux
	     go to 7701
 7702	     continue
	   endif
c
c	   item = 'WIND_3DP_ION_DENSITY_R4'
c	   ok = w_item_r4(ch, item, densit, 1, ret_size)
c	   if(ok.ne.1.) then
c	     print*,'3dp data not available,',s_scet
c	     return
c	   endif
c	   densi(is) = densit
c	   item = 'WIND_3DP_E_TEMP_R4'
c	   ok = w_item_r4(ch, item, densit, 1, ret_size)
c	   tempel(is) = densit
c	   item = 'WIND_3DP_ION_TEMP_R4'
c	   ok = w_item_r4(ch, item, densit, 1, ret_size)
c	   tempion(is) = densit
c
c************  THIS IS THE TEST FOR 'ON', BUT IT WONT WORK IF WE
c		GO TO BIAS ALWAYS ON.  IN THAT CASE I WILL HAVE TO
c		USE TIME.
c	note x_apc = 1 is relay disabled or off, 0 is enabled, or on
c
    	  if(x_apc(is).ne.0) then
	  
	    write(88,1549) S_SCET,z_apc(is),angle,av(1,is),av(2,is),
     1		av(3,is),(etime(j),j=4,5),eflux
	    ntot = is
	    itend = s_scet(2)/100
	    if(nstart.eq.0) nstart = is
	    if(istart.eq.0) then
	  	itstart = s_scet(2)/100
	 	istart = 1
	    endif
	  else
	    print*,'is,x_apc(is)=',is,x_apc(is)
	  endif
 1549	  format(i10,i8,i3,i5,3f8.2,i4.2,i2.2,e12.3)
	enddo

c 2001	GO TO 1000
C 	end do
 2000	continue
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
	integer*4	function	rassign(i)
	integer*4 i,ir,irtable(8)
	data irtable /1,3,1,4,2,3,2,4/

	  ir = i.and.7
	  rassign = irtable(i+1)

	return
	end
