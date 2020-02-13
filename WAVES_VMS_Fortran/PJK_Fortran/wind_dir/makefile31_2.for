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
