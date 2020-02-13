	PROGRAM ANYITEM
C
C	PRINTS OUT ANY ITEM
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	integer*4	ok
	integer*4	get_stream_name
	integer*4	wait_for_events			! an entry point
	integer*4	err_count
	character*80	stream
	character*4	event
	integer*4	return_size
	character*32	s
	integer*4	s_scet(2)
	real*8		scet,scettds,scetfill,beginevt,endevt
	integer*4	major, minor
	character*80	file
	character*32	item
!
C
	INTEGER*4 TDSCH,ch,NUMBER_EVENT
	DATA TWOPI /6.2831853/
	DATA EVENT /'TDSF'/
C
 1002	FORMAT(A)
C
C	GET STARTED
C
	ok = get_stream_name(stream)
	if (.not. ok) stop 'no file supplied.'
C
        if(stream.ne.'realtime') then
 10	  write(6,*)  'type hr,min to start, e.g. 0412'
	  read(5,3,err=10) iq, NHRMN
	  type*,NHRMN
	  HH = NHRMN/100
	  MM = MOD(NHRMN,100)
	endif
c
  5	format(q,a)
  4	format(1x,'enter number of events to find and process')
  3	format(q,i10)
c
	ok = w_channel_open(tdsch,stream)
	if (.not. ok) stop 'Cannot open tds channel'
	scettds = 0.
	call w_channel_position(tdsch,scettds)
	print*,'tds file starts at scettds',scettds
	ndds = scettds
	scettds = float(ndds) + hh/24. + mm/1440.
	print*,'set tds channel position to',scettds
	call w_channel_position(tdsch,scettds)
	print*,'tds channel position set to',scettds
	  OK = W_EVENT(TDSCH,'TDSF')
	  item = 'EZ_MONOPOLE_LENGTH_R4'
	  ok = w_item_r4(tdsch, item, RITEM, 1, return_size)
	  print*,ITEM,RITEM,' RETURN_SIZE=',RETURN_SIZE
	STOP
	END
c
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	integer*4	iq,NREM,NHRMN,IFASTSLOW

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
