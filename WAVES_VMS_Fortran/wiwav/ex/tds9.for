	program test

	real*8		scet
	integer 	ret_size
	character*16	string
	integer		ok, ST
	integer*4	ret_m
	integer*4	m(4000)
	real*4		x(4000)
	integer*4	w_event
	structure /equiv/
	union
	   map
	      integer*4	i4
	   endmap
	   map
	      real*4	r4
	   endmap
	endunion
	endstructure
	record /equiv/ e
	integer*4	i,j,k
	character*16	ver

	call w_version(ver)
	type *, 'Using windlib version ', ver

	call w_channel_open(ch, 'OFFLINE')

	call w_version(string)
	type *, 'WIND_lib version: ', string

	i = 0
	ok = 0
	do while (ok .ne. 1 .and. i .lt. 10)
	   ok = w_event(ch, 'FILL')
           i = i + 1
	   write(6,*) ' #', i, '  ok=', ok
	end do
	if (ok .ne. 1) stop 'cannot find a good event'

	call w_item_i4(ch, 'Event_Number',	m, 4, ret_m)
	type *, m(1)

	call w_item_i4(ch, 'Quality',		m, 4, ret_m)
	type *, m(1)

	call w_item_i4(ch, 'channel',		m, 4, ret_m)
	type *, m(1)

	call w_item_i4(ch, 'source_channel',	m, 4, ret_m)
	type *, m(1)

	call w_item_i4(ch, 'source',		m, 4, ret_m)
	type *, m(1)

	call w_item_char(ch, 'source',		string, 1, ret_m)
	type *, string

	call w_item_char(ch, 'pingpong',	string, 1, ret_m)
	type *, string

	call w_item_r4(ch, 'rx_speed_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_r4(ch, 'rx_filter_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_r4(ch, 'rx_period_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_r4(ch, 'rx_interval_r4',	x, 1, ret_m)
	type *, x(1)

	type *, '!-------------------------------------------------------------'
	call w_item_r4(ch, 'dpu_clock_r4',	x, 1, ret_m)
	type *, x(1)
	e.r4 = x(1)
	type '(1x,a,z8.8)', 'dpu_clock_r4 in hex: ', e.i4

	call w_item_i4(ch, 'dpu_clock',		m, 1, ret_m)
	type *, m(1)

	call w_item_i4(ch, 'dpu_clock',		m, 1, ret_m)
	type *, m(1)

	call w_item_r4(ch, 'dpu_clock_period_r4',	x, 1, ret_m)
	type *, x(1)

	stop 'ok?'

	end
