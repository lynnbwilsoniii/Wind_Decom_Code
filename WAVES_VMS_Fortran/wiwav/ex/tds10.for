	program test

	real*8		scet
	integer 	ret_size
	character*16	string
	integer		ok, ST
	integer*4	ret_m
	integer*4	m(4000)
	real*4		x(4000)
	integer*4	w_event

	call w_channel_open(ch, 'OFFLINE')

	call w_version(string)
	type *, 'WIND_lib version: ', string

	ok = w_event(ch, 'FILL')

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

	call w_item_i4(ch, 'dpu_clock',		m, 1, ret_m)
	type *, m(1)

	call w_item_r4(ch, 'dpu_clock_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_r4(ch, 'dpu_clock_period_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_i4(ch, 'sun_angle',		m, 1, ret_m)
	type *, m(1)

	call w_item_r4(ch, 'sun_angle_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_i4(ch, 'mag_angle',		m, 1, ret_m)
	type *, m(1)

	call w_item_r4(ch, 'mag_angle_r4',	x, 1, ret_m)
	type *, x(1)

	call w_item_i4(ch, 'mag_elevation',	m, 1, ret_m)
	type *, m(1)

	call w_item_r4(ch, 'mag_elevation',	x, 1, ret_m)
	type *, x(1)

	stop 'ok?'

	end
