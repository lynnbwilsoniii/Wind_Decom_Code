! trad2c.for - tests the get_event stuff and the dbms stuff for rad2 stuff
! with respect to cpu time and i/o, for event/item gathering over an entire
! file.

	program		trad2c
	implicit	none
	integer*4	ok
	integer*4	ch, major, minor
	character*4	event
	parameter	(event='RAD2')
	integer*4	i
	integer*4	error_count
	integer*4	event_number
	integer*4	wind_tm_open_channel
	integer*4	wind_tm_get_mfmf
	integer*4	wind_tm_increment_packet
	integer*4	wind_tm_close_channel
	integer*4	w_event
	integer*4	w_version
	integer*4	w_channel_filename
	character*16	ver 
	character*128	f
	integer*4	n_iterations
	integer*4	show_usage
	integer*4	lnblnk

	n_iterations = 1000

	ok = wind_tm_open_channel(ch,'*19950214*')
	!ok = wind_tm_open_channel(ch,'offline')
	if (ok .ne. 1) stop 'cannot open tm channel'

	ok = wind_tm_get_mfmf(ch,major,minor)
	if (ok .ne. 1) stop 'cannot get stream position'
	ok = wind_tm_increment_packet(major,minor)

	ok = w_channel_filename(ch,f)
	i = lnblnk(f)
	type *, ' '
	type *, 'File: ', f(1:i)

	ok = w_version(ver)
	type *, 'WIND_LIB version is: ', ver, ' Event type is ', event
	type *, 'No user items are gathered during this program run.'

	event_number = 0
 1000	continue

	ok = w_event(ch,event)
	i = wind_tm_get_mfmf(ch,major,minor)
	if (ok .eq. 82) then
	   goto 2000
	else if (ok .ne. 1) then
	   error_count = error_count + 1
	   ok = wind_tm_increment_packet(major,minor)
	   goto 1000
	end if
	event_number = event_number + 1

	goto 1000
 2000	continue
	type *, ' '
	type *, 'EOF encountered,', event_number, ' events processed,'
	type *, 'not including', error_count, ' events with errors.'
	type *, ' '

	ok = wind_tm_close_channel(ch)
	if (ok .ne. 1) stop 'cannot close channel'

	ok = show_usage()
	stop
	end
