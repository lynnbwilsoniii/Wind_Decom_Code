! k.for - sample wind_tm_lib application, types a word from successive minor
! frames continuously until the user presses cntl/c.
!
! to build:
!		$ fortran k
!		$ link k,wind_lib/lib
!
! Note that the logicals wind_lib and wind_tm_lib must be predefined.
!
	options/extend_source
	implicit	none
	integer*4	word_number
	integer*4	word
	integer*4	packet(431)
	integer*4	major
	integer*4	minor
	integer*4	ch
	integer*4	status
	integer*4	wind_tm_open_channel
	integer*4	wind_tm_where_am_i
	integer*4	wind_tm_increment
	integer*4	wind_tm_get_word

! get the word of the minor frame to print
 10	write(6,'(a,$)') ' Word number [0..255]? '
	read (5,*,err=10,end=99) word_number

! open a channel to the realtime telemetry data stream
	status = wind_tm_open_channel(ch,'realtime')
	if (.not. status) stop 'Cannot open channel.'

! get the current major/minor frame numbers
	status = wind_tm_get_my_position(ch,major,minor)
	if (.not. status) stop 'Cannot get current position.'

! loop forever
	do while(1)
	   status = wind_tm_get_packet(ch,major,minor,packet)
	   if (.not. status) stop 'Cannot get word.'
	   write(6,6) major, minor, packet(4),packet(6),packet(7)
	   call wind_tm_increment(major,minor)
	end do

  6	format(1x,'maj=',i9,2x,'min=',i3,2x,3i6)
 99	end

