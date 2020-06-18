	program display_packet

	integer*4	packet(1000)
	integer*4	major
	integer*4	minor
	integer*4	channel
	integer*4	ok
	integer*4	check
	integer*4	wind_tm_open_channel
	integer*4	wind_packet_sum
	!integer*4	wind_tm_increment_packet
	integer*4	wind_tm_get_packet
	integer*4	wind_tm_eof
	integer*4	i,j,k

	ok = wind_tm_open_channel(channel, 'offline')
	if (ok .ne. 1) stop 'cannot open channel'

	call wind_tm_get_mfmf(channel,major,minor)
	!call wind_tm_increment_packet(major,minor)
	type *, 'ch,major,minor=', channel, manor, minor

	j = 0
	do while(j .lt. 8)	! loop forever
	   j = j + 1
	    ok = wind_tm_get_packet(channel,major,minor,packet)
	    if (ok .ne. 1) then
	       type *, 'can''t get packet, printing line anyway...'
	       if (wind_tm_eof(channel,major,minor)) stop 'EOF'
	    end if
	    check = wind_packet_sum(packet,check)
	    write(6,600,iostat=ios) major,minor,
	1		(packet(i),i=1,12),packet(430),packet(431),
	1		check
	    call wind_tm_increment_packet(major,minor)
	end do

99	stop 'ok'
600	format(' MF.mf: ',i6,'.',i3.3,'   Packet: ',
	1		12z3.2,' ...',2z3.2,' (',z2.2,')')
	end
