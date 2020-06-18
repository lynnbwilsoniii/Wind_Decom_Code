
	s_buffer   = lonarr(512)
        s_size     = long(512)
        s_retsize  = long(0)
        f_buffer   = lonarr(512)
        t_size     = long(1)
        t_retsize  = long(0)
        scet_ur8   = double(0)
	channel    = long(0)
	mytimes    = dblarr(512)
	ok         = 0L
	ver        = 'abcdefghijklmnopqrstuvwxyz'

	print, '...getting the version number...'
;	ok = w_version(ver)
	print, 'Using wind_lib version: ', ver

	print,'...opening the channel...'
	ok = w_channel_open(channel,'*19991212*')
;	ok = w_channel_open(channel,'offline')
	if ok ne 1 then begin
	   print, '...channel not opened...'
	   print, '...exiting program...'
	   stop
	endif
	print,'...opened the channel...'

	try_again:	
 
	ok = w_event(channel,'RAD2')
        print,'getting rad1 event ',ok
	if (ok eq 1) then begin
	  ok = w_item_i4(channel, 'S',             s_buffer, 	$
                            s_size, s_retsize)
          print,'buffer size of ',s_retsize
	  print, 's, ok=', ok, ':', s_buffer(0:9)

	  ok = w_item_r8(channel, 'S_SCET_R8', mytimes, 	$
                            s_size, s_retsize)
	  print, 'times, ok=', ok, ':', mytimes(0:9)

	  ok = w_item_i4(channel, 'FREQUENCIES',   f_buffer, 	$
                            s_size, s_retsize)
	  print, 'freq, ok=', ok, ':', f_buffer(0:9)
	  ok = w_item_r8(channel, 'EVENT_SCET_R8', scet_ur8,    $
                            t_size, t_retsize)
	  print, 'scet, ok=', ok, ':', scet_ur8
;          if (ok eq 1) and (s_retsize gt 0) then begin
;            f_buffer = f_buffer(0:s_retsize-1)
;            s_buffer = s_buffer(0:s_retsize-1)
;	  endif

	endif 
        if (ok ne 82) and (ok ne 1) then goto, try_again

	stop
	end

