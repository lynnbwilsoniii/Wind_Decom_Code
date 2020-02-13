	  if(nchgp.eq.1) event = 'FFTH'         
	  if(nchgp.eq.2) event = 'FFTM'         
	  if(nchgp.eq.3) event = 'FFTL'         
	  first_channel = 1         
	  if(nchgp.eq.2) first_channel = 3         
	  if(nchgp.eq.3) first_channel = 7         
	  nw = 2         
	  if(nchgp.gt.1) nw = 4         
 10	  ok = w_event(ch,event)         
C         
	   item = 'EVENT_SCET_R8'         
	   ok = w_item_r8(ch, item, scet8, 1, return_size)         
	   call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)         
C         
	   item = 'channel_number'         
	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)         
	   if(fft_channel.ne.first_channel) then         
		last_channel = fft_channel         
		type*,'channel',fft_channel,' is not first, restart'         
		go to 10         
	   endif         
c	   last_channel = fft_channel         
C         
	   item = 'SOURCE'         
	   ok = w_item_i4(ch, item, ISRC, 1, return_size)         
C	         

