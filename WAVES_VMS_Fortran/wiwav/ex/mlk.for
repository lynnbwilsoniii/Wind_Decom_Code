! mlk.for - testing scet/event troubles 29-AUG-95

	program mlk
	real*8		scet
	integer*4	ok, channel, insize, outsize
	integer*4	scet_num(2), ert(2)
	integer*4	i,mjr,mnr,dpu_ert
	integer*4	w_event, w_item_r8, w_item_i4
	integer*4	w_channel_open

	insize = 1
	ok = w_channel_open(channel,'offline')

	do while (ok .ne. 82 .and. i .lt. 16)
	  i = i + 1
	  ok = w_event(channel,'RAD2')
	  ok = w_item_r8(channel,'event_scet_r8',scet,insize,outsize)
	  ok = w_item_i4(channel,'event_scet',scet_num,insize,outsize)
	  ok = w_item_i4(channel,'event_ert',ert,insize,outsize)
	  ok = w_item_i4(channel,'DPU_MAJOR_FRAME', mjr, insize, outsize)
	  ok = w_item_i4(channel,'DPU_MINOR_FRAME', mnr, insize, outsize)
	  ok = w_item_i4(channel,'DPU_MAJOR_ERT', dpu_ert, insize, outsize)
	  type *, i,': ',scet,scet_num, ' DPU:', mjr, mnr, dpu_ert, ert(1)
	end do
	stop
	end
