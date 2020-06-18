! tnr6.pro
	program		tnr6
	implicit	none
	integer*4	ok
	integer*4	ch
	integer*4	i4buf(512)
	real*4		r4buf(512)
	real*8		r8buf(512)
	integer*4	bufsz /512/
	integer*4	EOF /82/
	character*8	cbuf
	character*16	ver
	character*24	item
	integer*4	retsz /0/
	character*60	file
	1	/'/home/wind/data_test/wi_lz_wav_19950214_v01.dat'/
	character*60	vfile /'WIND_DATA:wi_lz_wav_19950214_v01.dat'/
	integer*4	get_items /1/
	integer*4	n_events /0/
	integer*4	n_err /0/
	real*8		a,b
	integer*4	w_channel_open
	integer*4	w_event
	integer*4	w_item_i4
	integer*4	w_item_r4
	integer*4	w_item_r8
	integer*4	w_item_char
	integer*4	w_version

	ok = w_channel_open(ch,file,a,b)
	if (ok .ne. 1)  stop 'cannot open file'

	ok = w_version(ver)
	type *, 'Using wind_lib version ', ver
	type *, 'get_items = ', get_items

	type *, 'File: ', file
	type *, ' '

	do while (ok .ne. EOF)
	   ok = w_event(ch,'TNR')
	   if (ok .ne. 1) n_err = n_err + 1
	   if (ok .eq. 1) n_events = n_events + 1

	   if (ok .eq. 1 .and. get_items .eq. 1) then
	      item = 'SPECTRA_1'
	      ok = w_item_i4(ch,item,i4buf,bufsz,retsz)

	      item = 'SPECTRA_1_MICROVOLTS_R4'
	      ok = w_item_r4(ch,item,r4buf,bufsz,retsz)

	      item = 'SPECTRA_1_SCET_R8'
	      ok = w_item_r8(ch,item,r8buf,bufsz,retsz)

	      item = 'EVENT_MODE'
	      ok = w_item_char(ch,item,cbuf,1,retsz)

	      item = 'ANTENNA'
	      ok = w_item_char(ch,item,cbuf,1,retsz)
	   endif
	enddo

	type *, 'Processed ', n_events, ' events.'
	type *, 'Event errs: ', n_err
	stop
	end
