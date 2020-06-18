! cdfu.for - tests getting cdf items

	program		test_cdf
	implicit	none
	integer*4	w_channel_open
	integer*4	w_event
	integer*4	w_item_i4
	integer*4	w_item_r4
	integer*4	w_item_r8
	integer*4	w_version
	integer*4	w_channel_position
	integer*4	w_ur8_to_epoch
	integer*4	ok,event_err,ch
	parameter	w_eof=82
	parameter	max_sz=512
	integer*4	i4buf(max_sz)
	real*4		r4buf(max_sz)
	real*8		r8buf(max_sz)
	character*4	ev /'RAD2'/
	character*80	file
	character*16	ver
	integer*4	i, o
	integer*4	major,minor
	integer*4	wind_tm_get_mfmf
	character*24	item
	integer*4	sz
	integer*4	ret_size
	real*8		scet, epoch
	integer*4	ios
	character*1	ans
	real*8		pos, a, b
	integer*4	ipos

!	ok = w_channel_open(ch,'offline')
!	ok = w_channel_open(ch,'WIND_DATA:wi_lz_wav_19950214_v01.dat')
	ok = w_channel_open(ch,'WIND_DATA:wi_lz_wav_19951214_v*')
	if (ok .ne.1) stop 'cannot open tm channel'

	ok = w_version(ver)
	type *, 'WIND_LIB version is: ', ver

	call wind_tm_get_filename(ch,file)
	type *, 'File: ',file(:72)

	ok = w_channel_position(ch, pos)
	ipos = pos
	write(6,4,iostat=o) 'Initial Pos: ', pos

  1	format(1x,a8,1x,10(1x,i4))
  2	format(1x,a24,1x,f10.5)
  3	format(1x,'Event MF.mf/SCET:ur8,epoch: ', i5,'.',i3.3, 1x, f14.8, 1x, f17.1)
  4	format(1x,a24,1x,f14.8)
  5	format(1x,a24,1x,e12.6)
  6	format(1x,a24,1x,'[',i3,'/',i3,']', 3(f10.5,1x),'...',f10.5)

	event_err = w_event(ch,ev)
	do while(event_err .ne. w_eof)
	   i = wind_tm_get_mfmf(ch,major,minor)

	   item = 'S'
	   ok = w_item_i4(ch, item, i4buf, max_sz, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item S at MF#', major
	   else
	      write(6,1,iostat=o) item, (i4buf(i), i=1,10)
	   end if

	   item = 'EVENT_SCET_R8'
	   ok = w_item_r8(ch, item, scet, 1, ret_size)
	   if (ok .ne. 1) then
	      scet = 0.0
	      epoch = 0.0
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      ok = w_ur8_to_epoch(scet, epoch)
	      write(6,3,iostat=ios) major, minor, scet, epoch
	   end if

	   item = 'WIND_SWE_DENSITY_R4'
	   ok = w_item_r4(ch, item, r4buf, 1, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,5,iostat=o)  item, r4buf(1)
	   end if

	   item = 'WIND_SWE_THERMAL_SPD_R4'
	   ok = w_item_r4(ch, item, r4buf, 1, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,5,iostat=o)  item, r4buf(1)
	   end if

	   item = 'WIND_SWE_SCET_R8'
	   ok = w_item_r8(ch, item, r8buf, 1, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,4,iostat=o) item, r8buf(1)
	   end if

	   item = 'WIND_MFI_BX(GSE)_R4'
	   sz = 1
	   ok = w_item_r4(ch, item, r4buf, sz, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,2,iostat=o)  item, r4buf(1)
	   end if

	   item = 'WIND_MFI_BY(GSE)_R4'
	   sz = 1
	   ok = w_item_r4(ch, item, r4buf, sz, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,2,iostat=o)  item, r4buf(1)
	   end if

	   item = 'WIND_MFI_BY(GSE)_R4'
	   sz = 7
	   ok = w_item_r4(ch, item, r4buf, sz, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,6,iostat=o) item, ret_size, sz,
	1       r4buf(1), r4buf(2), r4buf(3), r4buf(ret_size)
	   end if

	   item = 'WIND_MFI_BZ(GSE)_R4'
	   sz = 18
	   ok = w_item_r4(ch, item, r4buf, sz, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,6,iostat=o) item, ret_size, sz,
	1       r4buf(1), r4buf(2), r4buf(3), r4buf(ret_size)
	   end if

	   item = 'WIND_MFI_SCET_R8'
	   ok = w_item_r8(ch, item, r8buf, 1, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,4,iostat=o) item, r8buf(1)
	   end if

	   item = 'WIND_ORBIT_SPD_R8'
	   ok = w_item_r8(ch, item, r8buf, 1, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,4,iostat=o) item, r8buf(1)
	   end if

	   item = 'WIND_ORBIT_R_R8'
	   ok = w_item_r8(ch, item, r8buf, 1, ret_size)
	   if (ok .ne. 1) then
	      type *, 'cannot get item ',item, ' at MF#', major
	   else
	      write(6,5,iostat=o) item, r8buf(1)
	   end if

  10	   continue
	   write(6,'(1x,a,$)',err=10) 'Get another event [y,n,p]? '
	   read(5,'(a)',err=10,end=99) ans
	   if (ans .eq. 'y' .or. ans .eq. 'Y') then
	      event_err = w_event(ch,ev)
	   else if (ans .eq. 'p' .or. ans .eq. 'P') then
	      write(6,'(1x,a,$)',err=10) 'Enter SCET in Ur8: '
	      read(5,*,err=10,end=99) pos
	      if (pos .lt. 1.0) pos = ipos + pos
	      ok = w_channel_position(ch,pos)
	      if (ok .eq. 1) write(6,4,iostat=o) 'New Position: ', pos
	      if (ok .ne. 1) write(6,'(1x,a,i5)',iostat=o) 'Error, ok=', ok
	      event_err = w_event(ch,ev)
	   else
	      event_err = w_eof
	   end if
	end do

 99	continue
	stop
	end
