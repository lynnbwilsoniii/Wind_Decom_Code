	subroutine makefile(ch)
	integer*4 ch,ok,SCETI4(2)
	INTEGER*4 IONOFF,X_APC,Y_APC,Z_APC,APC_DAC
	INTEGER*4 X_RESISTOR,Y_RESISTOR,Z_RESISTOR
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT
	character*32 ITEM
	character*4 event
	data event /'HK'/
	data IOLD /-1/
C
 100	ok = w_event(ch,event)
	if (ok.ne.1) then
		if(ok.eq.82) then
		   ok = w_channel_close(ch)
		   return
	        endif
		write(6,*) 'cannot open ',event, ', ok=', ok
	endif
C
	ICHANGE = 0
	   item = 'EVENT_SCET'
	   ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
C
	   item = 'APC_X'		! word 69
	   ok = W_ITEM_I4(ch, item, x_apc, 1, ret_size)
	   ICHANGE = 4*ICHANGE + X_APC
C
	   item = 'APC_Y'		! word 69
	   ok = W_ITEM_I4(ch, item, y_apc, 1, ret_size)
	   ICHANGE = 4*ICHANGE + Y_APC
C
	   item = 'APC_Z'		! word 74
	   ok = W_ITEM_I4(ch, item, z_apc, 1, ret_size)
	   ICHANGE = 4*ICHANGE + Z_APC

	   item = 'APC_DAC'		! word 70
	   ok = W_ITEM_I4(ch, item, apc_dac, 1, ret_size)
	   drivev = apccal(apc_dac)
	   ICHANGE = 4*ICHANGE + APC_DAC

	   item = 'APC_X_RESISTOR'		! word 69
	   ok = W_ITEM_I4(ch, item, x_resistor, 1, ret_size)
	   if (ok) then
		irtm = x_resistor
		irx = rassign(irtm)
	   endif
	   ICHANGE = 4*ICHANGE + IRX
	   item = 'APC_Y_RESISTOR'		! word 69
	   ok = W_ITEM_I4(ch, item, y_resistor, 1, ret_size)
	   if (ok) then
		irtm = y_resistor
		iry = rassign(irtm)
	   endif
	   ICHANGE = 4*ICHANGE + IRY
C
	   item = 'APC_Z_RESISTOR'		! word ??
	   ok = W_ITEM_I4(ch, item, z_resistor, 1, ret_size)
	   if (ok) then
		irtm = z_resistor
		irz = rassign(irtm)
	   endif
	   ICHANGE = 4*ICHANGE + IRZ


C	   item = 'APM_X_PEAK'		! word 63
C	   ok = W_ITEM_I4(ch, item, x_peak, 1, ret_size)
C	   if (ok) then
C		is = MIN0(count(ir,1)+1,40)
C		count(ir,1) = is
C		pk(ir,1,is) = apmcal(1,x_peak(ix))
C		drive(ir,1,is) = drivev
C	   endif

C	   item = 'APM_X_DC'		! word 65
C	   ok = W_ITEM_I4(ch, item, x_dc(ix), 1, ret_size)
C	   if (ok) then
C		av(ir,1,is) = apmcal(3,x_dc(ix))
C	   endif

C	   item = 'APM_Y_PEAK'		! word 64
C	   ok = W_ITEM_I4(ch, item, y_peak(ix), 1, ret_size)
C	   if (ok) then
C		is = MIN0(count(ir,2)+1,40)
C		count(ir,2) = is
C		pk(ir,2,is) = apmcal(2,y_peak(ix))
C		drive(ir,2,is) = drivev
C	   endif

C	   item = 'APM_Y_DC'		! word 66
C	   ok = W_ITEM_I4(ch, item, y_dc(ix), 1, ret_size)
C	   if (ok) then
C		av(ir,2,is) = apmcal(4,y_dc(ix))
C
C	   endif

C	   item = 'APM_Z_DC'		! word 67
C	   ok = W_ITEM_I4(ch, item, z_dc(ix), 1, ret_size)
C	   if (ok) then
C		is = MIN0(count(ir,3)+1,40)
C		count(ir,3) = is
C		av(ir,3,is) = apmcal(5,z_dc(ix))
C		drive(ir,3,is) = drivev
C	   endif
	IF(ICHANGE.NE.IOLD) THEN
	
C	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

	  WRITE(88,1011) sceti4,APC_DAC,IRX,IRY,IRZ,
     1     x_apc,y_apc,z_apc,DRIVEV
	  print 1011,sceti4,APC_DAC,IRX,IRY,IRZ,x_apc,y_apc,z_apc
     1		,DRIVEV
	ENDIF
	  IOLD = ICHANGE
 1011	format(1X,2i8,i6,6i4,F7.2)
	IF(OK.NE.82) GO TO 100
	return
	end

