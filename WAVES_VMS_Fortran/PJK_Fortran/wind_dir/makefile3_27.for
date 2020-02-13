	subroutine makefile(ch)
C
C	LOOKS FOR CAL ON TDSS, WITH 3 E'S MODE
C		TO SPEED THINGS UP, ONLY LOOKS IN FIRST 3 HOURS OF DAY
C
	integer*4 ch,ok,okt,SCETI4(2)
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,EVENT_NO
	REAL*8 SCET8
	REAL SSPS
	character*32 ITEM
	character*4 event
	data event /'TDSS'/
C	data event /'FILL'/
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
	item = 'EVENT_SCET_R8'
	okt = W_ITEM_R8(ch, item, scet8, 1, ret_size)
	NDAY = SCET8
c	TYPE*,'START',SCET8,NDAY,SCET8-DFLOAT(NDAY)
C	ONLY DO FIRST 3 HOURS BUT GUARD AGAINST PREVIOUS DAY'S DATA
	IF(SCET8 - DFLOAT(NDAY).GT..125.AND.SCET8-DFLOAT(NDAY).LT..9) THEN
		ok = w_channel_close(ch)
		RETURN
	ENDIF
C	TYPE*,'EARLY ENOUGH',SCET8-DFLOAT(NDAY)
C
C	CHECK FOR THREE E"S MODE
C
	ICOUNT = 0
	   item = 'SOURCE_CHAN_3'
	   okt = W_ITEM_i4(ch, item, ISRC, 1, return_size)
	   if(isrc.eq.1) icount = icount + 1
	   item = 'SOURCE_CHAN_4'
	   okt = W_ITEM_i4(ch, item, ISRC, 1, return_size)
	   if(isrc.eq.1) icount = icount + 1
	   item = 'SOURCE_CHAN_5'
	   okt = W_ITEM_i4(ch, item, ISRC, 1, return_size)
	   if(isrc.eq.1) icount = icount + 1
	   item = 'SOURCE_CHAN_6'
	   okt = W_ITEM_i4(ch, item, ISRC, 1, return_size)
	   if(isrc.eq.1) icount = icount + 1
C	  print*,'GOT TO TIME, ICOUNT',sceti4,ICOUNT
	   if(icount.lt.3) GO TO 100
C
C	CHECK THAT EVENT TIME IS CLOSE TO 0106 
C		= .045833 HOURS 
C
	IF(DABS(SCET8 - DFLOAT(NDAY)-.4583D-1).GT..35D-2) GO TO 100
C
	   item = 'EVENT_SCET'
	   okt = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	   item = 'EVENT_NUMBER'
	   okt = W_ITEM_i4(ch, item, EVENT_NO, 1, return_size)
	   item = 'RX_SPEED_R4'
	   okt = W_ITEM_R4(ch, item, ssps, 1, return_size)
c	   type*,'slow rx speed',ssps

	  WRITE(88,1011) sceti4,EVENT_NO,SSPS
	  print 1011,sceti4,EVENT_NO,Ssps

 1011	format(1X,I10,I8.6,I12,F8.0)
C
C	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)
	IF(OK.NE.82) GO TO 100
	ok = w_channel_close(ch)
	return
	end

