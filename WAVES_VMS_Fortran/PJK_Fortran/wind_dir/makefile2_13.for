	subroutine makefile(ch)
	integer*4 ch,ok,okt,SCETI4(2)
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT
	REAL*8 SCET8
	REAL DBSPEC(1024),VDATA(1024)
	character*32 ITEM
	character*4 event
	data event /'FFTM'/
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
	ITEM = 'SOURCE'
	okt = W_ITEM_I4(ch, item, ISRC, 1, ret_size)
	IF(ISRC.NE.5) GO TO 100				! EYDC
C
	call fft_phys(ch,iraw,vdata,dbspec)
C
	IF(IRAW.EQ.0) THEN
	   item = 'EVENT_SCET_R8'
	   ok = W_ITEM_R8(ch, item, scet8, 1, ret_size)
	   call w_channel_position(ch,scet8+.041667)
	   nday = scet8
	   IF(OK.NE.82) GO TO 100
	ELSE
	   POWERLO = 0.
	   POWERHI = 0.
	   COUNT = 0.
	   DO N = 2,80
		POWERLO = POWERLO + DBSPEC(N)
		COUNT = COUNT + 1.
	   ENDDO
	   POWERLO = POWERLO/COUNT
	   COUNT = 0.
	   DO N = 81,1023
		POWERHI = POWERHI + DBSPEC(N)
		COUNT = COUNT + 1.
	   ENDDO
	   POWERHI = POWERHI/COUNT
	ENDIF
C
	   item = 'EVENT_SCET'
	   ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
C
	IF(POWERHI.GT.-1000.) THEN
	
C	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

	  WRITE(88,1011) sceti4,POWERLO,POWERHI
	  print 1011,sceti4,POWERLO,POWERHI
	ENDIF
 1011	format(1X,I10,I8.6,2F10.3)
	IF(OK.NE.82) GO TO 100
	return
	end

