	subroutine makefile(ch)
c
c	copied from makefile2, to try to find ion acoustic waves
c		in fftm data.  I only want periods of raw data.
c
	integer*4 ch,ok,okt,OK2,SCETI4(2),NDATA(1025)
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	REAL*8 SCET8
	REAL DBSPEC(1024),VDATA(1024),POWERLO(10),POWERHI(10)
	REAL WSPEC(1024),SPECSAVE(10,2)
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
c	OKT = W_MESSAGES_OFF(ch)
C
C	SOME CHECKING STUFF
C	ITEM = 'EX_LENGTH_EFF'
C	okt = W_ITEM_R4(ch, item, EFFLEN, 1, ret_size)
C	PRINT*,'X LEN',EFFLEN
C	ITEM = 'EY_LENGTH_EFF'
C	okt = W_ITEM_R4(ch, item, EFFLEN, 1, ret_size)
C	PRINT*,'Y LEN',EFFLEN
C	ITEM = 'EZ_LENGTH_EFF'
C	okt = W_ITEM_R4(ch, item, EFFLEN, 1, ret_size)
C	PRINT*,'Z LEN',EFFLEN
C
c	ITEM = 'DATA'
c	ok2 = W_ITEM_I4(ch, item, NDATA, 1024, ret_size)
c
        call w_item_i4(ch,'PACKET_SUBTYPE',my_subtype,1,n)
c        if (my_subtype .eq. 1) then
c            raw = .true.
c        else
c            raw = .false.
c        endif
C
	IF(my_subtype.EQ.1) THEN
	  IRAW = 1
	ELSE
	  IRAW = 0
	ENDIF
C
	ITEM = 'SOURCE'
	okt = W_ITEM_I4(ch, item, ISRC, 1, ret_size)
	ITEM = 'SPECTRUM_DB'
	ok2 = W_ITEM_R4(ch, item, WSPEC, 513, ret_size)
C	print*,'WSPEC,size,1st vals',ret_size,wspec(2),wspec(3),wspec(30)
c	IF(ISRC.LT.4.OR.ISRC.GT.7) GO TO 100		! SHOULD NOT HAPPEN
	ITEM = 'CHANNEL_NUMBER'
	okt = W_ITEM_I4(ch, item, ICHNNL, 1, ret_size)
C
C	call fft_phys(ch,iraw,vdata,dbspec)
C	print*,'dbSPEC,iraw,1st vals',iraw,dbspec(2),dbspec(3),dbspec(30)
C
C	print*,'source,channel,iraw',isrc,ichnnl,iraw
C
	IF(IRAW.EQ.0) THEN
C	   skip ahead one hour
	   print*,'skip ahead one hour',SCETI4
	   item = 'EVENT_SCET_R8'
	   ok = W_ITEM_R8(ch, item, scet8, 1, ret_size)
	   call w_channel_position(ch,scet8+.041667)
	   nday = scet8
	   IF(OK.NE.82) GO TO 100
	ELSE
	   POWERLO(ISRC) = 0.
	   POWERHI(ISRC) = 0.
	   COUNT = 0.
	   DO N = 2,20
C		POWERLO(ISRC) = POWERLO(ISRC) + DBSPEC(N)
		POWERLO(ISRC) = POWERLO(ISRC) + WSPEC(N)
		COUNT = COUNT + 1.
	   ENDDO
	   POWERLO(ISRC) = POWERLO(ISRC)/COUNT
	   COUNT = 0.
	   DO N = 21,512
		POWERHI(ISRC) = POWERHI(ISRC) + DBSPEC(N)
		POWERHI(ISRC) = POWERHI(ISRC) + WSPEC(N)
		COUNT = COUNT + 1.
	   ENDDO
	   POWERHI(ISRC) = POWERHI(ISRC)/COUNT
	   SPECSAVE(ISRC,1) = WSPEC(2)
	   SPECSAVE(ISRC,2) = WSPEC(3)
	ENDIF
C
	   item = 'EVENT_SCET'
	   ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
C
	IF(ICHNNL.EQ.6) THEN
C	IF(POWERHI.GT.-1000.) THEN
	
C	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

	  WRITE(88,1011) sceti4,(POWERLO(J),POWERHI(J),J=4,6)
	  WRITE(90,1011) sceti4,(SPECSAVE(J,1),SPECSAVE(J,2),J=4,6)
	  IF(POWERHI(4).GT.-136..AND.POWERHI(5).GT.-114.) THEN
	    print 1011,sceti4,(POWERLO(J),POWERHI(J),J=4,6)
	    WRITE(91,1011) sceti4,(POWERLO(J),POWERHI(J),J=4,6)
	  ENDIF
	ENDIF
 1011	format(1X,I10,I8.6,6F10.3)
	IF(OK.NE.82) GO TO 100
	return
	end

