	subroutine makefile(ch)
c
C	A PROGRAM TO MAKE A TABLE OF FFT SOURCES
C
	integer*4 ch,ok,okt,OK1,OK2,SCETI4(2),ISORC(10),IRAWSV(3)	
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	REAL*8 SCET8,RGSE
	character*32 ITEM
	character*4 event
	character*4 IRX(9),IRXPR(10)
	CHARACTER*2 IRAWPR(3)
	DATA NERR /0/
	DATA RE /6.378E3/
	DATA TWOPI /6.2831853/
	DATA ISORC /10*-1/
	DATA IRX /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1    ' BX ',' BY ',' BZ '/
	DATA IRAWPR /3*'  '/
C
C
	OKT = W_MESSAGES_OFF(ch)
C
 100	CONTINUE
	ICHG = 0
	IRAWPR(1) = '  '
	IRAWPR(2) = '  '
	IRAWPR(3) = '  '
	EVENT = 'FFTH'
	DO NEVENT = 1,4
	  ok = w_event(ch,event)
C
C	  CHECK FOR END OF RECORD
C
	  if (ok.ne.1) then
		if(ok.eq.82) then
		  write(6,*) 'cannot open ',event, ', ok=', ok
		  ok = w_channel_close(ch)
		  return
	        endif
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		ok = w_channel_close(ch)
		RETURN
          ELSE
	   	NERR = 0
	  endif
C
	  item = 'EVENT_SCET'
	  ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	  ITEM = 'CHANNEL_NUMBER'
	  ok = W_ITEM_I4(ch, item, ICH, 1, ret_size)
	  ITEM = 'SOURCE'
	  ok = W_ITEM_I4(ch, item, ISOURCE, 1, ret_size)
	  IF(ISOURCE.NE.0) IRXPR(ICH) = IRX(ISOURCE)
	  IF(ISOURCE.NE.ISORC(ICH)) ICHG = 1
	  ISORC(ICH) = ISOURCE
          ok = w_item_i4(ch, 'PACKET_SUBTYPE', iraw, 1, return_size)
C       print*,'packet subtype, iraw =', iraw
          if(iraw.ne.1) iraw = 0
	  IF(IRAW.NE.IRAWSV(1).AND.ICHG.EQ.0) THEN
	    ICHG = 1
	    item = 'EVENT_SCET'
	    ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	    WRITE(77,1077) SCETI4,IRAWPR(1),(IRXPR(J),J=1,2),IRAWPR(2),
     1	    (IRXPR(J),J=3,6),IRAWPR(3),(IRXPR(J),J=7,10)
	  ENDIF
	  IRAWSV(1) = IRAW
	  IF(IRAW.NE.0) IRAWPR(1) = ' R'
	ENDDO
c
	EVENT = 'FFTM'
	DO NEVENT = 1,8
	  ok = w_event(ch,event)
C
C	  CHECK FOR END OF RECORD
C
	  if (ok.ne.1) then
		if(ok.eq.82) then
		  write(6,*) 'cannot open ',event, ', ok=', ok
	  	  ok = w_channel_close(ch)
		  return
		endif
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		ok = w_channel_close(ch)
		RETURN
          ELSE
	   	NERR = 0
	  endif
C
	  ITEM = 'CHANNEL_NUMBER'
	  ok = W_ITEM_I4(ch, item, ICH, 1, ret_size)
	  ITEM = 'SOURCE'
	  ok = W_ITEM_I4(ch, item, ISOURCE, 1, ret_size)
C         print*,'channel,source ',ich,isource
          ok = w_item_i4(ch, 'PACKET_SUBTYPE', iraw, 1, return_size)
C       print*,'packet subtype, iraw =', iraw
          if(iraw.ne.1) iraw = 0
	  IF(IRAW.NE.IRAWSV(2).AND.ICHG.EQ.0) THEN
	     ICHG = 1
	     item = 'EVENT_SCET'
	     ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	     WRITE(77,1077) SCETI4,IRAWPR(1),(IRXPR(J),J=1,2),IRAWPR(2),
     1	     (IRXPR(J),J=3,6),IRAWPR(3),(IRXPR(J),J=7,10)
	  ENDIF
	  IRAWSV(2) = IRAW
	  IF(IRAW.NE.0) IRAWPR(2) = ' R'
	  IF(ISOURCE.NE.0) IRXPR(ICH) = IRX(ISOURCE)
	  IF(ISOURCE.NE.ISORC(ICH)) ICHG = 1
	  ISORC(ICH) = ISOURCE
	ENDDO
C
	EVENT = 'FFTL'
	DO NEVENT = 1,8
	  ok = w_event(ch,event)
C
C	  CHECK FOR END OF RECORD
C
	  if (ok.ne.1) then
		if(ok.eq.82) then
		  write(6,*) 'In makefile29, cannot open ',event, ', ok=', ok
	 	  ok = w_channel_close(ch)
		  return
		endif
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		ok = w_channel_close(ch)
		RETURN
          ELSE
	   	NERR = 0
	  endif
C
	  ITEM = 'CHANNEL_NUMBER'
	  ok1 = W_ITEM_I4(ch, item, ICH, 1, ret_size)
	  ITEM = 'SOURCE'
	  ok2 = W_ITEM_I4(ch, item, ISOURCE, 1, ret_size)
C	  print*,'channel,source ',ich,isource
	  if(ok1*ok2.ne.1) go to 100
          ok = w_item_i4(ch, 'PACKET_SUBTYPE', iraw, 1, return_size)
C       print*,'packet subtype, iraw =', iraw
          if(iraw.ne.1) iraw = 0
c	print*,'ch,ok1,ok2,ok',ch,ok1,ok2,ok
c	print*,'ich,isource,iraw',ich,isource,iraw
	  IF(ICH.LT.6.OR.ICH.GT.10) GO TO 100
	  IF(IRAW.NE.IRAWSV(3)) THEN
	    ICHG = 1
	    item = 'EVENT_SCET'
	    ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	    WRITE(77,1077) SCETI4,IRAWPR(1),(IRXPR(J),J=1,2),IRAWPR(2),
     1	    (IRXPR(J),J=3,6),IRAWPR(3),(IRXPR(J),J=7,10)
	  ENDIF
	  IRAWSV(3) = IRAW
	  IF(IRAW.NE.0) IRAWPR(3) = ' R'
	  IF(ISOURCE.NE.0) IRXPR(ICH) = IRX(ISOURCE)
	  IF(ISOURCE.NE.ISORC(ICH)) ICHG = 1
	  ISORC(ICH) = ISOURCE
	ENDDO
C
	IF(ICHG.NE.0) THEN
 1077	  FORMAT(I10,I7,2X,A2,2A5,2X,A2,4A5,2X,A2,4A5)
	ENDIF
C
	GO TO 100
C
c	ok = w_channel_close(ch)
c	return
	end
