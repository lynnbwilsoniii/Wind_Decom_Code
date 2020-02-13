	subroutine makefile(ch)
c
C	MAKEFILE30, A PROGRAM TO MAKE A TABLE OF TDS SOURCES
C
	integer*4 ch,ok,okt,OK1,OK2,SCETI4(2),ISORC(10),ISPDSV(3)	
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	REAL*8 SCET8,RGSE
	character*32 ITEM
	character*4 event
	character*4 IRX(9),IRXPR(10)
	CHARACTER*2 ISPDPR(2)
	DATA NERR /0/
	DATA RE /6.378E3/
	DATA TWOPI /6.2831853/
	DATA ISORC /10*-1/
	DATA IRX /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1    ' BX ',' BY ',' BZ '/
	DATA ISPDPR /2*'  '/
	DATA ISPDF,ISPDS /1,3/
C
C
	OKT = W_MESSAGES_OFF(ch)
C
 100	CONTINUE
	ICHG = 0
	ISPDPR(1) = '  '
	ISPDPR(2) = '  '
	EVENT = 'TDSS'
	DO NEVENT = 1,4
	  ok = w_event(ch,event)
C
C	  CHECK FOR END OF RECORD
C
	  if (ok.ne.1) then
		if(ok.eq.82) GO TO 200
		write(6,*) 'cannot open ',event, ', ok=', ok
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		RETURN
          ELSE
	   	NERR = 0
	  endif
C
	  item = 'EVENT_SCET'
	  ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	  ITEM = 'CHANNEL'
	  ok = W_ITEM_I4(ch, item, ICH, 1, ret_size)
	  if(ok.ne.1) then
	    print*,'could not get TDSS channel, ok=',ok
	    go to 100
	  endif
	  ITEM = 'SOURCE'
	  ok = W_ITEM_I4(ch, item, ISOURCE, 1, ret_size)
	  ITEM = 'RX_SPEED'
	  ok = W_ITEM_I4(ch, item, ISPDS, 1, ret_size)
	  IRXPR(ICH) = IRX(ISOURCE)
	  IF(ISOURCE.NE.ISORC(ICH)) ICHG = 1
	  ISORC(ICH) = ISOURCE
	  IF(ISPDS.NE.ISPDSV(2).AND.ICHG.EQ.0) THEN
	    ICHG = 1
	    item = 'EVENT_SCET'
	    ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	    print 1077,SCETI4,ISPDF,(IRXPR(J),J=1,2),ISPDS
     1      ,(IRXPR(J),J=3,6) 
	    WRITE(77,1077) SCETI4,ISPDF,(IRXPR(J),J=1,2),ISPDS
     1	    ,(IRXPR(J),J=3,6)
	  ENDIF
	  ISPDSV(2) = ISPDS
	ENDDO
c
	EVENT = 'TDSF'
	DO NEVENT = 1,2
	  ok = w_event(ch,event)
C
C	  CHECK FOR END OF RECORD
C
	  if (ok.ne.1) then
		if(ok.eq.82) GO TO 200
		write(6,*) 'cannot open ',event, ', ok=', ok
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		RETURN
          ELSE
	   	NERR = 0
	  endif
C
	  ITEM = 'CHANNEL'
	  ok = W_ITEM_I4(ch, item, ICH, 1, ret_size)
	  if(ok.ne.1) then
	    print*,'could not get TDSF channel, ok=',ok
	    go to 100
	  endif
	  ITEM = 'SOURCE'
	  ok = W_ITEM_I4(ch, item, ISOURCE, 1, ret_size)
	  ITEM = 'RX_SPEED'
	  ok = W_ITEM_I4(ch, item, ISPDF, 1, ret_size)
C         print*,'channel,source ',ich,isource
	  IF(ISPDF.NE.ISPDSV(1).AND.ICHG.EQ.0) THEN
	     ICHG = 1
	     item = 'EVENT_SCET'
	     ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	    print 1077,SCETI4,ISPDF,(IRXPR(J),J=1,2),ISPDS
     1      ,(IRXPR(J),J=3,6) 
	    WRITE(77,1077) SCETI4,ISPDF,(IRXPR(J),J=1,2),ISPDS
     1	    ,(IRXPR(J),J=3,6)
	  ENDIF
	  ISPDSV(1) = ISPDF
	  IRXPR(ICH) = IRX(ISOURCE)
	  IF(ISOURCE.NE.ISORC(ICH)) ICHG = 1
	  ISORC(ICH) = ISOURCE
	ENDDO
C
	IF(ICHG.NE.0) THEN
 1077	  FORMAT(I10,I7,2X,I4,2A5,4X,I2,X,4A5)
	ENDIF
C
	GO TO 100
C
 200	CONTINUE
	ok = w_channel_close(ch)
	return
	end
