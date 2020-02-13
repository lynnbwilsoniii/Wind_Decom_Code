	subroutine makefile(ch)
c
C	A PROGRAM TO FIND FFT RAW
C
C		1996 MAY 19 TO AUG 12   =   5252  to  5336
C		1997 JAN 12 TO JUL 4    =   5490  to  5663
C		1997 NOV 2  TO FEB 20   =   5784  to  5893
C
	integer*4 ch,ok,okt,OK1,OK2,SCETI4(2)	
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	REAL*8 SCET8,RGSE
	character*32 ITEM
	character*4 event
	DATA NERR /0/
	DATA RE /6.378E3/
	DATA TWOPI /6.2831853/
C	data event /'TDSF'/
C	data event /'FILL'/
	data event /'HK'/
C
C
	OKT = W_MESSAGES_OFF(ch)
C
 100	ok = w_event(ch,event)
C
C	CHECK FOR END OF RECORD
C
	if (ok.ne.1) then
		write(6,*) 'cannot open ',event, ', ok=', ok
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		RETURN
        ELSE
	   	NERR = 0
	endif
C
C
	ITEM = 'EVENT_SCET'
	ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
C	CHECK THAT IT IS NOT LEFT OVER FROM PREVIOUS DAY
	IF(SCETI4(2).GT.120000) GO TO 100
C
	   ok = w_item_i4(ch, 'PACKET_SUBTYPE', iraw, 1, return_size)
C	print*,'packet subtype, iraw =', iraw
	   if(iraw.ne.1) iraw = 0
C
	ITEM = 'WIND_ORBIT_X(GSE)_R8'
	ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	XGSE = RGSE/RE
	ITEM = 'WIND_ORBIT_Y(GSE)_R8'
	ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	YGSE = RGSE/RE
	ITEM = 'WIND_ORBIT_Z(GSE)_R8'
	ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	ZGSE = RGSE/RE
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	RRE = SQRT(XGSE**2 + YGSE**2 + ZGSE**2)
C
	IF(IRAW.NE.0) 
     1		WRITE(78,1078) SCET8,SCETI4,XGSE,YGSE,ZGSE,RRE,IRAW
 1078	FORMAT(F8.0,I10,I8,4F8.2,I4)
C
C	DO POINTS EVERY 6 HOURS IF WIND IS CLOSER THAN 25. RE
C
	 DO NNT = 1,3
	  SCET8 = SCET8 + .25D00
	  call w_channel_position(ch,scet8)
 	  ok = w_event(ch,event)
C
	   ok = w_item_i4(ch, 'PACKET_SUBTYPE', iraw, 1, return_size)
C	   print*,'packet subtype, iraw =', iraw
	   if(iraw.ne.1) iraw = 0
C
	  ITEM = 'WIND_ORBIT_X(GSE)_R8'
	  ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	  XGSE = RGSE/RE
	  ITEM = 'WIND_ORBIT_Y(GSE)_R8'
	  ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	  YGSE = RGSE/RE
	  ITEM = 'WIND_ORBIT_Z(GSE)_R8'
	  ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	  ZGSE = RGSE/RE
C	  ITEM = 'EVENT_SCET_R8'
C	  OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	  ITEM = 'EVENT_SCET'
	  ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
	  RRE = SQRT(XGSE**2 + YGSE**2 + ZGSE**2)
C
	IF(IRAW.NE.0) 
     1	  WRITE(78,1078) SCET8,SCETI4,XGSE,YGSE,ZGSE,RRE,IRAW
C
	 ENDDO
C
C
	ok = w_channel_close(ch)
	return
	end
