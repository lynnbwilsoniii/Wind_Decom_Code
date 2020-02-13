	subroutine makefile(ch)
c
C	A PROGRAM TO 	write antenna length and sample angle
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
 100	ok = w_event(ch,'HK')
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
	ITEM = 'APM_ANGLE'
	ok = W_ITEM_R4(ch, item, ANGLE, 1, ret_size)
	print*,'ok,angle',ok,angle,ret_size
C
C	ok = w_event(ch,'ALL')
	ITEM = 'EZ_MONOPOLE_LENGTH_R4'
	ok = W_ITEM_R4(ch, item, EZL, 1, ret_size)
	ITEM = 'EX_MONOPOLE_LENGTH_R4'
	ok = W_ITEM_R4(ch, item, EXML, 1, ret_size)
        ITEM = 'EX_LENGTH'
        ok = W_ITEM_R4(ch, item, EXL, 1, ret_size) 
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
C	RRE = SQRT(XGSE**2 + YGSE**2 + ZGSE**2)
C
	WRITE(90,1077) SCET8,SCETI4,ANGLE,EZL,EXML,EXL
 1077	FORMAT(F8.0,I10,I8,4F8.2)
	PRINT 1077,SCET8,SCETI4,ANGLE,EZL,EXML,EXL
C
C
	ok = w_channel_close(ch)
	return
	end
