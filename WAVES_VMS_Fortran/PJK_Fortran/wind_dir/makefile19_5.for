	subroutine makefile(ch)
c
C	A PROGRAM TO WRITE ORBIT PARAMETERS IN GSM, SAME AS MAKEFILE13 
C		EXCEPT DOES EVERY 4 HOURS
C
C	 PERIODS WHEN WIND IS MORE THAN
C		100 RE UPSTREAM
C		1996 MAY 19 TO AUG 12   =   5252  to  5336
C		1997 JAN 12 TO JUL 4    =   5490  to  5663
C		1997 NOV 2  TO FEB 20   =   5784  to  5893
C
	integer*4 ch,ok,okt,OK1,OK2,SCETI4(2)	
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8,W_CHANNEL_POSITION
	REAL*8 SCET8,RGSM
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
	ITEM = 'WIND_ORBIT_X(GSM)_R8'
	ok = W_ITEM_R8(ch, item, RGSM, 1, ret_size)
	XGSM = RGSM/RE
	ITEM = 'WIND_ORBIT_Y(GSM)_R8'
	ok = W_ITEM_R8(ch, item, RGSM, 1, ret_size)
	YGSM = RGSM/RE
	ITEM = 'WIND_ORBIT_Z(GSM)_R8'
	ok = W_ITEM_R8(ch, item, RGSM, 1, ret_size)
	ZGSM = RGSM/RE
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	RRE = SQRT(XGSM**2 + YGSM**2 + ZGSM**2)
C
	WRITE(77,1077) SCET8,SCETI4,XGSM,YGSM,ZGSM,RRE
 1077	FORMAT(F8.0,I10,I8,4F8.2)
C
C	DO EVERY FOUR HOURS
C
	DO II = 1,5
C	IF(RRE.LT.25.) THEN
	  SCET8 = SCET8 + .166667D00
	  OK = w_channel_position(ch,scet8)
	  IF(OK.NE.1) THEN
		ok = w_channel_close(ch)
		RETURN
	  ENDIF
	  ITEM = 'WIND_ORBIT_X(GSM)_R8'
	  ok = W_ITEM_R8(ch, item, RGSM, 1, ret_size)
	  XGSM = RGSM/RE
	  ITEM = 'WIND_ORBIT_Y(GSM)_R8'
	  ok = W_ITEM_R8(ch, item, RGSM, 1, ret_size)
	  YGSM = RGSM/RE
	  ITEM = 'WIND_ORBIT_Z(GSM)_R8'
	  ok = W_ITEM_R8(ch, item, RGSM, 1, ret_size)
	  ZGSM = RGSM/RE
C	  ITEM = 'EVENT_SCET_R8'
C	  OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
	  ITEM = 'EVENT_SCET'
	  ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
	  RRE = SQRT(XGSM**2 + YGSM**2 + ZGSM**2)
C
	  WRITE(77,1077) SCET8,SCETI4,XGSM,YGSM,ZGSM,RRE
C
C	ENDIF
	ENDDO
C
C	ITEM = 'WIND_MFI_BPHI(GSE)_R4'
C	ok = W_ITEM_R4(ch, item, AZMAG, 1, ret_size)
C	ITEM = 'MAG_ELEVATION'
C	ok = W_ITEM_I4(ch, item, MAGEL, 1, ret_size)
C	ITEM = 'SUN_ANGLE'
C	ok = W_ITEM_I4(ch, item, SUNCLOCK, 1, ret_size)
C	ITEM = 'WIND_3DP_E_TEMP_R4'
C	ok = W_ITEM_R4(ch, item, TEMPE, 1, ret_size)
C	ITEM = 'WIND_3DP_ION_TEMP_R4'
C	ok = W_ITEM_R4(ch, item, TEMPI, 1, ret_size)
C	ITEM = 'WIND_3DP_ION_DENSITY_R4'
C	ok = W_ITEM_R4(ch, item, DENS, 1, ret_size)
C	ITEM = 'WIND_3DP_ION_VX(GSE)_R4'
C	ok = W_ITEM_R4(ch, item, VX, 1, ret_size)
C	ITEM = 'WIND_3DP_ION_VY(GSE)_R4'
C	ok = W_ITEM_R4(ch, item, VY, 1, ret_size)
C	ITEM = 'WIND_MFI_BMAG_R4'
C	ok = W_ITEM_R4(ch, item, BMAG, 1, ret_size)
C	ITEM = 'WIND_SPIN_RATE_R4'
C	ok = W_ITEM_R4(ch, item, SPINRATE, 1, ret_size)
C	if(ok.ne.1) then
C	  spinrate = 2.
C	endif
C
	ok = w_channel_close(ch)
	return
	end
