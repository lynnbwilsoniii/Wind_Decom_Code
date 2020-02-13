	program gmecoords
c
C	A PROGRAM TO WRITE ORBIT PARAMETERS IN GSM, SAME AS MAKEFILE19
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
	INTEGER*4 W_CHANNEL_OPEN,GET_STREAM_NAME
	INTEGER*4 YYYY,MON,DD,HH,MM,TMCHANNEL
	REAL*8 SCET8,RGSM,SCETSTR,SCETEND
	character*32 ITEM
        character*80    stream 
	character*4 event
	DATA NERR /0/
	DATA RE /6.378E3/
	DATA TWOPI /6.2831853/
C	data event /'TDSF'/
C	data event /'FILL'/
	data event /'HK'/
C
C
        READ(5,*) SCETSTR,SCETEND
        SCET8 = SCETSTR
        PRINT*,'DAYS',SCET8,SCETEND
c       OPEN(UNIT=90,FILE='MAKEFILE5.RESULTS',STATUS='OLD',ACCESS='OLD')
        call w_ur8_to_ymd(scet8,yyyy,mon,dd,hh,mm,ss,ms)
        PRINT*,'YMD',YYYY,MON,DD,HH,MM
        WRITE(stream,30) YYYY,MON,DD
 30     FORMAT(I4.4,I2.2,I2.2)
        OK = GET_STREAM_NAME(stream)
C
C       call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
C
       print*,'in main, after get_stream_name, stream=',stream
C
        ok = w_channel_open(tmchannel,stream)
C
       print*,'open, TMCHANNEL,ok=',TMCHANNEL,ok
        if (ok.ne.1) stop 'Cannot open t/m channel'
c
        scet = 0.
        call w_channel_position(tmchannel,scet)
        print*,'t/m file starts at scet',scet
        dds = scet
C
        ok = w_channel_open(tmchannel,stream)
C
C       print*,'open, TMCHANNEL,ok=',TMCHANNEL,ok
        if (ok.ne.1) stop 'Cannot open t/m channel'
	CH = TMCHANNEL
c
        scet = 0.
        call w_channel_position(tmchannel,scet)
        print*,'t/m file starts at scet',scet
        dds = scet
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
		STOP
        ELSE
	   	NERR = 0
	endif
C
C
	ITEM = 'EVENT_SCET'
	ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
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
	PRINT*,'SCETI4 ',SCETI4(1)
	PRINT*,'SCETI4 ',SCETI4(2)
	WRITE(77,1077) SCET8,SCETI4(1),SCETI4(2),XGSM,YGSM,ZGSM,RRE
 1077	FORMAT(F11.3,I10,I8,4F9.3)
C
C	ENDDO
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
	IF(SCET8.LT.SCETEND) GO TO 100
C
	ok = w_channel_close(ch)
	STOP
	end
        options/extend_source
!------------------------------------------------------------------------------
        integer*4       function        get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
        implicit        none
        character*(*)   stream
        CHARACTER*80    YYYYMMDD
        integer*4       iq
        DATA IQ /1/
C
           YYYYMMDD = STREAM(1:8)
           WRITE(STREAM,30) YYYYMMDD
 30        FORMAT('wi_lz_wav_',A8,'_v*.dat')
           PRINT*,'in get_stream_name, file= ',STREAM
c       end if

        get_stream_name = 1

 20     return
c
  5     format(q,a)
  7     format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
        end
