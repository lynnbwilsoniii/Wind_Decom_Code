C
C	PLOT TDSF EVENTS
C
	TDS_COUNT = 0
	EVENT = 'TDSS'
	YMAX = 150.
	CALL MGOWINDOW(1,6,5)
C	write(67,*) 'window5',gx1,gx2,gy1,gy2
	CALL MGOSETLIM(XSTRT,0.,XEND,YMAX)
	CALL MGOBOX(1,2)
	CALL MGOYLABEL(4,'TDSS')
C
	tdsch = cdfch
	scett = 0.d00
	print*,'going to get TDS events'
	call w_channel_position(TDSCH,scett)
	call w_channel_position(TDSCH,scet1)
	print*,'TDSS channel set to',TDSCH,scet1
	PWRS = 0.
	TDSSM = 0.
 400	OK = W_EVENT(TDSCH,EVENT)
C	print*,'tds event, event, ok=  ',event,ok
	IF(OK.EQ.82) GO TO 450
	IF(OK.NE.1) GO TO 400
	ITEM = 'EVENT_NUMBER'
	ok = W_ITEM_I4(TDSCH, item, NEVT, 1, ret_size)
C	print*,'tdss event, event, ok, no =  ',event,ok,nevt
	IPRC = 4
	CALL TDS_PHYS(TDSCH,IPRC,NDATA,VDATA,SPECT)
	ITEM = 'EVENT_SCET'
	ok = W_ITEM_I4(TDSCH, item, SCETI4, 2, ret_size)
	ITEM = 'FAST_RX_SPEED'
	ok = W_ITEM_I4(cdfch, item, ISPS, 1, ret_size)
C
	ITEM = 'CHANNEL'
	ok = W_ITEM_I4(TDSCH, item, TDS_CHANNEL, 1, ret_size)
	EFL = EFFLEN(TDS_CHANNEL)  
C
	ITEM = 'EVENT_NUMBER'
	ok = W_ITEM_I4(TDSCH, item, NEVT, 1, ret_size)
C	IF(SCETT.GE.SCETTDS1.AND.SCETT.LE.SCETTDS2) print*,'tds',sceti4,nevt
	ITEM = 'FAST_RX_SPEED_R4'
	ok = W_ITEM_R4(TDSCH, item, SPS, 1, ret_size)
	ITEM = 'FAST_SAMPLER_THRESHOLD'
	ok = W_ITEM_R4(TDSCH, item, VMIN, 1, ret_size)
	NVMAX = MAX0(NDATA(1023),NDATA(1024),NDATA(1025))
	NVMIN = MIN0(NDATA(1023),NDATA(1024),NDATA(1025))
	VMAX = MAX0(NVMAX-128,128-NVMIN)
C	ITEM = 'EVENT_CENTER_SCET_R8'
	ITEM = 'EVENT_SCET_R8'
	ok = W_ITEM_R8(TDSCH, item, SCETT, 1, ret_size)
	IF(SCETT.GE.SCET1.AND.SCETT.LE.SCET2) THEN
	  TDSTIME = (SCETT - NDAY)*24.D00
	  CALL MGORELOCATE(TDSTIME,VMIN)
	  CALL MGODRAW(TDSTIME,VMAX)
C	  CALL TDSDATA(TDSCH,NDATA,SPS,AVRF,UPVSD)
	  IF(TDS_CHANNEL.EQ.1) THEN
	    AVRF1 = AVRF
	    UPVSD1 = UPVSD
	  ELSE
	    AVRF2 = AVRF
	    UPVSD2 = UPVSD
	  ENDIF
C
C	  IF(TDS_CHANNEL.EQ.1) WRITE(64,1064) NEVT,SCETI4,'T',TDS_CHANNEL,VMAX
C     1		,AVRF1,UPVSD1,UPVSD2
 1064	  FORMAT(I10,I10,I8,A2,I3,F5.0,F7.3,2F6.2)
	ENDIF
C
C	IF(SCETT.GE.SCETTDS1.AND.SCETT.LE.SCETTDS2) THEN
C	   TDS_COUNT = TDS_COUNT+1
C	   TDSC = TDS_COUNT
C	  DO N = 1,2048
C	    TDSMS = AMAX1(TDSMS,1000.*ABS(VDATA(N))/EFL)  ! in mV/m	    
C	    PWRS = PWRS + (VDATA(N)/EFL)**2
C	  ENDDO
C	ENDIF
C
	GO TO 400
C

