	DO 200 LOCATE = 1,3
C
	NEVENT=0
	event = 'FILL'
	if(locate.eq.2) event='TDSF'
	if(locate.eq.3) event='TDSS'
	SCET = 0.
	call w_channel_position(TDSCH,SCET)
	print*,' file starts at scet',SCET
	scetstr = scet
	dd = SCET

	ok = wind_tm_get_mfmf(TDSCH,major,minor)
	if (.not. ok) stop 'cannot get Mfmf stream position'
	print*,' after mfmf, at Mfmf=',major,minor
c
	get_tm_stream = 1
c
        ok = w_event(TDSCH,EVENT)
	print*,'for first event,OK=',ok
c
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
	   IF(TDS_CHANNEL.LE.2) THEN
	      ISPS = IFSPS
	      WRITE(PTITLE(2),1004) FSPS(IFSPS+1)
	      SPS = 1000.*FSPS(IFSPS+1)
	      WRITE(PTITLE(5),1008) FFILTER(IFILF+1)
	   ELSE
	      ISPS = ISSPS
	      WRITE(PTITLE(8),1008) SSPS(ISSPS+1)
	      SPS = SSPS(ISSPS+1)
	      WRITE(PTITLE(11),1008) SFILTER(IFILS+1)
	   ENDIF
	   call w_ur8_to_ydoy(scet,yyyy,doy,msday)
 1012	   FORMAT(I10)
C
 1004	   FORMAT(F7.2)
 1008	   FORMAT(F7.0)
C
	type*,'going to get first desired event'
	go to 110
c
C
C	GET NEXT EVENT
C
 100    continue

!	 this is the main program loop
C
	ok = w_event(TDSCH,EVENT)
	NEVENT = NEVENT+1
	   if (ok.ne.1) then
	      if(ok.eq.82) go to 200
	      ok = wind_tm_get_mfmf(TDSCH,major,minor)
	      type *, char(7), '***** not OK missing packet in event ****'
	   end if
	  ok = wind_tm_get_mfmf(TDSCH,major,minor)
	  if (.not. ok) stop 'cannot get Mfmf stream position'
C
 110	  CONTINUE


C
	ITEM = 'DATA'
	OK = WIND_TM_GET_ITEM(TDSCH,ITEM,NDATA,SIZE,RETURN_SIZE)
	IF(.NOT.OK) TYPE*,'CANNOT GET ITEM = DATA'
	CALL GETSTATUS(TDSCH,MAJOR,MINOR,STATOK)
	IF(IFILF0.LT.0) IFILF0=IFILF
	IF(IFILS0.LT.0) IFILS0=IFILS
	ITEM = 'CHANNEL'
	OK = W_ITEM_I4(TDSCH,ITEM,TDS_CHANNEL,1,RETURN_SIZE)
	item = 'EVENT_NUMBER'
	ok = w_item_I4(tdsch, item, itemp, 1, return_size)
C	PRINT*,'CHANNEL',TDS_CHANNEL
C	  PRINT*,'DATA, SIZE=',RETURN_SIZE
C	  PRINT 222, (NDATA(J),J=1,2048)
 222	FORMAT(10I6)
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(TDSCH,ITEM,SCET,1,RETURN_SIZE)
	item = 'DPU_CLOCK'
	OK = W_ITEM_I4(TDSCH,ITEM,DPUCLK,1,RETURN_SIZE)
c******		temporary fix for bad first event
	if(dd.eq.0) dd = scet
C
	DO IK = 1,2048
	  NDATA(IK) = NDATA(IK)-128
	ENDDO
C
 20	CONTINUE




C*********
	     item = 'EVENT_SCET'
	     ok = w_item_i4(TDSCH, item,s_scet, 2, return_size)
C	     type*,'s_scet',s_scet 
	     SS = MOD(S_SCET(2),100)
	     MM = S_SCET(2)/100
	     MM = MOD(MM,100)
	     HH = S_SCET(2)/10000
C
	IF(LOCATE.EQ.1) THEN			!FILL
	  IF(TDS_CHANNEL.EQ.1.AND.ICHPA(1).EQ.0) THEN      ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
c            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.2.AND.ICHPA(2).EQ.0) THEN       ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
c            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.3.AND.ICHPA(3).EQ.0) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
c	TYPE*,'PT NO, TIME',NPTL,HRLO(NPTL) 
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.1) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	  ENDIF
	  IF(TDS_CHANNEL.EQ.5.AND.ICHPA(5).EQ.0) THEN       !  BY
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.0) THEN       !  BX
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	ENDIF	
C
	IF(LOCATE.EQ.2) THEN			! TDSF
	  IF(TDS_CHANNEL.EQ.1.AND.ICHPA(1).EQ.0) THEN      ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
	    EVNO(NPTH,1) = ITEMP
C            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.2.AND.ICHPA(2).EQ.0) THEN       ! EXAC
	    NPTH = NPTH + 1
	    TDSHI(NPTH) = MAXTM
	    FRQHI(NPTH) = AVRFREQ
 	    EVNO(NPTH,1) = ITEMP
C            HRHI(NPTH) = FLOAT(HH) + MM/60. + SS/3600.
            HRHI(NPTH) = (scet-dd)*24.
	    DPUHI(NPTH) = DPUCLK
	    SPSHI(NPTH) = FSPS(IFSPS+1)
	    GO TO 100
	  ENDIF
	ENDIF
C
	IF(LOCATE.EQ.3) THEN			! TDSS
	  IF(TDS_CHANNEL.EQ.3.AND.ICHPA(3).EQ.0) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
 	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.1) THEN       ! EXDC
	    NPTL = NPTL + 1
	    TDSLO(NPTL) = MAXTM
	    FRQLO(NPTL) = AVRFREQ
	    EVNO(NPTL,2) = ITEMP
C            HRLO(NPTL) = FLOAT(HH) + MM/60. + SS/3600.
            HRLO(NPTL) = (scet-dd)*24.
	    DPULO(NPTL) = DPUCLK
	    SPSLO(NPTL) = SSPS(ISSPS+1)
	  ENDIF
	  IF(TDS_CHANNEL.EQ.5.AND.ICHPA(5).EQ.0) THEN       !  BY
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
C            HRB(NPTB) = FLOAT(HH) + MM/60. + SS/3600.
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	  IF(TDS_CHANNEL.EQ.4.AND.ICHPA(4).EQ.0) THEN       !  BX
	    NPTB = NPTB + 1
	    TDSB(NPTB) = MAXTM
	    FRQB(NPTB) = AVRFREQ
	    EVNO(NPTB,3) = ITEMP
            HRB(NPTB) = (scet-dd)*24.
	    GO TO 100
	  ENDIF
	ENDIF	
C
 1003	FORMAT(3(I9,E11.3,I5))
C
	if(npth.lt.1900) go to 100
	if(LOCATE.EQ.3.AND.nptL.lt.1900) go to 100
C
	PRINT*,NEVENT,' ',EVENT,' EVENTS FOUND'
 200	CONTINUE          ! END OF ILOCAT LOOP
C

