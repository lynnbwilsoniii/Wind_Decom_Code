C TDSPRO

C Lines 122-145
	ok = w_channel_open(tdsch,stream)
	if (.not. ok) stop 'Cannot open tds channel'
	scettds = 0.
	call w_channel_position(tdsch,scettds)
	print*,'tds file starts at scettds',scettds
	dds = scettds
	scettds = float(dds) + hh/24. + mm/1440.
	print*,'set tds channel position to',scettds
	call w_channel_position(tdsch,scettds)
	print*,'tds channel position set to',scettds

	ok = w_channel_open(fillch,stream)           
	if (.not. ok) stop 'Cannot open fill channel'      
	scetfill = 0.
	call w_channel_position(fillch,scetfill)
	print*,'fill channel starts at',scetfill
	dds = scetfill
	scetfill = float(dds) + hh/24. + mm/1440.
	print*,'set fill channel position to',scetfill
	call w_channel_position(fillch,scetfill)
	print*,'fill channel position set to',scetfill
c
	call w_ur8_to_ymd(scetfill,yyyy,mon,dd,hh,mm,ss,ms)
	call w_ur8_to_ydoy(scetfill,yyyy,doy,msday)


C Lines 165-166
	item = 'EVENT_SCET_R8'
	ok = w_item_r8(fillch, item, scetfill, 1, return_size)

C Lines 218-219
	item = 'EVENT_SCET_R8'
	ok = w_item_R8(ch, item, scett, 1, return_size)



C Lines 256-293
C
C  Know w_item_i4 is wrong
C
C
	       item = 'EVENT_SCET'
	       ok = w_item_i4(ch, item, s_scet, 2, return_size)
	       ss = mod(s_scet(2),100)
	       mm = s_scet(2)/100
	       mm = mod(mm,100)
	       hh = s_scet(2)/10000
	       scett = float(dds) + hh/24. + mm/1440. + ss/86400.
c*********
	       item = 'EVENT_NUMBER'
	       ok = w_item_i4(ch, item, itemp, 1, return_size)
	       type*,'event number',itemp,'  ',event
	       if(iend.eq.2.and.itemp.ne.nevent) go to 110
	       item = 'DPU_CLOCK'
	       ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
	       WRITE(PTITLE(14),1014) DPUCLK
 1014		FORMAT(F12.3)
	       item = 'SUN_ANGLE'
	       ok = w_item_R4(ch, item, sunclock, 1, return_size)
	       item = 'SUN_ANGLE_R4'
	       ok = w_item_R4(ch, item, sunANG, 1, return_size)
c		PRINT*,'#,SUN_ANGLE, SUN_ANGLE_R4',ITEMP,SUNCLOCK,SUNANG
	       item = 'source'
	       ok = w_item_i4(ch, item,isource, 1, return_size)
C
	       IF(ISOURCE.LE.3) THEN
		 FFTMIN = 150.
	       ELSEIF(ISOURCE.LT.7) THEN
		 FFTMIN = 50.      		! A GUESS, NOT WELL FOUNDED
C                 FFTMIN = 10.                   ! A 2nd GUESS, NOT WELL FOUNDED  
	       ELSE
		 FFTMIN = 3.3
	       ENDIF
c	       if(isource.ne.9) go to 110
C
	       item = 'event_boe_r8'
	       ok = w_item_R8(ch, item, beginevt, 1, return_size)
	       item = 'event_eoe_r8'
	       ok = w_item_R8(ch, item, endevt, 1, return_size)



C
C TDSBDAT
C

C line 266-267
      call w_ur8_to_ymd(scettds,yyyy,mon,dd,hh,mm,ss,ms)
      call w_ur8_to_ydoy(scettds,yyyy,doy,msday)

C line 291-293
        item = 'EVENT_SCET'
        ok = w_item_i4(fillch, item, s_scet, 2, return_size)
        print*,'initial fillch time',s_scet

C line 362-376
             item = 'EVENT_SCET'
             ok = w_item_i4(ch, item, s_scet, 2, return_size)
             ss = mod(s_scet(2),100)
             mm = s_scet(2)/100
             mm = mod(mm,100)
             hh = s_scet(2)/10000
             scett = float(dds) + hh/24. + mm/1440. + ss/86400.
c
             item = 'EVENT_SCET_R8'
             ok = w_item_R8(ch, item, scett, 1, return_size)
             if(ch.eq.tdsch) then
                scettds = scett
             else
                scetfill = scett
             endif

C line 379-380
             item = 'EVENT_NUMBER'
             ok = w_item_i4(ch, item, itemp, 1, return_size)

C line 385-386
             item = 'DPU_CLOCK'
             ok = w_item_R4(ch, item, DPUCLK, 1, return_size)

C line 389-396
             item = 'SUN_ANGLE'
             ok = w_item_R4(ch, item, sunclock, 1, return_size)
             item = 'WIND_SPIN_RATE_R4'
             ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
             item = 'event_boe_r8'
             ok = w_item_R8(ch, item, beginevt, 1, return_size)
             item = 'event_eoe_r8'
             ok = w_item_R8(ch, item, endevt, 1, return_size)

C line 416
           call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)

C line 442-443
         item = 'EVENT_NUMBER'
         ok = wind_tm_get_item(ch, item, itemp, 1, return_size)

C Lines 469-472
       item= 'EVENT_START_SCET_R8'
       ok = w_item_R8(ch, item, real_scet, 1, return_size)
       call w_ur8_to_ymd(real_scet,ryyyy,rmon,rdd,rhh,rmm,rss,rmsec)




