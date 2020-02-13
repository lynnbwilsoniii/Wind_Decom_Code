
C line 147
          ok = w_channel_open(tdsch,stream)

C line 151
          ok = w_channel_open(cdfch,stream)

C line 154-161
          scettds = 0.
          call w_channel_position(tdsch,scettds)
          print*,'tds file starts at scettds',scettds
          dds = scettds
          scettds = float(dds) + hh/24. + mm/1440.
          print*,'set tds channel position to',scettds
          call w_channel_position(tdsch,scettds)
          print*,'tds channel position set to',scettds


C line 173
          ok = w_channel_open(fillch,stream)

C line 176-183
          scetfill = 0.
          call w_channel_position(fillch,scetfill)
          print*,'fill file starts at scetfill',scetfill
          dds = scetfill
          scetfill = float(dds) + hh/24. + mm/1440.
          print*,'set fill channel position to',scetfill
          call w_channel_position(fillch,scetfill)
          print*,'fill channel position set to',scetfill


C line 232-234
 30           FORMAT('wi_lz_wav_',I8.8,'_v*.dat')
              PRINT*,'STREAM IN GET_4',STREAM
            ok = w_channel_open(tdsch,stream)

C line 247-257
            scettds = 0.
            call w_channel_position(tdsch,scettds)
            print*,'tds or fill channel starts at',scettds
            dds = scettds
              HH = scetr(2)/10000
              MM = MOD(NHRMN,10000)
            mm = mm/100
            scettds = dfloat(dds) + hh/24. + mm/1440. - 1./1440.
            print*,'set fill or tds channel position to',scettds
            call w_channel_position(tdsch,scettds)
            print*,'channel position set to',scettds


C line 266-267
      call w_ur8_to_ymd(scettds,yyyy,mon,dd,hh,mm,ss,ms)
      call w_ur8_to_ydoy(scettds,yyyy,doy,msday)

C line 284
          ok = w_event(fillch,'FILL')

C line 288
           go to 110
C line 310
 110    continue

C line 291-293
        item = 'EVENT_SCET'
        ok = w_item_i4(fillch, item, s_scet, 2, return_size)
        print*,'initial fillch time',s_scet

C line 316-348
      IF(IEND.LT.4) THEN
        if(scettds.lt.scetfill) then
          event = 'TDSF'
          IF(IFASTSLOW.NE.0)event = 'TDSS'
          ch = tdsch
        else
          event = 'FILL'
          ch = fillch
        endif
        if(icheof.eq.1) then
          event = 'TDSF'
          IF(IFASTSLOW.NE.0)event = 'TDSS'
          ch = tdsch
        elseif(icheof.eq.2) then
          event = 'FILL'
          ch = fillch
        else
        endif
      type*,'compare scettds,scetfill,evt',scettds,scetfill,event
      ELSE
        CH = TDSCH
        print*,'channel set to tdsch =',ch
          print*,'nevtr',nevtr
          print*,'evtr  ',evtr
        if(evtr.eq.'F') then
           event = 'FILL'
        elseif(evtr.eq.'T') then
           event = 'TDSF'
           if(ifastslow.eq.1) event = 'TDSS'
        else
           print*,'error in event read'
        endif
      ENDIF

C line 350
      ok = w_event(ch,event)

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

C line 447-472
         IF(TDS_CHANNEL.EQ.1) NEVTSV = ITEMP
C
         item = 'ERT_MAJOR_FRAME'
         ok = W_ITEM_I4(ch, item, MAJOR, 1, return_size)
         item = 'ERT_MINOR_FRAME'
         ok = W_ITEM_I4(ch, item, MINOR, 1, return_size)
         item = 'WIND_MFI_BZ(GSE)_R4'
         ok = w_item_R4(ch, item, BZ, 1, return_size)
         item = 'WIND_MFI_BY(GSE)_R4'
         ok = w_item_R4(ch, item, BY, 1, return_size)
         item = 'WIND_MFI_BX(GSE)_R4'
         ok = w_item_R4(ch, item, BX, 1, return_size)
         item = 'WIND_MFI_BPHI(GSE)_R4'
         ok = w_item_R4(ch, item, BANGLE, 1, return_size)
         item = 'WIND_ORBIT_X(GSE)_R8'
         ok = w_item_R8(ch, item, XR8, 1, return_size)
         XRE = XR8/RE
         item = 'WIND_ORBIT_Y(GSE)_R8'
         ok = w_item_R8(ch, item, YR8, 1, return_size)
         YRE = YR8/RE
         item = 'WIND_ORBIT_Z(GSE)_R8'
         ok = w_item_R8(ch, item, ZR8, 1, return_size)
         ZRE = ZR8/RE
       item= 'EVENT_START_SCET_R8'
       ok = w_item_R8(ch, item, real_scet, 1, return_size)
       call w_ur8_to_ymd(real_scet,ryyyy,rmon,rdd,rhh,rmm,rss,rmsec)

C line 511-512
      write(87,7737) itemp,event,s_scet(2),tds_channel,
     1      parx(irx),maxdata,tdscal(tds_channel,isps,maxdata+128)


C line 567
      number_event = itemp


C line 589-599
      WRITE(77,3011) 'FILE:',DATFILE
      WRITE(77,3012) 'SAMPLE TIME:',RYYYY,'/',RMON,'/',
     1 RDD,' ',RHH,':',RMM,':',RSS,'.',RMSEC,' UT'
      WRITE(77,3033) 'EVENT UR8:',real_scet
      WRITE(77,3013) 'EVENT NO:',ITEMP
      WRITE(77,3015) 'CHANNEL:',TDS_CHANNEL
      WRITE(77,3016) 'SAMPLE RATE:',(.001*SPS),' kHZ'
      WRITE(77,3017) 'LP FILTER:',FILTER,' HZ'
      WRITE(77,4444) 'BX (nT):',BX
      WRITE(77,4444) 'BY (nT):',BY
      WRITE(77,4445) 'BZ (nT):',BZ



