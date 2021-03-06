      PROGRAM TDSVIS
C
C     PLOTS TDS AND FREQUENCY ON SCREEN
C
      include           'wind_examples:wind_tm_routine_def.for'
      include           'wind_examples:wind_return_code_def.for'
      integer*4   ok
      integer*4   i,j,k,n,itemp
      integer*4   get_stream_name
      integer*4   wait_for_events               ! an entry point
      integer*4   err_count
      integer*4   ichpa(6),ipacal(6,4)
      character*80      stream
      character*105     junk
      character*4 event
      character*4 pa(6,4),parx(9)
      character*1 DISPOSE,EVTR
      parameter   size=2048
      integer*4   return_size
      integer*4   tds_channel,ifilf,ifils,ifil,ifsps,issps,isps
      integer*4   temp_waves,iend,n2
      character*32      s
      integer*4   s_scet(2),scetr(2),sceti4(2)
      REAL*8            XR8,YR8,ZR8,RE
      real*8            scet,scettds,scetfill,beginevt,endevt
      integer*4   major, minor
      character*80      file,eventfile
      character*32      item
      integer*4   ios,ms,doy,msday,dds
      integer*4   NREM,NHRMN,IHRMN,yyyy,mon,dd,hh,mm,ss,IFASTSLOW
      real        FILTER,ffilter,sfilter,fsps,ssps,sps
      REAL        S1,S2,dpuclk,sunclock
      REAL*8      real_scet,scet_corr
!
C
      CHARACTER*12 PTITLE(25)
      INTEGER*4 TDSCH,hkch,fillch,ch,NUMBER_EVENT
      COMPLEX CGAIN,FCOEF,FCOEFT,CCORR
C
C     VARS FOR DATAFILE OUTPUT
C     ************************
      CHARACTER*25 DATFILE
      INTEGER*4 ryyyy,rmon,rdd,rhh,rmm,rss,rmsec
      CHARACTER*4 YEARC,MONC,DAYC,HHC,MMC,MSECC
      CHARACTER*4 SSC,CHC,TOCHAR
      INTEGER*4 NUM
C     ************************
C     END DATAFILE VARS
      common /nrblk/ nrem,NHRMN,IFASTSLOW
      common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
      COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
      COMMON /HEADBL/ PTITLE,EVENT,NUMBER_EVENT
      COMMON /GAINBLK/ PHASE,CGAIN                   ! PHASE IS IN RADIANS
      COMMON /FITBLK/ NPT,VOLTIN(2048),TM(2048)
      COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
      COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4),SPINRATE
      COMMON /PARTBLK/ X4DATA(2050,4),XRE,YRE,ZRE
      COMMON /ANGLEBLK/ BANGLE,IAANGLE
      DATA TWOPI /6.2831853/
      DATA RE /6378.D00/
      DATA NTMDAY /100/
      DATA FFILTER /50000.,12500.,3125.,781./
      DATA SFILTER /3125.,781.,195.,49./
      DATA FSPS /120.,30.,7.5,1.875/
      DATA SSPS /7500.,1875.,468.75,117.2/
      DATA PARX /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',
     1    ' BX ',' BY ',' BZ '/
      DATA PA /'EXAC','EXAC','EXDC',' BX ',' BY ',' BZ ',
     1           'EXDC','EYAC','EYDC','EXDC','EYDC','EZDC',
     2           '    ','EZAC','EZDC','    ','    ','    ',
     3           '    ','EZAC','EZDC','    ','    ','    '/
      DATA IPACAL /1,    1,     4,     7,     8,     9,
     1               4,    2,     5,     4,     5,     6,
     2               0,    3,     6,     0,     0,     0,
     3               0,    3,     6,     0,     0,     0/
      DATA IFASTSLOW /0/            ! 0 IS FAST, 1 IS SLOW
      DATA ICHEOF /0/               ! 1 IS EOF ON FILL, 2 ON TDS
C
      PTITLE(1) = 'WIND-WAVES'
      PTITLE(2) = 'TIME DOMAIN'
      PTITLE(3) = 'SAMPLER'
      PTITLE(4) = 'EVENT NO.'
      PTITLE(8) = 'SAMPLE RATE'
      PTITLE(10) = 'L.P.FILTER'
C     PTITLE(12) = 'TRIGGERS'
      PTITLE(14) = 'DPU CLOCK'
      PTITLE(16) = 'SCET'
      PRINT*,' '
      print*,ptitle(1),ptitle(16)
C
 1001 FORMAT(I4,1X,20I5)
 1002 FORMAT(A)
C
C     GET STARTED
C
c 11     write(6,6)
c        write(6,7)
c      write(6,*) 'type 4,anything if events are to be read from a file'
c        read(5,*,err=11,end=20)  iend,n2
c        type*,iend,n2
        iend=1
        n2=4
C
c 12     write(6,4)
c        read(5,*,err=12,end=20)  ifastslow
        ifastslow=0
C
          type*,' '
c          type*,'type 1 if hodograms are wanted, 0 otherwise'
c          read(5,*) ihodo
c          type*,'ihodo= ',ihodo
         ihodo=0
C
      IF(IEND.NE.4) THEN
 10     CONTINUE
          ok = get_stream_name(stream)
          if (.not. ok) stop 'no file supplied.'
C
c        write(6,*)  'type hr,min to start, e.g. 0412'
c        read(5,3,err=10) iq, NHRMN
c        type*,NHRMN
        NHRMN=0
        HH = NHRMN/100
        MM = MOD(NHRMN,100)
          if(iend.eq.1) then
            nrem = 2000
          endif
          if(iend.eq.3) then
            nrem = 2
            if(ifastslow.eq.1) nrem = 4
            nevent = n2
          endif
C
c      type*,'type desired process level,0=raw,1=raw volts,2=fft,fft inv'
c      type*,' or 3,4 fft,fft inv and correct bad tds numbers'
c          read(5,3) iq,iprocess
          iprocess=4
          type*,' '
       if(iprocess.eq.0) type*,'ok, plot data in tm numbers - 128'
       if(iprocess.eq.1) type*,
     1    'ok, plot in volts, unity freq. response.'
          if(iprocess.ge.2) type*,
     1      'ok, plot volts, corrected for freq. response.'
          if(iprocess.ge.3) type*,
     1      'ok, plot volts, corrected for bad TDS data'
C
          ok = w_channel_open(tdsch,stream)
          PRINT*,'TDSCH,STREAM AFTER OPEN',TDSCH,STREAM
          if (.not. ok) stop 'Cannot open tds channel'
C
          ok = w_channel_open(cdfch,stream)
          PRINT*,'CDFCH,STREAM AFTER OPEN',CDFCH,STREAM
c
          scettds = 0.
          call w_channel_position(tdsch,scettds)
          print*,'tds file starts at scettds',scettds
          dds = scettds
          scettds = float(dds) + hh/24. + mm/1440.
          print*,'set tds channel position to',scettds
          call w_channel_position(tdsch,scettds)
          print*,'tds channel position set to',scettds

            IF(IFASTSLOW.EQ.0) THEN
            EVENT = 'TDSF'
C           ok = w_event(tdsch,'TDSF')
C           if (ok.ne.1) type*, 'cannot get TDSF event,ok=',ok
          ELSE
            EVENT = 'TDSS'
C           ok = w_event(tdsch,'TDSS')
C           if (ok.ne.1) type*, 'cannot get TDSS event,ok=',ok
          ENDIF
C
          ok = w_channel_open(fillch,stream)
          PRINT*,'FILLCH,STREAM AFTER OPEN',FILLCH,STREAM
          if (.not. ok) stop 'Cannot open fill channel'
          scetfill = 0.
          call w_channel_position(fillch,scetfill)
          print*,'fill file starts at scetfill',scetfill
          dds = scetfill
          scetfill = float(dds) + hh/24. + mm/1440.
          print*,'set fill channel position to',scetfill
          call w_channel_position(fillch,scetfill)
          print*,'fill channel position set to',scetfill
c         if(scetfill.lt.scettds) then
c            event = 'FILL'
c         endif
          GO TO 110
C
      ELSE                    ! entry to process from a file
          nrem = 2
          if(ifastslow.eq.1) nrem = 4
          iprocess = 4
          type*,'type name of data file, eg: makefile20.results'
          read(5,1002) eventfile
          open(unit=44,file=eventfile,status='old',readonly)
c         read(44,1044) scetr,nevtr,evtr,junk
C           FOR FOR070.DAT, RESULTS OF MAKEFILE
C         read(44,1044) scetr,nevtr,ntmday,evtr,junk
C           FOR MAKEFILE18.RESULTS
C     READ(44,1111,END=200) SCETR,NEVTR,EVTR,ISPS,IZCNT,FREQHZ
C     1      ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C     1111 IS NOW 114 CHARACTERS
 1111 FORMAT(I10,I8,I10,A2,I2,I4,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1            ,F6.3)
C
C           FOR MAKEFILE24.RESULTS, AND FOR058 FROM PROCMK18
C
      READ(44,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR
C     READ(56,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR,ISPS,IZCNT,FREQHZ
C     1      ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C     1114 IS NOW 113 CHARACTERS
 1114 FORMAT(I10,I7,I10,I3,A2,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1            ,F6.3)
C
 
          print*,'scetr',scetr
c         print*,'nevtr',nevtr
c         print*,'evtr  ',evtr
          print*,'the rest',junk
c 1044            FORMAT(I10,I8,I10,1X,A1,A100)
 1044       FORMAT(2x,I8,I7,I10,I3,1X,A1,A105)
            MTHDAY = MOD(SCETR(1),100)
              IF(NTMDAY.LT.100) THEN
                ISCET = SCETR(1)/100
              IF(NTMDAY.LT.MTHDAY) ISCET = ISCET + 1
              ISCET = 100*ISCET + NTMDAY
                PRINT*,'NEXT DATE',ISCET
                WRITE(STREAM,30) ISCET
            ELSE
                WRITE(STREAM,30) scetr(1)
              ENDIF
 30           FORMAT('wi_lz_wav_',I8.8,'_v*.dat')
              PRINT*,'STREAM IN GET_4',STREAM
            ok = w_channel_open(tdsch,stream)
            PRINT*,'IN 4, TDSCH,STREAM AFTER OPEN',TDSCH,STREAM
            if (.not. ok) stop 'Cannot open tds channel'
            if(evtr.eq.'F') then
               event = 'FILL'
            elseif(evtr.eq.'T') then
               event = 'TDSF'
               if(ifastslow.eq.1) event = 'TDSS'
            else
              print*,'error in event read'
            endif
            print*,' readin start at event ',event
C           IF(1) STOP 'FILE READ, '
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
      ENDIF
c
  6   format(1x,'for a specific event,type 2, event number')
  7   format(1x,'type 1,anything, to do all events after start time')
  5   format(q,a)
  4   format(1x,'type 0 for TDSF (fast), 1 FOR TDSS (slow)')
  3   format(q,i10)
c
      call w_ur8_to_ymd(scettds,yyyy,mon,dd,hh,mm,ss,ms)
      call w_ur8_to_ydoy(scettds,yyyy,doy,msday)
      print*,'scettds,yyyy,doy',scettds,yyyy,doy
c
      if(ok.eq.82) then
         scettds = 10000.           ! artificially large
      else
        item = 'EVENT_SCET'
        print*,'at 433,tdsch,ITEM=',tdsch,ITEM
        print*,'tdsch ',tdsch
        print*,' item ',item
        print*,' sceti4 ',sceti4
C       ok = w_item_i4(tdsch, item, sceti4, 2, return_size)
C       s_scet(1) = sceti4(1)
C       s_scet(2) = sceti4(2)
      endif
c
      if(iend.ne.4) then
          ok = w_event(fillch,'FILL')
        if(ok.eq.82) then
           print*,'end of file on fillch'
c           stop
           go to 110
        endif
        if (.not. ok) stop 'cannot get fill event'
        item = 'EVENT_SCET'
        ok = w_item_i4(fillch, item, s_scet, 2, return_size)
        print*,'initial fillch time',s_scet
      endif
        
c
      ok = w_channel_filename(tdsch,file)
      write(87,*) file
      print*,' after get filename',file
c
      get_tm_stream = 1
c
c           ok = wind_tm_get_next_event(tdsch,major,minor,'HK')
c        item = 'TEMP_WAVES2'
c        ok = wind_tm_get_item(tdsch, item, temp_waves, 1, return_size)
c
C
C     GET NEXT EVENT
C
 110    continue
C
      ! this is the main program loop
c 
      type*,'going to get next event,tds,fill times',scettds,scetfill
c
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
      print*,'call w_event, ch,event= ',ch,event
      ok = w_event(ch,event)
         if (.not. ok) then
            type *, char(7), '******** missing packet in event ********'
              if( ok.eq.82) then
            type*,'end of file on ',event
c            stop
              if(ch.eq.tdsch) icheof = 2
              if(ch.eq.fillch) icheof = 1
              go to 21
            endif
C              if( ok.eq.82) stop 'end of file on above channel '
         else
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
C
C
             item = 'EVENT_NUMBER'
             ok = w_item_i4(ch, item, itemp, 1, return_size)
             type*,'event number',itemp,'  ',event
             if(iend.eq.2.and.itemp.ne.nevent) go to 110
             if(iend.eq.4.and.itemp.ne.nevtr) go to 110
         endif
             item = 'DPU_CLOCK'
             ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
             WRITE(PTITLE(15),1014) DPUCLK
 1014       FORMAT(F12.3)
             item = 'SUN_ANGLE'
             ok = w_item_R4(ch, item, sunclock, 1, return_size)
             item = 'WIND_SPIN_RATE_R4'
             ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
             item = 'event_boe_r8'
             ok = w_item_R8(ch, item, beginevt, 1, return_size)
             item = 'event_eoe_r8'
             ok = w_item_R8(ch, item, endevt, 1, return_size)
             CALL TDS_PHYS(CH,IPROCESS,NDATA,DATA,SPECT)
      IF(TDS_CHANNEL.LE.2) THEN
        NCH = TDS_CHANNEL
      ELSE
        NCH = TDS_CHANNEL-2
      ENDIF
      do n = 1,2048
            X4DATA(N,NCH) = data(n)
      enddo
C
c     do n = 1,2048
c           write(56,*) n,ndata(n),data(n)
c     enddo
C

C          item = 'EVENT_SCET_R8'
c          ok = w_item_r8(tdsch, item, scet, 1, return_size)
c          if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c          call w_ur8_to_string(scet,PTITLE(16),PTITLE(18))
           call w_ur8_to_ymd(scetT,yyyy,mon,dd,hh,mm,ss,ms)
c          call w_ur8_to_ydoy(scetT,yyyy,doy,msday)
           ihrmn = 100*hh+mm
c          TYPE *,s_scet
c          TYPE *,'scett,doy',scett,doy
C           write(26,*) itemp,s_scet
         ihrmn = 100*hh+mm

      
         write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C        s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C     1     s(9:10)//':'//s(11:12)//':'//s(13:14)



         WRITE(PTITLE(17),1016) s(1:4),S(5:6),S(7:8)
         WRITE(PTITLE(18),1017) DOY
         WRITE(PTITLE(19),1018) s_scet(2)/100, MOD(s_scet(2),100)
 1016    format(A4,'/',A2,'/',A2)
 1017    FORMAT(' DOY ',I4)
 1018    FORMAT(I6.4,I3.2)
c        item = 'CHANNEL'
c        ok = wind_tm_get_item(ch, item, tds_channel, 1, return_size)
c        TYPE*,'CHANNEL',tds_channel
         WRITE(PTITLE(6),1019) TDS_CHANNEL
 1019    FORMAT('CHANNEL',I2)
         item = 'EVENT_NUMBER'
         ok = wind_tm_get_item(ch, item, itemp, 1, return_size)
c        type*,'event number',itemp
         WRITE(PTITLE(5),1012) ITEMP
 1012    FORMAT(I10)
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
       type*,'ok:',ok,'real_scet:',real_scet,'evnum:',itemp
       call w_ur8_to_ymd(real_scet,ryyyy,rmon,rdd,rhh,rmm,rss,rmsec)
C
         ipa = ichpa(tds_channel)
         WRITE(PTITLE(7),1007) PARX(IRX)
 1007    FORMAT('P/A ',A4)
c
         IF(TDS_CHANNEL.LE.2) THEN
            WRITE(PTITLE(9),1004) .001*SPS
         ELSE
            WRITE(PTITLE(9),1008) SPS
         ENDIF
            WRITE(PTITLE(11),1008) FILTER
 1004    FORMAT(F7.2,' kHZ')
 1008    FORMAT(F7.0,' HZ')
 1009    FORMAT('TDS CHANNEL',I4)

C        if (ok) type *, 'waves2 temperature', temp_waves2
CW          write(16,*) 'temp in t/m counts',temp_waves
C           write(26,*) 'temp in t/m counts',temp_waves
c        ok = wind_tm_xlate_item(ch, 'HK', item, temp_waves, title)
c           type*,'temp in degrees?',title
c        if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
c
C
      MAXDATA = 0
      IF(IPROCESS.EQ.0) THEN
        DO IK = 1,2048
          NDATA(IK) = NDATA(IK)-128
          MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
        ENDDO
      ELSE
        DO IK = 1,2048
          NDATA(IK) = NDATA(IK)-128
          MAXDATA = MAX0(MAXDATA,IABS(NDATA(IK)))
        ENDDO
      ENDIF
**********************
      IF(IFASTSLOW.EQ.0.AND.TDS_CHANNEL.GT.2) GO TO 110
      IF(IFASTSLOW.EQ.1.AND.TDS_CHANNEL.LE.2) GO TO 110
      write(87,7737) itemp,event,s_scet(2),tds_channel,
     1      parx(irx),maxdata,tdscal(tds_channel,isps,maxdata+128)
 7737 format(i10,2x,a4,i10,i5,2x,a4,i5,e12.3)
c     IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.85) GO TO 110
c     IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.1.AND.MAXDATA.LE.65) GO TO 110
c     IF(IEND.EQ.1.AND.TDS_CHANNEL.EQ.2.AND.MAXDATA.LE.75) GO TO 110
c     IF(IEND.EQ.1.AND.TDS_CHANNEL.GE.2) GO TO 110
c     IF(IEND.EQ.1.AND.TDS_CHANNEL.NE.3) GO TO 110
c     to pick out a specific event  (NOW MOVED EARLIER)
      if(iend.eq.2.and.itemp.ne.nevent) go to 110
C*******************
C
C     FIND ZERO CROSSING
C
      IZCNT = 0
      IL = 1
      IZ = IL
c       IF(NDATA(IL).EQ.0) PRINT*,'ZERO DATA',IL,NDATA(IL),NDATA(IL+1)
      DO IL = 2,2047
        IZ = IL
C
C           COUNT ONLY POS TO NEG
C
        IF(NDATA(IL).GT.0.AND.NDATA(IL+1).LE.0) THEN
              IZCNT = IZCNT+1
            IF(IPROCESS.EQ.0) THEN
              S1 = NDATA(IL)
              S2 = NDATA(IL+1)
            ELSE
              S1 = DATA(IL)
              S2 = DATA(IL+1)
            ENDIF
            IF((S1-S2).NE.0.) THEN
              DELZ = S1/(S1 - S2)
              ELSE
              DELZ = .5
              ENDIF
            IF(DELZ.LE.1.AND.DELZ.GE.0.) THEN
                  ZCROSS(IZCNT) = IL + DELZ
            ELSE
                  ZCROSS(IZCNT) = IL + .5
            ENDIF
        ENDIF
      ENDDO
      DO N = 1,IZCNT-1
        ZINT(N) = ZCROSS(N+1) - ZCROSS(N)
        IF(ZINT(N).EQ.0.) PRINT*,'ZINT=0 AT ',N
        IF(ZINT(N).EQ.0.) ZINT(N) = 1.E-6
      ENDDO
C     print*,'zero crossings found',izcnt
C     print*,'first 5',(zcross(kk),kk=1,5)
C
 20   CONTINUE
C
 1003 FORMAT(3(I9,E11.3,I5))
C
      number_event = itemp
C
C     SET UP DATFILE FOR DATE/TIME OF SAMPLE
C     **************************************


      YEARC=TOCHAR(RYYYY)
      MONC=TOCHAR(RMON)
      DAYC=TOCHAR(RDD)
      HHC=TOCHAR(RHH)
      MMC=TOCHAR(RMM)
      SSC=TOCHAR(RSS)
      MSECC=TOCHAR(RMSEC)
      CHC=TOCHAR(TDS_CHANNEL)
      DATFILE = YEARC // MONC(1:2) // DAYC(1:2) //
     1 '_' // HHC(1:2) // MMC(1:2) // '-' //
     2 SSC(1:2) //'x' // MSECC(1:3) // '_CH' // CHC(1:2) // '.DAT'
C     OPEN DATFILE FOR WRITING
      OPEN(UNIT=77,FILE=DATFILE,STATUS='NEW')
C
C     WRITE HEADER INFO TO DATFILE
C     ****************************
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

C     *******************************************
C     END DATFILE SETUP AND CALL THE PLOT ROUTINE
C     *******************************************
      CALL PUBCOMBO(3)
      CLOSE(77)
      type*,' '
      type*,'SAVED: ',DATFILE
      type*,' '
C
C     FORMAT STATEMENTS FOR DATFILE
C     *****************************
C 3001 FORMAT(T2,A8,T12,A15,T27,A15,T42,A15,T57,A15,T72,
C     1 A15,T87,A15) !LABEL
 3011 FORMAT(T2,A5,T25,A25,/) ! FILENAME
 3012 FORMAT(T2,A12,T25,I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,
     1 I2.2,A1,I2.2,A1,I3.3,A3) ! DATE/TIME
 3033 FORMAT(T2,A10,T25,F17.12)
 3013 FORMAT(T2,A9,T25,I8.8,/) ! EVENT NO
 3015 FORMAT(T2,A8,T25,I2.2) ! CHANNEL
 3016 FORMAT(T2,A12,T25,F6.2,A4) !SAMPLE RATE
 3017 FORMAT(T2,A10,T25,F6.0,A3,/) ! LP FILTER
 4444 FORMAT(T2,A8,T25,F18.12) !BX, BY
 4445 FORMAT(T2,A8,T25,F18.12,/) !BZ (with newline)

C     *****************************
      IF(IHODO.EQ.1) THEN
         print*,'hodo check',itemp,nevtsv,tds_channel
          IF(ITEMP.EQ.NEVTSV.AND.TDS_CHANNEL.EQ.2) CALL PLOTPART(3)
      ENDIF
C     READ(5,*) DISPOSE
      NPLOTS=NPLOTS+1
c     print*,'got to nplots.lt.nrem;',nplots,nrem
C
  21  CONTINUE
      IF(IEND.EQ.4) THEN
        IF(NPLOTS.LT.NREM.AND.OK.NE.82) GO TO 110
C           FOR FOR070.DAT = MAKEFILE18 RESULTS FOR 1999 JUNE-JULY
C       read(44,1044) scetr,nevtr,ntmday,evtr,junk
c       read(44,1044) scetr,nevtr,evtr,junk
C           FOR MAKEFILE18.RESULTS, FEB 2001
      READ(44,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR
C     READ(44,1111,END=200) SCETR,NEVTR,EVTR,ISPS,IZCNT,FREQHZ
      print*,'second read',sceti4(1),scetr,nevtr
C     1     ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C
        IF(SCETR(1).NE.SCETI4(1)) THEN
          OK = W_CHANNEL_CLOSE(TDSCH)
          MTHDAY = MOD(SCETR(1),100)
          ISCET = SCETR(1)/100
          IF(NTMDAY.LT.MTHDAY) ISCET = ISCET + 1   ! ADVANCE MONTH
          ISCET = 100*ISCET + NTMDAY
          PRINT*,'NEXT DATE',ISCET
          WRITE(STREAM,30) ISCET
          PRINT*,'STREAM IN NEXT',STREAM
          ok = w_channel_open(tdsch,stream)
          SCETI4(1) = SCETR(1)
        ENDIF
        NPLOTS = 0
        GO TO 110
      ELSE
        IF(NPLOTS.LT.NREM) GO TO 110
      ENDIF
 100  PRINT*,'ERROR IN READING RESULTS FILE'
      PRINT*,'JUNK',JUNK
 200  PRINT*, 'END OF RESULTS FILE'
      STOP
      END
C
C
C
C     FUNCTION FOR CHANGING AN INTEGER TO A CHARACTER
C     VALID ONLY FOR INTEGERS 0.LE.N.LE.9999
C     ***************************************
      FUNCTION TOCHAR(NUM)
      
      CHARACTER*4 RESULT,TOCHAR
      INTEGER*4 TEMP
      INTEGER*4 ONES,TENS,HUND,THOUS
      IF (NUM.GE.1000) THEN
        THOUS=NUM/1000
        HUND=(NUM-1000*THOUS)/100
        TENS=(NUM-1000*THOUS-100*HUND)/10
        ONES=NUM-1000*THOUS-100*HUND-10*TENS
        RESULT(1:1)=CHAR(THOUS+48)
        RESULT(2:2)=CHAR(HUND+48)
        RESULT(3:3)=CHAR(TENS+48)
        RESULT(4:4)=CHAR(ONES+48)
      ELSE IF (NUM.GE.100) THEN
        HUND=NUM/100
        TENS=(NUM-100*HUND)/10
        ONES=NUM-100*HUND-10*TENS
        RESULT(1:1)=CHAR(HUND+48)
        RESULT(2:2)=CHAR(TENS+48)
        RESULT(3:3)=CHAR(ONES+48) 
      ELSE IF (NUM.GE.10) THEN
        TENS=NUM/10
        ONES=NUM-10*TENS
        RESULT(1:1)=CHAR(TENS+48)
        RESULT(2:2)=CHAR(ONES+48)
      ELSE
        RESULT(1:1)='0'
        RESULT(2:2)=CHAR(NUM+48)
      ENDIF
      TOCHAR=RESULT
      END

C
C
C     PUBCOMBO CREATES THE PLOTS
C
C
      SUBROUTINE PUBCOMBO(ITERM)
C
C     SAME AS COMBO EXCEPT NO RAW DATA
C     THE FIRST (TOP) PANEL IS THE DATA IN PHYSICAL UNITS, 
C     THE SECOND IS THE FREQ  FROM 
C     ZERO CROSSINGS, AND THE THIRD IS THE FOURIER TRANSFORM
C     THIS VERSION DOES NOT HAVE INTERPOLATED PHYSICAL DATA
C
      CHARACTER*12 TITLE(25)
      CHARACTER*120 STR
      CHARACTER*4 EVENT
      CHARACTER*1 DISPOSE
      INTEGER*4 TDS_CHANNEL,S_SCET(2)
      INTEGER*4 NUMBER_EVENT,YYYY,DD
      INTEGER*4 OC
      REAL*8 BEGINevt,endevt
      integer*4 dds
      COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT
      common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
      COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
      COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
      COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4),SPINRATE
      COMMON /PUB/ HDATA(4100)
      DATA TWOPI /6.2831853/
C
      COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
      INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
      DIMENSION YY(4096),YYT(4096),PP(4096),ANG(2048)
      DIMENSION FF(4096),FFX(4096),POW(4096),POWX(4096)
C
c      CALL MGOINIT
c      CALL MGOSETUP(ITERM)
c      CALL MGOERASE
C
C     PUT LABELS ON RIGHT HAND SIDE
C
c      IF(ITERM.LT.0) THEN
c        XSTART = 350.
c        XEND = 2000.
c        CALL MGOSETLOC(XSTART,400.,XEND,2900.)
c      ELSE
c        XSTART = 100.
c        XEND = 880.
c        CALL MGOSETLOC(XSTART,80.,XEND,750.)
c      ENDIF
c      AVRFREQ=0.
c      IF(IZCNT.GT.1) THEN
c        AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
c        AVRFREQ = .001*SPS/AVRPER
c      ENDIF
c      TITLE(20) = ' AVR.FREQ.'
c      WRITE(TITLE(21),1020) AVRFREQ
c 1020 FORMAT(F8.2,' kHZ')
C
c      XTITLE = GX2 +.02*(GX2-GX1)
c      YTITLE = GY2
c      TRANGE = GY2-GY1
c      TINC = .03*TRANGE
c      CALL MGOSETEXPAND(.8)
c      DO N = 2,21
c        IF(N.EQ.4) YTITLE = YTITLE - TINC
c        IF(N.EQ.6) YTITLE = YTITLE - TINC
c        IF(N.EQ.12) YTITLE = YTITLE + TINC
c        IF(N.EQ.20) YTITLE = YTITLE - TINC
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        CALL MGOLABEL(12,TITLE(N))
c        YTITLE = YTITLE - TINC
c      ENDDO
C
c        YTITLE = YTITLE-2.*TINC
c        CALL MGOSETEXPAND(.7)
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        CALL MGOLABEL(12,' TRANSMITTED @')
c        YTITLE = YTITLE-.8*TINC
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        FDAY = BEGINEVT - DFLOAT(DDS)
c          NHR = FDAY*24.
c        MIN = FDAY*1440. - (NHR*60)
c        SEC = FDAY*86400. - 3600.*NHR - 60.*MIN   
c        WRITE(STR,1028) NHR,MIN,SEC
c 1028   FORMAT(1X,I2.2,I2.2,1X,F6.2)
c        CALL MGOLABEL(12,STR)
c        call w_ur8_to_ymd(BEGINEVT,yyyy,mon,dd,hh,mm,ss,ms)
C       call w_ur8_to_ydoy(beginevt,yyyy,doy,msday)
c        WRITE(STR,1029) yyyy,mon,dd
c 1029    format(I5,'/',I2,'/',I2)
c        YTITLE = YTITLE-.8*TINC
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        CALL MGOLABEL(12,STR)
C
c        YTITLE = YTITLE-TINC
c        CALL MGOSETEXPAND(.7)
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        CALL MGOLABEL(10,' SUN ANGLE')
c        YTITLE = YTITLE-.8*TINC
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        WRITE(STR,1027) SUNCLOCK*360./4096.
c 1027   FORMAT(F6.1' DEG.')
c        CALL MGOLABEL(10,STR)

c      IF(IPROCESS.GE.3) THEN
c        CALL MGOSETEXPAND(.7)
c        YTITLE = YTITLE-2.*TINC
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        CALL MGOLABEL(8,' BAD A/D')
c        YTITLE = YTITLE-2.*TINC
c        CALL MGOGRELOCATE(XTITLE,YTITLE)
c        CALL MGOLABEL(9,'CORRECTED')
c        CALL MGOSETEXPAND(.8)
c      ENDIF
C
c        CALL MGOSETEXPAND(.85)
c        IF(ITERM.GT.0) THEN
c          CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
c        ELSE
c          CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
c        ENDIF
c        CALL MGOSETEXPAND(1.)
c      CALL MGOSETEXPAND(.8)
c      CALL MGOPLOTID(EVENT,'[.WIND]TDSVIS,PUBCOMBO')
c      CALL MGOSETEXPAND(1.)
C
      END_ANGLE =  -360.*(SUNCLOCK-14.)/4096. - 45. ! ANGLE SUN TO +EX AT END
      IF(END_ANGLE.LT.-180.) END_ANGLE = END_ANGLE + 360.
      IF(END_ANGLE.GT.180.)  END_ANGLE = END_ANGLE - 360.
      DANG = SPINRATE*360./SPS/TWOPI
      ST_ANGLE = END_ANGLE + 3072.*DANG  ! ANGLE SUN TO +EX AT START 16nov99
      END_ANGLE = END_ANGLE + 1024.*DANG
c     ST_ANGLE = END_ANGLE + 2048.*DANG     ! ANGLE SUN TO +EX AT START
C
C
C     PLOT TDS DATA IN PHYSICAL UNITS
C
C          CALL REALFT(HDATA,2048,-1)           ! USE FFT TO INTERPOLATE
C          DO IK = 1,4096
C            HDATA(IK) = HDATA(IK)/1024.
C            VDATA(IK) = DATA(IK)
C          ENDDO
c
c      IF(ITERM.LT.0) THEN
c        CALL MGOSETLOC(XSTART,1800.,XEND,2900.)
c      ELSE
c        CALL MGOSETLOC(XSTART,465.,XEND,750.)
c      ENDIF
c        YMAX = 0.
C       EFFLEN = 45.                        ! X ANTENNA  18-JUL-95
C       IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 6.    ! Y ANTENNA   "
C       IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 4.    ! Z ANTENNA   "
        EFFLEN = 41.1                       ! X ANTENNA   9-NOV-96
        IF(IRX.EQ.2.OR.IRX.EQ.5) EFFLEN = 3.79  ! Y ANTENNA   "
        IF(IRX.EQ.3.OR.IRX.EQ.6) EFFLEN = 2.17  ! Z ANTENNA   "
        ACORR = 1000.
        IF(IRX.GE.7) THEN                 ! SEARCH COILS
            ACORR=1.
            EFFLEN = 1.
        ENDIF
C     CHANGE TO mV/meter
        DO N = 1,2048
          PP(N) = 1000.*(N-1)/SPS
          ANG(N) = ST_ANGLE - (N-1)*DANG
C         PP(N) = N
C         YY(N) = ACORR*HDATA(N)/EFFLEN         ! INTERPOLATED DATA
          YY(N) = ACORR*DATA(N)/EFFLEN
c          YMAX = AMAX1(YY(N),YMAX)
c          YMAX = AMAX1(-YY(N),YMAX)
        ENDDO
C
c********
c     do n = 1,4096
c       write(97,*) pp(n),yy(n)
c     enddo
c*****
C     PRINT*,'MAX mV/m',YMAX
C     CALL MGOTICKSIZE(0.,0.,0.,0.)  
c      CALL MGOSETLIM(PP(1),-YMAX,PP(2048),YMAX)
C
c     CALL MGOGRID(0)
C     CALL MGOSETLTYPE(1)
c     CALL MGOGRID(1)
c      CALL MGOSETLTYPE(0)
c      CALL MGOSETEXPAND(.6)
C
C     PLOT THE E-FIELD STRENGTH
C     ***************************
c      CALL MGOCONNECT(PP,YY,2048)
C     ***************************
C
c      CALL MGOSETEXPAND(.7)
c      CALL MGOBOX(1,2)
c      CALL MGOXLABEL(5,'mSEC.')
c      IF(IRX.GE.7) THEN             ! SEARCH COILS
c        CALL MGOYLABEL(2,'nT')
c      ELSE
c        CALL MGOYLABEL(4,'mV/m')
c      ENDIF
c      CALL MGOSETEXPAND(1.)
C
C     PLOT FREQUENCY FROM ZERO CROSSINGS, ALSO SMOOTH FREQUENCY
C
c      IF(ITERM.LT.0) THEN
c        CALL MGOSETLOC(XSTART,1250.,XEND,1595.)
c      ELSE
c        CALL MGOSETLOC(XSTART,310.,XEND,413.)
c      ENDIF

C
      SPSKHZ = .001*SPS
c      YMIN = .5*SPSKHZ
c      YMAX = 0.
C     PRINT*,'IZCNT',IZCNT
      DO N = 1,IZCNT-1
        IF(ZINT(N).EQ.0.) PRINT*,'IN PLOT, ZINT=0 AT',N
        FF(N) = SPSKHZ/ZINT(N)
        FFX(N) = 1000.*(ZCROSS(N)+ZCROSS(N+1))/SPS
        YMIN = AMIN1(FF(N),YMIN)
        YMAX = AMAX1(FF(N),YMAX)
      ENDDO
C     
C     SMOOTH FREQUENCY
C
      YMAX = 0.
      DO N = 2,IZCNT-2
        YYT(N) = .4*FF(N) + .3*(FF(N+1)+FF(N-1))
        YMAX = AMAX1(YYT(N),YMAX)
      ENDDO
      YYT(1) = FF(1)
      IF(IZCNT.GT.1) YYT(IZCNT-1) = FF(IZCNT-1)
C
      YMAX = AMIN1(.5*SPSKHZ,1.1*YMAX)
C     PRINT*,'YMIN,YMAX',YMIN,YMAX
C     YMIN =  .5*AVRFREQ
C     YMAX = 1.25*AVRFREQ
C**********
      YMIN =  0.
C     YMAX =  3.
C     YMIN =  18.
C     YMAX =  26.
C     YMIN =  12.
C     YMAX =  17.
C     PRINT*,'SET TO   ',YMIN,YMAX
c      CALL MGOSETEXPAND(.8)
c      CALL MGOSETLIM(0.,YMIN,PP(2048),YMAX)
C     CALL MGOTICKSIZE(0.,0.,0.,0.)  
C
C     PLOT FREQUENCY
C     ********************************
c      CALL MGOCONNECT(FFX,YYT,IZCNT-1)
C     ********************************
C
c      CALL MGOSETLIM(ANG(1),YMIN,ANG(2048),YMAX)
c      CALL MGOBOX(1,2)
c      CALL MGOSETEXPAND(.7)
c      CALL MGOXLABEL(16,'ANGLE,SUN TO EX')
c      CALL MGOYLABEL(10,'FREQ (kHZ)')
c      CALL MGOSETEXPAND(1.)
C
C     PLOT FOURIER SPECTRUM
C
c      IF(ITERM.LT.0) THEN
c        CALL MGOSETLOC(XSTART,300.,XEND,1020.)
c      ELSE
c        CALL MGOSETLOC(XSTART,50.,XEND,264.)
c      ENDIF
C
C
      N1 = 3
      N2 = 2048
      YMAX = -500.
      YMIN =  500.
      XMAX = 0.
      SPSUSE = SPS
      IF(TDS_CHANNEL.LE.2) SPSUSE = .001*SPS
      DO N = N1,N2,2
        NP = (N-1)/2
        POWX(NP) = NP*SPSUSE/2048.
C       YY(NP) = 10.*ALOG10(DATA(N)**2 + DATA(N+1)**2)
        POW(NP) = SPECT(NP)
        XMAX = AMAX1(XMAX,POWX(NP))
        IF(POW(NP).GT.YMAX) THEN
          NPSV = NP
          YMAX = POW(NP)
        ENDIF
        YMIN = AMIN1(YMIN,POW(NP))
      ENDDO
C
C     CALCULATE 10 DB BANDWIDTH
C
      MAXCOUNT = 0
      LOWCOUNT = 0
      DO N=NPSV,1024
         IF(SPECT(N).GT.(YMAX-10.)) THEN
            MAXCOUNT = MAXCOUNT+LOWCOUNT+1
            LOWCOUNT = 0
         ELSE
            LOWCOUNT = LOWCOUNT+1
         ENDIF
         IF(LOWCOUNT.GT.3) GO TO 410
      ENDDO
C 410 MAXCOUNT = MAXCOUNT + LOWCOUNT
 410  CONTINUE
      LOWCOUNT = 0
      DO N=NPSV-1,1,-1
         IF(SPECT(N).GT.(YMAX-10.)) THEN
            MAXCOUNT = MAXCOUNT+LOWCOUNT+1
            LOWCOUNT = 0
         ELSE
            LOWCOUNT = LOWCOUNT+1
         ENDIF
         IF(LOWCOUNT.GT.3) GO TO 420
      ENDDO
 420  CONTINUE
C     PRINT*,'DB,YMAX,YMIN',YMAX,YMIN
      BW = MAXCOUNT*SPSUSE/2048.
C     PRINT*,'MAXCOUNT, BANDWIDTH',MAXCOUNT,BW
      XN1 = N1-2
      XN2 = N2+2
c      YRANGE = YMAX-YMIN
c      YMAX = YMAX + .05*YRANGE
c      YMIN = YMIN - .05*YRANGE
c      YMIN = AMAX1(YMIN,-110.)
c      IF(TDS_CHANNEL.EQ.2) YMIN = -130.
c*************
c      CALL MGOSETLIM(-.017*XMAX,YMIN,XMAX,YMAX)
C     CALL MGOSETLIM(-.017*XMAX,YMIN,15.,YMAX)
c     write(66,*) 'pubcombo',ymin,ymax
c     CALL MGOGRID(0)
C     CALL MGOSETLTYPE(1)
c     CALL MGOGRID(1)
c      CALL MGOSETLTYPE(0)
c      CALL MGOSETEXPAND(.6)
C
C     ************SAMPLE DATFILE WRITE*********
C     WRITE(77,3001) 'YEARC','/','MONC','/','DAYC','HHC','MMC',':','SSC','CH'
C 3001 FORMAT(A4,A1,A2,A1,A2,T15,A2,A2,A1,A2,T29,A2)
C     *****************************************
C     DATFILE OUTPUT
C     **************
      WRITE(77,4446) 'EX ANGLE TO SUN (DEG)'
      WRITE(77,4447) 'START:',ST_ANGLE
      WRITE(77,4448) '  END:',END_ANGLE
      WRITE(77,3018) 'FORTRAN FORMATS'
      WRITE(77,3019) 'QUANTITY:','(T2,A9,T12,A4,T27,A7,',
     1 'T42,A13,T57,A11,T72,A11,T87,A5)'
      WRITE(77,3020) 'UNITS:','(T2,A6,T12,A1,T27,A4,',
     1 'T42,A7,T57,A3,T72,A3,T87,A9)'
      WRITE(77,3021) 'DATA:','(T2,I4.4,T12,E12.6,',
     1 'T27,E12.6,T42,E12.6,T57,E12.6,T72,E12.6,T87,E12.6)'
      WRITE(77,3022)'E-FIELD DATA POINTS:','2048'
      WRITE(77,3023)'FREQ DATA POINTS:',(IZCNT-1)
      WRITE(77,3024)'POWER DATA POINTS:',NP
      WRITE(77,3010) 'QUANTITY:','TIME','E-FIELD','             ',
     1 '           ','FREQ       ','POWER'
      WRITE(77,3099) 'UNITS:','s','mV/m','       ','   ','kHZ',
     1 'dB V^2/HZ'
      WRITE(77,3014) 'BEGIN!'
      OC=1
 333  IF (OC.LE.2048.AND.OC.LE.(IZCNT-1).AND.OC.LE.NP)THEN
        WRITE(77,3002) OC,PP(OC),YY(OC),POWX(OC),POW(OC)
        OC=OC+1
        GOTO 333
      ELSE IF (OC.LE.2048.AND.OC.LE.(IZCNT-1).AND.OC.GT.NP)THEN
        WRITE(77,3003) OC,PP(OC),YY(OC)
        OC=OC+1
        GOTO 333
      ELSE IF (OC.GT.2048.AND.OC.LE.(IZCNT-1).AND.OC.LE.NP)THEN
        WRITE(77,3004) OC,POWX(OC),POW(OC)
        OC=OC+1
        GOTO 333
      ELSE IF (OC.LE.2048.AND.OC.GT.(IZCNT-1).AND.OC.LE.NP)THEN
        WRITE(77,3005) OC,PP(OC),YY(OC),POWX(OC),POW(OC)
        OC=OC+1
        GOTO 333
      ELSE IF (OC.LE.2048.AND.OC.GT.(IZCNT-1).AND.OC.GT.NP)THEN
        WRITE(77,3006) OC,PP(OC),YY(OC)
        OC=OC+1
        GOTO 333
      ELSE IF (OC.GT.2048.AND.OC.GT.(IZCNT-1).AND.OC.LE.NP)THEN
        WRITE(77,3008) OC,POWX(OC),POW(OC)
        OC=OC+1
        GOTO 333
      ENDIF
      WRITE(77,3098) 'END!'     
C     DATFILE FORMATS
C     ***************
 3002 FORMAT(T2,I4.4,T12,E12.6,T27,E12.6,
     1 T72,E12.6,T87,E12.6)
 3003 FORMAT(T2,I4.4,T12,E12.6,T27,E12.6) !12
 3004 FORMAT(T2,I4.4,T72,E12.6,T87,E12.6) !23
 3005 FORMAT(T2,I4.4,T12,E12.6,T27,E12.6,T72,E12.6,T87,E12.6) !13
 3006 FORMAT(T2,I4.4,T12,E12.6,T27,E12.6) !1 
 3007 FORMAT(T2,I4.4,T42,E12.6,T57,E12.6) !2
 3008 FORMAT(T2,I4.4,T72,E12.6,T87,E12.6) !3
 3014 FORMAT(T2,A6) !BEGIN TAG
 3098 FORMAT(T2,A4) !END TAG
 3018 FORMAT(T2,A15)
 3019 FORMAT(T5,A9,T25,A21,A31)
 3020 FORMAT(T5,A6,T25,A21,A28)
 3021 FORMAT(T5,A5,T25,A19,A50,/)
 3022 FORMAT(T2,A20,T25,A4)
 3023 FORMAT(T2,A17,T25,I4.4)
 3024 FORMAT(T2,A18,T25,I4.4,/,/)
 3010 FORMAT(T2,A9,T12,A4,T27,A7,T42,A13,T57,A11,T72,
     1 A11,T87,A5) ! LABEL
 3099 FORMAT(T2,A6,T12,A1,T27,A4,T42,A7,T57,A3,T72,
     1 A3,T87,A9) ! UNITS
 4446 FORMAT(T2,A21) ! sun angle stuff
 4447 FORMAT(T8,A6,F18.12)
 4448 FORMAT(T8,A6,F18.12,/)
C     ***************
C
C     PLOT POWER SPECTRUM
C     *************************
c      CALL MGOCONNECT(POWX,POW,NP)
C     *************************
C
C     CALL MGOPOINTS(60.,1,PP,YY,NP)
c      CALL MGOSETEXPAND(.7)
c      CALL MGOBOX(1,2)
c      CALL MGOYLABEL(17,'POWER, DB V\u2/HZ')
c      IF(TDS_CHANNEL.GT.2) CALL MGOXLABEL(9,'FREQ, HZ')
c      IF(TDS_CHANNEL.LE.2) CALL MGOXLABEL(10,'FREQ, kHZ')
c      YTITLE = GY2
c      TRANGE = GY2-GY1
c      TINC = .1*TRANGE
c      YTITLE = YTITLE-TINC
c      CALL MGOGRELOCATE(XTITLE,YTITLE)
C     CALL MGOLABEL(10,'   10 dB  ')
C     YTITLE = YTITLE-TINC
C     CALL MGOGRELOCATE(XTITLE,YTITLE)
C     CALL MGOLABEL(10,' BANDWIDTH')
C     YTITLE = YTITLE-TINC
C     CALL MGOGRELOCATE(XTITLE,YTITLE)
C     CALL MGOLABEL(10,'  OF PEAK ')
c      IF(TDS_CHANNEL.GT.2) WRITE(STR,1021) .001*BW
c      IF(TDS_CHANNEL.LE.2) WRITE(STR,1022) BW
c 1021 FORMAT(F7.2,' HZ')
c 1022 FORMAT(F6.3,' kHZ')
c      YTITLE = YTITLE - TINC
c      CALL MGOGRELOCATE(XTITLE,YTITLE)
C     CALL MGOLABEL(10,STR)
c      CALL MGOSETEXPAND(1.)
C
c      IF(ITERM.LT.0) THEN
c        CALL MGOPRNTPLOT(NVEC)
c        PRINT*,' NO. VECTORS PLOTTED',NVEC
c      ELSE
C        READ(5,1023) DISPOSE
C 1023   FORMAT(A1)
c        CALL MGOTCLOSE
C      ENDIF
C
      RETURN
C
      END   
      SUBROUTINE DETAIL(ITERM,N1,N2)
C
C     PLOT TDS DATA AND FREQUENCY = .5/(INTERVAL BETWEEN ZEROS)
C
      CHARACTER*12 TITLE(25)
      CHARACTER*120 STR
      CHARACTER*4 EVENT
      INTEGER*4 NUMBER_EVENT
      COMMON /HEADBL/ TITLE,EVENT,NUMBER_EVENT
      COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
      COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
      COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4),SPINRATE
C
      COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
      INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
      DIMENSION YY(2048),PP(2048)
C
      CALL MGOINIT
      CALL MGOSETUP(ITERM)
      CALL MGOERASE
C
C     PLOT TDS DATA
C
      XEND = 2750.
      IF(ITERM.LT.0) THEN
        CALL MGOSETLOC(300.,400.,XEND,2230.)
      ENDIF
C
        CALL MGOSETEXPAND(.85)
        IF(ITERM.GT.0) THEN
          CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
        ELSE
          CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
        ENDIF
C       CALL MGOPUTLABEL(53,STR,9)
c       WRITE(STR,704) SAA()
c 704   FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c       CALL MGOPUTLABEL(55,STR,9)
        CALL MGOSETEXPAND(1.)
C
      DO N = N1,N2
        NP = N
        PP(N) = N
        YY(N) = NDATA(N)
      ENDDO
C
      CALL MGOTICKSIZE(0.,0.,5.6,28.)  
      XN1 = N1-2
      XN2 = N2+2
      CALL MGOSETLIM(XN1,-130.,XN2,130.)
c     CALL MGOGRID(0)
C     CALL MGOSETLTYPE(1)
c     CALL MGOGRID(1)
      CALL MGOSETLTYPE(0)
      CALL MGOSETEXPAND(.6)
      CALL MGOCONNECT(PP,YY,NP)
      CALL MGOPOINTS(60.,1,PP,YY,NP)
      CALL MGOSETEXPAND(.8)
        CALL MGOBOX(1,2)
        CALL MGOYLABEL(14,'T/M NUMBER-128')
        CALL MGOSETEXPAND(1.)
      TRANGE = GY2-GY1
      TINC = .04*TRANGE
      XTITLE = GX2 +.005*(GX2-GX1)
      YTITLE = GY2
      CALL MGOSETEXPAND(.8)
      AVRFREQ=0.
      IF(IZCNT.GT.1) THEN
        AVRPER = (ZCROSS(IZCNT)-ZCROSS(1))/(IZCNT-1)
        AVRFREQ = .001*SPS/AVRPER
      ENDIF
      TITLE(19) = 'AVR.FREQ.'
      WRITE(TITLE(20),1020) AVRFREQ
 1020 FORMAT(F8.2,' kHZ')
      DO N = 1,20
        YTITLE = YTITLE - TINC
        IF(N.EQ.4) YTITLE = YTITLE - TINC
        IF(N.EQ.6) YTITLE = YTITLE - TINC
        CALL MGOGRELOCATE(XTITLE,YTITLE)
        CALL MGOLABEL(12,TITLE(N))
      ENDDO
      CALL MGOSETEXPAND(1.)
      CALL MGOSETEXPAND(.8)
      CALL MGOPLOTID(EVENT,'[.WIND]TDSPRO')
      CALL MGOSETEXPAND(1.)
C
      IF(ITERM.LT.0) THEN
        CALL MGOPRNTPLOT(NVEC)
        PRINT*,' NO. VECTORS PLOTTED',NVEC
      ELSE
        CALL MGOTCLOSE
      ENDIF
C
      RETURN
C
      END
      options/extend_source
!------------------------------------------------------------------------------
      integer*4   function    get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
      implicit    none
      character*(*)     stream
      CHARACTER*80      YYYYMMDD
      common /nrblk/ nrem,NHRMN,IFASTSLOW
      integer*4   iq,NREM,NHRMN,IFASTSLOW

10      write(6,6)
        write(6,7)
        read(5,5,err=10,end=20) iq, stream
      print*,'in get_, initial stream=  ',stream
      if (iq .lt. 1) then
         stream = 'offline'
      else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
         stream = 'offline'
      else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
         stream = 'realtime'
      else
         ! assume the user entered the TIME of an offline file
         YYYYMMDD = STREAM(1:8)
         PRINT*,YYYYMMDD
         WRITE(STREAM,30) YYYYMMDD
 30      FORMAT('wi_lz_wav_',A8,'_v*.dat')
         PRINT*,'STREAM IN GET',STREAM
      end if

      get_stream_name = 1

 20   return
c
  5   format(q,a)
  6   format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
  7   format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
      end
      options/extend_source
!------------------------------------------------------------------------------
      integer*4   function    get_stream_four(stream)
! This routine gets the user's TM stream type specification.
!
      implicit    none
      character*(*)     stream
      CHARACTER*80      YYYYMMDD
      common /nrblk/ nrem,NHRMN,IFASTSLOW
      integer*4   iq,NREM,NHRMN,IFASTSLOW

c10     write(6,6)
c       write(6,7)
c       read(5,5,err=10,end=20) iq, stream
         ! assume the user entered the TIME of an offline file
         YYYYMMDD = STREAM(1:8)
         PRINT*,YYYYMMDD
         WRITE(STREAM,30) YYYYMMDD
 30      FORMAT('wi_lz_wav_',A8,'_v*.dat')
         PRINT*,'STREAM IN GET',STREAM

      get_stream_four = 1

 20   return
c
      end
      SUBROUTINE PLOTPART(ITERM)
C
C     PLOTS ONE COMPONENT AGAINST ANOTHER FOR PARTS OF AN
C           EVENT TO LOOK FOR POLARIZATION CHANGES
C
      CHARACTER*12 title(25)
      CHARACTER*120 STR
      CHARACTER*4 EVENT
      CHARACTER*8 LABEL(4)
      CHARACTER*1 DISPOSE
      INTEGER*4 TDS_CHANNEL,S_SCET(2),MAJOR,MINOR,NSYS
      COMMON /HEADBL/ TITLE,EVENT,NUMEVENT,FILE
      COMMON /FIXUPBLK/ NBAD3,NBAD1,NBAD2,IFXB
      COMMON /PLTBLK/ IZCNT,IPROCESS,ZCROSS(2048),ZINT(2048),
     1   NDATA(2048),DATA(2050),SPECT(1025)
      COMMON /TDS_STATUS/ TDS_CHANNEL,SPS,FILTER,IRX,ISPS
      COMMON /PARTBLK/ XDATA(2050,4),XRE,YRE,ZRE
      common /headblk/ major,minor,s_scet,sunclock,beginevt,endevt,dds
      COMMON /STATUS/ FFILTER(4),SFILTER(4),FSPS(4),SSPS(4),SPINRATE
      COMMON /XYBLOCK/ XSPECT,YSPECT,DPHASE
      COMMON /ANGLEBLK/ BANGLE,IAANGLE
C
      COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
      INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
      character*80      file
c     INTEGER*4 SUNCLOCK
      DIMENSION YY(2048),XX(2048),XD(2048),YD(2048),PP(2048)
      REAL XSPECT(1025),YSPECT(1025)

      DATA LABEL /'EX mV/m','EY mV/m','EZ mV/m','B nT'/
      DATA TWOPI /6.2831853/
C
            N1 = 1
            N2 = 2
C
        CALL MGOINIT
        CALL MGOSETUP(ITERM)
        CALL MGOERASE
C
C       PUT LABELS ON RIGHT HAND SIDE
C
        IF(ITERM.LT.0) THEN
          CALL MGOSETLOC(500.,450.,2000.,3050.)
        ELSE      
          CALL MGOSETLOC(100., 80., 844., 750.)
        ENDIF
C
        XTITLE = GX2 +.05*(GX2-GX1)
C       XTITLE = GX2 +.1*(GX2-GX1)              ! 3 dec 1996
        YTITLE = GY2
        TRANGE = GY2-GY1
        TINC = .03*TRANGE
        CALL MGOSETEXPAND(.6)
        TITLE(6) = ' '
        TITLE(7) = ' '
        DO N = 1,19
          YTITLE = YTITLE - TINC
          IF(N.EQ.4) YTITLE = YTITLE - TINC
          IF(N.EQ.6) YTITLE = YTITLE - TINC
          CALL MGOGRELOCATE(XTITLE,YTITLE)
          CALL MGOLABEL(12,TITLE(N))
        ENDDO
        YTITLE = YTITLE - TINC
        TITLE(20) = 'WIND ORBIT'
        WRITE(TITLE(21),1034) XRE
 1034 FORMAT('Xgse',F6.1)
        WRITE(TITLE(22),1035) YRE
 1035 FORMAT('Ygse',F6.1)
        WRITE(TITLE(23),1036) ZRE
 1036 FORMAT('Zgse',F6.1)
        DO N = 20,23
          YTITLE = YTITLE - TINC
          CALL MGOGRELOCATE(XTITLE,YTITLE)
          CALL MGOLABEL(12,TITLE(N))
        ENDDO
C
C       PLOT TDS DATA 
        YMAX = 0.
        EFFLENX = 41.1                    ! X ANTENNA  23-SEP-96
        EFFLENY = 3.79                          ! Y ANTENNA   "
        EFFLENZ = 2.17                          ! Z ANTENNA   "
        ACORR = 1000.
        IF(IRX.GE.7) THEN                 ! SEARCH COILS
            ACORR=1.
            EFFLEN = 1.
        ENDIF
C
C     CHANGE TO mV/meter
        DO N = 1,2048
          XD(N) = ACORR*XDATA(N,N1)/EFFLENX
          YD(N) = ACORR*XDATA(N,N2)/EFFLENY     
          YMAX = AMAX1(YY(N),YMAX)
          YMAX = AMAX1(-YY(N),YMAX)
          XMAX = AMAX1(XX(N),XMAX)
          XMAX = AMAX1(-XX(N),XMAX)
        ENDDO
C
        CALL MGOSETEXPAND(.85)
        IF(ITERM.GT.0) THEN
c         CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
        ELSE
          CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
        ENDIF
C       CALL MGOPUTLABEL(53,STR,9)
        CALL MGOSETEXPAND(1.)
C
        NXW = 3
        NYW = 3
        NW = NXW*NYW
        DO IW = 1,NW
          CALL MGOWINDOW(NXW,NYW,IW)
          NPTST = (IW-1)*2048/NW + 1
          NPTND = (IW)*2048/NW
          NPT = 0 
          XMAX = 0.
          YMAX = 0.
          NPTMAX = NPTST
          DO N = NPTST,NPTND
            NPT = NPT+1
            XX(NPT) = XD(N)
            YY(NPT) = YD(N)
            IF(ABS(XX(NPT)).GT.XMAX) NPTMAX = NPT
            XMAX = AMAX1(XMAX,ABS(XX(NPT)))
            YMAX = AMAX1(YMAX,ABS(YY(NPT)))
          ENDDO
          XMAX = 1.1*AMAX1(XMAX,YMAX)
          YMAX = XMAX
          CALL MGOSETLIM(-XMAX,-YMAX,XMAX,YMAX)
          CALL MGOSETEXPAND(.7)
C         CALL MGOTICKSIZE(0.,0.,0.,0.)  
          CALL MGOCONNECT(XX,YY,NPT)
C         PUT ON ARROW
          SIZE = .02*SQRT(XMAX**2 + YMAX**2)
          DX = XX(NPTMAX+1) - XX(NPTMAX)
          DY = YY(NPTMAX+1) - YY(NPTMAX)
          CALL ARROW(XX(NPTMAX),YY(NPTMAX),DX,DY,SIZE)
c     if(n2.eq.2) then
c       print*,nptmax-1,xx(nptmax-1),yy(nptmax-1)
c       print*,nptmax,xx(nptmax),yy(nptmax)
c       print*,nptmax+1,xx(nptmax+1),yy(nptmax+1)
c       write(76,*) 'iw =',iw
c       write(76,*) nptmax-1,xx(nptmax-1),yy(nptmax-1)
c       write(76,*) nptmax,xx(nptmax),yy(nptmax)
c       write(76,*) nptmax+1,xx(nptmax+1),yy(nptmax+1)
c     ENDIF
          CALL MGOSETEXPAND(.6)
          CALL MGOBOX(1,2)
          CALL MGOSETEXPAND(.6)
          CALL MGOXLABEL(7,LABEL(N1))
          CALL MGOYLABEL(7,LABEL(N2))
          CALL MGORELOCATE(-.95*XMAX,.85*YMAX)
          CALL MGOSETEXPAND(.5)
          WRITE(STR,1017)  NPTST,NPTND
 1017     FORMAT('SAMPLES',I5,' TO',I5)
          CALL MGOLABEL(20,STR)
C         CALL MGOSETEXPAND(.8)
          CALL MGOSETEXPAND(1.)
C
C     PUT ON B VECTOR
C
c     on 16 nov 1999 it was found that the sunclock is read 1024
c     samples after the end of the event, plus about 10.6 msec or
c     about 14 spin clock counts for message passing time
c
c     print*,'spinrate,sunclock,sps',spinrate,sunclock,sps
      END_ANGLE =  -360.*(SUNCLOCK-14.)/4096. - 45. ! ANGLE SUN TO +EX AT END
      IF(END_ANGLE.LT.-180.) END_ANGLE = END_ANGLE + 360.
      IF(END_ANGLE.GT.180.)  END_ANGLE = END_ANGLE - 360.
      DANG = SPINRATE*360./SPS/TWOPI
      ST_ANGLE = END_ANGLE + 3072.*DANG   ! ANGLE SUN TO +EX AT START 16nov99
      END_ANGLE = END_ANGLE + 1024.*DANG
C     print*,'start angle, end angle, dang',st_angle,end_angle,dang
C
      NAVR = (NPTST+NPTND)/2
      CTR_ANGLE = ST_ANGLE - NAVR*DANG  !ANGLE SUN TO +EX AT CENTER,nov 1999
C     not plot is in s/c coordinates, and so is as viewed from S ecliptic
C     pole, Y axis is reversed 
      PANGLE = CTR_ANGLE - BANGLE
C     PANGLE = 360. - PANGLE
      XBVEC = .3*XMAX*COSD(PANGLE)
      YBVEC = .3*XMAX*SIND(PANGLE)
C     PUT IN UPPER LEFT IF X,Y POSITIVE AT SAME TIME, ELSE UPPER RIGHT
      IF(XX(NPTMAX)*YY(NPTMAX).GT.0.) THEN
        XX1 = -.6*XMAX
        YY1 = .6*XMAX
      ELSE
        XX1 = .6*XMAX
        YY1 = .6*XMAX
      ENDIF
c     print*,sunclock,ctr_angle,bangle,pangle
      write(76,*) sunclock,ctr_angle,bangle,pangle
      CALL MGORELOCATE(XX1,YY1)
      CALL MGODRAW(XX1+XBVEC,YY1+YBVEC)
      CALL MGOSETEXPAND(.5)
      CALL MGOLABEL(1,'B')
      CALL MGOSETEXPAND(.8)
C           SPSKHZ = .001*SPS
C
        ENDDO
C
        IF(IPROCESS.GE.3) THEN
          YTITLE = YTITLE-2.*TINC
          CALL MGOSETEXPAND(.5)
          CALL MGOGRELOCATE(XTITLE,YTITLE)
          CALL MGOLABEL(8,' BAD TDS')
          YTITLE = YTITLE-TINC
          CALL MGOGRELOCATE(XTITLE,YTITLE)
          CALL MGOLABEL(9,'CORRECTED')
          YTITLE = YTITLE-TINC
          CALL MGOGRELOCATE(XTITLE,YTITLE)
          WRITE(STR,1024) IPROCESS
 1024     FORMAT(' LEVEL',I2)
          CALL MGOLABEL(8,STR)
C         WRITE(STR,1025) NBAD3
 1025     FORMAT(I5,' PTS')
C         YTITLE = YTITLE-TINC
C         CALL MGOGRELOCATE(XTITLE,YTITLE)
C         CALL MGOLABEL(9,STR)
          CALL MGOSETEXPAND(.8)
        ENDIF
C
        CALL MGOSETEXPAND(.6)
        CALL MGOPLOTID(' ','[.WIND]TDSVIS,PLOTPART')
        CALL MGOSETEXPAND(.8)
        IF(ITERM.LT.0) THEN
          CALL MGOPRNTPLOT(NVEC)
          PRINT*,' NO. VECTORS PLOTTED',NVEC
        ELSE
          READ(5,1023) DISPOSE
 1023     FORMAT(A1)
          CALL MGOTCLOSE
        ENDIF
C
      RETURN
C
      END   
