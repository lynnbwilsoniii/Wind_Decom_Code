	SUBROUTINE MAKEFILE(CH)
C
C	THIS IS MAKEFILE37
C
C	AN ADAPTATION OF ANTPOTENTIAL_TE TO DO ANTENNA POTENTIAL AS A 
C	FUNCTION OF Te AND DENSITY FOR SEVERAL DAYS
C	ON 1 JUNE 2007 AFTER RECEIVING A LOT OF DATA FROM STUART I 
C		COMMENTED OUT THE UNNECESSARY READS TO AVOID CONFUSION.  
C		THEY ARE MARKED CUR.  NEW DATA IS UNIT 81
C	this program is set up to use many different data sources, but
C		now, uses tmlib for density and Te_ files for e temperature
C	flux data,used for resistance paper, in unit 83
C	NP data used for resistance paper, in unit 82
C	SWE data in unit 84, format 1084
C	Te data for March 2000, in unit 85, format = *
C	 Te files are named Te_yyyymmdd.asc and are in data_a:[kellogg.wind] 
C	output data to unit 86
C
!	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	include		'apmplot_def.for'
        COMMON /MF37OUT/ ACOUNT,XASV,YASV,ZASV,XDSV,YDSV,ZDSV,TESV,
     1		DENSV,ETASV,SWEPOTSV
	integer*4	ok,okt
	integer*4	ch
	parameter	event='HK  '
	integer*4	ret_size
	integer*4	s_scet(2)
	character*80	stream
	character*32	item
	character*4	year
	character*40	com(15)
        character*50    filename 
	character*120   junk
	character*18	junk1,junk2
	real*4		acount(5,20,10),xasv(20,10),yasv(20,10),zasv(20,10)
	real*4		xdsv(20,10),ydsv(20,10),zdsv(20,10),TESV(20,10)
	REAL*4		DENSV(20,10),ETASV(20,10),SWEPOTSV(20,10)
        real*4          efluxsv(50),NPSV(1500)
        real*8          scet,delt,scet_ret,fluxtime(50)
	real*8		NPTIME(1500),scetswe,SCET_TE
c	real*8		SECDAY
        real*4          xresistor(9),yresistor(9),zresistor(9) 
	integer*4	error_count,hh,min,nday
	integer*4	ihrmn,yyyy,mon,dd,mm,ss,ms,YYYYMMDD
	integer*4	iv		! drive telemetry number
	integer*4	ir,irtm		! resistor index
	integer*4	ia		! antenna index, 1=x,2=y,3=z
	integer*4	Ix_apc,Iy_apc,Iz_apc		! 
	integer*4	IONOFF		! DAC ON OR OFF
	integer*4	ndaysdone
	integer*4	major, last_major,cal_on
	real*4 		apccal,apmcal,drivev
	real*4		wt(1000)
	real*4		etemp_te
	real		output(4,4)
	real		outmongo(200,7)
        data xresistor /34.8,375.,34.8,1025.,124.8,375.,124.8,2*1025./
        data yresistor /200.,1650.,200.,10150.,650.,1650.,650.,2*10150./
        data zresistor /8*68.,1.E6/ 
	data is /0/
	data ntot /0/
	data error_count /0/
	data ndaysdone /0/
	data nstart /0/
	data istart /0/
	data acount /1000*1.e-10/
	data xasv /200*0./
	data yasv /200*0./
	data zasv /200*0./
	DATA ELOW,EHI /9.,11./
c	data xrmeg /125./
c
 3000	CONTINUE
	call wind_tm_get_filename(ch,file)
	type *,'file: ',file(:72)
c	
	loop_and_gather = 1
	scet = 0.
	call w_channel_position(ch,scet)
	type*,'channel, scet',ch,scet
	if(ndaysdone.eq.0) then
		scetst = 0.d00
	endif
	if(scet.eq.0.d00) scet = 4767.d00
	nday = scet

        call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
	type*,'position channel pointer to',scet
        print*,'check time',scet,yyyy,mon,dd,mm

C
C	OPEN FILE 3DP. OF DENSITY AND ELECTRON TEMPERATURE DATA
C	NO, NOW IT'S OPENED IN MISSION_SCAN
C
C	WRITE(FILENAME,128) YYYY,MON,DD
 128	FORMAT('DATA_A:[KELLOGG.WIND]3DP_',I4.4,I2.2,I2.2,'.ASC')
C	OPEN(UNIT=81,FILE=FILENAME,STATUS='OLD',IOSTAT=IERRNTE,READONLY)
C*******
	IERRNTE = 0
C*******
	SCET_TE = -1.D00
	ETEMP_TE = 0.	
C	IF(IERRNTE.NE.0) THEN
C	  PRINT*,'FAILED TO OPEN 3DP N,Te DATA, IERR = ',IERRNTE
C	  RETURN
C	ENDIF
C
C	OPEN FILE Te. OF ELECTRON TEMPERATURE DATA
C
CUR	WRITE(FILENAME,128) YYYY,MON,DD
C 128	FORMAT('DATA_A:[KELLOGG.WIND]TE_',I4.4,I2.2,I2.2,'.ASC')
C 128	FORMAT('USER_A:[KELLOGG]TE_',I4.4,I2.2,I2.2,'.ASC')
CUR	OPEN(UNIT=85,FILE=FILENAME,STATUS='OLD',IOSTAT=IERRTE,READONLY)
CUR	SCET_NTE = -1.D00
CUR	ETEMP_NTE = 0.	
CUR	IF(IERRTE.NE.0) THEN
CUR	  PRINT*,'FAILED TO OPEN Te DATA, IERR = ',IERRTE
CUR	  RETURN
CUR	ENDIF
C
C       THIS SECTION READS IN 3DP DATA USED FOR RESISTANCE PAPER 
C
C               Flux is read every 96 sec.
C
CUR        WRITE(FILENAME,123) YYYY,MON,DD
CUR 123   FORMAT('USER_A:[KELLOGG.WIND.PAPERS]FLUX_',I4.4,I2.2,I2.2,'.ASC')
C        print*,'recheck time',scet,yyyy,mon,dd
C        print*,' name of flux file ',filename
CUR        OPEN(UNIT=83,FILE=FILENAME,STATUS='OLD',IOSTAT=IERR3DP,READONLY)
CUR	IF(IERR3DP.NE.0) GO TO 1123
C
C               np is read every 3 sec.
C
CUR        WRITE(FILENAME,124) YYYY,MON,DD
 124    FORMAT('USER_A:[KELLOGG.WIND.PAPERS]NP_',I4.4,I2.2,I2.2,'.ASC')
CUR        OPEN(UNIT=82,FILE=FILENAME,STATUS='OLD',READONLY)
C
CUR        NSS = 0
CUR        DO NS =  1,50
C         READ IN A TABLE OF STUART'S ACCURATE FLUXES
C
CUR          READ(83,*,END=142,ERR=150) NYR,MO,MONDAY,NHR,MIN,NSEC,EFLUXT
CUR          NSS = NSS+1
CUR          FDAYF = NHR/24. + MIN/1440. + NSEC/86400.
CUR          print*,'Flux,nyr,MO,MONDAY,min,eflux',nyr,MO,MONDAY,min,efluxt
CUR          FLUXTIME(NSS) = DFLOAT(NDAY) + DBLE(FDAYF)
c         FLUXTIME(NSS) = SCET
CUR          EFLUXSV(NSS) = EFLUXT
 150      CONTINUE
CUR        ENDDO
 142    continue
CUR        print*,' stuarts lines read in',nss,'  last ',fdayf
CUR          STRADDLEF = (FLUXTIME(2)-SCET)*(FLUXTIME(1)-SCET)
CUR          INDEXF = 1
C
CUR        NPS = 0
CUR        DO ND =  1,1500
C         READ IN A TABLE OF STUART'S ACCURATE PROTON DENSITIES
C
CUR          INDEXN = 1
CUR          READ(82,*,END=152,ERR=160) NYR,MO,MONDAY,NHR,MIN,NSEC,NPT
CUR          NPS = NPS+1
CUR          FDAYF = NHR/24. + MIN/1440. + NSEC/86400.
C         print*,'nyr,MO,MONDAY,min,eflux',nyr,MO,MONDAY,min,efluxt
CUR          NPTIME(NPS) = DFLOAT(NDAY) + DBLE(FDAYF)
c          NPTIME(NPS) = SCET
CUR          NPSV(NPS) = NPT
 160      CONTINUE
CUR        ENDDO
 152    CONTINUE
c
CUR        print*,'flux and np read in'
CUR          STRADDLEN = (NPTIME(2)-SCET)*(NPTIME(1)-SCET)
CUR          INDEXN = 1
C
 1123	CONTINUE
c
C
C 01-03-2000 02:00:08.169       123029.          4.59844       2.46818
C 01-03-2000 02:00:29.850       123505.          4.84017       1.27711 
C
   	read (84,1084,END=4001),
     1		dd,mon,yyyy,hh,mm,ss,ms,etempsv,edenssv,scpotsv
   	print*,'SWE read chk',etempsv,edenssv,scpotsv 
           call w_ur8_from_ymd(scetswe,yyyy,mon,dd,hh,mm,ss,ms)
   	print*,'ur8 chk',scetswe
   	ETEMP = ETEMPSV
   	EDENS = EDENSSV
   	SCPOT = SCPOTSV
C	
	  iterm = -1
	  iterm = 3
c
 3001   ok = w_event(ch,'HK')
	if ( ok.ne.1 ) then
		type *, 'cannot get event after', scet
		error_count = error_count + 1
		type*,'in getting started, error',error_count
		if(error_count.lt.100) go to 3001
		stop
	else
		type *,' hk event found'
	 	if(error_count.ne.0) then
c		  reset initial scet
c		  scet = 4767.d00
		  scet = 0.d00
	 	  error_count=0
		endif
	endif

	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	last_major = major-1

c	item = 'EVENT_SCET_R8'
C	type*,'going to get first item=',item
C	type*,'channel no',ch
c	ok = w_item_r8(ch, item, scet, 1, ret_size)
C	type*,'return size',ret_size
c	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
c	     ihrmn = 100*hh+mm
c	     TYPE 1005,YYYY,MON,DD,IHRMN
C		write(26,*) YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)
c
!	ok = wind_tm_set_messages_off(ch)
!	if (.not. ok) stop 'cannot turn messages off'
C
C       THIS SECTION OPENS THE SWE DATA FROM CDAWEB
C
C               Data are every 21 sec.
C
C        WRITE(FILENAME,125) YYYY,MON,DD
 125    FORMAT('DATA_A:[KELLOGG.WIND]SWE_H2_',I4.4,I2.2,I2.2,'.DAT')
C        print*,'recheck time',scet,yyyy,mon,dd
C        print*,' name of SWE file ',filename 
C
C	THIS SECTION SELECTS THE DATA TO BE USED
C
c	SCETSWE = 20000.D00		! ENSURE NO SWE DATA ARE READ
	SCETSWE =  0000.D00		! 
C	SCET_TE = 20000.D00		! DITTO FOR TE
C	IERR3DP = 10
C
C	type *, 'Please wait, gathering data and making plots...'

	go to 1010

 1000	   continue
c
c	get a new event
c
	ok = w_event(ch,event)
c
 1010	if(ok.eq.82) then		! END OF FILE
		ndaysdone = ndaysdone + 1
		type*,'finished at end of file',file
		type*,'DAYS DONE SO FAR',ndaysdone,' count',count(1)
		ok = wind_tm_close_channel(ch)
c		if(ndaysdone.lt.1) go to 3000	! get another file
	 	return
	 endif
c
	if (.not. ok) then
		type *, 'apmplot,cannot get event after', scet
		error_count = error_count + 1
		if(error_count.lt.10) go to 1000
C		CALL plotall(iterm)
		TYPE*,'TOO MANY ERRORS'
		GO TO 3000
	else
		error_count=0
	endif
	item = 'EVENT_SCET'
	okt = w_item_i4(ch, item, s_scet, 2, ret_size)
        item = 'EVENT_SCET_R8'
        okt = w_item_r8(ch, item, scet, 1, ret_size) 
C	print*,'waves event time',scet,s_scet
C
c	this was not commented but scetswe was set so large that it never 
c	happened
	IF(SCETSWE.LT.SCET) THEN
	  DOWHILE(SCETSWE.LT.SCET)
	    ETEMPSV = ETEMP
	    EDENSSV = EDENS
	    SCPOTSV = SCPOT
	    read (84,1084,END=4001),
     1		dd,mon,yyyy,hh,mm,ss,ms,etemp,edens,scpot
 1084	    format(i2,x,i2,x,i4,i3,x,i2,x,i2,x,i3,f14.0,f17.5,f15.5)
C	    print*,'CDAW SWE read chk',etemp,edens,scpot
            call w_ur8_from_ymd(scetswe,yyyy,mon,dd,hh,mm,ss,ms)
C	    print*,'ur8 chk',scetswe
	  ENDDO
	ENDIF
C	print*,'check swe CDAW readin'
C	print 1084,dd,mon,yyyy,hh,mm,ss,ms,etemp,edens,scpot
C
	istuart = 1
C	if(istuart.ne.1) print*,'3DP ion dens, etemp3dp data from windlib'
	if(istuart.ne.1) then
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(ch, item, dens3dp, 1, ret_size)
C        print 222,s_scet,yyyy,mon,dd,hh,mm,ss,edens,dens3dp 
 222	format(i10,i7,i5,i3,i3,i3,i3,i3,f6.2,f6.2)
C	  print*,'3dp ion dens,size,val',dens3dp,ret_size
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(ch, item, ETEMP3DP, 1, ret_size)
C	  print*,'3DP dens,etemp',dens3dp,etemp3DP
	  if(etemp.gt.0.) then
	    flux = dens3dp*sqrt(etemp)
	  else
	    go to 1000
	  endif
	else
	  item = 'EVENT_SCET_R8'
	  okt = w_item_r8(ch, item, scet, 1, ret_size)
	  fday = dmod(scet,1.d00)
C	  call tdpflux(yyyymmdd,fday,fluxst,dens,etemp)
C	  flux = fluxst/1.68e7
	endif
C
C	check that 3DP (old) accurate data is available
C
C	print*,'check ierredp',ierr3dp
	ierr3dp = 1
C	if(ierr3dp.ne.0) print*,'accurate 3dp data not available'
	IF(IERR3DP.EQ.0) THEN
	  IF(SCET.GE.FLUXTIME(1).AND.SCET.LE.FLUXTIME(NSS)) THEN
C
            STRADDLEF = (FLUXTIME(2)-SCET)*(FLUXTIME(1)-SCET)
c
            DOWHILE(STRADDLEF.GT.0.)
              print*,'straddle',indexf,straddlef
              print*,'cont    ',fluxtime(indexf),scet,fluxtime(indexf+1)
              INDEXF = INDEXF+1
              STRADDLEF=(FLUXTIME(INDEXF+1)-SCET)*
     1		(FLUXTIME(INDEXF)-SCET)
            ENDDO
	    eflux = .5*(efluxsv(indexf) + efluxsv(indexf+1))
	  ENDIF
C	  print*,'flux chk',indexf,fluxtime(indexf),fluxtime(indexf+1),eflux
c
	  IF(SCET.GE.NPTIME(1).AND.SCET.LE.NPTIME(NPS)) THEN
C
            STRADDLEN = (NPTIME(2)-SCET)*(NPTIME(1)-SCET) 
            DOWHILE(STRADDLEN.GT.0.)
              INDEXN = INDEXN+1
              STRADDLEN = (NPTIME(INDEXN+1)-SCET)*(NPTIME(INDEXN)-SCET)
            ENDDO
	    DENSI = .5*(NPSV(INDEXN)+NPSV(INDEXN+1))
	  ENDIF
c
	  ETEMPEV = (EFLUX/1.68E7/DENSI)**2
C	  print*,'np chk',indexn,nptime(indexn),nptime(indexn+1),densi
C	  print*,'electron temperature, ev',etempev
C	  IF(ETEMPEV.LT.ELOW.OR.ETEMPEV.GT.EHI) GO TO 1000
	ENDIF
C
C	GET ELECTRON TEMPERATURE FROM STUART'S (May 2007) TABLE
C
	IERRTE = 1
	IF(SCET_TE.LT.SCET.AND.IERRTE.EQ.0) THEN
	  DOWHILE(SCET_TE.LT.SCET)
	    etemp_tesv = etemp_te
	    read (85,*,end=4001,iostat=ierrte) secday,etemp_te
C		print*,ierrte,secday,etemp_te
	    if(ierrte.ne.0) print*,'te readin error',IERRTE,secday,etemp_te
	    scet_te = dble(nday) + secday/86400.d00
c	    print*,'Te ur8 chk',scet_te
	  ENDDO
c	   print*,'end Te dowhile,tm,3dp',scet,scet_te
	ENDIF
C	print 1084,dd,mon,yyyy,hh,mm,ss,ms,etemp,e

c	    read ¨85,*,END=4001,IOSTAT=IERRTE) secday,etemp_te
c       FLUX = 1.68E7*FLUX
C
C	GET N AND ELECTRON TEMPERATURE FROM STUART'S (2007) JUNE 1 TABLE
C
C	print*,'check',scet_te,scet,ierrnte
	IF(SCET_TE.LT.SCET.AND.IERRNTE.EQ.0) THEN
	  DOWHILE(SCET_TE.LT.SCET)
	    etemp_tesv = etemp_te
 1234	    read (81,*,end=4001,iostat=ierrnte) secday,DENS3DP,etemp_te
	    if(ierrNte.ne.0) print*,'te readin error',IERRNTE,secday,etemp_te
	    IF(IERRNTE.NE.0) GO TO 1234
	    scet_te = dble(nday) + secday/86400.d00
c	    print*,'Te ur8 chk',scet_te
	  ENDDO
c	   print*,'end Te dowhile,tm,3dp',scet,scet_te
	ENDIF
C	print 1084,dd,mon,yyyy,hh,mm,ss,ms,etemp,e

c	    read ¨85,*,END=4001,IOSTAT=IERRTE) secday,etemp_te
c       FLUX = 1.68E7*FLUX
C
c	item = 'CAL_GENERATOR'
	item = 'CAL_ON'
	ok = w_item_i4(ch, item, cal_on, 1, ret_size)
        if(cal_on.eq.1) then
          print*,'*********** radio cal on ***',s_scet,' *****'
          go to 1000            ! 1 means radio cal on.
        endif 
C
	item = 'DPU_MAJOR_FRAME'
	ok = w_item_i4(ch, item, major, 1, ret_size)
	if(major.eq.last_major) go to 1000
	last_major = major
c
c	event is ok, process it
c
	   is = MIN0(count(1)+1,sz_x_dim)

c 	    item = 'EVENT_SCET_R8'
c	    ok = w_item_r8(ch, item, scet, 1, ret_size)

	   item = 'EVENT_SCET'
	   ok = w_item_i4(ch, item, s_scet, 2, ret_size)
c***********   for routine processing, started 10 May 1997, which stops
c		at 0300, when only driven samplels are wanted
C		2000 IS WRITE ACCUMULATED SAMPLES AND RETURN
C	   if(s_scet(2).gt.30500) go to 2000
c		this ensures that output is written before return
	   if(s_scet(2).gt.235700) go to 2000
c*************
	   if(is.eq.1) type*, 'first sample, scet',
     1		s_scet
c	    print*,'scet',S_scet
c	    call w_ur8_to_string(scet,datestr,timestr)
c	    type*,datestr
c	    type*,timestr
	hh = S_SCET(2)/10000
	min = mod(S_SCET(2),10000)/100
	SS = MOD(S_SCET(2),100)
c	scet = dfloat(nday) + hh/24. + min/1440. + SS/86400.

	   item = 'APC_Z'		! word 74
C	   item = 'FFT_Z_APC_RELAY'		! word 74
C	   get another event if drive is on
           ok = w_item_i4(ch, item, ionofft, 1, ret_size)
	   if(ionofft.ne.1) print*,'drive voltage on'
	   if(ionofft.ne.1) go to 1000
c
           item = 'APC_X_RESISTOR'              ! word 69
           ok = w_item_i4(ch, item, x_resistor, 1, ret_size)
           if (ok) then
                irtm = x_resistor
                ir = rassign(irtm)
c                resistor(1,is) = ir
		XRMeg = xresistor(ir)
           endif  
C
	   item = 'APC_DAC'		! word 70
	   ok = w_item_I4(ch, item, idrive, 1, ret_size)
c	   item = 'APC_DAC_VOLTS_R4'		! word 70
c	   ok = w_item_R4(ch, item, drive(is), 1, ret_size)
	   if ( ok ) then
c		drivev = apccal(apc_dac(is))
		drivev = apccal(idrive)
	   else
		 write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif
	   drive(is) = drivev
c
	   item = 'APM_ANGLE'
	   ok = w_item_i4(ch, item, iangle, 1, ret_size)
c	   print*,'antenna angle',iangle
c
C	   item = 'X_APC'		! word 69
	   item = 'APC_X'		! word 69
	   ok = w_item_i4(ch, item, Ix_apc, 1, ret_size)
C	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok
	   if(Ix_apc.ne.1) go to 1000
C
C	   item = 'Y_APC'		! word    69
	   item = 'APC_Y'		! word 69
	   ok = w_item_I4(ch, item, Iy_apc, 1, ret_size)
	   if(Iy_apc.ne.1) go to 1000
C
C	  n3dp = ret_size
c	  print*,'3dp dens',dens3dp,'size',ret_size
C
	  item = 'EY_MONOPOLE_LENGTH_R4'
	  ok = w_item_r4(ch, item, EYL, 1, ret_size)
	  item = 'EZ_MONOPOLE_LENGTH_R4'
	  ok = w_item_r4(ch, item, EZL, 1, ret_size)
C	  PRINT*,'MONOPOLES',EYL,EZL
C
	  item = 'WIND_3DP_ION_VX(GSE)_R4'
	  ok = w_item_r4(ch, item, VX3, 1, ret_size)
	  item = 'WIND_3DP_ION_VY(GSE)_R4'
	  ok = w_item_r4(ch, item, VY3, 1, ret_size)
	  item = 'WIND_3DP_ION_VZ(GSE)_R4'
	  ok = w_item_r4(ch, item, VZ3, 1, ret_size)
	  item = 'WIND_3DP_ION_TEMP_R4'
	  ok = w_item_r4(ch, item, PTEMP, 1, ret_size)
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(ch, item, ETEMP3DP, 1, ret_size)
C
C	print*,'3dp,vx3,vy3,vz3,ptemp,etemp',vx3,vy3,vz3,ptemp,etemp3DP
C
	  item = 'WIND_MFI_BX(GSE)_R4'
	  ok = w_item_r4(ch, item, BX, 1, ret_size)
	  item = 'WIND_MFI_BY(GSE)_R4'
	  ok = w_item_r4(ch, item, BY, 1, ret_size)
	  item = 'WIND_MFI_BZ(GSE)_R4'
	  ok = w_item_r4(ch, item, BZ, 1, ret_size)
C
	  VxBX = 1.E-6*(VY3*BZ - VZ3*BY)
	  VxBY = 1.E-6*(VZ3*BX - VX3*BZ)
	  VxBZ = 1.E-6*(VX3*BY - VY3*BX)
c	print*,'V,VxB X,Y',VX3,VY3,VxBX,VxBy
C
	  IF(IANGLE.EQ.64) THEN
	    XCORR = 25.*(VxBX + VxBY)
	    YCORR = .5*EYL*(-VxBX + VxBY)
	    ZCORR = .5*EZL*VxBZ
	  ENDIF
C	THE CORRECTIONS, ABOVE, ARE THE ELECTRIC FIELDS ALONG THE
C	ANTENNAS.  THESE MAKE THE ANTENNA NEGATIVE, SO ADD THEM
C	HOWEVER, THIS CANNOT BE DONE WHEN I AM USING THE DIGITAL
C	TELEMETRY NUMBER TO HISTOGRAM THE DATA
C
C	   item = 'X_PEAK'		! word 63
	   item = 'APM_X_PEAK'		! word 63
	   ok = w_item_i4(ch, item, x_peak, 1, ret_size)
	   if (ok) then
		is = MIN0(count(1)+1,sz_x_dim)
		count(1) = is
		pk(1,is) = apmcal(1,x_peak)
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

C	   item = 'X_DC'		! word 65
	   item = 'APM_X_DC'		! word 65
	   ok = w_item_i4(ch, item, x_dc, 1, ret_size)
	   X_DC_V = apmcal(3,x_dc)
C
C	   item = 'Y_PEAK'		! word 64
	   item = 'APM_Y_PEAK'		! word 64
	   ok = w_item_i4(ch, item, y_peak, 1, ret_size)
	   Y_PK_V = apmcal(2,y_peak)
C
	   item = 'APM_Y_DC'		! word 66
	   ok = w_item_i4(ch, item, y_dc, 1, ret_size)
	   if (ok) then
		Y_DC_V = apmcal(4,y_dc)
C		acount(y_dc,2) = acount(y_dc,2) + 1
C		yasv(y_dc) = yasv(y_dc) + flux
C		ydsv(y_dc) = ydsv(y_dc) + dens3dp
	   endif
C
	   item = 'APM_Z_DC'		! word 67
	   ok = w_item_i4(ch, item, z_dc, 1, ret_size)
	   if (ok) Z_DC_V = apmcal(5,z_dc)
C		acount(3) = acount(z_dc,3) + 1
C		zasv(z_dc) = zasv(z_dc) + flux
C		zdsv(z_dc) = zdsv(z_dc) + dens3dp
C	   endif
c
	  item = 'WIND_WAV_NE_R4'
	  ok = w_item_r4(ch, item, wavdens, 1, ret_size)
	  item = 'WIND_WAV_NEQ'
	  ok = w_item_I4(ch, item, IWAVQUAL, 1, ret_size)
	  item = 'WIND_swe_VZ(GSE)_R4'
	  ok = w_item_r4(ch, item, VZ, 1, ret_size)
c
	  item = 'WIND_swe_VX(GSE)_R4'
	  ok = w_item_r4(ch, item, VX, 1, ret_size)
	  item = 'WIND_swe_VY(GSE)_R4'
	  ok = w_item_r4(ch, item, VY, 1, ret_size)
	  item = 'WIND_swe_VZ(GSE)_R4'
	  ok = w_item_r4(ch, item, VZ, 1, ret_size)
	  item = 'WIND_swe_VMAG_R4'
	  ok = w_item_r4(ch, item, VMAG, 1, ret_size)
	  item = 'WIND_swe_density_R4'
	  ok = w_item_r4(ch, item, densswe, 1, ret_size)
C	print*,'windlib swe ion density',densswe
	  nswe = ret_size
c
c
C       Bias current in microamps, positive sign is electrons to antenna,
C               i.e. current from antenna
        Xbias = (av(1,is) - drive(is))/XRMeg
C        print*, 'bias',is,scet,Xbias
C
c	  print*,'e temp',etemp
c	  if(etemp.gt.0.) then
c	    flux = dens3dp*sqrt(etemp)
C	    write(88,1088) is,flux,av(1,is),av(2,is),av(3,is)
C     1		,pk(1,is),pk(2,is)
 1088	    format(i5,f10.4,5f10.5)
c	  endif
 1549	format(i6,6f8.2)
c	WRITE(38,*) S_SCET,X_DC_V,SCPOT
	IF(IERRNTE.EQ.0) WRITE(47,1047) SCET,SCETSWE,DENS3DP,WAVDENS,DENSSWE,
     1	 DENS3DP,IWAVQUAL,ETEMP/11600.,ETEMP_TE,X_DC_V,SCPOT
 1047	FORMAT(2F10.4,4F7.2,I5,4F7.2)
c
C*******
C	AN ATTEMPT TO REMOVE BAD SWE DATA
C
c	IF(SCPOT.LT.2..OR.SCPOT.GT.15.) GO TO 1000
c	IF(SCPOT.GT.10.AND.X_DC_V.LT.1.3) WRITE(35,*) S_SCET,X_DC_V,SCPOT
c	IF(SCPOT.GT.10.AND.X_DC_V.LT.1.3) GO TO 1000
C	TO REMOVE BAD 3DP DATA
	IF(ETEMP_TE.LT.1.) WRITE(39,*) 'TE TOO COLD',SCET,ETEMP_TE
	IF(ETEMP_TE.LT.1.) GO TO 1000
C******
C	print*,'ITE check, etemp,dens3dp,TE=',etemp_TE,dens3dp
C	ITE = (ALOG(ETEMP/11600.) - .4406)/.2546
C        ITE = (ALOG(ETEMP/11600.) - 1.0225)/.23025
        IF(ETEMP_TE.GT.0.) ITE = (ALOG(ETEMP_Te) - 1.0225)/.23025
	IF(ITE.LT.1.OR.ITE.GT.10) PRINT*,'ITE OUT OF RANGE, =',SCET,ITE
	ITE = MAX0(ITE,1)
	ITE = MIN0(ITE,10)
C	BELOW, RANGE IS .5 TO 40., FP TO 57
C	IDN = (ALOG(DENS3dp) + 1.1313)/.4382
C	BELOW, RANGE IS .25 TO 50., FP TO 64, in 12 steps
C	IF(DENS3DP.GT.0.) IDN = (ALOG(DENS3dp) + 1.8278)/.44153
C	BELOW, RANGE IS .25 TO 40, FP TO 57, IN 20 STEPS
	IF(DENS3DP.GT.0.) IDN = (ALOG(DENS3dp) + 1.6401)/.25376
	IF(IDN.LT.1.OR.IDN.GT.20) PRINT*,'IDN OUT OF RANGE, =',IDN
	IDN = MAX0(IDN,1)
	IDN = MIN0(IDN,20)
	DO I = 1,5
	  ACOUNT(I,IDN,ITE) = ACOUNT(I,IDN,ITE)+1.
	ENDDO
C	print*,'count chk',is,idn,ite,acount(1,idn,ite)
	XASV(IDN,ITE) = XASV(IDN,ITE) + X_DC_V
        YASV(IDN,ITE) = YASV(IDN,ITE) + Y_DC_V
        ZASV(IDN,ITE) = ZASV(IDN,ITE) + Z_DC_V
        XDSV(IDN,ITE) = XDSV(IDN,ITE) + X_DC_V**2 
        YDSV(IDN,ITE) = YDSV(IDN,ITE) + X_DC_V**2 
        ZDSV(IDN,ITE) = ZDSV(IDN,ITE) + X_DC_V**2 
        ETA = SCPOT/(ETEMP_Te/11600.)
        ETASV(IDN,ITE) = ETASV(IDN,ITE) + ETA  
	SWEPOTSV(IDN,ITE) = SWEPOTSV(IDN,ITE) + SCPOT
	DENSV(IDN,ITE) = DENSV(IDN,ITE) + DENS3DP
c	TESV(IDN,ITE) = TESV(IDN,ITE) + ETEMP/11600.
	TESV(IDN,ITE) = TESV(IDN,ITE) + ETEMP_Te
C	print*,'1',etemp,dens3dp,edens,scpot
C	print*,'2',idn,ite
C	if(acount(1,idn,ite).ge.2.) stop
C	print*,'calling histo,x=',x_dc_v
	IF(IDN.EQ.6) THEN
	  CALL HISTOG(1,X_DC_V,30,0.,3.,.5,htotal,RET)
C	  print*,'called histo,x,tot=',x_dc_v,etemp/11600.,ite,htotal
	  WRITE (39,*) S_SCET,X_DC_V,ETEMP_Te/11600.,ITE
	ENDIF
 2001	GO TO 1000
C 	end do
 2000	continue

C	type*,'write accumulated samples as function of angle'
C	do JTE = 1,10
C	  DO JDN = 1,10
C	    xasv(JDN,JTE) = xasv(JDN,JTE)/acount(1,JDN,JTE)
C	    yasv(JDN,JTE) = yasv(JDN,JTE)/acount(2,JDN,JTE)
C	    zasv(JDN,JTE) = zasv(JDN,JTE)/acount(3,JDN,JTE)
C	  xdsv(JTE,JDN) = xdsv(JTE,JDN)/acount(i,1)
C	  ydsv(JTE,JDN) = ydsv(JTE,JDN)/acount(i,2)
C	  zdsv(JTE,JDN) = zdsv(JTE,JDN)/acount(i,3)
C	  ENDDO
C	enddo
c
C	DO JTE = 1,10
C	  WRITE(86,1086) (ACOUNT(1,J,JTE),J=1,10)
C	  PRINT 1086,(ACOUNT(1,J,JTE),J=1,10)
C	ENDDO
 1086	FORMAT(10I7)
C	DO JTE = 1,10
C	  WRITE(86,1087) (XASV(J,JTE),J=1,10)
C         PRINT 1087, (XASV(J,JTE),J=1,10) 
C	ENDDO
 1087	FORMAT(10F7.2)
C	do i = 1,256
C	  acsum = acount(i,1)+acount(i,2)+acount(i,3)
C	  if(acount(i,1).gt..5) then
C	    xpot = apmcal(3,i)	    
C	    write(67,167) i,acount(i,1),xasv(i),xpot,xdsv(i)
C	  endif
C	  if(acount(i,2).gt..5) then
C	    ypot = apmcal(4,i)	    
C	    write(68,167) i,acount(i,2),yasv(i),ypot,ydsv(i)
C	  endif
C	  if(acount(i,3).gt..5) then
C	    zpot = apmcal(5,i)	    
C	    write(69,167) i,acount(i,3),zasv(i),zpot,zdsv(i)
C	  endif
C	enddo
c	write(67,*) 'ang   no.   X_DC    Y_DC   Z_DC    X_PK    Y_PK  ',
c     1		' XDIFF   YDIFF'
c	write(67,*) ' '
 67	format(i6,f6.0,7f8.3)
 167	format(i6,f6.0,f8.3,f8.3,f8.3)
c
	type*,'generate plots at scet=',scet
c
	if(ntot.gt.0) then
C	  call plotall(iterm)
c	  call plotdrive(-1)
	  open(unit=89,file='antres.dat',status='old',access='append')
		write(89,*) ' '
		write(89,*) file(:78)
c
c		itend = s_scet(2)/100
c
  2	  format(1x,a,a,a,z8.8)
c
	  com(1) = 'printer 1'
	  write(com(2),1002) ntot-nstart
 1002	  format('lines 1 ',i5)
	  com(3) = 'ticksize .2  1. .2  1.'
	  com(4) = 'input xantres.mgo'
	  com(5) = 'limits 0. 1. 0. 1.'
	  com(6) = 'relocate .1 1.05'
c	  write(com(7),1003) file(31:39)
	  write(com(7),1003) file(31:34),file(35:36),file(37:38)
 1003	  format('label date',a5,'/',a2,'/',a2)
	  com(8) = 'hardcopy'
	  com(9) = 'end'
C	  call mongo(9,com,200,7,outmongo)
c
	  com(1) = 'printer 1'
	  write(com(2),1002) ntot-nstart
	  com(3) = 'ticksize .2  1. .2  1.'
	  com(4) = 'input yantres.mgo'
	  com(5) = 'limits 0. 1. 0. 1.'
	  com(6) = 'relocate .1 1.05'
	  write(com(7),1003) file(31:34),file(35:36),file(37:38)
	  com(8) = 'hardcopy'
	  com(9) = 'end'
C	  call mongo(9,com,200,7,outmongo)
c
	  com(1) = 'printer 1'
	  write(com(2),1002) ntot-nstart
	  com(3) = 'ticksize .2  1. 0.  0.'
	  com(4) = 'input zantres.mgo'
	  com(5) = 'limits 0. 1. 0. 1.'
	  com(6) = 'relocate .1 1.05'
	  write(com(7),1003) file(31:34),file(35:36),file(37:38)
	  com(8) = 'hardcopy'
	  com(9) = 'end'
C	  call mongo(9,com,200,7,outmongo)
	endif
 4001	PRINT*,'IERRTE=',IERRTE
	RETURN
	end

	options/extend_source
!------------------------------------------------------------------------------
	REAL*4		function	APMCAL(IANT,NTM)
	implicit	none
!	include		'apc_fit_def.for/nolist'
	real*4 		coeff(5),const(5),csq(5),corrf(5)
	integer*4 	iant,ntm
!
!	iant=1 is X_PK, 2 is Y_PK, 3 is X_DC, 4 is Y_DC, 5 is Z_DC
!
C	the following were used until Nov 1999
C	data coeff /.084848, .09678, .080956, .10424, .06669/
C	data const /13.315,  16.090,  13.069, 16.613, 9.0504/
C	data csq /-8.4573e-6, 2.2302e-5, 2.2793e-6, -9.197e-6, 5.1049e-6/	
!
	data coeff /.087818, .098714, .083789, .10633, .09728/
	data const /13.781,  16.412,  13.526, 16.954, 13.202/
	data csq /-8.7533e-6, 2.2748e-5, 2.3591e-6, -9.381e-6, 7.4466e-6/ 
c
	apmcal = coeff(iant)*ntm - const(iant) + csq(iant)*ntm**2

	return
	end
	options/extend_source
!------------------------------------------------------------------------------
	REAL*4		function	APCCAL(NTM)
C	implicit	none
	real		x
	integer*4 	ntm
	
!	preliminary calibration, needs replacement
C	apccal = .07843*ntm - 10.
C	WAS REPLACED 30 NOV 1994

	x = ntm
	apccal = -9.9874 + .07955*x - 5.0168E-5*x**2
     1	+ 9.7875E-7*x**3 - 8.4809E-9*x**4 + 3.3717E-11*x**5
     2	- 5.016E-14*x**6

	return
	end
	integer*4	function	rassign(i)
	integer*4 i,ir,irtable(8)
	data irtable /1,3,1,4,2,3,2,4/

	  ir = i.and.7
	  rassign = irtable(i+1)

	return
	end
