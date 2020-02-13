	program		antpotential_Te

c	derived from antpotential which plots antenna potential vs flux or 
c	density
c	this is to look at effects of Te on the antennas
c
c	carrington rotation 2203 began 19 nov 1994
c	carrington rotation 2204 began 16 dec 1994
c	carrington rotation 2205 began 12 Jan 1995
c	carrington rotation 2206 began 08 Feb 1995
c	carrington rotation 2207 began 07 Mar 1995
c	carrington rotation 2208 began 04 Apr 1995
c	carrington rotation 2209 began 01 May 1995
!							       X,Y  Z
!	implicit	integer*4 (a-z)
	include		'apmplot_def.for'
	integer*4	ok,get_output_device,loop_and_gather
	integer*4	get_stream_name
	integer*4 	ret_size
	character*80	stream
c	
	ok = 1
	terminal_output = 0
	if (ok) ok = loop_and_gather()
	TYPE*,'RETURN FROM LOOP_AND_GATHER'
	iterm = -1
	iterm = 3
	iterm = -3
C	call plotall(iterm)
	stop
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_output_device()
!	implicit	none
	include		'apmplot_def.for'
	integer*4	ok
	integer*4	i,j,k
	integer*4	ios
	character*1	c

 10	write(6,'(a,$)') ' Enter output device [H=hardcopy (default), X=xwindow]: '
	read(5,'(q,a)',iostat=ios,err=10) i, c
	if (i.eq.0 .or. c .eq. 'h' .or. c.eq.'H') then
	   ok = 1
	   terminal_output = 0
c	else if (c .eq. 'x' .or. c .eq. 'X') then
c	   terminal_output = 1
c	   ok = initialize_mongo_xwindows()
	else
	   type *, 'No output device selected.'
	   ok = 0
	end if

	get_output_device = ok
	return
	end

	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
!	implicit	none
	character*(*)	stream
	common /nrblk/ nrem,NHRMN,IFASTSLOW
	integer*4	iq,NREM,NHRMN,IFASTSLOW

10	  write(6,6)
	  read(5,5,err=10,end=20) iq, stream
	write(37,*) 'read name',stream

 	if (iq .lt. 1) then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
	   stream = 'offline'
	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then
	   stream = 'realtime'
	else
	   ! assume the user entered the name of an offline file
	end if
	write(37,*) 'decided on',stream

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
c
	end
c
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	loop_and_gather()
!	implicit	none
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
	include		'apmplot_def.for'
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
	real*4		acount(260,3),xasv(260),yasv(260),zasv(260)
	real*4		xdsv(260),ydsv(260),zdsv(260)
        real*4          efluxsv(50),NPSV(1500)
        real*8          scet,scetst,delt,scet_ret,fluxtime(50),NPTIME(1500)
        real*4          xresistor(9),yresistor(9),zresistor(9) 
	integer*4	error_count,hh,min,nday
	integer*4	ihrmn,yyyy,mon,dd,mm,ss,ms,YYYYMMDD
	integer*4	iv		! drive telemetry number
	integer*4	ir,irtm		! resistor index
	integer*4	ia		! antenna index, 1=x,2=y,3=z
	integer*4	is		! sample number
	integer*4	IONOFF,nonoff	! DAC ON OR OFF
	integer*4	ndaysdone
	integer*4	major, last_major,cal_on
	real*4 		apccal,apmcal,drivev
	real*4		wt(1000)
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
	data acount /780*1.e-10/
	data xasv /260*0./
	data yasv /260*0./
	data zasv /260*0./
	DATA ELOW,EHI /9.,11./
c	data xrmeg /125./
c
 3000	ok = get_stream_name(stream)
	type*,'return from stream name:',stream
	write(37,*) 'return from stream name:',stream
!	ok = wind_tm_open_channel(ch,'realtime')
	ok = wind_tm_open_channel(ch,stream)
	TYPE*,'RETURN FROM WIND_TM_OPEN_CHANNEL, OK=',OK
	if( ok.ne.1 ) stop 'cannot open tm channel'
	call wind_tm_get_filename(ch,file)
	type *,'file: ',file(:72)
c	type *,'file head ',file(:37)
c	type *,'year ',file(31:34)
c	type *,'month ',file(35:36)
c	type *,'day ',file(37:38)

	write(37,*) 'file: ',file(:72)
c	YYYYMMDD = FILE(11:18)
c	PRINT*,'DATE',YYYYMMDD
	YYYYMMDD = 20011216
c	
C	delt = 15.d00/1440.d00             ! 15 min. interval
	delt = 2.d00/1440.d00              !  2 min. interval
	delt = 0.			   !  do not skip data
	loop_and_gather = 1
	scet = 0.
	call w_channel_position(ch,scet)
	type*,'channel, scet',ch,scet
	if(ndaysdone.eq.0) then
		scetst = 0.d00
C		year = file(38:41)
		year = file(31:34)
	        print*,'check year, check=',year
		if(year.eq.'1994') scetst = 4382.d00
		if(year.eq.'1995') scetst = 4747.d00
		if(year.eq.'1996') scetst = 5112.d00
		if(year.eq.'1997') scetst = 5478.d00
		if(year.eq.'1998') scetst = 5843.d00
		if(year.eq.'1999') scetst = 6208.d00
		if(year.eq.'2000') scetst = 6573.d00
		if(year.eq.'2001') scetst = 6939.d00
		if(year.eq.'2002') scetst = 7304.d00
		if(year.eq.'2003') scetst = 7669.d00
		if(year.eq.'2004') scetst = 8034.d00
		if(scetst.eq.0.d00) stop 'no year found'
	endif
	if(scet.eq.0.d00) scet = 4767.d00
	nday = scet
	type*,'position channel pointer to',scet
c	ok = w_channel_position(ch,scet)
c	if ( ok.eq.1 ) then
c		type*,'channel pointer positioned to',scet
c	else
c		 type*,'cannot position pointer'
c	endif
c
        call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
        print*,'check time',scet,yyyy,mon,dd,mm
C
C
C       THIS SECTION READS IN STUART'S DATA AND FINDS THE CLOSEST
C       OF THE CDF DATA
C
C               Flux is read every 96 sec.
C
        WRITE(FILENAME,123) YYYY,MON,DD
C 123   FORMAT('MONTHLY:[KELLOGG.EFLUX]FLUX_',I4.4,I2.2,I2.2,'.ASC')
 123    FORMAT('USER_A:[KELLOGG.WIND.PAPERS]FLUX_',I4.4,I2.2,I2.2,'.ASC')
        print*,'recheck time',scet,yyyy,mon,dd
        print*,' name of flux file ',filename
       OPEN(UNIT=83,FILE=FILENAME,STATUS='OLD',READONLY)
C
C               np is read every 3 sec.
C
        WRITE(FILENAME,124) YYYY,MON,DD
 124    FORMAT('USER_A:[KELLOGG.WIND.PAPERS]NP_',I4.4,I2.2,I2.2,'.ASC')
        OPEN(UNIT=84,FILE=FILENAME,STATUS='OLD',READONLY)
C
        NSS = 0
        DO NS =  1,50
C         READ IN A TABLE OF STUART'S ACCURATE FLUXES
C
          READ(83,*,END=142,ERR=150) NYR,MO,MONDAY,NHR,MIN,NSEC,EFLUXT
          NSS = NSS+1
          FDAYF = NHR/24. + MIN/1440. + NSEC/86400.
          print*,'nyr,MO,MONDAY,min,eflux',nyr,MO,MONDAY,min,efluxt
          FLUXTIME(NSS) = DFLOAT(NDAY) + DBLE(FDAYF)
c         FLUXTIME(NSS) = SCET
          EFLUXSV(NSS) = EFLUXT
 150      CONTINUE
        ENDDO
 142    continue
        print*,' stuarts lines read in',nss,'  last ',fdayf
          STRADDLEF = (FLUXTIME(2)-SCET)*(FLUXTIME(1)-SCET)
          INDEXF = 1
C
        NPS = 0
        DO ND =  1,1500
C         READ IN A TABLE OF STUART'S ACCURATE PROTON DENSITIES
C
          INDEXN = 1
          READ(84,*,END=152,ERR=160) NYR,MO,MONDAY,NHR,MIN,NSEC,NPT
          NPS = NPS+1
          FDAYF = NHR/24. + MIN/1440. + NSEC/86400.
C         print*,'nyr,MO,MONDAY,min,eflux',nyr,MO,MONDAY,min,efluxt
          NPTIME(NPS) = DFLOAT(NDAY) + DBLE(FDAYF)
c          NPTIME(NPS) = SCET
          NPSV(NPS) = NPT
 160      CONTINUE
        ENDDO
 152    CONTINUE
c
        print*,'flux and np read in'
          STRADDLEN = (NPTIME(2)-SCET)*(NPTIME(1)-SCET)
          INDEXN = 1
c	
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

	type *, 'Please wait, gathering data and making plots...'

	go to 1010

 1000	   continue
c
c	get a new event
c
	ok = w_event(ch,event)
	item = 'EVENT_SCET'
	okt = w_item_i4(ch, item, s_scet, 2, ret_size)
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
	 if(is.ge.sz_x_dim) then
	   print*,'go to 2000 after is.ge.sz_x_dim'
	   go to 2000
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
c
	istuart = 0
	if(istuart.ne.1) then
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(ch, item, dens3dp, 1, ret_size)
	  print*,'3dp ion dens,size,val',dens3dp,ret_size
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(ch, item, ETEMP, 1, ret_size)
	  print*,'dens,e temp',dens3dp,etemp
	  if(etemp.gt.0.) then
	    flux = dens3dp*sqrt(etemp)
	  else
	    go to 1000
	  endif
	else
	  item = 'EVENT_SCET_R8'
	  okt = w_item_r8(ch, item, scet, 1, ret_size)
	  fday = dmod(scet,1.d00)
	  call tdpflux(yyyymmdd,fday,fluxst,dens,etemp)
	  flux = fluxst/1.68e7
	endif
C
C	check that 3DP accurate data is available
C
	IF(SCET.GE.FLUXTIME(1).AND.SCET.LE.FLUXTIME(NSS)) THEN
C
          STRADDLEF = (FLUXTIME(2)-SCET)*(FLUXTIME(1)-SCET)
c
          DOWHILE(STRADDLEF.GT.0.)
        print*,'straddle',indexf,straddlef
        print*,'cont    ',fluxtime(indexf),scet,fluxtime(indexf+1)
            INDEXF = INDEXF+1
            STRADDLEF = (FLUXTIME(INDEXF+1)-SCET)*(FLUXTIME(INDEXF)-SCET)
          ENDDO
	 eflux = .5*(efluxsv(indexf) + efluxsv(indexf+1))
	ENDIF
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
	print*,'electron temperature, ev',etempev
	IF(ETEMPEV.LT.ELOW.OR.ETEMPEV.GT.EHI) GO TO 1000
C
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
	   if(s_scet(2).gt.235000) go to 2000
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
	   if(is.gt.0) then
	     ok = w_item_i4(ch, item, z_apc(is), 1, ret_size)
	     if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok
c	     ONOFF(ix) = (.not. Z_APC(IS)) .and. 1
	     if(z_apc(is).ne.1) go to 1000
	   endif
c

           item = 'APC_X_RESISTOR'              ! word 69
           ok = w_item_i4(ch, item, x_resistor, 1, ret_size)
           if (ok) then
                irtm = x_resistor
                ir = rassign(irtm)
c                resistor(1,is) = ir
		XRMeg = xresistor(ir)
           endif  


c	   xtime(is) = (scet - scetst)
	   xtime(is) = dfloat(nday) + hh/24. + min/1440. + SS/86400.
     1        -scetst


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
	   ok = w_item_i4(ch, item, x_apc(is), 1, ret_size)
	   if (.not. ok) write(6,2) 'cannot get item ',item, ', ok=', ok
	   if(x_apc(is).ne.1) go to 1000

	  n3dp = ret_size
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
	  ok = w_item_r4(ch, item, ETEMP, 1, ret_size)
C
	print*,'3dp,vx3,vy3,vz3,ptemp,etemp',vx3,vy3,vz3,ptemp,etemp
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
	   if (ok) then
		av(1,is) = apmcal(3,x_dc)
		acount(x_dc,1) = acount(x_dc,1) + 1
		xasv(x_dc) = xasv(x_dc) + flux
		xdsv(x_dc) = xdsv(x_dc) + dens3dp
c	   else
c		write(6,2) 'cannot get item ',item, ', ok=', ok
	   endif

C	   item = 'Y_APC'		! word    69
	   item = 'APC_Y'		! word 69
	   ok = w_item_I4(ch, item, y_apc(is), 1, ret_size)
	   if(y_apc(is).ne.1) go to 1000

C	   item = 'Y_PEAK'		! word 64
	   item = 'APM_Y_PEAK'		! word 64
	   ok = w_item_i4(ch, item, y_peak, 1, ret_size)
	   if (ok) then
		is = MIN0(count(2)+1,sz_x_dim)
		count(2) = is
		pk(2,is) = apmcal(2,y_peak)
	   endif

	   item = 'APM_Y_DC'		! word 66
	   ok = w_item_i4(ch, item, y_dc, 1, ret_size)
	   if (ok) then
		av(2,is) = apmcal(4,y_dc)
		acount(y_dc,2) = acount(y_dc,2) + 1
		yasv(y_dc) = yasv(y_dc) + flux
		ydsv(y_dc) = ydsv(y_dc) + dens3dp
	   endif
C
	   item = 'APM_Z_DC'		! word 67
	   ok = w_item_i4(ch, item, z_dc, 1, ret_size)
	   if (ok) then
		is = MIN0(count(3)+1,sz_x_dim)
		count(3) = is
		av(3,is) = apmcal(5,z_dc)
		acount(z_dc,3) = acount(z_dc,3) + 1
		zasv(z_dc) = zasv(z_dc) + flux
		zdsv(z_dc) = zdsv(z_dc) + dens3dp
	   endif
c
	if(is.le.2) then
	   print*,'antenna angle,apc_x,y,z',iangle,x_apc(is),y_apc(is)
     1		,z_apc(is)
	endif
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
	  nswe = ret_size
c
c
C       Bias current in microamps, positive sign is electrons to antenna,
C               i.e. current from antenna
        Xbias = (av(1,is) - drive(is))/XRMeg
        print*, 'bias',is,scet,Xbias
C
c	  print*,'e temp',etemp
c	  if(etemp.gt.0.) then
c	    flux = dens3dp*sqrt(etemp)
C	    write(88,1088) is,flux,av(1,is),av(2,is),av(3,is)
C     1		,pk(1,is),pk(2,is)
 1088	    format(i5,f10.4,5f10.5)
c	  endif
	if(x_apc(is).eq.0) then
	  ntot = is
	  itend = s_scet(2)/100
	  if(nstart.eq.0) nstart = is
	  if(istart.eq.0) then
	  	itstart = s_scet(2)/100
	 	istart = 1
	  else
	    ist = is - nstart 
	    outmongo(ist,1) = is
	    outmongo(ist,2) = drivev
	    outmongo(ist,3) = av(1,is)
	    outmongo(ist,4) = pk(1,is)
	    outmongo(ist,5) = av(2,is)
	    outmongo(ist,6) = pk(2,is)
	    outmongo(ist,7) = av(3,is)
	  endif
	endif
 1549	format(i6,6f8.2)

 2001	GO TO 1000
C 	end do
 2000	continue

	type*,'write accumulated samples as function of angle'
	do i = 1,260
	  xasv(i) = xasv(i)/acount(i,1)
	  yasv(i) = yasv(i)/acount(i,2)
	  zasv(i) = zasv(i)/acount(i,3)
	  xdsv(i) = xdsv(i)/acount(i,1)
	  ydsv(i) = ydsv(i)/acount(i,2)
	  zdsv(i) = zdsv(i)/acount(i,3)
	enddo
c
	do i = 1,256
C	  acsum = acount(i,1)+acount(i,2)+acount(i,3)
	  if(acount(i,1).gt..5) then
	    xpot = apmcal(3,i)	    
	    write(67,167) i,acount(i,1),xasv(i),xpot,xdsv(i)
	  endif
	  if(acount(i,2).gt..5) then
	    ypot = apmcal(4,i)	    
	    write(68,167) i,acount(i,2),yasv(i),ypot,ydsv(i)
	  endif
	  if(acount(i,3).gt..5) then
	    zpot = apmcal(5,i)	    
	    write(69,167) i,acount(i,3),zasv(i),zpot,zdsv(i)
	  endif
	enddo
c	write(67,*) 'ang   no.   X_DC    Y_DC   Z_DC    X_PK    Y_PK  ',
c     1		' XDIFF   YDIFF'
c	write(67,*) ' '
 67	format(i6,f6.0,7f8.3)
 167	format(i6,f6.0,f8.3,f8.3,f8.3)
c
	type*,'generate plots at scet=',scet


	   write(37,*) 'last sample, scet',s_scet
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
	  call mongo(9,com,200,7,outmongo)
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
	  call mongo(9,com,200,7,outmongo)
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
	  call mongo(9,com,200,7,outmongo)
	endif
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
	SUBROUTINE GETSWE(FILENAME,SCET,ETEMP,SCPOT)
C
	REAL*8 SCET
	CHARACTER*20 FILENAME
	DATA IOPEN /0/
C
C	this is for possible use on SWE files
C
	IF(IOPEN.EQ.0) OPEN(UNIT=55,FILE=FILENAME,STATUS='OLD')
 1055	FORMAT(I3,X,I2,X,I4,I3,X,I2,X,F6.3,F16.0,F16.5)
	RETURN
	END
