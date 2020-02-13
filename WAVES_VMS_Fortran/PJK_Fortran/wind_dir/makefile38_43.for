	SUBROUTINE MAKEFILE(CH)
C
C	THIS IS MAKEFILE38
C
C	AN ADAPTATION OF MAKEFILE37 TO COMPARE SWE AND STUART'S E TEMP
C		FOR A GIVEN DATE, OPEN STUART FILE AND, IF OK, SET IERR3DP
C		FIND CORRESPONDING NP
C		OPEN CORRESPONDING SWE FILE, RETURN IF NO SUCH FILE
C		WRITE DATE,TIME,FLUX,NP,TE3DP,SWE N, SWE E TEMP TO FOR087.DAT
C
C	SWE data in unit 85, format 1084
C
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
	character*120	junk
        real*8          scet,SCET3DP,SCETSWE,SCETNP,scet_ret
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
	data is /0/
	data ntot /0/
	data error_count /0/
	data ndaysdone /0/
	data nstart /0/
	data istart /0/
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
	nday = scet

        call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
	type*,'position channel pointer to',scet
        print*,'check time',scet,yyyy,mon,dd,mm
C
C       THIS SECTION OPENS AND READS IN AND READS ONE LINE 
C       OF THE 3DP FLUX DATA FROM STUART
C
C               Flux is read every 96 sec.
C
        WRITE(FILENAME,123) YYYY,MON,DD
C 123   FORMAT('MONTHLY:[KELLOGG.EFLUX]FLUX_',I4.4,I2.2,I2.2,'.ASC')
 123   FORMAT('USER_A:[KELLOGG.WIND.PAPERS]FLUX_',I4.4,I2.2,I2.2,'.ASC')
        print*,'recheck time',scet,yyyy,mon,dd
        print*,' name of flux file ',filename
        OPEN(UNIT=83,FILE=FILENAME,STATUS='OLD',IOSTAT=IERR3DP,READONLY)
	IF(IERR3DP.NE.0) THEN
	  PRINT*,'NO FLUX DATA, IERR3DP=',IERR3DP
C		GO TO 1123
	  RETURN
	ENDIF
        READ(83,*) NYR,MO,MONDAY,NHR,MIN,NSEC,EFLUXSV
        call w_ur8_from_ymd(scet3DP,NYR,mo,MONDAY,NHR,mIN,NSEC,0)
	EFLUXT = EFLUXSV
C
C         OPEN AND READ IN ONE OF STUART'S ACCURATE PROTON DENSITIES
C               np is read every 3 sec.
C
        WRITE(FILENAME,124) YYYY,MON,DD
 124    FORMAT('USER_A:[KELLOGG.WIND.PAPERS]NP_',I4.4,I2.2,I2.2,'.ASC')
        OPEN(UNIT=84,FILE=FILENAME,STATUS='OLD',READONLY)
        READ(84,*) NYR,MO,MONDAY,NHR,MIN,NSEC,PDENSSV
        call w_ur8_from_ymd(scetNP,NYR,mo,MONDAY,NHR,mIN,NSEC,0)
C
C       OPEN AND READ JUNK AND ONE LINE OF THE SWE DATA FROM CDAWEB
C
C               Data are every 21 sec.
C
        WRITE(FILENAME,125) YYYY,MON,DD
 125    FORMAT('DATA_A:[KELLOGG.WIND]SWE_H2_',I4.4,I2.2,I2.2,'.DAT') 
        OPEN(UNIT=85,FILE=FILENAME,STATUS='OLD',IOSTAT=IERRSWE,READONLY)
	IF(IERR3DP.NE.0) THEN
	  PRINT*,'SWE READIN ERROR, IERRSWE=',IERRSWE
	  RETURN
	ENDIF
C
C	READ IN JUNK AND ONE LINE
C
        DO N = 1,9
          READ(85,1083) JUNK
          PRINT*,JUNK
        ENDDO
 1083   FORMAT(A)
c        WRITE(86,*) SCET,YYYY,MON,DD
C	read (85,1084),dd,mon,yyyy,hh,mm,ss,ms,etempsv,edenssv,scpotsv
c
	read (85,1084,END=4001),
     1		dd,mon,yyyy,hh,mm,ss,ms,etempsv,edenssv,scpotsv
	print*,'read chk',etempsv,edenssv,scpotsv 
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

C	type *, 'Please wait, gathering data and making plots...'

	go to 1010

 1000	   continue
c
C	READ IN A FLUX POINT, FLUX TIME CONTROLS TIME
C
        READ(83,*,END=4001,ERR=1000) NYR,MO,MONDAY,NHR,MIN,NSEC,EFLUXT
        call w_ur8_from_ymd(scet3DP,NYR,mo,MONDAY,NHR,mIN,NsEC,0)
	print*,'83 chk',mo,monday,nhr,min,nsec
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

c	get corresponding event
c
	call w_channel_position(ch,scet3DP)
	ok = w_event(ch,event)
C
C	FIND CORRESPONDING 3DP DENSITY
C
	print*,'np time check',scetnp,scet3dp
	DOWHILE(SCETNP.LT.SCET3DP) 
          READ(84,*,END=4001,ERR=2084) NYR,MO,MONDAY,NHR,MIN,NSEC,PDENS
 2084	  CONTINUE
          print*,'nyr,MO,MONDAY,min,PDENS',nyr,MO,MONDAY,min,PDENS
          call w_ur8_from_ymd(scetNP,NYR,mo,MONDAY,NHR,mIN,NSEC,0)
	ENDDO
	ETEMPST = EFLUXT/PDENS
	PRINT*,'NP CHK',SCET3DP,SCETNP,PDENS
C
C 01-03-2000 02:00:08.169       123029.          4.59844       2.46818
C 01-03-2000 02:00:29.850       123505.          4.84017       1.27711 
C
C	print*,'check swe readin ',scetswe,scet
	IF(SCETSWE.LT.SCET3DP) THEN
	  DOWHILE(SCETSWE.LT.SCET3DP)
	    ETEMPSV = ETEMP
	    EDENSSV = EDENS
	    SCPOTSV = SCPOT
	    read (85,1084,END=4001),
     1		dd,mon,yyyy,hh,mm,ss,ms,etemp,edens,scpot
 1084	    format(i2,x,i2,x,i4,i3,x,i2,x,i2,x,i3,f14.0,f17.5,f15.5)
C	    print*,'read chk',etemp,edens,scpot
            call w_ur8_from_ymd(scetswe,yyyy,mon,dd,hh,mm,ss,ms)
C	    print*,'ur8 chk',scetswe
	  ENDDO
	ENDIF
C

	  ETEMPEV = (EFLUXT/1.68E7/PDENS)**2
	  print*,'3DP electron temperature, ev',etempev
	PRINT*,'Te CHK',SCET3DP,SCETSWE,ETEMP/11600.,ETEMPEV

	istuart = 0
	if(istuart.ne.1) then
	  item = 'WIND_3DP_ION_density_R4'
	  ok = w_item_r4(ch, item, dens3dp, 1, ret_size)
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
C	ATTEMPT TO CORRECT SWE TEMPERATURES
	etempcorr = (etemp/11600.)*pdens/edens
	PRINT*,'NP CHK',SCET3DP,SCETNP,PDENS,DENS3DP
C
C          item = 'X_DC'                ! word 65
           item = 'APM_X_DC'            ! word 65
           ok = w_item_i4(ch, item, x_dc, 1, ret_size)
           X_DC_V = apmcal(3,x_dc)
C
	write(39,1139) scet3dp,SCETSWE
     1	,Pdens,EDENS,etemp/11600.,etempev,ETEMPCORR,X_DC_V,SCPOT
 1139	FORMAT(2F11.4,2F6.2,5F7.2)
	if(1) go to 1000
c	IF(1) STOP
c
	  ETEMPEV = (EFLUX/1.68E7/DENSI)**2
	  print*,'electron temperature, ev',etempev
C	  IF(ETEMPEV.LT.ELOW.OR.ETEMPEV.GT.EHI) GO TO 10
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
	   if(s_scet(2).gt.235800) go to 2000
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
C
c	  print*,'e temp',etemp
c	  if(etemp.gt.0.) then
c	    flux = dens3dp*sqrt(etemp)
C	    write(88,1088) is,flux,av(1,is),av(2,is),av(3,is)
C     1		,pk(1,is),pk(2,is)
 1088	    format(i5,f10.4,5f10.5)
c	  endif
 1549	format(i6,6f8.2)
c
 2001	GO TO 1000
C 	end do
 2000	continue

C	type*,'write accumulated samples as function of angle'
C	
 1087	FORMAT(10F7.2)
C
 4001	RETURN
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
