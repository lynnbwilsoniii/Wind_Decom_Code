	PROGRAM COMPARE_FLUX3
C
C	COMPARE ACCURATE FLUXES FROM STUART AND CDF DATA
C
	include		'wind_examples:wind_tm_routine_def.for'
	include		'wind_examples:wind_return_code_def.for'
C
	CHARACTER*50 	FILENAME
	character*80	stream
	character*32	item
	character*4	event
	INTEGER*4 	DATE,YYYYMMDD,ANGLE
	integer*4	ok,okt,return_size,CDFCH
	real*4		etemp(100),EFLUX(100),dens3dp(100),swedens(100)
	real*8		scetcdf,SWETIME(100),TIME3DP(100),FLUXTIME(100)
	REAL*8		NPTIME,TDIFF,TRIAL
	data event /'CDF'/
	DATA TWOPI /6.28318531/
	DATA BOLTZK /1.3807E-16/
	DATA CLIGHT /2.9979E10/
	DATA EVME /.511E6/
	DATA GMME / 9.10939E-28/
c
	OPEN(UNIT=72,FILE='COMPAREFLUX.RESULTS',ACCESS='APPEND')
C
 10	write(6,6)
  6	format(1x,'Enter TM stream name: ',$)
c	read(5,5,err=10,end=200) iq, stream
	read(5,*,err=10,end=200) yyyymmdd
  5	format(q,a)
	print*,'input ',yyyymmdd
	write(stream,1001) yyyymmdd
 1001	format('wi_lz_wav_',i8.8,'_v*.dat')
	print*,'stream ',stream
	ok = w_channel_open(cdfch,stream)
	if (ok.ne.1) stop 'Cannot open cdf channel'
C
	scetcdf = 0.
	call w_channel_position(cdfch,scetcdf)
	print*,'cdf file starts at scetcdf',scetcdf
	ndayfl = scetcdf
c	now add nearly two hours, as stuart's files start at 0200
	scetcdf = scetcdf + 1.9d00/24.d00
	call w_channel_position(cdfch,scetcdf)
	print*,'cdf file set to',scetcdf
C
 1000 	FORMAT(A)
	IS = 0
C
 100	CONTINUE
	  ok = w_event(cdfch,'CDF')
	  if (ok.ne.1) type*, 'cannot get CDF event,ok=',ok
	  item = 'WIND_SWE_DENSITY_R4'
	  ok = w_item_r4(cdfch, item, swedens, 100, return_size)
	  item = 'WIND_SWE_SCET_R8'
	  ok = w_item_r8(cdfch, item, swetime, 100, return_size)
	  item = 'WIND_3DP_ION_DENSITY_R4'
	  ok = w_item_r4(cdfch, item, dens3DP, 100, return_size)
	  item = 'WIND_3DP_E_TEMP_R4'
	  ok = w_item_r4(cdfch, item, ETEMP, 100, return_size)
	  item = 'WIND_3DP_SCET_R8'
	  ok = w_item_r8(cdfch, item, time3dp, 100, return_size)
C
	do is = 1,50
	  write(66,*) time3dp(is),dens3dp(is),etemp(is)
	enddo
C	  if(is.gt.20) stop
C	  go to 100
C
c	FLUX = 1.68E7*FLUX
C	
C
C	THIS SECTION READS IN STUART'S DATA AND FINDS THE CLOSEST
C	OF THE CDF DATA
C
	WRITE(FILENAME,123) YYYYMMDD
C 123	FORMAT('MONTHLY:[KELLOGG.EFLUX]FLUX_',I8.8,'.ASC')	
 123	FORMAT('USER_A:[KELLOGG.WIND.PAPERS]FLUX_',I8.8,'.ASC')	
	OPEN(UNIT=83,FILE=FILENAME,STATUS='OLD',READONLY)
C
	WRITE(FILENAME,124) YYYYMMDD
 124	FORMAT('USER_A:[KELLOGG.WIND.PAPERS]NP_',I8.8,'.ASC')	
C	OPEN(UNIT=84,FILE=FILENAME,STATUS='OLD',READONLY)
C
	NSS = 0
	DO NS =  1,50
	  READ(83,*,END=140,ERR=150) NYR,MO,NDAY,NHR,MIN,NSEC,EFLUXT
	  NSS = NSS+1
	  FDAYF = NHR/24. + MIN/1440. + NSEC/86400.
C	  print*,'nyr,MO,NDAY,NSEC,eflux',nyr,MO,NDAY,NSEC,efluxt
     	  FLUXTIME(NSS) = DFLOAT(NDAYFL) + DBLE(FDAYF)
	  EFLUX(NSS) = EFLUXT
 150	  CONTINUE
	ENDDO
 140	continue
	print*,' lines read in',nss,'  last ',fdayf
C	
C	SEARCH CDF DATA (NCDF = 1,100) FOR CLOSEST TIME
C
	DO NS = 1,NSS
	  TDIFF = 1.D00
	  DO NCDF = 1,100
C	  print*,'time3dp,flux',time3dp(ncdf),ns,fluxtime(ns)
	    IF(TIME3DP(NCDF).GT.FLUXTIME(NS)) GO TO 160
	    TRIAL = ABS(FLUXTIME(NS)-TIME3DP(NCDF))
	    IF(TRIAL.LT.TDIFF) THEN
		TDIFF = TRIAL
	        NBEST = NCDF
	    ENDIF
	  ENDDO
 160	  CONTINUE
C
C	  NOW NBEST IS THE CDF DATA NEARET STUART'S FLUX

	PRINT*,'ns,fluxtime,ncdf,time3dp ',ns,FLUXTIME(ns)
     1		,ncdf,time3dp(ncdf)
	  IF(DENS3DP(NBEST).LE.0.) GO TO 162 
	  XTEMP = EFLUX(NS)/DENS3DP(NBEST)
	  EFFTEMP = TWOPI*EVME*(XTEMP/CLIGHT)**2	! IN EV
	  RATIO = 1.
	  IF(ETEMP(NBEST).GT.0.) RATIO = EFFTEMP/ETEMP(NBEST)
	  WRITE(72,1072) YYYYMMDD,FLUXTIME(NS),DENS3DP(NBEST),EFLUX(NS),
     1	    ETEMP(NBEST),EFFTEMP,RATIO
 1072	  FORMAT(I9,F12.5,F8.3,E12.4,E12.4,F10.4,E12.4)
  162	  CONTINUE
	ENDDO
C
C	READ(84,*,END=141,ERR=140) NYR,MO,NDAY,NHR,MIN,NSEC,DENSI
C	print*,'nyr,MO,NDAY,NSEC,DENSI',nyr,MO,NDAY,NSEC,DENSI
C	FDAYN = NHR/24. + MIN/1440. + NSEC/86400.
C	NPTIME = 365.D00*DFLOAT(NYR-1982) + 30.D00*DFLOAT(MO) + 
C     1	  DFLOAT(NDAY) + DBLE(FDAYN)
C	PRINT*,FLUXTIME,NPTIME
C	IF(ABS(FDAYF-FDAYN).GT.1.E-7) STOP 'UNEQUAL FLUX AND NP TIMES'
C
 141	CONTINUE
	OK = W_CHANNEL_CLOSE(CDFCH)
	GO TO 10
C
 200	CONTINUE
	PRINT*,'END OF .COM INPUT'
	CLOSE(UNIT=83)
C	CLOSE(UNIT=84)
	CLOSE(UNIT=72)
	STOP
	END
