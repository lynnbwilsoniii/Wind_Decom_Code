	options/extend_source
!------------------------------------------------------------------------------

	PROGRAM MISSION_SCAN
C
C	include		'wind_examples:wind_tm_routine_def.for'
C	include		'wind_examples:wind_return_code_def.for'
C
	character*80	stream
c	CHARACTER*80	YYYYMMDD
	real*8 		scet8,scet,scetstr,scetend
	INTEGER*4 	OK,GET_STREAM_NAME,TMCHANNEL
	INTEGER*4	W_CHANNEL_OPEN
	integer*4	yyyy,mon,dd,hh,mm,ss,dds
C
C	DATA SCET8/4697.5D00/		! NOV-11-1994 = TURN ON DAY
C
	WRITE(77,*) ' '
	WRITE(77,*) ' '
	WRITE(77,1077)
 1077	FORMAT(3X,'   DATE   TIME',3X,'SPS',3X,'TDSF',6X,'SPS',6X,'-TDSS-')
	READ(5,*) SCETSTR,SCETEND
	SCET8 = SCETSTR
	PRINT*,'DAYS',SCET8,SCETEND
C	OPEN(UNIT=90,FILE='MAKEFILE5.RESULTS',STATUS='OLD',ACCESS='APPEND')
 100	continue
	call w_ur8_to_ymd(scet8,yyyy,mon,dd,hh,mm,ss,ms)
	PRINT*,'YMD',YYYY,MON,DD,HH,MM
	WRITE(stream,30) YYYY,MON,DD
 30	   FORMAT('wi_lz_wav_',I4.4,I2.2,I2.2,'_v*.dat')
	   PRINT*,'in get_stream_name, file= ',STREAM
C
C	call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
C
	ok = w_channel_open(tmchannel,stream)
C
C	print*,'open, TMCHANNEL,ok=',TMCHANNEL,ok
	if (ok.ne.1) stop 'Cannot open t/m channel'
c
	scet = 0.
	call w_channel_position(tmchannel,scet)
	print*,'t/m file starts at scet',scet
	dds = scet
	call makefile(tmchannel)
	scet8 = scet8 + 1.d00
	if(scet8.lt.scetend) go to 100   
	print*, 'STOP IN MISSION_SCAN12'
	STOP
	END
