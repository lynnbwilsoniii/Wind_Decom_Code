	options/extend_source
!------------------------------------------------------------------------------

	PROGRAM MISSION_SCAN
C
C	include		'wind_examples:wind_tm_routine_def.for'
C	include		'wind_examples:wind_return_code_def.for'
C
	character*80	stream,fluxfile
	real*8 		scet8,scet,scetstr,scetend
	INTEGER*4 	OK,GET_STREAM_NAME,TMCHANNEL
	INTEGER*4	W_CHANNEL_OPEN,W_CHANNEL_CLOSE
	integer*4	yyyy,mon,dd,hh,mm,ss,dds
C
C	DATA SCET8/4697.5D00/		! NOV-11-1994 = TURN ON DAY
C	DATA SCET8/5245.5D00/		! MAY-12-1996 = START OF LONG ORB
c	DATA SCET8/5256.D00/		! MAY-23-1996 =   " LOW BIT RATE
c	DATA SCET8/5607.D00/		! MAY-09-1997 = START OF R MEAS.
C	DATA SCET8/4741.0D00/		! JAN-01-1995 = START OF LONG ORB
C
	READ(5,*) SCETSTR,SCETEND
	SCET8 = SCETSTR
	PRINT*,'DAYS',SCET8,SCETEND
 100	continue
	call w_ur8_to_ymd(scet8,yyyy,mon,dd,hh,mm,ss,ms)
c	PRINT*,'YMD',YYYY,MON,DD,HH,MM
	WRITE(stream,30) YYYY,MON,DD
 30	FORMAT(I4.4,I2.2,I2.2)
	print*,'check stream before call ',stream
	WRITE(FLUXFILE,1077) STREAM
 1077	FORMAT('[KELLOGG.WIND.PAPERS]FLUX_',A8,'.ASC')
	PRINT*,'FLUXFILE',FLUXFILE
C
	OK = GET_STREAM_NAME(stream)
C
c	print*,'in main, stream=',stream
	print*,'after call ',stream
C
	ok = w_channel_open(tmchannel,stream)
c	print*,'open, TMCHANNEL,ok=',TMCHANNEL,ok
	if (ok.ne.1) stop 'Cannot open t/m channel'
	IF(SCETEND-SCET8.GT.6.) OK = W_MESSAGES_OFF(TMCHANNEL)
C
c	open results file
c
	OPEN(UNIT=77,FILE=MAKEFILE34.RESULTS,STATUS='OLD',READONLY)
C
c
	scet = 0.
	call w_channel_position(tmchannel,scet)
c	print*,'t/m file starts at scet',scet
	dds = scet
	call makefile(tmchannel)
	ok = w_channel_close(tmchannel)
C	scet8 = scet8 + 1.d00
	scet8 = scet8 + 2.d00
C
C	if(scet8.lt.5223.d00) go to 100    ! MAR 30-1996
c	if(scet8.lt.5286.d00) go to 100    ! ONE MONTH LARGE-1997
C	if(scet8.lt.5708.5d00) go to 100    ! TEST
C
	if(scet8.lt.scetend) go to 100   
	print*, 'STOP IN MISSION_SCAN34'
	STOP
	END
	options/extend_source
!------------------------------------------------------------------------------
	integer*4	function	get_stream_name(stream)
! This routine gets the user's TM stream type specification.
!
	implicit	none
	character*(*)	stream
	CHARACTER*80	YYYYMMDD
	integer*4	iq
	DATA IQ /1/
C
C10	  write(6,6)
C	  write(6,7)
C	  read(5,5,err=10,end=20) iq, stream
c	print*,'in get_, initial stream=  ',stream
c 	if (iq .lt. 1) then
c	   stream = 'offline'
c	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
c	   stream = 'offline'
c	else
c	   ! assume the user entered the TIME of an offline file
	   YYYYMMDD = STREAM(1:8)
	   WRITE(STREAM,30) YYYYMMDD
 30	   FORMAT('wi_lz_wav_',A8,'_v*.dat')
c	   PRINT*,'in get_stream_name, file= ',STREAM
c	end if

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
  7	format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
	end
