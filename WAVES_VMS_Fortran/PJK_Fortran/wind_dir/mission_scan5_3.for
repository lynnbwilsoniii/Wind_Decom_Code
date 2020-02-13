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
C	DATA SCET8/5313.5D00/		! JUL-19-1996 = TEST
C	DATA SCET8/5479.0D00/		! JAN-01-1997 = TEST
C	DATA SCET8/5708.0D00/		! AUG-18-1997 = 3B TEST
C	DATA SCET8/5245.5D00/		! MAY-12-1996 = START OF LONG ORB
C	DATA SCET8/5233.5D00/		! MAY-1-1996  
C	DATA SCET8/5195.D00/		! MAR-24-1996  
C	DATA SCET8/5217.D00/		! APR-15-1996 
c	DATA SCET8/5256.D00/		! MAY-23-1996 =   " LOW BIT RATE
c	DATA SCET8/5260.D00/		! MAY-27-1996 =    test
C	DATA SCET8/4741.0D00/		! JAN-01-1995 = START OF LONG ORB
C
C	WRITE(88,1012) 
C 1012	format(2X,'TIME OF CHANGE',4X,'DRV  RX  RY  RZ  VX  VY  VZ  DRV V' )
	READ(5,*) SCETSTR,SCETEND
	SCET8 = SCETSTR
	PRINT*,'DAYS',SCET8,SCETEND
	OPEN(UNIT=90,FILE='MAKEFILE5.RESULTS',STATUS='OLD',ACCESS='APPEND')
 100	continue
	call w_ur8_to_ymd(scet8,yyyy,mon,dd,hh,mm,ss,ms)
	PRINT*,'YMD',YYYY,MON,DD,HH,MM
	WRITE(stream,30) YYYY,MON,DD
 30	FORMAT(I4.4,I2.2,I2.2)
	OK = GET_STREAM_NAME(stream)
C
C	call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)
C
C	print*,'in main, stream=',stream
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
c	if(scet8.lt.4720.d00) go to 100
C	if(scet8.lt.5330.d00) go to 100    ! APPROX END OF LONG ORB AUG 1996
C	if(scet8.lt.4964.d00) go to 100    ! APPROX END OF LONG ORB AUG 1995
C	if(scet8.lt.5511.d00) go to 100    ! FEB-01-1997
c	if(scet8.lt.5262.d00) go to 100    ! MAY 29-1997
C	if(scet8.lt.5253.d00) go to 100    ! MAY 20-1996
C	if(scet8.lt.5201.d00) go to 100    ! MAR 30-1996
C	if(scet8.lt.5223.d00) go to 100    ! MAR 30-1996
c	if(scet8.lt.5286.d00) go to 100    ! ONE MONTH LARGE-1997
C	if(scet8.lt.5708.5d00) go to 100    ! TEST
	if(scet8.lt.scetend) go to 100   
	print*, 'STOP IN MISSION_SCAN6'
c	TMCHANNEL = 20
c	call makefile(tmchannel)
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
	print*,'in get_, initial stream=  ',stream
c 	if (iq .lt. 1) then
c	   stream = 'offline'
c	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then
c	   stream = 'offline'
c	else
c	   ! assume the user entered the TIME of an offline file
	   YYYYMMDD = STREAM(1:8)
	   WRITE(STREAM,30) YYYYMMDD
 30	   FORMAT('wi_lz_wav_',A8,'_v*.dat')
	   PRINT*,'in get_stream_name, file= ',STREAM
c	end if

	get_stream_name = 1

 20	return
c
  5	format(q,a)
  6	format(1x,'Enter TM stream type [O=offline (default), R=realtime ]: ',$)
  7	format(1x,'or type desired time as YYYYMMDD, e.g. 19961113  ',$)
c
	end
