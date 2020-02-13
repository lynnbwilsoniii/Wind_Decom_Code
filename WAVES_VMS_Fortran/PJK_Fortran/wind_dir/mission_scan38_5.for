
	options/extend_source
!------------------------------------------------------------------------------

	PROGRAM MISSION_SCAN
C
C	include		'wind_examples:wind_tm_routine_def.for'
C	include		'wind_examples:wind_return_code_def.for'
C
        COMMON /MF37OUT/ ACOUNT,XASV,YASV,ZASV,XDSV,YDSV,ZDSV,TESV,
     1          DENSV,ETASV,SWEPOTSV
	INTEGER*4 FHIST
	character*80	stream,FILENAME
	character*120	Junk
c	CHARACTER*80	YYYYMMDD
	real*8 		scet8,scet,scetstr,scetend
	INTEGER*4 	OK,GET_STREAM_NAME,TMCHANNEL
	INTEGER*4	W_CHANNEL_OPEN,W_CHANNEL_CLOSE
	integer*4	yyyy,mon,dd,hh,mm,ss,dds
        real*4          acount(5,10,10),xasv(10,10),yasv(10,10),zasv(10,10)
        real*4          xdsv(10,10),ydsv(10,10),zdsv(10,10),TESV(10,10)
        REAL*4          DENSV(10,10),SWEPOTSV(10,10),ETASV(10,10) 
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
C	OPEN(UNIT=56,FILE='MAKEFILE37.RESULTS',STATUS='OLD',ACCESS='APPEND')
C	OPEN(UNIT=57,FILE='MAKEFILE18.VOLTS',STATUS='OLD',ACCESS='APPEND')
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
	print*,'open, TMCHANNEL,ok=',TMCHANNEL,ok
	if (ok.ne.1) stop 'Cannot open t/m channel'
c
	scet = 0.
	call w_channel_position(tmchannel,scet)
	print*,'t/m file starts at scet',scet
	dds = scet
C
C	open SWE data file and advance through header
C
        WRITE(FILENAME,123) YYYY,MON,DD
C 123   FORMAT('MONTHLY:[KELLOGG.EFLUX]FLUX_',I4.4,I2.2,I2.2,'.ASC')
C 123    FORMAT('USER_A:[KELLOGG.WIND.PAPERS]FLUX_',I4.4,I2.2,I2.2,'.ASC')
 123    FORMAT('DATA_A:[KELLOGG.WIND]SWE_H2_',I4.4,I2.2,I2.2,'.DAT')
        print*,'recheck time',scet,yyyy,mon,dd
        print*,' name of swe file ',filename
c        OPEN(UNIT=85,FILE=FILENAME,STATUS='OLD',IOSTAT=I85ERR,READONLY)
c	PRINT*,'OPEN 85, IOSTAT= ',I85ERR
c	IF (I85ERR.NE.0) THEN
c	  SCET8 = SCET8 + 1.D00
c	  ERR = W_CHANNEL_CLOSE(TMCHANNEL)
c	  GO TO 100
c	ENDIF
c	DO N = 1,9
c	  READ(85,1083) JUNK
c	  PRINT*,JUNK
c	ENDDO
c 1083	FORMAT(A)
c	WRITE(86,*) SCET,YYYY,MON,DD
C
	call makefile(tmchannel)
C	CALL MAKEFILE
	print*,'closing unit 85'
	CLOSE(UNIT=83)
	CLOSE(UNIT=84)
	CLOSE(UNIT=85)
	print*,'closing tm channel, tmchannel=',tmchannel
	OK = W_CHANNEL_CLOSE(TMCHANNEL)
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
C 
C	WRITE(89,*) SCETSTR,SCETEND
C	write(89,1088) FHIST,NHISTST
C	print*,'fhist',fhist
 1088	FORMAT(14I5) 
C
       type*,'write accumulated samples as function of angle'
C
 1087   FORMAT(10F7.2)

	print*, 'STOP IN MISSION_SCAN38'
	CLOSE(UNIT=86)
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
C	print*,'in get_, initial stream=  ',stream
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
