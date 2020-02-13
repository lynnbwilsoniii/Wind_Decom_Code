	FUNCTION TDSCAL(CHANNEL,SPEED,TM)
C
C	this function converts the telemetry number TM from the TDS of
C	Wind-Waves to volts--voltage output from the preamplifiers.
C	If voltage input, i.e. voltage on the antenna, is desired, some
C	further calculation to correct for preamp gain as a function of
C	frequency is required.  
C		As there are 5 different A/D converters,
C	it is necessary to specify the channel  (1 to 6, ITEM = 'CHANNEL')  
C	For channels 1 and 2, the fast channels, it is also necessary to 
C	specify the speed, i.e. the sample rate (ITEM = "RX_SPEED")
C
	INCLUDE 	'TDS_CALDATA.FOR'
C
C	COMMON /TCALDATA/ A(6),B(6),POFFS(6),NOFFS(6)
C	INTEGER*4 CHANNEL,SPEED,TM,INDEX
C	REAL TDSCAL,A,B,POFFS,NOFFS
C	DATA B /.08316,.08151,.08099,.08212,.08293,.08293/
C	DATA A /3.19E-4,2.14E-4,3.89E-4,2.40E-4,1.75E-4,1.94E-4/
C	DATA POFFS /4.01,6.31,4*0./
C	DATA NOFFS /1.59,1.79,4*0./
C
	INDEX = MIN0(2*CHANNEL + MIN0(SPEED,1) - 1, 5)
	TMNB = TM - 128
	IF(INDEX.EQ.5.AND.ABS(TMNB).LT.78.) INDEX = 6
C
C	INDEX = 1 IS CHANNEL 1, FASTEST, 2 IS CHANNEL 1, SLOWER, 3 IS
C	CHANNEL 2 FASTEST, 4 IS CHANNEL 2 SLOWER, 5,6 ARE CHANNELS 3-6,
C	5 FOR TM GT. 78, 6 FOR TM LT 78
C
	IF(TMNB.GT.0.) THEN
	  TDSCAL = A(INDEX)*(EXP(B(INDEX)*TMNB) - POFFS(INDEX))
	ELSE
	  TDSCAL = -A(INDEX)*(EXP(-B(INDEX)*TMNB) - NOFFS(INDEX))
	ENDIF
	RETURN
	END

