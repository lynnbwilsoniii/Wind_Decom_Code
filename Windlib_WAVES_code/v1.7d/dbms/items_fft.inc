! items_fft.inc - WIND/WAVES Item Database: item definitions common to all
! FFT event types
!

ITEM=INSTR_HEADER    
	FIXED_VALUE=1:0   
	DESC="LENGTH OF INSTRUMENT HEADER"
	AUTHOR_DATE=19931124
ITEM=MEASURE_HEADER  
	FIXED_VALUE=1:16   
	DESC=!LENGTH OF MEASUREMENT HEADER!
	AUTHOR_DATE=19931124
ITEM=PACKET_HEADER   
	FIXED_VALUE=1:8   
	DESC="SIZE OF PACKET HEADER"
	AUTHOR_DATE=19931124
!
!  Frequency/Timing items
!
ITEM=NUMBER_OF_SAMPLES
	DATA_TYPE=INTEGER*4
	FIXED_VALUE=1:1024
	AUTHOR_DATE=19951017
	DESC=Time series sample size (samples)

ITEM=SAMPLE_PERIOD_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:1.0/SAMPLE_FREQUENCY_R4
	AUTHOR_DATE=19951017
	DESC=Time between time series samples (Seconds)

ITEM=TIME_SERIES_PERIOD_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:SAMPLE_PERIOD_R4*NUMBER_OF_SAMPLES
	AUTHOR_DATE=19960323
	DESC=Time series collection period (Seconds)

ITEM=NYQUIST_FREQUENCY_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:SAMPLE_FREQUENCY_R4/2.0
	AUTHOR_DATE=19951017
	DESC=Highest resolved frequency (Hertz)

ITEM=FUNDAMENTAL_FREQUENCY_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:SAMPLE_FREQUENCY_R4/NUMBER_OF_SAMPLES
	AUTHOR_DATE=19951017
	DESC=FFT Fundamental (lowest) frequency (Hertz)

ITEM=TOP_FREQUENCY_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:NYQUIST_FREUENCY_R4
	AUTHOR_DATE=19960323
	DESC=Top frequency for this band (Hertz)

ITEM=BOTTOM_FREQUENCY_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:FUNDAMENTAL_FREQUENCY_R4
	AUTHOR_DATE=19960323
	DESC=Bottom frequency for this band (Hertz)

ITEM=STEP_FREQUENCY_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:FUNDAMENTAL_FREQUENCY_R4
	AUTHOR_DATE=19960323
	DESC=Frequency step size (Hertz)
!
!
!
ITEM=CHANNEL_NUMBER  
	EXTRACT=0,8,-1,1,0,AREA:3
	DESC="CHANNEL NUMBER FOR THIS SPECTRUM"
	AUTHOR_DATE=19931124
ITEM=SOURCE          
	EXTRACT=8,8,-1,1,0,AREA:3
	DESC=!FFT source antenna!
	XLATE=1,"ExAC"
	XLATE=2,"EyAC"
	XLATE=3,"EzAC"
	XLATE=4,"ExDC"
	XLATE=5,"EyDC"
	XLATE=6,"EzDC"
	XLATE=7,"Bx" 
	XLATE=8,"By" 
	XLATE=9,"Bz" 
	AUTHOR_DATE=19931124
ITEM=PACKET_COUNT    
	EXTRACT=0,8,-1,1,0,AREA:4
	DESC="COUNT OF THIS PACKET WITHIN EVENT"
	AUTHOR_DATE=19931124

ITEM=EXPONENT        
	VALIDATION=PACKET_SUBTYPE,MO,b10
	VALIDATION=DPU_VERSION,GE,3
	EXTRACT=12,2,16,1,0,AREA:5
	DESC="EXPONENT REPRESENTING GAIN"
	AUTHOR_DATE=19931124
ITEM=MANTISSA        
	VALIDATION=PACKET_SUBTYPE,MO,b10
	VALIDATION=DPU_VERSION,GE,3
	EXTRACT=0,12,16,1,0,AREA:5
	DESC="MEASUREMENT AT THE GIVEN GAIN LEVEL"
	AUTHOR_DATE=19931124
ITEM=POWER_SPECTRUM  
	VALIDATION=PACKET_SUBTYPE,MO,b10
	VALIDATION=DPU_VERSION,GE,3
	EXTRACT=0,16,16,1,0,AREA:5
	DESC="ENTIRE POWER SPECTRUM FROM ONE CHANNEL"
	AUTHOR_DATE=19931124
ITEM=DATA            
	VALIDATION=PACKET_SUBTYPE,MO,b100
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=0,32,32,1,0,AREA:5
	DESC="32 bit data - KAG"
	AUTHOR_DATE=19931127
ITEM=DATA            
	VALIDATION=PACKET_SUBTYPE,MO,b1000
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=0,32,32,1,0,AREA:5
	DESC="32 bit data - KAG"
	AUTHOR_DATE=19931127
ITEM=DATA            
	VALIDATION=PACKET_SUBTYPE,MO,b10000
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=0,8,8,1,0,AREA:5
	DESC="8 bit data - KAG"
	AUTHOR_DATE=19931127
ITEM=X_BUNDLE_COUNT  
	VALIDATION=PACKET_SUBTYPE,MO,b100000
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=0,8,-1,1,0,AREA:5
	DESC="Subtype 5, number of bundles - KAG"
	AUTHOR_DATE=19931206
ITEM=X_PEAK          
	VALIDATION=PACKET_SUBTYPE,MO,b100000
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=8,8,24,1,0,AREA:5
	DESC="Subtype 5, 8-bit bundled data - KAG"
	AUTHOR_DATE=19931204
ITEM=X_AVERAGE       
	VALIDATION=PACKET_SUBTYPE,MO,b100000
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=16,8,24,1,0,AREA:5
	DESC="Subtype 5, 8-bit bundled data - KAG"
	AUTHOR_DATE=19931204
ITEM=X_PHASE         
	VALIDATION=PACKET_SUBTYPE,MO,b100000
	VALIDATION=DPU_VERSION,GE,5
	EXTRACT=24,8,24,1,0,AREA:5
	DESC="Subtype 5, 8-bit bundled data - KAG"
	AUTHOR_DATE=19931204

ITEM=DPU_MAJOR_FRAME 
	EXTRACT=224,32,1,1,0,AREA:6
	DESC=From associated HK of encompassing major frame
ITEM=DPU_MINOR_FRAME 
	EXTRACT=96,8,-1,1,0,AREA:6
	DESC=Minor frame number of first minor frame in 1st packet of event
