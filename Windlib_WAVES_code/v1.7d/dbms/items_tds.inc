! items_tds.inc - WIND/WAVES Item Database: item definitions for all TDS events
!

ITEM=MEASURE_HEADER  
	VALIDATION=TDS_VERSION,LE,2
	FIXED_VALUE=1:160    
	DESC=Number of bits in measurement header
	AUTHOR_DATE=19910601
ITEM=MEASURE_HEADER  
	VALIDATION=TDS_VERSION,GE,3
	FIXED_VALUE=1:216    
	DESC=Number of bits in measurement header
	AUTHOR_DATE=19931030
ITEM=MYTDSCOUNT      
	FIXED_VALUE=1:2048   
	DESC=Count of vaues in a TDS data block
	AUTHOR_DATE=19910601
ITEM=PACKET_HEADER   
	FIXED_VALUE=1:8      
	DESC=Count of bits in packet header
	AUTHOR_DATE=19910601
ITEM=INSTR_HEADER    
	FIXED_VALUE=1:48     
	DESC=Number of bits in instrument header
	AUTHOR_DATE=19910601
ITEM=MAJOR_BITS      
	VALIDATION=PACKET_SUBTYPE,MO,b1
	VALIDATION=EVENT_SCET,GT,DATE:19920516-190000
	VALIDATION=TDS_VERSION,LE,2
	FIXED_VALUE=1:32     
	DESC=Number of bits in major frame counter
	AUTHOR_DATE=19920516
ITEM=MAJOR_BITS      
	VALIDATION=PACKET_SUBTYPE,MO,b1
	VALIDATION=EVENT_SCET,GT,DATE:19920516-190000
	VALIDATION=TDS_VERSION,GE,3
	FIXED_VALUE=1:24     
	DESC=Number of bits in major frame counter
	AUTHOR_DATE=19920516
ITEM=MAX_STATE       
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=18,1,-1,1,0,AREA:2
	DESC=0=NOMAX OFF, 1=NOMAX ON
	AUTHOR_DATE=19920507
	XLATE=0,"NOMAX off"
	XLATE=1,"NOMAX on"
ITEM=TANDEM_STATE    
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=19,1,-1,1,0,AREA:2
	DESC=0=TRIGGER HIGH AND LOW SEPARATELY, 1=TRIGGER TOGETHER
	AUTHOR_DATE=19920507
	XLATE=0,"TANDEM off"
	XLATE=1,"TANDEM on"

ITEM=RX_FILTER  
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_RX_FILTER
	XLATE=0,"50,000 Hz"
	XLATE=1,"12,500 Hz"
	XLATE=2,"3,125 Hz"
	XLATE=3,"781 Hz"
	XLATE=4,"195 Hz"
	XLATE=5,"49 Hz"
	DESC=Event's low pass filter selection 
	AUTHOR_DATE=19960404
ITEM=RX_FILTER  
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_RX_FILTER+2
	XLATE=0,"50,000 Hz"
	XLATE=1,"12,500 Hz"
	XLATE=2,"3,125 Hz"
	XLATE=3,"781 Hz"
	XLATE=4,"195 Hz"
	XLATE=5,"49 Hz"
	DESC=Event's low pass filter selection 
	AUTHOR_DATE=19960404

ITEM=RX_FILTER_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_RX_FILTER_R4
	DESC=Event's low pass filter selection (Hertz)
	AUTHOR_DATE=19960329
ITEM=RX_FILTER_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_RX_FILTER_R4
	DESC=Event's low pass filter selection (Hertz)
	AUTHOR_DATE=19960329

ITEM=RX_SPEED  
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_RX_SPEED
	XLATE=0,"120,000 S/S"
	XLATE=1,"30,000 S/S"
	XLATE=2,"7,500 S/S"
	XLATE=3,"1,875 S/S"
	XLATE=4,"468 S/S"
	XLATE=5,"117 S/S"
	DESC=Event's speed selection 
	AUTHOR_DATE=19960404
ITEM=RX_SPEED  
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_RX_SPEED+2
	XLATE=0,"120,000 S/S"
	XLATE=1,"30,000 S/S"
	XLATE=2,"7,500 S/S"
	XLATE=3,"1,875 S/S"
	XLATE=4,"468 S/S"
	XLATE=5,"117 S/S"
	DESC=Event's speed selection 
	AUTHOR_DATE=19960404
ITEM=RX_SPEED_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_RX_SPEED_R4
	DESC=Event's sampling speed (samples/second)
	AUTHOR_DATE=19960329
ITEM=RX_SPEED_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_RX_SPEED_R4
	DESC=Event's sampling speed (samples/second)
	AUTHOR_DATE=19960329

ITEM=RX_PERIOD_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_RX_PERIOD_R4
	DESC=Event's Sample period (seconds)
	AUTHOR_DATE=19960329
ITEM=RX_PERIOD_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_RX_PERIOD_R4
	DESC=Event's Sample period (seconds)
	AUTHOR_DATE=19960329

ITEM=RX_INTERVAL_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_RX_INTERVAL_R4
	DESC=Duration of entire event (seconds)
	AUTHOR_DATE=19960329
ITEM=RX_INTERVAL_R4
	DATA_TYPE=REAL*4
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_RX_INTERVAL_R4
	DESC=Duration of entire event (seconds)
	AUTHOR_DATE=19960329

ITEM=RX_TRIGGER_CHANNEL
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_SAMPLER_TRIGGER
	DESC=Trigger channel for this event (1-6)
	AUTHOR_DATE=19960329
	XLATE=1,"Trigger on Channel 1"
	XLATE=2,"Trigger on Channel 2"
	XLATE=3,"Trigger on Channel 3"
	XLATE=4,"Trigger on Channel 4"
	XLATE=5,"Trigger on Channel 5"
	XLATE=6,"Trigger on Channel 6"
ITEM=RX_TRIGGER_CHANNEL
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_SAMPLER_TRIGGER
	DESC=Trigger channel for this event (1-6)
	AUTHOR_DATE=19960329
	XLATE=1,"Trigger on Channel 1"
	XLATE=2,"Trigger on Channel 2"
	XLATE=3,"Trigger on Channel 3"
	XLATE=4,"Trigger on Channel 4"
	XLATE=5,"Trigger on Channel 5"
	XLATE=6,"Trigger on Channel 6"

ITEM=RX_TRIGGER_THRESHOLD
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_SAMPLER_THRESHOLD
	DESC=Trigger threshold for this event (0-127)
	AUTHOR_DATE=19960329
ITEM=RX_TRIGGER_THRESHOLD
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_SAMPLER_THRESHOLD
	DESC=Trigger threshold for this event (0-127)
	AUTHOR_DATE=19960329

ITEM=RX_TRIGGER_MODE
	VALIDATION=CHANNEL,LE,2
	FIXED_VALUE=1:FAST_SAMPLER_TRIGGER_MODE
	XLATE=0,"Reset MAX"
	XLATE=1,"Save MAX" 
	DESC=Event's trigger mode
	AUTHOR_DATE=19960404
ITEM=RX_TRIGGER_MODE
	VALIDATION=CHANNEL,GE,3
	FIXED_VALUE=1:SLOW_SAMPLER_TRIGGER_MODE
	XLATE=0,"Reset MAX"
	XLATE=1,"Save MAX" 
	DESC=Event's trigger mode
	AUTHOR_DATE=19960404

ITEM=SOURCE_CHAN_1   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=8,1,-1,1,0,AREA:2
	DESC=SOURCE, 0=ExAC, 1=ExDC
	AUTHOR_DATE=19920507
	XLATE=0,"ExAC"
	XLATE=1,"ExDC"
ITEM=SOURCE_CHAN_2   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=9,2,-1,1,0,AREA:2
	DESC=SOURCE, 0=ExAc,1=EyAC,2=EzAC,3=EzAC
	AUTHOR_DATE=19920507
	XLATE=0,"ExAC"
	XLATE=1,"EyAC"
	XLATE=2,"EzAC"
	XLATE=3,"EzAC"
ITEM=SOURCE_CHAN_3   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=11,2,-1,1,0,AREA:2
	DESC=SOURCE, 0=ExDC,1=EyDC,Z=EyDC,3=EzDC
	AUTHOR_DATE=19920507
	XLATE=0,"ExDC"
	XLATE=1,"EyDC"
	XLATE=2,"EzDC"
	XLATE=3,"EzDC"
ITEM=SOURCE_CHAN_4   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=13,1,-1,1,0,AREA:2
	DESC=SOURCE, 0=Bx, 1=ExDC
	AUTHOR_DATE=19920507
	XLATE=0,"Bx"
	XLATE=1,"ExDC"
ITEM=SOURCE_CHAN_5   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=14,1,-1,1,0,AREA:2
	DESC=SOURCE, 0=By, 1=EyDC
	AUTHOR_DATE=19920507
	XLATE=0,"By"
	XLATE=1,"EyDC"
ITEM=SOURCE_CHAN_6   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=15,1,-1,1,0,AREA:2
	DESC=SOURCE, 0=Bz, 1=EzDC
	AUTHOR_DATE=19920507
	XLATE=0,"Bz"
	XLATE=1,"EzDC"
ITEM=ZZ_BIG_INSTR    
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=0,8,-1,6,0,AREA:2
	DESC=Diagnostic to get the entire instrument header
	AUTHOR_DATE=19920507
ITEM=CHANNEL         
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=152,8,-1,1,0,AREA:3
	DESC=CHANNEL # FOR THIS EVENT
	AUTHOR_DATE=19920507
ITEM=CHANNEL         
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=136,8,-1,1,0,AREA:3
	DESC=CHANNEL # FOR THIS EVENT
	AUTHOR_DATE=19931030
ITEM=DPU_CLOCK       
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=40,24,-1,1,0,AREA:3
	DESC=DPU clock at end of event (ticks)
	AUTHOR_DATE=19920507
ITEM=DPU_CLOCK       
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=32,24,-1,1,0,AREA:3
	DESC=DPU clock at end of event (ticks)
	AUTHOR_DATE=19931030
ITEM=DPU_CLOCK_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:DPU_CLOCK*DPU_CLOCK_PERIOD_R4
	DESC=DPU clock at end of event (seconds)
	AUTHOR_DATE=19960329
ITEM=DPU_MAJOR_FRAME 
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=8,32,-1,1,0,AREA:3
	DESC=DPU major frame at start of measurement
	AUTHOR_DATE=19920507
ITEM=DPU_MAJOR_FRAME 
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=8,24,-1,1,0,AREA:3
	DESC=DPU major frame at start of measurement
	AUTHOR_DATE=19931030
ITEM=DPU_MINOR_FRAME 
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=0,8,-1,1,0,AREA:3
	DESC=DPU minor frame at start of measruement
	AUTHOR_DATE=19920507
ITEM=EVENT_MASK      
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=136,8,-1,1,0,AREA:3
	DESC=MASK SHOWING CHANNELS IN THIS EVENT
	AUTHOR_DATE=19920507
	XLATE=0,"No channels on"
	XLATE=1,"Channel 1 only"
ITEM=EVENT_MASK      
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=120,8,-1,1,0,AREA:3
	DESC=MASK SHOWING CHANNELS IN THIS EVENT
	AUTHOR_DATE=19931030
	XLATE=0,"No channels on"
	XLATE=1,"Channel 1 only"
	XLATE=2,"Channel 2 only"
	XLATE=3,"Channels 1 & 2"
	XLATE=4,"Channel 3 only"
	XLATE=5,"Channels 3 & 1"
	XLATE=6,"Channels 3 & 2"
	XLATE=7,"Channels 3,2 & 1"

	XLATE=8,"Channel 4 only"
	XLATE=9,"Channels 4 & 1"
	XLATE=10,"Channels 4 & 2"
	XLATE=11,"Channels 4,2 & 1"
	XLATE=12,"Channels 4 & 3"
	XLATE=13,"Channels 4,3 & 1"
	XLATE=14,"Channels 4,3 & 2"
	XLATE=15,"Channels 4, 3, 2 & 1"

	XLATE=16,"Channel 5 only"
	XLATE=17,"Channels 5 & 1"
	XLATE=18,"Channels 5 & 2"
	XLATE=19,"Channels 5,2 & 1"
	XLATE=20,"Channels 5 & 3"
	XLATE=21,"Channels 5, 3 & 1"
	XLATE=22,"Channels 5, 3 & 2"
	XLATE=23,"Channels 5, 3, 2 & 1"
	XLATE=24,"Channels 5 & 4"
	XLATE=25,"Channels 5, 4 & 1"
	XLATE=26,"Channels 5, 4 & 2"
	XLATE=27,"Channels 5, 4, 2 & 1"
	XLATE=28,"Channels 5, 4 & 3"
	XLATE=29,"Channels 5, 4, 3 & 1"
	XLATE=30,"Channels 5, 4, 3 & 2"
	XLATE=31,"Channels 5, 4, 3, 2 & 1"

	XLATE=32,"Channel 6 only"
	XLATE=33,"Channels 6 & 1"
	XLATE=34,"Channels 6 & 2"
	XLATE=35,"Channels 6, 2 & 1"
	XLATE=36,"Channels 6 & 3"
	XLATE=37,"Channels 6, 3 & 1"
	XLATE=38,"Channels 6, 3 & 2"
	XLATE=39,"Channels 6, 3, 2 & 1"
	XLATE=40,"Channels 6 & 4"
	XLATE=41,"Channels 6, 4 & 1"
	XLATE=42,"Channels 6, 4 & 2"
	XLATE=43,"Channels 6, 4, 2 & 1"
	XLATE=44,"Channels 6, 4 & 3"
	XLATE=45,"Channels 6, 4, 3 & 1"
	XLATE=46,"Channels 6, 4, 3 & 2"
	XLATE=47,"Channels 6, 4, 3, 2 & 1"

	XLATE=48,"Channels 6 & 5"
	XLATE=49,"Channels 6, 5 & 1"
	XLATE=50,"Channels 6, 5 & 2"
	XLATE=51,"Channels 6, 5, 2 & 1"
	XLATE=52,"Channels 6, 5 & 3"
	XLATE=53,"Channels 6, 5, 3 & 1"
	XLATE=54,"Channels 6, 5, 3 & 2"
	XLATE=55,"Channels 6, 5, 3, 2 & 1"

	XLATE=56,"Channels 6, 5 & 4"
	XLATE=57,"Channels 6, 5, 4 & 1"
	XLATE=58,"Channels 6, 5, 4 & 2"
	XLATE=59,"Channels 6, 5, 4, 2 & 1"
	XLATE=60,"Channels 6, 5, 4 & 3"
	XLATE=61,"Channels 6, 5, 4, 3 & 1"
	XLATE=62,"Channels 6, 5, 4, 3 & 2"
	XLATE=63,"Channels 6, 5, 4, 3, 2, & 1"

ITEM=EVENT_NUMBER    
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=104,32,-1,1,0,AREA:3
	DESC=ON BOARD EVENT COUNTER
	AUTHOR_DATE=19920507
ITEM=EVENT_NUMBER    
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=88,32,-1,1,0,AREA:3
	DESC=ON BOARD EVENT COUNTER
	AUTHOR_DATE=19931030
ITEM=EVENT_SWEEPS    
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=144,8,-1,1,0,AREA:3
	DESC=NUMBER OF EVENTS IN THE SWEEP (1 EVENT PER CHANNEL)
	AUTHOR_DATE=19920507
ITEM=EVENT_SWEEPS    
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=128,8,-1,1,0,AREA:3
	DESC=NUMBER OF EVENTS IN THE SWEEP (1 EVENT PER CHANNEL)
	AUTHOR_DATE=19931030
ITEM=QUALITY         
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=72,32,-1,1,0,AREA:3
	DESC=Indicator of quality of data.  Bigger is better
	AUTHOR_DATE=19920507
ITEM=QUALITY         
	VALIDATION=TDS_VERSION,EQ,3
	EXTRACT=56,32,-1,1,0,AREA:3
	DESC=Indicator of quality of data.  Bigger is better
	AUTHOR_DATE=19931030
ITEM=QUALITY         
	VALIDATION=TDS_VERSION,GE,4
	EXTRACT=64,24,-1,1,0,AREA:3
	DESC=Indicator of quality of data.  Bigger is better
	AUTHOR_DATE=19960329
ITEM=PINGPONG
	VALIDATION=TDS_VERSION,LE,3
	FIXED_VALUE=1:0
	XLATE=0,"Ping/Pong"
	DESC=Event from PING or PONG
	AUTHOR_DATE=19960329
ITEM=PINGPONG
	VALIDATION=TDS_VERSION,GE,4
	EXTRACT=56,8,-1,1,0,AREA:3
	XLATE=0,"Illegal"
	XLATE=1,"Ping"
	XLATE=2,"Pong"
	DESC=Event from PING or PONG
	AUTHOR_DATE=19960329
ITEM=ZZ_BIG_MEAS     
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=TDS_VERSION,LE,2
	EXTRACT=0,8,-1,20,0,AREA:3
	DESC=Diagnostic to get the entire measurement header
	AUTHOR_DATE=19920507
ITEM=SPINS           
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=144,32,-1,1,0,AREA:3
	DESC=Spin number
	AUTHOR_DATE=19931030
ITEM=SUN_ANGLE       
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=176,16,-1,1,0,AREA:3
	DESC=Sun angle (count 4096 from sun)
	AUTHOR_DATE=19931030
ITEM=SUN_ANGLE_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:SUN_ANGLE*STICS2DEGREES_R4
	DESC=Sun angle (degrees)
	AUTHOR_DATE=19960329
ITEM=MAG_ANGLE       
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=192,16,-1,1,0,AREA:3
	DESC=Magnetic azimuth (count4096 from 0 crossing)
	AUTHOR_DATE=19931030
ITEM=MAG_ANGLE_R4
	DATA_TYPE=REAL*4
	FIXED_VALUE=1:MAG_ANGLE*STICS2DEGREES_R4
	DESC=Magnetic azimuth (degrees from 0 crossing)
	AUTHOR_DATE=19960329
ITEM=MAG_ELEVATION   
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=208,8,-1,1,0,AREA:3
	DESC=Magnetic Elevation (0-180 degrees)
	AUTHOR_DATE=19931030
ITEM=ZZ_BIG_MEAS     
	VALIDATION=TDS_VERSION,GE,3
	EXTRACT=0,8,-1,27,0,AREA:3
	DESC=Diagnostic to get the entire measurement header
	AUTHOR_DATE=19931030
ITEM=ZZ_BIG_PACKET   
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=0,8,-1,1,0,AREA:4
	DESC=Diagnostic to get the entire packet header
	AUTHOR_DATE=19920507
ITEM=PACKET_COUNT    
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	EXTRACT=0,8,-1,1,0,AREA:4
	DESC=COUNT OF THIS PACKET IN THE EVENT
	AUTHOR_DATE=19920507

ITEM=DATA            
	EXTRACT=0,8,-1,2048,0,AREA:5
	COUNT=MYTDSCOUNT  
	DESC=Entire sweep of 2048 data words
	AUTHOR_DATE=19920321

ITEM=SOURCE_CHANNEL
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,1
	VALIDATION=SOURCE_CHAN_1,EQ,0
	FIXED_VALUE=1:1
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,1
	VALIDATION=SOURCE_CHAN_1,EQ,1
	FIXED_VALUE=1:4
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,2
	VALIDATION=SOURCE_CHAN_2,EQ,0
	FIXED_VALUE=1:1
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,2
	VALIDATION=SOURCE_CHAN_2,EQ,1
	FIXED_VALUE=1:2
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,2
	VALIDATION=SOURCE_CHAN_2,GE,2
	FIXED_VALUE=1:3
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,3
	VALIDATION=SOURCE_CHAN_3,EQ,0
	FIXED_VALUE=1:4
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,3
	VALIDATION=SOURCE_CHAN_3,EQ,1
	FIXED_VALUE=1:5
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,3
	VALIDATION=SOURCE_CHAN_3,GE,2
	FIXED_VALUE=1:6
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,4
	VALIDATION=SOURCE_CHAN_4,EQ,0
	FIXED_VALUE=1:7
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,4
	VALIDATION=SOURCE_CHAN_4,EQ,1
	FIXED_VALUE=1:4
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,5
	VALIDATION=SOURCE_CHAN_5,EQ,0
	FIXED_VALUE=1:8
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,5
	VALIDATION=SOURCE_CHAN_5,EQ,1
	FIXED_VALUE=1:5
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,6
	VALIDATION=SOURCE_CHAN_6,EQ,0
	FIXED_VALUE=1:9
	AUTHOR_DATE=19941018
ITEM=SOURCE_CHANNEL  
	VALIDATION=EVENT_SCET,GT,DATE:19920507-220000
	VALIDATION=CHANNEL,EQ,6
	VALIDATION=SOURCE_CHAN_6,EQ,1
	FIXED_VALUE=1:6
	AUTHOR_DATE=19941018
ITEM=SOURCE  
	FIXED_VALUE=1:SOURCE_CHANNEL
	XLATE=1,"ExAC"
	XLATE=2,"EyAC"
	XLATE=3,"EzAC"
	XLATE=4,"ExDC"
	XLATE=5,"EyDC"
	XLATE=6,"EzDC"
	XLATE=7,"Bx"
	XLATE=8,"By"
	XLATE=9,"Bz"
	DESC=Source antenna translations
	AUTHOR_DATE=19960329

ITEM=TDS_COEFFS_NUM
	FIXED_VALUE=1:2
	DESC=Number of coefficients for physical units
	AUTHOR_DATE=19941018
ITEM=TDS_COEFFS_VERSION
	FIXED_VALUE=1:1
	DESC=Version number for physical units coefficients
	AUTHOR_DATE=19941018