$SET DEF [KELLOGG.WIND]
$WIND_LOGIN
$FORT/LIST/CHECK/WARN TDSVIS
$!FORT/LIST/CHECK/WARN TDSVIST
$LINK TDSVIS,tds_phys,FIXBADTDS6,gaussj,huntmn,BZGLITCH,wind_minn,-
 ARROW,wind_lib/lib,MONGO
$!PURGE TDSVIS.*
$exit
$RUN TDSVIS               
19941227
 1310
1,2
2
$exit
$RUN TDSVIS                   		 !   marty goldman event
wi_lz_wav_19960419_v*.dat
 0850
2,12750039
4
$exit
