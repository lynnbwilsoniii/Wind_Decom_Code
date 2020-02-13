


;<<<<<<<<<<<<<<<<<<<<<<< getrelgains >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pro getrelgains,relgain,suntim_vsbl

;given record tjd (trunc jul day) and sec (seconds of day),
;convert to elapsed seconds from reference date and time and  
;calculate relgains 

;Relative gains vs time are computed using fitrelgains.pro which creates a
;save file containing fit coefficicients, which are read by this procedure. 
;The fitted relative gains are checked using the criterion that solar wind
;flow angle must be within 10 degrees of spin ecliptic. This is the basis for
;the FINE TUNING done on the fitted coefficients near the end of this procedure

;!!!NOTE: The relgains for intrvl 4_3 are OK  only through April, 1997
;         (see FINETUNING)
         

;Relative gains may be computed for any given time using this procedure.
;In practice, this procedure is used by getrelgains_tblgen.pro  to create 
;a table in a save file of date vs relative gains for use by 
;the procedure, lzinput.pro, which is used in creating the moments and pitch 
;files and to display the levelzero plots.
 

pb5=sec_pb5(suntim_vsbl)

;filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_97dec08'
;filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_98apr06'
;filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_98aug11'
;filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_98nov18'
;filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_99jun10'
filen=getenv('WGGSBASE')+'swelz/swecal/gains/rgcoef_99jun15'
restore,filen

;refdate,pb5ref,pb5_change1,pb5_change2,pb5_change3,$
;    pb5_change4_1,pb5_change4_2,pb5_change4_3,pb5_change4_4,pb5_change4_5,$
;    pb5_change5_1,pb5_change5_2,pb5_change6,pb5_change7,coeff  

elapsec=pb5_elapsec(pb5,pb5ref)

;time intervals
elapsec_change1=pb5_elapsec(pb5_change1,pb5ref)
intrvl1 = elapsec le elapsec_change1

elapsec_change2=pb5_elapsec(pb5_change2,pb5ref)
intrvl2 = elapsec gt elapsec_change1 and elapsec le elapsec_change2

elapsec_change3=pb5_elapsec(pb5_change3,pb5ref)     
intrvl3 = elapsec gt elapsec_change2 and elapsec le elapsec_change3

elapsec_change4_1=pb5_elapsec(pb5_change4_1,pb5ref)
intrvl4_1 = elapsec gt elapsec_change3 and elapsec lt elapsec_change4_1

elapsec_change4_2=pb5_elapsec(pb5_change4_2,pb5ref)
intrvl4_2 = elapsec ge elapsec_change4_1 and elapsec lt elapsec_change4_2

elapsec_change4_3=pb5_elapsec(pb5_change4_3,pb5ref)
intrvl4_3 = elapsec ge elapsec_change4_2 and elapsec lt elapsec_change4_3

elapsec_change4_4=pb5_elapsec(pb5_change4_4,pb5ref)
intrvl4_4 = elapsec ge elapsec_change4_3 and elapsec lt elapsec_change4_4


;after pb5_change4_4 (19971028), det gains (esp det2) deteriorate rapidly 
;until bias is raised on pb5_change4_5 (19980514)

elapsec_change4_5=pb5_elapsec(pb5_change4_5,pb5ref)
intrvl4_5 = elapsec ge elapsec_change4_4 and elapsec lt elapsec_change4_5

ymd_change5=19980728l   ;bias change made 
;pb5_change5=[1998l,   209l,  0l]
elapsec_change5=pb5_elapsec(pb5_change5,pb5ref)
intrvl5 = elapsec ge elapsec_change4_5 and elapsec lt elapsec_change5

ymd_change6=19980811l   ;bias change made 
;pb5_change6=[1998l,   223l,  0l]
elapsec_change6=pb5_elapsec(pb5_change6,pb5ref)
intrvl6 = elapsec ge elapsec_change5 and elapsec lt elapsec_change6

ymd_change7_1=19980905l; change in det 2 response 
;pb5_change7_1=[1998l, 248l, 0l]
elapsec_change7_1=pb5_elapsec(pb5_change7_1,pb5ref)
intrvl7_1 = elapsec ge elapsec_change6 and elapsec lt elapsec_change7_1

ymd_change7_2=19981124l   ;bias change made 
;pb5_change7_2=[1998l,   328l,  0l]
elapsec_change7_2=pb5_elapsec(pb5_change7_2,pb5ref)
intrvl7_2 = elapsec ge elapsec_change7_1 and elapsec lt elapsec_change7_2

ymd_change8=19990326l   ;bias change made 
;pb5_change8=[1999l,   85l,  0l]
elapsec_change8=pb5_elapsec(pb5_change8,pb5ref)
intrvl8 = elapsec ge elapsec_change7_2 and elapsec lt elapsec_change8

 
ymd_change9=19990405l   ;bias change made 
;pb5_change9=[1999l,   95l,  0l]
elapsec_change9=pb5_elapsec(pb5_change9,pb5ref)
intrvl9 = elapsec ge elapsec_change8 and elapsec lt elapsec_change9

ymd_change10=20000101l   ;testing detector pairs method of gain balancing 
pb5_change10=[2000l,   1l,  0l]
elapsec_change10=pb5_elapsec(pb5_change10,pb5ref)
intrvl10 = elapsec ge elapsec_change9 and elapsec lt elapsec_change10

ymd_change11=20010101l   ;testing detector pairs method of gain balancing 
pb5_change11=[2001l,   1l,  0l]
elapsec_change11=pb5_elapsec(pb5_change11,pb5ref)
intrvl11 = elapsec ge elapsec_change10 and elapsec lt elapsec_change11


intrvl_after = elapsec ge elapsec_change11
  

;if intrvl5 then intrvl=7
;elapsec_end=elapsec_change5


relgain=fltarr(6)

indx=-1
if intrvl1   then indx=0
if intrvl2   then indx=1
if intrvl3   then indx=2
if intrvl4_1 then indx=3
if intrvl4_2 then indx=4
if intrvl4_3 then indx=5
if intrvl4_4 then indx=6
if intrvl4_5 then indx=7
if intrvl5   then indx=8
if intrvl6   then indx=9
if intrvl7_1 then indx=10
if intrvl7_2 then indx=11
if intrvl8   then indx=12
if intrvl9   then indx=13

if indx ge 0 and indx le 6 then begin

  for i=0,5 do relgain(i)=$
    coeff(0,i,indx)+$
    coeff(1,i,indx)*elapsec+$
    coeff(2,i,indx)*elapsec^2

endif else if  intrvl4_5 then begin                      ;19971028 to 19980514

  relgain=[1.000, 0.829, 10.158, 3.442, 0.943, 2.770]
  
endif else if  intrvl5 then begin                        ;19980514 to 19980728
  
  relgain=[1.000, 0.829, 10.158, 3.442, 0.943, 2.770]
  
endif else if  intrvl6 then begin                        ;19980728 to 19980811


  relgain=[1.000,1.052,2.710,2.721,1.340,2.640]
  
endif else if  intrvl7_1 then begin                        ;19980811 to 19980904
  
  for i=0,5 do relgain(i)=$
    coeff(0,i,indx)+$
    coeff(1,i,indx)*elapsec+$
    coeff(2,i,indx)*elapsec^2
    
  relgain(2)=0.70*relgain(2)  

endif else if  intrvl7_2 then begin                        ;19980904 to 19981124
  
  for i=0,5 do relgain(i)=$
    coeff(0,i,indx)+$
    coeff(1,i,indx)*elapsec+$
    coeff(2,i,indx)*elapsec^2
    
  relgain(2)=0.80*relgain(2)  
    
endif else if  intrvl8 then begin                        ;19981124 to 19990326
  
  ;relgain=[1.000, 0.829, 10.158, 3.442, 0.943, 2.770] 
  relgain=[1.000, 1.000, 1.000, 1.000, 1.000, 1.000 ]
     
endif else if  intrvl9 then begin                        ;19990326 to 19990405

  ;relgain=[1.000, 0.829, 10.158,3.442, 0.943, 2.770] 
  relgain=[1.000, 1.000, 1.000, 1.000, 1.000, 1.000 ]
          
endif else if intrvl10 then begin            ;19990405 to 20000101

  ;relgain=[1.000, 1.08, 1.98, 3.30, 1.00, 1.80]
  ;relgain=[1.000, 1.010, 1.908, 2.68, 0.968, 1.759]
  ;relgain=[1.000, 1.010, 1.908, 2.00, 0.968, 1.759]
  ;relgain=[1.000, 1.010, 1.908, 4.00, 0.968, 1.759]
  ;relgain=[1.000, 1.000, 1.000, 1.000, 1.000, 1.000 ]
  ;step7,detpairs+pitch method 19990622 recn=496:526   --this works! thu=25deg
     relgain=[0.689,  1.096,  1.322,  2.525,  1.000,  2.179]
  ;step7,pitch method 19990622 recn=496:526   --this doesn't work! thu=50deg  
     ;relgain=[1.000,  1.500,  1.729,  3.301,  1.404,  3.383]

endif else if intrvl11 then begin           ;20000101 to 20010101   
   
 ;first balance by pairs then balance by original pitch angle fit method
  ;relgain=[6.,1.,1.,6.,1.,6.] ;detector pair balance alone      
  ;relgain=[18.947,  1.045,  0.905,  4.392,  1.000,  2.821]  ;20000110
   relgain=[29.522,  1.164 , 0.652,  3.877,  1.000,  2.236]  ;20000127 
   
endif else if intrvl_after then begin      ;after 20010101

  ;initial estimate
  relgain=[1.000, 1.000, 1.000, 1.000, 1.000, 1.000 ]
   
endif else stop,'getrelgains: bad time'
  

 
 
;FINE TUNING 

  if intrvl4_1 then begin
    relgain(0)=1.0
    relgain(1)=1.25
    relgain(2)=1.21
    relgain(3)= 1.1*relgain(3)
    relgain(4)=1.97
    relgain(5)=1.32
  endif
  
  if intrvl4_2 then relgain(2)= 0.8*relgain(2)  

  if intrvl4_3 then begin 
     relgain(2)=0.9*relgain(2)
     relgain(3)=1.2*relgain(3)
    ;relgain(3)=1.3*relgain(3)
  endif 
  
  if intrvl4_4 then begin
     relgain(2)=0.9*relgain(2)
     relgain(3)=1.1*relgain(3) 
  endif
  
    
  elapsec_ft1=pb5_elapsec(ymd_pb5(long(19950101)),pb5ref)
  elapsec_ft2=pb5_elapsec(ymd_pb5(long(19950729)),pb5ref) 

  if elapsec ge elapsec_ft1 and elapsec lt elapsec_ft2 then $
    relgain(2)=relgain(0) 
 
   
  

end
