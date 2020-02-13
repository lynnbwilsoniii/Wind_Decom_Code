
pro getrelgains,relgain,suntim_vsbl,relgainchange

;given record tjd (trunc jul day) and sec (seconds of day),
;convert to elapsed seconds from reference date and time and  
;calculate relgains 

;Relative gains vs time are computed using fitrelgains.pro which creates a
;save file containing fit coefficicients, which are read by this procedure. 
;The fitted relative gains are checked using the criterion that solar wind
;flow angle must be within 10 degrees of spin ecliptic. This is the basis for
;the FINE TUNING done on the fitted coefficients near the end of this procedure.

;!!!NOTE: The relgains for intrvl 4_3 are OK  only through April, 1997
;         (see FINETUNING)
         

pb5=sec_pb5(suntim_vsbl)

;filen=getenv('WGGSBASE')+'swe/gains/rgcoef_97aug25'
;filen=getenv('WGGSBASE')+'swe/gains/rgcoef_97aug27'
;filen=getenv('WGGSBASE')+'swe/gains/rgcoef_97aug28'
;filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_97dec08'
filen=getenv('WGGSBASE')+'swe/cal/gains/rgcoef_98jun02'
restore,filen

;refdate,pb5ref,pb5_change1,pb5_change2,pb5_change3,$
;    pb5_change4_1,pb5_change4_2,$
;    coeff 

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

pb5_end=long([1997,         285,    13665978])
elapsec_end=pb5_elapsec(pb5_end,pb5ref)
intrvl4_4 = elapsec ge elapsec_change4_3 and elapsec le elapsec_end

intrvl4_4_after = elapsec gt elapsec_end

intrvl=-1
if intrvl1   then intrvl=0
if intrvl2   then intrvl=1
if intrvl3   then intrvl=2
if intrvl4_1 then intrvl=3
if intrvl4_2 then intrvl=4
if intrvl4_3 then intrvl=5
if intrvl4_4 then intrvl=6
intrvl_end=6


relgain=fltarr(6)

if intrvl ge 0 and intrvl le intrvl_end then begin

  for i=0,5 do relgain(i)=$
    coeff(0,i,intrvl)+coeff(1,i,intrvl)*elapsec+coeff(2,i,intrvl)*elapsec^2
      
endif else if intrvl4_4_after then begin

  for i=0,5 do relgain(i)=$
    coeff(0,i,intrvl_end)+coeff(1,i,intrvl_end)*elapsec_end+$
    coeff(2,i,intrvl_end)*elapsec_end^2 
    ;stop  
    
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
    
       
relgainchange=0


end
