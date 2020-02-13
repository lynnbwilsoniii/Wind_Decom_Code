;============== function strahlvar =============================================

function swe_strahlvar,varname

common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common shared,d
common swestuff,swest
common wstuff,wst

fill=-1.e31

idatype=where(d.datype eq 'swe_strahl')
                  
sdat=d.swe_strahldat(d.ndx(0,idatype):d.ndx(1,idatype))
enstp=swest.strlstep_slct     ; 5 (96ev)
energy=volt_en_strl(enstp,/en)
;print,'strahl energy ',energy


;find subset of spins with enstep = en  
  wen=where(sdat.enstep eq enstp,nwen)
  if wen(0) ne -1 then sdat=sdat(wen)
  
;find min of strahl
  mn=min(sdat.strl)
  ;wgtmn=where(sdat.strl gt 3*mn)
  ;stop
  ;sdat=sdat(wgtmn)  
   

case varname of

'strl_max_cts' : begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.f=sdat.strl
    xy.ta=sdat.ta
    xy.energy=long(energy)
    return,xy
                 endcase
    

'pa(strl_max_cts)' : begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.f=sdat.pstrl
    xy.ta=sdat.ta
    xy.energy=long(energy)
    return,xy
                     endcase


'strl-widthmx' : begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.f=sdat.wstrl
    xy.ta=sdat.ta
    xy.energy=long(energy)
    return,xy
                     endcase


'anti_strl_max_cts' : begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.f=sdat.astrl
    xy.ta=sdat.ta
    xy.energy=long(energy)
    return,xy
                      endcase
    

'pa(anti_strl_max_cts)' : begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.f=sdat.pastrl
    xy.ta=sdat.ta
    xy.energy=long(energy)
    return,xy
                          endcase


'anti-strl-widthmx' : begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.f=sdat.wastrl
    xy.ta=sdat.ta
    xy.energy=long(energy)
    return,xy
                      endcase
                      
'strl/anti-strl ratio' : begin                          
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    wne0=where(sdat.astrl ne 0)
    if wne0(0) ne -1 then begin
      xy.f(wne0)=sdat(wne0).astrl/sdat(wne0).strl
      xy.ta=sdat.ta
      xy.energy=long(energy)
    endif
    return,xy
                      endcase
                      
'B magnetic field': begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
   
    xy.ta=t
    xy.energy=long(energy)
    return,xy
                      endcase


'th_b': begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.ta=t
    xy.energy=long(energy)
    return,xy
                      endcase


'ph_b': begin
    xy={f:fltarr(nwen),ta:dblarr(nwen),energy:0l}
    xy.ta=t
    xy.energy=long(energy)
    return,xy
                      endcase
endcase
                 

end
