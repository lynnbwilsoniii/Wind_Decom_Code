;============== function strahlenvar =========================================

function swe_strahlenvar,varname

common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common shared,d
common swestuff,swest
common wstuff,wst

fill=-1.e31

idatype=where(d.datype eq 'swe_strahlen')
                  
sdat=d.swe_strahlendat(d.ndx(0,idatype):d.ndx(1,idatype))   
;find energy available steps
  enstep0=min(sdat.enstep,max=enstep1)

;data at available ensteps
  wen0=where(sdat.enstep eq enstep0,nwen0)
  wen1=where(sdat.enstep eq enstep1,nwen1) 
  energy0=sdat(wen0(0)).en
  energy1=sdat(wen1(0)).en
   
case varname of

'strlen0' : begin
    xy={f:fltarr(nwen0),ta:dblarr(nwen0),energy:0l}
    xy.f=sdat(wen0).strl
    xy.ta=sdat(wen0).ta
    xy.energy=long(energy0)
    return,xy
                 endcase
    
'paen0' : begin
    xy={f:fltarr(nwen0),ta:dblarr(nwen0),energy:0l}
    xy.f=sdat(wen0).pstrl
    xy.ta=sdat(wen0).ta
    xy.energy=long(energy0)
    return,xy
                     endcase
                     
'strlen0-wdth' : begin
    xy={f:fltarr(nwen0),ta:dblarr(nwen0),energy:0l}
    xy.f=sdat(wen0).wstrl
    xy.ta=sdat(wen0).ta
    xy.energy=long(energy0)
    return,xy
                     endcase


'anti_strlen0' : begin
    xy={f:fltarr(nwen0),ta:dblarr(nwen0),energy:0l}
    xy.f=sdat(wen0).astrl
    xy.ta=sdat(wen0).ta
    xy.energy=long(energy0)
    return,xy
                      endcase

'anti_paen0' : begin
    xy={f:fltarr(nwen0),ta:dblarr(nwen0),energy:0l}
    xy.f=sdat(wen0).pastrl
    xy.ta=sdat(wen0).ta
    xy.energy=long(energy0)
    return,xy
                     endcase
                         
'anti-strlen0-wdth' : begin
    xy={f:fltarr(nwen0),ta:dblarr(nwen0),energy:0l}
    xy.f=sdat(wen0).wastrl
    xy.ta=sdat(wen0).ta
    xy.energy=long(energy0)
    return,xy
                      endcase
                      
'strlen1' : begin
    xy={f:fltarr(nwen1),ta:dblarr(nwen1),energy:0l}
    xy.f=sdat(wen1).strl
    xy.ta=sdat(wen1).ta
    xy.energy=long(energy1)
    return,xy
                 endcase
                 
'paen1' : begin
    xy={f:fltarr(nwen1),ta:dblarr(nwen1),energy:0l}
    xy.f=sdat(wen1).pstrl
    xy.ta=sdat(wen1).ta
    xy.energy=long(energy1)
    return,xy
                     endcase    

'strlen1-wdth' : begin
    xy={f:fltarr(nwen1),ta:dblarr(nwen1),energy:0l}
    xy.f=sdat(wen1).wstrl
    xy.ta=sdat(wen1).ta
    xy.energy=long(energy1)
    return,xy
                     endcase


'anti_strlen1' : begin
    xy={f:fltarr(nwen1),ta:dblarr(nwen1),energy:0l}
    xy.f=sdat(wen1).astrl
    xy.ta=sdat(wen1).ta
    xy.energy=long(energy1)
    return,xy
                      endcase

'anti_paen1' : begin
    xy={f:fltarr(nwen1),ta:dblarr(nwen1),energy:0l}
    xy.f=sdat(wen1).pastrl
    xy.ta=sdat(wen1).ta
    xy.energy=long(energy1)
    return,xy
                     endcase    
                         
'anti-strlen1-wdth' : begin
    xy={f:fltarr(nwen1),ta:dblarr(nwen1),energy:0l}
    xy.f=sdat(wen1).wastrl
    xy.ta=sdat(wen1).ta
    xy.energy=long(energy1)
    return,xy
                      endcase
                      

endcase
                 

end
