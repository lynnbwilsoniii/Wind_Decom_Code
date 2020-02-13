pro plasmap,p


common shared,d

idatype=where(d.datype eq 'swe_ionkp')
id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

idatype_swe_moments=where(d.datype eq 'swe_moments')

ionp=d.swe_ionkpdat(id1:id2).ionp

ed1=d.ndx(0,idatype_swe_moments) & ed2=d.ndx(1,idatype_swe_moments)
elecp = d.swe_mdat(ed1:ed2).fnout*1.38e-16*d.swe_mdat(ed1:ed2).trout

elecp_interpol = interpol(elecp,(d.swe_mdat(ed1:ed2).ta-d.refsec)/3600.d,$
    (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
           
elecp=0

p = ionp + elecp_interpol

end


pro totalp,p

common shared,d

idatype=where(d.datype eq 'swe_ionkp')
id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

idatype_swe_moments=where(d.datype eq 'swe_moments')

ionp=d.swe_ionkpdat(id1:id2).ionp 

ed1=d.ndx(0,idatype_swe_moments) & ed2=d.ndx(1,idatype_swe_moments)
;elecp = d.swe_mdat(ed1:ed2).fnout * d.swe_mdat(ed1:ed2).eavg * 1.6022e-12
elecp=d.swe_mdat(ed1:ed2).fnout*1.38e-16*d.swe_mdat(ed1:ed2).trout

elecp_interpol = interpol(elecp,(d.swe_mdat(ed1:ed2).ta-d.refsec)/3600.d,$
           (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
elecp=0

magp_interpol=interpol(d.swe_mdat(ed1:ed2).magp,$
  (d.swe_mdat(ed1:ed2).ta-d.refsec)/3600.d,$
  (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)

p = ionp + elecp_interpol + magp_interpol

end


function elec_avg_on_ion_time,ti,te,evar

ivar=fltarr(n_elements(ti))
for i=0,n_elements(ti)-1 do begin
      case i of
      0: begin
        t0=ti(0)
        t1=ti(0)+(ti(1)-ti(0))/2
      endcase
      n_elements(ti)-1: begin
        t0=ti(n_elements(ti)-1)-(ti(n_elements(ti)-1)-ti(n_elements(ti)-2))/2
        t1=ti(n_elements(ti)-1)
      endcase
      else: begin
        t0=ti(i)-(ti(i)-ti(i-1))/2
        t1=ti(i)+(ti(i+1)-ti(i))/2
      endcase
      endcase
      w=where(te ge t0 and te lt t1,nw)
      if nw gt 0 then ivar(i)=total(evar(w))/nw $
      else ivar(i)=0.
    endfor
    return,ivar
    end
    
    
;============== function ionkpvar =============================================


;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function swe_ionkpvar,varname

common magkpstuff,magkpflnm,magkpdat
common shared,d

idatype=where(d.datype eq 'swe_ionkp')
idatype_swe_moments=where(d.datype eq 'swe_moments')
idatype_mfi_magkp=where(d.datype eq 'mfi_magkp')

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)
print,'ionkpvar : varname ',varname

case varname of

'Ui flow speed': return,d.swe_ionkpdat(id1:id2).v

'Uew': return,d.swe_ionkpdat(id1:id2).vew

'Uns': return,d.swe_ionkpdat(id1:id2).vns

'Ux': return, d.swe_ionkpdat(id1:id2).v * $
         cos(d.swe_ionkpdat(id1:id2).vns*!dtor)*$
         cos((180.-d.swe_ionkpdat(id1:id2).vew)*!dtor)
         
'Uy': return, d.swe_ionkpdat(id1:id2).v * $
         cos(d.swe_ionkpdat(id1:id2).vns*!dtor)*$
         sin((180.-d.swe_ionkpdat(id1:id2).vew)*!dtor) 

'Uz' : return, d.swe_ionkpdat(id1:id2).v * sin(d.swe_ionkpdat(id1:id2).vns*!dtor)         
                 

'W thermal speed': return,d.swe_ionkpdat(id1:id2).w   ;most probable speed

'Ni density': return,d.swe_ionkpdat(id1:id2).n

'ram pressure' : return, $
  0.5*d.swe_ionkpdat(id1:id2).n * 1.6726e-24 * (d.swe_ionkpdat(id1:id2).v *1e5)^2 * 1e8

'plasma pressure (i+e)': begin
     plasmap,p
     d.swe_ionkpdat(id1:id2).eip=p & p=0
     return,1e10 * d.swe_ionkpdat(id1:id2).eip
   endcase
   
'ion pressure': return,d.swe_ionkpdat(id1:id2).ionp*1e10

'ion + magkp pressure' : begin
   idatype_mfi_magkp=where(d.datype eq 'mfi_magkp')
   if idatype_mfi_magkp(0) eq -1 then return,fltarr(id2-id1+1)
   ionp=d.swe_ionkpdat(id1:id2).ionp
   md1=d.ndx(0,idatype_mfi_magkp) & md2=d.ndx(1,idatype_mfi_magkp)
   magp_interpol=interpol(d.mfi_magkpdat(md1:md2).magp,$
  (d.mfi_magkpdat(md1:md2).ta-d.refsec)/3600.d,(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
  return,(ionp + magp_interpol)*1e10
                         endcase

'total pressure (i+e+b)': begin
     totalp,p
     return,1e10 * p
   endcase

'plasma beta': begin
     magp=d.swe_mdat(d.ndx(0,idatype_swe_moments):d.ndx(1,idatype_swe_moments)).magp 
     magt=(d.swe_mdat(d.ndx(0,idatype_swe_moments):d.ndx(1,idatype_swe_moments)).ta-$
       d.refsec)/3600.d   
     magp_interpol=interpol(magp(where(magp ne 0)),magt(where(magp ne 0)), $ 
               (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)   
     magp=0 & magt=0
     plasmap,p
     d.swe_ionkpdat(id1:id2).beta=p/magp_interpol
     return,d.swe_ionkpdat(id1:id2).beta
   endcase

'Ti temperature': begin
  return,$
   (1.6726e-24/(2*1.38e-16))*(d.swe_ionkpdat(id1:id2).w*d.swe_ionkpdat(id1:id2).w)*1e10
  endcase

'Ue - Ui': begin
     elecv_interpol = $
       interpol(d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                     d.ndx(1,idatype_swe_moments)).umag,$
           (d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                 d.ndx(1,idatype_swe_moments)).ta-d.refsec)/3600.d,$
           (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
     d.swe_ionkpdat(id1:id2).eiv=elecv_interpol-d.swe_ionkpdat(id1:id2).v
     return,d.swe_ionkpdat(id1:id2).eiv
   endcase

'Te / Ti' : begin
   te_interpol = interpol(d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                               d.ndx(1,idatype_swe_moments)).trout,$
           (d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                 d.ndx(1,idatype_swe_moments)).ta-d.refsec)/3600.d,$
           (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
   ti=(2*1.6726e-24/(3*1.38e-16))*$
    (d.swe_ionkpdat(id1:id2).w*d.swe_ionkpdat(id1:id2).w)*1e10
   return,te_interpol / ti
            endcase
    
'V ionacous thresh' : begin
    te_interpol = interpol(d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                           d.ndx(1,idatype_swe_moments)).trout,$
           (d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                 d.ndx(1,idatype_swe_moments)).ta-d.refsec)/3600.d,$
           (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
    ti=(2*1.6726e-24/(3*1.38e-16))*$
      (d.swe_ionkpdat(id1:id2).w*d.swe_ionkpdat(id1:id2).w)*1e10
    v_thrsh=sqrt(ti/te_interpol) * sqrt(1e-10*1.38e-16*te_interpol/9.11e-28)
    return,v_thrsh
                      endcase

'QI' : begin
   idatype_mfi_magkp=where(d.datype eq 'mfi_magkp')
   if idatype_mfi_magkp(0) eq -1 then return,fltarr(id2-id1+1)
   md1=d.ndx(0,idatype_mfi_magkp) & md2=d.ndx(1,idatype_mfi_magkp)
   magp_interpol=interpol(d.mfi_magkpdat(md1:md2).magp,$
  (d.mfi_magkpdat(md1:md2).ta-d.refsec)/3600.d,(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
  ramp=$
  0.5*d.swe_ionkpdat(id1:id2).n * 1.6726e-24 * (d.swe_ionkpdat(id1:id2).v *1e5)^2 * 1e8
  return,(magp_interpol / ramp)*1e10  
endcase

'Te (interpolated)' : begin
  te_interpol = interpol($
            d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                 d.ndx(1,idatype_swe_moments)).trout,$
           (d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                 d.ndx(1,idatype_swe_moments)).ta-d.refsec)/3600.d,$
           (d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d)
  return,te_interpol
endcase

  
'Bx (mfikp avg on ion times)': begin
    te=(d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).ta-d.refsec)/3600.d
    evar=d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).bf(0)
    ti=(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d                   
    ivar=elec_avg_on_ion_time(ti,te,evar)
    return,ivar
  endcase 

'By (mfikp avg on ion times)': begin
    te=(d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).ta-d.refsec)/3600.d
    evar=d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).bf(1)
    ti=(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d                   
    ivar=elec_avg_on_ion_time(ti,te,evar)
    return,ivar
  endcase 
  
'Bz (mfikp avg on ion times)': begin
    te=(d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).ta-d.refsec)/3600.d
    evar=d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).bf(2)
    ti=(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d                   
    ivar=elec_avg_on_ion_time(ti,te,evar)
    return,ivar
  endcase 

'B (mfikp avg on ion times)': begin
    te=(d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).ta-d.refsec)/3600.d
    ti=(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d 

    ;Bx:    
      evar=d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).bf(0)                  
      bx=elec_avg_on_ion_time(ti,te,evar)
    
    ;By:    
      evar=d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).bf(1)                  
      by=elec_avg_on_ion_time(ti,te,evar)
      
    ;Bz:    
      evar=d.mfi_magkpdat(d.ndx(0,idatype_mfi_magkp):$
                     d.ndx(1,idatype_mfi_magkp)).bf(2)                  
      bz=elec_avg_on_ion_time(ti,te,evar)    
    
    ;B magnitude
      ivar=sqrt(bx^2 + by^2 + bz^2)
        
    return,ivar
  endcase 
    
'Te (avg on ion times)': begin
    te=(d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                     d.ndx(1,idatype_swe_moments)).ta-d.refsec)/3600.d
    evar=d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                     d.ndx(1,idatype_swe_moments)).trout
    ti=(d.swe_ionkpdat(id1:id2).ta-d.refsec)/3600.d                   
    ivar=elec_avg_on_ion_time(ti,te,evar)
    return,ivar
  endcase                   
endcase
end
