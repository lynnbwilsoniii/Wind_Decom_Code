pro cts_f,vsteps,cts_factor,cf,ion=ion


common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

;returns conversion of counts to f given offset into voltage table,
; ex: phase density = uncompressed counts * cts_factor
;where vsteps = volt table offset

;also returns 1 count level 
; ex: one count level of phase density = cts_factor

  ;if sp.spinp lt 0.9*3.05 or sp.spinp gt 1.1*3.05 then begin 
   ; print,'cts_f: bad spinperiod'
  ;  return
  ;endif

if keyword_set(ion) eq 0 then ions=0 else ions=1
 
case ions of
  0: delt=vsmjf.delt_ele
  1: delt=vsmjf.delt_ion
endcase

cf = vsmjf.geomf * delt

cts_factor=fltarr(6,n_elements(vsteps))

cts_factor= 1.e-32 / (cf # (volt_en(vsteps,/vel,ion=ions)/1e8)^4)

;checking matrix multiplication
   ;print,cts_factor
   ;stop
   ;for i=0,5 do for j=0,n_elements(vsteps)-1 do $
   ;cts_factor(i,j)=1.e-32  / (cf(i) * (volt_en(vsteps(j),/vel,ion=ions)/1e8)^4)
   ;print,cts_factor
   ;stop

;print,'relgains ',vsmjf.relgain,format='(a10,6f7.3)'

end
