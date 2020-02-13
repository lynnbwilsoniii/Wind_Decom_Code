;================================ Cts_f_Strl =================================
; This procedure returns (through OUTPUT parameters) the conversion factor of
;  strahl counts to f (phase density) given an offset into the strahl voltage
;  table.  Ex: phase density = uncompressed counts*strl_cts_factor, where
;  strl_vsteps = strahl voltage table offset.  Also returns 1 count level. 
;          Ex: one count level of phase density = strl_cts_factor.
PRO cts_f_strl,strl_vsteps,strl_cts_factor,cf_strl,Mode2=mode2,Mode7=mode7

; These common-blocks allow data sharing among the reading/plotting routines...
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp & common m1stuff,hkm1,vsm1,vdatc,sdatc

mode2 = keyword_set(mode2) & mode7 = keyword_set(mode7) & delt = vsmjf.delt_ele

;             Strahl counts to f factor: cf_strl (efficiencies for hv step2)...
if mode2 then begin ;                         Special case of science mode 2...
   efficiency = [0.75926, 0.75926, 0.82945, 0.82945]
   geometry = [(4.4349 + 5.4222 + 6.1882), (6.1182 + 6.4422 + 6.5440),$
               (6.5149 + 6.2687 + 5.9394), (6.2334  + 5.5779  + 4.5819)]
endif else begin ;                            All other cases (except below)...
   efficiency = [0.75926, 0.75926, 0.75926, 0.75926, 0.75926, 0.75926,$
                 0.82945,  0.82945, 0.82945, 0.82945, 0.82945, 0.82945]
   geometry = [4.4349, 5.4222, 6.1882, 6.1182, 6.4422, 6.5440,$
               6.5149, 6.2687, 5.9394, 6.2334, 5.5779, 4.5819]
endelse              

if mode7 then begin ; Special case of science mode 7 (overrides other modes)...
   efficiency = [0.75926, 0.75926, 0.75926, 0.82945, 0.82945, 0.82945]
   geometry = [ (4.4349 + 5.4222), (6.1882 + 6.1182), (6.4422 + 6.5440),$
                (6.5149 + 6.2687), (5.9394 + 6.2334), (5.5779 + 4.5819)]
endif

cf_strl = geometry*efficiency*delt*0.5*1.e-5 ;                 Final outputs...
strl_cts_factor = fltarr(n_elements(geometry),n_elements(strl_vsteps))
strl_cts_factor = 1.e-32/(cf_strl#(volt_en_strl(strl_vsteps,/vel)/1e8)^4)

end
