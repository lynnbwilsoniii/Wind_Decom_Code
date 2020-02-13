pro cts_f_strl_det,det,pb5,strl_enstep,scimod,strl_cts_factor


;returns conversion factor (one-count level) of strahl counts to f for given det 
;given energy and veis mode which determines sampling time

;this version is used with the strahl survey files

iclicks_sunpulse=4096 & iclicks_bci=40 
deltasp=double(iclicks_bci)/double(iclicks_sunpulse)
tjdsec=pb5_tjd(pb5)
if tjdsec(0) ge 9657 and $
   (double(tjdsec(0))+tjdsec(1)/86400.d) le (double(9976)+41345.485d/86400.d) $
  then deadtim_ele=0.000800d else deadtim_ele=0.004d
spinp=3.1  ;nominal spin period  
delt=spinp*deltasp - deadtim_ele

;strahl counts to f factor: cf_strl;  (efficiencies for hv step2)
  if scimod eq 2 then begin
    efficiency = [0.75926, 0.75926, $
                  0.82945, 0.82945]
    geometry = [(4.4349 + 5.4222 + 6.1882), (6.1182 + 6.4422 + 6.5440), $
                (6.5149 + 6.2687 + 5.9394), (6.2334  + 5.5779  + 4.5819)]
  endif else begin
    efficiency = [0.75926, 0.75926, 0.75926, 0.75926, 0.75926, 0.75926, $
                  0.82945,  0.82945, 0.82945, 0.82945, 0.82945, 0.82945]

    geometry = [ 4.4349, 5.4222, 6.1882,  6.1182, 6.4422, 6.5440,$
                 6.5149, 6.2687, 5.9394, 6.2334, 5.5779, 4.5819]
  endelse              

cf_strl = geometry(det) * efficiency(det) * delt * 0.5 * 1.e-5

strl_cts_factor=fltarr(n_elements(strl_enstep))

strl_cts_factor= $
  1.e-32 / (cf_strl * (volt_en_strl(strl_enstep,/vel)/1e8)^4)


end
