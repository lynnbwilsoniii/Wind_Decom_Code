; based on R.J. Fitzenreiter's cts_f_strl_det.pro

; f is 14x12 array
; strl_enstep the energy step of the measurement (integer)

; time is seconds since '1970-01-01/00:00:00'  (format used by time_double.pro)

FUNCTION swe_strahl_counts_to_phys, f, strl_enstep, time

;returns conversion factor (one-count level) of strahl counts to f
;given energy and veis mode which determines sampling time

iclicks_sunpulse=4096 & iclicks_bci=40 
deltasp=double(iclicks_bci)/double(iclicks_sunpulse)

;v old v
;tjdsec=pb5_tjd(pb5)                      ; DOES THIS WORK CORRECTLY???
;if tjdsec(0) ge 9657 and $
;   (double(tjdsec(0))+tjdsec(1)/86400.d) le (double(9976)+41345.485d/86400.d) $
;  then deadtim_ele=0.000800d else deadtim_ele=0.004d
;^ old ^
;v new ^
;translation of the pb5 code above
IF time_double(strmid(time_string(time),0,10)) GE time_double('1994-10-01') AND $
   time LE time_double('1995-09-16') + 41345.485d $
  then deadtim_ele=0.000800d else deadtim_ele=0.004d
;^ new ^

spinp=3.1d  ;nominal spin period
delt=spinp*deltasp - deadtim_ele

;strahl counts to f factor: cf_strl;  (efficiencies for hv step2)
efficiency = [0.75926d, 0.75926, 0.75926, 0.75926, 0.75926, 0.75926, $
              0.82945,  0.82945, 0.82945, 0.82945, 0.82945, 0.82945]

geometry = [ 4.4349d, 5.4222, 6.1882,  6.1182, 6.4422, 6.5440,$
             6.5149, 6.2687, 5.9394, 6.2334, 5.5779, 4.5819]

cf_strl = geometry * efficiency * delt * 0.5d * 1.d-5

;strl_cts_factor=fltarr(n_elements(strl_enstep))

strl_cts_factor= $
  1.e-32 / (cf_strl * (volt_en_strl(strl_enstep,/vel)/1d8)^4)

counts_to_phys = (dblarr(14) + 1.d) # reverse(strl_cts_factor)   ; reversed because thetas are reversed from
                                                                 ; the order used by Fitzenreiter

RETURN, f*counts_to_phys     ; units cm^-6 * sec^-3

END
