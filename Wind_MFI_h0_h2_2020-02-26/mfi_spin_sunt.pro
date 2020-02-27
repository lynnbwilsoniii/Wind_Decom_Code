PRO MFI_SPIN_SUNT, HK_RECS=RECS, SPIN=SPIN, QSPIN=QSPIN, SUNT=SUNT, QSUNT=QSUNT, $
   LSTART=LSTART, QLSTART=QLSTART, TSTART=TSTART, QTSTART=QTSTART

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- spin and sunt arrays
n_mafr = (size(recs))[1]
spin   = dblarr(10, n_mafr)
qspin  = bytarr(10, n_mafr)
sunt   = dblarr(50, n_mafr)
qsunt  = bytarr(50, n_mafr)

;- reform recs
records = reform(recs, 1, n_mafr)

;--- spin rate and spin rate initial check
;- spin rate (rad/ms) and quality; check that spin period is within 10% of 3s
for i=0, 9 do begin
   spin[i,*]   = (records.mafr[0, 13 + i*25]*256L + records.mafr[1, 13 + i*25])*$
                 5.4931640625d-3*!DTOR/1d3
   qspin[i,*]  = (records.q_mifr[13 + i*25] ne 0) or ((records.mafr[0, 16 + i*25] and 1b) ne 1) or $
                 (spin[i,*]*60d3/(2d*!dpi) lt 60d/3.3d) or (spin[i,*]*60d3/(2d*!dpi) gt 60d/2.7d)
endfor

;- replace bad quality spin with neareset good quality spin from the same mafr
total_qspin_mafr = total(qspin, 1) 
sub_qspin_mafr   = where((total_qspin_mafr gt 0) and (total_qspin_mafr lt 10), n_qspin_mafr)
for i_mafr=0, n_qspin_mafr-1 do begin
   sub_bad = where(qspin[*,sub_qspin_mafr[i_mafr]] eq 1, n_bad, complement=sub_good)
   for i_bad=0, n_bad-1 do begin
      temp = min(abs(sub_good - sub_bad[i_bad]), sub_min)
      spin[sub_bad[i_bad], sub_qspin_mafr[i_mafr]]  = spin[sub_good[sub_min], sub_qspin_mafr[i_mafr]] 
      qspin[sub_bad[i_bad], sub_qspin_mafr[i_mafr]] = 0b
   endfor
endfor
;--- end: spin rate and spin rate initial check

;--- sun time, and sun time and spin rate check
;- sun times, msec since start of cycle, each cycle 256000 ms, first cycle starts at day start
for i=0, 49 do begin
   sunt[i,*]  = records.mafr[4, 1 + i*5]*1000L + records.mafr[5, 1 + i*5]*4L + $
               (records.mafr[6, 1 + i*5] and 192b)/64L + (records.mafr[6, 1 + i*5] and 63b)/64d
   qsunt[i,*] = (records.q_mifr[1 + i*5] ne 0)
endfor

;- handle bad see sun times, all bytes eq 0
sub_temp = where((qsunt eq 0) and (sunt eq 0), n_temp)
if (n_temp ne 0) then qsunt[sub_temp] = 1

;- convert sunt to epoch, check for sunt out of major frame
records_epoch     = reform(records.epoch, 1, n_mafr)
records_msec_day  = reform(records.msec_day,  1, n_mafr)
cycle_epoch   = records_epoch - (records_msec_day mod CYCLE)
record_rate   = reform(recs.tel_mode lt 5, 1, n_mafr)*TIMEMF/2L + TIMEMF/2L
for i=0, 49 do begin
   sunt[i,*] = cycle_epoch + sunt[i,*]
   sub_temp  = where(sunt[i,*] lt (records_epoch - 3.6d3), n_temp)
   if (n_temp gt 0) then sunt[i,sub_temp] = sunt[i,sub_temp] + CYCLE
   sub_temp  = where(sunt[i,*] gt (records_epoch + record_rate), n_temp)
   if (n_temp gt 0) then sunt[i,sub_temp] = sunt[i,sub_temp] - CYCLE
   sub_temp  = where(sunt[i,*] lt (records_epoch - 3.6d3), n_temp)
   if (n_temp gt 0) then qsunt[i,sub_temp] = 1
   sub_temp  = where(sunt[i,*] gt (records_epoch + record_rate), n_temp)
   if (n_temp gt 0) then qsunt[i,sub_temp] = 1
endfor

;- for last mafr, which contains data from next day, with new counter, use previous mafr sunt
if n_mafr gt 1 then begin
   sunt[*,n_mafr-1]  = sunt[*,n_mafr-2]
   qsunt[*,n_mafr-1] = qsunt[*,n_mafr-2]
endif

;- check that sun time and spin rate are consistent withing 1%
sun_dt0        = sunt[1L:50L*n_mafr-2L] - sunt[0L:50L*n_mafr-3L]
sun_dt1        = sunt[2L:50L*n_mafr-1L] - sunt[1L:50L*n_mafr-2L]
sub_temp       = indgen(50L*n_mafr)/5L
spin_to_sunt   = spin[sub_temp]
spin_to_sunt0  = spin_to_sunt[0L:50L*n_mafr-3L]
spin_to_sunt1  = spin_to_sunt[1L:50L*n_mafr-2L]
spin_to_sunt2  = spin_to_sunt[2L:50L*n_mafr-1L]
mask0  = ((sun_dt0*spin_to_sunt0/(2d*!dpi) lt 0.99d) or (sun_dt0*spin_to_sunt0/(2d*!dpi) gt 1.01d)) and $
         (sun_dt0 ne 0)
mask10 = ((sun_dt0*spin_to_sunt1/(2d*!dpi) lt 0.99d) or (sun_dt0*spin_to_sunt1/(2d*!dpi) gt 1.01d)) and $
         (sun_dt0 ne 0)
mask11 = ((sun_dt1*spin_to_sunt1/(2d*!dpi) lt 0.99d) or (sun_dt1*spin_to_sunt1/(2d*!dpi) gt 1.01d)) and $
         (sun_dt1 ne 0)
mask2  = ((sun_dt1*spin_to_sunt2/(2d*!dpi) lt 0.99d) or (sun_dt1*spin_to_sunt2/(2d*!dpi) gt 1.01d)) and $
         (sun_dt1 ne 0)
sub_bad_sunt   = where(mask0 and mask10 and mask11 and mask2, n_bad_sunt)
sub_bad_spin   = where(mask10 and mask11, n_bad_spin)
if n_bad_sunt gt 0 then begin
   qsunt[sub_bad_sunt + 1L] = 1
end
if n_bad_spin gt 0 then begin
   qspin_temp = bytarr(50, n_mafr)
   qspin_temp[sub_bad_spin + 1L] = 1
   qspin or= product(reform(qspin_temp, 5, 10, n_mafr), 1, /preserve_type)
endif

;- replace 0 and last sunt with nearest values
sunt[0]   =  sunt[1]
qsunt[0]  = qsunt[1]
sunt[50L*n_mafr-1L]  =  sunt[50L*n_mafr-2L]
qsunt[50L*n_mafr-1L] = qsunt[50L*n_mafr-2L]

;- set quality of isolated good sunt to bad
sub0 = where(qsunt eq 0, n_sub0)
if n_sub0 gt 0 then begin
   d_sub0_left  = (sub0[1L:n_sub0-2L] - sub0[0L:n_sub0-3L])
   d_sub0_right = (sub0[2L:n_sub0-1L] - sub0[1L:n_sub0-2L])
   sub1 = where((d_sub0_left gt 3) and (d_sub0_right gt 3), n_sub1)
   if n_sub1 gt 0 then qsunt[sub0[sub1+1L]] = 1
endif

;- replace bad quality sunt with nearest good quality sunt from the same mafr
total_qsunt_mafr = total(qsunt, 1) 
sub_qsunt_mafr   = where((total_qsunt_mafr gt 0) and (total_qsunt_mafr lt 50), n_qsunt_mafr)
for i_mafr=0, n_qsunt_mafr-1 do begin
   sub_bad = where(qsunt[*,sub_qsunt_mafr[i_mafr]] eq 1, n_bad, complement=sub_good)
   for i_bad=0, n_bad-1 do begin
      temp = min(abs(sub_good - sub_bad[i_bad]), sub_min)
      sunt[sub_bad[i_bad], sub_qsunt_mafr[i_mafr]]  = sunt[sub_good[sub_min], sub_qsunt_mafr[i_mafr]] 
      qsunt[sub_bad[i_bad], sub_qsunt_mafr[i_mafr]] = 0b
   endfor
endfor

;- replace bad quality spin with neareset good quality spin from the same mafr
total_qspin_mafr = total(qspin, 1) 
sub_qspin_mafr   = where((total_qspin_mafr gt 0) and (total_qspin_mafr lt 10), n_qspin_mafr)
for i_mafr=0, n_qspin_mafr-1 do begin
   sub_bad = where(qspin[*,sub_qspin_mafr[i_mafr]] eq 1, n_bad, complement=sub_good)
   for i_bad=0, n_bad-1 do begin
      temp = min(abs(sub_good - sub_bad[i_bad]), sub_min)
      spin[sub_bad[i_bad], sub_qspin_mafr[i_mafr]]  = spin[sub_good[sub_min], sub_qspin_mafr[i_mafr]] 
      qspin[sub_bad[i_bad], sub_qspin_mafr[i_mafr]] = 0b
   endfor
endfor
;--- end: sun time, and sun time and spin rate check

END