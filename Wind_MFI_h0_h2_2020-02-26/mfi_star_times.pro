PRO MFI_STAR_TIMES, HK_RECS=RECS, LSTART=LSTART, QLSTART=QLSTART, TSTART=TSTART, QTSTART=QTSTART

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- spin and sunt arrays
n_mafr  = (size(recs))[1]
lstart  = dblarr(50, n_mafr)
qlstart = dblarr(50, n_mafr)
tstart  = dblarr(50, n_mafr)
qtstart = dblarr(50, n_mafr)

;- reform recs
records = reform(recs, 1, n_mafr)

;--- leadstar times
;- leadstar times, msec since start of cycle, each cycle 256000 ms, first cycle starts at day start
for i=0, 49 do begin
   lstart[i,*]  = records.mafr[4, 2 + i*5]*1000L + records.mafr[5, 2 + i*5]*4L + $
                 (records.mafr[6, 2 + i*5] and 192b)/64L + (records.mafr[6, 2 + i*5] and 63b)/64d
   qlstart[i,*] = (records.q_mifr[1 + i*5] ne 0)
endfor

;- handle bad see leadstar times, all bytes eq 0
sub_temp = where((qlstart eq 0) and (lstart eq 0), n_temp)
if (n_temp ne 0) then qlstart[sub_temp] = 1

;- convert lstart to epoch, and check for lstart out of major frame
records_epoch     = reform(records.epoch, 1, n_mafr)
records_msec_day  = reform(records.msec_day,  1, n_mafr)
cycle_epoch   = records_epoch - (records_msec_day mod CYCLE)
record_rate   = reform(recs.tel_mode lt 5, 1, n_mafr)*TIMEMF/2L + TIMEMF/2L
for i=0, 49 do begin
   lstart[i,*] = cycle_epoch + lstart[i,*]
   sub_temp  = where(lstart[i,*] lt (records_epoch - 3.6d3), n_temp)
   if (n_temp gt 0) then lstart[i,sub_temp] = lstart[i,sub_temp] + CYCLE
   sub_temp  = where(lstart[i,*] gt (records_epoch + record_rate), n_temp)
   if (n_temp gt 0) then lstart[i,sub_temp] = lstart[i,sub_temp] - CYCLE
   sub_temp  = where(lstart[i,*] lt (records_epoch - 3.6d3), n_temp)
   if (n_temp gt 0) then qlstart[i,sub_temp] = 1
   sub_temp  = where(lstart[i,*] gt (records_epoch + record_rate), n_temp)
   if (n_temp gt 0) then qlstart[i,sub_temp] = 1
endfor

;- handle bad see leadstar times 10% off
lstart_dt  = lstart[1:49,*] - lstart[0:48,*]
sub_temp = where( ((lstart_dt lt 2.7d3) or (lstart_dt gt 3.3d3)) and $
                  (lstart_dt ne 0) and (qlstart[0:48,*] eq 0), n_sub_temp)
if (n_sub_temp ne 0) then begin
   var_temp = qlstart[0:48,*] & var_temp[sub_temp] = 1 & qlstart[0:48,*] = var_temp
   var_temp = qlstart[1:49,*] & var_temp[sub_temp] = 1 & qlstart[1:49,*] = var_temp
end

;- for last mafr, which contain data from next day, with new counter, use previous mafr lstart
if n_mafr gt 1 then begin
   lstart[*,n_mafr-1]  = lstart[*,n_mafr-2]
   qlstart[*,n_mafr-1] = qlstart[*,n_mafr-2]
endif
;--- end: leadstar time

;--- tailstar times
;- tailstar times, msec since start of cycle, each cycle 256000 ms, first cycle starts at day start
for i=0, 49 do begin
   tstart[i,*]  = records.mafr[4, 3 + i*5]*1000L + records.mafr[5, 3 + i*5]*4L + $
                 (records.mafr[6, 3 + i*5] and 192b)/64L + (records.mafr[6, 3 + i*5] and 63b)/64d
   qtstart[i,*] = (records.q_mifr[1 + i*5] ne 0)
endfor

;- handle bad see tailstar times, all bytes eq 0
sub_temp = where((qtstart eq 0) and (tstart eq 0), n_temp)
if (n_temp ne 0) then qtstart[sub_temp] = 1

;- convert tstart to epoch, and check for tstart out of major frame
records_epoch     = reform(records.epoch, 1, n_mafr)
records_msec_day  = reform(records.msec_day,  1, n_mafr)
cycle_epoch   = records_epoch - (records_msec_day mod CYCLE)
record_rate   = reform(recs.tel_mode lt 5, 1, n_mafr)*TIMEMF/2L + TIMEMF/2L
for i=0, 49 do begin
   tstart[i,*] = cycle_epoch + tstart[i,*]
   sub_temp  = where(tstart[i,*] lt (records_epoch - 3.6d3), n_temp)
   if (n_temp gt 0) then tstart[i,sub_temp] = tstart[i,sub_temp] + CYCLE
   sub_temp  = where(tstart[i,*] gt (records_epoch + record_rate), n_temp)
   if (n_temp gt 0) then tstart[i,sub_temp] = tstart[i,sub_temp] - CYCLE
   sub_temp  = where(tstart[i,*] lt (records_epoch - 3.6d3), n_temp)
   if (n_temp gt 0) then qtstart[i,sub_temp] = 1
   sub_temp  = where(tstart[i,*] gt (records_epoch + record_rate), n_temp)
   if (n_temp gt 0) then qtstart[i,sub_temp] = 1
endfor

;- handle bad see tailstar times 10% off
tstart_dt  = tstart[1:49,*] - tstart[0:48,*]
sub_temp = where( ((tstart_dt lt 2.7d3) or (tstart_dt gt 3.3d3)) and $
                  (tstart_dt ne 0) and (qtstart[0:48,*] eq 0), n_sub_temp)
if (n_sub_temp ne 0) then begin
   var_temp = qtstart[0:48,*] & var_temp[sub_temp] = 1 & qtstart[0:48,*] = var_temp
   var_temp = qtstart[1:49,*] & var_temp[sub_temp] = 1 & qtstart[1:49,*] = var_temp
end

;- for last mafr, which contain data from next day, with new counter, use previous mafr tstart
if n_mafr gt 1 then begin
   tstart[*,n_mafr-1]  = tstart[*,n_mafr-2]
   qtstart[*,n_mafr-1] = qtstart[*,n_mafr-2]
endif
;--- end: tailstar time

END