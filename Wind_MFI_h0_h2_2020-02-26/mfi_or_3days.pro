FUNCTION MFI_OR_3DAYS, EPOCH_DATA=EPOCH_DATA, EPOCH_POS=EPOCH_POS, POS_GSE=POS_GSE, Q_POS_GSE=Q_POS_GSE, $
                       POS_GSM=POS_GSM, Q_POS_GSM=Q_POS_GSM

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read orbit (current, previous, and next days)
n_data_hr = n_elements(epoch_hr)
cdf_epoch, epoch_data[0],       year_cur,  month_cur,  day_cur,  /b
cdf_epoch, epoch_data[0]-MSDAY, year_prev, month_prev, day_prev, /b
cdf_epoch, epoch_data[0]+MSDAY, year_next, month_next, day_next, /b
date_str_cur  = string(year_cur, format='(I4)') + string(month_cur, format='(I02)') + string(day_cur, format='(I02)')
date_str_prev = string(year_prev,format='(I4)') + string(month_prev,format='(I02)') + string(day_prev,format='(I02)')
date_str_next = string(year_next,format='(I4)') + string(month_next,format='(I02)') + string(day_next,format='(I02)')
;- read_or_res_cur
   read_or_res_cur  = mfi_read_or(or_path=def_or_path, or_prefix=def_or_prefix+date_str_cur,  orbit=or_cur)
if read_or_res_cur ne 0 then $
   read_or_res_cur  = mfi_read_or(or_path=pre_or_path, or_prefix=pre_or_prefix+date_str_cur,  orbit=or_cur)
;- read_or_res_prev
   read_or_res_prev = mfi_read_or(or_path=def_or_path, or_prefix=def_or_prefix+date_str_prev, orbit=or_prev)
if read_or_res_prev ne 0 then $
   read_or_res_prev = mfi_read_or(or_path=pre_or_path, or_prefix=pre_or_prefix+date_str_prev, orbit=or_prev)
;- read_or_res_next
   read_or_res_next = mfi_read_or(or_path=def_or_path, or_prefix=def_or_prefix+date_str_next, orbit=or_next)
if read_or_res_next ne 0 then $
   read_or_res_next = mfi_read_or(or_path=pre_or_path, or_prefix=pre_or_prefix+date_str_next, orbit=or_next)

;- fill epoch_or, or_gse, or_gsm, q_or_gse, q_or_gsm
if read_or_res_cur eq 0 then begin
   ;- current day
   epoch_pos = or_cur.epoch.value
   pos_gse   = or_cur.gse_pos.value
   pos_gsm   = or_cur.gsm_pos.value
   q_pos_gse = or_cur.gse_pos.q[0,*] or or_cur.gse_pos.q[1,*] or or_cur.gse_pos.q[2,*] or or_cur.epoch.q
   q_pos_gsm = or_cur.gsm_pos.q[0,*] or or_cur.gsm_pos.q[1,*] or or_cur.gsm_pos.q[2,*] or or_cur.epoch.q

   ;- previous day
   if read_or_res_prev eq 0 then begin
      epoch_pos_prev = or_prev.epoch.value
      pos_gse_prev   = or_prev.gse_pos.value
      pos_gsm_prev   = or_prev.gsm_pos.value
      q_pos_gse_prev = or_prev.gse_pos.q[0,*] or or_prev.gse_pos.q[1,*] or or_prev.gse_pos.q[2,*] or or_prev.epoch.q
      q_pos_gsm_prev = or_prev.gsm_pos.q[0,*] or or_prev.gsm_pos.q[1,*] or or_prev.gsm_pos.q[2,*] or or_prev.epoch.q
                      
      ;- add to current day records
      epoch_pos = [[epoch_pos_prev], [epoch_pos]]
      pos_gse   = [[pos_gse_prev],   [pos_gse]]
      pos_gsm   = [[pos_gsm_prev],   [pos_gsm]]
      q_pos_gse = [[q_pos_gse_prev], [q_pos_gse]]
      q_pos_gsm = [[q_pos_gsm_prev], [q_pos_gsm]]
   endif
   
   ;- next day
   if read_or_res_next eq 0 then begin
      epoch_pos_next = or_next.epoch.value
      pos_gse_next   = or_next.gse_pos.value
      pos_gsm_next   = or_next.gsm_pos.value
      q_pos_gse_next = or_next.gse_pos.q[0,*] or or_next.gse_pos.q[1,*] or or_next.gse_pos.q[2,*] or or_next.epoch.q
      q_pos_gsm_next = or_next.gsm_pos.q[0,*] or or_next.gsm_pos.q[1,*] or or_next.gsm_pos.q[2,*] or or_next.epoch.q
   
      ;- add to current day records
      epoch_pos = [[epoch_pos], [epoch_pos_next]]
      pos_gse   = [[pos_gse],   [pos_gse_next]]
      pos_gsm   = [[pos_gsm],   [pos_gsm_next]]
      q_pos_gse = [[q_pos_gse], [q_pos_gse_next]]
      q_pos_gsm = [[q_pos_gsm], [q_pos_gsm_next]]
   endif
   
   ;- transfer pos_gse, pos_gsm to Re
   pos_gse /= RE_KM
   pos_gsm /= RE_KM

   ;- take only unique elements
   sub_uniq_sort  = uniq(epoch_pos, sort(epoch_pos, /L64))
   epoch_pos      = epoch_pos[sub_uniq_sort]
   pos_gse        = pos_gse[*,sub_uniq_sort]
   q_pos_gse      = q_pos_gse[sub_uniq_sort]
   pos_gsm        = pos_gsm[*,sub_uniq_sort]
   q_pos_gsm      = q_pos_gsm[sub_uniq_sort]
   
   ;- return 0
   return, 0
endif else begin

   ;- return 1
   return, 1
endelse

END