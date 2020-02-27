PRO MFI_CALIB_SPINFIT, START_DATE=START_DATE, END_DATE=END_DATE, RANGE=RANGE, OUTER_MAG=OUTER_MAG

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_date)  eq 1) then start_date  = long(start_date)  else message, 'no start_date'
if (n_elements(end_date)    eq 1) then end_date    = long(end_date)    else end_date  = start_date
if (n_elements(range)       eq 1) then range       = byte(range)       else range     = 1b
if keyword_set(outer_mag)         then outer_mag   = 1                 else outer_mag = 0

;- start_ms, end_ms, int_ms; everywhere in the following program end_ms or end_epoch means last ms
start_ms = 0d
end_ms   = MSDAY - 1d
int_ms   = MSDAY

;- convert start_date+start_ms and end_date+end_ms to epoch, and calculate int_ms
cdf_epoch, start_epoch, floor(start_date/1d4), floor((start_date/1d2) mod 1d2), $
                        floor(start_date mod 1d2), 0, 0, 0, start_ms, /compute_epoch
cdf_epoch, end_epoch,   floor(end_date/1d4), floor((end_date/1d2) mod 1d2), $
                        floor(end_date mod 1d2), 0, 0, 0, end_ms, /compute_epoch

;- define start and end epoch of first interval
int_start_epoch = start_epoch
int_end_epoch   = start_epoch + int_ms - 1d

;--- process intervals while int_end_epoch le end_epoch
while (int_end_epoch le end_epoch) do begin
   ;--- find dates covered by interval
   ;- breakdown int_start_epoch and int_end_epoch
   cdf_epoch, int_start_epoch, int_start_year, int_start_month, int_start_day, $
              int_start_hour, int_start_minut, int_start_sec, int_start_milli, $
              /breakdown_epoch
   cdf_epoch, int_end_epoch, int_end_year, int_end_month, int_end_day, $
              int_end_hour, int_end_minut, int_end_sec, int_end_milli, $
              /breakdown_epoch

   ;- interval start date string
   int_start_date_str = string(int_start_year,format='(I4)') + string(int_start_month,format='(I02)') + $
                        string(int_start_day,format='(I02)')
   ;- interval start date string
   int_end_date_str   = string(int_end_year,format='(I4)') + string(int_end_month,format='(I02)') + $
                        string(int_end_day,format='(I02)')
   
   ;- find dates covered by current interval
   date_str_arr = [int_start_date_str]
   if int_end_date_str ne int_start_date_str then begin
      epoch_temp = int_start_epoch
      repeat begin
         epoch_temp += MSDAY
         cdf_epoch, epoch_temp, yr, mo, dy, hr, mn, sc, msc, /breakdown_epoch
         date_str_temp = string(yr,format='(I4)') + string(mo,format='(I02)') + string(dy,format='(I02)')
         date_str_arr = [date_str_arr, date_str_temp]
      endrep until (date_str_temp eq int_end_date_str)
   endif
   ;--- end: find dates covered by interval

   ;- define arrays to store outer/inner spin fit for whole interval
   n_temp = 2000L*(N_SPINFIT_MAFR_PR > N_SPINFIT_MAFR_SC)
   if keyword_set(outer_mag) then begin
      spinfit_ou      = replicate({spinfit_struc}, n_temp)
      qcoefxyz_ou     = bytarr(n_temp) + 1b
      int_coef_ou_sub = 0L
   endif else begin
      spinfit_in      = replicate({spinfit_struc}, n_temp)
      qcoefxyz_in     = bytarr(n_temp) + 1b
      int_coef_in_sub = 0L
   endelse

   ;--- read lz and hk files covered by interval
   for i_lz=0, n_elements(date_str_arr)-1 do begin
      ;- read lz, hr, and spha files
      result_lz   = mfi_read_lz_hk(n_mafr=n_mafr_lz, data_recs=lz_recs, file_path=lz_path, $
                                  file_prefix=lz_prefix+(date_str_arr[i_lz])[0])
      result_hk   = mfi_read_lz_hk(/hk, n_mafr=n_mafr_hk, data_recs=hk_recs, file_path=hk_path, $
                                  file_prefix=hk_prefix+(date_str_arr[i_lz])[0])
      result_spha = mfi_read_spha(spha_recs=spha_recs, file_path=spha_path, $
                                  file_prefix=spha_prefix+(date_str_arr[i_lz])[0])
      ;- check that both lz data and spin/sun are available                            
      if ((result_lz ne 0) or ((result_hk ne 0) and (result_spha ne 0))) then continue
      
      ;--- read spin rate (rad/ms) and sun times from hk_recs
      if result_hk eq 0 then $
         mfi_spin_sunt, hk_recs=hk_recs, spin=spin_hk, qspin=qspin_hk, sunt=sunt_hk, qsunt=qsunt_hk
      
      ;- rewrite bad quality spin_hk with neareset qood quality spin_hk from the same mafr
      total_qspin_hk_mafr = total(qspin_hk, 1) 
      sub_qspin_hk_mafr   = where((total_qspin_hk_mafr gt 0) and (total_qspin_hk_mafr lt 10), n_qspin_hk_mafr)
      for i_hk_mafr=0, n_qspin_hk_mafr-1 do begin
         sub_bad = where(qspin_hk[*,sub_qspin_hk_mafr[i_hk_mafr]] eq 1, n_bad, complement=sub_good)
         for i_bad=0, n_bad-1 do begin
            temp = min(abs(sub_good - sub_bad[i_bad]), sub_min)
            spin_hk[sub_bad[i_bad], sub_qspin_hk_mafr[i_hk_mafr]] = $
               spin_hk[sub_good[sub_min], sub_qspin_hk_mafr[i_hk_mafr]] 
            qspin_hk[sub_bad[i_bad], sub_qspin_hk_mafr[i_hk_mafr]] = 0b
         endfor
      endfor

      ;- rewrite bad quality sunt_hk with neareset qood quality sunt_hk from the same mafr
      total_qsunt_hk_mafr = total(qsunt_hk, 1) 
      sub_qsunt_hk_mafr   = where((total_qsunt_hk_mafr gt 0) and (total_qsunt_hk_mafr lt 50), n_qsunt_hk_mafr)
      for i_hk_mafr=0, n_qsunt_hk_mafr-1 do begin
         sub_bad = where(qsunt_hk[*,sub_qsunt_hk_mafr[i_hk_mafr]] eq 1, n_bad, complement=sub_good)
         for i_bad=0, n_bad-1 do begin
            temp = min(abs(sub_good - sub_bad[i_bad]), sub_min)
            sunt_hk[sub_bad[i_bad], sub_qsunt_hk_mafr[i_hk_mafr]] = $
               sunt_hk[sub_good[sub_min], sub_qsunt_hk_mafr[i_hk_mafr]] 
            qsunt_hk[sub_bad[i_bad], sub_qsunt_hk_mafr[i_hk_mafr]] = 0b
         endfor
      endfor
      ;--- end: read spin rate (rad/ms) and sun times from hk_recs

      ;--- make spin, qspin, sunt, qsunt from either hk (if available) or spha
      ;- make arrays to store
      spin  = dblarr(10, n_mafr_lz)
      qspin = bytarr(10, n_mafr_lz) + 1b
      sunt  = dblarr(50, n_mafr_lz)
      qsunt = bytarr(50, n_mafr_lz) + 1b
      
      ;- first fill arrays from hk
      for i_lz_mafr=0, n_mafr_lz-1 do begin
         sub_lz_hk_mafr = (where(hk_recs.epoch eq lz_recs[i_lz_mafr].epoch, n_lz_hk_mafr))[0]
         if (n_lz_hk_mafr gt 0) then begin
            spin[*,i_lz_mafr]  = spin_hk[*,sub_lz_hk_mafr]
            qspin[*,i_lz_mafr] = qspin_hk[*,sub_lz_hk_mafr]
            sunt[*,i_lz_mafr]  = sunt_hk[*,sub_lz_hk_mafr]
            qsunt[*,i_lz_mafr] = qsunt_hk[*,sub_lz_hk_mafr]
         endif
      endfor

      ;- then fill arrays elements with bad quality with data from spha (if available)
      sub_badq_mafr = where((qspin[0,*] ne 0) or (qsunt[0,*] ne 0), n_badq_mafr)
      if (result_spha eq 0) and (n_badq_mafr gt 0) then begin
         sub_qspha = where((spha_recs.fault.value eq 0) and (spha_recs.fault.q eq 0) and $
                           (spha_recs.epoch.q eq 0) and (spha_recs.spin_phase.q eq 0) and $
                           (spha_recs.avg_spin_rate.q eq 0), n_qspha)
         if (n_qspha gt 0) then begin
            for i_lz_mafr=0, n_badq_mafr-1 do begin
               d_epoch = (abs(spha_recs.epoch.value[sub_qspha] - lz_recs[sub_badq_mafr[i_lz_mafr]].epoch))
               min_d_epoch = min(d_epoch, sub_min_d_epoch)
               if (min_d_epoch lt 60d3) then begin
                  spin[*, sub_badq_mafr[i_lz_mafr]]  = $
                     spha_recs.avg_spin_rate.value[sub_qspha[sub_min_d_epoch]]/1d3
                  qspin[*, sub_badq_mafr[i_lz_mafr]] = 0b
                  sunt[*, sub_badq_mafr[i_lz_mafr]]  = spha_recs.epoch.value[sub_qspha[sub_min_d_epoch]] - $
                     spha_recs.spin_phase.value[sub_qspha[sub_min_d_epoch]]/$
                     (spha_recs.avg_spin_rate.value[sub_qspha[sub_min_d_epoch]]/1d3)
                  qsunt[*, sub_badq_mafr[i_lz_mafr]] = 0b
               endif
            endfor
         endif
      endif
      ;--- make spin, qspin, sunt, qsunt from either hk (if available) or spha
      
      ;- truncate lz_recs to records with available spin and sunt
      sub_qmafr_lz = where((product(qspin, 1) eq 0) and (product(qsunt, 1) eq 0), n_qmafr_lz)
         if (n_qmafr_lz eq 0) then continue
      spin      = spin[*,sub_qmafr_lz]
      qspin     = qspin[*,sub_qmafr_lz]
      sunt      = sunt[*,sub_qmafr_lz]
      qsunt     = qsunt[*,sub_qmafr_lz]
      lz_recs   = lz_recs[sub_qmafr_lz]
      n_mafr_lz = n_qmafr_lz

      ;- read status word from lz_recs
      mfi_status_word12, lz_recs=lz_recs, sw12=sw12, qsw12=qsw12

      ;- eliminate fake range change (change that lasted lt 3 cycles)
      sw12_range   = sw12.range[outer_mag]
      n_cycle      = n_elements(sw12_range)
      d_sw12_range = sw12_range[1:n_cycle-1] - sw12_range[0:n_cycle-2]
      sub_sw12_range_change = where(d_sw12_range ne 0, n_sw12_range_change)
      if n_sw12_range_change gt 0 then begin
         if sub_sw12_range_change[0]                     eq 0           then qsw12[0] = 1
         if sub_sw12_range_change[n_sw12_range_change-1] eq (n_cycle-2) then qsw12[n_cycle-1] = 1
      endif
      if n_sw12_range_change gt 1 then begin
         d_sub     = sub_sw12_range_change[1:n_sw12_range_change-1] - sub_sw12_range_change[0:n_sw12_range_change-2]
         sub_d_sub = where(d_sub lt 3, n_d_sub)
         for i_temp=0, n_d_sub-1 do $
            qsw12[sub_sw12_range_change[sub_d_sub[i_temp]]+1:sub_sw12_range_change[sub_d_sub[i_temp]+1]] = 1
      endif
      
      ;- take last good sw12 for each major frame 
      sw12_mafr  =  sw12[0,*]
      for i=0, 4 do begin
         sub_temp = where(qsw12[i,*] eq 0, n_temp)
         if (n_temp ne 0) then sw12_mafr[sub_temp] = sw12[i,sub_temp]
      endfor
      qsw12_mafr = reform(product(qsw12, 1) ne 0, 1, n_mafr_lz)

      ;- average spin for each major frame
      spin_mafr  = spin[0,*]
      quality    = (qspin eq 0)
      sub_temp   = where(total(quality, 1) ne 0, n_temp)
      if (n_temp ne 0) then $
         spin_mafr[sub_temp] = total(spin[*,sub_temp]*quality[*,sub_temp], 1)/total(quality[*,sub_temp], 1)
      qspin_mafr = reform(product(qspin, 1), 1, n_mafr_lz)

      ;--- decompose and process mafrs with epoch inside [int_start_epoch, int_end_epoch]
      sub_mafr = where((lz_recs.epoch ge int_start_epoch) and (lz_recs.epoch le int_end_epoch),  n_mafr)
      ;- for each major frame
      for i_sub_mafr=0, n_mafr-1 do begin
         ;- subscript in lz_recs
         i_mafr = sub_mafr[i_sub_mafr]

         ;- check sw12 and spin for availability; check that operational mode is science
         if (qsw12_mafr[i_mafr] ne 0) or (qspin_mafr[i_mafr] ne 0) then continue
         if (lz_recs[i_mafr].tel_mode ne 1) and (lz_recs[i_mafr].tel_mode ne 5) then continue

         ;- decompose major frame
         case sw12_mafr[i_mafr].mode of
            0: mfi_decom_science_0,  lz_rec=lz_recs[i_mafr], cnts_pr=cnts_pr, cnts_sc=cnts_sc
            1: mfi_decom_science_12, lz_rec=lz_recs[i_mafr], cnts_pr=cnts_pr, cnts_sc=cnts_sc
            2: mfi_decom_science_12, lz_rec=lz_recs[i_mafr], cnts_pr=cnts_pr, cnts_sc=cnts_sc
         endcase

         ;- outboard magnetometer primary or secondary and find which mag (pr or sc) to process
         out_pr = (sw12_mafr[i_mafr].swap eq 0)
         out_sc = (sw12_mafr[i_mafr].swap ne 0)
         if keyword_set(outer_mag) then begin
            if out_pr then process_pr=1 else process_pr=0
         endif else begin
            if out_sc then process_pr=1 else process_pr=0
         endelse
         process_sc = (process_pr eq 0)

         ;--- process primary magnetometer counts
         if process_pr then begin
            n_cnts_pr = n_elements(cnts_pr)

            ;- epoch of each vector, first find epoch of first vector in mafr, then add dt
            epoch_pr  = lz_recs[i_mafr].epoch + $
                        TA[sw12_mafr[i_mafr].mode, sw12_mafr[i_mafr].tel_rate, 0] + $
                        TF[sw12_mafr[i_mafr].mode, sw12_mafr[i_mafr].tel_rate, 0]
            dt_pr     = double(TIMEMF - TIMEMF/2d*sw12_mafr[i_mafr].tel_rate)/n_cnts_pr
            epoch_pr += dt_pr*dindgen(n_cnts_pr)

            ;- sun time and quality
            sub_temp  = indgen(n_cnts_pr)/(n_cnts_pr/50)
            sunt_pr   =  sunt[sub_temp, i_mafr]
            qsunt_pr  = qsunt[sub_temp, i_mafr]

            ;- status word and quality, necessary since range can change during mafr
            sub_temp  = indgen(n_cnts_pr)/(n_cnts_pr/5)
            sw12_pr   =  sw12[sub_temp, i_mafr]
            qsw12_pr  = qsw12[sub_temp, i_mafr]

            ;- sun phase (which is also equal to average) and quality
            omega_pr  = (epoch_pr - sunt_pr)*spin_mafr[i_mafr]
            sub_temp  = where(omega_pr ge 2*!dpi, n_temp)
            if (n_temp ne 0) then omega_pr[sub_temp] = omega_pr[sub_temp] - 2*!dpi
            sub_temp  = where(omega_pr lt 0, n_temp)
            if (n_temp ne 0) then omega_pr[sub_temp] = omega_pr[sub_temp] + 2*!dpi
            qomega_pr = (sw12_pr.range[out_pr] ne range) or $
                        (qsw12_pr ne 0) or (qsunt_pr ne 0) or (cnts_pr.q ne 0) or $
                        (cnts_pr.x le 2) or (cnts_pr.y le 2) or (cnts_pr.z le 2) or $
                        (cnts_pr.x ge 4093) or (cnts_pr.y ge 4093) or (cnts_pr.z ge 4093)

            ;--- split mafr into N_SPINFIT_MAFR_PR intervals and run spinfit on each
            n_in_spinfit_pr = floor(double(n_cnts_pr)/N_SPINFIT_MAFR_PR)
            sub_set = indgen(n_in_spinfit_pr)

            ;- for each complete interval, with n_in_spinfit_pr points
            for i=0, N_SPINFIT_MAFR_PR-1 do begin
               ;- check quality
               sub_set1 = where((qomega_pr[sub_set] eq 0), n_set1)

               ;- process if enough points
               if (n_set1 ge MIN_N_IN_SPINFIT) then begin
                  ;- epoch of first and last good points of interval
                  first_epoch_pr = epoch_pr[sub_set[sub_set1[0]]]
                  last_epoch_pr  = epoch_pr[sub_set[sub_set1[n_set1-1]]]

                  ;- spinfit itself
                  sin_omega_pr = sin(omega_pr[sub_set[sub_set1]])
                  cos_omega_pr = cos(omega_pr[sub_set[sub_set1]])
                  mfi_spinfit_reduced, data=cnts_pr[sub_set[sub_set1]].x, $
                               sin_omega=sin_omega_pr, cos_omega=cos_omega_pr, $
                               coef=coefx_pr, qcoef=qcoefx_pr, alpha=1d, beta=0.4d
                  mfi_spinfit_reduced, data=cnts_pr[sub_set[sub_set1]].y, $
                               sin_omega=sin_omega_pr, cos_omega=cos_omega_pr, $
                               coef=coefy_pr, qcoef=qcoefy_pr, alpha=1d, beta=0.4d
                  mfi_spinfit_reduced, data=cnts_pr[sub_set[sub_set1]].z, $
                               sin_omega=sin_omega_pr, cos_omega=cos_omega_pr, $
                               coef=coefz_pr, qcoef=qcoefz_pr, alpha=1d, beta=0.4d

                  ;- store coefficients
                  if out_pr then begin
                     spinfit_ou[int_coef_ou_sub].first_epoch   = first_epoch_pr
                     spinfit_ou[int_coef_ou_sub].last_epoch    = last_epoch_pr
                     spinfit_ou[int_coef_ou_sub].range         = range
                     spinfit_ou[int_coef_ou_sub].coefx         = coefx_pr
                     spinfit_ou[int_coef_ou_sub].coefy         = coefy_pr
                     spinfit_ou[int_coef_ou_sub].coefz         = coefz_pr
                     qcoefxyz_ou[int_coef_ou_sub] = (qcoefx_pr + qcoefy_pr + qcoefz_pr)
                     int_coef_ou_sub += 1L
                  endif else begin
                     spinfit_in[int_coef_in_sub].first_epoch   = first_epoch_pr
                     spinfit_in[int_coef_in_sub].last_epoch    = last_epoch_pr
                     spinfit_in[int_coef_in_sub].range         = range
                     spinfit_in[int_coef_in_sub].coefx         = coefx_pr
                     spinfit_in[int_coef_in_sub].coefy         = coefy_pr
                     spinfit_in[int_coef_in_sub].coefz         = coefz_pr
                     qcoefxyz_in[int_coef_in_sub] = (qcoefx_pr + qcoefy_pr + qcoefz_pr)
                     int_coef_in_sub += 1L
                  endelse
               endif

               ;- subscript of start of next interval
               sub_set += n_in_spinfit_pr
            endfor
            ;--- end: split mafr into N_SPINFIT_MAFR_PR intervals and run spinfit on each
         endif
         ;--- end: process primary magnetometer counts

         ;--- process secondary magnetometer counts
         if process_sc then begin
            n_cnts_sc = n_elements(cnts_sc)

            ;- epoch of each vector, first find epoch of first vector in mafr, then add dt
            epoch_sc = lz_recs[i_mafr].epoch + $
               TA[sw12_mafr[i_mafr].mode, sw12_mafr[i_mafr].tel_rate, 1] + $
               TF[sw12_mafr[i_mafr].mode, sw12_mafr[i_mafr].tel_rate, 1]
            dt_sc     = double(TIMEMF - TIMEMF/2*sw12_mafr[i_mafr].tel_rate)/n_cnts_sc
            epoch_sc += dt_sc*dindgen(n_cnts_sc)

            ;- sun time and quality
            sub_temp  = indgen(n_cnts_sc)/(n_cnts_sc/50)
            sunt_sc   =  sunt[sub_temp, i_mafr]
            qsunt_sc  = qsunt[sub_temp, i_mafr]

            ;- status word and quality, necessary since range can change during mafr
            sub_temp  = indgen(n_cnts_sc)/(n_cnts_sc/5)
            sw12_sc   =  sw12[sub_temp, i_mafr]
            qsw12_sc  = qsw12[sub_temp, i_mafr]

            ;- sun phase (which is also equal to average) and quality
            omega_sc  = (epoch_sc - sunt_sc)*spin_mafr[i_mafr]
            sub_temp  = where(omega_sc ge 2*!dpi, n_temp)
            if (n_temp ne 0) then omega_sc[sub_temp] = omega_sc[sub_temp] - 2*!dpi
            sub_temp  = where(omega_sc lt 0, n_temp)
            if (n_temp ne 0) then omega_sc[sub_temp] = omega_sc[sub_temp] + 2*!dpi
            qomega_sc = (sw12_sc.range[out_sc] ne range) or $
                        (qsw12_sc ne 0) or (qsunt_sc ne 0) or (cnts_sc.q ne 0) or $
                        (cnts_sc.x le 2) or (cnts_sc.y le 2) or (cnts_sc.z le 2) or $
                        (cnts_sc.x ge 4093) or (cnts_sc.y ge 4093) or (cnts_sc.z ge 4093)

            ;--- split mafr into N_SPINFIT_MAFR_SC intervals and run spinfit on each
            n_in_spinfit_sc = floor(double(n_cnts_sc)/N_SPINFIT_MAFR_SC)
            sub_set = indgen(n_in_spinfit_sc)

            ;- for each complete interval, with n_in_spinfit_sc points
            for i=0, N_SPINFIT_MAFR_SC-1 do begin
               ;- check quality
               sub_set1 = where((qomega_sc[sub_set] eq 0), n_set1)

               ;- process if enough points
               if (n_set1 ge MIN_N_IN_SPINFIT) then begin
                  ;- epoch of first and last good points of interval
                  first_epoch_sc = epoch_sc[sub_set[sub_set1[0]]]
                  last_epoch_sc  = epoch_sc[sub_set[sub_set1[n_set1-1]]]

                  ;- spinfit itself
                  sin_omega_sc = sin(omega_sc[sub_set[sub_set1]])
                  cos_omega_sc = cos(omega_sc[sub_set[sub_set1]])
                  mfi_spinfit_reduced, data=cnts_sc[sub_set[sub_set1]].x, $
                               sin_omega=sin_omega_sc, cos_omega=cos_omega_sc, $
                               coef=coefx_sc, qcoef=qcoefx_sc, alpha=1d, beta=0.4d
                  mfi_spinfit_reduced, data=cnts_sc[sub_set[sub_set1]].y, $
                               sin_omega=sin_omega_sc, cos_omega=cos_omega_sc, $
                               coef=coefy_sc, qcoef=qcoefy_sc, alpha=1d, beta=0.4d
                  mfi_spinfit_reduced, data=cnts_sc[sub_set[sub_set1]].z, $
                               sin_omega=sin_omega_sc, cos_omega=cos_omega_sc, $
                               coef=coefz_sc, qcoef=qcoefz_sc, alpha=1d, beta=0.4d

                  ;- store coefficients
                  if out_sc then begin
                     spinfit_ou[int_coef_ou_sub].first_epoch   = first_epoch_sc
                     spinfit_ou[int_coef_ou_sub].last_epoch    = last_epoch_sc
                     spinfit_ou[int_coef_ou_sub].range         = range
                     spinfit_ou[int_coef_ou_sub].coefx         = coefx_sc
                     spinfit_ou[int_coef_ou_sub].coefy         = coefy_sc
                     spinfit_ou[int_coef_ou_sub].coefz         = coefz_sc
                     qcoefxyz_ou[int_coef_ou_sub] = (qcoefx_sc + qcoefy_sc + qcoefz_sc)
                     int_coef_ou_sub += 1L
                  endif else begin
                     spinfit_in[int_coef_in_sub].first_epoch   = first_epoch_sc
                     spinfit_in[int_coef_in_sub].last_epoch    = last_epoch_sc
                     spinfit_in[int_coef_in_sub].range         = range
                     spinfit_in[int_coef_in_sub].coefx         = coefx_sc
                     spinfit_in[int_coef_in_sub].coefy         = coefy_sc
                     spinfit_in[int_coef_in_sub].coefz         = coefz_sc
                     qcoefxyz_in[int_coef_in_sub] = (qcoefx_sc + qcoefy_sc + qcoefz_sc)
                     int_coef_in_sub += 1L
                  endelse
               endif

               ;- subscript of start of next interval
               sub_set += n_in_spinfit_sc
            endfor
            ;--- end: split mafr into N_SPINFIT_MAFR_SC intervals and run spinfit on each
         endif
         ;--- end: process secondary magnetometer counts
      endfor
      ;--- end: decompose and process mafrs with epoch inside ...
   endfor
   ;--- end: read lz and hk files covered by interval

   ;- save spinfit records with good quality
   if keyword_set(outer_mag) then begin
      sub_ok_ou = where(qcoefxyz_ou eq 0, n_ok_ou)
      if (n_ok_ou gt 0) then begin
         spinfit_ou = spinfit_ou[sub_ok_ou]
         filename = spinfit_path + spinfit_prefix_ou + string(range, format='(I1)') + mfi_prefix + $
                     string(int_start_year, format='(I4)') + string(int_start_month, format='(I02)') + $
                     string(int_start_day, format='(I02)') + '_v01.sav'
         save, spinfit_ou, filename=filename
      endif
   endif else begin
      sub_ok_in = where(qcoefxyz_in eq 0, n_ok_in)
      if (n_ok_in gt 0) then begin
         spinfit_in = spinfit_in[sub_ok_in]
         filename = spinfit_path + spinfit_prefix_in + string(range, format='(I1)') + mfi_prefix + $
                    string(int_start_year, format='(I4)') + string(int_start_month, format='(I02)') + $
                    string(int_start_day, format='(I02)') + '_v01.sav'
         save, spinfit_in, filename=filename
      endif
   endelse

   ;- define start and end epoch of next interval
   int_start_epoch = int_end_epoch + 1d
   int_end_epoch   = int_start_epoch + int_ms - 1
endwhile
;--- end: process intervals while int_end_epoch le end_epoch

END