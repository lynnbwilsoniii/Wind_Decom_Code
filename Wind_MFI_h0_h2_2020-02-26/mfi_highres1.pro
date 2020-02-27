FUNCTION MFI_HIGHRES1, DATE=DATE, OUTER_MAG=OUTER_MAG, SPHA_ONLY=SPHA_ONLY, CNT=COUNTS, PAY=PAYLOAD, $
                       NO_OUTLIER_REMOVAL=NO_OUTLIER_REMOVAL, NO_NOISE_CORRECTION=NO_NOISE_CORRECTION, $ 
                       EPOCH_HR=EPOCH_HR, BGSE_HR=BGSE_HR, BGSM_HR=BGSM_HR, CALIB_HR=CALIB_HR, $
                       SW12_HR=SW12_HR, AT_GSE_HR=AT_GSE_HR, AT_GSM_HR=AT_GSM_HR, $
                       LZ_FILES=LZ_FILES, HK_FILES=HK_FILES, AT_FILES=AT_FILES, OR_FILES=OR_FILES

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(date) eq 1)  then date=long(date) else message, 'no date'
if keyword_set(outer_mag)   then outer_mag=1     else outer_mag=0
if keyword_set(spha_only)   then spha_only=1     else spha_only=0
if keyword_set(counts)      then counts=1        else counts=0
if keyword_set(payload)     then payload=1       else payload=0
if keyword_set(no_outlier_removal)   then no_outlier_removal  = 1  else  no_outlier_removal  = 0
if keyword_set(no_noise_correction)  then no_noise_correction = 1  else  no_noise_correction = 0

;- convert date to epoch
cdf_epoch, date_epoch, floor(date/1d4), floor((date/1d2) mod 1d2), (date mod 1d2), /compute_epoch

;- define start and end epoch of the interval
int_start_epoch = date_epoch
int_end_epoch   = date_epoch + MSDAY - 1

;- breakdown int_start_epoch-MSDAY
int_start_epoch_ = int_start_epoch - MSDAY
cdf_epoch, int_start_epoch_, int_start_year_, int_start_month_, int_start_day_, $
           int_start_hour_, int_start_minut_, int_start_sec_, int_start_milli_, /breakdown_epoch
int_start_date_str_ = string(int_start_year_,format='(I4)') + string(int_start_month_,format='(I02)') + $
                      string(int_start_day_,format='(I02)')

;- find dates covered by interval, include prev date to account for last mafr and next due to negative TA+TF 
date_str_arr = [int_start_date_str_]
epoch_temp   = int_start_epoch_
repeat begin
   epoch_temp += MSDAY
   cdf_epoch, epoch_temp, yr, mo, dy, hr, mn, sc, msc, /breakdown_epoch
   date_str_temp = string(yr,format='(I4)') + string(mo,format='(I02)') + string(dy,format='(I02)')
   date_str_arr = [date_str_arr, date_str_temp]
endrep until (epoch_temp gt int_end_epoch)

;- define arrays to store cnts, range, omega, and epoch for the whole interval
cnts_arr     = replicate({x:0d,y:0d,z:0d,q:1b}, 2d6)
range_arr    = bytarr(2d6)
omega_arr    = dblarr(2d6)
epoch_arr    = dblarr(2d6)
sw12_arr     = replicate({sw12_struc}, 2d6)
sub_cnts_arr = 0L

;- defile arrays to store lz and hk file names
n_date_str_arr = n_elements(date_str_arr)
lz_files = strarr(n_date_str_arr)
hk_files = strarr(n_date_str_arr)

;--- read lz and hk files covered by interval
for i_lz=0, n_date_str_arr-1 do begin
   ;- read lz, hr, and spha files
   result_lz   = mfi_read_lz_hk(n_mafr=n_mafr_lz, data_recs=lz_recs, file_path=lz_path, $
                               file_prefix=lz_prefix+(date_str_arr[i_lz])[0], filename=lz_file)
   result_hk   = mfi_read_lz_hk(/hk, n_mafr=n_mafr_hk, data_recs=hk_recs, file_path=hk_path, $
                               file_prefix=hk_prefix+(date_str_arr[i_lz])[0], filename=hk_file)
   result_spha = mfi_read_spha(spha_recs=spha_recs, file_path=spha_path, $
                               file_prefix=spha_prefix+(date_str_arr[i_lz])[0])

   ;- check that both lz data and spin/sunt are available                            
   if ((result_lz ne 0) or ((result_hk ne 0) and (result_spha ne 0))) then continue
   
   ;- fill lz_files and hk_files
   lz_files[i_lz] = lz_file
   hk_files[i_lz] = hk_file

   ;- process last date_str_arr file only if it may contain data with epoch le int_end_epoch
   if (i_lz eq (n_date_str_arr-1)) and (lz_recs[0].epoch gt (int_end_epoch + 3d3)) then continue

   ;- read spin rate (rad/ms) and sun times from hk_recs
   if result_hk eq 0 then mfi_spin_sunt, hk_recs=hk_recs, spin=spin_hk, qspin=qspin_hk, sunt=sunt_hk, qsunt=qsunt_hk

   ;--- take spin, qspin, sunt, qsunt from either hk (if available) or spha
   spin  = dblarr(10, n_mafr_lz)
   qspin = bytarr(10, n_mafr_lz) + 1b
   sunt  = dblarr(50, n_mafr_lz)
   qsunt = bytarr(50, n_mafr_lz) + 1b

   ;- first fill arrays from hk; if spha_only eq 0
   if spha_only eq 0 then begin
      for i_lz_mafr=0, n_mafr_lz-1 do begin
         sub_lz_hk_mafr = (where(hk_recs.epoch eq lz_recs[i_lz_mafr].epoch, n_lz_hk_mafr))[0]
         if (n_lz_hk_mafr gt 0) then begin
            spin[*,i_lz_mafr]  = spin_hk[*,sub_lz_hk_mafr]
            qspin[*,i_lz_mafr] = qspin_hk[*,sub_lz_hk_mafr]
            sunt[*,i_lz_mafr]  = sunt_hk[*,sub_lz_hk_mafr]
            qsunt[*,i_lz_mafr] = qsunt_hk[*,sub_lz_hk_mafr]
         endif
      endfor
   endif

   ;- then fill arrays elements with bad quality from spha (if available)
   sub_badq_mafr = where((product(qspin, 1) ne 0) or (product(qsunt, 1) ne 0), n_badq_mafr)
   if (result_spha eq 0) and (n_badq_mafr gt 0) then begin
      sub_qspha = where((spha_recs.fault.value  eq 0) and (spha_recs.fault.q eq 0) and (spha_recs.epoch.q eq 0) and $
                        (spha_recs.spin_phase.q eq 0) and (spha_recs.avg_spin_rate.q eq 0), n_qspha)
      if (n_qspha gt 0) then begin
         for i_lz_mafr=0, n_badq_mafr-1 do begin
            d_epoch = (abs(spha_recs.epoch.value[sub_qspha] - lz_recs[sub_badq_mafr[i_lz_mafr]].epoch))
            min_d_epoch = min(d_epoch, sub_min_d_epoch)
            if (min_d_epoch lt 120d3) then begin
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
   ;--- end: take spin, qspin, sunt, qsunt from either hk (if available) or spha

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
      sub_d_sub =  where(d_sub lt 3, n_d_sub)
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
   sub_temp   = where(total(quality, 1, /double) ne 0, n_temp)
   if (n_temp ne 0) then spin_mafr[sub_temp] = total(spin[*,sub_temp]*quality[*,sub_temp], 1, /double)/$
                                               total(quality[*,sub_temp], 1, /double)
   qspin_mafr = reform(product(qspin, 1, /preserve_type), 1, n_mafr_lz)

   ;--- decompose and process mafrs with epoch inside [int_start_epoch-1.1d*TIMEMF, int_end_epoch+3d3] 
   sub_mafr = where((lz_recs.epoch ge (int_start_epoch - 1.1d*TIMEMF)) and $
                    (lz_recs.epoch le (int_end_epoch   + 3d3)), n_mafr)

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

      ;- process primary magnetometer counts, convert to nT
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

         ;- range
         range_pr  = sw12_pr.range[out_pr]

         ;- sun phase (which is also equal to average) and quality
         omega_pr  = (epoch_pr - sunt_pr)*spin_mafr[i_mafr]
         sub_temp  = where(omega_pr ge 2*!dpi, n_temp)
         if (n_temp ne 0) then omega_pr[sub_temp] = omega_pr[sub_temp] - 2*!dpi
         sub_temp  = where(omega_pr lt 0, n_temp)
         if (n_temp ne 0) then omega_pr[sub_temp] = omega_pr[sub_temp] + 2*!dpi
         qomega_pr = (qsw12_pr ne 0) or (qsunt_pr ne 0) or (cnts_pr.q ne 0) or $
                     (cnts_pr.x le 2) or (cnts_pr.y le 2) or (cnts_pr.z le 2) or $
                     (cnts_pr.x ge 4093) or (cnts_pr.y ge 4093) or (cnts_pr.z ge 4093)

         ;- save cnts, range, omega, and epoch
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L].x = cnts_pr.x
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L].y = cnts_pr.y
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L].z = cnts_pr.z
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L].q = qomega_pr
         range_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L]  = range_pr
         omega_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L]  = omega_pr
         epoch_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L]  = epoch_pr
         sw12_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_pr -1L]   = sw12_pr

         ;- increase subscript
         sub_cnts_arr += n_cnts_pr
      endif

      ;- process secondary magnetometer counts, convert to nT
      if process_sc then begin
         n_cnts_sc = n_elements(cnts_sc)

         ;- epoch of each vector, first find epoch of first vector in mafr, then add dt
         epoch_sc = lz_recs[i_mafr].epoch + $
            TA[sw12_mafr[i_mafr].mode, sw12_mafr[i_mafr].tel_rate, 1] + $
            TF[sw12_mafr[i_mafr].mode, sw12_mafr[i_mafr].tel_rate, 1]
         dt_sc     = double(TIMEMF - TIMEMF/2d*sw12_mafr[i_mafr].tel_rate)/n_cnts_sc
         epoch_sc += dt_sc*dindgen(n_cnts_sc)

         ;- sun time and quality
         sub_temp  = indgen(n_cnts_sc)/(n_cnts_sc/50)
         sunt_sc   =  sunt[sub_temp, i_mafr]
         qsunt_sc  = qsunt[sub_temp, i_mafr]

         ;- status word and quality, necessary since range can change during mafr
         sub_temp  = indgen(n_cnts_sc)/(n_cnts_sc/5)
         sw12_sc   =  sw12[sub_temp, i_mafr]
         qsw12_sc  = qsw12[sub_temp, i_mafr]

         ;- range
         range_sc  = sw12_sc.range[out_sc]

         ;- sun phase (which is also equal to average) and quality
         omega_sc  = (epoch_sc - sunt_sc)*spin_mafr[i_mafr]
         sub_temp  = where(omega_sc ge 2*!dpi, n_temp)
         if (n_temp ne 0) then omega_sc[sub_temp] = omega_sc[sub_temp] - 2*!dpi
         sub_temp  = where(omega_sc lt 0, n_temp)
         if (n_temp ne 0) then omega_sc[sub_temp] = omega_sc[sub_temp] + 2*!dpi
         qomega_sc = (qsw12_sc ne 0) or (qsunt_sc ne 0) or (cnts_sc.q ne 0) or $
                     (cnts_sc.x le 2) or (cnts_sc.y le 2) or (cnts_sc.z le 2) or $
                     (cnts_sc.x ge 4093) or (cnts_sc.y ge 4093) or (cnts_sc.z ge 4093)

         ;- save cnts, range, omega, and epoch
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L].x = cnts_sc.x
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L].y = cnts_sc.y
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L].z = cnts_sc.z
         cnts_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L].q = qomega_sc
         range_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L]  = range_sc
         omega_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L]  = omega_sc
         epoch_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L]  = epoch_sc
         sw12_arr[sub_cnts_arr:sub_cnts_arr + n_cnts_sc -1L]   = sw12_sc

         ;- increase subscript
         sub_cnts_arr += n_cnts_sc
      endif
   endfor
   ;--- end: decompose and process mafrs with epoch inside [int_start_epoch-1.1d*TIMEMF, int_end_epoch+3d3]...
endfor


;- take only quality data in the current interval epoch range
mask_x = (cnts_arr.x eq 0) or (cnts_arr.y eq 0) or (cnts_arr.z eq 0) or $
         (shift(cnts_arr.z,  1) eq    0) or (shift(cnts_arr.y,  1) eq 0) or $
         (cnts_arr.x eq 4095) or (cnts_arr.y eq 4095) or (cnts_arr.z eq 4095) or $
         (shift(cnts_arr.z,  1) eq 4095) or (shift(cnts_arr.y,  1) eq 4095)
mask_y = (cnts_arr.x eq 0) or (cnts_arr.y eq 0) or (cnts_arr.z eq 0) or $
         (shift(cnts_arr.z,  1) eq    0) or (shift(cnts_arr.x, -1) eq 0) or $
         (cnts_arr.x eq 4095) or (cnts_arr.y eq 4095) or (cnts_arr.z eq 4095) or $
         (shift(cnts_arr.z,  1) eq 4095) or (shift(cnts_arr.x, -1) eq 4095)
mask_z = (cnts_arr.x eq 0) or (cnts_arr.y eq 0) or (cnts_arr.z eq 0) or $
         (shift(cnts_arr.x, -1) eq    0) or (shift(cnts_arr.y, -1) eq 0) or $
         (cnts_arr.x eq 4095) or (cnts_arr.y eq 4095) or (cnts_arr.z eq 4095) or $
         (shift(cnts_arr.x, -1) eq 4095) or (shift(cnts_arr.y, -1) eq 4095)
sub_ok = where((cnts_arr.q eq 0) and (mask_x eq 0) and (mask_y eq 0) and (mask_z eq 0) and $
               (epoch_arr ge int_start_epoch) and (epoch_arr le int_end_epoch), n_ok)
if n_ok lt 10 then return, 1
cnts_arr  = cnts_arr[sub_ok]
range_arr = range_arr[sub_ok]
omega_arr = omega_arr[sub_ok]
epoch_arr = epoch_arr[sub_ok]
sw12_arr  = sw12_arr[sub_ok]
;--- end: read lz and hk files covered by interval

;--- find calibration constants for each data point
;- make mcalib and mcalib_flag
mcalib      = replicate({mcalib_struc}, n_ok)
mcalib_flag = replicate( 1b, n_ok)

;- find all ranges
all_ranges  = range_arr[uniq(range_arr, sort(range_arr))]
n_ranges    = n_elements(all_ranges)

;- for each range fill mcalib and mcalib_flag
for i_r=0, n_ranges-1 do begin
   ;- read calibration constants for all data points in current range
   sub_cur_range = where(range_arr eq all_ranges[i_r], n_cur_range)
      if (n_cur_range eq 0) then continue
   mfi_read_caliblist_multi, epoch=epoch_arr[sub_cur_range], range=all_ranges[i_r], outer_mag=outer_mag, $
                             mcalib=mcalib_cur, flag=mcalib_flag_cur
   ;- fill mcalib and  mcalib_flag
   mcalib[sub_cur_range]      = mcalib_cur
   mcalib_flag[sub_cur_range] = mcalib_flag_cur
endfor

stop
stop

;- take only data points and corresponding calib points for which flag ne 1
sub_ok = where(mcalib_flag ne 1, n_epoch)
   if n_epoch eq 0 then return, 1
mcalib     = mcalib[sub_ok]
cnts_arr   = cnts_arr[sub_ok]
omega_arr  = omega_arr[sub_ok]
epoch_arr  = epoch_arr[sub_ok]
sw12_arr   = sw12_arr[sub_ok]
range_arr  = range_arr[sub_ok]
;--- end: find calibration constants for each data point

;--- convert to rawb, then to apparent and payload (despun) coords
;- rewrite mcalib.sensx/y/z with b_fit values
mcalib.sensx = cal[outer_mag].sens_bfit[mcalib.range].x
mcalib.sensy = cal[outer_mag].sens_bfit[mcalib.range].y
mcalib.sensz = cal[outer_mag].sens_bfit[mcalib.range].z

;- make rawb_arr and convert to rawb
rawb_arr    = replicate({vect_struc}, n_epoch)
rawb_arr.x  = (cnts_arr.x - mcalib.zerox)*mcalib.sensx
rawb_arr.y  = (cnts_arr.y - mcalib.zeroy)*mcalib.sensy
rawb_arr.z  = (cnts_arr.z - mcalib.zeroz)*mcalib.sensz

;- convert rawb from magnetometer to apparent (spinning payload) coord
matrix00   =  cos(mcalib.thetax)*cos(mcalib.phix)
matrix01   =  cos(mcalib.thetax)*sin(mcalib.phix)
matrix02   =  sin(mcalib.thetax)
matrix10   = -cos(mcalib.thetay)*sin(mcalib.phiy)
matrix11   =  cos(mcalib.thetay)*cos(mcalib.phiy)
matrix12   =  sin(mcalib.thetay)
matrix20   =  sin(mcalib.thetaz)*cos(mcalib.phiz)
matrix21   =  sin(mcalib.thetaz)*sin(mcalib.phiz)
matrix22   =  cos(mcalib.thetaz)
det_denom  =   matrix00*(matrix11*matrix22-matrix12*matrix21) $
             - matrix10*(matrix01*matrix22-matrix02*matrix21) $
             + matrix20*(matrix01*matrix12-matrix02*matrix11)
det_numer1 =   rawb_arr.x*(matrix11*matrix22-matrix12*matrix21) $
             - rawb_arr.y*(matrix01*matrix22-matrix02*matrix21) $
             + rawb_arr.z*(matrix01*matrix12-matrix02*matrix11)
det_numer2 = - rawb_arr.x*(matrix10*matrix22-matrix12*matrix20) $
             + rawb_arr.y*(matrix00*matrix22-matrix02*matrix20) $
             - rawb_arr.z*(matrix00*matrix12-matrix02*matrix10)
det_numer3 =   rawb_arr.x*(matrix10*matrix21-matrix11*matrix20) $
             - rawb_arr.y*(matrix00*matrix21-matrix01*matrix20) $
             + rawb_arr.z*(matrix00*matrix11-matrix01*matrix10)
appb_arr   = rawb_arr
appb_arr.x = det_numer1/det_denom
appb_arr.y = det_numer2/det_denom
appb_arr.z = det_numer3/det_denom
appb_arr.z*=-1d

;- convert from apparent (spinning payload) to payload
payb_arr   = appb_arr
payb_arr.x = cos(omega_arr)*appb_arr.x - sin(omega_arr)*appb_arr.y
payb_arr.y = sin(omega_arr)*appb_arr.x + cos(omega_arr)*appb_arr.y
;--- end: convert to rawb, then to apparent and payload (despun) coords
stop

;--- find spin harmonics noise in xyz payb_arr, eliminate them, and plot spectra before and after
d_epoch_arr    = epoch_arr[1L:n_epoch-1L]-epoch_arr[0L:n_epoch-2L]
d_epoch_median = round(median(d_epoch_arr)/46d)*46d
n_harm         = 52/round(median(d_epoch_arr)/46d)
;if (mean(d_epoch_arr) gt 1.2d*median(d_epoch_arr)) and (d_epoch_median lt 184) then n_harm /= 2
prime_power    = [1d, 2d, 2d^2d, 2d^3d, 2d^4d, 2d^4d*3d^1d, 2d^4d*3d^2d, 2d^4d*3d^3d, 2d^4d*3d^4d, 2d^4d*3d^5d, $ 
                 2d^4d*3d^6d, 2d^7d*3d^6d, 2d^8d*3d^6d, 2d^10d*3d^5d, 2d^12d*3d^4d, 2d^14d*3d^3d, 2d^19d, $
                 2d^17d*5d^1d, 2d^18d*3d^1d, 2d^15d*3d^3d, 2d^17d*3d^2d, 2d^18d*3d^0d*5d^1d,2d^19d*3d^1d,2d^16d*3d^3d]

;- find spin harmonics noise
sub_e1    = lindgen(n_elements(epoch_arr))
;stop
;cdf_epoch, epoch_day, 2017, 05, 28, /c
;sub_e1    = where(((epoch_arr-epoch_day)/3600d3 gt 9) and ((epoch_arr-epoch_day)/3600d3 lt 12))
cosx_coef = dblarr(n_harm)
cosy_coef = dblarr(n_harm)
cosz_coef = dblarr(n_harm)
sinx_coef = dblarr(n_harm)
siny_coef = dblarr(n_harm)
sinz_coef = dblarr(n_harm)
for i_h=1, n_harm do begin
   cos_i_h_omega = cos(i_h*omega_arr[sub_e1])
   sin_i_h_omega = sin(i_h*omega_arr[sub_e1])
   total_cos_i_h_omega_sq = total(cos_i_h_omega^2d)
   total_sin_i_h_omega_sq = total(sin_i_h_omega^2d)
   cosx_coef[i_h-1] = total(payb_arr[sub_e1].x*cos_i_h_omega)/total_cos_i_h_omega_sq
   cosy_coef[i_h-1] = total(payb_arr[sub_e1].y*cos_i_h_omega)/total_cos_i_h_omega_sq
   cosz_coef[i_h-1] = total(payb_arr[sub_e1].z*cos_i_h_omega)/total_cos_i_h_omega_sq
   sinx_coef[i_h-1] = total(payb_arr[sub_e1].x*sin_i_h_omega)/total_sin_i_h_omega_sq
   siny_coef[i_h-1] = total(payb_arr[sub_e1].y*sin_i_h_omega)/total_sin_i_h_omega_sq
   sinz_coef[i_h-1] = total(payb_arr[sub_e1].z*sin_i_h_omega)/total_sin_i_h_omega_sq
endfor
;save,cosx_coef,cosy_coef,cosz_coef,sinx_coef,siny_coef,sinz_coef,filename='/mfi/data/_noise.sav'
;stop
;restore, '/mfi/data/_noise.sav'
noise_x = dblarr(n_epoch)
noise_y = dblarr(n_epoch)
noise_z = dblarr(n_epoch)
for i_h=1, n_harm do begin
   cos_i_h_omega = cos(i_h*omega_arr)
   sin_i_h_omega = sin(i_h*omega_arr)
   noise_x += (cosx_coef[i_h-1]*cos_i_h_omega + sinx_coef[i_h-1]*sin_i_h_omega)
   noise_y += (cosy_coef[i_h-1]*cos_i_h_omega + siny_coef[i_h-1]*sin_i_h_omega)
   noise_z += (cosz_coef[i_h-1]*cos_i_h_omega + sinz_coef[i_h-1]*sin_i_h_omega)
endfor

;- find epoch0 to interpolate to
epoch_temp = epoch_arr[0] + d_epoch_median*dindgen(n_epoch)
sub_temp   = where(epoch_temp le epoch_arr[n_epoch-1L], n_epoch0)
epoch0     = epoch_temp[sub_temp]
sub_temp   = where(n_epoch0 ge prime_power, n_ge)
n_epoch1   = prime_power[sub_temp[n_ge-1]]

;- compute spectra with noise
paybx0  = interpol(payb_arr.x, epoch_arr, epoch0)
payby0  = interpol(payb_arr.y, epoch_arr, epoch0)
paybz0  = interpol(payb_arr.z, epoch_arr, epoch0)
epoch1   = epoch0[0L:n_epoch1-1L]
paybx1   = paybx0[0L:n_epoch1-1L]
payby1   = payby0[0L:n_epoch1-1L]
paybz1   = paybz0[0L:n_epoch1-1L]
n_freq      = n_epoch1/2L + 1L
freq        = dindgen(n_freq)/(n_epoch1*d_epoch_median*1d-3)
fft_x_noise = (fft(paybx1, /double))[0:n_freq-1L]
fft_y_noise = (fft(payby1, /double))[0:n_freq-1L]
fft_z_noise = (fft(paybz1, /double))[0:n_freq-1L]

;- remove noise
if no_noise_correction eq 0 then begin
   payb_arr.x -= noise_x
   payb_arr.y -= noise_y
   payb_arr.z -= noise_z
endif

;--- remove outliers and compute spectra --------------------------------
;- x,y,z-component glitch masks (1 means glitch)
maskx = mfi_glitch_mask(data_arr=payb_arr.x, ratio=12)
masky = mfi_glitch_mask(data_arr=payb_arr.y, ratio=12)
maskz = mfi_glitch_mask(data_arr=payb_arr.z, ratio=12)
;- mask_range to eliminate two points at each range change (one point at each side)
n_mcalib   =  n_elements(mcalib)
d_range    = mcalib[1L:n_mcalib-1L].range - mcalib[0L:n_mcalib-2L].range
mask_range = ([d_range, 0] ne 0) or ([0, d_range] ne 0)
;- truncate mcalib, payb_arr, range, omega, and epoch
sub_not_glitch = where((temporary(maskx) or temporary(masky) or temporary(maskz) or $
                        temporary(mask_range)) eq 0, n_not_glitch)
                        
;- compute spectra without noise
paybx0  = interpol(payb_arr[sub_not_glitch].x, epoch_arr[sub_not_glitch], epoch0)
payby0  = interpol(payb_arr[sub_not_glitch].y, epoch_arr[sub_not_glitch], epoch0)
paybz0  = interpol(payb_arr[sub_not_glitch].z, epoch_arr[sub_not_glitch], epoch0)
epoch1   = epoch0[0L:n_epoch1-1L]
paybx1   = paybx0[0L:n_epoch1-1L]
payby1   = payby0[0L:n_epoch1-1L]
paybz1   = paybz0[0L:n_epoch1-1L]
n_freq  = n_epoch1/2L + 1L
freq    = dindgen(n_freq)/(n_epoch1*d_epoch_median*1d-3)
fft_x   = (fft(paybx1, /double))[0:n_freq-1L]
fft_y   = (fft(payby1, /double))[0:n_freq-1L]
fft_z   = (fft(paybz1, /double))[0:n_freq-1L]
;------------------------------------------------------------------------


;- prefixes
if keyword_set(outer_mag) then begin
   prefix_temp ='outer_sp_'
   title_temp  =', outer mag'
endif else begin
   prefix_temp ='inner_sp_'
   title_temp  =', inner mag'
endelse

;- plot spectra with and without noise
set_plot,  'Z'
device,    set_pixel_depth=24, set_resolution=[1200,900]
device,    decomposed=0
loadct,    39, /silent
!p.multi = [0,3,3,0,1]
plot, freq, n_epoch1*(d_epoch_median*1d-3)*abs(fft_x_noise)^2d, /xlog, /ylog, /xstyle, /ystyle, $
      xrange=[1d-1, 11], yrange=[1d-8,1d3], xtitle='f [Hz]', ytitle='Bx_pay_noise PSD [nT^2/Hz]', $
      title=string(date, format='(I8)')+title_temp
plot, freq, n_epoch1*(d_epoch_median*1d-3)*abs(fft_y_noise)^2d, /xlog, /ylog, /xstyle, /ystyle, $
      xrange=[1d-1, 11], yrange=[1d-8,1d3], xtitle='f [Hz]', ytitle='By_pay_noise PSD [nT^2/Hz]'
plot, freq, n_epoch1*(d_epoch_median*1d-3)*abs(fft_z_noise)^2d, /xlog, /ylog, /xstyle, /ystyle, $
      xrange=[1d-1, 11], yrange=[1d-8,1d3], xtitle='f [Hz]', ytitle='Bz_pay_noise PSD [nT^2/Hz]'
plot, freq, n_epoch1*(d_epoch_median*1d-3)*abs(fft_x)^2d, /xlog, /ylog, /xstyle, /ystyle, $
      xrange=[1d-1, 11], yrange=[1d-8,1d3], xtitle='f [Hz]', ytitle='Bx_pay PSD [nT^2/Hz]', $
      title='after outliers removal'
plot, freq, n_epoch1*(d_epoch_median*1d-3)*abs(fft_y)^2d, /xlog, /ylog, /xstyle, /ystyle, $
      xrange=[1d-1, 11], yrange=[1d-8,1d3], xtitle='f [Hz]', ytitle='By_pay PSD [nT^2/Hz]'
plot, freq, n_epoch1*(d_epoch_median*1d-3)*abs(fft_z)^2d, /xlog, /ylog, /xstyle, /ystyle, $
      xrange=[1d-1, 11], yrange=[1d-8,1d3], xtitle='f [Hz]', ytitle='Bz_pay PSD [nT^2/Hz]'
plot,  epoch_arr-epoch_arr[0], noise_x, xrange=[0, 15d3], ytitle='X noise [nT]'
oplot, epoch_arr-epoch_arr[0], 0.02d*cos(omega_arr), color=250
plot,  epoch_arr-epoch_arr[0], noise_y, xrange=[0, 15d3], ytitle='Y noise [nT]'
oplot, epoch_arr-epoch_arr[0], 0.02d*cos(omega_arr), color=250
plot,  epoch_arr-epoch_arr[0], noise_z, xrange=[0, 15d3], ytitle='Z noise [nT]'
oplot, epoch_arr-epoch_arr[0], 0.02d*cos(omega_arr), color=250
write_png, highres_spectra_path+prefix_temp+string(date, format='(I8)')+'.png', tvrd(true=1)
device, /close
!p.multi = 0
set_plot, 'X'
;--- end: find spin harmonics noise in xyz payb_arr, eliminate them, and plot spectra before and after
stop

;--- eliminate glitches and two points at each range change (one on each side) in payb_arr
;- mask_range to eliminate two points at each range change (one point at each side)
n_mcalib   =  n_elements(mcalib)
d_range    = mcalib[1L:n_mcalib-1L].range - mcalib[0L:n_mcalib-2L].range
mask_range = ([d_range, 0] ne 0) or ([0, d_range] ne 0)

;- truncate mcalib, payb_arr, range, omega, and epoch
if no_outlier_removal eq 0 then begin
   ;- x,y,z-component glitch masks (1 means glitch)
   maskx = mfi_glitch_mask(data_arr=payb_arr.x, ratio=12)
   masky = mfi_glitch_mask(data_arr=payb_arr.y, ratio=12)
   maskz = mfi_glitch_mask(data_arr=payb_arr.z, ratio=12)
   sub_not_glitch = where((maskx or masky or maskz or mask_range) eq 0, n_not_glitch)
endif else begin
   sub_not_glitch = where(mask_range eq 0, n_not_glitch)
endelse
   if n_not_glitch eq 0 then return, 1
epoch_hr = epoch_arr[sub_not_glitch]
bpay_hr  = replicate({vect_q_struc}, n_not_glitch)
   bpay_hr.x  = payb_arr[sub_not_glitch].x
   bpay_hr.y  = payb_arr[sub_not_glitch].y
   bpay_hr.z  = payb_arr[sub_not_glitch].z
   bpay_hr.q  = 0b
bapp_hr  = replicate({vect_q_struc}, n_not_glitch)
   bapp_hr.x  = appb_arr[sub_not_glitch].x
   bapp_hr.y  = appb_arr[sub_not_glitch].y
   bapp_hr.z  = appb_arr[sub_not_glitch].z
   bapp_hr.q  = 0b
bcnts_hr = replicate({vect_q_struc}, n_not_glitch)
   bcnts_hr.x = cnts_arr[sub_not_glitch].x
   bcnts_hr.y = cnts_arr[sub_not_glitch].y
   bcnts_hr.z = cnts_arr[sub_not_glitch].z
   bcnts_hr.q = 0b
bblank_hr= replicate({vect_q_struc}, n_not_glitch)
   bblank_hr.q= 1b
calib_hr = mcalib[sub_not_glitch]
sw12_hr  = sw12_arr[sub_not_glitch]
omega_hr = omega_arr[sub_not_glitch]
;---end: eliminate glitches and two points at each range change (one on each side) in payb_arr

;- convert from payload to gse and gsm coord
gse_gsm_res = mfi_pay_to_gse_gsm(epoch=epoch_hr, pay=bpay_hr, gse=bgse_hr, gsm=bgsm_hr, $
                                 at_gse=at_gse_hr, at_gsm=at_gsm_hr, at_files=at_files, or_files=or_files)
if gse_gsm_res ne 0 then return, 1

;- check counts and payload keywords, and replace bgsm_hr and bgse_hr
if counts eq 1 then begin
   bgsm_hr = bcnts_hr
   bgse_hr = bblank_hr
endif
if payload eq 1 then begin
   bgsm_hr = bapp_hr
   bgse_hr = bpay_hr
endif
stop

;- return 0
return, 0

END