PRO MFI_PRODUCT, START_DATE=START_DATE, END_DATE=END_DATE, VERSION=VERSION, HIRES=HIGHRES, CNT=COUNTS, PAY=PAYLOAD, $
                 NO_OUTLIER_REMOVAL=NO_OUTLIER_REMOVAL, NO_NOISE_CORRECTION=NO_NOISE_CORRECTION 

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_date) eq 1) then start_date = long(start_date)  else message, 'no start_date'
if (n_elements(end_date)   eq 1) then end_date   = long(end_date)    else end_date = start_date
if (n_elements(version)    eq 1) then version    = long(version)     else version  = 5
if keyword_set(highres)  then highres = 1  else  highres = 0
if keyword_set(counts)   then counts  = 1  else  counts  = 0
if keyword_set(payload)  then payload = 1  else  payload = 0
if keyword_set(no_outlier_removal)   then no_outlier_removal  = 1  else  no_outlier_removal  = 0
if keyword_set(no_noise_correction)  then no_noise_correction = 1  else  no_noise_correction = 0

;- convert start_date and end_date+MSDAY-1 to start_epoch and end_epoch
cdf_epoch, start_epoch, floor(start_date/1d4), floor((start_date/1d2) mod 1d2), (start_date mod 1d2), /compute_epoch
cdf_epoch, end_epoch,   floor(end_date/1d4),   floor((end_date/1d2) mod 1d2), (end_date mod 1d2), $
                        0, 0, 0, MSDAY-1L, /compute_epoch

;- define start epoch and end epoch of first interval
int_start_epoch = start_epoch
int_end_epoch   = int_start_epoch + MSDAY - 1L

;---- read event files -----------------------------------------------------
;- read spha_list file
spha_list_files = file_search(spha_list_path, spha_list_prefix + '*.dat', count=n_all_files)
spha_list_file  = (spha_list_files[sort(spha_list_files)])[n_all_files-1]
spha_rec  = {spha_record, date:0L}
spha_list = replicate({spha_record}, 1d4)
sub_rec   = 0
;- read
openr, lun, spha_list_file, /get_lun
while eof(lun) eq 0 do begin
   readf, lun, spha_rec
   spha_list[sub_rec].date = spha_rec.date
   sub_rec +=1
endwhile
free_lun, lun
if sub_rec gt 0 then spha_list = spha_list[0:sub_rec-1]
n_spha_list = sub_rec

;- records and format used in data_gap_list and outer_mag_list
temp   = {record, year_start:0L, month_start:0L, day_start:0L, hour_start:0L, min_start:0L, sec_start:0L, $
          year_end:0L, month_end:0L, day_end:0L, hour_end:0L, min_end:0L, sec_end:0L}
format = '(I4, I2, I2, X, I2, I2, I2, 3X, I4, I2, I2, X, I2, I2, I2)'
temp   = {record_epoch, epoch_start:0d, epoch_end:0d}

;- read data_gap_list file
data_gap_list_files = file_search(data_gap_list_path, data_gap_list_prefix + '*.dat', count=n_all_files)
data_gap_list_file  = (data_gap_list_files[sort(data_gap_list_files)])[n_all_files-1]
data_gap_rec        = {record}
data_gap_epoch_list = replicate({record_epoch}, 1d4)
sub_rec = 0
;read
openr, lun, data_gap_list_file, /get_lun
while eof(lun) eq 0 do begin
   readf, lun, data_gap_rec, format=format
   cdf_epoch, data_gap_epoch_start, data_gap_rec.year_start, data_gap_rec.month_start, data_gap_rec.day_start, $
                                    data_gap_rec.hour_start, data_gap_rec.min_start, data_gap_rec.sec_start, /c
   cdf_epoch, data_gap_epoch_end,   data_gap_rec.year_end, data_gap_rec.month_end, data_gap_rec.day_end, $
                                    data_gap_rec.hour_end, data_gap_rec.min_end, data_gap_rec.sec_end, /c
   data_gap_epoch_list[sub_rec].epoch_start = data_gap_epoch_start
   data_gap_epoch_list[sub_rec].epoch_end   = data_gap_epoch_end
   sub_rec +=1
endwhile
free_lun, lun
if sub_rec gt 0 then data_gap_epoch_list = data_gap_epoch_list[0:sub_rec-1]
n_data_gap_list = sub_rec

;- read outer_mag_list file
outer_mag_list_files = file_search(outer_mag_list_path, outer_mag_list_prefix + '*.dat', count=n_all_files)
outer_mag_list_file  = (outer_mag_list_files[sort(outer_mag_list_files)])[n_all_files-1]
outer_mag_rec        = {record}
outer_mag_epoch_list = replicate({record_epoch}, 1d4)
sub_rec = 0
;read
openr, lun, outer_mag_list_file, /get_lun
while eof(lun) eq 0 do begin
   readf, lun, outer_mag_rec, format=format
   cdf_epoch, outer_mag_epoch_start, outer_mag_rec.year_start, outer_mag_rec.month_start, outer_mag_rec.day_start, $
                                    outer_mag_rec.hour_start, outer_mag_rec.min_start, outer_mag_rec.sec_start, /c
   cdf_epoch, outer_mag_epoch_end,   outer_mag_rec.year_end, outer_mag_rec.month_end, outer_mag_rec.day_end, $
                                    outer_mag_rec.hour_end, outer_mag_rec.min_end, outer_mag_rec.sec_end, /c
   outer_mag_epoch_list[sub_rec].epoch_start = outer_mag_epoch_start
   outer_mag_epoch_list[sub_rec].epoch_end   = outer_mag_epoch_end
   sub_rec +=1
endwhile
free_lun, lun
if sub_rec gt 0 then outer_mag_epoch_list = outer_mag_epoch_list[0:sub_rec-1]
n_outer_mag_list = sub_rec
;---- end: read event files ------------------------------------------------

;--- process intervals while int_start_epoch lt end_epoch ------------------
while (int_start_epoch lt end_epoch) do begin
   ;- decompose int_start_epoch
   cdf_epoch, int_start_epoch, int_start_year, int_start_month, int_start_day, $
              int_start_hour, int_start_minut, int_start_sec, int_start_milli, /breakdown_epoch
   int_start_date = long(int_start_year*1d4 + int_start_month*1d2 + int_start_day)
   
   ;- check current date for presence in spha_list
   sub_spha_only = where(spha_list.date eq int_start_date, n_spha_only)
   if n_spha_only gt 0 then spha_only=1 else spha_only=0
   
   ;- check current date for presence in outer_mag_list
   outer_mag_epoch_cur_date_list = replicate({record_epoch}, 1d2)
   sub_rec = 0
   for i_t=0L, n_outer_mag_list-1L do begin
      if (int_start_epoch le outer_mag_epoch_list[i_t].epoch_start) and $
           (int_end_epoch ge outer_mag_epoch_list[i_t].epoch_end) then begin
         outer_mag_epoch_cur_date_list[sub_rec].epoch_start = outer_mag_epoch_list[i_t].epoch_start
         outer_mag_epoch_cur_date_list[sub_rec].epoch_end   = outer_mag_epoch_list[i_t].epoch_end
      endif else begin
         if (int_start_epoch gt outer_mag_epoch_list[i_t].epoch_start) and $
              (int_end_epoch lt outer_mag_epoch_list[i_t].epoch_end) then begin
            outer_mag_epoch_cur_date_list[sub_rec].epoch_start = int_start_epoch
            outer_mag_epoch_cur_date_list[sub_rec].epoch_end   = int_end_epoch
         endif else begin
            if (int_start_epoch le outer_mag_epoch_list[i_t].epoch_start) and $
                 (int_end_epoch gt outer_mag_epoch_list[i_t].epoch_start) and $
                 (int_end_epoch lt outer_mag_epoch_list[i_t].epoch_end) then begin
               outer_mag_epoch_cur_date_list[sub_rec].epoch_start = outer_mag_epoch_list[i_t].epoch_start
               outer_mag_epoch_cur_date_list[sub_rec].epoch_end   = int_end_epoch
            endif else begin
               if (int_start_epoch   gt outer_mag_epoch_list[i_t].epoch_start) and $
                    (int_start_epoch lt outer_mag_epoch_list[i_t].epoch_end) and $
                    (int_end_epoch   gt outer_mag_epoch_list[i_t].epoch_end) then begin
                  outer_mag_epoch_cur_date_list[sub_rec].epoch_start = int_start_epoch
                  outer_mag_epoch_cur_date_list[sub_rec].epoch_end   = outer_mag_epoch_list[i_t].epoch_end
               endif else begin
                  continue
               endelse
            endelse
         endelse
      endelse
      
      sub_rec += 1
   endfor
   if sub_rec gt 0 then outer_mag_epoch_cur_date_list = outer_mag_epoch_cur_date_list[0:sub_rec-1]
   n_outer_mag_cur_date_list = sub_rec

   ;- check current date for presence in data_gap_list
   data_gap_epoch_cur_date_list = replicate({record_epoch}, 1d2)
   sub_rec = 0
   for i_t=0L, n_data_gap_list-1L do begin
      if (int_start_epoch le data_gap_epoch_list[i_t].epoch_start) and $
           (int_end_epoch ge data_gap_epoch_list[i_t].epoch_end) then begin
         data_gap_epoch_cur_date_list[sub_rec].epoch_start = data_gap_epoch_list[i_t].epoch_start
         data_gap_epoch_cur_date_list[sub_rec].epoch_end   = data_gap_epoch_list[i_t].epoch_end
      endif else begin
         if (int_start_epoch gt data_gap_epoch_list[i_t].epoch_start) and $
              (int_end_epoch lt data_gap_epoch_list[i_t].epoch_end) then begin
            data_gap_epoch_cur_date_list[sub_rec].epoch_start = int_start_epoch
            data_gap_epoch_cur_date_list[sub_rec].epoch_end   = int_end_epoch
         endif else begin 
            if (int_start_epoch le data_gap_epoch_list[i_t].epoch_start) and $
                 (int_end_epoch gt data_gap_epoch_list[i_t].epoch_start) and $
                 (int_end_epoch lt data_gap_epoch_list[i_t].epoch_end) then begin
               data_gap_epoch_cur_date_list[sub_rec].epoch_start = data_gap_epoch_list[i_t].epoch_start
               data_gap_epoch_cur_date_list[sub_rec].epoch_end   = int_end_epoch
            endif else begin
               if (int_start_epoch   gt data_gap_epoch_list[i_t].epoch_start) and $
                    (int_start_epoch lt data_gap_epoch_list[i_t].epoch_end) and $
                    (int_end_epoch   gt data_gap_epoch_list[i_t].epoch_end) then begin
                  data_gap_epoch_cur_date_list[sub_rec].epoch_start = int_start_epoch
                  data_gap_epoch_cur_date_list[sub_rec].epoch_end   = data_gap_epoch_list[i_t].epoch_end
               endif else begin
                  continue
               endelse
            endelse
         endelse
      endelse
      
      sub_rec += 1
   endfor
   if sub_rec gt 0 then data_gap_epoch_cur_date_list = data_gap_epoch_cur_date_list[0:sub_rec-1]
   n_data_gap_cur_date_list = sub_rec

   ;- run mfi_highres for inner mag
   result_in = mfi_highres(date=int_start_date, outer_mag=0, spha_only=spha_only, cnt=counts, pay=payload, $
                           no_outlier_removal=no_outlier_removal, no_noise_correction=no_noise_correction, $
                           epoch_hr=epoch_hr_in, bgse_hr=bgse_hr_in, bgsm_hr=bgsm_hr_in, calib_hr=calib_hr_in, $
                           sw12_hr=sw12_hr_in, at_gse_hr=at_gse_hr_in, at_gsm_hr=at_gsm_hr_in, $
                           lz_files=lz_files_in, hk_files=hk_files_in, at_files=at_files_in, or_files=or_files_in)
   if result_in eq 0 then begin
      n_hr_in   = n_elements(epoch_hr_in)
      q_in_temp = replicate(0b, n_hr_in)
      for i_t=0L, n_outer_mag_cur_date_list-1L do begin
         sub_temp = where((epoch_hr_in ge outer_mag_epoch_cur_date_list[i_t].epoch_start) and $
                          (epoch_hr_in le outer_mag_epoch_cur_date_list[i_t].epoch_end), n_temp)
         if n_temp gt 0 then q_in_temp[sub_temp] = 1b
      endfor
      for i_t=0L, n_data_gap_cur_date_list-1L do begin
         sub_temp = where((epoch_hr_in ge data_gap_epoch_cur_date_list[i_t].epoch_start) and $
                          (epoch_hr_in le data_gap_epoch_cur_date_list[i_t].epoch_end), n_temp)
         if n_temp gt 0 then q_in_temp[sub_temp] = 1b
      endfor
      sub_q_in = where(q_in_temp eq 0b, n_q_in)
      if n_q_in eq 0 then result_in = 1
   endif

   ;- run mfi_highres for outer mag if necessary
   result_ou = 1
   if n_outer_mag_cur_date_list gt 0 then begin
      result_ou = mfi_highres(date=int_start_date, outer_mag=1, spha_only=spha_only, cnt=counts, pay=payload, $
                              no_outlier_removal=no_outlier_removal, no_noise_correction=no_noise_correction, $
                              epoch_hr=epoch_hr_ou, bgse_hr=bgse_hr_ou, bgsm_hr=bgsm_hr_ou, calib_hr=calib_hr_ou, $
                              sw12_hr=sw12_hr_ou, at_gse_hr=at_gse_hr_ou, at_gsm_hr=at_gsm_hr_ou, $
                              lz_files=lz_files_ou, hk_files=hk_files_ou, at_files=at_files_ou, or_files=or_files_ou)
      if result_ou eq 0 then begin
         n_hr_ou   = n_elements(epoch_hr_ou)
         q_ou_temp = replicate(1b, n_hr_ou)
         for i_t=0L, n_outer_mag_cur_date_list-1L do begin
            sub_temp = where((epoch_hr_ou ge outer_mag_epoch_cur_date_list[i_t].epoch_start) and $
                             (epoch_hr_ou le outer_mag_epoch_cur_date_list[i_t].epoch_end), n_temp)
            if n_temp gt 0 then q_ou_temp[sub_temp] = 0b
         endfor
         for i_t=0L, n_data_gap_cur_date_list-1L do begin
            sub_temp = where((epoch_hr_ou ge data_gap_epoch_cur_date_list[i_t].epoch_start) and $
                             (epoch_hr_ou le data_gap_epoch_cur_date_list[i_t].epoch_end), n_temp)
            if n_temp gt 0 then q_ou_temp[sub_temp] = 1b
         endfor
         sub_q_ou = where(q_ou_temp eq 0b, n_q_ou)
         if n_q_ou eq 0 then result_ou = 1
      endif
   endif
   
   ;- combine inner and outer mag data
   if (result_in eq 0) and (result_ou ne 0) then begin
      epoch_hr    = epoch_hr_in[sub_q_in]
      bgse_hr     = bgse_hr_in[sub_q_in]
      bgsm_hr     = bgsm_hr_in[sub_q_in]
      calib_hr    = calib_hr_in[sub_q_in]
      sw12_hr     = sw12_hr_in[sub_q_in]
      at_gse_hr   = at_gse_hr_in[sub_q_in]
      at_gsm_hr   = at_gsm_hr_in[sub_q_in]
      outer_mag_hr= replicate(0, n_q_in)
      lz_files    = lz_files_in
      hk_files    = hk_files_in
      at_files    = at_files_in
      or_files    = or_files_in
   endif
   if (result_ou eq 0) and (result_in ne 0) then begin
      epoch_hr    = epoch_hr_ou[sub_q_ou]
      bgse_hr     = bgse_hr_ou[sub_q_ou]
      bgsm_hr     = bgsm_hr_ou[sub_q_ou]
      calib_hr    = calib_hr_ou[sub_q_ou]
      sw12_hr     = sw12_hr_ou[sub_q_ou]
      at_gse_hr   = at_gse_hr_ou[sub_q_ou]
      at_gsm_hr   = at_gsm_hr_ou[sub_q_ou]
      outer_mag_hr= replicate(1, n_q_ou)
      lz_files    = lz_files_ou
      hk_files    = hk_files_ou
      at_files    = at_files_ou
      or_files    = or_files_ou
   endif
   if (result_in eq 0) and (result_ou eq 0) then begin
      epoch_hr    = [epoch_hr_in[sub_q_in],  epoch_hr_ou[sub_q_ou]]
      bgse_hr     = [bgse_hr_in[sub_q_in],   bgse_hr_ou[sub_q_ou]]
      bgsm_hr     = [bgsm_hr_in[sub_q_in],   bgsm_hr_ou[sub_q_ou]]
      calib_hr    = [calib_hr_in[sub_q_in],  calib_hr_ou[sub_q_ou]]
      sw12_hr     = [sw12_hr_in[sub_q_in],   sw12_hr_ou[sub_q_ou]]
      at_gse_hr   = [at_gse_hr_in[sub_q_in], at_gse_hr_ou[sub_q_ou]]
      at_gsm_hr   = [at_gsm_hr_in[sub_q_in], at_gsm_hr_ou[sub_q_ou]]
      outer_mag_hr= [replicate(0, n_q_in), replicate(1, n_q_ou)]
      lz_files    = lz_files_in
      hk_files    = hk_files_in
      at_files    = at_files_in
      or_files    = or_files_in
      ;- sort by epoch_hr
      sort_epoch_hr = sort(epoch_hr, /L64)
      epoch_hr    = epoch_hr[sort_epoch_hr]
      bgse_hr     = bgse_hr[sort_epoch_hr]
      bgsm_hr     = bgsm_hr[sort_epoch_hr]
      calib_hr    = calib_hr[sort_epoch_hr]
      sw12_hr     = sw12_hr[sort_epoch_hr]
      at_gse_hr   = at_gse_hr[sort_epoch_hr]
      at_gsm_hr   = at_gsm_hr[sort_epoch_hr]
      outer_mag_hr= outer_mag_hr[sort_epoch_hr]
   endif
   if (result_in ne 0) and (result_ou ne 0) then begin
      ;- define start epoch of next interval and continue
      int_start_epoch += MSDAY
      int_end_epoch   += MSDAY
      continue
   endif

   
   ;- save to cdf
   if (result_in eq 0) or (result_ou eq 0) then begin
      if highres then begin
         mfi_write_h2_cdf, date=int_start_date, version=version, cnt=counts, pay=payload, epoch_hr=epoch_hr, $
                           bgse_hr=bgse_hr, bgsm_hr=bgsm_hr, calib_hr=calib_hr, sw12_hr=sw12_hr, $
                           outer_mag_hr=outer_mag_hr, lz_files=lz_files, hk_files=hk_files, $
                           at_files=at_files, or_files=or_files
      endif else begin
         mfi_write_h0_cdf, date=int_start_date, version=version, cnt=counts, pay=payload, epoch_hr=epoch_hr, $
                           bgse_hr=bgse_hr, bgsm_hr=bgsm_hr, calib_hr=calib_hr, sw12_hr=sw12_hr, $
                           outer_mag_hr=outer_mag_hr,  at_gse_hr=at_gse_hr, at_gsm_hr=at_gsm_hr, $
                           lz_files=lz_files, hk_files=hk_files, at_files=at_files, or_files=or_files
      endelse
   endif
   
   ;- define start epoch of next interval
   int_start_epoch += MSDAY
   int_end_epoch   += MSDAY
endwhile
;--- end: process intervals while int_start_epoch lt end_epoch -------------

END