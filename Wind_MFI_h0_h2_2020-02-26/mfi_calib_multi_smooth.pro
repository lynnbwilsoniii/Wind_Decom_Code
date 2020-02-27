PRO MFI_CALIB_MULTI_SMOOTH, START_DATE=START_DATE, END_DATE=END_DATE, RANGE=RANGE, OUTER_MAG=OUTER_MAG

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_date)  eq 1) then start_date = long(start_date) else message, 'no start_date'
if (n_elements(end_date)    eq 1) then end_date   = long(end_date)   else end_date = start_date
if (n_elements(range)       eq 1) then range      = byte(range)      else range    = 1b
if keyword_set(outer_mag) then begin
   outer_mag             = 1
   mcalib_prefix         = mcalib_prefix_ou
   mcalib_smooth_prefix  = mcalib_smooth_prefix_ou
endif else begin
   outer_mag             = 0
   mcalib_prefix         = mcalib_prefix_in
   mcalib_smooth_prefix  = mcalib_smooth_prefix_in
endelse

;--- find dates covered by [start_date, end_date] interval, also include one previous and one next date
;- epoch of start_date-1day
cdf_epoch, start_date_epoch, floor(start_date/1d4), floor((start_date/1d2) mod 1d2), $
                            (start_date mod 1d2), /compute_epoch
start_date_epoch_ = start_date_epoch - MSDAY
cdf_epoch, start_date_epoch_, start_date_year_, start_date_month_, start_date_day_, $
           start_date_hour_, start_date_minut_, start_date_sec_, start_date_milli_, $
           /breakdown_epoch
;- string of start_date-1day
start_date_ = string(start_date_year_,format='(I4)') + string(start_date_month_,format='(I02)') + $
              string(start_date_day_,format='(I02)')

;- make array of all dates
date_arr   = [start_date_]
epoch_arr  = [start_date_epoch_]
epoch_temp = start_date_epoch_
repeat begin
   epoch_temp += MSDAY
   cdf_epoch, epoch_temp, yr, mo, dy, hr, mn, sc, msc, /breakdown_epoch
   date_temp = string(yr,format='(I4)') + string(mo,format='(I02)') + string(dy,format='(I02)')
   date_arr  = [date_arr,  date_temp]
   epoch_arr = [epoch_arr, epoch_temp]
endrep until (date_temp gt end_date)
n_dates = n_elements(date_arr)
;--- end: find dates covered by [start_date, end_date] interval, also include one previous and one next date

;--- for each data from date_arr (excluding 1st and last)
for i_d=1, n_dates-2 do begin
   ;- read current date mcalib file (latest version), add mcalib to mcalib_smooth
   file_mask = mcalib_prefix + string(range,format='(I1)') + mfi_prefix + date_arr[i_d] + '_v01.sav'
   files = file_search(mcalib_path, file_mask, count=n_files)
      if (n_files eq 0) then continue
   restore, files[n_files-1]

   ;- make mcalib_smooth array to store calib constants
   mcalib_smooth = replicate({mcalib_struc}, (N_SPINFIT_MAFR_PR > N_SPINFIT_MAFR_SC)*3d3)
   mcalib_smooth_first_sub = 500L

   ;- add mcalib to mcalib_smooth
   n_mcalib = n_elements(mcalib)
   mcalib_smooth_last_sub  = mcalib_smooth_first_sub+n_mcalib-1L
   mcalib_smooth[mcalib_smooth_first_sub:mcalib_smooth_last_sub] = mcalib

   ;- read previous date mcalib file if exists, and add last 3 minutes to mcalib_smooth
   file_mask = mcalib_prefix + string(range,format='(I1)') + mfi_prefix + date_arr[i_d-1L] + '*.sav'
   files     = file_search(mcalib_path, file_mask, count=n_files)
   if n_files gt 0 then begin
      restore, files[n_files-1]
      ;- add last 3 minutes to mcalib_smooth
      sub_temp = where((mcalib.last_epoch - epoch_arr[i_d])/60d3 gt -3, n_temp)
      if (n_temp gt 0) then begin
         mcalib_smooth_first_sub = mcalib_smooth_first_sub-n_temp
         mcalib_smooth[mcalib_smooth_first_sub:mcalib_smooth_first_sub+n_temp-1L] = mcalib[sub_temp]
      endif
   endif
   
   ;- read next date mcalib file if exists, and add first 3 minutes to mcalib_smooth
   file_mask = mcalib_prefix + string(range,format='(I1)') + mfi_prefix + date_arr[i_d+1L] + '*.sav'
   files = file_search(mcalib_path, file_mask, count=n_files)
   if (n_files gt 0) then begin
      restore, files[n_files-1]
      ;- add first 3 minutes to mcalib_smooth
      sub_temp = where((mcalib.first_epoch - epoch_arr[i_d+1L])/60d3 lt 3, n_temp)
      if (n_temp gt 0) then begin
         mcalib_smooth_last_sub = mcalib_smooth_last_sub+n_temp
         mcalib_smooth[mcalib_smooth_last_sub-n_temp+1L:mcalib_smooth_last_sub] = mcalib[sub_temp]
      endif
   endif

   ;- truncate mcalib_smooth
   mcalib_smooth   = mcalib_smooth[mcalib_smooth_first_sub:mcalib_smooth_last_sub]
   n_mcalib_smooth = n_elements(mcalib_smooth)

   ;- find subscripts of good both zerox, zeroy, and sensxy and truncate mcalib_smooth
   mask_ok_zerox  = mcalib_smooth.coefx[3] lt 1d3
   mask_ok_zeroy  = mcalib_smooth.coefy[3] lt 1d3
   mask_ok_zfit   = mcalib_smooth.coefz[3] lt 1d3
   mask_ok_sensxy = $
         (mcalib_smooth.coefx[3]/sqrt(mcalib_smooth.coefx[1]^2d +mcalib_smooth.coefx[2]^2d) lt 0.20) and $
         (mcalib_smooth.coefy[3]/sqrt(mcalib_smooth.coefy[1]^2d +mcalib_smooth.coefy[2]^2d) lt 0.20)
   sub_temp = where(mask_ok_zerox and mask_ok_zeroy and mask_ok_zfit and mask_ok_sensxy, n_temp)
      if (n_temp lt 1) then continue ;- check that there is at least 1 point
   mcalib_smooth   = mcalib_smooth[sub_temp]
   n_mcalib_smooth = n_temp 

   ;- separate mcalib_smooth into intervals separated by more than 3 minutes
   if n_mcalib_smooth gt 1 then begin
      d_first_epoch = mcalib_smooth[1L:n_mcalib_smooth-1L].first_epoch - $
                      mcalib_smooth[0L:n_mcalib_smooth-2L].first_epoch
      sub_temp = where(d_first_epoch gt 3d*60d3, n_temp)
      if (n_temp gt 0) then begin
         sub_int_first = [0L, sub_temp+1L] 
         sub_int_last  = [sub_temp, n_mcalib_smooth-1L] 
         n_ints        = n_temp + 1
      endif else begin
         sub_int_first = 0L
         sub_int_last  = n_mcalib_smooth-1L
         n_ints        = 1
      endelse
   endif else begin
      sub_int_first = 0L
      sub_int_last  = n_mcalib_smooth-1L
      n_ints        = 1
   endelse

   ;- for each interval smooth calibration constants
   sub_mcalib_smooth = [0]
   for i_i=0, n_ints-1 do begin
      ;- save subscripts
      n_int_points = (sub_int_last[i_i] - sub_int_first[i_i] + 1L)
      sub_mcalib_smooth = [sub_mcalib_smooth, sub_int_first[i_i] + lindgen(n_int_points)]

      ;- smooth zerox, zeroy, sensx, sensy
      if n_int_points gt 5 then begin
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zerox = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zerox, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroy = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroy, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroz = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroz, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensx = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensx, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensy = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensy, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensz = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensz, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetax = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetax, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetay = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetay, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetaz = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetaz, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phix = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phix, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiy = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiy, 5, /edge_truncate, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiz = $
               smooth(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiz, 5, /edge_truncate, /nan)
      endif else begin
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zerox = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zerox, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroy = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroy, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroz = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].zeroz, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensx = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensx, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensy = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensy, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensz = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].sensz, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetax = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetax, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetay = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetay, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetaz = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].thetaz, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phix = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phix, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiy = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiy, /nan)
         mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiz = $
               mean(mcalib_smooth[sub_int_first[i_i]:sub_int_last[i_i]].phiz, /nan)
      endelse
   endfor

   ;- take only intervals with sufficient number of points
   n_sub_mcalib_smooth = n_elements(sub_mcalib_smooth)
      if n_sub_mcalib_smooth le 1 then continue
   mcalib_smooth = mcalib_smooth[sub_mcalib_smooth[1L:n_sub_mcalib_smooth-1L]]
   
   ;- truncate mcalib_smooth to current date
   sub_temp = where((mcalib_smooth.first_epoch ge epoch_arr[i_d]) and $
                    (mcalib_smooth.first_epoch lt epoch_arr[i_d+1L]), n_temp)
     if (n_temp eq 0) then continue
   mcalib_smooth = mcalib_smooth[sub_temp]

   ;- save to file
   save, mcalib_smooth, filename=mcalib_smooth_path + mcalib_smooth_prefix + $
      string(range,format='(I1)') + mfi_prefix + date_arr[i_d] + '_v01.sav'
endfor
;--- end: for each data from date_arr (excluding 1st and last)

END