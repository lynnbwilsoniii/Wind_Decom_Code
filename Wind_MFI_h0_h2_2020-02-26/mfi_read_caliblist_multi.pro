PRO MFI_READ_CALIBLIST_MULTI, EPOCH=EPOCH, RANGE=RANGE, OUTER_MAG=OUTER_MAG, MCALIB=MCALIB, FLAG=MCALIB_FLAG

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
n_epoch = n_elements(epoch)
if n_epoch eq 0 then message, 'no epoch' 
if n_elements(range) ne 1 then message, 'range must be scalar' 
if keyword_set(outer_mag) then begin
   outer_mag            = 1
   mcalib_smooth_prefix = mcalib_smooth_prefix_ou
endif else begin
   outer_mag            = 0
   mcalib_smooth_prefix = mcalib_smooth_prefix_in
endelse

;--- find dates covered by [start_date, end_date] interval, also include one previous and one next date
;- start_date and end_date (dates of first and last epoch)
cdf_epoch, epoch[0], start_year, start_month, start_day, /breakdown_epoch
cdf_epoch, epoch[n_epoch-1d], end_year, end_month, end_day, /breakdown_epoch
start_date = string(start_year, format='(I4)') + string(start_month, format='(I02)') + $
             string(start_day, format='(I02)')
end_date   = string(end_year, format='(I4)')  + string(end_month, format='(I02)') + $
             string(end_day, format='(I02)')

;- epoch of start_date-1day
cdf_epoch, start_date_epoch, floor(start_date/1d4), floor((start_date/1d2) mod 1d2), $
                             (start_date mod 1d2), /compute_epoch
start_date_epoch_ = start_date_epoch - MSDAY
cdf_epoch, start_date_epoch_, start_date_year_, start_date_month_, start_date_day_, $
           start_date_hour_, start_date_minut_, start_date_sec_, start_date_milli_, /breakdown_epoch

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

;--- restore current range mcalib_smooth for dates in date_arr, and save in mcalib_cur 
range_cur = range

;- make mcalib array to store calib constants
mcalib_cur = replicate({mcalib_struc}, (N_SPINFIT_MAFR_PR > N_SPINFIT_MAFR_SC)*3d3*n_dates)
mcalib_cur_first_sub =  0L

;- for each date from date_arr
for i_d=0, n_dates-1 do begin
   ;- read current date mcalib file (latest version), add mcalib to mcalib_smooth
   file_mask = mcalib_smooth_prefix + string(range_cur, format='(I1)') + mfi_prefix + date_arr[i_d] + '*.sav'
   files = file_search(mcalib_smooth_path, file_mask, count=n_files)
      if (n_files eq 0) then continue
   restore, files[n_files-1]

   ;- if first date from date_arr than truncate mcalib_smooth to the last 3 hours
   if i_d eq 0 then begin
      sub_temp = where((mcalib_smooth.last_epoch - epoch_arr[i_d+1])/3600d3 gt -3, n_temp)
         if n_temp eq 0 then continue
      mcalib_smooth = mcalib_smooth[sub_temp]
   endif
   
   ;- if last date from date_arr than truncate mcalib_smooth to the first 3 hours
   if i_d eq (n_dates-1) then begin
      sub_temp = where((mcalib_smooth.first_epoch - epoch_arr[i_d])/3600d3 lt 3, n_temp)
         if n_temp eq 0 then continue
      mcalib_smooth = mcalib_smooth[sub_temp]
   endif
   
   ;- add mcalib_smooth to mcalib
   n_mcalib_smooth = n_elements(mcalib_smooth)
   mcalib_cur[mcalib_cur_first_sub:mcalib_cur_first_sub+n_mcalib_smooth-1L] = mcalib_smooth
   mcalib_cur_first_sub += n_mcalib_smooth
endfor

;- truncate mcalib_cur to valid elements
if mcalib_cur_first_sub gt 0 then begin
   mcalib_cur   = mcalib_cur[0L:mcalib_cur_first_sub-1L]
   n_mcalib_cur = mcalib_cur_first_sub
endif else begin
   n_mcalib_cur = 0
endelse
;--- end: restore current range mcalib_smooth for dates in date_arr, and save in mcalib_cur 

;--- restore cur_range-1(2,3...) mcalib_smooth for dates in date_arr, and save in mcalib_minus_1
range_minus_1          = range
n_mcalib_minus_1       = 0
continue_cycle_minus_1 = 1

;- go through all ranges down until calibration constants are available or range 0 is reached
while continue_cycle_minus_1 do begin
   ;- set previous range and check it is ge 0
   range_minus_1 -= 1
   if range_minus_1 lt 0 then begin
      n_mcalib_minus_1       = 0
      continue_cycle_minus_1 = 0
      continue
   endif
   
   ;- make mcalib array to store calib constants
   mcalib_minus_1 = replicate({mcalib_struc}, (N_SPINFIT_MAFR_PR > N_SPINFIT_MAFR_SC)*3d3*n_dates)
   mcalib_minus_1_first_sub =  0L
   
   ;- for each date from date_arr
   for i_d=0, n_dates-1 do begin
      ;- read current date mcalib file (latest version), add mcalib to mcalib_smooth
      file_mask = mcalib_smooth_prefix + string(range_minus_1, format='(I1)')+mfi_prefix+date_arr[i_d]+'*.sav'
      files = file_search(mcalib_smooth_path, file_mask, count=n_files)
         if (n_files eq 0) then continue
      restore, files[n_files-1]
   
      ;- if first date from date_arr than truncate mcalib_smooth to the last 3 hours
      if i_d eq 0 then begin
         sub_temp = where((mcalib_smooth.last_epoch - epoch_arr[i_d+1])/3600d3 gt -3, n_temp)
            if n_temp eq 0 then continue
         mcalib_smooth = mcalib_smooth[sub_temp]
      endif

      ;- if last date from date_arr than truncate mcalib_smooth to the first 3 hours
      if i_d eq (n_dates-1) then begin
         sub_temp = where((mcalib_smooth.first_epoch - epoch_arr[i_d])/3600d3 lt 3, n_temp)
            if n_temp eq 0 then continue
         mcalib_smooth = mcalib_smooth[sub_temp]
      endif
      
      ;- add mcalib_smooth to mcalib
      n_mcalib_smooth = n_elements(mcalib_smooth)
      mcalib_minus_1[mcalib_minus_1_first_sub:mcalib_minus_1_first_sub+n_mcalib_smooth-1L] = mcalib_smooth
      mcalib_minus_1_first_sub += n_mcalib_smooth
   endfor
   
   ;- truncate mcalib_minus_1 to valid elements
   if mcalib_minus_1_first_sub ge 2 then begin
      mcalib_minus_1   = mcalib_minus_1[0L:mcalib_minus_1_first_sub-1L]
      n_mcalib_minus_1 = mcalib_minus_1_first_sub
      continue_cycle_minus_1 = 0
   endif else begin
      n_mcalib_minus_1 = 0
   endelse
endwhile
;--- end: restore cur_range-1(2,3...) mcalib_smooth for dates in date_arr, and save in mcalib_minus_1

;--- restore cur_range+1(2,3...) mcalib_smooth for dates in date_arr, and save in mcalib_plus_1
range_plus_1          = range
n_mcalib_plus_1       = 0
continue_cycle_plus_1 = 1

;- go through all ranges up until calibration constants are available or range 7 is reached
while continue_cycle_plus_1 do begin
   ;- set next range and check it is le 7
   range_plus_1 += 1
   if range_plus_1 gt 7 then begin
      n_mcalib_plus_1       = 0
      continue_cycle_plus_1 = 0
      continue
   endif
   
   ;- make mcalib array to store calib constants
   mcalib_plus_1 = replicate({mcalib_struc}, (N_SPINFIT_MAFR_PR > N_SPINFIT_MAFR_SC)*3d3*n_dates)
   mcalib_plus_1_first_sub =  0L
   
   ;- for each date from date_arr
   for i_d=0, n_dates-1 do begin
      ;- read current date mcalib file (latest version), add mcalib to mcalib_smooth
      file_mask = mcalib_smooth_prefix + string(range_plus_1, format='(I1)')+mfi_prefix+date_arr[i_d]+'*.sav'
      files = file_search(mcalib_smooth_path, file_mask, count=n_files)
         if (n_files eq 0) then continue
      restore, files[n_files-1]
   
      ;- if first date from date_arr than truncate mcalib_smooth to the last 3 hours
      if i_d eq 0 then begin
         sub_temp = where((mcalib_smooth.last_epoch - epoch_arr[i_d+1])/3600d3 gt -3, n_temp)
            if n_temp eq 0 then continue
         mcalib_smooth = mcalib_smooth[sub_temp]
      endif

      ;- if last date from date_arr than truncate mcalib_smooth to the first 3 hours
      if i_d eq (n_dates-1) then begin
         sub_temp = where((mcalib_smooth.first_epoch - epoch_arr[i_d])/3600d3 lt 3, n_temp)
            if n_temp eq 0 then continue
         mcalib_smooth = mcalib_smooth[sub_temp]
      endif
      
      ;- add mcalib_smooth to mcalib
      n_mcalib_smooth = n_elements(mcalib_smooth)
      mcalib_plus_1[mcalib_plus_1_first_sub:mcalib_plus_1_first_sub+n_mcalib_smooth-1L] = mcalib_smooth
      mcalib_plus_1_first_sub += n_mcalib_smooth
   endfor
   
   ;- truncate mcalib_plus_1 to valid elements
   if mcalib_plus_1_first_sub ge 2 then begin
      mcalib_plus_1   = mcalib_plus_1[0L:mcalib_plus_1_first_sub-1L]
      n_mcalib_plus_1 = mcalib_plus_1_first_sub
      continue_cycle_plus_1 = 0
   endif else begin
      n_mcalib_plus_1 = 0
   endelse
endwhile
;--- end: restore cur_range+1(2,3...) mcalib_smooth for dates in date_arr, and save in mcalib_plus_

;--- check that calibration constants exist for range_cur, range_minus_1 and range_plus_1
if (n_mcalib_cur lt 1) and (n_mcalib_minus_1 lt 1) and (n_mcalib_plus_1 lt 1) then begin
   mcalib      = replicate({mcalib_struc}, n_epoch)
   mcalib_flag = replicate(1b, n_epoch)
   return
endif

;- check mcalib_cur and fill with mcalib_minus_1 or mcalib_plus_1 if needed
if n_mcalib_cur lt 1 then begin
   if n_mcalib_minus_1 gt n_mcalib_plus_1 then begin
      mcalib_cur   = mcalib_minus_1
      n_mcalib_cur = n_mcalib_minus_1
      range_cur    = range_minus_1
   endif else begin
      mcalib_cur   = mcalib_plus_1
      n_mcalib_cur = n_mcalib_plus_1
      range_cur    = range_plus_1
   endelse
endif

;- check mcalib_minus_1 and fill with mcalib_cur or mcalib_plus_1 if needed
if n_mcalib_minus_1 lt 1 then begin
   if n_mcalib_cur gt n_mcalib_plus_1 then begin
      mcalib_minus_1   = mcalib_cur
      n_mcalib_minus_1 = n_mcalib_cur
      range_minus_1    = range_cur
   endif else begin
      mcalib_minus_1   = mcalib_plus_1
      n_mcalib_minus_1 = n_mcalib_plus_1
      range_minus_1    = range_plus_1
   endelse
endif

;- check mcalib_plus_1 and fill with mcalib_cur or mcalib_minus_1 if needed
if n_mcalib_plus_1 lt 1 then begin
   if n_mcalib_cur gt n_mcalib_minus_1 then begin
      mcalib_plus_1   = mcalib_cur
      n_mcalib_plus_1 = n_mcalib_cur
      range_plus_1    = range_cur
   endif else begin
      mcalib_plus_1   = mcalib_minus_1
      n_mcalib_plus_1 = n_mcalib_minus_1
      range_plus_1    = range_minus_1
   endelse
endif
;--- end: check that calibration constants exist for range_cur, range_minus_1 and range_plus_1

;--- interpolate calibration constants from best nearby points, then transfer sens and zero to range
mcalib      = replicate({mcalib_struc}, n_epoch)
mcalib_flag = replicate(1b, n_epoch)

mcalib_epoch_cur  = (mcalib_cur.first_epoch + mcalib_cur.last_epoch)/2d
if n_mcalib_cur gt 1 then sub_epoch_int_cur=value_locate(mcalib_epoch_cur, epoch) $
   else sub_epoch_int_cur=replicate(0L, n_epoch)
d_epoch_left_cur  = abs(epoch - mcalib_epoch_cur[sub_epoch_int_cur > 0])
d_epoch_right_cur = abs(epoch - mcalib_epoch_cur[(sub_epoch_int_cur+1L) < (n_mcalib_cur-1L)])
d_epoch_min_cur   = min([[d_epoch_left_cur], [d_epoch_right_cur]], dimension=2, /nan)
left_right_cur    = d_epoch_left_cur gt d_epoch_right_cur

mcalib_epoch_minus_1  = (mcalib_minus_1.first_epoch + mcalib_minus_1.last_epoch)/2d
if n_mcalib_minus_1 gt 1 then sub_epoch_int_minus_1=value_locate(mcalib_epoch_minus_1, epoch) $
   else sub_epoch_int_minus_1=replicate(0L, n_epoch)
d_epoch_left_minus_1  = abs(epoch - mcalib_epoch_minus_1[sub_epoch_int_minus_1 > 0])
d_epoch_right_minus_1 = abs(epoch - mcalib_epoch_minus_1[(sub_epoch_int_minus_1+1L) < (n_mcalib_minus_1-1L)])
d_epoch_min_minus_1   = min([[d_epoch_left_minus_1], [d_epoch_right_minus_1]], dimension=2, /nan)
left_right_minus_1    = d_epoch_left_minus_1 gt d_epoch_right_minus_1

mcalib_epoch_plus_1  = (mcalib_plus_1.first_epoch + mcalib_plus_1.last_epoch)/2d
if n_mcalib_plus_1 gt 1 then sub_epoch_int_plus_1=value_locate(mcalib_epoch_plus_1, epoch) $
   else sub_epoch_int_plus_1=replicate(0L, n_epoch)
d_epoch_left_plus_1  = abs(epoch - mcalib_epoch_plus_1[sub_epoch_int_plus_1 > 0])
d_epoch_right_plus_1 = abs(epoch - mcalib_epoch_plus_1[(sub_epoch_int_plus_1+1L) < (n_mcalib_plus_1-1L)])
d_epoch_min_plus_1   = min([[d_epoch_left_plus_1], [d_epoch_right_plus_1]], dimension=2, /nan)
left_right_plus_1    = d_epoch_left_plus_1 gt d_epoch_right_plus_1

;- find which calibration (in cur, minus_1, or plus_1) is best to use for each epoch
sub_within_cur    = where((d_epoch_min_cur le 3d*3600d3) or ((d_epoch_min_cur le (d_epoch_min_minus_1+3d*3600d3)) $
                              and (d_epoch_min_cur le (d_epoch_min_plus_1+3d*3600d3))), n_within_cur)
sub_within_minus_1= where((d_epoch_min_cur gt 3d*3600d3) and (d_epoch_min_cur gt (d_epoch_min_minus_1+3d*3600d3)) $
                              and (d_epoch_min_minus_1 le d_epoch_min_plus_1), n_within_minus_1)
sub_within_plus_1 = where((d_epoch_min_cur gt 3d*3600d3) and (d_epoch_min_cur gt (d_epoch_min_plus_1+3d*3600d3)) $
                              and (d_epoch_min_plus_1 lt d_epoch_min_minus_1), n_within_plus_1)
if (n_within_cur + n_within_plus_1 + n_within_minus_1) ne n_epoch then stop
   
;- range_cur
if n_within_cur gt 0 then begin
   mcalib_flag[sub_within_cur]        = 0b   
   sub_temp = ((sub_epoch_int_cur[sub_within_cur] + left_right_cur[sub_within_cur]) > 0) < (n_mcalib_cur-1L)
   mcalib[sub_within_cur].first_epoch = mcalib_cur[sub_temp].first_epoch
   mcalib[sub_within_cur].last_epoch  = mcalib_cur[sub_temp].last_epoch
   mcalib[sub_within_cur].range       = range
   mcalib[sub_within_cur].from_range  = range_cur
   mcalib[sub_within_cur].qzeroz      = mcalib_cur[sub_temp].qzeroz
   mcalib[sub_within_cur].qangles     = mcalib_cur[sub_temp].qangles
   mcalib[sub_within_cur].coefx       = mcalib_cur[sub_temp].coefx
   mcalib[sub_within_cur].coefy       = mcalib_cur[sub_temp].coefy
   mcalib[sub_within_cur].coefz       = mcalib_cur[sub_temp].coefz
   if n_mcalib_cur gt 1 then begin
      mcalib[sub_within_cur].sensx  = interpol(mcalib_cur.sensx,  mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].sensy  = interpol(mcalib_cur.sensy,  mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].sensz  = interpol(mcalib_cur.sensz,  mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].zerox  = interpol(mcalib_cur.zerox,  mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].zeroy  = interpol(mcalib_cur.zeroy,  mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].zeroz  = interpol(mcalib_cur.zeroz,  mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].thetax = interpol(mcalib_cur.thetax, mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].thetay = interpol(mcalib_cur.thetay, mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].thetaz = interpol(mcalib_cur.thetaz, mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].phix   = interpol(mcalib_cur.phix,   mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].phiy   = interpol(mcalib_cur.phiy,   mcalib_epoch_cur, epoch[sub_within_cur])
      mcalib[sub_within_cur].phiz   = interpol(mcalib_cur.phiz,   mcalib_epoch_cur, epoch[sub_within_cur])
   endif else begin
      mcalib[sub_within_cur].sensx  = mcalib_cur[sub_temp].sensx
      mcalib[sub_within_cur].sensy  = mcalib_cur[sub_temp].sensy
      mcalib[sub_within_cur].sensz  = mcalib_cur[sub_temp].sensz
      mcalib[sub_within_cur].zerox  = mcalib_cur[sub_temp].zerox
      mcalib[sub_within_cur].zeroy  = mcalib_cur[sub_temp].zeroy
      mcalib[sub_within_cur].zeroz  = mcalib_cur[sub_temp].zeroz
      mcalib[sub_within_cur].thetax = mcalib_cur[sub_temp].thetax
      mcalib[sub_within_cur].thetay = mcalib_cur[sub_temp].thetay
      mcalib[sub_within_cur].thetaz = mcalib_cur[sub_temp].thetaz
      mcalib[sub_within_cur].phix   = mcalib_cur[sub_temp].phix
      mcalib[sub_within_cur].phiy   = mcalib_cur[sub_temp].phiy
      mcalib[sub_within_cur].phiz   = mcalib_cur[sub_temp].phiz
   endelse
endif

;- range_minus_1
if n_within_minus_1 gt 0 then begin
   mcalib_flag[sub_within_minus_1]        = 0b   
   sub_temp = ((sub_epoch_int_minus_1[sub_within_minus_1] + left_right_minus_1[sub_within_minus_1]) > 0) < $
              (n_mcalib_minus_1-1L)
   mcalib[sub_within_minus_1].first_epoch = mcalib_minus_1[sub_temp].first_epoch
   mcalib[sub_within_minus_1].last_epoch  = mcalib_minus_1[sub_temp].last_epoch
   mcalib[sub_within_minus_1].range       = range
   mcalib[sub_within_minus_1].from_range  = range_minus_1   
   mcalib[sub_within_minus_1].qzeroz      = mcalib_minus_1[sub_temp].qzeroz
   mcalib[sub_within_minus_1].qangles     = mcalib_minus_1[sub_temp].qangles
   mcalib[sub_within_minus_1].coefx       = mcalib_minus_1[sub_temp].coefx
   mcalib[sub_within_minus_1].coefy       = mcalib_minus_1[sub_temp].coefy
   mcalib[sub_within_minus_1].coefz       = mcalib_minus_1[sub_temp].coefz
   if n_mcalib_minus_1 gt 1 then begin
      mcalib[sub_within_minus_1].sensx  = interpol(mcalib_minus_1.sensx,  mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].sensy  = interpol(mcalib_minus_1.sensy,  mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].sensz  = interpol(mcalib_minus_1.sensz,  mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].zerox  = interpol(mcalib_minus_1.zerox,  mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].zeroy  = interpol(mcalib_minus_1.zeroy,  mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].zeroz  = interpol(mcalib_minus_1.zeroz,  mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].thetax = interpol(mcalib_minus_1.thetax, mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].thetay = interpol(mcalib_minus_1.thetay, mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].thetaz = interpol(mcalib_minus_1.thetaz, mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].phix   = interpol(mcalib_minus_1.phix,   mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].phiy   = interpol(mcalib_minus_1.phiy,   mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
      mcalib[sub_within_minus_1].phiz   = interpol(mcalib_minus_1.phiz,   mcalib_epoch_minus_1, $
                                                   epoch[sub_within_minus_1])
   endif else begin
      mcalib[sub_within_minus_1].sensx  = mcalib_minus_1[sub_temp].sensx
      mcalib[sub_within_minus_1].sensy  = mcalib_minus_1[sub_temp].sensy
      mcalib[sub_within_minus_1].sensz  = mcalib_minus_1[sub_temp].sensz
      mcalib[sub_within_minus_1].zerox  = mcalib_minus_1[sub_temp].zerox
      mcalib[sub_within_minus_1].zeroy  = mcalib_minus_1[sub_temp].zeroy
      mcalib[sub_within_minus_1].zeroz  = mcalib_minus_1[sub_temp].zeroz
      mcalib[sub_within_minus_1].thetax = mcalib_minus_1[sub_temp].thetax
      mcalib[sub_within_minus_1].thetay = mcalib_minus_1[sub_temp].thetay
      mcalib[sub_within_minus_1].thetaz = mcalib_minus_1[sub_temp].thetaz
      mcalib[sub_within_minus_1].phix   = mcalib_minus_1[sub_temp].phix
      mcalib[sub_within_minus_1].phiy   = mcalib_minus_1[sub_temp].phiy
      mcalib[sub_within_minus_1].phiz   = mcalib_minus_1[sub_temp].phiz
   endelse
endif

;- range_plus_1
if n_within_plus_1 gt 0 then begin
   mcalib_flag[sub_within_plus_1]        = 0b   
   sub_temp = ((sub_epoch_int_plus_1[sub_within_plus_1] + left_right_plus_1[sub_within_plus_1]) > 0) < $
              (n_mcalib_plus_1-1L)
   mcalib[sub_within_plus_1].first_epoch = mcalib_plus_1[sub_temp].first_epoch
   mcalib[sub_within_plus_1].last_epoch  = mcalib_plus_1[sub_temp].last_epoch
   mcalib[sub_within_plus_1].range       = range
   mcalib[sub_within_plus_1].from_range  = range_plus_1   
   mcalib[sub_within_plus_1].qzeroz      = mcalib_plus_1[sub_temp].qzeroz
   mcalib[sub_within_plus_1].qangles     = mcalib_plus_1[sub_temp].qangles
   mcalib[sub_within_plus_1].coefx       = mcalib_plus_1[sub_temp].coefx
   mcalib[sub_within_plus_1].coefy       = mcalib_plus_1[sub_temp].coefy
   mcalib[sub_within_plus_1].coefz       = mcalib_plus_1[sub_temp].coefz
   if n_mcalib_plus_1 gt 0 then begin
      mcalib[sub_within_plus_1].sensx  = interpol(mcalib_plus_1.sensx,  mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].sensy  = interpol(mcalib_plus_1.sensy,  mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].sensz  = interpol(mcalib_plus_1.sensz,  mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].zerox  = interpol(mcalib_plus_1.zerox,  mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].zeroy  = interpol(mcalib_plus_1.zeroy,  mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].zeroz  = interpol(mcalib_plus_1.zeroz,  mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].thetax = interpol(mcalib_plus_1.thetax, mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].thetay = interpol(mcalib_plus_1.thetay, mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].thetaz = interpol(mcalib_plus_1.thetaz, mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].phix   = interpol(mcalib_plus_1.phix,   mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].phiy   = interpol(mcalib_plus_1.phiy,   mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
      mcalib[sub_within_plus_1].phiz   = interpol(mcalib_plus_1.phiz,   mcalib_epoch_plus_1, $
                                                  epoch[sub_within_plus_1])
   endif else begin
      mcalib[sub_within_plus_1].sensx  = mcalib_plus_1[sub_temp].sensx 
      mcalib[sub_within_plus_1].sensy  = mcalib_plus_1[sub_temp].sensy 
      mcalib[sub_within_plus_1].sensz  = mcalib_plus_1[sub_temp].sensz
      mcalib[sub_within_plus_1].zerox  = mcalib_plus_1[sub_temp].zerox
      mcalib[sub_within_plus_1].zeroy  = mcalib_plus_1[sub_temp].zeroy
      mcalib[sub_within_plus_1].zeroz  = mcalib_plus_1[sub_temp].zeroz
      mcalib[sub_within_plus_1].thetax = mcalib_plus_1[sub_temp].thetax
      mcalib[sub_within_plus_1].thetay = mcalib_plus_1[sub_temp].thetay
      mcalib[sub_within_plus_1].thetaz = mcalib_plus_1[sub_temp].thetaz
      mcalib[sub_within_plus_1].phix   = mcalib_plus_1[sub_temp].phix
      mcalib[sub_within_plus_1].phiy   = mcalib_plus_1[sub_temp].phiy
      mcalib[sub_within_plus_1].phiz   = mcalib_plus_1[sub_temp].phiz
   endelse
endif

;- transfer sens and zero to range if range ne from_range
sub_from_to = where(mcalib.range ne mcalib.from_range, n_from_to)
if n_from_to gt 0 then begin
   ;- sensx
   sensx_from       = mcalib[sub_from_to].sensx
   sensx_ratio_from = cal[outer_mag].sens[mcalib[sub_from_to].from_range].x/sensx_from
   sensx_ratio_1    = dblarr(n_from_to)                    
   sub_zero         = where(cal[outer_mag].sens_coef[mcalib[sub_from_to].from_range].x[1] eq 0, n_zero, $
                            complement=sub_nonzero, ncomplement=n_nonzero)
   if n_zero    gt 0 then sensx_ratio_1[sub_zero]    = 1d
   if n_nonzero gt 0 then sensx_ratio_1[sub_nonzero] = $
      (sensx_ratio_from[sub_nonzero] - cal[outer_mag].sens_coef[mcalib[sub_from_to[sub_nonzero]].from_range].x[0])/$
      cal[outer_mag].sens_coef[mcalib[sub_from_to[sub_nonzero]].from_range].x[1]
   sensx_ratio_to   = (cal[outer_mag].sens_coef[mcalib[sub_from_to].range].x[0] + $
                       sensx_ratio_1*cal[outer_mag].sens_coef[mcalib[sub_from_to].range].x[1])
   sensx_to         = cal[outer_mag].sens[mcalib[sub_from_to].range].x/sensx_ratio_to

   ;- zerox
   dzerox_nt_from   = (mcalib[sub_from_to].zerox - cal[outer_mag].zero[mcalib[sub_from_to].from_range].x)*sensx_from
   dzerox_nt_1      = (dzerox_nt_from - cal[outer_mag].zero_coef[mcalib[sub_from_to].from_range].x[0])/$
                      cal[outer_mag].zero_coef[mcalib[sub_from_to].from_range].x[1]
   dzerox_nt_to     = cal[outer_mag].zero_coef[mcalib[sub_from_to].range].x[0] + $
                      dzerox_nt_1*cal[outer_mag].zero_coef[mcalib[sub_from_to].range].x[1]
   zerox_to         = dzerox_nt_to/sensx_to + cal[outer_mag].zero[mcalib[sub_from_to].range].x
   
   ;- sensy
   sensy_from       = mcalib[sub_from_to].sensy
   sensy_ratio_from = cal[outer_mag].sens[mcalib[sub_from_to].from_range].y/sensy_from
   sensy_ratio_1    = dblarr(n_from_to)                    
   sub_zero         = where(cal[outer_mag].sens_coef[mcalib[sub_from_to].from_range].y[1] eq 0, n_zero, $
                            complement=sub_nonzero, ncomplement=n_nonzero)
   if n_zero    gt 0 then sensy_ratio_1[sub_zero]    = 1d
   if n_nonzero gt 0 then sensy_ratio_1[sub_nonzero] = $
      (sensy_ratio_from[sub_nonzero] - cal[outer_mag].sens_coef[mcalib[sub_from_to[sub_nonzero]].from_range].y[0])/$
      cal[outer_mag].sens_coef[mcalib[sub_from_to[sub_nonzero]].from_range].y[1]
   sensy_ratio_to   = cal[outer_mag].sens_coef[mcalib[sub_from_to].range].y[0] + $
                      sensy_ratio_1*cal[outer_mag].sens_coef[mcalib[sub_from_to].range].y[1]
   sensy_to         = cal[outer_mag].sens[mcalib[sub_from_to].range].y/sensy_ratio_to

   ;- zeroy
   dzeroy_nt_from   = (mcalib[sub_from_to].zeroy - cal[outer_mag].zero[mcalib[sub_from_to].from_range].y)*sensy_from
   dzeroy_nt_1      = (dzeroy_nt_from - cal[outer_mag].zero_coef[mcalib[sub_from_to].from_range].y[0])/$
                      cal[outer_mag].zero_coef[mcalib[sub_from_to].from_range].y[1]
   dzeroy_nt_to     = cal[outer_mag].zero_coef[mcalib[sub_from_to].range].y[0] + $
                      dzeroy_nt_1*cal[outer_mag].zero_coef[mcalib[sub_from_to].range].y[1]
   zeroy_to         = dzeroy_nt_to/sensy_to + cal[outer_mag].zero[mcalib[sub_from_to].range].y

   ;- sensz
   sensz_from       = mcalib[sub_from_to].sensz
   sensz_ratio_from = cal[outer_mag].sens[mcalib[sub_from_to].from_range].z/sensz_from
   sensz_ratio_1    = dblarr(n_from_to)                    
   sub_zero         = where(cal[outer_mag].sens_coef[mcalib[sub_from_to].from_range].z[1] eq 0, n_zero, $
                            complement=sub_nonzero, ncomplement=n_nonzero)
   if n_zero    gt 0 then sensz_ratio_1[sub_zero]    = 1d
   if n_nonzero gt 0 then sensz_ratio_1[sub_nonzero] = $
      (sensz_ratio_from[sub_nonzero] - cal[outer_mag].sens_coef[mcalib[sub_from_to[sub_nonzero]].from_range].z[0])/$
      cal[outer_mag].sens_coef[mcalib[sub_from_to[sub_nonzero]].from_range].z[1]
   sensz_ratio_to   = cal[outer_mag].sens_coef[mcalib[sub_from_to].range].z[0] + $
                      sensz_ratio_1*cal[outer_mag].sens_coef[mcalib[sub_from_to].range].z[1]
   sensz_to         = cal[outer_mag].sens[mcalib[sub_from_to].range].z/sensz_ratio_to
 
   ;- zeroz
   dzeroz_nt_from   = (mcalib[sub_from_to].zeroz - cal[outer_mag].zero[mcalib[sub_from_to].from_range].z)*sensz_from
   dzeroz_nt_1      = (dzeroz_nt_from - cal[outer_mag].zero_coef[mcalib[sub_from_to].from_range].z[0])/$
                      cal[outer_mag].zero_coef[mcalib[sub_from_to].from_range].z[1]
   dzeroz_nt_to     = cal[outer_mag].zero_coef[mcalib[sub_from_to].range].z[0] + $
                      dzeroz_nt_1*cal[outer_mag].zero_coef[mcalib[sub_from_to].range].z[1]
   zeroz_to         = dzeroz_nt_to/sensz_to + cal[outer_mag].zero[mcalib[sub_from_to].range].z
   
   ;- replace sens and zero
   mcalib[sub_from_to].sensx = sensx_to
   mcalib[sub_from_to].sensy = sensy_to
   mcalib[sub_from_to].sensz = sensz_to
   mcalib[sub_from_to].zerox = zerox_to
   mcalib[sub_from_to].zeroy = zeroy_to
   mcalib[sub_from_to].zeroz = zeroz_to
endif
;--- end: interpolate calibration constants from best nearby points, then transfer sens and zero to range
         
END