PRO MFI_ALF_CAL_2, START_YEAR=START_YEAR

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read and check arguments
if (n_elements(start_year) eq 1) then start_year = long(start_year)  else message, 'no start_year'

;- number of days
cdf_epoch, epoch_start_year, start_year, /compute
cdf_epoch, epoch_end_year,   start_year+1, /compute
n_days = round((epoch_end_year-epoch_start_year)/(24d*3600d3))
i_day = -1L

;- create array of structures
zero_z_arr = replicate({year:0L, doy:0d, n_win:0L, zero_z:0d, q:1b}, 200, n_days)

;- run through all days
for i_day=0L, n_days-1L do begin
   print, strtrim(string(i_day+1L),2), '  of  ', strtrim(string(n_days),2)

   ;- compute year and doy
   cdf_epoch, epoch_start_year + i_day*24d*3600d3, year, month, day, /break
   cdf_epoch, epoch_year_current, year, /compute
   doy = round((epoch_start_year + i_day*24d*3600d3 - epoch_year_current)/(24d*3600d3)) + 1L

   ;- restore win_result_arr for current day
   filename = file_search(calib_alf_path + 'wi_mfi_alf_' + $
                string(year, format='(I04)') + string(month, format='(I02)') + string(day, format='(I02)') + '_v01.sav')
   if (filename eq '') then continue
   restore, filename

   ;- sort windows
   sub_sort = sort(win_result_arr.sub_start  + (win_result_arr.sub_end - win_result_arr.sub_start)/2L)
   win_result_arr = win_result_arr[sub_sort]

   ;- select windows
   sub_win = where( (win_result_arr.mean_bt_w_zero/win_result_arr.stddev_bt_w_zero gt 50) and $
                    (win_result_arr.stddev_bz/win_result_arr.stddev_bt_w_zero gt   7) and $
                    (win_result_arr.stddev_bz/win_result_arr.stddev_bt_w_zero lt  20) and $
                    ((win_result_arr.sub_end - win_result_arr.sub_start) le 100), n_sub_win)

      if (n_sub_win eq 0) then continue

   ;- split all windows into iterations
   n_iter_doy=1L
      if (n_iter_doy eq 0) then continue
   n_win_iter = floor(n_sub_win/n_iter_doy)

   ;- for each iteration
   for i_iter=0L, n_iter_doy-1L  do begin
      ;- reset accumulators
      number_cumul   = 0d
      bz_sq_cumul    = 0d
      bz_bt_sq_cumul = 0d
      
      ;- for all windows compute accumulators
      for i=i_iter*n_win_iter, (i_iter+1L)*n_win_iter-1L do begin
         bx    = bx_3s[win_result_arr[sub_win[i]].sub_start:win_result_arr[sub_win[i]].sub_end]
         by    = by_3s[win_result_arr[sub_win[i]].sub_start:win_result_arr[sub_win[i]].sub_end]
         bz    = bz_3s[win_result_arr[sub_win[i]].sub_start:win_result_arr[sub_win[i]].sub_end]
         bt_sq = (bx^2d + by^2d + bz^2d)
         bz_minus_mean    = bz - mean(bz)
         bt_sq_minus_mean = bt_sq - mean(bt_sq)
         
         number_cumul   += win_result_arr[sub_win[i]].sub_end - win_result_arr[sub_win[i]].sub_start + 1L
         bz_sq_cumul    += total(bz_minus_mean^2d)
         bz_bt_sq_cumul +=total(bz_minus_mean*bt_sq_minus_mean)   
      endfor
      
      ;- compute zero_z
      zero_z_cumul = 0.5d*(bz_bt_sq_cumul/number_cumul)/(bz_sq_cumul/number_cumul)

      ;- fill results and set quality flag to 0
      zero_z_arr[i_iter,i_day].year   = year
      zero_z_arr[i_iter,i_day].doy    = doy + double(i_iter)/double(n_iter_doy)
      zero_z_arr[i_iter,i_day].n_win  = n_win_iter
      zero_z_arr[i_iter,i_day].zero_z = zero_z_cumul
      zero_z_arr[i_iter,i_day].q      = 0b
   endfor
endfor

;- save zero_z
openw, lun, bz_offset_path + 'wi_bzi_offset_mfi_' + string(start_year,format='(I4)') + '_v01.dat', /get_lun 
for i_=0L, n_days-1L do $
   if zero_z_arr[0,i_].q eq 0b then $
      printf, lun, zero_z_arr[0,i_].year, zero_z_arr[0,i_].doy, zero_z_arr[0,i_].zero_z, 'v3', $
              format='(X, I4, 3X, I3, 4X, F9.5, 2X, A2)'
free_lun, lun

END
