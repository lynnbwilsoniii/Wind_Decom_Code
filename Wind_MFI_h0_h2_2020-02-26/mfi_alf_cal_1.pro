FUNCTION MFI_ALF_CAL_1, YEAR=YEAR, DOY=DOY

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- find month and day
cdf_epoch, epoch_day, year, 0, doy, /compute
cdf_epoch, epoch_day, year, month, day, /break

;- read 3-second file
file_name = file_search(h0_original_path + '/wi_h0_mfi_' + string(year, format='(I4)') + string(month, format='(I02)') + $
                        string(day, format='(I02)') +  '_v03.cdf')
   if file_name eq '' then return, 'no    ' + string(year, format='(I4)') + string(month, format='(I02)') + $
      string(day, format='(I02)') + '   file'
;-
cdfid = cdf_open(file_name[0], /readonly)
;-
var_name = 'Epoch3'
cdf_control, cdfid, get_var_info=var_info, variable=var_name
cdf_varget,  cdfid, var_name, epoch3_value, rec_count=var_info.maxrec+1
cdf_attget,  cdfid, 'FILLVAL', var_name, epoch3_fillvalue
;-
var_name = 'B3GSE'
cdf_control, cdfid, get_var_info=var_info, variable=var_name
cdf_varget,  cdfid, var_name, b3gse_value, rec_count=var_info.maxrec+1
cdf_attget,  cdfid, 'FILLVAL', var_name, b3gse_fillvalue
;-
sub_3s = where((b3gse_value[0,*] ne b3gse_fillvalue) and (b3gse_value[1,*] ne b3gse_fillvalue) and $
                 (b3gse_value[2,*] ne b3gse_fillvalue) and (epoch3_value ne epoch3_fillvalue), n_sub_3s)
   if n_sub_3s lt 1000 then  return, 'less then 1000 values in    ' + string(year, format='(I4)') + $
         string(month, format='(I02)') + string(day, format='(I02)') + '   file
;-
epoch3_value = epoch3_value[sub_3s]
b3gse_value  = b3gse_value[*, sub_3s]
;-
cdf_close, cdfid

;- create 3-second variables
epoch_3s = reform(epoch3_value)
hour_3s  = (epoch_3s - epoch_day)/3600d3
bx_3s    = reform(b3gse_value[0,*])
by_3s    = reform(b3gse_value[1,*])
bz_3s    = reform(b3gse_value[2,*])


;--- define and run all possible windows ------------------------------------------------------------------------
;- define all posible windows
win_size_min_s   = long(5L*60L/3L)
win_size_max_s   = long(30L*60L/3L)
win_dsize_s      = long(1L*60L/3L)
win_dshift_s     = long(10L/3L)
win_result_arr   = replicate({sub_start:0L, sub_end:0L, zerox:0., zeroy:0., zeroz:0., mean_bx_w_zero:0., mean_by_w_zero:0., $
                              mean_bz_w_zero:0., mean_bt_w_zero:0., mean_bt_sq_w_zero:0., stddev_bx:0., stddev_by:0., $
                              stddev_bz:0., stddev_bt_w_zero:0., stddev_bt_sq_w_zero:0.}, 1d6)
win_result_cntr = 0L

;- run through all possible windows
win_size_cur_s = win_size_min_s 
while (win_size_cur_s le win_size_max_s) do begin
   print, win_size_cur_s
   win_shift_cur_s = 0d

   ;-
   while (win_shift_cur_s lt win_size_cur_s) do begin
      ;- 
      n_win = floor((n_sub_3s-win_shift_cur_s)/win_size_cur_s)
         if n_win eq 0 then begin
            win_shift_cur_s += win_dshift_s
            continue
         endif
      bx_win_2d = reform(bx_3s[win_shift_cur_s:(win_shift_cur_s+win_size_cur_s*n_win-1L)], win_size_cur_s, n_win)
      by_win_2d = reform(by_3s[win_shift_cur_s:(win_shift_cur_s+win_size_cur_s*n_win-1L)], win_size_cur_s, n_win)
      bz_win_2d = reform(bz_3s[win_shift_cur_s:(win_shift_cur_s+win_size_cur_s*n_win-1L)], win_size_cur_s, n_win)
      bt_win_2d = sqrt(bx_win_2d^2d + by_win_2d^2d + bz_win_2d^2d)
      
      ;-
      for i_w=0L,n_win-1L do begin
         bx = bx_win_2d[*,i_w]
         by = by_win_2d[*,i_w]
         bz = bz_win_2d[*,i_w]
         bt = bt_win_2d[*,i_w]

         bx_mean    = mean(bx)
         by_mean    = mean(by)
         bz_mean    = mean(bz)
         bz_sq_mean = mean(bz^2d)
         bt_sq_mean = mean(bt^2d)
         bz_bt_sq_mean = mean(bz*bt^2d)
         

         zero_z_cur = 0.5d*(bz_bt_sq_mean-bz_mean*bt_sq_mean)/(bz_sq_mean-bz_mean^2d)
         
         bt_w_zeros   = sqrt(bx^2d + by^2d + (bz - zero_z_cur)^2d)

         win_result_arr[win_result_cntr].sub_start = win_shift_cur_s + win_size_cur_s*i_w
         win_result_arr[win_result_cntr].sub_end   = win_shift_cur_s + win_size_cur_s*(i_w+1L) - 1L
         win_result_arr[win_result_cntr].zeroz     = zero_z_cur
         win_result_arr[win_result_cntr].mean_bx_w_zero      = bx_mean
         win_result_arr[win_result_cntr].mean_by_w_zero      = by_mean
         win_result_arr[win_result_cntr].mean_bz_w_zero      = bz_mean - zero_z_cur
         win_result_arr[win_result_cntr].mean_bt_w_zero      = mean(bt_w_zeros)
         win_result_arr[win_result_cntr].mean_bt_sq_w_zero   = mean(bt_w_zeros^2d)
         win_result_arr[win_result_cntr].stddev_bx           = stddev(bx)
         win_result_arr[win_result_cntr].stddev_by           = stddev(by)
         win_result_arr[win_result_cntr].stddev_bz           = stddev(bz)
         win_result_arr[win_result_cntr].stddev_bt_w_zero    = stddev(bt_w_zeros)
         win_result_arr[win_result_cntr].stddev_bt_sq_w_zero = stddev(bt_w_zeros^2d)
         win_result_cntr += 1L
      endfor
      
      win_shift_cur_s += win_dshift_s
   endwhile
   
   win_size_cur_s += win_dsize_s
endwhile
win_result_arr = win_result_arr[0L:win_result_cntr-1L]
;--- end: define and run all possible windows -------------------------------------------------------------------


;- save results into file
save, hour_3s, bx_3s, by_3s, bz_3s, win_result_arr, filename=calib_alf_path + '/wi_mfi_alf_' + $
      string(year, format='(I04)') + string(month, format='(I02)') + string(day, format='(I02)') + '_v01.sav'

return, 'OK - ' + string(year, format='(I04)') + string(month, format='(I02)') + string(day, format='(I02)')


END