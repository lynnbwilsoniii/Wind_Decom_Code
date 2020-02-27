PRO MFI_CALIB_MULTI, START_DATE=START_DATE, END_DATE=END_DATE, RANGE=RANGE, OUTER_MAG=OUTER_MAG

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_date)  eq 1) then start_date = long(start_date) else message, 'no start_date'
if (n_elements(end_date)    eq 1) then end_date   = long(end_date)   else end_date = start_date
if (n_elements(range)       eq 1) then range      = byte(range)      else range    = 1b
if keyword_set(outer_mag) then begin
   outer_mag      = 1
   zeroz_prefix   = zeroz_prefix_ou
   acalib_prefix  = acalib_prefix_ou
   spinfit_prefix = spinfit_prefix_ou
   mcalib_prefix  = mcalib_prefix_ou
endif else begin
   outer_mag      = 0
   zeroz_prefix   = zeroz_prefix_in
   acalib_prefix  = acalib_prefix_in
   spinfit_prefix = spinfit_prefix_in
   mcalib_prefix  = mcalib_prefix_in
endelse

;--- read zeroz and acalib annual files in the range [start_date-1day, end_date+1day]
;- start_year (start_year_minus1day), end_year(end_year_plus1day), n_years
start_year   = floor(start_date/1d4)
start_month  = floor((start_date mod 1d4)/1d2)
start_day    = start_date mod 100
end_year     = floor(end_date/1d4)
end_month    = floor((end_date mod 1d4)/1d2)
end_day      = end_date mod 100
cdf_epoch, start_epoch, start_year, start_month, start_day, /c
cdf_epoch, start_epoch-MSDAY, start_year_minus1day, start_month_minus1day, start_day_minus1day, /b
cdf_epoch, end_epoch, end_year, end_month, end_day, /c
cdf_epoch, end_epoch+MSDAY, end_year_plus1day, end_month_plus1day, end_day_plus1day, /b
n_years = end_year_plus1day - start_year_minus1day + 1

;-zeroz_arr
if ((range eq 0) or (range eq 2)) and (outer_mag eq 0) then range_zeroz=range else range_zeroz=1b
zeroz_arr = replicate({zeroz_struc}, 366L*n_years)
zeroz_rec = {zeroz_struc}
sub_zeroz = 0L
;- for each year read all zeroz records
for i_y=start_year_minus1day, end_year_plus1day do begin
   ;- find zeroz files
   zeroz_files = file_search(zeroz_path, zeroz_prefix + string(range_zeroz, format='(I1)') + mfi_prefix + $
                             string(i_y,format='(I4)') + '*.dat', count=n_zeroz_files)
      if n_zeroz_files eq 0 then message, 'no zeroz files found' 
   ;- read latest version zeroz file
   openr, lun, zeroz_files[n_zeroz_files-1], /get_lun 
   while eof(lun) eq 0 do begin
      readf, lun, zeroz_rec
      zeroz_arr[sub_zeroz] = zeroz_rec
      sub_zeroz +=1L
   endwhile
   ;- close file
   free_lun, lun
endfor
;- truncate zeroz_arr
zeroz_arr = zeroz_arr[0:sub_zeroz-1]

;- acalib_arr
acalib_arr  = replicate({acalib_struc}, 366L*n_years)
acalib_rec  = {acalib_struc}
sub_acalib  = 0L
;- for each year read all acalib records
for i_y=start_year_minus1day, end_year_plus1day do begin
   ;- find latest version acalib file
   acalib_files = file_search(acalib_path, acalib_prefix+'1' + mfi_prefix + string(i_y,format='(I4)') + $
                             '*.dat', count=n_acalib_files)
      if n_acalib_files eq 0 then message, 'no acalib files found' 
   ;- read acalib
   openr, lun, acalib_files[n_acalib_files-1], /get_lun 
   while eof(lun) eq 0 do begin
      readf, lun, acalib_rec
      acalib_arr[sub_acalib] = acalib_rec
      sub_acalib +=1L
   endwhile
   ;- close file
   free_lun, lun
endfor
;- truncate acalib_arr
acalib_arr = acalib_arr[0:sub_acalib-1]
;- transfer angles to radians
acalib_arr.thetax *=!dtor
acalib_arr.thetay *=!dtor
acalib_arr.thetaz *=!dtor
acalib_arr.phix   *=!dtor
acalib_arr.phiy   *=!dtor
acalib_arr.phiz   *=!dtor
;--- end: read zeroz and acalib annual files in the range [start_date-1day, end_date+1day]

;- find spinfit files with latest versions for [start_date, end_date] interval and current range
file_mask = spinfit_prefix + string(range,format='(I1)') + mfi_prefix + '*.sav'
files     = file_search(spinfit_path, file_mask, count=n_files)
   if (n_files eq 0) then return  ;message, 'no spinfit files found'
;- find files in the [start_date, end_date] range
files_date= strarr(n_files)
for i_f=0, n_files-1 do $
   files_date[i_f] = strmid(files[i_f], 15, 8, /reverse_offset)
sub_ok_files = where((long(files_date) ge start_date) and (long(files_date) le end_date), n_ok_files)
   if (n_ok_files eq 0) then return
;- select latest version files                     
files      = files[ sub_ok_files[uniq(files_date[sub_ok_files])] ]
n_files    = n_elements(files)
files_date = strarr(n_files)
for i_f=0, n_files-1 do files_date[i_f] = strmid(files[i_f], 15, 8, /reverse_offset)

;- for each spinfit file, restore spinfit, correct zeroz and angles, compute calib consts, and save result to mcalib
for i_f=0, n_files-1 do begin
   ;- restore spinfit from file
   restore, files[i_f]
   if outer_mag then spinfit=temporary(spinfit_ou) else spinfit=temporary(spinfit_in)
   n_spinfit = n_elements(spinfit)

   ;- find zeroz and acalib sub corresponding to current day
   sub_zeroz  = (where(zeroz_arr.date eq files_date[i_f], n_zeroz))[0]
      if (n_zeroz eq 0) then message, 'no zeroz record found'
   sub_acalib = (where(acalib_arr.date eq files_date[i_f], n_acalib))[0]
      if (n_acalib eq 0) then message, 'no acalib record found'
   
   ;--- compute calibration constants
   ;- sub of good data points
   sub_ok = where((sqrt(spinfit.coefx[1]^2d + spinfit.coefx[2]^2d) gt 1) and $
                  (sqrt(spinfit.coefy[1]^2d + spinfit.coefy[2]^2d) gt 1) and $
                  (sqrt(spinfit.coefz[1]^2d + spinfit.coefz[2]^2d) gt 1), n_ok)
   if (n_ok eq 0) then continue
   spinfit = spinfit[sub_ok]
   
   ;- interpolate zeroz, thetaxyz, and phixyz to spinfit epoch
   n_zeroz_arr  = n_elements(zeroz_arr)
   zeroz_epoch  = dblarr(n_zeroz_arr) 
   zeroz_year   = floor(zeroz_arr.date/1d4)
   zeroz_month  = floor((zeroz_arr.date mod 1d4)/1d2)
   zeroz_day    = zeroz_arr.date mod 100
   for i_e=0, n_zeroz_arr-1 do begin
      cdf_epoch, epoch_temp,  zeroz_year[i_e], zeroz_month[i_e], zeroz_day[i_e], /c
      zeroz_epoch[i_e] = epoch_temp + MSDAY/2d
   endfor
   n_acalib_arr  = n_elements(acalib_arr)
   acalib_epoch  = dblarr(n_acalib_arr)
   acalib_year   = floor(acalib_arr.date/1d4)
   acalib_month  = floor((acalib_arr.date mod 1d4)/1d2)
   acalib_day    = acalib_arr.date mod 100
   for i_e=0, n_acalib_arr-1 do begin
      cdf_epoch, epoch_temp,  acalib_year[i_e], acalib_month[i_e], acalib_day[i_e], /c
      acalib_epoch[i_e] = epoch_temp + MSDAY/2d
   endfor
   spinfit_epoch   = (spinfit.first_epoch + spinfit.last_epoch)/2d
   zeroz_interpol  = interpol(zeroz_arr.zeroz,   zeroz_epoch,  spinfit_epoch) 
   thetax_interpol = interpol(acalib_arr.thetax, acalib_epoch, spinfit_epoch) 
   thetay_interpol = interpol(acalib_arr.thetay, acalib_epoch, spinfit_epoch) 
   thetaz_interpol = interpol(acalib_arr.thetaz, acalib_epoch, spinfit_epoch) 
   phix_interpol   = interpol(acalib_arr.phix,   acalib_epoch, spinfit_epoch) 
   phiy_interpol   = interpol(acalib_arr.phiy,   acalib_epoch, spinfit_epoch) 
   phiz_interpol   = interpol(acalib_arr.phiz,   acalib_epoch, spinfit_epoch) 
   
   ;- arrays of real sensitivities
   tempx_arr  = sqrt(spinfit.coefx[1]^2d +spinfit.coefx[2]^2d)/cos(thetax_interpol)
   tempy_arr  = sqrt(spinfit.coefy[1]^2d +spinfit.coefy[2]^2d)/cos(thetay_interpol)
   sensx_ratio_arr = sqrt((cal[outer_mag].sens[range].y*tempy_arr)/(cal[outer_mag].sens[range].x*tempx_arr))
   sensy_ratio_arr = 1d/sensx_ratio_arr
   sensx_arr  = sensx_ratio_arr*cal[outer_mag].sens[range].x
   sensy_arr  = sensy_ratio_arr*cal[outer_mag].sens[range].y
   
   ;- arrays of slopes
   slopex_arr = cal[outer_mag].sens[range].z/sensx_arr*sin(thetax_interpol)/cos(thetaz_interpol)
   slopey_arr = cal[outer_mag].sens[range].z/sensy_arr*sin(thetay_interpol)/cos(thetaz_interpol)
   
   ;- arrays of x,y zeroes, use real zeroz (but take range into account) to compute x,y zeroes
   if range eq range_zeroz then begin
      zeroz_cur_range = zeroz_interpol
   endif else begin
      db_nt_1         = (zeroz_interpol - cal[outer_mag].zero[1].z)*cal[outer_mag].sens[1].z
      zeroz_cur_range = ((cal[outer_mag].zero_coef[range].z)[0] + db_nt_1*(cal[outer_mag].zero_coef[range].z)[1])/$
                        cal[outer_mag].sens[range].z + cal[outer_mag].zero[range].z
   endelse

   zerox_arr = spinfit.coefx[0] - slopex_arr*(spinfit.coefz[0] - zeroz_cur_range)
   zeroy_arr = spinfit.coefy[0] - slopey_arr*(spinfit.coefz[0] - zeroz_cur_range)
   ;--- end: find calibration constants

   ;- make structure array and store calibration constants for current day
   mcalib               = replicate({mcalib_struc}, n_ok)
   mcalib.first_epoch   = spinfit.first_epoch
   mcalib.last_epoch    = spinfit.last_epoch
   mcalib.range         = range
   mcalib.from_range    = range
   mcalib.qzeroz        = zeroz_arr[sub_zeroz].flag
   mcalib.qangles       = acalib_arr[sub_acalib].flag
   mcalib.coefx         = spinfit.coefx
   mcalib.coefy         = spinfit.coefy
   mcalib.coefz         = spinfit.coefz
   mcalib.sensx         = sensx_arr
   mcalib.sensy         = sensy_arr
   mcalib.sensz         = cal[outer_mag].sens[range].z
   mcalib.zerox         = zerox_arr
   mcalib.zeroy         = zeroy_arr
   mcalib.zeroz         = zeroz_cur_range
   mcalib.thetax        = thetax_interpol
   mcalib.thetay        = thetay_interpol
   mcalib.thetaz        = thetaz_interpol
   mcalib.phix          = phix_interpol
   mcalib.phiy          = phiy_interpol
   mcalib.phiz          = phiz_interpol

   ;- save to file
   save, mcalib, filename = mcalib_path + mcalib_prefix + string(range, format='(I1)') + $
                            mfi_prefix + files_date[i_f] + '_v01.sav'
endfor

END