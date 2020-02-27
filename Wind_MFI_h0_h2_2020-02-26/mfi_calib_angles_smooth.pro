PRO MFI_CALIB_ANGLES_SMOOTH, START_YEAR=START_YEAR, END_YEAR=END_YEAR, OUTER_MAG=OUTER_MAG, NO_SMOOTH=NO_SMOOTH

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_year) eq 1) then start_year = long(start_year)  else message, 'no start_year'
if (n_elements(end_year)   eq 1) then end_year   = long(end_year)    else end_year = start_year
if keyword_set(outer_mag) then begin
   outer_mag        = 1 
   acalib_prefix    = acalib_prefix_ou
endif else begin
   outer_mag        = 0
   acalib_prefix    = acalib_prefix_in 
endelse

;- read acalib in the interval [start_year, end_year]
n_years    = end_year - start_year + 1
acalib_arr = replicate({acalib_struc}, n_years*366L)
sub_acalib = 0
for i_y=start_year, end_year do begin
   ;- number of days in current year
   cdf_epoch, epoch_cur_year,  i_y,   /compute_epoch
   cdf_epoch, epoch_next_year, i_y+1, /compute_epoch
   n_days = round((epoch_next_year-epoch_cur_year)/MSDAY)
   ;- read file
   files = file_search(acalib_path, acalib_prefix + '1' + mfi_prefix + string(i_y,format='(I4)') + $
                       '_v01.dat', count=n_all_files)
   acalib_filename = (files[sort(files)])[n_all_files-1]
   acalib_arr_temp = replicate({acalib_struc}, n_days)
   openr, lun, acalib_filename, /get_lun
   readf, lun, acalib_arr_temp
   free_lun, lun
   acalib_arr[sub_acalib:sub_acalib+n_days-1L] = acalib_arr_temp
   sub_acalib += n_days
endfor
acalib_arr = acalib_arr[0L:sub_acalib-1L]
n_acalib   = sub_acalib

;- set flag to 0 for all points
acalib_arr.flag = 0

;- smooth angles
if keyword_set(no_smooth) eq 0 then begin
   if n_acalib gt 50 then begin
      acalib_arr.thetax = smooth(acalib_arr.thetax, 50, /edge_truncate, /nan)
      acalib_arr.thetay = smooth(acalib_arr.thetay, 50, /edge_truncate, /nan)
      acalib_arr.thetaz = smooth(acalib_arr.thetaz, 50, /edge_truncate, /nan)
      acalib_arr.phix   = smooth(acalib_arr.phix, 50, /edge_truncate, /nan)
      acalib_arr.phiy   = smooth(acalib_arr.phiy, 50, /edge_truncate, /nan)
      acalib_arr.phiz   = smooth(acalib_arr.phiz, 50, /edge_truncate, /nan)
   endif else begin
      acalib_arr.thetax = mean(acalib_arr.thetax, 50, /nan)
      acalib_arr.thetay = mean(acalib_arr.thetay, 50, /nan)
      acalib_arr.thetaz = mean(acalib_arr.thetaz, 50, /nan)
      acalib_arr.phix   = mean(acalib_arr.phix,   50, /nan)
      acalib_arr.phiy   = mean(acalib_arr.phiy,   50, /nan)
      acalib_arr.phiz   = mean(acalib_arr.phiz,   50, /nan)
   endelse
endif

;- save smoothed angles to v02 files
for i_y=start_year, end_year do begin
   filename     = acalib_path + acalib_prefix+'1' + mfi_prefix + string(i_y,format='(I4)') + '_v02.dat'
   sub_cur_year = where(floor(acalib_arr.date/1d4) eq i_y, n_cur_year)
   if (n_cur_year eq 0) then stop
   openw, lun, filename, /get_lun 
   printf, lun, acalib_arr[sub_cur_year], format='(I8, 2X, I3, 2X, I1, 6F11.5)'
   free_lun, lun
endfor

END