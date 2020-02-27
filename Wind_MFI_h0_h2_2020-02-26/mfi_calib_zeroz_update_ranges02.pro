PRO MFI_CALIB_ZEROZ_UPDATE_RANGES02, START_YEAR=START_YEAR, END_YEAR=END_YEAR, RANGE=RANGE, OUTER_MAG=OUTER_MAG

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_year) eq 1) then start_year = long(start_year)  else message, 'no start_year'
if (n_elements(end_year)   eq 1) then end_year   = long(end_year)    else end_year = start_year
if (range ne 0) and (range ne 2) then return
if keyword_set(outer_mag) then begin
   outer_mag        = 1 
   zeroz_prefix     = zeroz_prefix_ou
endif else begin
   outer_mag        = 0
   zeroz_prefix     = zeroz_prefix_in 
endelse

;- read version 2 range 1 zeroz in the interval [start_year, end_year]
n_years   = end_year - start_year + 1
zeroz_1_arr = replicate({zeroz_struc}, n_years*366L)
sub_zeroz = 0
for i_y=start_year, end_year do begin
   ;- number of days in current year
   cdf_epoch, epoch_cur_year,  i_y,   /compute_epoch
   cdf_epoch, epoch_next_year, i_y+1, /compute_epoch
   n_days = round((epoch_next_year-epoch_cur_year)/MSDAY)
   ;- read file
   files = file_search(zeroz_path, zeroz_prefix + '1' + mfi_prefix + $
                       string(i_y,format='(I4)') + '_v02.dat', count=n_all_files)
   zeroz_filename = (files[sort(files)])[n_all_files-1]
   zeroz_1_arr_temp = replicate({zeroz_struc}, n_days)
   openr, lun, zeroz_filename, /get_lun
   readf, lun, zeroz_1_arr_temp
   free_lun, lun
   zeroz_1_arr[sub_zeroz:sub_zeroz+n_days-1L] = zeroz_1_arr_temp
   sub_zeroz += n_days
endfor
zeroz_1_arr = zeroz_1_arr[0L:sub_zeroz-1L]

;- read version 1 current range zeroz in the interval [start_year, end_year]
n_years   = end_year - start_year + 1
zeroz_cur_arr = replicate({zeroz_struc}, n_years*366L)
sub_zeroz = 0
for i_y=start_year, end_year do begin
   ;- number of days in current year
   cdf_epoch, epoch_cur_year,  i_y,   /compute_epoch
   cdf_epoch, epoch_next_year, i_y+1, /compute_epoch
   n_days = round((epoch_next_year-epoch_cur_year)/MSDAY)
   ;- read file
   files = file_search(zeroz_path, zeroz_prefix + string(range, format='(I1)') + mfi_prefix + $
                       string(i_y,format='(I4)') + '_v01.dat', count=n_all_files)
   zeroz_filename = (files[sort(files)])[n_all_files-1]
   zeroz_cur_arr_temp = replicate({zeroz_struc}, n_days)
   openr, lun, zeroz_filename, /get_lun
   readf, lun, zeroz_cur_arr_temp
   free_lun, lun
   zeroz_cur_arr[sub_zeroz:sub_zeroz+n_days-1L] = zeroz_cur_arr_temp
   sub_zeroz += n_days
endfor
zeroz_cur_arr = zeroz_cur_arr[0L:sub_zeroz-1L]

;- use latest good value of cur range zeroz to update other values
sub_cur_ok   = where(zeroz_cur_arr.flag eq 0, n_cur_ok)
   if n_cur_ok eq 0 then stop
sub_cur_temp = sub_cur_ok[n_cur_ok-1L]
sub_1_temp   = where(zeroz_1_arr.date eq zeroz_cur_arr[sub_cur_temp].date, n_1_temp)
   if n_1_temp eq 0 then stop
db_nt = (zeroz_cur_arr[sub_cur_temp].zeroz - cal[outer_mag].zero[range].z)*cal[outer_mag].sens[range].z - $
        (zeroz_1_arr[sub_1_temp].zeroz     - cal[outer_mag].zero[1].z    )*cal[outer_mag].sens[1].z
zeroz_cur_arr[sub_cur_temp+1L:*].zeroz = ((zeroz_1_arr[sub_1_temp+1L:*].zeroz-cal[outer_mag].zero[1].z)*$
               cal[outer_mag].sens[1].z + db_nt)/cal[outer_mag].sens[range].z + cal[outer_mag].zero[range].z 
zeroz_cur_arr[sub_cur_temp+1L:*].flag  = 2b

;- save updated cur range zeroz back to version 1 files
for i_y=start_year, end_year do begin
   filename = zeroz_path+zeroz_prefix+string(range, format='(I1)')+mfi_prefix+string(i_y,format='(I4)')+'_v01.dat'
   sub_cur_year = where(floor(zeroz_cur_arr.date/1d4) eq i_y, n_cur_year)
   if (n_cur_year eq 0) then continue
   openw, lun, filename, /get_lun 
   printf, lun, zeroz_cur_arr[sub_cur_year], format='(I8, 2X, I3, 2X, I1, 2X, F6.1)'
   free_lun, lun
endfor

END