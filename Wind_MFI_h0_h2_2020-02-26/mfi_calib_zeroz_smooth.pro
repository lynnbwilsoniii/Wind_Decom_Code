PRO MFI_CALIB_ZEROZ_SMOOTH, START_YEAR=START_YEAR, END_YEAR=END_YEAR, RANGE=RANGE, $
                            OUTER_MAG=OUTER_MAG, NO_SMOOTH=NO_SMOOTH

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_year) eq 1) then start_year = long(start_year)  else message, 'no start_year'
if (n_elements(end_year)   eq 1) then end_year   = long(end_year)    else end_year = start_year
if (n_elements(range)      eq 1) then range      = byte(range)       else range    = 1b
if keyword_set(outer_mag) then begin
   outer_mag        = 1 
   zeroz_prefix     = zeroz_prefix_ou
   zeroz_llimit  = 1600d
endif else begin
   outer_mag        = 0
   zeroz_prefix     = zeroz_prefix_in 
   zeroz_llimit  = 1850d
endelse

;- read zeroz in the interval [start_year, end_year]
n_years   = end_year - start_year + 1
zeroz_arr = replicate({zeroz_struc}, n_years*366L)
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
   zeroz_arr_temp = replicate({zeroz_struc}, n_days)
   openr, lun, zeroz_filename, /get_lun
   readf, lun, zeroz_arr_temp
   free_lun, lun
   zeroz_arr[sub_zeroz:sub_zeroz+n_days-1L] = zeroz_arr_temp
   sub_zeroz += n_days
endfor
zeroz_arr = zeroz_arr[0L:sub_zeroz-1L]

;- replace zeroz with flag eq 1 by linear interpolation of nearest good points, set flag to 2
sub_bad = where((zeroz_arr.flag eq 1) or (zeroz_arr.zeroz lt zeroz_llimit), n_bad, complement=sub_ok, $
                ncomplement=n_ok)
   if n_ok eq 0 then return
if n_bad gt 0 then begin
   for i_b=0, n_bad-1 do begin
      if sub_bad[i_b] lt sub_ok[0] then begin
         zeroz_arr[sub_bad[i_b]].zeroz = zeroz_arr[sub_ok[0]].zeroz
         zeroz_arr[sub_bad[i_b]].flag  = 2b
         continue
      endif
      if sub_bad[i_b] gt sub_ok[n_ok-1] then begin
         zeroz_arr[sub_bad[i_b]].zeroz = zeroz_arr[sub_ok[n_ok-1]].zeroz
         zeroz_arr[sub_bad[i_b]].flag  = 2b
         continue
      endif
      zeroz_arr[sub_bad[i_b]].zeroz = interpol(zeroz_arr[sub_ok].zeroz, sub_ok, sub_bad[i_b])
      zeroz_arr[sub_bad[i_b]].flag  = 2b
   endfor
endif

;- smooth zeroz
if keyword_set(no_smooth) eq 0 then zeroz_arr.zeroz=smooth(zeroz_arr.zeroz, 20, /edge_truncate)

;- save smooth zeroz to files
for i_y=start_year, end_year do begin
   filename = zeroz_path+zeroz_prefix+string(range, format='(I1)')+mfi_prefix+string(i_y,format='(I4)')+'_v02.dat'
   sub_cur_year = where(floor(zeroz_arr.date/1d4) eq i_y, n_cur_year)
   if (n_cur_year eq 0) then continue
   openw, lun, filename, /get_lun 
   printf, lun, zeroz_arr[sub_cur_year], format='(I8, 2X, I3, 2X, I1, 2X, F6.1)'
   free_lun, lun
endfor

END