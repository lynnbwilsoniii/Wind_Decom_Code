PRO MFI_CALIB_ZEROZ_LIST, YEAR=YEAR, OUTER_MAG=OUTER_MAG

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(year) eq 1) then year = long(year)  else message, 'no year'
if keyword_set(outer_mag)  then begin
   outer_mag         = 1 
   bz_offset_prefix  = bz_offset_prefix_ou
   calib_orig_prefix = calib_orig_prefix_ou
   zeroz_prefix      = zeroz_prefix_ou
   zero_var_str      = 'ZERO_O'
   inner_outer_prefix= 'o_'
endif else begin
   outer_mag         = 0
   bz_offset_prefix  = bz_offset_prefix_in
   calib_orig_prefix = calib_orig_prefix_in
   zeroz_prefix      = zeroz_prefix_in
   zero_var_str      = 'ZERO_I'
   inner_outer_prefix= 'i_'
endelse

;- read bzi/o_offset_mfi_yyyy_v01.dat file
files = file_search(bz_offset_path, bz_offset_prefix + string(year,format='(I4)') + '_v01.dat', count=n_files)
if n_files eq 1 then begin
   ;- make array to store data
   bz_offset_arr = replicate({bz_offset_struc}, 1d3)
   rec_temp      = {bz_offset_struc}
   sub_rec       = 0

   ;- open, read, close file
   filename_latest = (files[sort(files)])[n_files-1]
   openr, lun, filename_latest, /get_lun
   while eof(lun) eq 0 do begin
      readf, lun, rec_temp
      bz_offset_arr[sub_rec] = rec_temp
      sub_rec +=1
   endwhile
   bz_offset_arr = bz_offset_arr[0:sub_rec-1]
   free_lun, lun
   
   ;- fix ver
   n_temp = n_elements(bz_offset_arr.ver)
   for i_t=0, n_temp-1 do bz_offset_arr[i_t].ver=(strsplit(bz_offset_arr[i_t].ver, /extract))[0]

   ;- leave only good quality records
   sub_ok = where(((bz_offset_arr.ver eq 'v3') or (bz_offset_arr.ver eq 'cb4') or $
                   (bz_offset_arr.ver eq 'k3') or (bz_offset_arr.ver eq 'k4')), n_bz_offset_ok)
   if n_bz_offset_ok gt 0 then begin
      bz_offset_arr = bz_offset_arr[sub_ok]
      bz_offset_arr = bz_offset_arr[uniq(bz_offset_arr.doy, sort(bz_offset_arr.doy))]
   endif
endif else begin
   n_bz_offset_ok = 0
endelse

;- number of days in current year
cdf_epoch, epoch_cur_year,  year,   /compute_epoch
cdf_epoch, epoch_next_year, year+1, /compute_epoch
n_days_year = round((epoch_next_year-epoch_cur_year)/MSDAY)

;- read array of zeroz from wi_zzi1_mfi_yyyy_v01.dat file or make array of default zeroz if file does not exit
files     = file_search(zeroz_path, zeroz_prefix+'1'+mfi_prefix+string(year,format='(I4)')+'_v01.dat', count=n_files)
zeroz_arr = replicate({zeroz_struc}, n_days_year)
if n_files eq 1 then begin
   filename_latest = (files[sort(files)])[n_files-1]
   openr, lun, filename_latest, /get_lun
   readf, lun, zeroz_arr
   free_lun, lun
endif else begin
   for i_d=0, n_days_year-1 do begin
      cdf_epoch, epoch_cur_year + MSDAY*i_d, year, month, day, /breakdown_epoch
      zeroz_arr[i_d].date  = long(year*1d4 + month*1d2 + day)
      zeroz_arr[i_d].doy   = i_d + 1
      zeroz_arr[i_d].flag  = 1b
      zeroz_arr[i_d].zeroz = cal[outer_mag].zero[1].z
   endfor
endelse

;- update zeroz_arr with values from bz_offset_arr; take into account 0.05nT shift due to intercalibration with ACE
for i_bz=0, n_bz_offset_ok-1 do begin
   sub_zeroz = where(zeroz_arr.doy eq bz_offset_arr[i_bz].doy, n_zeroz)
   if n_zeroz eq 1 then begin
      ;- find orig_cdf_file
      case bz_offset_arr[i_bz].ver of
         'v3'  : orig_cdf_file = h0_original_path + 'v3/'+ h0_prefix + mfi_prefix + $
                                 string(zeroz_arr[sub_zeroz].date,format='(I8)') + '_v03.cdf'
         'cb4' : orig_cdf_file = h0_original_path + 'combined/'+ h0_prefix + mfi_prefix + $
                                 string(zeroz_arr[sub_zeroz].date,format='(I8)') + '_v04.cdf'
         'k3'  : orig_cdf_file = h0_original_path + 'k3_k4/'+ h0_prefix + mfi_prefix + inner_outer_prefix + $
                                 string(zeroz_arr[sub_zeroz].date,format='(I8)') + '_v03.cdf'
         'k4'  : orig_cdf_file = h0_original_path + 'k3_k4/'+ h0_prefix + mfi_prefix + inner_outer_prefix + $
                                 string(zeroz_arr[sub_zeroz].date,format='(I8)') + '_v04.cdf'
          else: stop
      endcase
      
      ;- read cdf file and update zeroz
      cdfid = cdf_open(orig_cdf_file, /readonly)
      cdf_varget, cdfid, zero_var_str, zero_temp
      cdf_close, cdfid
      zeroz_arr[sub_zeroz].zeroz = (bz_offset_arr[i_bz].bz_offset+0.05d)/cal[outer_mag].sens[1].z + zero_temp[2,1]
      zeroz_arr[sub_zeroz].flag  = 0b
   endif else begin
      continue
   endelse
endfor

;- save zeroz_arr
filename = zeroz_path + zeroz_prefix+'1' + mfi_prefix + string(year,format='(I4)') + '_v01.dat'
openw, lun, filename, /get_lun 
printf, lun, zeroz_arr, format='(I8, 2X, I3, 2X, I1, 2X, F6.1)'
free_lun, lun

END