PRO MFI_CALIB_ANGLES_LIST, START_YEAR=START_YEAR, END_YEAR=END_YEAR, OUTER_MAG=OUTER_MAG

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

;- for each year from start_year to end_year make angles file
for i_y=start_year, end_year do begin
      ;- compute angles
      angles_result  = mfi_calib_angles(start_date=long(i_y*1d4 +0101), end_date=long(i_y*1d4 +1231), $
                                        range=1, outer_mag=outer_mag, angles=angles)

      ;- number of days in current year
      cdf_epoch, epoch_cur_year,  i_y,   /compute_epoch
      cdf_epoch, epoch_next_year, i_y+1, /compute_epoch
      n_days = round((epoch_next_year-epoch_cur_year)/MSDAY)

      ;- make array of angles for current year
      acalib_arr = replicate({acalib_struc}, n_days)
      for i_d=0, n_days-1 do begin
         cdf_epoch, epoch_cur_year + MSDAY*i_d, year, month, day, /breakdown_epoch
         acalib_arr[i_d].date   = year*1d4 + month*1d2 + day
         acalib_arr[i_d].doy    = i_d + 1
         if angles_result eq 0 then sub_cur=(where(angles.doy eq i_d+1, n_cur))[0] else n_cur=0
         if n_cur gt 0 then begin
            acalib_arr[i_d].flag   = 0
            acalib_arr[i_d].thetax = angles[sub_cur].thetax
            acalib_arr[i_d].thetay = angles[sub_cur].thetay
            acalib_arr[i_d].thetaz = angles[sub_cur].thetaz
            acalib_arr[i_d].phix   = angles[sub_cur].phix
            acalib_arr[i_d].phiy   = angles[sub_cur].phiy
            acalib_arr[i_d].phiz   = angles[sub_cur].phiz
         endif else begin
            acalib_arr[i_d].flag   = 1
            acalib_arr[i_d].thetax = cal[outer_mag].theta_bfit[0]
            acalib_arr[i_d].thetay = cal[outer_mag].theta_bfit[1]
            acalib_arr[i_d].thetaz = cal[outer_mag].theta_bfit[2]
            acalib_arr[i_d].phix   = cal[outer_mag].phi_bfit[0]
            acalib_arr[i_d].phiy   = cal[outer_mag].phi_bfit[1]
            acalib_arr[i_d].phiz   = cal[outer_mag].phi_bfit[2]
         endelse
      endfor
      
      ;- change angles to degrees
      acalib_arr.thetax *=!radeg 
      acalib_arr.thetay *=!radeg
      acalib_arr.thetaz *=!radeg
      acalib_arr.phix   *=!radeg
      acalib_arr.phiy   *=!radeg
      acalib_arr.phiz   *=!radeg
                              
      ;- save zeroz array to file
      filename = acalib_path + acalib_prefix+'1' + mfi_prefix + string(i_y,format='(I4)') + '_v01.dat'
      openw, lun, filename, /get_lun 
      printf, lun, acalib_arr, format='(I8, 2X, I3, 2X, I1, 6F11.5)'
      free_lun, lun
endfor

END
