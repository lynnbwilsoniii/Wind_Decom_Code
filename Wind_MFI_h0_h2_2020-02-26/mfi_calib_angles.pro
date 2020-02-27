FUNCTION MFI_CALIB_ANGLES, START_DATE=START_DATE, END_DATE=END_DATE, RANGE=RANGE, OUTER_MAG=OUTER_MAG, $
                           ANGLES=ANGLES

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- read arguments
if (n_elements(start_date)  eq 1) then start_date = long(start_date) else message, 'no start_date'
if (n_elements(end_date)    eq 1) then end_date   = long(end_date)   else end_date = start_date
if (n_elements(range)       eq 1) then range      = byte(range)      else range = 1b
if keyword_set(outer_mag) then begin
   outer_mag      = 1
   spinfit_prefix = spinfit_prefix_ou
   acalib_prefix  = acalib_prefix_ou
endif else begin
   outer_mag      = 0
   spinfit_prefix = spinfit_prefix_in
   acalib_prefix  = acalib_prefix_in
endelse

;- make structure array to store calibration constants per day
n_years    = floor(end_date/1d4) - floor(start_date/1d4) + 1
angles     = replicate({year:0, doy:0, range:-1, thetax:0d, phix:0d, thetay:0d, phiy:0d, $
                        thetaz:0d, phiz:0d, n:0d, q:1d}, n_years*366d)
sub_angles = 0d

;- find files with latest versions for [start_date, end_date] interval and current range
file_mask  = spinfit_prefix + string(range,format='(I1)') + mfi_prefix + '*.sav'
files      = file_search(spinfit_path, file_mask, count=n_files)
   if (n_files eq 0) then return, 1
files      = files[sort(files)]
files_date = strarr(n_files)
for i_f=0, n_files-1 do $
   files_date[i_f]  = strmid(files[i_f], 15, 8, /reverse_offset)
sub_ok_files = where((long(files_date) ge start_date) and (long(files_date) le end_date), n_ok_files) 
   if (n_ok_files eq 0) then return, 1
files   = files[ sub_ok_files[uniq(files_date[sub_ok_files])] ]
n_files = n_elements(files)

;- restore spinfit data from each file and find one value of each angle per day
for i_f=0, n_files-1 do begin
   ;- restore spinfit from file
   restore, files[i_f]
   if keyword_set(outer_mag) then spinfit=temporary(spinfit_ou) else spinfit=temporary(spinfit_in)
   if n_elements(spinfit) lt MIN_N_IN_SLOPINTFIT then continue

   ;--- find angles ----------------------------------------------------------
   ;- sub of good data points
   sub_ok = where((sqrt(spinfit.coefx[1]^2d + spinfit.coefx[2]^2d) gt 1) and $
                  (sqrt(spinfit.coefx[1]^2d + spinfit.coefx[2]^2d) gt 100d*spinfit.coefx[3]) and $
                  (sqrt(spinfit.coefy[1]^2d + spinfit.coefy[2]^2d) gt 1) and $
                  (sqrt(spinfit.coefy[1]^2d + spinfit.coefy[2]^2d) gt 100d*spinfit.coefy[3]) and $
                  (sqrt(spinfit.coefz[1]^2d + spinfit.coefz[2]^2d) gt 1) and $
                  (sqrt(spinfit.coefz[1]^2d + spinfit.coefz[2]^2d) gt 10d*spinfit.coefz[3]), n_ok)
   if (n_ok lt MIN_N_IN_SLOPINTFIT) then continue
   spinfit = spinfit[sub_ok]
   
   ;- arrays of slopes
   slopex_arr = (spinfit[1:n_ok-1d].coefx[0]-spinfit[0:n_ok-2d].coefx[0])/$
                (spinfit[1:n_ok-1d].coefz[0]-spinfit[0:n_ok-2d].coefz[0])
   slopey_arr = (spinfit[1:n_ok-1d].coefy[0]-spinfit[0:n_ok-2d].coefy[0])/$
                (spinfit[1:n_ok-1d].coefz[0]-spinfit[0:n_ok-2d].coefz[0])
   
   ;- additional variables
   ax_sq_arr = (spinfit[0:n_ok-2].coefx[1]^2d + spinfit[0:n_ok-2].coefx[2]^2d)
   ay_sq_arr = (spinfit[0:n_ok-2].coefy[1]^2d + spinfit[0:n_ok-2].coefy[2]^2d)
   az_sq_arr = (spinfit[0:n_ok-2].coefz[1]^2d + spinfit[0:n_ok-2].coefz[2]^2d)
   
   ;- sensitivities
   sensx_ratio_arr = sqrt( (cal[outer_mag].sens[range].y*sqrt(ay_sq_arr))/$
                           (cal[outer_mag].sens[range].x*sqrt(ax_sq_arr)) )
   sensy_ratio_arr = 1d/sensx_ratio_arr
   sensx_arr  = sensx_ratio_arr*cal[outer_mag].sens[range].x
   sensy_arr  = sensy_ratio_arr*cal[outer_mag].sens[range].y
   
   ;- thetax(y,z)
   sin_thetax_arr = slopex_arr*sensx_arr/cal[outer_mag].sens[range].z
   sin_thetay_arr = slopey_arr*sensy_arr/cal[outer_mag].sens[range].z
   sin_thetaz_arr = (sqrt(az_sq_arr)*cal[outer_mag].sens[range].z/(sqrt(ax_sq_arr)*sensx_arr) + $
                     sqrt(az_sq_arr)*cal[outer_mag].sens[range].z/(sqrt(ay_sq_arr)*sensy_arr))/2d

   ;- phipsix
   cos_phipsix_arr =  spinfit[0:n_ok-2].coefx[1]/sqrt(ax_sq_arr)
   sin_phipsix_arr = -spinfit[0:n_ok-2].coefx[2]/sqrt(ax_sq_arr)
   phipsix_arr     = asin(sin_phipsix_arr)
   phipsix_arr    += (cos_phipsix_arr lt 0)*(-2d*phipsix_arr + !dpi)
   
   ;- phipsiy   
   cos_phipsiy_arr = -spinfit[0:n_ok-2].coefy[2]/sqrt(ay_sq_arr)
   sin_phipsiy_arr = -spinfit[0:n_ok-2].coefy[1]/sqrt(ay_sq_arr)
   phipsiy_arr     = asin(sin_phipsiy_arr)
   phipsiy_arr    += (cos_phipsiy_arr lt 0)*(-2d*phipsiy_arr + !dpi)
   
   ;- phipsiz   
   cos_phipsiz_arr =  spinfit[0:n_ok-2].coefz[1]/sqrt(az_sq_arr)
   sin_phipsiz_arr = -spinfit[0:n_ok-2].coefz[2]/sqrt(az_sq_arr)
   phipsiz_arr     = asin(sin_phipsiz_arr)
   phipsiz_arr    += (cos_phipsiz_arr lt 0)*(-2d*phipsiz_arr + !dpi)
   
   ;- phiyx(zx)
   phiyx_arr  = phipsiy_arr - phipsix_arr
   phiyx_arr += (phiyx_arr gt  !dpi)*(-2d*!dpi) + (phiyx_arr lt -!dpi)*(2d*!dpi)
   phizx_arr  = phipsiz_arr - phipsix_arr
   phizx_arr += (phizx_arr gt  !dpi)*(-2d*!dpi) + (phizx_arr lt -!dpi)*(2d*!dpi)

   ;- single values of angles
   angles[sub_angles].thetax = asin(median(sin_thetax_arr, /double, /even))
   angles[sub_angles].thetay = asin(median(sin_thetay_arr, /double, /even))
   angles[sub_angles].thetaz = asin(median(sin_thetaz_arr, /double, /even))
   angles[sub_angles].phix   = cal[outer_mag].phi[0]
   angles[sub_angles].phiy   = angles[sub_angles].phix + median(phiyx_arr, /even)
   angles[sub_angles].phiy  += (angles[sub_angles].phiy gt  !dpi)*(-2d*!dpi) + $
                               (angles[sub_angles].phiy lt -!dpi)*( 2d*!dpi)
   angles[sub_angles].phiz   = angles[sub_angles].phix + median(phizx_arr, /even)
   angles[sub_angles].phiz  += (angles[sub_angles].phiz gt  !dpi)*(-2d*!dpi) + $
                               (angles[sub_angles].phiz lt -!dpi)*( 2d*!dpi)

   ;- save year, doy, range, n_ok, set quality to 0
   year  = double(strmid(files[i_f], 15, 4, /reverse_offset))
   month = double(strmid(files[i_f], 11, 2, /reverse_offset))
   day   = double(strmid(files[i_f],  9, 2, /reverse_offset))
   cdf_epoch, epoch_cur_year, year, 1, 1, /compute_epoch
   cdf_epoch, epoch_cur_date, year, month, day, /compute_epoch
   doy = (epoch_cur_date - epoch_cur_year)/double(MSDAY) + 1d
   ;- save
   angles[sub_angles].year = year
   angles[sub_angles].doy  = doy
   angles[sub_angles].range= range   
   angles[sub_angles].n    = n_ok
   angles[sub_angles].q    = 0
   
   ;- increase sub_angles
   sub_angles += 1d
   ;--- find angles ----------------------------------------------------------
endfor

;- truncate and return angles
if sub_angles gt 0 then begin
   angles = angles[0:sub_angles-1]
   return, 0
endif else begin
   temp = temporary(angles)
   return, 1
endelse

END