FUNCTION MFI_PAY_TO_GSE_GSM, EPOCH=EPOCH, PAY=PAY, GSE=GSE, GSM=GSM, AT_GSE=AT_GSE, AT_GSM=AT_GSM, $
                             AT_FILES=AT_FILES, OR_FILES=OR_FILES

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- number of data points
n_data = n_elements(epoch)
if n_data ne n_elements(pay) then message, 'epoch and pay must be of the same size'

;- create gse and gsm data and attitude variables
gse        = replicate({vect_q_struc}, n_data)
gse.q      = 1b
gsm        = replicate({vect_q_struc}, n_data)
gsm.q      = 1b
gse_r_ascn = dblarr(n_data)
gse_decl   = dblarr(n_data)
gsm_r_ascn = dblarr(n_data)
gsm_decl   = dblarr(n_data)

;--- find dates covered by epoch, include previous and next date
;- int_start_epoch_ and int_end_epoch
int_start_epoch_ = epoch[0] - MSDAY
int_end_epoch    = epoch[n_data-1L]

;- breakdown int_start_epoch_ and int_end_epoch
cdf_epoch, int_start_epoch_, int_start_year_, int_start_month_, int_start_day_, int_start_hour_, $
                             int_start_minut_, int_start_sec_, int_start_milli_, /breakdown_epoch
cdf_epoch, int_end_epoch, int_end_year, int_end_month, int_end_day, int_end_hour, $
                          int_end_minut, int_end_sec, int_end_milli, /breakdown_epoch

;- int_start_date_str_ and int_end_date_str
int_start_date_str_ = string(int_start_year_,format='(I4)') + string(int_start_month_,format='(I02)') + $
                      string(int_start_day_,format='(I02)')
int_end_date_str = string(int_end_year,format='(I4)') + string(int_end_month,format='(I02)') + $
                   string(int_end_day,format='(I02)')

;- find dates
date_str_arr = [int_start_date_str_]
epoch_temp   = int_start_epoch_
repeat begin
   epoch_temp += MSDAY
   cdf_epoch, epoch_temp, yr, mo, dy, hr, mn, sc, msc, /breakdown_epoch
   date_str_temp = string(yr,format='(I4)') + string(mo,format='(I02)') + string(dy,format='(I02)')
   date_str_arr = [date_str_arr, date_str_temp]
endrep until (date_str_temp gt int_end_date_str)
;--- end: find dates covered by epoch, include previous and next date

;- create arrays to store attitude
n_date        = n_elements(date_str_arr)
at_gse_epoch  = dblarr(n_date*1d3)
at_gse_r_ascn = dblarr(n_date*1d3)
at_gse_decl   = dblarr(n_date*1d3)
at_gse_q      = replicate(1b, n_date*2d2)
at_gse_sub    = 0L
at_gsm_epoch  = dblarr(n_date*1d3)
at_gsm_r_ascn = dblarr(n_date*1d3)
at_gsm_decl   = dblarr(n_date*1d3)
at_gsm_q      = replicate(1b, n_date*2d2)
at_gsm_sub    = 0L
at_files      = strarr(n_date)
or_files      = strarr(n_date)

;- read files and fill arrays
for i_f=0, n_date-1 do begin
   ;- read attitude file
   read_def_at_res = mfi_read_at(at_path=def_at_path, at_prefix=def_at_prefix+date_str_arr[i_f], $
                                 or_path=def_or_path, or_prefix=def_or_prefix+date_str_arr[i_f], $
                                 attitude=def_attit, at_file=def_at_file, or_file=def_or_file)
   read_pre_at_res = mfi_read_at(at_path=pre_at_path, at_prefix=pre_at_prefix+date_str_arr[i_f], $
                                 or_path=pre_or_path, or_prefix=pre_or_prefix+date_str_arr[i_f], $
                                 attitude=pre_attit, at_file=pre_at_file, or_file=pre_or_file)
   if (read_def_at_res eq 1) and (read_pre_at_res eq 1) then continue
   if (read_def_at_res eq 0) or ((read_def_at_res eq 2) and (read_pre_at_res ne 0)) then begin
      read_at_res = read_def_at_res
      attit       = def_attit
      at_file     = def_at_file
      or_file     = def_or_file
   endif else begin
      read_at_res = read_pre_at_res
      attit       = pre_attit
      at_file     = pre_at_file
      or_file     = pre_or_file
   endelse
   
   ;- fill at_files and or_files
   at_files[i_f] = at_file
   or_files[i_f] = or_file

   ;- decompose gse attit and fill arrays
   at_qgse  = (attit.epoch.q eq 1) or (attit.gse_r_ascn.q eq 1) or (attit.gse_decl.q eq 1)
   temp_sub = where(at_qgse eq 0, n_at_gse)
   if (n_at_gse gt 0) then begin
      if (i_f eq 0)          then begin &  temp_sub = temp_sub[n_at_gse-1L] &  n_at_gse=1 &  endif
      if (i_f eq (n_date-1)) then begin &  temp_sub = temp_sub[0]           &  n_at_gse=1 &  endif
      at_gse_epoch[at_gse_sub:at_gse_sub+n_at_gse-1L]  = attit.epoch.value[temp_sub]
      at_gse_r_ascn[at_gse_sub:at_gse_sub+n_at_gse-1L] = attit.gse_r_ascn.value[temp_sub]
      at_gse_decl[at_gse_sub:at_gse_sub+n_at_gse-1L]   = attit.gse_decl.value[temp_sub]
      at_gse_q[at_gse_sub:at_gse_sub+n_at_gse-1L]      = attit.gse_r_ascn.q[temp_sub]
      at_gse_sub += n_at_gse
   endif

   ;- decompose gsm attit and fill arrays
   at_qgsm  = (attit.epoch.q eq 1) or (attit.gsm_r_ascn.q eq 1) or (attit.gsm_decl.q eq 1)
   temp_sub = where(at_qgsm eq 0, n_at_gsm)
   if (n_at_gsm gt 0) then begin
      if (i_f eq 0)          then begin &  temp_sub = temp_sub[n_at_gsm-1L] &  n_at_gsm=1 &  endif
      if (i_f eq (n_date-1)) then begin &  temp_sub = temp_sub[0]           &  n_at_gsm=1 &  endif
      at_gsm_epoch[at_gsm_sub:at_gsm_sub+n_at_gsm-1L]  = attit.epoch.value[temp_sub]
      at_gsm_r_ascn[at_gsm_sub:at_gsm_sub+n_at_gsm-1L] = attit.gsm_r_ascn.value[temp_sub]
      at_gsm_decl[at_gsm_sub:at_gsm_sub+n_at_gsm-1L]   = attit.gsm_decl.value[temp_sub]
      at_gsm_q[at_gsm_sub:at_gsm_sub+n_at_gsm-1L]      = attit.gsm_r_ascn.q[temp_sub]
      at_gsm_sub += n_at_gsm
   endif
endfor

;- find subscripts of gse attitude with quality 0 (corrected) and 2 (original) and gsm attitude with quality 0 
sub_q0_at_gse = where(at_gse_q eq 0b, n_q0_at_gse)
sub_q2_at_gse = where(at_gse_q eq 2b, n_q2_at_gse)
sub_q0_at_gsm = where(at_gsm_q eq 0b, n_q0_at_gsm)
if (n_q0_at_gse lt 2) and (n_q2_at_gse lt 2) and (n_q0_at_gsm lt 143) then return, 1

;--- interpolate at_gse (with quality 0, if possible, or quality 2) to epoch
if (n_q0_at_gse ge 2) or (n_q2_at_gse ge 2) then begin
   ;- select attitude with qulity 0, if at least two points present, or quality 2, propagate quality to gse.q
   if (n_q0_at_gse ge 2) then begin
      at_gse_epoch  = at_gse_epoch[sub_q0_at_gse]
      at_gse_r_ascn = at_gse_r_ascn[sub_q0_at_gse]
      at_gse_decl   = at_gse_decl[sub_q0_at_gse]
      gse[*].q      = 0b
   endif else begin
      at_gse_epoch  = at_gse_epoch[sub_q2_at_gse]
      at_gse_r_ascn = at_gse_r_ascn[sub_q2_at_gse]
      at_gse_decl   = at_gse_decl[sub_q2_at_gse]
      gse[*].q      = 2b
   endelse
   
   ;- at_gse in cartesian cordinates
   at_xgse = cos(at_gse_decl)*cos(at_gse_r_ascn)
   at_ygse = cos(at_gse_decl)*sin(at_gse_r_ascn)
   at_zgse = sin(at_gse_decl)

   ;- zpay in gse
   zpay_xgse  = interpol(at_xgse, at_gse_epoch, epoch)
   zpay_ygse  = interpol(at_ygse, at_gse_epoch, epoch)
   zpay_zgse  = interpol(at_zgse, at_gse_epoch, epoch)
   at_gse   = replicate({vect_struc}, n_elements(zpay_xgse))
   at_gse.x = zpay_xgse
   at_gse.y = zpay_ygse
   at_gse.z = zpay_zgse
   
   ;- define temporal basis with ztem=zpay and xtem_ygse=0
   ztem_xgse = zpay_xgse
   ztem_ygse = zpay_ygse
   ztem_zgse = zpay_zgse
   xtem_xgse = sqrt(ztem_zgse^2d/(1d - ztem_ygse^2d))
   xtem_ygse = 0d
   xtem_zgse = -ztem_xgse/ztem_zgse*xtem_xgse
   ytem_xgse = ztem_ygse*xtem_zgse - ztem_zgse*xtem_ygse
   ytem_ygse = ztem_zgse*xtem_xgse - ztem_xgse*xtem_zgse
   ytem_zgse = ztem_xgse*xtem_ygse - ztem_ygse*xtem_xgse

   ;- take projec. of xgse onto temp_xy, which is paralel with xpay_tem
   temp_length = sqrt(xtem_xgse^2d + ytem_xgse^2d)
   xpay_xtem = xtem_xgse/temp_length  ; and adjust to unit length
   xpay_ytem = ytem_xgse/temp_length  ; and adjust to unit length
   xpay_ztem = 0d

   ;- transf xpay to gse
   xpay_xgse = xtem_xgse*xpay_xtem + ytem_xgse*xpay_ytem + ztem_xgse*xpay_ztem
   xpay_ygse = xtem_ygse*xpay_xtem + ytem_ygse*xpay_ytem + ztem_ygse*xpay_ztem
   xpay_zgse = xtem_zgse*xpay_xtem + ytem_zgse*xpay_ytem + ztem_zgse*xpay_ztem

   ;- find ypay_gse
   ypay_xgse = zpay_ygse*xpay_zgse - zpay_zgse*xpay_ygse
   ypay_ygse = zpay_zgse*xpay_xgse - zpay_xgse*xpay_zgse
   ypay_zgse = zpay_xgse*xpay_ygse - zpay_ygse*xpay_xgse

   ;- transform to gse
   gse[*].x = xpay_xgse*pay.x + ypay_xgse*pay.y + zpay_xgse*pay.z
   gse[*].y = xpay_ygse*pay.x + ypay_ygse*pay.y + zpay_ygse*pay.z
   gse[*].z = xpay_zgse*pay.x + ypay_zgse*pay.y + zpay_zgse*pay.z
endif
;--- end: interpolate at_gse (with quality 0, if possible, or quality 2) to epoch

;--- interpolate at_gsm with quality 0 to epoch
if (n_q0_at_gsm ge 143) then begin
   ;- select attitude with qulity 0
   at_gsm_epoch  = at_gsm_epoch[sub_q0_at_gsm]
   at_gsm_r_ascn = at_gsm_r_ascn[sub_q0_at_gsm]
   at_gsm_decl   = at_gsm_decl[sub_q0_at_gsm]
   gsm.q[*]      = 0b
   
   ;- at_gsm in cartesian cordinates
   at_xgsm = cos(at_gsm_decl)*cos(at_gsm_r_ascn)
   at_ygsm = cos(at_gsm_decl)*sin(at_gsm_r_ascn)
   at_zgsm = sin(at_gsm_decl)
   
   ;- zpay in gsm
   zpay_xgsm = spline(at_gsm_epoch, at_xgsm, epoch)
   zpay_ygsm = spline(at_gsm_epoch, at_ygsm, epoch)
   zpay_zgsm = spline(at_gsm_epoch, at_zgsm, epoch)
   at_gsm   = replicate({vect_struc}, n_elements(zpay_xgsm))
   at_gsm.x = zpay_xgsm
   at_gsm.y = zpay_ygsm
   at_gsm.z = zpay_zgsm
   
   ;- define temporal basis with ztem=zpay and xtem_ygsm=0
   ztem_xgsm = zpay_xgsm
   ztem_ygsm = zpay_ygsm
   ztem_zgsm = zpay_zgsm
   xtem_xgsm = sqrt(ztem_zgsm^2d/(1d - ztem_ygsm^2d))
   xtem_ygsm = 0d
   xtem_zgsm = -ztem_xgsm/ztem_zgsm*xtem_xgsm
   ytem_xgsm = ztem_ygsm*xtem_zgsm - ztem_zgsm*xtem_ygsm
   ytem_ygsm = ztem_zgsm*xtem_xgsm - ztem_xgsm*xtem_zgsm
   ytem_zgsm = ztem_xgsm*xtem_ygsm - ztem_ygsm*xtem_xgsm

   ;- take projec. of xgsm onto temp_xy, which is paralel with xpay_tem
   temp_length = sqrt(xtem_xgsm^2d + ytem_xgsm^2d)
   xpay_xtem = xtem_xgsm/temp_length  ; and adjust to unit length
   xpay_ytem = ytem_xgsm/temp_length  ; and adjust to unit length
   xpay_ztem = 0d

   ;- transf xpay to gsm
   xpay_xgsm = xtem_xgsm*xpay_xtem + ytem_xgsm*xpay_ytem + ztem_xgsm*xpay_ztem
   xpay_ygsm = xtem_ygsm*xpay_xtem + ytem_ygsm*xpay_ytem + ztem_ygsm*xpay_ztem
   xpay_zgsm = xtem_zgsm*xpay_xtem + ytem_zgsm*xpay_ytem + ztem_zgsm*xpay_ztem

   ;- find ypay_gsm
   ypay_xgsm = zpay_ygsm*xpay_zgsm - zpay_zgsm*xpay_ygsm
   ypay_ygsm = zpay_zgsm*xpay_xgsm - zpay_xgsm*xpay_zgsm
   ypay_zgsm = zpay_xgsm*xpay_ygsm - zpay_ygsm*xpay_xgsm

   ;- transform to gsm
   gsm[*].x = xpay_xgsm*pay.x + ypay_xgsm*pay.y + zpay_xgsm*pay.z
   gsm[*].y = xpay_ygsm*pay.x + ypay_ygsm*pay.y + zpay_ygsm*pay.z
   gsm[*].z = xpay_zgsm*pay.x + ypay_zgsm*pay.y + zpay_zgsm*pay.z
endif
;--- end: interpolate at_gsm with quality 0 to epoch

;- return 0, which means that either gse or gsm data are available
return, 0

END