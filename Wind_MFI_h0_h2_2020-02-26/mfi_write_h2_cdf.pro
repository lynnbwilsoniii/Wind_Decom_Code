PRO MFI_WRITE_H2_CDF, DATE=DATE, VERSION=VERSION, CNT=COUNTS, PAY=PAYLOAD, EPOCH_HR=EPOCH_HR, BGSE_HR=BGSE_HR, $
                      BGSM_HR=BGSM_HR, CALIB_HR=CALIB_HR, SW12_HR=SW12_HR, OUTER_MAG_HR=OUTER_MAG_HR, $
                      LZ_FILES=LZ_FILES, HK_FILES=HK_FILES, AT_FILES=AT_FILES, OR_FILES=OR_FILES

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;--- additional information
;- b magnitude and quality
n_epoch_hr  = n_elements(epoch_hr)
btot_hr     = replicate({value:0d, q:1b}, n_epoch_hr)
sub_bgsm_hr = where(bgsm_hr.q ne 1, n_bgsm_hr_ok)
sub_bgse_hr = where(bgse_hr.q ne 1, n_bgse_hr_ok)
if n_bgse_hr_ok gt 0 then begin
   btot_hr[sub_bgse_hr].value = sqrt(bgse_hr[sub_bgse_hr].x^2d +bgse_hr[sub_bgse_hr].y^2d +bgse_hr[sub_bgse_hr].z^2d)
   btot_hr[sub_bgse_hr].q     = 0b
endif
sub_btot_hr = where(btot_hr.q eq 0, n_btot_hr_ok)

;- date information
year_cur  = floor(date/1d4)
month_cur = floor((date/1d2) mod 1d2)
day_cur   = floor(date mod 1d2)
cdf_epoch, epoch_date_cur, year_cur, month_cur, day_cur, /c
cdf_epoch, epoch_year_cur, year_cur, 1, 1, /c
doy_cur   = floor((epoch_date_cur-epoch_year_cur)/MSDAY) + 1L
epoch_date_plus1day = epoch_date_cur + MSDAY
cdf_epoch, epoch_date_plus1day, year_plus1day, month_plus1day, day_plus1day, /b
cdf_epoch, epoch_year_plus1day, year_plus1day, 1, 1, /c
doy_plus1day = floor((epoch_date_plus1day-epoch_year_plus1day)/MSDAY) + 1L
;--- end: additional information

;- variable fill values
Epoch_fillval     = -1d31
Time_PB5_fillval  = -1L
BF1_fillval       = -1e31
BGSM_fillval      = -1e31
BGSE_fillval      = -1e31
RANGE_fillval     = -1L
SPC_MODE_fillval  = -1L
MAG_MODE_fillval  = -1L
Epoch1_fillval    = -1d31
Time1_PB5_fillval = -1L
NUM1_PTS_O_fillval= -1L
ZERO1_O_fillval   = -1e31
SENS1_O_fillval   = -1e31
AMPL1_O_fillval   = -1e31
ORTH1_O_fillval   = -1d31
PAYLD1_O_fillval  = -1d31
FLAG1_O_fillval   = -1L
NUM1_PTS_I_fillval= -1L
ZERO1_I_fillval   = -1e31
SENS1_I_fillval   = -1e31
AMPL1_I_fillval   = -1e31
ORTH1_I_fillval   = -1d31
PAYLD1_I_fillval  = -1d31
FLAG1_I_fillval   = -1L

;--- high-resolution data ----------------------------------------
;- create variables and fill with fill values
epoch    = replicate(epoch_fillval, 1, n_epoch_hr)
time_PB5 = replicate(time_PB5_fillval, 3, n_epoch_hr)
bf1      = replicate(bf1_fillval,   1, n_epoch_hr)
bgsm     = replicate(bgsm_fillval,  3, n_epoch_hr)
bgse     = replicate(bgse_fillval,  3, n_epoch_hr)
range    = replicate(range_fillval, 1, n_epoch_hr)
spc_mode = replicate(spc_mode_fillval, 1, n_epoch_hr)
mag_mode = replicate(mag_mode_fillval, 1, n_epoch_hr)

;- epoch, time_PB5
epoch[0,*]    = epoch_hr
time_PB5[0,*] = year_cur
time_PB5[1,*] = doy_cur
time_PB5[2,*] = long(epoch_hr-epoch_date_cur)

;- magnetic field
if n_btot_hr_ok gt 0 then bf1[sub_btot_hr] = btot_hr[sub_btot_hr].value
if n_bgsm_hr_ok gt 0 then begin
   bgsm[0,sub_bgsm_hr] = bgsm_hr[sub_bgsm_hr].x
   bgsm[1,sub_bgsm_hr] = bgsm_hr[sub_bgsm_hr].y
   bgsm[2,sub_bgsm_hr] = bgsm_hr[sub_bgsm_hr].z
endif
if n_bgse_hr_ok gt 0 then begin
   bgse[0,sub_bgse_hr] = bgse_hr[sub_bgse_hr].x
   bgse[1,sub_bgse_hr] = bgse_hr[sub_bgse_hr].y
   bgse[2,sub_bgse_hr] = bgse_hr[sub_bgse_hr].z
endif

;- range, spc_mode, mag_mode
range[0,*]    = calib_hr.range
spc_mode[0,*] = 1L + 4L*sw12_hr.tel_rate
mag_mode[0,*] = 10L*sw12_hr.swap + sw12_hr.mode + 7L*((sw12_hr.swap eq 0)and(sw12_hr.mode eq 0)and(outer_mag_hr eq 0))
;--- end: high-resolution data ----------------------------------------

;--- 1-minute averages -------------------------------------------
;- create variables and fill with fill values
n_epoch_1m = 24L*60L
epoch1     = replicate(epoch1_fillval, 1, n_epoch_1m)
time1_PB5  = replicate(time1_PB5_fillval, 3, n_epoch_1m)
num1_pts_o = replicate(num1_pts_o_fillval, 1, n_epoch_1m)
zero1_o    = replicate(zero1_o_fillval,  3, 8, n_epoch_1m)
sens1_o    = replicate(sens1_o_fillval,  3, 8, n_epoch_1m)
ampl1_o    = replicate(ampl1_o_fillval,  3, 8, n_epoch_1m)
orth1_o    = replicate(orth1_o_fillval,  3, 3, n_epoch_1m)
payld1_o   = replicate(payld1_o_fillval, 3, 3, n_epoch_1m)
flag1_o    = replicate(flag1_o_fillval,  1, n_epoch_1m)
num1_pts_i = replicate(num1_pts_i_fillval, 1, n_epoch_1m)
zero1_i    = replicate(zero1_i_fillval,  3, 8, n_epoch_1m)
sens1_i    = replicate(sens1_i_fillval,  3, 8, n_epoch_1m)
ampl1_i    = replicate(ampl1_i_fillval,  3, 8, n_epoch_1m)
orth1_i    = replicate(orth1_i_fillval,  3, 3, n_epoch_1m)
payld1_i   = replicate(payld1_i_fillval, 3, 3, n_epoch_1m)
flag1_i    = replicate(flag1_i_fillval,  1, n_epoch_1m)

;- epoch, time_PB5
epoch_1m_start = epoch_date_cur + dindgen(n_epoch_1m)*60d3
epoch_1m_end   = epoch_1m_start + 60d3 - 1d
epoch1[0,*]    = epoch_1m_start + 30d3
time1_PB5[0,*] = year_cur
time1_PB5[1,*] = doy_cur
time1_PB5[2,*] = long(epoch1-epoch_date_cur)

;- necessary subscripts 
sub_epoch_1m_start = value_locate(epoch_hr, epoch_1m_start, /L64)
sub_epoch_1m_end   = value_locate(epoch_hr, epoch_1m_end, /L64)

;- zero, sens, orth, flag
for i_e=0, n_epoch_1m-1L do begin
   if sub_epoch_1m_start[i_e] lt sub_epoch_1m_end[i_e] then begin
      ;- zero, sens, orth
      sub_hr_ou = where(outer_mag_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]] eq 1, n_hr_ou, $
                        complement=sub_hr_in, ncomplement=n_hr_in)
      if n_hr_ou gt 0 then begin
         num1_pts_o[0, i_e] = n_hr_ou
         calib_hr_ou   = (calib_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]])[sub_hr_ou]
         ;- sens, zero
         for i_r=0, 7 do begin
            sub_cur_range_ou = where(calib_hr_ou.range eq i_r, n_cur_range_ou)
            if n_cur_range_ou gt 0 then begin
               ;- sens
               sens1_o[0,i_r,i_e] = mean(calib_hr_ou[sub_cur_range_ou].sensx)
               sens1_o[1,i_r,i_e] = mean(calib_hr_ou[sub_cur_range_ou].sensy)
               sens1_o[2,i_r,i_e] = mean(calib_hr_ou[sub_cur_range_ou].sensz)
               ;- zero
               zero1_o[0,i_r,i_e] = mean(calib_hr_ou[sub_cur_range_ou].zerox)
               zero1_o[1,i_r,i_e] = mean(calib_hr_ou[sub_cur_range_ou].zeroy)
               zero1_o[2,i_r,i_e] = mean(calib_hr_ou[sub_cur_range_ou].zeroz)
            endif
         endfor
         ;- orth
         thetax_mean = mean(calib_hr_ou.thetax)  
         thetay_mean = mean(calib_hr_ou.thetay)
         thetaz_mean = mean(calib_hr_ou.thetaz)
         phix_mean   = mean(calib_hr_ou.phix)
         phiy_mean   = mean(calib_hr_ou.phiy)
         phiz_mean   = mean(calib_hr_ou.phiz)
         matrix_temp = [[ cos(thetax_mean)*cos(phix_mean), cos(thetax_mean)*sin(phix_mean), sin(thetax_mean)], $
                        [-cos(thetay_mean)*sin(phiy_mean), cos(thetay_mean)*cos(phiy_mean), sin(thetay_mean)], $
                        [ sin(thetaz_mean)*cos(phiz_mean), sin(thetaz_mean)*sin(phiz_mean), cos(thetaz_mean)]]
         determ_matrix_temp = determ(matrix_temp, /check)
         orth1_o[0,0,i_e] = (matrix_temp[1,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[1,2])/determ_matrix_temp
         orth1_o[1,0,i_e] =-(matrix_temp[1,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[1,2])/determ_matrix_temp
         orth1_o[2,0,i_e] = (matrix_temp[1,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[1,1])/determ_matrix_temp
         orth1_o[0,1,i_e] =-(matrix_temp[0,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[0,2])/determ_matrix_temp
         orth1_o[1,1,i_e] = (matrix_temp[0,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[0,2])/determ_matrix_temp
         orth1_o[2,1,i_e] =-(matrix_temp[0,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[0,1])/determ_matrix_temp
         orth1_o[0,2,i_e] = (matrix_temp[0,1]*matrix_temp[1,2] - matrix_temp[1,1]*matrix_temp[0,2])/determ_matrix_temp
         orth1_o[1,2,i_e] =-(matrix_temp[0,0]*matrix_temp[1,2] - matrix_temp[1,0]*matrix_temp[0,2])/determ_matrix_temp
         orth1_o[2,2,i_e] = (matrix_temp[0,0]*matrix_temp[1,1] - matrix_temp[1,0]*matrix_temp[0,1])/determ_matrix_temp
         ;- payld
         payld1_o[*,*,i_e]= [[1d, 0d, 0d], [0d, 1d, 0d], [0d, 0d, -1d]] 
         ;- ampl
         ampl1_o[*,*,i_e] = 1d
         ;- flag
         flag1_o[0, i_e]  = 4L + round(mean(calib_hr_ou.qzeroz ne 1))
      endif
      if n_hr_in gt 0 then begin
         num1_pts_i[0, i_e] = n_hr_in
         calib_hr_in   = (calib_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]])[sub_hr_in]
         ;- sens, zero
         for i_r=0, 7 do begin
            sub_cur_range_in = where(calib_hr_in.range eq i_r, n_cur_range_in)
            if n_cur_range_in gt 0 then begin
               ;- sens
               sens1_i[0,i_r,i_e] = mean(calib_hr_in[sub_cur_range_in].sensx)
               sens1_i[1,i_r,i_e] = mean(calib_hr_in[sub_cur_range_in].sensy)
               sens1_i[2,i_r,i_e] = mean(calib_hr_in[sub_cur_range_in].sensz)
               ;- zero
               zero1_i[0,i_r,i_e] = mean(calib_hr_in[sub_cur_range_in].zerox)
               zero1_i[1,i_r,i_e] = mean(calib_hr_in[sub_cur_range_in].zeroy)
               zero1_i[2,i_r,i_e] = mean(calib_hr_in[sub_cur_range_in].zeroz)
            endif
         endfor
         ;- orth
         thetax_mean = mean(calib_hr_in.thetax)  
         thetay_mean = mean(calib_hr_in.thetay)
         thetaz_mean = mean(calib_hr_in.thetaz)
         phix_mean   = mean(calib_hr_in.phix)
         phiy_mean   = mean(calib_hr_in.phiy)
         phiz_mean   = mean(calib_hr_in.phiz)
         matrix_temp = [[ cos(thetax_mean)*cos(phix_mean), cos(thetax_mean)*sin(phix_mean), sin(thetax_mean)], $
                        [-cos(thetay_mean)*sin(phiy_mean), cos(thetay_mean)*cos(phiy_mean), sin(thetay_mean)], $
                        [ sin(thetaz_mean)*cos(phiz_mean), sin(thetaz_mean)*sin(phiz_mean), cos(thetaz_mean)]]
         determ_matrix_temp = determ(matrix_temp, /check)
         orth1_i[0,0,i_e] = (matrix_temp[1,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[1,2])/determ_matrix_temp
         orth1_i[1,0,i_e] =-(matrix_temp[1,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[1,2])/determ_matrix_temp
         orth1_i[2,0,i_e] = (matrix_temp[1,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[1,1])/determ_matrix_temp
         orth1_i[0,1,i_e] =-(matrix_temp[0,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[0,2])/determ_matrix_temp
         orth1_i[1,1,i_e] = (matrix_temp[0,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[0,2])/determ_matrix_temp
         orth1_i[2,1,i_e] =-(matrix_temp[0,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[0,1])/determ_matrix_temp
         orth1_i[0,2,i_e] = (matrix_temp[0,1]*matrix_temp[1,2] - matrix_temp[1,1]*matrix_temp[0,2])/determ_matrix_temp
         orth1_i[1,2,i_e] =-(matrix_temp[0,0]*matrix_temp[1,2] - matrix_temp[1,0]*matrix_temp[0,2])/determ_matrix_temp
         orth1_i[2,2,i_e] = (matrix_temp[0,0]*matrix_temp[1,1] - matrix_temp[1,0]*matrix_temp[0,1])/determ_matrix_temp
         ;- payld
         payld1_i[*,*,i_e]= [[1d, 0d, 0d], [0d, 1d, 0d], [0d, 0d, -1d]] 
         ;- ampl
         ampl1_i[*,*,i_e] = 1d
         ;- flag
         flag1_i[0, i_e]  = 4L + round(mean(calib_hr_in.qzeroz ne 1))
      endif
   endif
endfor
;--- end: 1-minute averages -------------------------------------------

;--- create and write into cdf file
;- create file
if keyword_set(counts)  then prefix_temp=prefix_cnts
if keyword_set(payload) then prefix_temp=prefix_payld
cdf_set_cdf27_backward_compatible, /yes
cdf_filename = highres_path + highres_prefix + mfi_prefix+(strsplit(string(date), /extract))[0] + $
               '_v' + string(version, format='(I02)') + '.cdf'
cdfid = cdf_create(cdf_filename, [3], /clobber, /single_file, /col_major, /network_encoding, /host_decoding)

;- create variables
varid = cdf_varcreate(cdfid, 'Epoch',    [0], dim=[1], /rec_vary, allocate=n_epoch_hr, /cdf_epoch, /zvar)
varid = cdf_varcreate(cdfid, 'Time_PB5', [1], dim=[3], /rec_vary, allocate=n_epoch_hr, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'BF1',      [0], dim=[1], /rec_vary, allocate=n_epoch_hr, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BGSM',     [1], dim=[3], /rec_vary, allocate=n_epoch_hr, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BGSE',     [1], dim=[3], /rec_vary, allocate=n_epoch_hr, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'RANGE',    [0], dim=[1], /rec_vary, allocate=n_epoch_hr, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'SPC_MODE', [0], dim=[1], /rec_vary, allocate=n_epoch_hr, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'MAG_MODE', [0], dim=[1], /rec_vary, allocate=n_epoch_hr, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'Epoch1',    [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_epoch, /zvar)
varid = cdf_varcreate(cdfid, 'Time1_PB5', [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'NUM1_PTS_O',  [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'ZERO1_O',     [1,1], dim=[3,8], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SENS1_O',     [1,1], dim=[3,8], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'AMPL1_O',     [1,1], dim=[3,8], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'ORTH1_O',     [1,1], dim=[3,3], /rec_vary, allocate=n_epoch_1m, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'PAYLD1_O',    [1,1], dim=[3,3], /rec_vary, allocate=n_epoch_1m, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'FLAG1_O',     [0],   dim=[1],   /rec_vary, allocate=n_epoch_1m, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'NUM1_PTS_I',  [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'ZERO1_I',     [1,1], dim=[3,8], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SENS1_I',     [1,1], dim=[3,8], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'AMPL1_I',     [1,1], dim=[3,8], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'ORTH1_I',     [1,1], dim=[3,3], /rec_vary, allocate=n_epoch_1m, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'PAYLD1_I',    [1,1], dim=[3,3], /rec_vary, allocate=n_epoch_1m, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'FLAG1_I',     [0],   dim=[1],   /rec_vary, allocate=n_epoch_1m, /cdf_int4,  /zvar)
varid = cdf_varcreate(cdfid, 'label_time',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=27, /zvar)
varid = cdf_varcreate(cdfid, 'format_time', [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=2,  /zvar)
varid = cdf_varcreate(cdfid, 'unit_time',   [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=4,  /zvar)
varid = cdf_varcreate(cdfid, 'label_bgsm',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=8,  /zvar)
varid = cdf_varcreate(cdfid, 'label_bgse',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=8,  /zvar)
varid = cdf_varcreate(cdfid, 'cartesian',   [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=11, /zvar)

;- set variable padvalues equal to fill values
cdf_control, cdfid, variable='Epoch',    set_padvalue=Epoch_fillval
cdf_control, cdfid, variable='Time_PB5', set_padvalue=Time_PB5_fillval
cdf_control, cdfid, variable='BF1',      set_padvalue=BF1_fillval
cdf_control, cdfid, variable='BGSM',     set_padvalue=BGSM_fillval
cdf_control, cdfid, variable='BGSE',     set_padvalue=BGSE_fillval
cdf_control, cdfid, variable='RANGE',    set_padvalue=RANGE_fillval
cdf_control, cdfid, variable='SPC_MODE', set_padvalue=SPC_MODE_fillval
cdf_control, cdfid, variable='MAG_MODE', set_padvalue=MAG_MODE_fillval
cdf_control, cdfid, variable='Epoch1',    set_padvalue=Epoch1_fillval
cdf_control, cdfid, variable='Time1_PB5', set_padvalue=Time1_PB5_fillval
cdf_control, cdfid, variable='NUM1_PTS_O',  set_padvalue=NUM1_PTS_O_fillval
cdf_control, cdfid, variable='ZERO1_O',     set_padvalue=ZERO1_O_fillval
cdf_control, cdfid, variable='SENS1_O',     set_padvalue=SENS1_O_fillval
cdf_control, cdfid, variable='AMPL1_O',     set_padvalue=AMPL1_O_fillval
cdf_control, cdfid, variable='ORTH1_O',     set_padvalue=ORTH1_O_fillval
cdf_control, cdfid, variable='PAYLD1_O',    set_padvalue=PAYLD1_O_fillval
cdf_control, cdfid, variable='FLAG1_O',     set_padvalue=FLAG1_O_fillval
cdf_control, cdfid, variable='NUM1_PTS_I',  set_padvalue=NUM1_PTS_I_fillval
cdf_control, cdfid, variable='ZERO1_I',     set_padvalue=ZERO1_I_fillval
cdf_control, cdfid, variable='SENS1_I',     set_padvalue=SENS1_I_fillval
cdf_control, cdfid, variable='AMPL1_I',     set_padvalue=AMPL1_I_fillval
cdf_control, cdfid, variable='ORTH1_I',     set_padvalue=ORTH1_I_fillval
cdf_control, cdfid, variable='PAYLD1_I',    set_padvalue=PAYLD1_I_fillval
cdf_control, cdfid, variable='FLAG1_I',     set_padvalue=FLAG1_I_fillval

;- put variable values
cdf_varput, cdfid, 'Epoch',     epoch
cdf_varput, cdfid, 'Time_PB5',  time_pb5
cdf_varput, cdfid, 'BF1',       bf1
cdf_varput, cdfid, 'BGSM',      bgsm
cdf_varput, cdfid, 'BGSE',      bgse
cdf_varput, cdfid, 'RANGE',     range
cdf_varput, cdfid, 'SPC_MODE',  spc_mode
cdf_varput, cdfid, 'MAG_MODE',  mag_mode
cdf_varput, cdfid, 'Epoch1',     epoch1
cdf_varput, cdfid, 'Time1_PB5',  time1_pb5
cdf_varput, cdfid, 'NUM1_PTS_O',   num1_pts_o
cdf_varput, cdfid, 'ZERO1_O',      zero1_o
cdf_varput, cdfid, 'SENS1_O',      sens1_o
cdf_varput, cdfid, 'AMPL1_O',      ampl1_o
cdf_varput, cdfid, 'ORTH1_O',      orth1_o
cdf_varput, cdfid, 'PAYLD1_O',     payld1_o
cdf_varput, cdfid, 'FLAG1_O',      flag1_o
cdf_varput, cdfid, 'NUM1_PTS_I',   num1_pts_i
cdf_varput, cdfid, 'ZERO1_I',      zero1_i
cdf_varput, cdfid, 'SENS1_I',      sens1_i
cdf_varput, cdfid, 'AMPL1_I',      ampl1_i
cdf_varput, cdfid, 'ORTH1_I',      orth1_i
cdf_varput, cdfid, 'PAYLD1_I',     payld1_i
cdf_varput, cdfid, 'FLAG1_I',      flag1_i
cdf_varput, cdfid, 'label_time', ['Year                       ', 'Day of Year (Jan 1 = Day 1)',  $
                                  'Elapsed milliseconds of day']
cdf_varput, cdfid, 'format_time', ['I4', 'I3', 'I8']
cdf_varput, cdfid, 'unit_time',   ['year', 'day ', 'msec']
cdf_varput, cdfid, 'label_bgsm',  ['Bx (GSM)', 'By (GSM)', 'Bz (GSM)']
cdf_varput, cdfid, 'label_bgse',  ['Bx (GSE)', 'By (GSE)', 'Bz (GSE)']
cdf_varput, cdfid, 'cartesian',   ['x-component', 'y-component', 'z-component']

;- create all attributes
attid = cdf_attcreate(cdfid, 'Project',    /global_scope)
attid = cdf_attcreate(cdfid, 'Discipline', /global_scope)
attid = cdf_attcreate(cdfid, 'Source_name', /global_scope)
attid = cdf_attcreate(cdfid, 'Software_version', /global_scope)
attid = cdf_attcreate(cdfid, 'Data_type', /global_scope)
attid = cdf_attcreate(cdfid, 'Descriptor', /global_scope)
attid = cdf_attcreate(cdfid, 'Data_version', /global_scope)
attid = cdf_attcreate(cdfid, 'TITLE', /global_scope)
attid = cdf_attcreate(cdfid, 'TEXT', /global_scope)
attid = cdf_attcreate(cdfid, 'MODS', /global_scope)
attid = cdf_attcreate(cdfid, 'ADID_ref', /global_scope)
attid = cdf_attcreate(cdfid, 'Logical_file_id', /global_scope)
attid = cdf_attcreate(cdfid, 'Logical_source', /global_scope)
attid = cdf_attcreate(cdfid, 'Logical_source_description', /global_scope)
attid = cdf_attcreate(cdfid, 'Processing_date', /global_scope)
attid = cdf_attcreate(cdfid, 'Level_0_file', /global_scope)
attid = cdf_attcreate(cdfid, 'HKlvl_0_file', /global_scope)
attid = cdf_attcreate(cdfid, 'Orbit_file', /global_scope)
attid = cdf_attcreate(cdfid, 'Attitude_file', /global_scope)
attid = cdf_attcreate(cdfid, 'def_FLAGS', /global_scope)
attid = cdf_attcreate(cdfid, 'def_SPC_MODE', /global_scope)
attid = cdf_attcreate(cdfid, 'def_MAG_MODE', /global_scope)
attid = cdf_attcreate(cdfid, 'PI_name', /global_scope)
attid = cdf_attcreate(cdfid, 'PI_affiliation', /global_scope)
attid = cdf_attcreate(cdfid, 'Mission_group', /global_scope)
attid = cdf_attcreate(cdfid, 'Instrument_type', /global_scope)
attid = cdf_attcreate(cdfid, 'TEXT_supplement_1', /global_scope)
attid = cdf_attcreate(cdfid, 'Web_sites', /global_scope)
attid = cdf_attcreate(cdfid, 'FIELDNAM', /variable_scope)
attid = cdf_attcreate(cdfid, 'VALIDMIN', /variable_scope)
attid = cdf_attcreate(cdfid, 'VALIDMAX', /variable_scope)
attid = cdf_attcreate(cdfid, 'SCALEMIN', /variable_scope)
attid = cdf_attcreate(cdfid, 'SCALEMAX', /variable_scope)
attid = cdf_attcreate(cdfid, 'UNITS', /variable_scope)
attid = cdf_attcreate(cdfid, 'LABLAXIS', /variable_scope)
attid = cdf_attcreate(cdfid, 'FORMAT', /variable_scope)
attid = cdf_attcreate(cdfid, 'MONOTON', /variable_scope)
attid = cdf_attcreate(cdfid, 'SCALETYP', /variable_scope)
attid = cdf_attcreate(cdfid, 'CATDESC', /variable_scope)
attid = cdf_attcreate(cdfid, 'FILLVAL', /variable_scope)
attid = cdf_attcreate(cdfid, 'LABL_PTR_1', /variable_scope)
attid = cdf_attcreate(cdfid, 'LABL_PTR_2', /variable_scope)
attid = cdf_attcreate(cdfid, 'UNIT_PTR', /variable_scope)
attid = cdf_attcreate(cdfid, 'FORM_PTR', /variable_scope)
attid = cdf_attcreate(cdfid, 'DEPEND_0', /variable_scope)
attid = cdf_attcreate(cdfid, 'DEPEND_1', /variable_scope)
attid = cdf_attcreate(cdfid, 'VAR_TYPE', /variable_scope)
attid = cdf_attcreate(cdfid, 'DICT_KEY', /variable_scope)
attid = cdf_attcreate(cdfid, 'AVE_TYPE', /variable_scope)
attid = cdf_attcreate(cdfid, 'TIME_RES', /variable_scope)
attid = cdf_attcreate(cdfid, 'VAR_NOTES', /variable_scope)

;- put global attribute values
cdf_attput, cdfid, 'Project',      0, 'ISTP>International Solar-Terrestrial Physics'
cdf_attput, cdfid, 'Discipline',   0, 'Space Physics>Heliospheric Science'
cdf_attput, cdfid, 'Source_name',  0, 'WIND>Wind Interplanetary Plasma Laboratory'
cdf_attput, cdfid, 'Software_version', 0, 'V05.00'
cdf_attput, cdfid, 'Data_type',    0, 'H2>High-resolution Definitive Data'
cdf_attput, cdfid, 'Descriptor',   0, 'MFI>Magnetic Fields Investigation'
cdf_attput, cdfid, 'Data_version', 0, string(version, format='(I02)')
cdf_attput, cdfid, 'TITLE',  0, 'WIND Magnetic Field Investigation (MFI) High-Resolution Production'
cdf_attput, cdfid, 'TEXT',   0, 'WIND MFI high-resolution data file. Time resolution varies with instrument mode.'
cdf_attput, cdfid, 'TEXT',   1, 'Modes 0 & 10, low rate: .184s, high rate: .092s                                 '
cdf_attput, cdfid, 'TEXT',   2, 'Modes 1 & 11, low rate: Prim .092s Sec 1.84s, high rate: Prim .046s Sec .92s    '
cdf_attput, cdfid, 'TEXT',   3, 'Modes 2 & 12, Same as Modes 1 & 11                                              '
cdf_attput, cdfid, 'TEXT',   4, 'Calibration constants are 1 minute averages.                                    '
cdf_attput, cdfid, 'TEXT',   5, 'WIND MFI Instrument turn on 11/12/1994                                          '
cdf_attput, cdfid, 'TEXT',   6, 'Data versions:                                                                  '
cdf_attput, cdfid, 'TEXT',   7, '03 - Extrapolated Bz correction                                                 '
cdf_attput, cdfid, 'TEXT',   8, '04 - Final Bz correction                                                        '
cdf_attput, cdfid, 'TEXT',   9, '05 - Final attitude and Bz correction                                              '
cdf_attput, cdfid, 'TEXT',  10, 'References:                                                                     '
cdf_attput, cdfid, 'TEXT',  11, '1. Lepping, R. P., et al., The WIND Magnetic Field Investigation, p. 207 in     '
cdf_attput, cdfid, 'TEXT',  12, 'The Global Geospace Mission, ed. by C. T. Russell, Kluwer,1995                  '
cdf_attput, cdfid, 'TEXT',  13, '2. Panetta, P. (GSFC), GGS WIND MFI Operator''s Manual, September 15, 1992.     '
cdf_attput, cdfid, 'TEXT',  14, '3. Computer Sciences Corporation, Data Format Control Document (DFCD) Between   '
cdf_attput, cdfid, 'TEXT',  15, 'The International Solar-Terrestrial Physics (ISTP) Program Information          '
cdf_attput, cdfid, 'TEXT',  16, 'Processing Division Ground Data Processing System and The ISTP Mission          '
cdf_attput, cdfid, 'TEXT',  17, 'Investigators, CSC/TR-91/6014, 560-1DFD/0190, July 1992.                        '
cdf_attput, cdfid, 'TEXT',  18, '4. Behannon, K. W., International Solar Terrestrial Physics (ISTP) Program      '
cdf_attput, cdfid, 'TEXT',  19, 'Investigator Data Analysis Requirements For WIND and GEOTAIL Spacecraft         '
cdf_attput, cdfid, 'TEXT',  20, 'Magnetometer Experiment, September 1987.                                        '
cdf_attput, cdfid, 'TEXT',  21, '5. National Space Science Data Center, CDF User''s Guide, Version 2.3.0,        '
cdf_attput, cdfid, 'TEXT',  22, 'October 1, 1992.                                                                '
cdf_attput, cdfid, 'TEXT',  23, '6. Mish, W. H., International Solar-Terrestrial Physics (ISTP) Key Parameter    '
cdf_attput, cdfid, 'TEXT',  24, 'Generation Software (KPGS) Standards & Conventions, September 1992.             '
cdf_attput, cdfid, 'TEXT',  25, '7. Mish, W. H., IMP F and G Phase I Magnetic Field Analysis, April 1972         '
cdf_attput, cdfid, 'MODS',   0, ' 10/01/2011 Initial release                    '
cdf_attput, cdfid, 'ADID_ref',   0, 'NSSD0141'
cdf_attput, cdfid, 'Logical_file_id',0, highres_prefix + mfi_prefix1 + '_00000000_v00'
cdf_attput, cdfid, 'Logical_source', 0, highres_prefix + mfi_prefix1
cdf_attput, cdfid, 'Logical_source_description',   0, $
                    'Wind Magnetic Fields Investigation, High-resolution Definitive Data'
cdf_attput, cdfid, 'Processing_date',0, systime(/utc) + ' UTC'
if lz_files[0] ne '' then lz_file0_basename=file_basename(lz_files[0]) else lz_file0_basename=' '
   cdf_attput, cdfid, 'Level_0_file', 0, lz_file0_basename
if lz_files[1] ne '' then lz_file1_basename=file_basename(lz_files[1]) else lz_file1_basename=' '
   cdf_attput, cdfid, 'Level_0_file', 1, lz_file1_basename
if lz_files[2] ne '' then lz_file2_basename=file_basename(lz_files[2]) else lz_file2_basename=' '
   cdf_attput, cdfid, 'Level_0_file', 2, lz_file2_basename
if hk_files[0] ne '' then hk_file0_basename=file_basename(hk_files[0]) else hk_file0_basename=' '
   cdf_attput, cdfid, 'HKlvl_0_file', 0, hk_file0_basename
if hk_files[1] ne '' then hk_file1_basename=file_basename(hk_files[1]) else hk_file1_basename=' '
   cdf_attput, cdfid, 'HKlvl_0_file', 1, hk_file1_basename
if hk_files[2] ne '' then hk_file2_basename=file_basename(hk_files[2]) else hk_file2_basename=' '
   cdf_attput, cdfid, 'HKlvl_0_file', 2, hk_file2_basename
if or_files[0] ne '' then or_file0_basename=file_basename(or_files[0]) else or_file0_basename=' '
   cdf_attput, cdfid, 'Orbit_file', 0, or_file0_basename
if or_files[1] ne '' then or_file1_basename=file_basename(or_files[1]) else or_file1_basename=' '
   cdf_attput, cdfid, 'Orbit_file', 1, or_file1_basename
if or_files[2] ne '' then or_file2_basename=file_basename(or_files[2]) else or_file2_basename=' '
   cdf_attput, cdfid, 'Orbit_file', 2, or_file2_basename
if at_files[0] ne '' then at_file0_basename=file_basename(at_files[0]) else at_file0_basename=' '
   cdf_attput, cdfid, 'Attitude_file', 0, at_file0_basename
if at_files[1] ne '' then at_file1_basename=file_basename(at_files[1]) else at_file1_basename=' '
   cdf_attput, cdfid, 'Attitude_file', 1, at_file1_basename
if at_files[2] ne '' then at_file2_basename=file_basename(at_files[2]) else at_file2_basename=' '
   cdf_attput, cdfid, 'Attitude_file', 2, at_file2_basename
cdf_attput, cdfid, 'def_FLAGS',      0, '00 - Interpolated   Calibration Values                     '
cdf_attput, cdfid, 'def_FLAGS',      1, '01 - Last available Calibration Values                     '
cdf_attput, cdfid, 'def_FLAGS',      2, '02 - Daily Computed Calibration Values                     '
cdf_attput, cdfid, 'def_FLAGS',      3, '03 - Daily Computed Calibration Values with Bz corrections '
cdf_attput, cdfid, 'def_FLAGS',      4, '04 - High-Resolution Calibration Values                    '
cdf_attput, cdfid, 'def_FLAGS',      5, '05 - High-Resolution Calibration Values with Bz corrections'
cdf_attput, cdfid, 'def_SPC_MODE',   0, '01 - Science     mode (92 sec)'
cdf_attput, cdfid, 'def_SPC_MODE',   1, '03 - Maneuver    mode (92 sec)'
cdf_attput, cdfid, 'def_SPC_MODE',   2, '04 - Contingency mode (92 sec)'
cdf_attput, cdfid, 'def_SPC_MODE',   3, '05 - Science     mode (46 sec)'
cdf_attput, cdfid, 'def_SPC_MODE',   4, '07 - Maneuver    mode (46 sec)'
cdf_attput, cdfid, 'def_SPC_MODE',   5, '08 - Contingency mode (46 sec)'
cdf_attput, cdfid, 'def_MAG_MODE',   0, '00 - Primary to Secondary Ratio  (1:1) Outboard prime w/ FFT & Snapshot'
cdf_attput, cdfid, 'def_MAG_MODE',   1, '01 - Primary to Secondary Ratio (20:1) Outboard prime w/ FFT & Snapshot'
cdf_attput, cdfid, 'def_MAG_MODE',   2, '02 - Primary to Secondary Ratio (20:1) Outboard prime w/ Snapshot only '
cdf_attput, cdfid, 'def_MAG_MODE',   3, '03 - Primary to Secondary Ratio  (1:1) Outboard prime Maneuver         '
cdf_attput, cdfid, 'def_MAG_MODE',   4, '07 - Primary to Secondary Ratio  (1:1) Outboard prime used Inboard data'
cdf_attput, cdfid, 'def_MAG_MODE',   5, '10 - Primary to Secondary Ratio  (1:1)  Inboard prime w/ FFT & Snapshot'
cdf_attput, cdfid, 'def_MAG_MODE',   6, '11 - Primary to Secondary Ratio (20:1)  Inboard prime w/ FFT & Snapshot'
cdf_attput, cdfid, 'def_MAG_MODE',   7, '12 - Primary to Secondary Ratio (20:1)  Inboard prime w/ Snapshot only '
cdf_attput, cdfid, 'def_MAG_MODE',   8, '13 - Primary to Secondary Ratio  (1:1)  Inboard prime Maneuver         '
cdf_attput, cdfid, 'PI_name',        0, 'A. Koval'
cdf_attput, cdfid, 'PI_affiliation', 0, 'UMBC, NASA/GSFC'
cdf_attput, cdfid, 'Mission_group',  0, 'Wind'
cdf_attput, cdfid, 'Instrument_type',0, 'Magnetic Fields (space)'
cdf_attput, cdfid, 'TEXT_supplement_1', 0, ' '
cdf_attput, cdfid, 'Web_sites',      0, 'http://wind.nasa.gov'

;- put variable attribute values
cdf_attput, cdfid, 'FIELDNAM',  'Epoch',     'Time Line'
cdf_attput, cdfid, 'FIELDNAM',  'Time_PB5',  'Time Line'
cdf_attput, cdfid, 'FIELDNAM',  'BF1',       'Magnetic field magnitude'
cdf_attput, cdfid, 'FIELDNAM',  'BGSM',      'Magnetic field vector in GSM cartesian coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'BGSE',      'Magnetic field vector in GSE cartesian coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'RANGE',     'Magnetometer Range'
cdf_attput, cdfid, 'FIELDNAM',  'SPC_MODE',  'S/C operational mode'
cdf_attput, cdfid, 'FIELDNAM',  'MAG_MODE',  'WIND/MFI operational mode'
cdf_attput, cdfid, 'FIELDNAM',  'Epoch1',     'Time Line (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'Time1_PB5',  'Time Line (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'NUM1_PTS_O',   'Outer Sensor number of points in average (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'ZERO1_O',      'Outer Sensor Zeros (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'SENS1_O',      'Outer Sensor Sensitivities (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'AMPL1_O',      'Outer Sensor Amplitude Correction (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'ORTH1_O',      'Outer Sensor Orthogonalization Matrix (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'PAYLD1_O',     'Outer Sensor Payload Rotation Matrix (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'FLAG1_O',      'Outer Sensor Zero Flag (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'NUM1_PTS_I',   'Inner Sensor number of points in average (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'ZERO1_I',      'Inner Sensor Zeros (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'SENS1_I',      'Inner Sensor Sensitivities (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'AMPL1_I',      'Inner Sensor Amplitude Correction (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'ORTH1_I',      'Inner Sensor Orthogonalization Matrix (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'PAYLD1_I',     'Inner Sensor Payload Rotation Matrix (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'FLAG1_I',      'Inner Sensor Zero Flag (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'label_time',  'Label for Time_PB5'
cdf_attput, cdfid, 'FIELDNAM',  'format_time', 'Format for Time_PB5'
cdf_attput, cdfid, 'FIELDNAM',  'unit_time',   'Units for Time_PB5'
cdf_attput, cdfid, 'FIELDNAM',  'label_bgsm',  'Label for B in GMS coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'label_bgse',  'Label for B in GSE coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'cartesian',   'Dimension Label'
cdf_attput, cdfid, 'VALIDMIN',  'Epoch',        62951817600000d
cdf_attput, cdfid, 'VALIDMIN',  'Time_PB5',     [1994L, 316L, 0L]
cdf_attput, cdfid, 'VALIDMIN',  'BF1',          0.
cdf_attput, cdfid, 'VALIDMIN',  'BGSM',         [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'BGSE',         [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'RANGE',        0L
cdf_attput, cdfid, 'VALIDMIN',  'SPC_MODE',     0L
cdf_attput, cdfid, 'VALIDMIN',  'MAG_MODE',     0L
cdf_attput, cdfid, 'VALIDMIN',  'Epoch1',       62951817600000d
cdf_attput, cdfid, 'VALIDMIN',  'Time1_PB5',    [1994L, 316L, 0L]
cdf_attput, cdfid, 'VALIDMIN',  'NUM1_PTS_O',     0L
cdf_attput, cdfid, 'VALIDMIN',  'NUM1_PTS_I',     0L
cdf_attput, cdfid, 'VALIDMAX',  'Epoch',       64092124800000d
cdf_attput, cdfid, 'VALIDMAX',  'Time_PB5',    [2030L, 365L, 0L]
cdf_attput, cdfid, 'VALIDMAX',  'BF1',         65534.
cdf_attput, cdfid, 'VALIDMAX',  'BGSM',        [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'BGSE',        [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'RANGE',       7L
cdf_attput, cdfid, 'VALIDMAX',  'SPC_MODE',    10L
cdf_attput, cdfid, 'VALIDMAX',  'MAG_MODE',    15L
cdf_attput, cdfid, 'VALIDMAX',  'Epoch1',      64092124800000d
cdf_attput, cdfid, 'VALIDMAX',  'Time1_PB5',   [2030L, 365L, 0L]
cdf_attput, cdfid, 'VALIDMAX',  'NUM1_PTS_O',    1500L
cdf_attput, cdfid, 'VALIDMAX',  'NUM1_PTS_I',    1500L
cdf_attput, cdfid, 'SCALEMIN',  'Epoch',        epoch_date_cur
cdf_attput, cdfid, 'SCALEMIN',  'Time_PB5',     [long(year_cur), long(doy_cur), 0L]
cdf_attput, cdfid, 'SCALEMIN',  'BF1',          mfi_min_max(bf1,     fillval=bf1_fillval)   
cdf_attput, cdfid, 'SCALEMIN',  'BGSM',        [mfi_min_max(bgsm[0,*], fillval=bgsm_fillval), $
                                                mfi_min_max(bgsm[1,*], fillval=bgsm_fillval), $
                                                mfi_min_max(bgsm[2,*], fillval=bgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'BGSE',        [mfi_min_max(bgse[0,*], fillval=bgse_fillval), $
                                                mfi_min_max(bgse[1,*], fillval=bgse_fillval), $
                                                mfi_min_max(bgse[2,*], fillval=bgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'RANGE',        mfi_min_max(range,    fillval=range_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'SPC_MODE',     mfi_min_max(spc_mode, fillval=spc_mode_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'MAG_MODE',     mfi_min_max(mag_mode, fillval=mag_mode_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'Epoch1',       epoch_date_cur
cdf_attput, cdfid, 'SCALEMIN',  'Time1_PB5',    [long(year_cur), long(doy_cur), 0L]
cdf_attput, cdfid, 'SCALEMIN',  'NUM1_PTS_O',    mfi_min_max(num1_pts_o, fillval=num1_pts_o_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'NUM1_PTS_I',    mfi_min_max(num1_pts_i, fillval=num1_pts_i_fillval)
cdf_attput, cdfid, 'SCALEMAX',  'Epoch',        epoch_date_plus1day
cdf_attput, cdfid, 'SCALEMAX',  'Time_PB5',     [long(year_plus1day), long(doy_plus1day), 0L]
cdf_attput, cdfid, 'SCALEMAX',  'BF1',          mfi_min_max(bf1,     fillval=bf1_fillval, /max)   
cdf_attput, cdfid, 'SCALEMAX',  'BGSM',        [mfi_min_max(bgsm[0,*], fillval=bgsm_fillval, /max), $
                                                mfi_min_max(bgsm[1,*], fillval=bgsm_fillval, /max), $
                                                mfi_min_max(bgsm[2,*], fillval=bgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'BGSE',        [mfi_min_max(bgse[0,*], fillval=bgse_fillval, /max), $
                                                mfi_min_max(bgse[1,*], fillval=bgse_fillval, /max), $
                                                mfi_min_max(bgse[2,*], fillval=bgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'RANGE',        mfi_min_max(range,    fillval=range_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'SPC_MODE',     mfi_min_max(spc_mode, fillval=spc_mode_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'MAG_MODE',     mfi_min_max(mag_mode, fillval=mag_mode_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'Epoch1',       epoch_date_plus1day
cdf_attput, cdfid, 'SCALEMAX',  'Time1_PB5',    [long(year_plus1day), long(doy_plus1day), 0L]
cdf_attput, cdfid, 'SCALEMAX',  'NUM1_PTS_O',   mfi_min_max(num1_pts_o, fillval=num1_pts_o_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'NUM1_PTS_I',   mfi_min_max(num1_pts_i, fillval=num1_pts_i_fillval, /max)
cdf_attput, cdfid, 'UNITS',  'Epoch',        'ms'
cdf_attput, cdfid, 'UNITS',  'BF1',          'nT'
cdf_attput, cdfid, 'UNITS',  'BGSM',         'nT'
cdf_attput, cdfid, 'UNITS',  'BGSE',         'nT'
cdf_attput, cdfid, 'UNITS',  'RANGE',        ' '
cdf_attput, cdfid, 'UNITS',  'SPC_MODE',     ' '
cdf_attput, cdfid, 'UNITS',  'MAG_MODE',     ' '
cdf_attput, cdfid, 'UNITS',  'Epoch1',       'ms'
cdf_attput, cdfid, 'UNITS',  'NUM1_PTS_O',     ' '
cdf_attput, cdfid, 'UNITS',  'NUM1_PTS_I',     ' '
cdf_attput, cdfid, 'LABLAXIS',  'Epoch',       'Epoch'
cdf_attput, cdfid, 'LABLAXIS',  'BF1',         'B'
cdf_attput, cdfid, 'LABLAXIS',  'RANGE',       'Range'
cdf_attput, cdfid, 'LABLAXIS',  'SPC_MODE',    'S/C MODE'
cdf_attput, cdfid, 'LABLAXIS',  'MAG_MODE',    'MFI MODE'
cdf_attput, cdfid, 'LABLAXIS',  'Epoch1',      'Epoch'
cdf_attput, cdfid, 'LABLAXIS',  'NUM1_PTS_O',  'No. Points (Outer)'
cdf_attput, cdfid, 'LABLAXIS',  'NUM1_PTS_I',  'No. Points (Inner)'
cdf_attput, cdfid, 'FORMAT',  'Epoch',       'E14.8'
cdf_attput, cdfid, 'FORMAT',  'BF1',         'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BGSM',        'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BGSE',        'E13.6'
cdf_attput, cdfid, 'FORMAT',  'RANGE',       'I1'
cdf_attput, cdfid, 'FORMAT',  'SPC_MODE',    'I2'
cdf_attput, cdfid, 'FORMAT',  'MAG_MODE',    'I2'
cdf_attput, cdfid, 'FORMAT',  'Epoch1',      'E14.8'
cdf_attput, cdfid, 'FORMAT',  'NUM1_PTS_O',  'I4'
cdf_attput, cdfid, 'FORMAT',  'NUM1_PTS_I',  'I4'
cdf_attput, cdfid, 'MONOTON',  'Epoch',        'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'Time_PB5',     'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'BF1',          'FALSE'
cdf_attput, cdfid, 'MONOTON',  'BGSM',         'FALSE' 
cdf_attput, cdfid, 'MONOTON',  'BGSE',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'RANGE',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'SPC_MODE',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'MAG_MODE',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'Epoch1',       'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'Time1_PB5',    'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'NUM1_PTS_O',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'ZERO1_O',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'SENS1_O',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'AMPL1_O',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'ORTH1_O',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'PAYLD1_O',       'FALSE'
cdf_attput, cdfid, 'MONOTON',  'FLAG1_O',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'NUM1_PTS_I',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'ZERO1_I',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'SENS1_I',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'AMPL1_I',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'ORTH1_I',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'PAYLD1_I',       'FALSE'
cdf_attput, cdfid, 'MONOTON',  'FLAG1_I',        'FALSE'
cdf_attput, cdfid, 'SCALETYP',  'Epoch',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Time_PB5',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BF1',          'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BGSM',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BGSE',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'RANGE',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'SPC_MODE',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'MAG_MODE',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Epoch1',       'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Time1_PB5',    'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'NUM1_PTS_O',   'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'ZERO1_O',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'SENS1_O',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'AMPL1_O',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'ORTH1_O',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'PAYLD1_O',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'FLAG1_O',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'NUM1_PTS_I',   'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'ZERO1_I',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'SENS1_I',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'AMPL1_I',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'ORTH1_I',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'PAYLD1_I',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'FLAG1_I',      'LINEAR'
cdf_attput, cdfid, 'CATDESC',  'Epoch',       'Time, Centered, Number of milliseconds since the epoch'
cdf_attput, cdfid, 'CATDESC',  'Time_PB5',    'Time of observation in Year, Day, & milliseconds'
cdf_attput, cdfid, 'CATDESC',  'BF1',         'Magnetic field magnitude'
cdf_attput, cdfid, 'CATDESC',  'BGSM',        'Magnetic field vector in GSM cartesian coordinates'
cdf_attput, cdfid, 'CATDESC',  'BGSE',        'Magnetic field vector in GSE cartesian coordinates'
cdf_attput, cdfid, 'CATDESC',  'RANGE',       'Magnetometer Range'
cdf_attput, cdfid, 'CATDESC',  'SPC_MODE',    'S/C operational mode'
cdf_attput, cdfid, 'CATDESC',  'MAG_MODE',    'WIND/MFI operational mode'
cdf_attput, cdfid, 'CATDESC',  'Epoch1',      'Time, Centered, Number of milliseconds since the epoch (1 min)'
cdf_attput, cdfid, 'CATDESC',  'Time1_PB5',   'Time of observation in Year, Day, & milliseconds (1 min)'
cdf_attput, cdfid, 'CATDESC',  'NUM1_PTS_O',  'Outer Sensor number of points in average (1 min)'
cdf_attput, cdfid, 'CATDESC',  'ZERO1_O',     'Outer Sensor Zeros (1 min)'
cdf_attput, cdfid, 'CATDESC',  'SENS1_O',     'Outer Sensor Sensitivities (1 min)'
cdf_attput, cdfid, 'CATDESC',  'AMPL1_O',     'Outer Sensor Amplitude Correction (1 min)'
cdf_attput, cdfid, 'CATDESC',  'ORTH1_O',     'Outer Sensor Orthogonalization Matrix (1 min)'
cdf_attput, cdfid, 'CATDESC',  'PAYLD1_O',    'Outer Sensor Payload Rotation Matrix (1 min)'
cdf_attput, cdfid, 'CATDESC',  'FLAG1_O',     'Outer Sensor Zero Flag (1 min)'
cdf_attput, cdfid, 'CATDESC',  'NUM1_PTS_I',  'Inner Sensor number of points in average (1 min)'
cdf_attput, cdfid, 'CATDESC',  'ZERO1_I',     'Inner Sensor Zeros (1 min)'
cdf_attput, cdfid, 'CATDESC',  'SENS1_I',     'Inner Sensor Sensitivities (1 min)'
cdf_attput, cdfid, 'CATDESC',  'AMPL1_I',     'Inner Sensor Amplitude Correction (1 min)'
cdf_attput, cdfid, 'CATDESC',  'ORTH1_I',     'Inner Sensor Orthogonalization Matrix (1 min)'
cdf_attput, cdfid, 'CATDESC',  'PAYLD1_I',    'Inner Sensor Payload Rotation Matrix (1 min)'
cdf_attput, cdfid, 'CATDESC',  'FLAG1_I',     'Inner Sensor Zero Flag (1 min)'
cdf_attput, cdfid, 'FILLVAL',  'Epoch',       Epoch_fillval
cdf_attput, cdfid, 'FILLVAL',  'Time_PB5',    Time_PB5_fillval
cdf_attput, cdfid, 'FILLVAL',  'BF1',         BF1_fillval
cdf_attput, cdfid, 'FILLVAL',  'BGSM',        BGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'BGSE',        BGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'RANGE',       RANGE_fillval
cdf_attput, cdfid, 'FILLVAL',  'SPC_MODE',    SPC_MODE_fillval
cdf_attput, cdfid, 'FILLVAL',  'MAG_MODE',    MAG_MODE_fillval
cdf_attput, cdfid, 'FILLVAL',  'Epoch1',      Epoch1_fillval
cdf_attput, cdfid, 'FILLVAL',  'Time1_PB5',   Time1_PB5_fillval
cdf_attput, cdfid, 'FILLVAL',  'NUM1_PTS_O',    NUM1_PTS_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'ZERO1_O',       ZERO1_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'SENS1_O',       SENS1_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'AMPL1_O',       AMPL1_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'ORTH1_O',       ORTH1_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'PAYLD1_O',      PAYLD1_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'FLAG1_O',       FLAG1_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'NUM1_PTS_I',    NUM1_PTS_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'ZERO1_I',       ZERO1_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'SENS1_I',       SENS1_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'AMPL1_I',       AMPL1_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'ORTH1_I',       ORTH1_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'PAYLD1_I',      PAYLD1_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'FLAG1_I',       FLAG1_I_fillval
cdf_attput, cdfid, 'LABL_PTR_1',  'Time_PB5',      'label_time'
cdf_attput, cdfid, 'LABL_PTR_1',  'BGSM',          'label_bgsm'
cdf_attput, cdfid, 'LABL_PTR_1',  'BGSE',          'label_bgse'
cdf_attput, cdfid, 'LABL_PTR_1',  'Time1_PB5',     'label_time'
cdf_attput, cdfid, 'UNIT_PTR',  'Time_PB5',      'unit_time'
cdf_attput, cdfid, 'UNIT_PTR',  'Time1_PB5',     'unit_time'
cdf_attput, cdfid, 'FORM_PTR',  'Time_PB5',    'format_time'
cdf_attput, cdfid, 'FORM_PTR',  'Time1_PB5',   'format_time'
cdf_attput, cdfid, 'DEPEND_0',  'Time_PB5',      'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BF1',           'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BGSM',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BGSE',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'RANGE',         'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'SPC_MODE',      'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'MAG_MODE',      'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'Time1_PB5',     'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'NUM1_PTS_O',      'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'ZERO1_O',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'SENS1_O',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'AMPL1_O',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'ORTH1_O',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'PAYLD1_O',        'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'FLAG1_O',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'NUM1_PTS_I',      'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'ZERO1_I',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'SENS1_I',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'AMPL1_I',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'ORTH1_I',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'PAYLD1_I',        'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'FLAG1_I',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_1',  'Time_PB5',    'unit_time'
cdf_attput, cdfid, 'DEPEND_1',  'Time1_PB5',   'unit_time'
cdf_attput, cdfid, 'VAR_TYPE',  'Epoch',         'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Time_PB5',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'BF1',           'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BGSM',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BGSE',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'RANGE',         'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'SPC_MODE',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'MAG_MODE',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Epoch1',        'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Time1_PB5',     'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'NUM1_PTS_O',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'ZERO1_O',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'SENS1_O',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'AMPL1_O',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'ORTH1_O',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'PAYLD1_O',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'FLAG1_O',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'NUM1_PTS_I',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'ZERO1_I',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'SENS1_I',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'AMPL1_I',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'ORTH1_I',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'PAYLD1_I',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'FLAG1_I',         'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_time',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'format_time',   'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'unit_time',     'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_bgsm',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_bgse',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'cartesian',     'metadata'
cdf_attput, cdfid, 'DICT_KEY',  'cartesian',     'ISTP>vector>cartesian'
cdf_attput, cdfid, 'TIME_RES',  'Epoch',       'Variable'
cdf_attput, cdfid, 'TIME_RES',  'Time_PB5',    'Variable'
cdf_attput, cdfid, 'TIME_RES',  'BF1',         'Variable'
cdf_attput, cdfid, 'TIME_RES',  'BGSM',        'Variable'
cdf_attput, cdfid, 'TIME_RES',  'BGSE',        'Variable'
cdf_attput, cdfid, 'TIME_RES',  'RANGE',       'Variable'
cdf_attput, cdfid, 'TIME_RES',  'SPC_MODE',    'Variable'
cdf_attput, cdfid, 'TIME_RES',  'MAG_MODE',    'Variable'
cdf_attput, cdfid, 'TIME_RES',  'Epoch1',      '1 min'
cdf_attput, cdfid, 'TIME_RES',  'Time1_PB5',   '1 min'
cdf_attput, cdfid, 'TIME_RES',  'NUM1_PTS_O',    '1 min'
cdf_attput, cdfid, 'TIME_RES',  'ZERO1_O',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'SENS1_O',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'AMPL1_O',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'ORTH1_O',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'PAYLD1_O',      '1 min'
cdf_attput, cdfid, 'TIME_RES',  'FLAG1_O',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'NUM1_PTS_I',    '1 min'
cdf_attput, cdfid, 'TIME_RES',  'ZERO1_I',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'SENS1_I',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'AMPL1_I',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'ORTH1_I',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'PAYLD1_I',      '1 min'
cdf_attput, cdfid, 'TIME_RES',  'FLAG1_I',       '1 min'
cdf_attput, cdfid, 'VAR_NOTES',  'BF1',        'B field Magnitude'

;- close file
cdf_close, cdfid
;--- end: create and write into cdf file

END