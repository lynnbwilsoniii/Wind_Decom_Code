PRO MFI_WRITE_H0_CDF, DATE=DATE, VERSION=VERSION, CNT=COUNTS, PAY=PAYLOAD, EPOCH_HR=EPOCH_HR, BGSE_HR=BGSE_HR, $
                      BGSM_HR=BGSM_HR, CALIB_HR=CALIB_HR, SW12_HR=SW12_HR, OUTER_MAG_HR=OUTER_MAG_HR, $
                      AT_GSE_HR=AT_GSE_HR, AT_GSM_HR=AT_GSM_HR, LZ_FILES=LZ_FILES, HK_FILES=HK_FILES, $
                      AT_FILES=AT_FILES, OR_FILES=OR_FILES

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

;- read orbit for current (defined by epoch_hr[0]), previous, and next days; interpolate to epoch_hr
res_mfi_or_3days = mfi_or_3days(epoch_data=epoch_hr, epoch_pos=epoch_pos, pos_gse=pos_gse, q_pos_gse=q_pos_gse, $
                                pos_gsm=pos_gsm, q_pos_gsm=q_pos_gsm)
if res_mfi_or_3days eq 0 then begin
   sub_pos_gsm = where(q_pos_gsm eq 0, n_pos_gsm)
   sub_pos_gse = where(q_pos_gse eq 0, n_pos_gse)
   pos_gsm_hr  = dblarr(3, n_epoch_hr)
   pos_gse_hr  = dblarr(3, n_epoch_hr)
   
   if n_pos_gsm gt 3 then begin
      sub_temp   = value_locate(epoch_pos[sub_pos_gsm], epoch_hr, /L64)
      sub_temp  >= 0L
      sub_temp  <= n_pos_gsm - 2L
      sub_pos_gsm_hr = where((abs(epoch_pos[sub_pos_gsm[sub_temp]]    - epoch_hr) lt 15d*60d3) and $
                             (abs(epoch_pos[sub_pos_gsm[sub_temp+1L]] - epoch_hr) lt 15d*60d3), n_pos_hr_gsm_ok)
      if n_pos_hr_gsm_ok gt 0 then begin
         pos_gsm_hr[0,sub_pos_gsm_hr]=spline(epoch_pos[sub_pos_gsm], pos_gsm[0,sub_pos_gsm], epoch_hr[sub_pos_gsm_hr])
         pos_gsm_hr[1,sub_pos_gsm_hr]=spline(epoch_pos[sub_pos_gsm], pos_gsm[1,sub_pos_gsm], epoch_hr[sub_pos_gsm_hr])
         pos_gsm_hr[2,sub_pos_gsm_hr]=spline(epoch_pos[sub_pos_gsm], pos_gsm[2,sub_pos_gsm], epoch_hr[sub_pos_gsm_hr])
      endif
   endif else begin
      n_pos_hr_gsm_ok = 0L
   endelse
   
   if n_pos_gse gt 3 then begin
      sub_temp   = value_locate(epoch_pos[sub_pos_gse], epoch_hr, /L64)
      sub_temp  >= 0L
      sub_temp  <= n_pos_gse - 2L
      sub_pos_gse_hr = where((abs(epoch_pos[sub_pos_gse[sub_temp]]    - epoch_hr) lt 15d*60d3) and $
                             (abs(epoch_pos[sub_pos_gse[sub_temp+1L]] - epoch_hr) lt 15d*60d3), n_pos_hr_gse_ok)
      if n_pos_hr_gse_ok gt 0 then begin
         pos_gse_hr[0,sub_pos_gse_hr]=spline(epoch_pos[sub_pos_gse], pos_gse[0,sub_pos_gse], epoch_hr[sub_pos_gse_hr])
         pos_gse_hr[1,sub_pos_gse_hr]=spline(epoch_pos[sub_pos_gse], pos_gse[1,sub_pos_gse], epoch_hr[sub_pos_gse_hr])
         pos_gse_hr[2,sub_pos_gse_hr]=spline(epoch_pos[sub_pos_gse], pos_gse[2,sub_pos_gse], epoch_hr[sub_pos_gse_hr])
      endif
   endif else begin
      n_pos_hr_gse_ok = 0L
   endelse
endif else begin

   n_pos_hr_gsm_ok = 0L
   n_pos_hr_gse_ok = 0L
endelse
;--- end: additional information

;- variable fill values
Epoch_fillval    = -1d31
Time_PB5_fillval = -1L
NUM_PTS_fillval  = -1L
BF1_fillval      = -1e31
BRMSF1_fillval   = -1e31
BGSM_fillval     = -1e31
BRMSGSM_fillval  = -1e31
BGSE_fillval     = -1e31
BRMSGSE_fillval  = -1e31
DIST_fillval     = -1e31
PGSM_fillval     = -1e31
PGSE_fillval     = -1e31
SGSM_fillval     = -1e31
SGSE_fillval     = -1e31
DB_SC_fillval    = -1e31
TILTANG_fillval  = -1e31
RANGE_I_fillval  = -1e31
RANGE_O_fillval  = -1e31
SPC_MODE_fillval = -1L
MAG_MODE_fillval = -1L
Epoch3_fillval   = -1d31
Time3_PB5_fillval= -1L
NUM3_PTS_fillval = -1L
B3F1_fillval     = -1e31
B3RMSF1_fillval  = -1e31
B3GSM_fillval    = -1e31
B3RMSGSM_fillval = -1e31
B3GSE_fillval    = -1e31
B3RMSGSE_fillval = -1e31
Epoch1_fillval   = -1d31
Time1_PB5_fillval= -1L
NUM1_PTS_fillval = -1L
B1F1_fillval     = -1e31
B1RMSF1_fillval  = -1e31
B1GSM_fillval    = -1e31
B1RMSGSM_fillval = -1e31
B1GSE_fillval    = -1e31
B1RMSGSE_fillval = -1e31
DIST1_fillval    = -1e31
P1GSM_fillval    = -1e31
P1GSE_fillval    = -1e31
S1GSM_fillval    = -1e31
S1GSE_fillval    = -1e31
ZERO_I_fillval   = -1e31
SENS_I_fillval   = -1e31
AMPL_I_fillval   = -1e31
ORTH_I_fillval   = -1d31
PAYLD_I_fillval  = -1d31
FLAG_I_fillval   = -1L
ZERO_O_fillval   = -1e31
SENS_O_fillval   = -1e31
AMPL_O_fillval   = -1e31
ORTH_O_fillval   = -1d31
PAYLD_O_fillval  = -1d31
FLAG_O_fillval   = -1L

;--- 1-minute averages -------------------------------------------
;- create variables and fill with fill values
n_epoch_1m = 24L*60L
epoch    = replicate(epoch_fillval, 1, n_epoch_1m)
time_PB5 = replicate(time_PB5_fillval, 3, n_epoch_1m)
num_pts  = replicate(num_pts_fillval, 1, n_epoch_1m)
bf1      = replicate(bf1_fillval,     1, n_epoch_1m)
brmsf1   = replicate(brmsf1_fillval,  1, n_epoch_1m)
bgsm     = replicate(bgsm_fillval,    3, n_epoch_1m)
brmsgsm  = replicate(brmsgsm_fillval, 3, n_epoch_1m)
bgse     = replicate(bgse_fillval,    3, n_epoch_1m)
brmsgse  = replicate(brmsgse_fillval, 3, n_epoch_1m)
dist     = replicate(dist_fillval, 1, n_epoch_1m)
pgsm     = replicate(pgsm_fillval, 3, n_epoch_1m)
pgse     = replicate(pgse_fillval, 3, n_epoch_1m)
sgsm     = replicate(sgsm_fillval, 3, n_epoch_1m)
sgse     = replicate(sgse_fillval, 3, n_epoch_1m)
db_sc    = replicate(db_sc_fillval, 3, n_epoch_1m)
tiltang  = replicate(tiltang_fillval, 1, n_epoch_1m)
range_i  = replicate(range_i_fillval, 1, n_epoch_1m)
range_o  = replicate(range_o_fillval, 1, n_epoch_1m)
spc_mode = replicate(spc_mode_fillval, 1, n_epoch_1m)
mag_mode = replicate(mag_mode_fillval, 1, n_epoch_1m)

;- epoch, time_PB5
epoch_1m_start = epoch_date_cur + dindgen(n_epoch_1m)*60d3
epoch_1m_end   = epoch_1m_start + 60d3 - 1d
epoch[0,*]     = epoch_1m_start + 30d3
time_PB5[0,*]  = year_cur
time_PB5[1,*]  = doy_cur
time_PB5[2,*]  = long(epoch-epoch_date_cur)

;- necessary subscripts 
sub_epoch_1m_start = value_locate(epoch_hr, epoch_1m_start, /L64)
sub_epoch_1m_end   = value_locate(epoch_hr, epoch_1m_end, /L64)
if n_bgsm_hr_ok gt 0 then begin
   sub_epoch_1m_start_gsm     = value_locate(epoch_hr[sub_bgsm_hr], epoch_1m_start, /L64)
   sub_epoch_1m_end_gsm       = value_locate(epoch_hr[sub_bgsm_hr], epoch_1m_end, /L64)
endif else begin
   sub_epoch_1m_start_gsm     = replicate(0, n_epoch_1m)
   sub_epoch_1m_end_gsm       = replicate(0, n_epoch_1m)
endelse
if n_bgse_hr_ok gt 0 then begin
   sub_epoch_1m_start_gse     = value_locate(epoch_hr[sub_bgse_hr], epoch_1m_start, /L64)
   sub_epoch_1m_end_gse       = value_locate(epoch_hr[sub_bgse_hr], epoch_1m_end, /L64)
endif else begin
   sub_epoch_1m_start_gse     = replicate(0, n_epoch_1m)
   sub_epoch_1m_end_gse       = replicate(0, n_epoch_1m)
endelse
if n_pos_hr_gsm_ok gt 0 then begin
   sub_epoch_1m_start_pos_gsm = value_locate(epoch_hr[sub_pos_gsm_hr], epoch_1m_start, /L64)
   sub_epoch_1m_end_pos_gsm   = value_locate(epoch_hr[sub_pos_gsm_hr], epoch_1m_end, /L64)
endif else begin
   sub_epoch_1m_start_pos_gsm = replicate(0, n_epoch_1m)
   sub_epoch_1m_end_pos_gsm   = replicate(0, n_epoch_1m)
endelse
if n_pos_hr_gse_ok gt 0 then begin
   sub_epoch_1m_start_pos_gse = value_locate(epoch_hr[sub_pos_gse_hr], epoch_1m_start, /L64)
   sub_epoch_1m_end_pos_gse   = value_locate(epoch_hr[sub_pos_gse_hr], epoch_1m_end, /L64)
endif else begin
   sub_epoch_1m_start_pos_gse = replicate(0, n_epoch_1m)
   sub_epoch_1m_end_pos_gse   = replicate(0, n_epoch_1m)
endelse

;- range_i/o, spc_mode, mag_mode, magnetic field, spin, position, distance
for i_e=0, n_epoch_1m-1L do begin
   ;- range_i/o, spacecraft mode, magnetometer mode
   if sub_epoch_1m_start[i_e] lt sub_epoch_1m_end[i_e] then begin
      ;- range_i/o
      sub_outer_temp = where(outer_mag_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]] eq 1, n_outer_temp, $
                             complement=sub_inner_temp, ncomplement=n_inner_temp)
      range_temp     = calib_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]].range
      if n_outer_temp gt 0 then range_o[0, i_e] = mean(range_temp[sub_outer_temp])
      if n_inner_temp gt 0 then range_i[0, i_e] = mean(range_temp[sub_inner_temp])
      ;- spacecraft mode
      spc_mode[0, i_e] = 1L + 4L*round(mean(sw12_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]].tel_rate))
      ;- magnetometer mode
      if ((n_outer_temp gt 0) xor (n_inner_temp gt 0)) then begin
         round_mean_swap  = round(mean(sw12_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]].swap))
         round_mean_mode  = round(mean(sw12_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]].mode))
         round_mean_ou    = round(mean(outer_mag_hr[sub_epoch_1m_start[i_e]+1L:sub_epoch_1m_end[i_e]]))
         mag_mode[0, i_e] = 10L*round_mean_swap + round_mean_mode + $
                            7L*((round_mean_swap eq 0) and (round_mean_mode eq 0) and (round_mean_ou eq 0))
      endif
   endif

   ;- gsm number of point, magnetic field, spin
   if sub_epoch_1m_start_gsm[i_e] lt sub_epoch_1m_end_gsm[i_e] then begin
      ;- number of points
      num_pts[i_e]    = sub_epoch_1m_end_gsm[i_e] - sub_epoch_1m_start_gsm[i_e]
      ;- magnetic field
      moments_bgsm_x  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_1m_start_gsm[i_e]+1L:sub_epoch_1m_end_gsm[i_e]]].x)
      bgsm[   0, i_e] = moments_bgsm_x[0]
      brmsgsm[0, i_e] = sqrt(moments_bgsm_x[1])
      moments_bgsm_y  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_1m_start_gsm[i_e]+1L:sub_epoch_1m_end_gsm[i_e]]].y)
      bgsm[   1, i_e] = moments_bgsm_y[0]
      brmsgsm[1, i_e] = sqrt(moments_bgsm_y[1])
      moments_bgsm_z  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_1m_start_gsm[i_e]+1L:sub_epoch_1m_end_gsm[i_e]]].z)
      bgsm[   2, i_e] = moments_bgsm_z[0]
      brmsgsm[2, i_e] = sqrt(moments_bgsm_z[1])
      ;- spin vector
      sgsm[0, i_e] = mean(at_gsm_hr[sub_bgsm_hr[sub_epoch_1m_start_gsm[i_e]+1L:sub_epoch_1m_end_gsm[i_e]]].x)
      sgsm[1, i_e] = mean(at_gsm_hr[sub_bgsm_hr[sub_epoch_1m_start_gsm[i_e]+1L:sub_epoch_1m_end_gsm[i_e]]].y)
      sgsm[2, i_e] = mean(at_gsm_hr[sub_bgsm_hr[sub_epoch_1m_start_gsm[i_e]+1L:sub_epoch_1m_end_gsm[i_e]]].z)
   endif

   ;- gse number of point, magnetic field, spin
   if sub_epoch_1m_start_gse[i_e] lt sub_epoch_1m_end_gse[i_e] then begin
      ;- number of points
      if num_pts[i_e] eq NUM_PTS_fillval $
         then num_pts[i_e]  = sub_epoch_1m_end_gse[i_e] - sub_epoch_1m_start_gse[i_e] $
         else num_pts[i_e] <= sub_epoch_1m_end_gse[i_e] - sub_epoch_1m_start_gse[i_e]
      ;- magnetic field
      moments_btot    = moment(btot_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].value)
      bf1[   0, i_e]  = moments_btot[0]
      brmsf1[0, i_e]  = sqrt(moments_btot[1])
      moments_bgse_x  = moment(bgse_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].x)
      bgse[   0, i_e] = moments_bgse_x[0]
      brmsgse[0, i_e] = sqrt(moments_bgse_x[1])
      moments_bgse_y  = moment(bgse_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].y)
      bgse[   1, i_e] = moments_bgse_y[0]
      brmsgse[1, i_e] = sqrt(moments_bgse_y[1])
      moments_bgse_z  = moment(bgse_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].z)
      bgse[   2, i_e] = moments_bgse_z[0]
      brmsgse[2, i_e] = sqrt(moments_bgse_z[1])
      ;- spin vector
      sgse[0, i_e] = mean(at_gse_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].x)
      sgse[1, i_e] = mean(at_gse_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].y)
      sgse[2, i_e] = mean(at_gse_hr[sub_bgse_hr[sub_epoch_1m_start_gse[i_e]+1L:sub_epoch_1m_end_gse[i_e]]].z)
   endif
   
   ;- gsm position
   if sub_epoch_1m_start_pos_gsm[i_e] lt sub_epoch_1m_end_pos_gsm[i_e] then begin
      epoch_temp = mean(epoch_hr[sub_pos_gsm_hr[sub_epoch_1m_start_pos_gsm[i_e]+1L:sub_epoch_1m_end_pos_gsm[i_e]]])
      if abs(epoch_temp-epoch[i_e]) lt 6d3 then begin
         pgsm[0,i_e] = mean(pos_gsm_hr[0,sub_pos_gsm_hr[sub_epoch_1m_start_pos_gsm[i_e]+1L:$
                                                        sub_epoch_1m_end_pos_gsm[i_e]]])
         pgsm[1,i_e] = mean(pos_gsm_hr[1,sub_pos_gsm_hr[sub_epoch_1m_start_pos_gsm[i_e]+1L:$
                                                        sub_epoch_1m_end_pos_gsm[i_e]]])
         pgsm[2,i_e] = mean(pos_gsm_hr[2,sub_pos_gsm_hr[sub_epoch_1m_start_pos_gsm[i_e]+1L:$
                                                        sub_epoch_1m_end_pos_gsm[i_e]]])
      endif else begin
         pgsm[0,i_e] = spline(epoch_pos[sub_pos_gsm], pos_gsm[0,sub_pos_gsm], epoch[i_e])
         pgsm[1,i_e] = spline(epoch_pos[sub_pos_gsm], pos_gsm[1,sub_pos_gsm], epoch[i_e])
         pgsm[2,i_e] = spline(epoch_pos[sub_pos_gsm], pos_gsm[2,sub_pos_gsm], epoch[i_e])
      endelse
   endif

   ;- gse position, distance
   if sub_epoch_1m_start_pos_gse[i_e] lt sub_epoch_1m_end_pos_gse[i_e] then begin
      epoch_temp = mean(epoch_hr[sub_pos_gse_hr[sub_epoch_1m_start_pos_gse[i_e]+1L:sub_epoch_1m_end_pos_gse[i_e]]])
      if abs(epoch_temp-epoch[i_e]) lt 6d3 then begin
         pgse[0,i_e] = mean(pos_gse_hr[0,sub_pos_gse_hr[sub_epoch_1m_start_pos_gse[i_e]+1L:$
                                                        sub_epoch_1m_end_pos_gse[i_e]]])
         pgse[1,i_e] = mean(pos_gse_hr[1,sub_pos_gse_hr[sub_epoch_1m_start_pos_gse[i_e]+1L:$
                                                        sub_epoch_1m_end_pos_gse[i_e]]])
         pgse[2,i_e] = mean(pos_gse_hr[2,sub_pos_gse_hr[sub_epoch_1m_start_pos_gse[i_e]+1L:$
                                                        sub_epoch_1m_end_pos_gse[i_e]]])
      endif else begin
         pgse[0,i_e] = spline(epoch_pos[sub_pos_gse], pos_gse[0,sub_pos_gse], epoch[i_e])
         pgse[1,i_e] = spline(epoch_pos[sub_pos_gse], pos_gse[1,sub_pos_gse], epoch[i_e])
         pgse[2,i_e] = spline(epoch_pos[sub_pos_gse], pos_gse[2,sub_pos_gse], epoch[i_e])
      endelse
      dist[0,i_e]=sqrt(pgse[0,i_e]^2d + pgse[1,i_e]^2d + pgse[2,i_e]^2d)
   endif
endfor

;- tilt angle
tiltang[0,*] = mfi_tiltang(epoch)
;--- end: 1-minute averages --------------------------------------

;--- 3-second averages  ------------------------------------------
;- create variables and fill with fill values
n_epoch_3s= 24L*3600L/3L
epoch3    = replicate(epoch3_fillval, 1, n_epoch_3s)
time3_PB5 = replicate(time3_PB5_fillval, 3, n_epoch_3s)
num3_pts  = replicate(num3_pts_fillval, 1, n_epoch_3s)
b3f1      = replicate(b3f1_fillval,     1, n_epoch_3s)
b3rmsf1   = replicate(b3rmsf1_fillval,  1, n_epoch_3s)
b3gsm     = replicate(b3gsm_fillval,    3, n_epoch_3s)
b3rmsgsm  = replicate(b3rmsgsm_fillval, 3, n_epoch_3s)
b3gse     = replicate(b3gse_fillval,    3, n_epoch_3s)
b3rmsgse  = replicate(b3rmsgse_fillval, 3, n_epoch_3s)

;- epoch, time_PB5
epoch_3s_start = epoch_date_cur + dindgen(n_epoch_3s)*3d3
epoch_3s_end   = epoch_3s_start + 3d3 - 1d
epoch3[0,*]    = epoch_3s_start + 1.5d3
time3_PB5[0,*] = year_cur
time3_PB5[1,*] = doy_cur
time3_PB5[2,*] = long(epoch3-epoch_date_cur)

;- necessary subscripts 
if n_bgsm_hr_ok gt 0 then begin
   sub_epoch_3s_start_gsm = value_locate(epoch_hr[sub_bgsm_hr], epoch_3s_start, /L64)
   sub_epoch_3s_end_gsm   = value_locate(epoch_hr[sub_bgsm_hr], epoch_3s_end, /L64)
endif else begin
   sub_epoch_3s_start_gsm = replicate(0, n_epoch_3s)
   sub_epoch_3s_end_gsm   = replicate(0, n_epoch_3s)
endelse
if n_bgse_hr_ok gt 0 then begin
   sub_epoch_3s_start_gse = value_locate(epoch_hr[sub_bgse_hr], epoch_3s_start, /L64)
   sub_epoch_3s_end_gse   = value_locate(epoch_hr[sub_bgse_hr], epoch_3s_end, /L64)
endif else begin
   sub_epoch_3s_start_gse = replicate(0, n_epoch_3s)
   sub_epoch_3s_end_gse   = replicate(0, n_epoch_3s)
endelse

;- magnetic field
for i_e=0, n_epoch_3s-1L do begin
   ;- gsm number of points, magnetic field
   if sub_epoch_3s_start_gsm[i_e] lt sub_epoch_3s_end_gsm[i_e] then begin
      ;- number of points
      num3_pts[i_e]    = sub_epoch_3s_end_gsm[i_e] - sub_epoch_3s_start_gsm[i_e]
      ;- magnetic field
      moments_b3gsm_x  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_3s_start_gsm[i_e]+1L:sub_epoch_3s_end_gsm[i_e]]].x)
      b3gsm[   0, i_e] = moments_b3gsm_x[0]
      b3rmsgsm[0, i_e] = sqrt(moments_b3gsm_x[1])
      moments_b3gsm_y  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_3s_start_gsm[i_e]+1L:sub_epoch_3s_end_gsm[i_e]]].y)
      b3gsm[   1, i_e] = moments_b3gsm_y[0]
      b3rmsgsm[1, i_e] = sqrt(moments_b3gsm_y[1])
      moments_b3gsm_z  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_3s_start_gsm[i_e]+1L:sub_epoch_3s_end_gsm[i_e]]].z)
      b3gsm[   2, i_e] = moments_b3gsm_z[0]
      b3rmsgsm[2, i_e] = sqrt(moments_b3gsm_z[1])
   endif

   ;- gse number of points, magnetic field
   if sub_epoch_3s_start_gse[i_e] lt sub_epoch_3s_end_gse[i_e] then begin
      ;- number of points
      if num3_pts[i_e] eq NUM3_PTS_fillval $
         then num3_pts[i_e]  = sub_epoch_3s_end_gse[i_e] - sub_epoch_3s_start_gse[i_e] $
         else num3_pts[i_e] <= sub_epoch_3s_end_gse[i_e] - sub_epoch_3s_start_gse[i_e]
      ;- magnetic field
      moments_b3tot    = moment(btot_hr[sub_bgse_hr[sub_epoch_3s_start_gse[i_e]+1L:sub_epoch_3s_end_gse[i_e]]].value)
      b3f1[   0, i_e]  = moments_b3tot[0]
      b3rmsf1[0, i_e]  = sqrt(moments_b3tot[1])
      moments_b3gse_x  = moment(bgse_hr[sub_bgse_hr[sub_epoch_3s_start_gse[i_e]+1L:sub_epoch_3s_end_gse[i_e]]].x)
      b3gse[   0, i_e] = moments_b3gse_x[0]
      b3rmsgse[0, i_e] = sqrt(moments_b3gse_x[1])
      moments_b3gse_y  = moment(bgse_hr[sub_bgse_hr[sub_epoch_3s_start_gse[i_e]+1L:sub_epoch_3s_end_gse[i_e]]].y)
      b3gse[   1, i_e] = moments_b3gse_y[0]
      b3rmsgse[1, i_e] = sqrt(moments_b3gse_y[1])
      moments_b3gse_z  = moment(bgse_hr[sub_bgse_hr[sub_epoch_3s_start_gse[i_e]+1L:sub_epoch_3s_end_gse[i_e]]].z)
      b3gse[   2, i_e] = moments_b3gse_z[0]
      b3rmsgse[2, i_e] = sqrt(moments_b3gse_z[1])
   endif
endfor
;--- end: 3-second averages --------------------------------------

;--- 1-hour averages ---------------------------------------------
;- create variables and fill with fill values
n_epoch_1h= 24L
epoch1    = replicate(epoch1_fillval, 1, n_epoch_1h)
time1_PB5 = replicate(time1_PB5_fillval, 3, n_epoch_1h)
num1_pts  = replicate(num1_pts_fillval, 1, n_epoch_1h)
b1f1      = replicate(b1f1_fillval,     1, n_epoch_1h)
b1rmsf1   = replicate(b1rmsf1_fillval,  1, n_epoch_1h)
b1gsm     = replicate(b1gsm_fillval,    3, n_epoch_1h)
b1rmsgsm  = replicate(b1rmsgsm_fillval, 3, n_epoch_1h)
b1gse     = replicate(b1gse_fillval,    3, n_epoch_1h)
b1rmsgse  = replicate(b1rmsgse_fillval, 3, n_epoch_1h)
dist1     = replicate(dist1_fillval, 1, n_epoch_1h)
p1gsm     = replicate(p1gsm_fillval, 3, n_epoch_1h)
p1gse     = replicate(p1gse_fillval, 3, n_epoch_1h)
s1gsm     = replicate(s1gsm_fillval, 3, n_epoch_1h)
s1gse     = replicate(s1gse_fillval, 3, n_epoch_1h)
zero_i    = replicate(zero_i_fillval, 3, 8)
sens_i    = replicate(sens_i_fillval, 3, 8)
ampl_i    = replicate(ampl_i_fillval, 3, 8)
orth_i    = replicate(orth_i_fillval, 3, 3)
payld_i   = replicate(payld_i_fillval, 3, 3)
flag_i    = flag_i_fillval
zero_o    = replicate(zero_o_fillval, 3, 8)
sens_o    = replicate(sens_o_fillval, 3, 8)
ampl_o    = replicate(ampl_o_fillval, 3, 8)
orth_o    = replicate(orth_o_fillval, 3, 3)
payld_o   = replicate(payld_o_fillval, 3, 3)
flag_o    = flag_o_fillval

;- epoch time_PB5
epoch_1h_start  = epoch_date_cur + dindgen(n_epoch_1h)*3600d3
epoch_1h_end    = epoch_1h_start + 3600d3 - 1d
epoch1[0,*]     = epoch_1h_start + 1800d3
time1_PB5[0,*]  = year_cur
time1_PB5[1,*]  = doy_cur
time1_PB5[2,*]  = long(epoch1-epoch_date_cur)

;- necessary subscripts 
sub_epoch_1h_start = value_locate(epoch_hr, epoch_1h_start, /L64)
sub_epoch_1h_end   = value_locate(epoch_hr, epoch_1h_end, /L64)
if n_bgsm_hr_ok gt 0 then begin
   sub_epoch_1h_start_gsm     = value_locate(epoch_hr[sub_bgsm_hr], epoch_1h_start, /L64)
   sub_epoch_1h_end_gsm       = value_locate(epoch_hr[sub_bgsm_hr], epoch_1h_end, /L64)
endif else begin
   sub_epoch_1h_start_gsm     = replicate(0, n_epoch_1h)
   sub_epoch_1h_end_gsm       = replicate(0, n_epoch_1h)
endelse
if n_bgse_hr_ok gt 0 then begin
   sub_epoch_1h_start_gse     = value_locate(epoch_hr[sub_bgse_hr], epoch_1h_start, /L64)
   sub_epoch_1h_end_gse       = value_locate(epoch_hr[sub_bgse_hr], epoch_1h_end, /L64)
endif else begin
   sub_epoch_1h_start_gse     = replicate(0, n_epoch_1h)
   sub_epoch_1h_end_gse       = replicate(0, n_epoch_1h)
endelse
if n_pos_hr_gsm_ok gt 0 then begin
   sub_epoch_1h_start_pos_gsm = value_locate(epoch_hr[sub_pos_gsm_hr], epoch_1h_start, /L64)
   sub_epoch_1h_end_pos_gsm   = value_locate(epoch_hr[sub_pos_gsm_hr], epoch_1h_end, /L64)
endif else begin
   sub_epoch_1h_start_pos_gsm = replicate(0, n_epoch_1h)
   sub_epoch_1h_end_pos_gsm   = replicate(0, n_epoch_1h)
endelse
if n_pos_hr_gse_ok gt 0 then begin
   sub_epoch_1h_start_pos_gse = value_locate(epoch_hr[sub_pos_gse_hr], epoch_1h_start, /L64)
   sub_epoch_1h_end_pos_gse   = value_locate(epoch_hr[sub_pos_gse_hr], epoch_1h_end, /L64)
endif else begin
   sub_epoch_1h_start_pos_gse = replicate(0, n_epoch_1h)
   sub_epoch_1h_end_pos_gse   = replicate(0, n_epoch_1h)
endelse

;- magnetic field, spin, position, distance
for i_e=0, n_epoch_1h-1L do begin
   ;- gsm number of points, magnetic field, spin
   if sub_epoch_1h_start_gsm[i_e] lt sub_epoch_1h_end_gsm[i_e] then begin
      ;- number of points
      num1_pts[i_e]    = sub_epoch_1h_end_gsm[i_e] - sub_epoch_1h_start_gsm[i_e]
      ;- magnetic field
      moments_b1gsm_x  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_1h_start_gsm[i_e]+1L:sub_epoch_1h_end_gsm[i_e]]].x)
      b1gsm[   0, i_e] = moments_b1gsm_x[0]
      b1rmsgsm[0, i_e] = sqrt(moments_b1gsm_x[1])
      moments_b1gsm_y  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_1h_start_gsm[i_e]+1L:sub_epoch_1h_end_gsm[i_e]]].y)
      b1gsm[   1, i_e] = moments_b1gsm_y[0]
      b1rmsgsm[1, i_e] = sqrt(moments_b1gsm_y[1])
      moments_b1gsm_z  = moment(bgsm_hr[sub_bgsm_hr[sub_epoch_1h_start_gsm[i_e]+1L:sub_epoch_1h_end_gsm[i_e]]].z)
      b1gsm[   2, i_e] = moments_b1gsm_z[0]
      b1rmsgsm[2, i_e] = sqrt(moments_b1gsm_z[1])
      ;- spin vector
      s1gsm[0, i_e] = mean(at_gsm_hr[sub_bgsm_hr[sub_epoch_1h_start_gsm[i_e]+1L:sub_epoch_1h_end_gsm[i_e]]].x)
      s1gsm[1, i_e] = mean(at_gsm_hr[sub_bgsm_hr[sub_epoch_1h_start_gsm[i_e]+1L:sub_epoch_1h_end_gsm[i_e]]].y)
      s1gsm[2, i_e] = mean(at_gsm_hr[sub_bgsm_hr[sub_epoch_1h_start_gsm[i_e]+1L:sub_epoch_1h_end_gsm[i_e]]].z)
   endif

   ;- gse number of points, magnetic field, spin
   if sub_epoch_1h_start_gse[i_e] lt sub_epoch_1h_end_gse[i_e] then begin
      ;- number of points
      if num1_pts[i_e] eq NUM1_PTS_fillval $
         then num1_pts[i_e]  = sub_epoch_1h_end_gse[i_e] - sub_epoch_1h_start_gse[i_e] $
         else num1_pts[i_e] <= sub_epoch_1h_end_gse[i_e] - sub_epoch_1h_start_gse[i_e]
      ;- magnetic field
      moments_b1tot    = moment(btot_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].value)
      b1f1[   0, i_e]  = moments_b1tot[0]
      b1rmsf1[0, i_e]  = sqrt(moments_b1tot[1])
      moments_b1gse_x  = moment(bgse_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].x)
      b1gse[   0, i_e] = moments_b1gse_x[0]
      b1rmsgse[0, i_e] = sqrt(moments_b1gse_x[1])
      moments_b1gse_y  = moment(bgse_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].y)
      b1gse[   1, i_e] = moments_b1gse_y[0]
      b1rmsgse[1, i_e] = sqrt(moments_b1gse_y[1])
      moments_b1gse_z  = moment(bgse_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].z)
      b1gse[   2, i_e] = moments_b1gse_z[0]
      b1rmsgse[2, i_e] = sqrt(moments_b1gse_z[1])
      ;- spin vector
      s1gse[0, i_e] = mean(at_gse_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].x)
      s1gse[1, i_e] = mean(at_gse_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].y)
      s1gse[2, i_e] = mean(at_gse_hr[sub_bgse_hr[sub_epoch_1h_start_gse[i_e]+1L:sub_epoch_1h_end_gse[i_e]]].z)
   endif

   ;- gsm position
   if sub_epoch_1h_start_pos_gsm[i_e] lt sub_epoch_1h_end_pos_gsm[i_e] then begin
      epoch_temp = mean(epoch_hr[sub_pos_gsm_hr[sub_epoch_1h_start_pos_gsm[i_e]+1L:sub_epoch_1h_end_pos_gsm[i_e]]])
      if abs(epoch_temp-epoch1[i_e]) lt 6d*60d3 then begin
         p1gsm[0,i_e]=mean(pos_gsm_hr[0,sub_pos_gsm_hr[sub_epoch_1h_start_pos_gsm[i_e]+1L:$
                                        sub_epoch_1h_end_pos_gsm[i_e]]])
         p1gsm[1,i_e]=mean(pos_gsm_hr[1,sub_pos_gsm_hr[sub_epoch_1h_start_pos_gsm[i_e]+1L:$
                                        sub_epoch_1h_end_pos_gsm[i_e]]])
         p1gsm[2,i_e]=mean(pos_gsm_hr[2,sub_pos_gsm_hr[sub_epoch_1h_start_pos_gsm[i_e]+1L:$
                                        sub_epoch_1h_end_pos_gsm[i_e]]])
      endif else begin
         p1gsm[0,i_e] = spline(epoch_pos[sub_pos_gsm], pos_gsm[0,sub_pos_gsm], epoch1[i_e])
         p1gsm[1,i_e] = spline(epoch_pos[sub_pos_gsm], pos_gsm[1,sub_pos_gsm], epoch1[i_e])
         p1gsm[2,i_e] = spline(epoch_pos[sub_pos_gsm], pos_gsm[2,sub_pos_gsm], epoch1[i_e])
      endelse
   endif

   ;- gse position, distance
   if sub_epoch_1h_start_pos_gse[i_e] lt sub_epoch_1h_end_pos_gse[i_e] then begin
      epoch_temp = mean(epoch_hr[sub_pos_gse_hr[sub_epoch_1h_start_pos_gse[i_e]+1L:sub_epoch_1h_end_pos_gse[i_e]]])
      if abs(epoch_temp-epoch1[i_e]) lt 6d*60d3 then begin
         p1gse[0,i_e] = mean(pos_gse_hr[0,sub_pos_gse_hr[sub_epoch_1h_start_pos_gse[i_e]+1L:$
                                                         sub_epoch_1h_end_pos_gse[i_e]]])
         p1gse[1,i_e] = mean(pos_gse_hr[1,sub_pos_gse_hr[sub_epoch_1h_start_pos_gse[i_e]+1L:$
                                                         sub_epoch_1h_end_pos_gse[i_e]]])
         p1gse[2,i_e] = mean(pos_gse_hr[2,sub_pos_gse_hr[sub_epoch_1h_start_pos_gse[i_e]+1L:$
                                                         sub_epoch_1h_end_pos_gse[i_e]]])
      endif else begin
         p1gse[0,i_e] = spline(epoch_pos[sub_pos_gse], pos_gse[0,sub_pos_gse], epoch1[i_e])
         p1gse[1,i_e] = spline(epoch_pos[sub_pos_gse], pos_gse[1,sub_pos_gse], epoch1[i_e])
         p1gse[2,i_e] = spline(epoch_pos[sub_pos_gse], pos_gse[2,sub_pos_gse], epoch1[i_e])
      endelse
      dist1[0,i_e]=sqrt(p1gse[0,i_e]^2d + p1gse[1,i_e]^2d + p1gse[2,i_e]^2d)
   endif
endfor

;- zero, sens, ampl, orth, payld, flag
sub_hr_ou = where(outer_mag_hr eq 1, n_hr_ou, complement=sub_hr_in, ncomplement=n_hr_in)
if n_hr_ou gt 0 then begin
   calib_hr_ou   = calib_hr[sub_hr_ou]
   ;- sens, zero
   for i_r=0, 7 do begin
      sub_cur_range_ou = where(calib_hr_ou.range eq i_r, n_cur_range_ou)
      if n_cur_range_ou gt 0 then begin
         ;- sens
         sens_o[0, i_r] = mean(calib_hr_ou[sub_cur_range_ou].sensx)
         sens_o[1, i_r] = mean(calib_hr_ou[sub_cur_range_ou].sensy)
         sens_o[2, i_r] = mean(calib_hr_ou[sub_cur_range_ou].sensz)
         ;- zero
         zero_o[0, i_r] = mean(calib_hr_ou[sub_cur_range_ou].zerox)
         zero_o[1, i_r] = mean(calib_hr_ou[sub_cur_range_ou].zeroy)
         zero_o[2, i_r] = mean(calib_hr_ou[sub_cur_range_ou].zeroz)
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
   orth_o[0,0] =  (matrix_temp[1,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[1,2])/determ_matrix_temp
   orth_o[1,0] = -(matrix_temp[1,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[1,2])/determ_matrix_temp
   orth_o[2,0] =  (matrix_temp[1,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[1,1])/determ_matrix_temp
   orth_o[0,1] = -(matrix_temp[0,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[0,2])/determ_matrix_temp
   orth_o[1,1] =  (matrix_temp[0,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[0,2])/determ_matrix_temp
   orth_o[2,1] = -(matrix_temp[0,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[0,1])/determ_matrix_temp
   orth_o[0,2] =  (matrix_temp[0,1]*matrix_temp[1,2] - matrix_temp[1,1]*matrix_temp[0,2])/determ_matrix_temp
   orth_o[1,2] = -(matrix_temp[0,0]*matrix_temp[1,2] - matrix_temp[1,0]*matrix_temp[0,2])/determ_matrix_temp
   orth_o[2,2] =  (matrix_temp[0,0]*matrix_temp[1,1] - matrix_temp[1,0]*matrix_temp[0,1])/determ_matrix_temp
   ;- payld
   payld_o     = [[1d, 0d, 0d], [0d, 1d, 0d], [0d, 0d, -1d]] 
   ;- ampl
   ampl_o[*]   = 1d
   ;- flag
   flag_o      = 4L + round(mean(calib_hr_ou.qzeroz ne 1))
endif
if n_hr_in gt 0 then begin
   calib_hr_in = calib_hr[sub_hr_in]
   ;- sens, zero
   for i_r=0, 7 do begin
      sub_cur_range_in = where(calib_hr_in.range eq i_r, n_cur_range_in)
      if n_cur_range_in gt 0 then begin
         ;- sens
         sens_i[0, i_r] = mean(calib_hr_in[sub_cur_range_in].sensx)
         sens_i[1, i_r] = mean(calib_hr_in[sub_cur_range_in].sensy)
         sens_i[2, i_r] = mean(calib_hr_in[sub_cur_range_in].sensz)
         ;- zero
         zero_i[0, i_r] = mean(calib_hr_in[sub_cur_range_in].zerox)
         zero_i[1, i_r] = mean(calib_hr_in[sub_cur_range_in].zeroy)
         zero_i[2, i_r] = mean(calib_hr_in[sub_cur_range_in].zeroz)
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
   orth_i[0,0] =  (matrix_temp[1,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[1,2])/determ_matrix_temp
   orth_i[1,0] = -(matrix_temp[1,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[1,2])/determ_matrix_temp
   orth_i[2,0] =  (matrix_temp[1,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[1,1])/determ_matrix_temp
   orth_i[0,1] = -(matrix_temp[0,1]*matrix_temp[2,2] - matrix_temp[2,1]*matrix_temp[0,2])/determ_matrix_temp
   orth_i[1,1] =  (matrix_temp[0,0]*matrix_temp[2,2] - matrix_temp[2,0]*matrix_temp[0,2])/determ_matrix_temp
   orth_i[2,1] = -(matrix_temp[0,0]*matrix_temp[2,1] - matrix_temp[2,0]*matrix_temp[0,1])/determ_matrix_temp
   orth_i[0,2] =  (matrix_temp[0,1]*matrix_temp[1,2] - matrix_temp[1,1]*matrix_temp[0,2])/determ_matrix_temp
   orth_i[1,2] = -(matrix_temp[0,0]*matrix_temp[1,2] - matrix_temp[1,0]*matrix_temp[0,2])/determ_matrix_temp
   orth_i[2,2] =  (matrix_temp[0,0]*matrix_temp[1,1] - matrix_temp[1,0]*matrix_temp[0,1])/determ_matrix_temp
   ;- payld
   payld_i     = [[1d, 0d, 0d], [0d, 1d, 0d], [0d, 0d, -1d]] 
   ;- ampl
   ampl_i[*]   = 1d
   ;- flag
   flag_i      = 4L + round(mean(calib_hr_in.qzeroz ne 1))
endif
;--- end: 1-hour averages ----------------------------------------

;--- create and write into cdf file
;- create file
if keyword_set(counts)  then prefix_temp=prefix_cnts
if keyword_set(payload) then prefix_temp=prefix_payld
cdf_set_cdf27_backward_compatible, /yes
cdf_filename = h0_path + h0_prefix + mfi_prefix + (strsplit(string(date), /extract))[0] + $
               '_v' + string(version, format='(I02)') + '.cdf'
cdfid = cdf_create(cdf_filename, [3], /clobber, /single_file, /col_major, /network_encoding, /host_decoding)

;- create variables
varid = cdf_varcreate(cdfid, 'Epoch',    [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_epoch, /zvar)
varid = cdf_varcreate(cdfid, 'Time_PB5', [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'NUM_PTS',  [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'BF1',      [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BRMSF1',   [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BGSM',     [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BRMSGSM',  [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BGSE',     [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'BRMSGSE',  [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'DIST',     [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'PGSM',     [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'PGSE',     [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SGSM',     [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SGSE',     [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'DB_SC',    [1], dim=[3], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'TILTANG',  [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'RANGE_I',  [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'RANGE_O',  [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SPC_MODE', [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'MAG_MODE', [0], dim=[1], /rec_vary, allocate=n_epoch_1m, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'Epoch3',   [0], dim=[1], /rec_vary, allocate=n_epoch_3s, /cdf_epoch, /zvar)
varid = cdf_varcreate(cdfid, 'Time3_PB5',[1], dim=[3], /rec_vary, allocate=n_epoch_3s, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'NUM3_PTS', [0], dim=[1], /rec_vary, allocate=n_epoch_3s, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'B3F1',     [0], dim=[1], /rec_vary, allocate=n_epoch_3s, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B3RMSF1',  [0], dim=[1], /rec_vary, allocate=n_epoch_3s, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B3GSM',    [1], dim=[3], /rec_vary, allocate=n_epoch_3s, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B3RMSGSM', [1], dim=[3], /rec_vary, allocate=n_epoch_3s, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B3GSE',    [1], dim=[3], /rec_vary, allocate=n_epoch_3s, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B3RMSGSE', [1], dim=[3], /rec_vary, allocate=n_epoch_3s, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'Epoch1',   [0], dim=[1], /rec_vary, allocate=n_epoch_1h, /cdf_epoch, /zvar)
varid = cdf_varcreate(cdfid, 'Time1_PB5',[1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'NUM1_PTS', [0], dim=[1], /rec_vary, allocate=n_epoch_1h, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'B1F1',     [0], dim=[1], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B1RMSF1',  [0], dim=[1], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B1GSM',    [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B1RMSGSM', [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B1GSE',    [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'B1RMSGSE', [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'DIST1',    [0], dim=[1], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'P1GSM',    [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'P1GSE',    [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'S1GSM',    [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'S1GSE',    [1], dim=[3], /rec_vary, allocate=n_epoch_1h, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'ZERO_I',   [1,1], dim=[3,8], /rec_novary, allocate=1, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SENS_I',   [1,1], dim=[3,8], /rec_novary, allocate=1, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'AMPL_I',   [1,1], dim=[3,8], /rec_novary, allocate=1, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'ORTH_I',   [1,1], dim=[3,3], /rec_novary, allocate=1, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'PAYLD_I',  [1,1], dim=[3,3], /rec_novary, allocate=1, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'FLAG_I',   [0],   dim=[1],   /rec_novary, allocate=1, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'ZERO_O',   [1,1], dim=[3,8], /rec_novary, allocate=1, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'SENS_O',   [1,1], dim=[3,8], /rec_novary, allocate=1, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'AMPL_O',   [1,1], dim=[3,8], /rec_novary, allocate=1, /cdf_real4, /zvar)
varid = cdf_varcreate(cdfid, 'ORTH_O',   [1,1], dim=[3,3], /rec_novary, allocate=1, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'PAYLD_O',  [1,1], dim=[3,3], /rec_novary, allocate=1, /cdf_real8, /zvar)
varid = cdf_varcreate(cdfid, 'FLAG_O',   [0],   dim=[1],   /rec_novary, allocate=1, /cdf_int4, /zvar)
varid = cdf_varcreate(cdfid, 'label_time',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=27, /zvar)
varid = cdf_varcreate(cdfid, 'format_time', [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=2,  /zvar)
varid = cdf_varcreate(cdfid, 'unit_time',   [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=4,  /zvar)
varid = cdf_varcreate(cdfid, 'label_bgsm',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=8,  /zvar)
varid = cdf_varcreate(cdfid, 'label_bgse',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=8,  /zvar)
varid = cdf_varcreate(cdfid, 'label_bgsmr', [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=12, /zvar)
varid = cdf_varcreate(cdfid, 'label_bgser', [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=12, /zvar)
varid = cdf_varcreate(cdfid, 'label_dbsc',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=12, /zvar)
varid = cdf_varcreate(cdfid, 'label_pgsm',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=7,  /zvar)
varid = cdf_varcreate(cdfid, 'label_pgse',  [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=7,  /zvar)
varid = cdf_varcreate(cdfid, 'cartesian',   [1], dim=[3], /rec_novary, allocate=1, /cdf_char, numelem=11, /zvar)

;- set variable padvalues equal to fill values
cdf_control, cdfid, variable='Epoch',    set_padvalue=Epoch_fillval
cdf_control, cdfid, variable='Time_PB5', set_padvalue=Time_PB5_fillval
cdf_control, cdfid, variable='NUM_PTS',  set_padvalue=NUM_PTS_fillval
cdf_control, cdfid, variable='BF1',      set_padvalue=BF1_fillval
cdf_control, cdfid, variable='BRMSF1',   set_padvalue=BRMSF1_fillval
cdf_control, cdfid, variable='BGSM',     set_padvalue=BGSM_fillval
cdf_control, cdfid, variable='BRMSGSM',  set_padvalue=BRMSGSM_fillval
cdf_control, cdfid, variable='BGSE',     set_padvalue=BGSE_fillval
cdf_control, cdfid, variable='BRMSGSE',  set_padvalue=BRMSGSE_fillval
cdf_control, cdfid, variable='DIST',     set_padvalue=DIST_fillval
cdf_control, cdfid, variable='PGSM',     set_padvalue=PGSM_fillval
cdf_control, cdfid, variable='PGSE',     set_padvalue=PGSE_fillval
cdf_control, cdfid, variable='SGSM',     set_padvalue=SGSM_fillval
cdf_control, cdfid, variable='SGSE',     set_padvalue=SGSE_fillval
cdf_control, cdfid, variable='DB_SC',    set_padvalue=DB_SC_fillval
cdf_control, cdfid, variable='TILTANG',  set_padvalue=TILTANG_fillval
cdf_control, cdfid, variable='RANGE_I',  set_padvalue=RANGE_I_fillval
cdf_control, cdfid, variable='RANGE_O',  set_padvalue=RANGE_O_fillval
cdf_control, cdfid, variable='SPC_MODE', set_padvalue=SPC_MODE_fillval
cdf_control, cdfid, variable='MAG_MODE', set_padvalue=MAG_MODE_fillval
cdf_control, cdfid, variable='Epoch3',   set_padvalue=Epoch3_fillval
cdf_control, cdfid, variable='Time3_PB5',set_padvalue=Time3_PB5_fillval
cdf_control, cdfid, variable='NUM3_PTS', set_padvalue=NUM3_PTS_fillval
cdf_control, cdfid, variable='B3F1',     set_padvalue=B3F1_fillval
cdf_control, cdfid, variable='B3RMSF1',  set_padvalue=B3RMSF1_fillval
cdf_control, cdfid, variable='B3GSM',    set_padvalue=B3GSM_fillval
cdf_control, cdfid, variable='B3RMSGSM', set_padvalue=B3RMSGSM_fillval
cdf_control, cdfid, variable='B3GSE',    set_padvalue=B3GSE_fillval
cdf_control, cdfid, variable='B3RMSGSE', set_padvalue=B3RMSGSE_fillval
cdf_control, cdfid, variable='Epoch1',   set_padvalue=Epoch1_fillval
cdf_control, cdfid, variable='Time1_PB5',set_padvalue=Time1_PB5_fillval
cdf_control, cdfid, variable='NUM1_PTS', set_padvalue=NUM1_PTS_fillval
cdf_control, cdfid, variable='B1F1',     set_padvalue=B1F1_fillval
cdf_control, cdfid, variable='B1RMSF1',  set_padvalue=B1RMSF1_fillval
cdf_control, cdfid, variable='B1GSM',    set_padvalue=B1GSM_fillval
cdf_control, cdfid, variable='B1RMSGSM', set_padvalue=B1RMSGSM_fillval
cdf_control, cdfid, variable='B1GSE',    set_padvalue=B1GSE_fillval
cdf_control, cdfid, variable='B1RMSGSE', set_padvalue=B1RMSGSE_fillval
cdf_control, cdfid, variable='DIST1',    set_padvalue=DIST1_fillval
cdf_control, cdfid, variable='P1GSM',    set_padvalue=P1GSM_fillval
cdf_control, cdfid, variable='P1GSE',    set_padvalue=P1GSE_fillval
cdf_control, cdfid, variable='S1GSM',    set_padvalue=S1GSM_fillval
cdf_control, cdfid, variable='S1GSE',    set_padvalue=S1GSE_fillval
cdf_control, cdfid, variable='ZERO_I',   set_padvalue=ZERO_I_fillval
cdf_control, cdfid, variable='SENS_I',   set_padvalue=SENS_I_fillval
cdf_control, cdfid, variable='AMPL_I',   set_padvalue=AMPL_I_fillval
cdf_control, cdfid, variable='ORTH_I',   set_padvalue=ORTH_I_fillval
cdf_control, cdfid, variable='PAYLD_I',  set_padvalue=PAYLD_I_fillval
cdf_control, cdfid, variable='FLAG_I',   set_padvalue=FLAG_I_fillval
cdf_control, cdfid, variable='ZERO_O',   set_padvalue=ZERO_O_fillval
cdf_control, cdfid, variable='SENS_O',   set_padvalue=SENS_O_fillval
cdf_control, cdfid, variable='AMPL_O',   set_padvalue=AMPL_O_fillval
cdf_control, cdfid, variable='ORTH_O',   set_padvalue=ORTH_O_fillval
cdf_control, cdfid, variable='PAYLD_O',  set_padvalue=PAYLD_O_fillval
cdf_control, cdfid, variable='FLAG_O',   set_padvalue=FLAG_O_fillval

;- put variable values
cdf_varput, cdfid, 'Epoch',     epoch
cdf_varput, cdfid, 'Time_PB5',  time_pb5
cdf_varput, cdfid, 'NUM_PTS',   num_pts
cdf_varput, cdfid, 'BF1',       bf1
cdf_varput, cdfid, 'BRMSF1',    brmsf1
cdf_varput, cdfid, 'BGSM',      bgsm
cdf_varput, cdfid, 'BRMSGSM',   brmsgsm
cdf_varput, cdfid, 'BGSE',      bgse
cdf_varput, cdfid, 'BRMSGSE',   brmsgse
cdf_varput, cdfid, 'DIST',      dist
cdf_varput, cdfid, 'PGSM',      pgsm
cdf_varput, cdfid, 'PGSE',      pgse
cdf_varput, cdfid, 'SGSM',      sgsm
cdf_varput, cdfid, 'SGSE',      sgse
cdf_varput, cdfid, 'DB_SC',     db_sc
cdf_varput, cdfid, 'TILTANG',   tiltang
cdf_varput, cdfid, 'RANGE_I',   range_i
cdf_varput, cdfid, 'RANGE_O',   range_o
cdf_varput, cdfid, 'SPC_MODE',  spc_mode
cdf_varput, cdfid, 'MAG_MODE',  mag_mode
cdf_varput, cdfid, 'Epoch3',    epoch3
cdf_varput, cdfid, 'Time3_PB5', time3_pb5
cdf_varput, cdfid, 'NUM3_PTS',  num3_pts
cdf_varput, cdfid, 'B3F1',      b3f1
cdf_varput, cdfid, 'B3RMSF1',   b3rmsf1
cdf_varput, cdfid, 'B3GSM',     b3gsm
cdf_varput, cdfid, 'B3RMSGSM',  b3rmsgsm
cdf_varput, cdfid, 'B3GSE',     b3gse
cdf_varput, cdfid, 'B3RMSGSE',  b3rmsgse
cdf_varput, cdfid, 'Epoch1',    epoch1
cdf_varput, cdfid, 'Time1_PB5', time1_pb5
cdf_varput, cdfid, 'NUM1_PTS',  num1_pts
cdf_varput, cdfid, 'B1F1',      b1f1
cdf_varput, cdfid, 'B1RMSF1',   b1rmsf1
cdf_varput, cdfid, 'B1GSM',     b1gsm
cdf_varput, cdfid, 'B1RMSGSM',  b1rmsgsm
cdf_varput, cdfid, 'B1GSE',     b1gse
cdf_varput, cdfid, 'B1RMSGSE',  b1rmsgse
cdf_varput, cdfid, 'DIST1',     dist1
cdf_varput, cdfid, 'P1GSM',     p1gsm
cdf_varput, cdfid, 'P1GSE',     p1gse
cdf_varput, cdfid, 'S1GSM',     s1gsm
cdf_varput, cdfid, 'S1GSE',     s1gse
cdf_varput, cdfid, 'ZERO_I',    zero_i
cdf_varput, cdfid, 'SENS_I',    sens_i
cdf_varput, cdfid, 'AMPL_I',    ampl_i
cdf_varput, cdfid, 'ORTH_I',    orth_i
cdf_varput, cdfid, 'PAYLD_I',   payld_i
cdf_varput, cdfid, 'FLAG_I',    flag_i
cdf_varput, cdfid, 'ZERO_O',    zero_o
cdf_varput, cdfid, 'SENS_O',    sens_o
cdf_varput, cdfid, 'AMPL_O',    ampl_o
cdf_varput, cdfid, 'ORTH_O',    orth_o
cdf_varput, cdfid, 'PAYLD_O',   payld_o
cdf_varput, cdfid, 'FLAG_O',    flag_o
cdf_varput, cdfid, 'label_time', ['Year                       ', 'Day of Year (Jan 1 = Day 1)',  $
                                  'Elapsed milliseconds of day']
cdf_varput, cdfid, 'format_time', ['I4', 'I3', 'I8']
cdf_varput, cdfid, 'unit_time',   ['year', 'day ', 'msec']
cdf_varput, cdfid, 'label_bgsm',  ['Bx (GSM)', 'By (GSM)', 'Bz (GSM)']
cdf_varput, cdfid, 'label_bgse',  ['Bx (GSE)', 'By (GSE)', 'Bz (GSE)']
cdf_varput, cdfid, 'label_bgsmr', ['Bx_RMS (GSM)', 'By_RMS (GSM)', 'Bz_RMS (GSM)']
cdf_varput, cdfid, 'label_bgser', ['Bx_RMS (GSE)', 'By_RMS (GSE)', 'Bz_RMS (GSE)']
cdf_varput, cdfid, 'label_dbsc',  ['X (Delta SC)', 'Y (Delta SC)', 'Z (Delta SC)']
cdf_varput, cdfid, 'label_pgsm',  ['X (GSM)', 'Y (GSM)', 'Z (GSM)']
cdf_varput, cdfid, 'label_pgse',  ['X (GSE)', 'Y (GSE)', 'Z (GSE)']
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
cdf_attput, cdfid, 'Data_type',    0, 'H0>3 sec, 1 min, and hourly Definitive Data'
cdf_attput, cdfid, 'Descriptor',   0, 'MFI>Magnetic Fields Investigation'
cdf_attput, cdfid, 'Data_version', 0, string(version, format='(I02)')
cdf_attput, cdfid, 'TITLE',  0, 'WIND Magnetic Field Investigation (MFI) Standard Production'
cdf_attput, cdfid, 'TEXT',   0, 'WIND MFI Composite data file.  This file contains multiple time resolution data.'
cdf_attput, cdfid, 'TEXT',   1, '1 Minute data averages                                                          '
cdf_attput, cdfid, 'TEXT',   2, '3 Second data averages                                                          '
cdf_attput, cdfid, 'TEXT',   3, '1 Hour   data averages                                                          '
cdf_attput, cdfid, 'TEXT',   4, 'WIND MFI Instrument turn on 11/12/1994                                          '
cdf_attput, cdfid, 'TEXT',   5, 'Data versions:                                                                  '
cdf_attput, cdfid, 'TEXT',   6, '03 - Extrapolated Bz correction                                                 '
cdf_attput, cdfid, 'TEXT',   7, '04 - Final Bz correction                                                        '
cdf_attput, cdfid, 'TEXT',   8, '05 - Final orbit and Bz correction                                              '
cdf_attput, cdfid, 'TEXT',   9, 'References:                                                                     '
cdf_attput, cdfid, 'TEXT',  10, '1. Lepping, R. P., et al., The WIND Magnetic Field Investigation, p. 207 in     '
cdf_attput, cdfid, 'TEXT',  11, 'The Global Geospace Mission, ed. by C. T. Russell, Kluwer,1995                  '
cdf_attput, cdfid, 'TEXT',  12, '2. Panetta, P. (GSFC), GGS WIND MFI Operator''s Manual, September 15, 1992.     '
cdf_attput, cdfid, 'TEXT',  13, '3. Computer Sciences Corporation, Data Format Control Document (DFCD) Between   '
cdf_attput, cdfid, 'TEXT',  14, 'The International Solar-Terrestrial Physics (ISTP) Program Information          '
cdf_attput, cdfid, 'TEXT',  15, 'Processing Division Ground Data Processing System and The ISTP Mission          '
cdf_attput, cdfid, 'TEXT',  16, 'Investigators, CSC/TR-91/6014, 560-1DFD/0190, July 1992.                        '
cdf_attput, cdfid, 'TEXT',  17, '4. Behannon, K. W., International Solar Terrestrial Physics (ISTP) Program      '
cdf_attput, cdfid, 'TEXT',  18, 'Investigator Data Analysis Requirements For WIND and GEOTAIL Spacecraft         '
cdf_attput, cdfid, 'TEXT',  19, 'Magnetometer Experiment, September 1987.                                        '
cdf_attput, cdfid, 'TEXT',  20, '5. National Space Science Data Center, CDF User''s Guide, Version 2.3.0,        '
cdf_attput, cdfid, 'TEXT',  21, 'October 1, 1992.                                                                '
cdf_attput, cdfid, 'TEXT',  22, '6. Mish, W. H., International Solar-Terrestrial Physics (ISTP) Key Parameter    '
cdf_attput, cdfid, 'TEXT',  23, 'Generation Software (KPGS) Standards & Conventions, September 1992.             '
cdf_attput, cdfid, 'TEXT',  24, '7. Mish, W. H., IMP F and G Phase I Magnetic Field Analysis, April 1972         '
cdf_attput, cdfid, 'MODS',   0, ' 10/01/2011 Initial release                    '
cdf_attput, cdfid, 'ADID_ref',   0, 'NSSD0141'
cdf_attput, cdfid, 'Logical_file_id',0, h0_prefix + mfi_prefix1 + '_00000000_v00'
cdf_attput, cdfid, 'Logical_source', 0, h0_prefix + mfi_prefix1
cdf_attput, cdfid, 'Logical_source_description',   0, $
                   'Wind Magnetic Fields Investigation: 3 sec, 1 min, and hourly Definitive Data.'
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
cdf_attput, cdfid, 'FIELDNAM',  'Epoch',     'Time Line (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'Time_PB5',  'Time Line (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'NUM_PTS',   'Number of points in average (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'BF1',       'Magnetic field magnitude (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'BRMSF1',    'RMS magnitude (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'BGSM',      'Magnetic field vector in GSM cartesian coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'BRMSGSM',   'RMS vector in GSM coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'BGSE',      'Magnetic field vector in GSE cartesian coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'BRMSGSE',   'RMS vector in GSE coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'DIST',      'Radial Distance (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'PGSM',      'Position in GSM coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'PGSE',      'Position in GSE coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'SGSM',      'Unit spin vector in GSM coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'SGSE',      'Unit spin vector in GSE coordinates (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'DB_SC',     'Delta B (Outer - Inner) (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'TILTANG',   'Dipole Tilt Angle (Degrees) (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'RANGE_I',   'Average Range Inner Magnetometer (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'RANGE_O',   'Average Range Outer Magnetometer (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'SPC_MODE',  'S/C operational mode (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'MAG_MODE',  'WIND/MFI operational mode (1 min)'
cdf_attput, cdfid, 'FIELDNAM',  'Epoch3',    'Time Line (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'Time3_PB5', 'Time Line (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'NUM3_PTS',  'Number of points in average (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'B3F1',      'Magnetic field magnitude (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'B3RMSF1',   'RMS magnitude (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'B3GSM',     'Magnetic field vector in GSM cartesian coordinates (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'B3RMSGSM',  'RMS vector in GSM coordinates (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'B3GSE',     'Magnetic field vector in GSE cartesian coordinates (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'B3RMSGSE',  'RMS vector in GSE coordinates (3 sec)'
cdf_attput, cdfid, 'FIELDNAM',  'Epoch1',    'Time Line (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'Time1_PB5', 'Time Line (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'NUM1_PTS',  'Number of points in average (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'B1F1',      'Magnetic field magnitude (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'B1RMSF1',   'RMS magnitude (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'B1GSM',     'Magnetic field vector in GSM cartesian coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'B1RMSGSM',  'RMS vector in GSM coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'B1GSE',     'Magnetic field vector in GSE cartesian coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'B1RMSGSE',  'RMS vector in GSE coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'DIST1',     'Radial Distance (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'P1GSM',     'Position in GSM coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'P1GSE',     'Position in GSE coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'S1GSM',     'Unit spin vector in GSM coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'S1GSE',     'Unit spin vector in GSE coordinates (1 hour)'
cdf_attput, cdfid, 'FIELDNAM',  'ZERO_I',    'Sensor Zeros (Inner)'
cdf_attput, cdfid, 'FIELDNAM',  'SENS_I',    'Sensor Sensitivities (Inner)'
cdf_attput, cdfid, 'FIELDNAM',  'AMPL_I',    'Amplitude Correction (Inner)'
cdf_attput, cdfid, 'FIELDNAM',  'ORTH_I',    'Orthogonalization Matrix (Inner)'
cdf_attput, cdfid, 'FIELDNAM',  'PAYLD_I',   'Payload Rotation Matrix (Inner)'
cdf_attput, cdfid, 'FIELDNAM',  'FLAG_I',    'Sensor Zero Flag (Inner)'
cdf_attput, cdfid, 'FIELDNAM',  'ZERO_O',    'Sensor Zeros (Outer)'
cdf_attput, cdfid, 'FIELDNAM',  'SENS_O',    'Sensor Sensitivities (Outer)'
cdf_attput, cdfid, 'FIELDNAM',  'AMPL_O',    'Amplitude Correction (Outer)'
cdf_attput, cdfid, 'FIELDNAM',  'ORTH_O',    'Orthogonalization Matrix (Outer)'
cdf_attput, cdfid, 'FIELDNAM',  'PAYLD_O',   'Payload Rotation Matrix (Outer)'
cdf_attput, cdfid, 'FIELDNAM',  'FLAG_O',    'Sensor Zero Flag (Outer)'
cdf_attput, cdfid, 'FIELDNAM',  'label_time',  'Label for Time_PB5'
cdf_attput, cdfid, 'FIELDNAM',  'format_time', 'Format for Time_PB5'
cdf_attput, cdfid, 'FIELDNAM',  'unit_time',   'Units for Time_PB5'
cdf_attput, cdfid, 'FIELDNAM',  'label_bgsm',  'Label for B in GMS coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'label_bgse',  'Label for B in GSE coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'label_bgsmr', 'Label for B RMS in GSM coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'label_bgser', 'Label for B RMS in GSE coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'label_dbsc',  'Label for delta B spacecraft'
cdf_attput, cdfid, 'FIELDNAM',  'label_pgsm',  'Label for S/C Position in GSM coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'label_pgse',  'Label for S/C Position in GSE coordinates'
cdf_attput, cdfid, 'FIELDNAM',  'cartesian',   'Dimension Label'
cdf_attput, cdfid, 'VALIDMIN',  'Epoch',        62951817600000d
cdf_attput, cdfid, 'VALIDMIN',  'Time_PB5',     [1994L, 316L, 0L]
cdf_attput, cdfid, 'VALIDMIN',  'NUM_PTS',      0L
cdf_attput, cdfid, 'VALIDMIN',  'BF1',          0.
cdf_attput, cdfid, 'VALIDMIN',  'BRMSF1',       0.
cdf_attput, cdfid, 'VALIDMIN',  'BGSM',         [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'BRMSGSM',      [0., 0., 0.]
cdf_attput, cdfid, 'VALIDMIN',  'BGSE',         [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'BRMSGSE',      [0., 0., 0.]
cdf_attput, cdfid, 'VALIDMIN',  'DIST',         0.
cdf_attput, cdfid, 'VALIDMIN',  'PGSM',         [-300., -300., -300.]
cdf_attput, cdfid, 'VALIDMIN',  'PGSE',         [-300., -300., -300.]
cdf_attput, cdfid, 'VALIDMIN',  'SGSM',         [-1., -1., -1.]
cdf_attput, cdfid, 'VALIDMIN',  'SGSE',         [-1., -1., -1.]
cdf_attput, cdfid, 'VALIDMIN',  'DB_SC',        -25.
cdf_attput, cdfid, 'VALIDMIN',  'TILTANG',      -35.
cdf_attput, cdfid, 'VALIDMIN',  'RANGE_I',      0.
cdf_attput, cdfid, 'VALIDMIN',  'RANGE_O',      0.
cdf_attput, cdfid, 'VALIDMIN',  'SPC_MODE',     0L
cdf_attput, cdfid, 'VALIDMIN',  'MAG_MODE',     0L
cdf_attput, cdfid, 'VALIDMIN',  'Epoch3',       62951817600000d
cdf_attput, cdfid, 'VALIDMIN',  'Time3_PB5',    [1994L, 316L, 0L]
cdf_attput, cdfid, 'VALIDMIN',  'NUM3_PTS',     0L
cdf_attput, cdfid, 'VALIDMIN',  'B3F1',         0.
cdf_attput, cdfid, 'VALIDMIN',  'B3RMSF1',      0.
cdf_attput, cdfid, 'VALIDMIN',  'B3GSM',        [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'B3RMSGSM',     [0., 0., 0.]
cdf_attput, cdfid, 'VALIDMIN',  'B3GSE',        [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'B3RMSGSE',     [0., 0., 0.]
cdf_attput, cdfid, 'VALIDMIN',  'Epoch1',       62951817600000d
cdf_attput, cdfid, 'VALIDMIN',  'Time1_PB5',    [1994L, 316L, 0L]
cdf_attput, cdfid, 'VALIDMIN',  'NUM1_PTS',     0L
cdf_attput, cdfid, 'VALIDMIN',  'B1F1',         0.
cdf_attput, cdfid, 'VALIDMIN',  'B1RMSF1',      0.
cdf_attput, cdfid, 'VALIDMIN',  'B1GSM',        [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'B1RMSGSM',     [0., 0., 0.]
cdf_attput, cdfid, 'VALIDMIN',  'B1GSE',        [-65534.0, -65534.0, -65534.0]
cdf_attput, cdfid, 'VALIDMIN',  'B1RMSGSE',     [0., 0., 0.]
cdf_attput, cdfid, 'VALIDMIN',  'DIST1',        0.
cdf_attput, cdfid, 'VALIDMIN',  'P1GSM',        [-300., -300., -300.]
cdf_attput, cdfid, 'VALIDMIN',  'P1GSE',        [-300., -300., -300.]
cdf_attput, cdfid, 'VALIDMIN',  'S1GSM',        [-1., -1., -1.]
cdf_attput, cdfid, 'VALIDMIN',  'S1GSE',        [-1., -1., -1.]
cdf_attput, cdfid, 'VALIDMAX',  'Epoch',      64092124800000d
cdf_attput, cdfid, 'VALIDMAX',  'Time_PB5',   [2030L, 365L, 0L]
cdf_attput, cdfid, 'VALIDMAX',  'NUM_PTS',    1500L
cdf_attput, cdfid, 'VALIDMAX',  'BF1',        65534.
cdf_attput, cdfid, 'VALIDMAX',  'BRMSF1',     65534.
cdf_attput, cdfid, 'VALIDMAX',  'BGSM',       [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'BRMSGSM',    [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'BGSE',       [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'BRMSGSE',    [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'DIST',       300.
cdf_attput, cdfid, 'VALIDMAX',  'PGSM',       [300., 300., 300.]
cdf_attput, cdfid, 'VALIDMAX',  'PGSE',       [300., 300., 300.]
cdf_attput, cdfid, 'VALIDMAX',  'SGSM',       [1., 1., 1.]
cdf_attput, cdfid, 'VALIDMAX',  'SGSE',       [1., 1., 1.]
cdf_attput, cdfid, 'VALIDMAX',  'DB_SC',      25.
cdf_attput, cdfid, 'VALIDMAX',  'TILTANG',    35.
cdf_attput, cdfid, 'VALIDMAX',  'RANGE_I',    7.
cdf_attput, cdfid, 'VALIDMAX',  'RANGE_O',    7.
cdf_attput, cdfid, 'VALIDMAX',  'SPC_MODE',   10L
cdf_attput, cdfid, 'VALIDMAX',  'MAG_MODE',   15L
cdf_attput, cdfid, 'VALIDMAX',  'Epoch3',     64092124800000d
cdf_attput, cdfid, 'VALIDMAX',  'Time3_PB5',  [2030L, 365L, 0L]
cdf_attput, cdfid, 'VALIDMAX',  'NUM3_PTS',   100L
cdf_attput, cdfid, 'VALIDMAX',  'B3F1',       65534.
cdf_attput, cdfid, 'VALIDMAX',  'B3RMSF1',    65534.
cdf_attput, cdfid, 'VALIDMAX',  'B3GSM',      [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'B3RMSGSM',   [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'B3GSE',      [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'B3RMSGSE',   [65534.0, 65534.0, 65534.0] 
cdf_attput, cdfid, 'VALIDMAX',  'Epoch1',     64092124800000d
cdf_attput, cdfid, 'VALIDMAX',  'Time1_PB5',  [2030L, 365L, 0L]
cdf_attput, cdfid, 'VALIDMAX',  'NUM1_PTS',   80000L
cdf_attput, cdfid, 'VALIDMAX',  'B1F1',       65534.
cdf_attput, cdfid, 'VALIDMAX',  'B1RMSF1',    65534
cdf_attput, cdfid, 'VALIDMAX',  'B1GSM',      [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'B1RMSGSM',   [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'B1GSE',      [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'B1RMSGSE',   [65534.0, 65534.0, 65534.0]
cdf_attput, cdfid, 'VALIDMAX',  'DIST1',      300.
cdf_attput, cdfid, 'VALIDMAX',  'P1GSM',      [300., 300., 300.]
cdf_attput, cdfid, 'VALIDMAX',  'P1GSE',      [300., 300., 300.]
cdf_attput, cdfid, 'VALIDMAX',  'S1GSM',      [1., 1., 1.]
cdf_attput, cdfid, 'VALIDMAX',  'S1GSE',      [1., 1., 1.]
cdf_attput, cdfid, 'SCALEMIN',  'Epoch',        epoch_date_cur
cdf_attput, cdfid, 'SCALEMIN',  'Time_PB5',     [long(year_cur), long(doy_cur), 0L]
cdf_attput, cdfid, 'SCALEMIN',  'NUM_PTS',      mfi_min_max(num_pts, fillval=num_pts_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'BF1',          mfi_min_max(bf1,     fillval=bf1_fillval)   
cdf_attput, cdfid, 'SCALEMIN',  'BRMSF1',       mfi_min_max(brmsf1,  fillval=brmsf1_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'BGSM',        [mfi_min_max(bgsm[0,*], fillval=bgsm_fillval), $
                                                mfi_min_max(bgsm[1,*], fillval=bgsm_fillval), $
                                                mfi_min_max(bgsm[2,*], fillval=bgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'BRMSGSM',     [mfi_min_max(brmsgsm[0,*], fillval=brmsgsm_fillval), $
                                                mfi_min_max(brmsgsm[1,*], fillval=brmsgsm_fillval), $
                                                mfi_min_max(brmsgsm[2,*], fillval=brmsgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'BGSE',        [mfi_min_max(bgse[0,*], fillval=bgse_fillval), $
                                                mfi_min_max(bgse[1,*], fillval=bgse_fillval), $
                                                mfi_min_max(bgse[2,*], fillval=bgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'BRMSGSE',     [mfi_min_max(brmsgse[0,*], fillval=brmsgse_fillval), $
                                                mfi_min_max(brmsgse[1,*], fillval=brmsgse_fillval), $
                                                mfi_min_max(brmsgse[2,*], fillval=brmsgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'DIST',         mfi_min_max(dist, fillval=dist_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'PGSM',        [mfi_min_max(pgsm[0,*], fillval=pgsm_fillval), $
                                                mfi_min_max(pgsm[1,*], fillval=pgsm_fillval), $
                                                mfi_min_max(pgsm[2,*], fillval=pgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'PGSE',        [mfi_min_max(pgse[0,*], fillval=pgse_fillval), $
                                                mfi_min_max(pgse[1,*], fillval=pgse_fillval), $
                                                mfi_min_max(pgse[2,*], fillval=pgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'SGSM',        [mfi_min_max(sgsm[0,*], fillval=sgsm_fillval), $
                                                mfi_min_max(sgsm[1,*], fillval=sgsm_fillval), $
                                                mfi_min_max(sgsm[2,*], fillval=sgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'SGSE',        [mfi_min_max(sgse[0,*], fillval=sgse_fillval), $
                                                mfi_min_max(sgse[1,*], fillval=sgse_fillval), $
                                                mfi_min_max(sgse[2,*], fillval=sgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'DB_SC',       [mfi_min_max(db_sc[0,*], fillval=db_sc_fillval), $
                                                mfi_min_max(db_sc[1,*], fillval=db_sc_fillval), $
                                                mfi_min_max(db_sc[2,*], fillval=db_sc_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'TILTANG',      mfi_min_max(tiltang, fillval=tiltang_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'RANGE_I',      mfi_min_max(range_i, fillval=range_i_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'RANGE_O',      mfi_min_max(range_o, fillval=range_o_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'SPC_MODE',     mfi_min_max(spc_mode, fillval=spc_mode_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'MAG_MODE',     mfi_min_max(mag_mode, fillval=mag_mode_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'Epoch3',       epoch_date_cur
cdf_attput, cdfid, 'SCALEMIN',  'Time3_PB5',    [long(year_cur), long(doy_cur), 0L]
cdf_attput, cdfid, 'SCALEMIN',  'NUM3_PTS',     mfi_min_max(num3_pts, fillval=num3_pts_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'B3F1',         mfi_min_max(b3f1,     fillval=b3f1_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'B3RMSF1',      mfi_min_max(b3rmsf1,  fillval=b3rmsf1_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'B3GSM',       [mfi_min_max(b3gsm[0,*], fillval=b3gsm_fillval), $
                                                mfi_min_max(b3gsm[1,*], fillval=b3gsm_fillval), $
                                                mfi_min_max(b3gsm[2,*], fillval=b3gsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'B3RMSGSM',    [mfi_min_max(b3rmsgsm[0,*], fillval=b3rmsgsm_fillval), $
                                                mfi_min_max(b3rmsgsm[1,*], fillval=b3rmsgsm_fillval), $
                                                mfi_min_max(b3rmsgsm[2,*], fillval=b3rmsgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'B3GSE',       [mfi_min_max(b3gse[0,*], fillval=b3gse_fillval), $
                                                mfi_min_max(b3gse[1,*], fillval=b3gse_fillval), $
                                                mfi_min_max(b3gse[2,*], fillval=b3gse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'B3RMSGSE',    [mfi_min_max(b3rmsgse[0,*], fillval=b3rmsgse_fillval), $
                                                mfi_min_max(b3rmsgse[1,*], fillval=b3rmsgse_fillval), $
                                                mfi_min_max(b3rmsgse[2,*], fillval=b3rmsgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'Epoch1',       epoch_date_cur
cdf_attput, cdfid, 'SCALEMIN',  'Time1_PB5',    [long(year_cur), long(doy_cur), 0L]
cdf_attput, cdfid, 'SCALEMIN',  'NUM1_PTS',     mfi_min_max(num1_pts, fillval=num1_pts_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'B1F1',         mfi_min_max(b1f1,     fillval=b1f1_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'B1RMSF1',      mfi_min_max(b1rmsf1,  fillval=b1rmsf1_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'B1GSM',       [mfi_min_max(b1gsm[0,*], fillval=b1gsm_fillval), $
                                                mfi_min_max(b1gsm[1,*], fillval=b1gsm_fillval), $
                                                mfi_min_max(b1gsm[2,*], fillval=b1gsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'B1RMSGSM',    [mfi_min_max(b1rmsgsm[0,*], fillval=b1rmsgsm_fillval), $
                                                mfi_min_max(b1rmsgsm[1,*], fillval=b1rmsgsm_fillval), $
                                                mfi_min_max(b1rmsgsm[2,*], fillval=b1rmsgsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'B1GSE',       [mfi_min_max(b1gse[0,*], fillval=b1gse_fillval), $
                                                mfi_min_max(b1gse[1,*], fillval=b1gse_fillval), $
                                                mfi_min_max(b1gse[2,*], fillval=b1gse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'B1RMSGSE',    [mfi_min_max(b1rmsgse[0,*], fillval=b1rmsgse_fillval), $
                                                mfi_min_max(b1rmsgse[1,*], fillval=b1rmsgse_fillval), $
                                                mfi_min_max(b1rmsgse[2,*], fillval=b1rmsgse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'DIST1',        mfi_min_max(dist1, fillval=dist1_fillval)
cdf_attput, cdfid, 'SCALEMIN',  'P1GSM',       [mfi_min_max(p1gsm[0,*], fillval=p1gsm_fillval), $
                                                mfi_min_max(p1gsm[1,*], fillval=p1gsm_fillval), $
                                                mfi_min_max(p1gsm[2,*], fillval=p1gsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'P1GSE',       [mfi_min_max(p1gse[0,*], fillval=p1gse_fillval), $
                                                mfi_min_max(p1gse[1,*], fillval=p1gse_fillval), $
                                                mfi_min_max(p1gse[2,*], fillval=p1gse_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'S1GSM',       [mfi_min_max(s1gsm[0,*], fillval=s1gsm_fillval), $
                                                mfi_min_max(s1gsm[1,*], fillval=s1gsm_fillval), $
                                                mfi_min_max(s1gsm[2,*], fillval=s1gsm_fillval)]
cdf_attput, cdfid, 'SCALEMIN',  'S1GSE',       [mfi_min_max(s1gse[0,*], fillval=s1gse_fillval), $
                                                mfi_min_max(s1gse[1,*], fillval=s1gse_fillval), $
                                                mfi_min_max(s1gse[2,*], fillval=s1gse_fillval)]
cdf_attput, cdfid, 'SCALEMAX',  'Epoch',        epoch_date_plus1day
cdf_attput, cdfid, 'SCALEMAX',  'Time_PB5',     [long(year_plus1day), long(doy_plus1day), 0L]
cdf_attput, cdfid, 'SCALEMAX',  'NUM_PTS',      mfi_min_max(num_pts, fillval=num_pts_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'BF1',          mfi_min_max(bf1,     fillval=bf1_fillval, /max)   
cdf_attput, cdfid, 'SCALEMAX',  'BRMSF1',       mfi_min_max(brmsf1,  fillval=brmsf1_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'BGSM',        [mfi_min_max(bgsm[0,*], fillval=bgsm_fillval, /max), $
                                                mfi_min_max(bgsm[1,*], fillval=bgsm_fillval, /max), $
                                                mfi_min_max(bgsm[2,*], fillval=bgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'BRMSGSM',     [mfi_min_max(brmsgsm[0,*], fillval=brmsgsm_fillval, /max), $
                                                mfi_min_max(brmsgsm[1,*], fillval=brmsgsm_fillval, /max), $
                                                mfi_min_max(brmsgsm[2,*], fillval=brmsgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'BGSE',        [mfi_min_max(bgse[0,*], fillval=bgse_fillval, /max), $
                                                mfi_min_max(bgse[1,*], fillval=bgse_fillval, /max), $
                                                mfi_min_max(bgse[2,*], fillval=bgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'BRMSGSE',     [mfi_min_max(brmsgse[0,*], fillval=brmsgse_fillval, /max), $
                                                mfi_min_max(brmsgse[1,*], fillval=brmsgse_fillval, /max), $
                                                mfi_min_max(brmsgse[2,*], fillval=brmsgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'DIST',         mfi_min_max(dist, fillval=dist_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'PGSM',        [mfi_min_max(pgsm[0,*], fillval=pgsm_fillval, /max), $
                                                mfi_min_max(pgsm[1,*], fillval=pgsm_fillval, /max), $
                                                mfi_min_max(pgsm[2,*], fillval=pgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'PGSE',        [mfi_min_max(pgse[0,*], fillval=pgse_fillval, /max), $
                                                mfi_min_max(pgse[1,*], fillval=pgse_fillval, /max), $
                                                mfi_min_max(pgse[2,*], fillval=pgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'SGSM',        [mfi_min_max(sgsm[0,*], fillval=sgsm_fillval, /max), $
                                                mfi_min_max(sgsm[1,*], fillval=sgsm_fillval, /max), $
                                                mfi_min_max(sgsm[2,*], fillval=sgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'SGSE',        [mfi_min_max(sgse[0,*], fillval=sgse_fillval, /max), $
                                                mfi_min_max(sgse[1,*], fillval=sgse_fillval, /max), $
                                                mfi_min_max(sgse[2,*], fillval=sgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'DB_SC',       [mfi_min_max(db_sc[0,*], fillval=db_sc_fillval, /max), $
                                                mfi_min_max(db_sc[1,*], fillval=db_sc_fillval, /max), $
                                                mfi_min_max(db_sc[2,*], fillval=db_sc_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'TILTANG',      mfi_min_max(tiltang, fillval=tiltang_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'RANGE_I',      mfi_min_max(range_i, fillval=range_i_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'RANGE_O',      mfi_min_max(range_o, fillval=range_o_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'SPC_MODE',     mfi_min_max(spc_mode, fillval=spc_mode_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'MAG_MODE',     mfi_min_max(mag_mode, fillval=mag_mode_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'Epoch3',       epoch_date_plus1day
cdf_attput, cdfid, 'SCALEMAX',  'Time3_PB5',    [long(year_plus1day), long(doy_plus1day), 0L]
cdf_attput, cdfid, 'SCALEMAX',  'NUM3_PTS',     mfi_min_max(num3_pts, fillval=num3_pts_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'B3F1',         mfi_min_max(b3f1,     fillval=b3f1_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'B3RMSF1',      mfi_min_max(b3rmsf1,  fillval=b3rmsf1_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'B3GSM',       [mfi_min_max(b3gsm[0,*], fillval=b3gsm_fillval, /max), $
                                                mfi_min_max(b3gsm[1,*], fillval=b3gsm_fillval, /max), $
                                                mfi_min_max(b3gsm[2,*], fillval=b3gsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'B3RMSGSM',    [mfi_min_max(b3rmsgsm[0,*], fillval=b3rmsgsm_fillval, /max), $
                                                mfi_min_max(b3rmsgsm[1,*], fillval=b3rmsgsm_fillval, /max), $
                                                mfi_min_max(b3rmsgsm[2,*], fillval=b3rmsgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'B3GSE',       [mfi_min_max(b3gse[0,*], fillval=b3gse_fillval, /max), $
                                                mfi_min_max(b3gse[1,*], fillval=b3gse_fillval, /max), $
                                                mfi_min_max(b3gse[2,*], fillval=b3gse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'B3RMSGSE',    [mfi_min_max(b3rmsgse[0,*], fillval=b3rmsgse_fillval, /max), $
                                                mfi_min_max(b3rmsgse[1,*], fillval=b3rmsgse_fillval, /max), $
                                                mfi_min_max(b3rmsgse[2,*], fillval=b3rmsgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'Epoch1',       epoch_date_plus1day
cdf_attput, cdfid, 'SCALEMAX',  'Time1_PB5',    [long(year_plus1day), long(doy_plus1day), 0L]
cdf_attput, cdfid, 'SCALEMAX',  'NUM1_PTS',     mfi_min_max(num1_pts, fillval=num1_pts_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'B1F1',         mfi_min_max(b1f1,     fillval=b1f1_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'B1RMSF1',      mfi_min_max(b1rmsf1,  fillval=b1rmsf1_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'B1GSM',       [mfi_min_max(b1gsm[0,*], fillval=b1gsm_fillval, /max), $
                                                mfi_min_max(b1gsm[1,*], fillval=b1gsm_fillval, /max), $
                                                mfi_min_max(b1gsm[2,*], fillval=b1gsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'B1RMSGSM',    [mfi_min_max(b1rmsgsm[0,*], fillval=b1rmsgsm_fillval, /max), $
                                                mfi_min_max(b1rmsgsm[1,*], fillval=b1rmsgsm_fillval, /max), $
                                                mfi_min_max(b1rmsgsm[2,*], fillval=b1rmsgsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'B1GSE',       [mfi_min_max(b1gse[0,*], fillval=b1gse_fillval, /max), $
                                                mfi_min_max(b1gse[1,*], fillval=b1gse_fillval, /max), $
                                                mfi_min_max(b1gse[2,*], fillval=b1gse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'B1RMSGSE',    [mfi_min_max(b1rmsgse[0,*], fillval=b1rmsgse_fillval, /max), $
                                                mfi_min_max(b1rmsgse[1,*], fillval=b1rmsgse_fillval, /max), $
                                                mfi_min_max(b1rmsgse[2,*], fillval=b1rmsgse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'DIST1',        mfi_min_max(dist1, fillval=dist1_fillval, /max)
cdf_attput, cdfid, 'SCALEMAX',  'P1GSM',       [mfi_min_max(p1gsm[0,*], fillval=p1gsm_fillval, /max), $
                                                mfi_min_max(p1gsm[1,*], fillval=p1gsm_fillval, /max), $
                                                mfi_min_max(p1gsm[2,*], fillval=p1gsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'P1GSE',       [mfi_min_max(p1gse[0,*], fillval=p1gse_fillval, /max), $
                                                mfi_min_max(p1gse[1,*], fillval=p1gse_fillval, /max), $
                                                mfi_min_max(p1gse[2,*], fillval=p1gse_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'S1GSM',       [mfi_min_max(s1gsm[0,*], fillval=s1gsm_fillval, /max), $
                                                mfi_min_max(s1gsm[1,*], fillval=s1gsm_fillval, /max), $
                                                mfi_min_max(s1gsm[2,*], fillval=s1gsm_fillval, /max)]
cdf_attput, cdfid, 'SCALEMAX',  'S1GSE',       [mfi_min_max(s1gse[0,*], fillval=s1gse_fillval, /max), $
                                                mfi_min_max(s1gse[1,*], fillval=s1gse_fillval, /max), $
                                                mfi_min_max(s1gse[2,*], fillval=s1gse_fillval, /max)]
cdf_attput, cdfid, 'UNITS',  'Epoch',        'ms'
cdf_attput, cdfid, 'UNITS',  'NUM_PTS',      ' '
cdf_attput, cdfid, 'UNITS',  'BF1',          'nT'
cdf_attput, cdfid, 'UNITS',  'BRMSF1',       'nT'
cdf_attput, cdfid, 'UNITS',  'BGSM',         'nT'
cdf_attput, cdfid, 'UNITS',  'BRMSGSM',      'nT'
cdf_attput, cdfid, 'UNITS',  'BGSE',         'nT'
cdf_attput, cdfid, 'UNITS',  'BRMSGSE',      'nT'
cdf_attput, cdfid, 'UNITS',  'DIST',         'Re'
cdf_attput, cdfid, 'UNITS',  'PGSM',         'Re'
cdf_attput, cdfid, 'UNITS',  'PGSE',         'Re'
cdf_attput, cdfid, 'UNITS',  'SGSM',         ' '
cdf_attput, cdfid, 'UNITS',  'SGSE',         ' '
cdf_attput, cdfid, 'UNITS',  'DB_SC',        'nT'
cdf_attput, cdfid, 'UNITS',  'TILTANG',      'Degrees'
cdf_attput, cdfid, 'UNITS',  'RANGE_I',      ' '
cdf_attput, cdfid, 'UNITS',  'RANGE_O',      ' '
cdf_attput, cdfid, 'UNITS',  'SPC_MODE',     ' '
cdf_attput, cdfid, 'UNITS',  'MAG_MODE',     ' '
cdf_attput, cdfid, 'UNITS',  'Epoch3',       'ms'
cdf_attput, cdfid, 'UNITS',  'NUM3_PTS',     ' '
cdf_attput, cdfid, 'UNITS',  'B3F1',         'nT'
cdf_attput, cdfid, 'UNITS',  'B3RMSF1',      'nT'
cdf_attput, cdfid, 'UNITS',  'B3GSM',        'nT'
cdf_attput, cdfid, 'UNITS',  'B3RMSGSM',     'nT'
cdf_attput, cdfid, 'UNITS',  'B3GSE',        'nT'
cdf_attput, cdfid, 'UNITS',  'B3RMSGSE',     'nT'
cdf_attput, cdfid, 'UNITS',  'Epoch1',       'ms'
cdf_attput, cdfid, 'UNITS',  'NUM1_PTS',     ' '
cdf_attput, cdfid, 'UNITS',  'B1F1',         'nT'
cdf_attput, cdfid, 'UNITS',  'B1RMSF1',      'nT'
cdf_attput, cdfid, 'UNITS',  'B1GSM',        'nT'
cdf_attput, cdfid, 'UNITS',  'B1RMSGSM',     'nT'
cdf_attput, cdfid, 'UNITS',  'B1GSE',        'nT'
cdf_attput, cdfid, 'UNITS',  'B1RMSGSE',     'nT'
cdf_attput, cdfid, 'UNITS',  'DIST1',        'Re'
cdf_attput, cdfid, 'UNITS',  'P1GSM',        'Re'
cdf_attput, cdfid, 'UNITS',  'P1GSE',        'Re'
cdf_attput, cdfid, 'UNITS',  'S1GSM',        ' '
cdf_attput, cdfid, 'UNITS',  'S1GSE',        ' '
cdf_attput, cdfid, 'LABLAXIS',  'Epoch',     'Epoch'
cdf_attput, cdfid, 'LABLAXIS',  'NUM_PTS',   'No. Points'
cdf_attput, cdfid, 'LABLAXIS',  'BF1',       'B'
cdf_attput, cdfid, 'LABLAXIS',  'BRMSF1',    'B_RMS'
cdf_attput, cdfid, 'LABLAXIS',  'DIST',      'Rad. Dist.'
cdf_attput, cdfid, 'LABLAXIS',  'SGSM',      'Unit Vector'
cdf_attput, cdfid, 'LABLAXIS',  'SGSE',      'Unit Vector'
cdf_attput, cdfid, 'LABLAXIS',  'TILTANG',   'Dipole Tilt'
cdf_attput, cdfid, 'LABLAXIS',  'RANGE_I',   'Range Inner'
cdf_attput, cdfid, 'LABLAXIS',  'RANGE_O',   'Range Outer'
cdf_attput, cdfid, 'LABLAXIS',  'SPC_MODE',  'S/C MODE'
cdf_attput, cdfid, 'LABLAXIS',  'MAG_MODE',  'MFI MODE'
cdf_attput, cdfid, 'LABLAXIS',  'Epoch3',    'Epoch'
cdf_attput, cdfid, 'LABLAXIS',  'NUM3_PTS',  'No. Points'
cdf_attput, cdfid, 'LABLAXIS',  'B3F1',      'B'
cdf_attput, cdfid, 'LABLAXIS',  'B3RMSF1',   'B_RMS'
cdf_attput, cdfid, 'LABLAXIS',  'Epoch1',    'Epoch'
cdf_attput, cdfid, 'LABLAXIS',  'NUM1_PTS',  'No. Points'
cdf_attput, cdfid, 'LABLAXIS',  'B1F1',      'B'
cdf_attput, cdfid, 'LABLAXIS',  'B1RMSF1',   'B_RMS'
cdf_attput, cdfid, 'LABLAXIS',  'DIST1',     'Rad. Dist.'
cdf_attput, cdfid, 'LABLAXIS',  'S1GSM',     'Unit Vector'
cdf_attput, cdfid, 'LABLAXIS',  'S1GSE',     'Unit Vector'
cdf_attput, cdfid, 'FORMAT',  'Epoch',     'E14.8'
cdf_attput, cdfid, 'FORMAT',  'NUM_PTS',   'I4'
cdf_attput, cdfid, 'FORMAT',  'BF1',       'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BRMSF1',    'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BGSM',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BRMSGSM',   'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BGSE',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'BRMSGSE',   'E13.6'
cdf_attput, cdfid, 'FORMAT',  'DIST',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'PGSM',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'PGSE',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'SGSM',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'SGSE',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'DB_SC',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'TILTANG',   'E13.6'
cdf_attput, cdfid, 'FORMAT',  'RANGE_I',   'F4.2'
cdf_attput, cdfid, 'FORMAT',  'RANGE_O',   'F4.2'
cdf_attput, cdfid, 'FORMAT',  'SPC_MODE',  'I2'
cdf_attput, cdfid, 'FORMAT',  'MAG_MODE',  'I2'
cdf_attput, cdfid, 'FORMAT',  'Epoch3',    'E14.8'
cdf_attput, cdfid, 'FORMAT',  'NUM3_PTS',  'I4'
cdf_attput, cdfid, 'FORMAT',  'B3F1',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B3RMSF1',   'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B3GSM',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B3RMSGSM',  'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B3GSE',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B3RMSGSE',  'E13.6'
cdf_attput, cdfid, 'FORMAT',  'Epoch1',    'E14.8'
cdf_attput, cdfid, 'FORMAT',  'NUM1_PTS',  'I4'
cdf_attput, cdfid, 'FORMAT',  'B1F1',      'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B1RMSF1',   'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B1GSM',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B1RMSGSM',  'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B1GSE',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'B1RMSGSE',  'E13.6'
cdf_attput, cdfid, 'FORMAT',  'DIST1',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'P1GSM',     'E13.6'
cdf_attput, cdfid, 'FORMAT',  'P1GSE',     'E13.6
cdf_attput, cdfid, 'FORMAT',  'S1GSM',     'E13.6
cdf_attput, cdfid, 'FORMAT',  'S1GSE',     'E13.6
cdf_attput, cdfid, 'MONOTON',  'Epoch',        'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'Time_PB5',     'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'NUM_PTS',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'BF1',          'FALSE'
cdf_attput, cdfid, 'MONOTON',  'BRMSF1',       'FALSE'
cdf_attput, cdfid, 'MONOTON',  'BGSM',         'FALSE' 
cdf_attput, cdfid, 'MONOTON',  'BRMSGSM',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'BGSE',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'BRMSGSE',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'DIST',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'PGSM',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'PGSE',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'SGSM',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'SGSE',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'DB_SC',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'TILTANG',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'RANGE_I',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'RANGE_O',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'SPC_MODE',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'MAG_MODE',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'Epoch3',       'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'Time3_PB5',    'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'NUM3_PTS',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B3F1',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B3RMSF1',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B3GSM',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B3RMSGSM',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B3GSE',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B3RMSGSE',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'Epoch1',       'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'Time1_PB5',    'INCREASE'
cdf_attput, cdfid, 'MONOTON',  'NUM1_PTS',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B1F1',         'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B1RMSF1',      'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B1GSM',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B1RMSGSM',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B1GSE',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'B1RMSGSE',     'FALSE'
cdf_attput, cdfid, 'MONOTON',  'DIST1',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'P1GSM',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'P1GSE',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'S1GSM',        'FALSE'
cdf_attput, cdfid, 'MONOTON',  'S1GSE',        'FALSE'
cdf_attput, cdfid, 'SCALETYP',  'Epoch',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Time_PB5',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'NUM_PTS',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BF1',          'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BRMSF1',       'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BGSM',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BRMSGSM',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BGSE',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'BRMSGSE',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'DIST',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'PGSM',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'PGSE',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'SGSM',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'SGSE',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'DB_SC',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'TILTANG',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'RANGE_I',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'RANGE_O',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'SPC_MODE',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'MAG_MODE',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Epoch3',       'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Time3_PB5',    'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'NUM3_PTS',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B3F1',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B3RMSF1',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B3GSM',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B3RMSGSM',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B3GSE',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B3RMSGSE',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Epoch1',       'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'Time1_PB5',    'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'NUM1_PTS',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B1F1',         'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B1RMSF1',      'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B1GSM',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B1RMSGSM',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B1GSE',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'B1RMSGSE',     'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'DIST1',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'P1GSM',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'P1GSE',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'S1GSM',        'LINEAR'
cdf_attput, cdfid, 'SCALETYP',  'S1GSE',        'LINEAR'
cdf_attput, cdfid, 'CATDESC',  'Epoch',       'Time, Centered, Number of milliseconds since the epoch (1 min)'
cdf_attput, cdfid, 'CATDESC',  'Time_PB5',    'Time of observation in Year, Day, & milliseconds (1 min)'
cdf_attput, cdfid, 'CATDESC',  'NUM_PTS',     'Number of points in average (1 min)'
cdf_attput, cdfid, 'CATDESC',  'BF1',         'Magnetic field magnitude (1 min)'
cdf_attput, cdfid, 'CATDESC',  'BRMSF1',      'RMS magnitude (1 min)'
cdf_attput, cdfid, 'CATDESC',  'BGSM',        'Magnetic field vector in GSM cartesian coordinates (1 min)'
cdf_attput, cdfid, 'CATDESC',  'BRMSGSM',     'RMS vector in GSM coordinates (1 min)'
cdf_attput, cdfid, 'CATDESC',  'BGSE',        'Magnetic field vector in GSE cartesian coordinates (1 min)'
cdf_attput, cdfid, 'CATDESC',  'BRMSGSE',     'RMS vector in GSE coordinates (1 min)'
cdf_attput, cdfid, 'CATDESC',  'DIST',        'Distance from the center of the earth (Define Re = 6378km) (1 min)'
cdf_attput, cdfid, 'CATDESC',  'PGSM',        'Position vector in GSM coordinates (Define Re = 6378km) (1 min)'
cdf_attput, cdfid, 'CATDESC',  'PGSE',        'Position vector in GSE coordinates (Define Re = 6378km) (1 min)'
cdf_attput, cdfid, 'CATDESC',  'SGSM',        'Unit spin vector in GSM coordinates (1 min)'
cdf_attput, cdfid, 'CATDESC',  'SGSE',        'Unit spin vector in GSE coordinates (1 min)'
cdf_attput, cdfid, 'CATDESC',  'DB_SC',       'Delta B (Outer - Inner) (1 min)'
cdf_attput, cdfid, 'CATDESC',  'TILTANG',     'Dipole Tilt Angle (Degrees)'
cdf_attput, cdfid, 'CATDESC',  'RANGE_I',     'Average Range Inner Magnetometer (1 min)'
cdf_attput, cdfid, 'CATDESC',  'RANGE_O',     'Average Range Outer Magnetometer (1 min)'
cdf_attput, cdfid, 'CATDESC',  'SPC_MODE',    'S/C operational mode (1 min)'
cdf_attput, cdfid, 'CATDESC',  'MAG_MODE',    'WIND/MFI operational mode (1 min)'
cdf_attput, cdfid, 'CATDESC',  'Epoch3',      'Time, Centered, Number of milliseconds since the epoch (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'Time3_PB5',   'Time of observation in Year, Day, & milliseconds (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'NUM3_PTS',    'Number of points in average (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'B3F1',        'Magnetic field magnitude (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'B3RMSF1',     'RMS magnitude (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'B3GSM',       'Magnetic field vector in GSM cartesian coordinates (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'B3RMSGSM',    'RMS vector in GSM coordinates (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'B3GSE',       'Magnetic field vector in GSE cartesian coordinates (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'B3RMSGSE',    'RMS vector in GSE coordinates (3 sec)'
cdf_attput, cdfid, 'CATDESC',  'Epoch1',      'Time, Centered, Number of milliseconds since the epoch (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'Time1_PB5',   'Time of observation in Year, Day, & milliseconds (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'NUM1_PTS',    'Number of points in average (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'B1F1',        'Magnetic field magnitude (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'B1RMSF1',     'RMS magnitude (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'B1GSM',       'Magnetic field vector in GSM cartesian coordinates (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'B1RMSGSM',    'RMS vector in GSM coordinates (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'B1GSE',       'Magnetic field vector in GSE cartesian coordinates (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'B1RMSGSE',    'RMS vector in GSE coordinates (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'DIST1',       'Distance from the center of the earth (Define Re = 6378km) (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'P1GSM',       'Position vector in GSM coordinates (Define Re = 6378km) (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'P1GSE',       'Position vector in GSE coordinates (Define Re = 6378km) (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'S1GSM',       'Unit spin vector in GSM coordinates (1 hour)'
cdf_attput, cdfid, 'CATDESC',  'S1GSE',       'Unit spin vector in GSE coordinates (1 hour)'
cdf_attput, cdfid, 'FILLVAL',  'Epoch',       Epoch_fillval
cdf_attput, cdfid, 'FILLVAL',  'Time_PB5',    Time_PB5_fillval
cdf_attput, cdfid, 'FILLVAL',  'NUM_PTS',     NUM_PTS_fillval
cdf_attput, cdfid, 'FILLVAL',  'BF1',         BF1_fillval
cdf_attput, cdfid, 'FILLVAL',  'BRMSF1',      BRMSF1_fillval
cdf_attput, cdfid, 'FILLVAL',  'BGSM',        BGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'BRMSGSM',     BRMSGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'BGSE',        BGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'BRMSGSE',     BRMSGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'DIST',        DIST_fillval
cdf_attput, cdfid, 'FILLVAL',  'PGSM',        PGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'PGSE',        PGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'SGSM',        SGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'SGSE',        SGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'DB_SC',       DB_SC_fillval
cdf_attput, cdfid, 'FILLVAL',  'TILTANG',     TILTANG_fillval
cdf_attput, cdfid, 'FILLVAL',  'RANGE_I',     RANGE_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'RANGE_O',     RANGE_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'SPC_MODE',    SPC_MODE_fillval
cdf_attput, cdfid, 'FILLVAL',  'MAG_MODE',    MAG_MODE_fillval
cdf_attput, cdfid, 'FILLVAL',  'Epoch3',      Epoch3_fillval
cdf_attput, cdfid, 'FILLVAL',  'Time3_PB5',   Time3_PB5_fillval
cdf_attput, cdfid, 'FILLVAL',  'NUM3_PTS',    NUM3_PTS_fillval
cdf_attput, cdfid, 'FILLVAL',  'B3F1',        B3F1_fillval
cdf_attput, cdfid, 'FILLVAL',  'B3RMSF1',     B3RMSF1_fillval
cdf_attput, cdfid, 'FILLVAL',  'B3GSM',       B3GSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'B3RMSGSM',    B3RMSGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'B3GSE',       B3GSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'B3RMSGSE',    B3RMSGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'Epoch1',      Epoch1_fillval
cdf_attput, cdfid, 'FILLVAL',  'Time1_PB5',   Time1_PB5_fillval
cdf_attput, cdfid, 'FILLVAL',  'NUM1_PTS',    NUM1_PTS_fillval
cdf_attput, cdfid, 'FILLVAL',  'B1F1',        B1F1_fillval
cdf_attput, cdfid, 'FILLVAL',  'B1RMSF1',     B1RMSF1_fillval
cdf_attput, cdfid, 'FILLVAL',  'B1GSM',       B1GSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'B1RMSGSM',    B1RMSGSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'B1GSE',       B1GSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'B1RMSGSE',    B1RMSGSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'DIST1',       DIST1_fillval
cdf_attput, cdfid, 'FILLVAL',  'P1GSM',       P1GSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'P1GSE',       P1GSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'S1GSM',       S1GSM_fillval
cdf_attput, cdfid, 'FILLVAL',  'S1GSE',       S1GSE_fillval
cdf_attput, cdfid, 'FILLVAL',  'ZERO_I',      ZERO_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'SENS_I',      SENS_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'AMPL_I',      AMPL_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'ORTH_I',      ORTH_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'PAYLD_I',     PAYLD_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'FLAG_I',      FLAG_I_fillval
cdf_attput, cdfid, 'FILLVAL',  'ZERO_O',      ZERO_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'SENS_O',      SENS_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'AMPL_O',      AMPL_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'ORTH_O',      ORTH_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'PAYLD_O',     PAYLD_O_fillval
cdf_attput, cdfid, 'FILLVAL',  'FLAG_O',      FLAG_O_fillval
cdf_attput, cdfid, 'LABL_PTR_1',  'Time_PB5',      'label_time'
cdf_attput, cdfid, 'LABL_PTR_1',  'BGSM',          'label_bgsm'
cdf_attput, cdfid, 'LABL_PTR_1',  'BRMSGSM',       'label_bgsmr'
cdf_attput, cdfid, 'LABL_PTR_1',  'BGSE',          'label_bgse'
cdf_attput, cdfid, 'LABL_PTR_1',  'BRMSGSE',       'label_bgser'
cdf_attput, cdfid, 'LABL_PTR_1',  'PGSM',          'label_pgsm'
cdf_attput, cdfid, 'LABL_PTR_1',  'PGSE',          'label_pgse'
cdf_attput, cdfid, 'LABL_PTR_1',  'DB_SC',         'label_dbsc'
cdf_attput, cdfid, 'LABL_PTR_1',  'Time3_PB5',     'label_time'
cdf_attput, cdfid, 'LABL_PTR_1',  'B3GSM',         'label_bgsm'
cdf_attput, cdfid, 'LABL_PTR_1',  'B3RMSGSM',      'label_bgsmr'
cdf_attput, cdfid, 'LABL_PTR_1',  'B3GSE',         'label_bgse'
cdf_attput, cdfid, 'LABL_PTR_1',  'B3RMSGSE',      'label_bgser'
cdf_attput, cdfid, 'LABL_PTR_1',  'Time1_PB5',     'label_time'
cdf_attput, cdfid, 'LABL_PTR_1',  'B1GSM',         'label_bgsm'
cdf_attput, cdfid, 'LABL_PTR_1',  'B1RMSGSM',      'label_bgsmr'
cdf_attput, cdfid, 'LABL_PTR_1',  'B1GSE',         'label_bgse'
cdf_attput, cdfid, 'LABL_PTR_1',  'B1RMSGSE',      'label_bgser'
cdf_attput, cdfid, 'LABL_PTR_1',  'P1GSM',         'label_pgsm'
cdf_attput, cdfid, 'LABL_PTR_1',  'P1GSE',         'label_pgse'
cdf_attput, cdfid, 'UNIT_PTR',  'Time_PB5',      'unit_time'
cdf_attput, cdfid, 'UNIT_PTR',  'Time3_PB5',     'unit_time'
cdf_attput, cdfid, 'UNIT_PTR',  'Time1_PB5',     'unit_time'
cdf_attput, cdfid, 'FORM_PTR',  'Time_PB5',    'format_time'
cdf_attput, cdfid, 'FORM_PTR',  'Time3_PB5',   'format_time'
cdf_attput, cdfid, 'FORM_PTR',  'Time1_PB5',   'format_time'
cdf_attput, cdfid, 'DEPEND_0',  'Time_PB5',      'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'NUM_PTS',       'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BF1',           'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BRMSF1',        'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BGSM',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BRMSGSM',       'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BGSE',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'BRMSGSE',       'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'DIST',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'PGSM',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'PGSE',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'SGSM',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'SGSE',          'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'DB_SC',         'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'TILTANG',       'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'RANGE_I',       'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'RANGE_O',       'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'SPC_MODE',      'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'MAG_MODE',      'Epoch'
cdf_attput, cdfid, 'DEPEND_0',  'Time3_PB5',     'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'NUM3_PTS',      'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'B3F1',          'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'B3RMSF1',       'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'B3GSM',         'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'B3RMSGSM',      'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'B3GSE',         'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'B3RMSGSE',      'Epoch3'
cdf_attput, cdfid, 'DEPEND_0',  'Time1_PB5',     'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'NUM1_PTS',      'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'B1F1',          'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'B1RMSF1',       'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'B1GSM',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'B1RMSGSM',      'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'B1GSE',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'B1RMSGSE',      'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'DIST1',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'P1GSM',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'P1GSE',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'S1GSM',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_0',  'S1GSE',         'Epoch1'
cdf_attput, cdfid, 'DEPEND_1',  'Time_PB5',    'unit_time'
cdf_attput, cdfid, 'DEPEND_1',  'PGSM',        'cartesian'
cdf_attput, cdfid, 'DEPEND_1',  'PGSE',        'cartesian'
cdf_attput, cdfid, 'DEPEND_1',  'Time3_PB5',   'unit_time'
cdf_attput, cdfid, 'DEPEND_1',  'Time1_PB5',   'unit_time'
cdf_attput, cdfid, 'DEPEND_1',  'P1GSM',       'cartesian'
cdf_attput, cdfid, 'DEPEND_1',  'P1GSE',       'cartesian'
cdf_attput, cdfid, 'VAR_TYPE',  'Epoch',         'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Time_PB5',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'NUM_PTS',       'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'BF1',           'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BRMSF1',        'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BGSM',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BRMSGSM',       'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BGSE',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'BRMSGSE',       'data'
cdf_attput, cdfid, 'VAR_TYPE',  'DIST',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'PGSM',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'PGSE',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'SGSM',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'SGSE',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'DB_SC',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'TILTANG',       'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'RANGE_I',       'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'RANGE_O',       'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'SPC_MODE',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'MAG_MODE',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Epoch3',        'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Time3_PB5',     'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'NUM3_PTS',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'B3F1',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B3RMSF1',       'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B3GSM',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B3RMSGSM',      'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B3GSE',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B3RMSGSE',      'data'
cdf_attput, cdfid, 'VAR_TYPE',  'Epoch1',        'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'Time1_PB5',     'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'NUM1_PTS',      'support_data'
cdf_attput, cdfid, 'VAR_TYPE',  'B1F1',          'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B1RMSF1',       'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B1GSM',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B1RMSGSM',      'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B1GSE',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'B1RMSGSE',      'data'
cdf_attput, cdfid, 'VAR_TYPE',  'DIST1',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'P1GSM',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'P1GSE',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'S1GSM',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'S1GSE',         'data'
cdf_attput, cdfid, 'VAR_TYPE',  'ZERO_I',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'SENS_I',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'AMPL_I',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'ORTH_I',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'PAYLD_I',       'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'FLAG_I',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'ZERO_O',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'SENS_O',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'AMPL_O',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'ORTH_O',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'PAYLD_O',       'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'FLAG_O',        'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_time',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'format_time',   'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'unit_time',     'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_bgsm',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_bgse',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_bgsmr',   'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_bgser',   'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_dbsc',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_pgsm',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'label_pgse',    'metadata'
cdf_attput, cdfid, 'VAR_TYPE',  'cartesian',     'metadata'
cdf_attput, cdfid, 'DICT_KEY',  'cartesian',     'ISTP>vector>cartesian'
cdf_attput, cdfid, 'TIME_RES',  'Epoch',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'Time_PB5',    '1 min'
cdf_attput, cdfid, 'TIME_RES',  'NUM_PTS',     '1 min'
cdf_attput, cdfid, 'TIME_RES',  'BF1',         '1 min'
cdf_attput, cdfid, 'TIME_RES',  'BRMSF1',      '1 min'
cdf_attput, cdfid, 'TIME_RES',  'BGSM',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'BRMSGSM',     '1 min'
cdf_attput, cdfid, 'TIME_RES',  'BGSE',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'BRMSGSE',     '1 min'
cdf_attput, cdfid, 'TIME_RES',  'DIST',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'PGSM',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'PGSE',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'SGSM',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'SGSE',        '1 min'
cdf_attput, cdfid, 'TIME_RES',  'DB_SC',       '1 min'
cdf_attput, cdfid, 'TIME_RES',  'TILTANG',     '1 min'
cdf_attput, cdfid, 'TIME_RES',  'RANGE_I',     '1 min'
cdf_attput, cdfid, 'TIME_RES',  'RANGE_O',     '1 min'
cdf_attput, cdfid, 'TIME_RES',  'SPC_MODE',    '1 min'
cdf_attput, cdfid, 'TIME_RES',  'MAG_MODE',    '1 min'
cdf_attput, cdfid, 'TIME_RES',  'Epoch3',      '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'Time3_PB5',   '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'NUM3_PTS',    '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'B3F1',        '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'B3RMSF1',     '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'B3GSM',       '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'B3RMSGSM',    '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'B3GSE',       '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'B3RMSGSE',    '3 sec'
cdf_attput, cdfid, 'TIME_RES',  'Epoch1',      '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'Time1_PB5',   '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'NUM1_PTS',    '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'B1F1',        '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'B1RMSF1',     '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'B1GSM',       '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'B1RMSGSM',    '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'B1GSE',       '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'B1RMSGSE',    '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'DIST1',       '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'P1GSM',       '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'P1GSE',       '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'S1GSM',       '1 hour'
cdf_attput, cdfid, 'TIME_RES',  'S1GSE',       '1 hour'
cdf_attput, cdfid, 'VAR_NOTES',  'BF1',      'Average of the magnitudes (F1)'
cdf_attput, cdfid, 'VAR_NOTES',  'BRMSF1',   'RMS of the magnitudes (F1 RMS)'
cdf_attput, cdfid, 'VAR_NOTES',  'B3F1',     'Average of the magnitudes (F1)'
cdf_attput, cdfid, 'VAR_NOTES',  'B3RMSF1',  'RMS of the magnitudes (F1 RMS)'
cdf_attput, cdfid, 'VAR_NOTES',  'B1F1',     'Average of the magnitudes (F1)'
cdf_attput, cdfid, 'VAR_NOTES',  'B1RMSF1',  'RMS of the magnitudes (F1 RMS)'

;- close file
cdf_close, cdfid
;--- end: create and write into cdf file

END