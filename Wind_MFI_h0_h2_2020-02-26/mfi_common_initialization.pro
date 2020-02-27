PRO MFI_COMMON_INITIALIZATION
common common_mfi_calib

;- constants
RE_KM               =     6378d
MSDAY               = 86400000d
CYCLE               =   256000d
TIMEMF              =    92000d
N_IT                =  2d
N_SPINFIT_MAFR_PR   =  5d
N_SPINFIT_MAFR_SC   =  5d
MIN_N_IN_SPINFIT    = 50d
MIN_N_IN_SLOPINTFIT = 50d


;- file path and prefix
lz_path            = '/mfi/data/lz/'
hk_path            = '/mfi/data/lz/'
spha_path          = '/mfi/data/spha/'
pre_or_path        = '/mfi/data/or_pre/'
pre_at_path        = '/mfi/data/at_pre/'
def_or_path        = '/mfi/data/or_def/'
def_at_path        = '/mfi/data/at_def/'
bz_offset_path     = '/mfi/data/calib_bz_offset/'
zeroz_path         = '/mfi/data/calib_zeroz/'
spinfit_path       = '/mfi/data/calib_spinfit/'
acalib_path        = '/mfi/data/calib_angles/'
mcalib_path        = '/mfi/data/calib_multi/'
mcalib_smooth_path = '/mfi/data/calib_multi_smooth/'
calib_alf_path     = '/mfi/data/calib_alf/'
h0_original_path   = '/mfi/data/h0_original/'
h0_path            = '/mfi/data/h0/'
highres_path       = '/mfi/data/h2/'
highres_spectra_path='/mfi/data/h2_spectra/'
spectra_path       = '/mfi/data/spectra/'
spectra_1h_path    = '/mfi/data/spectra_1h/'
swe_k0_path        = '/mfi/data/swe_k0/'
outer_mag_list_path= '/mfi/data/event_lists/'
spha_list_path     = '/mfi/data/event_lists/'
data_gap_list_path = '/mfi/data/event_lists/'
lz_prefix          = 'wi_lz_mfi_'
hk_prefix          = 'wi_lz_scr_'
spha_prefix        = 'wi_k0_spha_'
pre_or_prefix      = 'wi_or_pre_'
pre_at_prefix      = 'wi_at_pre_'
def_or_prefix      = 'wi_or_def_'
def_at_prefix      = 'wi_at_def_'
calib_orig_prefix_in = 'wi_ci1_mfi_'
calib_orig_prefix_ou = 'wi_co1_mfi_'
bz_offset_prefix_in= 'wi_bzi_offset_mfi_'
bz_offset_prefix_ou= 'wi_bzo_offset_mfi_'
zeroz_prefix_in    = 'wi_zzi'
zeroz_prefix_ou    = 'wi_zzo'
spinfit_prefix_in  = 'wi_sfi'
spinfit_prefix_ou  = 'wi_sfo'
acalib_prefix_in   = 'wi_aci'
acalib_prefix_ou   = 'wi_aco'
mcalib_prefix_in   = 'wi_mci'
mcalib_prefix_ou   = 'wi_mco'
mcalib_smooth_prefix_in = 'wi_smci'
mcalib_smooth_prefix_ou = 'wi_smco'
h0_prefix          = 'wi_h0'
highres_prefix     = 'wi_h2'
prefix_cnts        = '_cnt'
prefix_payld       = '_pay'
mfi_prefix         = '_mfi_'
mfi_prefix1        = '_mfi'
spectra_prefix     = 'wi_spi'
spectra_1h_prefix  = 'wi_sp1hi'
swe_k0_prefix      = 'wi_k0_swe_'
outer_mag_list_prefix= 'outer_mag_list'
spha_list_prefix     = 'spha_list'
data_gap_list_prefix = 'data_gap_list'


;- msec shift from major frame epoch to first vector; TA,TF[oper_mode,tel_rate,mag], mag=0 -primary
TA = double([ [[80.5, 34.5, 34.5,80.5], $
               [34.5, 11.5, 11.5,34.5]],$
              [[80.5,908.5,908.5,80.5], $
               [34.5,448.5,448.5,34.5]] ])
TF = double([ [[-368, -368, -368,-368], $
               [-184, -184, -184,-184]],$
              [[-368,-3680,-3680,-368], $
               [-184,-1840,-1840,-184]] ])
               

;- calibration structure for each magnetometer and each range, cal[0] - inner mag
temp = {vect_struc,   x:0d, y:0d, z:0d}
temp = {vect_q_struc, x:0d, y:0d, z:0d, q:0b}
temp = {cor_coef_struc, x:[0d, 0d], y:[0d, 0d], z:[0d, 0d]}
preflight_calib_template = {preflight_calib_struc, $
                            sens:replicate({vect_struc}, 8), sens_bfit:replicate({vect_struc}, 8), $
                            sens_coef:replicate({cor_coef_struc}, 8), $
                            zero:replicate({vect_struc}, 8), zero_coef:replicate({cor_coef_struc}, 8), $
                            ampl:replicate({vect_struc}, 8), orth:dblarr(3,3), pay:dblarr(3,3), $
                            theta:dblarr(3), phi:dblarr(3),  theta_bfit:dblarr(3), phi_bfit:dblarr(3), $
                            noise_cos:replicate({vect_struc}, 40), noise_sin:replicate({vect_struc}, 40)}
cal = replicate({preflight_calib_struc}, 2)


;- inbloard magnetometer calibration -------------------------------------------------
;- preflight sensitivity
cal[0].sens[0].x = 0.00207d  & cal[0].sens[0].y = 0.00202d  & cal[0].sens[0].z = 0.00206d
cal[0].sens[1].x = 0.007818d & cal[0].sens[1].y = 0.007849d & cal[0].sens[1].z = 0.007905d
cal[0].sens[2].x = 0.03225d  & cal[0].sens[2].y = 0.03186d  & cal[0].sens[2].z = 0.03231d
cal[0].sens[3].x = 0.1265d   & cal[0].sens[3].y = 0.12551d  & cal[0].sens[3].z = 0.12691d
cal[0].sens[4].x = 0.50116d  & cal[0].sens[4].y = 0.49449d  & cal[0].sens[4].z = 0.50356d
cal[0].sens[5].x = 1.96465d  & cal[0].sens[5].y = 1.94902d  & cal[0].sens[5].z = 1.97663d
cal[0].sens[6].x = 8.0301d   & cal[0].sens[6].y = 7.92275d  & cal[0].sens[6].z = 8.06849d
cal[0].sens[7].x = 31.4693d  & cal[0].sens[7].y = 31.221d   & cal[0].sens[7].z = 31.6655d

;- inflight sensitivity
cal[0].sens_bfit[0].x = 0.002059619d & cal[0].sens_bfit[0].y = 0.0020301813d & cal[0].sens_bfit[0].z = 0.00206d
cal[0].sens_bfit[1].x = 0.007866956d & cal[0].sens_bfit[1].y = 0.0078001560d & cal[0].sens_bfit[1].z = 0.007905d
cal[0].sens_bfit[2].x = 0.032278008d & cal[0].sens_bfit[2].y = 0.031832355d  & cal[0].sens_bfit[2].z = 0.03231d
cal[0].sens_bfit[3].x = 0.12653841d  & cal[0].sens_bfit[3].y = 0.12547190d   & cal[0].sens_bfit[3].z = 0.12691d
cal[0].sens_bfit[4].x = 0.50116d     & cal[0].sens_bfit[4].y = 0.49449d      & cal[0].sens_bfit[4].z = 0.50356d
cal[0].sens_bfit[5].x = 1.96465d     & cal[0].sens_bfit[5].y = 1.94902d      & cal[0].sens_bfit[5].z = 1.97663d
cal[0].sens_bfit[6].x = 8.0301d      & cal[0].sens_bfit[6].y = 7.92275d      & cal[0].sens_bfit[6].z = 8.06849d
cal[0].sens_bfit[7].x = 31.4693d     & cal[0].sens_bfit[7].y = 31.221d       & cal[0].sens_bfit[7].z = 31.6655d

;- preflight zeros
cal[0].zero[0].x = 1780.2d   & cal[0].zero[0].y = 1719.5d   & cal[0].zero[0].z = 1695.0d
cal[0].zero[1].x = 1980.1d   & cal[0].zero[1].y = 1964.3d   & cal[0].zero[1].z = 1968.0d
cal[0].zero[2].x = 2039.1d   & cal[0].zero[2].y = 2037.6d   & cal[0].zero[2].z = 2025.0d
cal[0].zero[3].x = 2045.4d   & cal[0].zero[3].y = 2045.2d   & cal[0].zero[3].z = 2039.0d
cal[0].zero[4].x = 2043.9d   & cal[0].zero[4].y = 2048.1d   & cal[0].zero[4].z = 2044.0d
cal[0].zero[5].x = 2045.0d   & cal[0].zero[5].y = 2047.0d   & cal[0].zero[5].z = 2047.0d
cal[0].zero[6].x = 2049.0d   & cal[0].zero[6].y = 2052.0d   & cal[0].zero[6].z = 2049.0d
cal[0].zero[7].x = 2048.0d   & cal[0].zero[7].y = 2048.0d   & cal[0].zero[7].z = 2048.0d

;- inflight best fit values of sensitivity correction coefficients (for transformation from range 1 to other ranges)
;  example: sensx_ratio_range1 = cal[0].sens[1].x/sensx_range1
;  sensx_range2 = cal[0].sens[2].x/(cal[0].sens_coef[1].x[0] + sensx_ratio_range1*cal[0].sens_coef[1].x[1])
cal[0].sens_coef[0].x = [-0.51202d, 1.52674d]
cal[0].sens_coef[0].y = [-0.50839d, 1.49384d]
cal[0].sens_coef[0].z = [ 1d, 0d]
cal[0].sens_coef[1].x = [ 0d, 1d]
cal[0].sens_coef[1].y = [ 0d, 1d]             
cal[0].sens_coef[1].z = [ 0d, 1d]
cal[0].sens_coef[2].x = [ 0.57198d, 0.42987d]
cal[0].sens_coef[2].y = [ 0.57311d, 0.42504d] 
cal[0].sens_coef[2].z = [ 1d, 0d]
cal[0].sens_coef[3].x = [ 0.96002d, 0.03988d]
cal[0].sens_coef[3].y = [ 0.96066d, 0.03945d] 
cal[0].sens_coef[3].z = [ 1d, 0d]
cal[0].sens_coef[4].x = [ 0.99955d, 0d]
cal[0].sens_coef[4].y = [ 1.00045d, 0d]       
cal[0].sens_coef[4].z = [ 1d, 0d]
cal[0].sens_coef[5].x = [ 0.99962d, 0d]
cal[0].sens_coef[5].y = [ 1.00039d, 0d]       
cal[0].sens_coef[5].z = [ 1d, 0d]
cal[0].sens_coef[6].x = [ 1d, 0d]
cal[0].sens_coef[6].y = [ 1d, 0d]             
cal[0].sens_coef[6].z = [ 1d, 0d]
cal[0].sens_coef[7].x = [ 1d, 0d]
cal[0].sens_coef[7].y = [ 1d, 0d]             
cal[0].sens_coef[7].z = [ 1d, 0d]

;- inflight best fit values of zero correction coefficients (for transformation from range 1 to other ranges)
;  example: db_nT = (zerox_range1-cal[0].zero[1].x)*cal[0].sens[1].x, 
;  zerox_range2 = (cal[0].zero_coef[2].x[0] + db_nT*cal[0].zero_coef[2].x[1])/cal[0].sens[2].x + cal[0].zero[2].x
cal[0].zero_coef[0].x = [ 0.01506d, 1.01629d]
cal[0].zero_coef[0].y = [ 0.00344d, 1.03118d]
cal[0].zero_coef[0].z = [ 0.10241d, 1.00000d]
cal[0].zero_coef[1].x = [ 0d, 1d]
cal[0].zero_coef[1].y = [ 0d, 1d]
cal[0].zero_coef[1].z = [ 0d, 1d]
cal[0].zero_coef[2].x = [-0.18635d, 0.93114d]
cal[0].zero_coef[2].y = [ 0.05089d, 1.00296d]
cal[0].zero_coef[2].z = [ 0.35141d, 1.05702d]
cal[0].zero_coef[3].x = [-0.31888d, 0.92282d]
cal[0].zero_coef[3].y = [-0.15376d, 1.03327d]
cal[0].zero_coef[3].z = [ 0.70000d, 1.00000d]
cal[0].zero_coef[4].x = [ 0d, 1d]
cal[0].zero_coef[4].y = [ 0d, 1d]
cal[0].zero_coef[4].z = [ 0d, 1d]
cal[0].zero_coef[5].x = [ 0d, 1d]
cal[0].zero_coef[5].y = [ 0d, 1d]
cal[0].zero_coef[5].z = [ 0d, 1d]
cal[0].zero_coef[6].x = [ 0d, 1d]
cal[0].zero_coef[6].y = [ 0d, 1d]
cal[0].zero_coef[6].z = [ 0d, 1d]
cal[0].zero_coef[7].x = [ 0d, 1d]
cal[0].zero_coef[7].y = [ 0d, 1d]
cal[0].zero_coef[7].z = [ 0d, 1d]

;- preflight amplification
cal[0].ampl[*].x = 1d
cal[0].ampl[*].y = 1d
cal[0].ampl[*].z = 1d

;- preflight transformation matrix from magnetometer to orthogonal coordinates
cal[0].orth[0,0] = 1.0000081429d & cal[0].orth[1,0] =  2.0921713d-3  & cal[0].orth[2,0] = -8.2212036d-3
cal[0].orth[0,1] = 8.8914909d-3  & cal[0].orth[1,1] =  1.000063775d  & cal[0].orth[2,1] = -1.721411d-3
cal[0].orth[0,2] = 5.619587d-3   & cal[0].orth[1,2] = -2.5407984d-3  & cal[0].orth[2,2] =  0.9999771818d

;- preflight transformation matrix from orthogonal to spinning payload coordinates
cal[0].pay[0,0]  = 0.999700153d  & cal[0].pay[1,0]  = 0.0d           & cal[0].pay[2,0]  =  2.4486798d-2
cal[0].pay[0,1]  = 0.0d          & cal[0].pay[1,1]  = 0.999952521d   & cal[0].pay[2,1]  =  9.74448d-3
cal[0].pay[0,2]  = 2.4486798d-2  & cal[0].pay[1,2]  = 9.74448d-3     & cal[0].pay[2,2]  = -0.99965266d

;- theta and phi for x,y,z magnetometers ralative to x,y,-z spinning payload coordinates derived
;  from preflight orth and pay matrices. -z is since z payload is antiparallel with z magnetometer
;  ! mag xyz is left-handed system (z to north, rotates clockwise as seen from north)
cal[0].theta[0] = -0.93109d*!dtor  &  cal[0].phi[0] = -0.11372d*!dtor
cal[0].theta[1] = -0.45124d*!dtor  &  cal[0].phi[1] =  0.50745d*!dtor
cal[0].theta[2] =  1.28928d*!dtor  &  cal[0].phi[2] = 33.12207d*!dtor

;- inflight best fit values of theta and phi
cal[0].theta_bfit[0] = -1.24771d*!dtor  &  cal[0].phi_bfit[0] = -0.11372d*!dtor
cal[0].theta_bfit[1] = -1.00842d*!dtor  &  cal[0].phi_bfit[1] =  0.60373d*!dtor
cal[0].theta_bfit[2] =  1.37148d*!dtor  &  cal[0].phi_bfit[2] = 35.36851d*!dtor
;--- inbloard magnetometer calibration -------------------------------------------------


;--- outboard magnetometer preflight calibration ---------------------------------------
;- preflight sensitivities
cal[1].sens[0].x = 0.002181d  & cal[1].sens[0].y = 0.00195d   & cal[1].sens[0].z = 0.00204d
cal[1].sens[1].x = 0.0080295d & cal[1].sens[1].y = 0.00793d   & cal[1].sens[1].z = 0.007955d
cal[1].sens[2].x = 0.032382d  & cal[1].sens[2].y = 0.03228d   & cal[1].sens[2].z = 0.0322484d
cal[1].sens[3].x = 0.128089d  & cal[1].sens[3].y = 0.126676d  & cal[1].sens[3].z = 0.1272023d
cal[1].sens[4].x = 0.5036019d & cal[1].sens[4].y = 0.4999251d & cal[1].sens[4].z = 0.5010588d
cal[1].sens[5].x = 1.984707d  & cal[1].sens[5].y = 1.966734d  & cal[1].sens[5].z = 1.9758060d
cal[1].sens[6].x = 8.07895d   & cal[1].sens[6].y = 7.98206d   & cal[1].sens[6].z = 8.032180d
cal[1].sens[7].x = 31.8262d   & cal[1].sens[7].y = 31.397d    & cal[1].sens[7].z = 31.664d

;- inflight sensitivities
cal[1].sens_bfit[0].x = 0.0020644985d & cal[1].sens_bfit[0].y = 0.0020600402d & cal[1].sens_bfit[0].z = 0.00204d
cal[1].sens_bfit[1].x = 0.0080015087d & cal[1].sens_bfit[1].y = 0.0079577411d & cal[1].sens_bfit[1].z = 0.007955d
cal[1].sens_bfit[2].x = 0.032458501d  & cal[1].sens_bfit[2].y = 0.032203920d  & cal[1].sens_bfit[2].z = 0.0322484d
cal[1].sens_bfit[3].x = 0.128089d     & cal[1].sens_bfit[3].y = 0.126676d     & cal[1].sens_bfit[3].z = 0.1272023d
cal[1].sens_bfit[4].x = 0.5036019d    & cal[1].sens_bfit[4].y = 0.4999251d    & cal[1].sens_bfit[4].z = 0.5010588d
cal[1].sens_bfit[5].x = 1.984707d     & cal[1].sens_bfit[5].y = 1.966734d     & cal[1].sens_bfit[5].z = 1.9758060d
cal[1].sens_bfit[6].x = 8.07895d      & cal[1].sens_bfit[6].y = 7.98206d      & cal[1].sens_bfit[6].z = 8.032180d
cal[1].sens_bfit[7].x = 31.8262d      & cal[1].sens_bfit[7].y = 31.397d       & cal[1].sens_bfit[7].z = 31.664d

;- preflight zeros
cal[1].zero[0].x = 2080.0d    & cal[1].zero[0].y = 2347.0d    & cal[1].zero[0].z = 780.0d
cal[1].zero[1].x = 2052.0d    & cal[1].zero[1].y = 2076.0d    & cal[1].zero[1].z = 1731.0d
cal[1].zero[2].x = 2058.0d    & cal[1].zero[2].y = 2075.0d    & cal[1].zero[2].z = 1974.0d
cal[1].zero[3].x = 2053.0d    & cal[1].zero[3].y = 2056.0d    & cal[1].zero[3].z = 2047.0d
cal[1].zero[4].x = 2050.0d    & cal[1].zero[4].y = 2047.0d    & cal[1].zero[4].z = 2046.0d
cal[1].zero[5].x = 2049.0d    & cal[1].zero[5].y = 2048.0d    & cal[1].zero[5].z = 2047.0d
cal[1].zero[6].x = 2051.0d    & cal[1].zero[6].y = 2048.0d    & cal[1].zero[6].z = 2050.0d
cal[1].zero[7].x = 2048.0d    & cal[1].zero[7].y = 2048.0d    & cal[1].zero[7].z = 2048.0d

;- inflight best fit values of sensitivity correction coefficients (for transformation from range 1 to other ranges)
;  example: sensx_ratio_range1 = cal[1].sens[1].x/sensx_range1
;  sensx_range2 = cal[1].sens[2].x/(cal[1].sens_coef[1].x[0] + sensx_ratio_range1*cal[1].sens_coef[1].x[1])
cal[1].sens_coef[0].x = [1d, 0d]
cal[1].sens_coef[0].y = [1d, 0d]
cal[1].sens_coef[0].z = [1d, 0d]
cal[1].sens_coef[1].x = [0d, 1d]
cal[1].sens_coef[1].y = [0d, 1d]
cal[1].sens_coef[1].z = [0d, 1d]
cal[1].sens_coef[2].x = [1d, 0d]
cal[1].sens_coef[2].y = [1d, 0d]
cal[1].sens_coef[2].z = [1d, 0d]
cal[1].sens_coef[3].x = [1d, 0d]
cal[1].sens_coef[3].y = [1d, 0d]
cal[1].sens_coef[3].z = [1d, 0d]
cal[1].sens_coef[4].x = [1d, 0d]
cal[1].sens_coef[4].y = [1d, 0d]
cal[1].sens_coef[4].z = [1d, 0d]
cal[1].sens_coef[5].x = [1d, 0d]
cal[1].sens_coef[5].y = [1d, 0d]
cal[1].sens_coef[5].z = [1d, 0d]
cal[1].sens_coef[6].x = [1d, 0d]
cal[1].sens_coef[6].y = [1d, 0d]
cal[1].sens_coef[6].z = [1d, 0d]
cal[1].sens_coef[7].x = [1d, 0d]
cal[1].sens_coef[7].y = [1d, 0d]
cal[1].sens_coef[7].z = [1d, 0d]

;- inflight best fit values of zero correction coefficients (for transformation from range 1 to other ranges)
;  example: db_nT = (zerox_range1-cal[1].zero[1].x)*cal[1].sens[1].x, 
;  zerox_range2 = (cal[1].zero_coef[2].x[0] + db_nT*cal[1].zero_coef[2].x[1])/cal[1].sens[2].x + cal[1].zero[2].x
cal[1].zero_coef[0].x = [0d, 1d]
cal[1].zero_coef[0].y = [0d, 1d]
cal[1].zero_coef[0].z = [0d, 1d]
cal[1].zero_coef[1].x = [0d, 1d]
cal[1].zero_coef[1].y = [0d, 1d]
cal[1].zero_coef[1].z = [0d, 1d]
cal[1].zero_coef[2].x = [0d, 1d]
cal[1].zero_coef[2].y = [0d, 1d]
cal[1].zero_coef[2].z = [0d, 1d]
cal[1].zero_coef[3].x = [0d, 1d]
cal[1].zero_coef[3].y = [0d, 1d]
cal[1].zero_coef[3].z = [0d, 1d]
cal[1].zero_coef[4].x = [0d, 1d]
cal[1].zero_coef[4].y = [0d, 1d]
cal[1].zero_coef[4].z = [0d, 1d]
cal[1].zero_coef[5].x = [0d, 1d]
cal[1].zero_coef[5].y = [0d, 1d]
cal[1].zero_coef[5].z = [0d, 1d]
cal[1].zero_coef[6].x = [0d, 1d]
cal[1].zero_coef[6].y = [0d, 1d]
cal[1].zero_coef[6].z = [0d, 1d]
cal[1].zero_coef[7].x = [0d, 1d]
cal[1].zero_coef[7].y = [0d, 1d]
cal[1].zero_coef[7].z = [0d, 1d]

;- amplification
cal[1].ampl[*].x = 1d
cal[1].ampl[*].y = 1d
cal[1].ampl[*].z = 1d

;- preflight transformation matrix from magnetometer to orthogonal coordinates
cal[1].orth[0,0] = 0.9999764031d & cal[1].orth[1,0] = -5.0417287d-3  & cal[1].orth[2,0] = -3.3009636d-3
cal[1].orth[0,1] = 7.7098731d-3  & cal[1].orth[1,1] =  0.9999914558d & cal[1].orth[2,1] =  1.3036705d-3
cal[1].orth[0,2] = 8.678818d-4   & cal[1].orth[1,2] = -2.050239d-4   & cal[1].orth[2,2] =  0.9999972665d

;- preflight transformation matrix from orthogonal to spinning payload coordinates
cal[1].pay[0,0]  = 1.0d          & cal[1].pay[1,0]  = 0.0d           & cal[1].pay[2,0]  =  0.0d
cal[1].pay[0,1]  = 0.0d          & cal[1].pay[1,1]  = 0.999897582d   & cal[1].pay[2,1]  =  1.4311699d-2
cal[1].pay[0,2]  = 0.0d          & cal[1].pay[1,2]  = 1.4311699d-2   & cal[1].pay[2,2]  = -0.999897582d

;- theta and phi for x,y,z magnetometers ralative to x,y,-z spinning payload coordinates derived
;  from preflight orth and pay matrices. -z is since z payload is antiparallel with z magnetometer
;  ! mag xyz is left-handed system (z to north, rotates clockwise as seen from north)
cal[1].theta[0] =  0.18460d*!dtor  &  cal[1].phi[0] =  0.29157d*!dtor
cal[1].theta[1] = -0.89615d*!dtor  &  cal[1].phi[1] =  0.44172d*!dtor
cal[1].theta[2] =  0.83301d*!dtor  &  cal[1].phi[2] = 93.42856d*!dtor

;- inflight best fit values of theta and phi (for outer mag assumed to be the same as preflight falues)
cal[1].theta_bfit[0] =  0.18460d*!dtor  &  cal[1].phi_bfit[0] =  0.29157d*!dtor
cal[1].theta_bfit[1] = -0.89615d*!dtor  &  cal[1].phi_bfit[1] =  0.44172d*!dtor
cal[1].theta_bfit[2] =  0.83301d*!dtor  &  cal[1].phi_bfit[2] = 93.42856d*!dtor
;--- outboard magnetometer preflight calibration ---------------------------------------


;- status word structure; [inboard, outboard] for two-element arrays
temp = {sw12_struc, tel_rate:0b, swap:0b, auto_man:[0b,0b], range:[0b,0b], calib:[0b,0b], el_flip:[0b,0b], mode:0b}

;- structures of inflight calibration constants
temp = {calib_orig_struc, doy:0L, first_ms:0d, last_ms:0d, flag:0b, thetaz:0d, $
                          zerox:0d, zerox_err:0d, thetax:0d, zeroy:0d, zeroy_err:0d, thetay:0d, zeroz:0d}
temp = {bz_offset_struc, year:0,  doy:0, bz_offset:0d, ver:''}
temp = {zeroz_struc,     date:0L, doy:0, flag:1b, zeroz:0d}
temp = {spinfit_struc,   first_epoch:0d, last_epoch:0d, range:0b, coefx:dblarr(4), coefy:dblarr(4), coefz:dblarr(4)}
temp = {acalib_struc,    date:0L, doy:0, flag:1b, thetax:0d, phix:0d, thetay:0d, phiy:0d, thetaz:0d, phiz:0d}
temp = {mcalib_struc,    first_epoch:0d, last_epoch:0d, range:0b, from_range:8b, qzeroz:0b, qangles:0b, $
                         coefx:dblarr(4), coefy:dblarr(4), coefz:dblarr(4), $
                         sensx:0d, zerox:0d, thetax:0d, phix:0d, sensy:0d, zeroy:0d, thetay:0d, phiy:0d, $
                         sensz:0d, zeroz:0d, thetaz:0d, phiz:0d}
END
