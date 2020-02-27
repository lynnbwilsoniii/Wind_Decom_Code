PRO MFI_SPINFIT, DATA=DATA, SIN_OMEGA=SIN_OMEGA, COS_OMEGA=COS_OMEGA, COEF=COEF, QCOEF=QCOEF

;- compute coefficients [a,b,c, rmsdev] of linear fit data=(a+b*cos+c*sin)
;  rmsdev is root mean square deviation of data from model. qcoef=0 means ok

n_data   = n_elements(data)
matrix   = dblarr(4,3)

matrix[0,0] = total(sin_omega)
matrix[1,0] = total(cos_omega*sin_omega)
matrix[2,0] = total(sin_omega*sin_omega)
matrix[3,0] = total(data*sin_omega)

matrix[0,1] = total(cos_omega)
matrix[1,1] = total(cos_omega*cos_omega)
matrix[2,1] = matrix[1,0]
matrix[3,1] = total(data*cos_omega)

matrix[0,2] = n_data
matrix[1,2] = matrix[0,1]
matrix[2,2] = matrix[0,0]
matrix[3,2] = total(data)

det_denom  = determ(matrix[[0,1,2],*], /check)
det_numer1 = determ(matrix[[3,1,2],*], /check)
det_numer2 = determ(matrix[[0,3,2],*], /check)
det_numer3 = determ(matrix[[0,1,3],*], /check)

;- check determinants, return qcoef=1 if any =0
if ((det_denom  ne 0) and (det_numer1 ne 0) and (det_numer2 ne 0) and (det_numer3 ne 0)) then begin
   coef  = [det_numer1/det_denom , det_numer2/det_denom , det_numer3/det_denom]
   qcoef = 0
   ;- root mean square deviation of data from model, last element in coef
   rmsdev = sqrt(mean((coef[0] + coef[1]*cos_omega + coef[2]*sin_omega - data)^2d))
   coef   = [coef, rmsdev]
endif else begin
   coef  = [0,0,0,0]
   qcoef = 1
endelse

END


PRO MFI_SPINFIT_REDUCED, DATA=DATA_ORIG, SIN_OMEGA=SIN_OMEGA_ORIG, COS_OMEGA=COS_OMEGA_ORIG, $
                         COEF=COEF, QCOEF=QCOEF, ALPHA=ALPHA, BETA=BETA
;- iteratively runs mfi_spinfit and removes outliers using restrictions alpha and beta,
;  i.e. on each iteration removes all points that are farther from model than rmsdev*(alpha+beta*it).

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- copy finite values of data_orig and omega_orig, and set it=0
sub_finite  = where(finite(data_orig) and finite(sin_omega_orig) and finite(cos_omega_orig), n_finite)
if (n_finite eq 0) then begin
   coef  = [0,0,0,0]
   qcoef = 1
   return
endif
data        = data_orig[sub_finite]
sin_omega   = sin_omega_orig[sub_finite]
cos_omega   = cos_omega_orig[sub_finite]
it          = 0

;- repeat until no points are removed
repeat begin
   mfi_spinfit, data=data, sin_omega=sin_omega, cos_omega=cos_omega, coef=coef, qcoef=qcoef
   if (qcoef ne 0) then return
   delta_abs = abs(coef[0] + coef[1]*cos_omega + coef[2]*sin_omega - data)
   sub_bad   = where(delta_abs gt coef[3]*(alpha+beta*it), n_bad, complement=sub_good, ncomplement=n_good)
   if (n_good lt MIN_N_IN_SPINFIT) then begin
      coef  = [0,0,0,0]
      qcoef = 1
      return
   endif
   data      = data[sub_good]
   sin_omega = sin_omega[sub_good]
   cos_omega = cos_omega[sub_good]
   it       += 1
endrep until (n_bad eq 0)

END