PRO MFI_SLOPINTFIT, DATAX=DATAX, DATAY=DATAY, COEF=COEF, QCOEF=QCOEF

;- compute coefficients [a,b, rmsdev] of linear fit datay=(a+b*datax)
;  rmsdev is root mean square deviation of data from model

n_data = n_elements(datax)
matrix = dblarr(3,2)

matrix[0,0]= n_data
matrix[1,0]= total(datax)
matrix[2,0]= total(datay)
matrix[0,1]= matrix[1,0]
matrix[1,1]= total(datax^2d)
matrix[2,1]= total(datax*datay)

det_denom  = determ(matrix[[0,1],*], /check)
det_numer1 = determ(matrix[[2,1],*], /check)
det_numer2 = determ(matrix[[0,2],*], /check)

;- check determinants, return qcoef=1 if any =0
if ((det_denom ne 0) and (det_numer1 ne 0) and (det_numer2 ne 0)) then begin
   coef  = [det_numer1/det_denom , det_numer2/det_denom]
   qcoef = 0
   ;- root mean square deviation of data from model, last element in coef
   rmsdev = sqrt(mean((coef[0] + coef[1]*datax - datay)^2d))
   coef   = [coef, rmsdev]
endif else begin
   coef  = [0,0,0]
   qcoef = 1
endelse

END


PRO MFI_SLOPINTFIT_REDUCED, DATAX=DATAX_ORIG, DATAY=DATAY_ORIG, COEF=COEF, $
                            QCOEF=QCOEF, ALPHA=ALPHA, BETA=BETA
;- iteratively runs mfi_slopintfit and removes outliers using
;  restrictions alpha and beta, i.e. on each iteration removes all
;  points that are farther from model than rmsdev*(alpha+beta*it).

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- copy finite values of datax_orig and datay_orig, and set it=0
sub_finite = where(finite(datax_orig) and finite(datay_orig), n_finite)
if (n_finite le 1) then begin
   coef  = [0,0,0]
   qcoef = 1
   return
endif
datax = datax_orig[sub_finite]
datay = datay_orig[sub_finite]
it    = 0

;- repeat until no points are removed
repeat begin
   mfi_slopintfit, datax=datax, datay=datay, coef=coef, qcoef=qcoef
   if (qcoef ne 0) then return
   delta_abs = abs(coef[0] + coef[1]*datax - datay)
   sub_bad   = where(delta_abs gt coef[2]*(alpha+beta*it), n_bad, complement=sub_good, ncomplement=n_good)
   if (n_good lt MIN_N_IN_SLOPINTFIT) then begin
      coef  = [0,0,0]
      qcoef = 1
      return
   endif
   datax = datax[sub_good]
   datay = datay[sub_good]
   it += 1
endrep until (n_bad eq 0)

END