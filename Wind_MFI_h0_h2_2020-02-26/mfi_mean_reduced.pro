PRO MFI_MEAN_REDUCED, DATA_ARR=DATA_ORIG, DATA_MEAN=DATA_MEAN, DATA_STDDEV=DATA_STDDEV, $
                      ALPHA=ALPHA, BETA=BETA

;- iteratively finds mean of data and removes outliers using
;  restrictions alpha and beta, i.e. on each iteration removes all
;  points that are farther from mean than rmsdev*(alpha+beta*it).
;  however, it uses all points in calculating stddev

;- copy finite values of data_orig and set it=0
sub_finite = where(finite(data_orig), n_finite)
if (n_finite eq 0) then begin
   data_mean   = !value.d_nan
   data_stddev = !value.d_nan   
   return
endif
if (n_finite eq 1) then begin
   data_mean   = data_orig[sub_finite[0]]
   data_stddev = 0d   
   return
endif
data = data_orig[sub_finite]
it   = 0

;- repeat until no points are removed
repeat begin
   data_mean   = mean(data)
   data_stddev = stddev(data)
   delta_abs   = abs(data - data_mean)
   sub_bad     = where(delta_abs gt data_stddev*(alpha+beta*it), n_bad, complement=sub_good)
   data = data[sub_good]
   it += 1
endrep until (n_bad eq 0)

;- compute rms deviation of all original points from data_mean and assign it to data_stddev
data_stddev = sqrt(mean((data_orig[sub_finite] - data_mean)^2d))

END