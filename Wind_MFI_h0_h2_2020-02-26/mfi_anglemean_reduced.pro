PRO MFI_ANGLEMEAN_REDUCED, ANGLE_ARR=ANGLE_ORIG, ANGLE_MEAN=ANGLE_MEAN, $
                           ANGLE_STDDEV=ANGLE_STDDEV, ALPHA=ALPHA, BETA=BETA

;- iteratively finds mean of angle (as mean vector, to eliminate problems with
;  2Pi crossing) and removes outliers using restrictions alpha and beta.
;  however, it uses all points in calculating stddev

;- copy finite values of angle_orig and set it=0
sub_finite = where(finite(angle_orig), n_finite)
if (n_finite eq 0) then begin
   angle_mean   = !value.d_nan
   angle_stddev = !value.d_nan
   return
endif
if (n_finite eq 1) then begin
   angle_mean   = angle_orig[sub_finite[0]]
   angle_stddev = 0d
   return
endif
angle = angle_orig[sub_finite]
it    = 0

;- repeat until no points are removed
repeat begin
   vectorx  = cos(angle)
   vectory  = sin(angle)
   meanx    = mean(vectorx)
   meany    = mean(vectory)
   dot_prod = (vectorx*meanx + vectory*meany)
   dot_prod = (dot_prod <  (1d))
   dot_prod = (dot_prod > (-1d))
   dangle   = acos(dot_prod)
   angle_stddev = sqrt(total(dangle^2d)/(n_elements(dangle)-1d))
   sub_bad  = where((dangle gt angle_stddev*(alpha+beta*it)), n_bad, complement=sub_good, ncomplement=n_good)
      if (n_good eq 0) then stop
   angle    = angle[sub_good]
   it += 1
endrep until (n_bad eq 0)

;- compute angle_mean
angle_mean   = acos(meanx/sqrt(meanx^2d +meany^2d))
angle_mean   = angle_mean + (meany lt 0)*(-2d*angle_mean)

;- compute rms deviation (angle) of original vectors from mean vector and assign to angle_stddev
vectorx  = cos(angle_orig[sub_finite])
vectory  = sin(angle_orig[sub_finite])
dot_prod = (vectorx*meanx + vectory*meany)
dot_prod = (dot_prod <  (1d))
dot_prod = (dot_prod > (-1d))
dangle   = acos(dot_prod)
angle_stddev = sqrt(mean(dangle^2d))

END