pro mean_variance,a,mean,stdev=stdev,err=err

;given array a with at least 2 elements, computes mean and stabdard deviation

n=n_elements(a)
if n lt 2 then begin
  err=-1 & return
endif else err=0

mean=total(a)/n

if keyword_set(stdev) ne 0 then begin
  variance=$
    (1./(n-1)) * (total((a-mean)*(a-mean)) - total(a-mean)*total(a-mean)/n)
  stdev=sqrt(variance)
endif

end
