;From vinas@unamuno.gsfc.nasa.gov Fri Jan  9 13:44 EST 1998
;Date: Fri, 9 Jan 1998 13:39:56 -0500
;From: vinas@unamuno.gsfc.nasa.gov (Adolfo F. Vinas)

function moment_errors,fn,fp,n_sectors,dnout,teout
;
emass=9.1095e-28 & boltzk=1.3807e-16 
;
; Routine to determine error estimates on moment density & temperature
;
a=dblarr(3,3)*0.
te=dblarr(n_sectors)
eigval=dblarr(3)*0.
;
delta_ne=sqrt(total((fn-dnout)^2)/(n_elements(fn)-1))
;
for k=0,n_sectors-1 do begin
   trace_a=fp(k,0,0)+fp(k,1,1)+fp(k,2,2)
   a=reform(fp(k,*,*)/trace_a)
   eigval=HQR(ELMHES(a,/double),/double)
;
   eigval=temporary(eigval*trace_a)/fn(k)
   te(k)=total(eigval)/n_elements(eigval)
endfor
;
te=temporary(te/boltzk)
delta_te=float(sqrt(total((te-teout)^2)/(n_elements(te)-1)))
;
return, [delta_ne, delta_te]
end
   

