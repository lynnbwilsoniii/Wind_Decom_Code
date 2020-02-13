; zero out data within 10 azimuthal degrees of sun
; f is [14, 12] element 2D array, phi is 14 element 1D array
; phi in degrees

FUNCTION sunmask, f, phi

   f_out = f
   phi_2d = phi #  (dblarr(12)+ 1.d)
   ind = where(abs(phi_2d - 180.d) LT 10.d, nind)
   if nind gt 0 THEN f_out[ind] = !values.f_nan
   RETURN, f_out

END

