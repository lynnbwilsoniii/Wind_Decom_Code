; some raw data (ex. anti-strahl 1999-05-11/02:45:46) has spurious 255 counts
; f is [14, 12] element 2D array

FUNCTION mask_255, f

   ncount_max = 6 ; set arbitrarily, some examples have 12 (one theta column)
                  ; of 255 counts, so require ncount_max<12 
   
   f_out = f
   ind = where(f EQ 255, nind)
   ind2 = where(f gt 126, nind2)
   if nind gt ncount_max OR (nind EQ nind2 AND nind GT 0) THEN f_out[ind] = !values.f_nan
   RETURN, f_out

END

