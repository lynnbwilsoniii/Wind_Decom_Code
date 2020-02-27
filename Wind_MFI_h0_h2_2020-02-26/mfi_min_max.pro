FUNCTION MFI_MIN_MAX, ARRAY, MAX=MAX, FILLVAL=FILLVAL

if n_elements(fillval) eq 0 then begin
   if keyword_set(max) then return, max(array) else return, min(array)
endif else begin
   sub = where(array ne fillval, n_sub)
   if n_sub gt 0 then begin
      if keyword_set(max) then return, max(array[sub]) else return, min(array[sub])
   endif else begin
      return, fillval
   endelse
endelse

END