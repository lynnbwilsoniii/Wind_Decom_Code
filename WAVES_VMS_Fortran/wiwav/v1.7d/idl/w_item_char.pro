function w_item_char, ch, item, buf, buf_sz, ret_sz
ok = 0L
i = 0L
j = 0L
k = 0L
n = 0L
 
v = size(buf)
if v(0) eq 0 then begin		; scalar string
   ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
       'w_item_char_idl', ch, item, buf, 1L, ret_sz)
;   ok = call_external('wind_tm_lib','w_item_char', $
;        ch, item, buf, 1L, ret_sz)
   return, ok
endif
 
; for an idl array of strings emulate a fortran array of strings
n = n_elements(buf)			; # of array elements
if buf_sz lt n then n = buf_sz
i = strlen(buf(0))		; assume length of first element is ok for all
if i eq 0 then i = 16		; supply a default for uninitialized data
j = i*n				; total number of bytes
fmt = '(a'+strtrim(string(j),2)+')'
c = string(format=fmt,' ')	; create an empty scaler, blank filled
 
ok = call_external('/home/wind/lib/libIDL_WAVES.so','w_item_char_idl_ary', $
        ch, item, c, n, ret_sz, i)
 
; copy scaler into array in fixed size chunks
for k=0,ret_sz-1 do begin
   buf(k) = strmid(c, k*i, i)
endfor
 
return, ok
end
