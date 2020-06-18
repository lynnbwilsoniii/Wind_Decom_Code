function w_ur8_to_string, ur8, str
ok = 0L
if strlen(str) eq 0 then begin
   str = 'dd-mmm-yyyy, hh:mm:ss.xxx....'
endif
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),  $
     'w_ur8_to_string', ur8, str)
str = strtrim(str,2)
return, ok
end
