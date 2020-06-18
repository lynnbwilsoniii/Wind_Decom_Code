function w_status, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_status', ch)
return, ok
end

