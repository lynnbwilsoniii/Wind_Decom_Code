function w_version, ver
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(ver) eq 0 then ver = big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_version', ver)
ver = strtrim(ver,2)
return, ok
end
