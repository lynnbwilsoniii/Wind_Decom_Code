function w_ur8_to_ymd, ur8, year, month, day, hour, minute, second, msec
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),  $
     'w_ur8_to_ymd', $
     ur8, year, month, day, hour, minute, second, msec)
return, ok
end
