function w_ur8_to_ydoy, ur8, year, doy, msec
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),  $
     'w_ur8_to_ydoy', ur8, year, doy, msec)
return, ok
end
