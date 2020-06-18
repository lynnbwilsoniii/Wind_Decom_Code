function wind_tm_get_hk, ch, major, hkindex, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_get_hk', $
   ch, major, hkindex, buf)
return, ok
end
