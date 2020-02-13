function wind_tm_get_hk, ch, major, hkindex, buf
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_get_hk', $
   ch, major, hkindex, buf)
return, ok
end
