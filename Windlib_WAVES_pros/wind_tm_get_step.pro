function wind_tm_get_step, ch, major, minor, buf
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_get_step', $
   ch, major, minor, buf)
return, ok
end