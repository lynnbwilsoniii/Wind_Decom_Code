function wind_tm_set_wait, ch
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_set_wait', ch)
return, ok
end
