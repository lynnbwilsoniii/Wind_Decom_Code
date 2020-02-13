function wind_tm_set_nowait, ch
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_set_nowait', ch)
return, ok
end
