function wind_tm_close_channel, ch
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_close_channel', ch)
return, ok
end
