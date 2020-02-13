function wind_tm_open_channel, ch, context
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_open_channel', ch, context)
return, ok
end
