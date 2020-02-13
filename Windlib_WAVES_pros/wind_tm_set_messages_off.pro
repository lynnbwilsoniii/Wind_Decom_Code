function wind_tm_set_messages_off, ch
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_set_messages_off', ch)
return, ok
end
