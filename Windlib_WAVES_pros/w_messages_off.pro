function w_messages_off, ch
ok = 0L
ok = call_external('wind_tm_lib','w_messages_off', ch)
return, ok
end
