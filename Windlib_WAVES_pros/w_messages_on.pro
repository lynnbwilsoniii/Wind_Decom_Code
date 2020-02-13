function w_messages_on, ch
ok = 0L
ok = call_external('wind_tm_lib','w_messages_on', ch)
return, ok
end
