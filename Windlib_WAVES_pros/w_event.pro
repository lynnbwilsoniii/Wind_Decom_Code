function w_event, ch, event
ok = 0L
ok = call_external('wind_tm_lib','w_event', ch, event)
return, ok
end
