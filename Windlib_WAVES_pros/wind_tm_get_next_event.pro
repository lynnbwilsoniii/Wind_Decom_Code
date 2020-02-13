function wind_tm_get_next_event, ch, major, minor, event
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_get_next_event', ch, major, minor, event)
return, ok
end
