function wind_tm_decrement_packet, major, minor
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_decrement_packet', major, minor)
return, ok
end
