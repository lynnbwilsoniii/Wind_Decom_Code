function wind_tm_bit_rate, ch, major, minor, bit_rate
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_bit_rate', ch, major, minor, bit_rate)
return, ok
end
