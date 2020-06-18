function wind_tm_bit_rate, ch, major, minor, bit_rate
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_bit_rate', ch, major, minor, bit_rate)
return, ok
end
