function wind_tm_increment_packet, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_increment_packet', major, minor)
return, ok
end
