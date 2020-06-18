function wind_tm_get_packet, ch, major, minor, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_get_packet', $
   ch, major, minor, buf)
return, ok
end
