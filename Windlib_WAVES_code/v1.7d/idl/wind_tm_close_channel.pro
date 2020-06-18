function wind_tm_close_channel, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_close_channel', ch)
return, ok
end
