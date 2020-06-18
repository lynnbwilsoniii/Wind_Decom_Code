function wind_tm_open_channel, ch, context
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_open_channel', ch, context)
return, ok
end
