function wind_tm_set_messages_on, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_set_messages_on', ch)
return, ok
end
