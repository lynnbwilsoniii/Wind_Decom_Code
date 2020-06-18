function wind_tm_set_nowait, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_set_nowait', ch)
return, ok
end
