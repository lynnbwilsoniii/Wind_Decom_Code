function wind_tm_get_previous_event, ch, event
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'wind_tm_get_previous_event', ch, event)
return, ok
end
