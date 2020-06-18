function w_event, ch, event
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_event', ch, event)
return, ok
end
