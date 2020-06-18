function w_messages_off, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_messages_off', ch)
return, ok
end
