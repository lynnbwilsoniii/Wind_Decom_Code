function w_channel_open, ch, context
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_open', ch, context)
return, ok
end
