function w_channel_close, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_close', ch)
return, ok
end
