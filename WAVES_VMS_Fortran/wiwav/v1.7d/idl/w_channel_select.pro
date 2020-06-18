function w_channel_select, ch, context, t1, t2
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_select', ch, context, t1, t2)
return, ok
end
