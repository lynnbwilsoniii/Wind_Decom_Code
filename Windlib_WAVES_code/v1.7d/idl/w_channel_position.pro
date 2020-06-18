function w_channel_position, ch, ur8
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_position', ch, ur8)
return, ok
end
