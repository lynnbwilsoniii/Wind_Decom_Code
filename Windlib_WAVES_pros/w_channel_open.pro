function w_channel_open, ch, context
ok = 0L
ok = call_external('wind_tm_lib','w_channel_open', ch, context)
return, ok
end
