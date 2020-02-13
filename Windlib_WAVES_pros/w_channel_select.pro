function w_channel_select, ch, context, t1, t2
ok = 0L
ok = call_external('wind_tm_lib','w_channel_select', ch, context, t1, t2)
return, ok
end
