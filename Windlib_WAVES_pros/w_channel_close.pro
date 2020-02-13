function w_channel_close, ch
ok = 0L
ok = call_external('wind_tm_lib','w_channel_close', ch)
return, ok
end
