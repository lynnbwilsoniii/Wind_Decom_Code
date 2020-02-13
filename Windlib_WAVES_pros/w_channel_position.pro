function w_channel_position, ch, ur8
ok = 0L
ok = call_external('wind_tm_lib','w_channel_position', ch, ur8)
return, ok
end
