function w_ur8_to_epoch, ur8, epoch
ok = 0L
ok = call_external('wind_time2_lib', 'w_ur8_to_epoch', ur8, epoch)
return, ok
end
