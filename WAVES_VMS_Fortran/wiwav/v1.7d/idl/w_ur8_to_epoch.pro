function w_ur8_to_epoch, ur8, epoch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), 'w_ur8_to_epoch', ur8, epoch)
return, ok
end
