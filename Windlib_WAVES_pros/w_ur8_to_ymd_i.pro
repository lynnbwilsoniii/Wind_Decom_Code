function w_ur8_to_ymd_i, ur8, ymd, hms
ok = 0L
ok = call_external('wind_time2_lib', 'w_ur8_to_ymd_i', ur8, ymd, hms)
return, ok
end
