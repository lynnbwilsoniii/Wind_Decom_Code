function w_ur8_from_ymd, ur8, year, month, day, hour, minute, second, msec
ok = 0L
ok = call_external('wind_time2_lib', 'w_ur8_from_ymd', $
     ur8, year, month, day, hour, minute, second, msec)
return, ok
end
