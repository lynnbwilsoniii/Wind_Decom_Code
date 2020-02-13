function w_ur8_to_ydoy, ur8, year, doy, msec
ok = 0L
ok = call_external('wind_time2_lib', 'w_ur8_to_ydoy', ur8, year, doy, msec)
return, ok
end
