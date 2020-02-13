function w_status, ch
ok = 0L
ok = call_external('wind_tm_lib','w_status', ch)
return, ok
end
