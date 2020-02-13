function w_phys_tds_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external('wind_tm_lib', 'w_phys_tds_r8', ch, cal, start_sz, ret_sz)
return, ok
end
