function w_phys_tds_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),  $
     'w_phys_tds_r8', ch, cal, start_sz, ret_sz)
return, ok
end
