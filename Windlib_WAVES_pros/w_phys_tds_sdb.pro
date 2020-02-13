
function w_phys_tds_sdb, ch, iprc, ndata, vdata, spect
ok = 0L
ok = call_external('wind_tm_lib',  $
     'w_tds_phys_sdb', $
     ch, iprc, ndata, vdata, spect)
return, ok
end
