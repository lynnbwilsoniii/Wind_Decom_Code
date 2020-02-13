function wind_tm_get_mfmf, ch, major, minor
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_get_mfmf', ch, major, minor)
return, ok
end
