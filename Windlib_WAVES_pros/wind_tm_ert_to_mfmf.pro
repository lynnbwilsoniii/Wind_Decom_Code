function wind_tm_ert_to_mfmf, ch, major, minor, ur8ert
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_ert_to_mfmf', $
   ch, major, minor, ur8ert)
return, ok
end
