function wind_tm_mfmf_to_scet, ch, major, minor, ur8scet
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_mfmf_to_scet', $
   ch, major, minor, ur8scet)
return, ok
end
