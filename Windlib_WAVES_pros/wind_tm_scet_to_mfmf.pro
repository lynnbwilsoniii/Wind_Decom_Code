function wind_tm_scet_to_mfmf, ch, major, minor, ur8scet
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_scet_to_mfmf', $
   ch, major, minor, ur8scet)
return, ok
end
