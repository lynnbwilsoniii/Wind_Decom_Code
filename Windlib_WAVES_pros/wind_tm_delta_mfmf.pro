function wind_tm_delta_mfmf, major1, minor1, major2, minor2, diff
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_delta_mfmf', $
             major1, minor1, major2, minor2, diff)
return, ok
end
