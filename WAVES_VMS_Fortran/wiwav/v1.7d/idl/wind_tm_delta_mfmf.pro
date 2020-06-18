function wind_tm_delta_mfmf, major1, minor1, major2, minor2, diff
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_delta_mfmf', $
             major1, minor1, major2, minor2, diff)
return, ok
end
