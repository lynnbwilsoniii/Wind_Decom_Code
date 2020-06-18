function wind_tm_mfmf_to_ert, ch, major, minor, ur8ert
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_mfmf_to_ert', $
   ch, major, minor, ur8ert)
return, ok
end
