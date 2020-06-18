function wind_tm_mfmf_to_scet, ch, major, minor, ur8scet
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_mfmf_to_scet', $
   ch, major, minor, ur8scet)
return, ok
end
