function wind_tm_scet_to_mfmf, ch, major, minor, ur8scet
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_scet_to_mfmf', $
   ch, major, minor, ur8scet)
return, ok
end