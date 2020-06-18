function wind_tm_increment_mfmf, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_increment_mfmf', major, minor)
return, ok
end
