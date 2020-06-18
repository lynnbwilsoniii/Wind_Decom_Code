function w_ur8_from_ymd_i, ur8, ymd, hms
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), 'w_ur8_from_ymd_i', ur8, ymd, hms)
return, ok
end
