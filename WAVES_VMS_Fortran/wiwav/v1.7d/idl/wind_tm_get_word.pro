function wind_tm_get_word, ch, major, minor, word
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')),'wind_tm_get_word', ch, major, minor, word)
return, ok
end
