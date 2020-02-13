function wind_tm_get_word, ch, major, minor, word
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_get_word', ch, major, minor, word)
return, ok
end
