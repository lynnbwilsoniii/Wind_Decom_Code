function wind_tm_get_filename, ch, filename
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(filename) eq 0 then filename = big
ok = call_external('wind_tm_lib','wind_tm_get_filename', ch, filename)
filename = strtrim(filename,2)
return, ok
end
