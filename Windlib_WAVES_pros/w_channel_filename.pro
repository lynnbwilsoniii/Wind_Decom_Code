function w_channel_filename, ch, filename
ok = 0L
big = '                                                  '
if strlen(filename) eq 0 then filename = big+big+big
ok = call_external('wind_tm_lib','w_channel_filename', ch, filename)
filename = strtrim(filename,2)
return, ok
end
