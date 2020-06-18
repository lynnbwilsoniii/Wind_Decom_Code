function w_channel_filename, ch, filename
ok = 0L
big = '                                                  '
if strlen(filename) eq 0 then filename = big+big+big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_filename', ch, filename)
filename = strtrim(filename,2)
return, ok
end
