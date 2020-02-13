function w_ur8_to_string, ur8, str
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(str) eq 0 then str = big
ok = call_external('wind_time2_lib', 'w_ur8_to_string', ur8, str)
str = strtrim(str,2)
return, ok
end
