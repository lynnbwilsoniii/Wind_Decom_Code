function wind_tm_version, ver
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(ver) eq 0 then ver = big
ok = call_external('wind_tm_lib','wind_tm_version', ver)
ver = strtrim(ver,2)
return, ok
end
