function w_invert_r8, in, out
ok = 0L
ok = call_external('wind_tm_lib', 'w_invert_r8', in, out)
return, ok
end
