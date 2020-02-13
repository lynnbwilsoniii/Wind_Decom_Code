function wind_tm_get_item, ch, item, buf, buf_sz, ret_sz
ok = 0L
ok = call_external('wind_tm_lib','wind_tm_get_item', ch, item, buf, buf_sz, ret_sz)
return, ok
end
