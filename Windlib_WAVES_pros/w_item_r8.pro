function w_item_r8, ch, item, buf, buf_sz, ret_sz
ok = 0L
ok = call_external('wind_tm_lib','w_item_r8', ch, item, buf, buf_sz, ret_sz)
return, ok
end
