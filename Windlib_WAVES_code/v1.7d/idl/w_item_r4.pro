function w_item_r4, ch, item, buf, buf_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_r4_idl', ch, item, buf, buf_sz, ret_sz)
return, ok
end
