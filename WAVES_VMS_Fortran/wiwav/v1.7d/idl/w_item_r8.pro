function w_item_r8, ch, item, buf, buf_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_r8_idl', ch, item, buf, buf_sz, ret_sz)
return, ok
end
