function wind_tm_get_item, ch, item, buf, buf_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'wind_tm_get_item', ch, item, buf, buf_sz, ret_sz)
return, ok
end
