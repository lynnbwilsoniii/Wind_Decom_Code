function w_item_xlate, ch, event, item, item_val, item_str
ok = 0L
big = '                                                  '
if strlen(item_str) eq 0 then item_str = big+big+big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_xlate', $
     ch, event, item, item_val, item_str)
item_str = strtrim(item_str,2)
return, ok
end
