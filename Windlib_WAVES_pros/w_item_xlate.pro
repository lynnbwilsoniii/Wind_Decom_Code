function w_item_xlate, ch, event, item, item_val, item_str
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(item_str) eq 0 then item_str = big
ok = call_external('wind_tm_lib','w_item_xlate', $
     ch, event, item, item_val, item_str)
item_str = strtrim(item_str,2)
return, ok
end
