; carr.pro - tests idl character arrays with wind_lib
@wdir:vms_wind_idl.pro

s = strarr(12)
ch = 0L
ok = 0L
ret_size = 0L
i = 0L
j = 0L
k = 0L
n = 0L
        sr4_buffer   = fltarr(1024)
        s_size     = long(1024)
        s_retsize  = long(0)

ok = w_channel_open(ch,'offline')

ok = w_event(ch,'RAD1')

n = 12L
ok = w_item_char(ch,'PACKET_ID_ARRAY',s,n,ret_size)

print, 'ok, ret_size=', ok, ret_size
for i=0,11 do begin
   print, i, ' ', s(i)
endfor

print, '...getting s_mv_r4...'
ok = w_item_r4(ch, 'S_mv_r4', sr4_buffer, s_size, s_retsize)
print, sr4_buffer(0:3)

stop
end
