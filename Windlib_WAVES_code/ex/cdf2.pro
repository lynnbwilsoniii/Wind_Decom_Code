ok=0l
ch=0l
ok=w_channel_open(ch,'*19960401*')
print, 'open ok=', ok
ok=w_event(ch,'HK')
print,'event ok=', ok
i4=0l
ret=0l
sz=0l
ok=w_item_i4(ch,'dpu_major_frame',i4,1l,ret)
print,'item_i4 ok=', ok, i4, ret
r4=0.0
ok=w_item_r4(ch,'wind_mfi_bx(gse)_r4',r4,1l,ret)
print, 'item_r4 ok=', ok, r4, ret
stop
end
