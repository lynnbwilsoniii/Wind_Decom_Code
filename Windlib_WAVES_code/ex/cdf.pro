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
r8=0.0d
ok=w_item_r8(ch,'polar_orbit_x(gse)_r8',r8,1l,ret)
print, 'item_r8 ok=', ok, r8, ret
stop
end
