function pb5_elapsec,pb5,pb5ref

;Given pb5 time and pbr reference time, this function computes the 
;  time difference pb5-pb5ref in seconds

;This is done by first convering pb5 and pb5ref to sec fron truncated julian 
;  date epoch using procedure pb5_sec.pro which takes into account the 
;  rollover in truncated julian date tjd to tjd=0 on pb5=[1995 283 0], Oct 10

sec=pb5_sec(pb5)
secref=pb5_sec(pb5ref)
diff=sec-secref
if diff lt 0.d then diff=10000*double(86400.) + diff

return,diff

end
