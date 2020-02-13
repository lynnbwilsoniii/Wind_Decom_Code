function tjdepoch_sec,sec

;returns   tjd and ms of day given seconds from tjd epoch 

tjd=long(fix(sec/86400.d))

ms=long((sec-double(tjd)*86400.d)*1000l)

return,[tjd,ms]

end
