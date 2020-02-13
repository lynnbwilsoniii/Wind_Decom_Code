pro undo_sec_tjd,sec,tjd,ms

;returns   tjd and ms of day given seconds from tjd epoch 

tjd=long(fix(sec/86400.d))
ms=(sec-tjd*86400.d)


end
