pro undo_makestring,tjd_ms,tjd,ms

;extract variable from string

 tjd=long(strmid(xhms,0,5))
 ms=long(strmid(xhms,6,8))
 
 msd=h*3600000+h*60000+s*1000+ms

end
