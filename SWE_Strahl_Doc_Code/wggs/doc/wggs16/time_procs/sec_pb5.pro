function sec_pb5,seconds,err=err

;converts seconds from truncated julian data epoch to pb5 time (y d millisec of day)

;first find trucated julian day and millisec of day
;then convert to pb5 time using procedure tjd_pb5.pro

;rollover in truncated julian day (to tjd=0) occurred pb5=[1995 283 0], Oct 10
;rollover is accounted for in procedure tjd_pb5.pro

tjd=long(fix(seconds/86400.d))
ms=long((seconds - tjd*86400.d)*1000)

timpb5=tjd_pb5(tjd,ms,err=err)  ;convert tjd,ms to pb5 time

return,timpb5

end
  


