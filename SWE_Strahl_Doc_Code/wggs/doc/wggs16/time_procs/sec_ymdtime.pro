pro sec_ymdtime,seconds,yr,mo,da,h,min,sec,ms

;converts seconds from tjd epoch to date and time as 
;long  integers yr,mo,da,h,min,sec,ms  

pb5=sec_pb5(seconds)

ymd=yrmoda(pb5)

yr=long(strmid(ymd,6,4))
mo=long(strmid(ymd,0,2))
da=long(strmid(ymd,4,2))

hour_hms,double(pb5(2))/3600000.d,hms,hr=h,m=min,s=sec
h=long(h)
min=long(min)
sec=long(sec)
ms=0l
end
