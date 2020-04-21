; @(#)time_utc.pro  VERSION 1.2    7/28/94   16:13:34
pro time_utc,data,tjd,sec,hour,min,isec,ms,hms,spincnt

utc=0l
utc=long(data(0))
utc=utc + ishft(long(data(1)),8)
utc=utc + ishft(long(data(2)),16)
utc=utc + ishft(long(data(3)),24)

ms=0
ms=long(data(4))
ms=ms + ishft(long(data(5)),8) 
ms=2l^16-1 - ms ;ones complement
if ms gt 999 then ms=0


tjd=ishft(utc,-17)
isec=utc and '1ffff'xl
hour=isec/3600
sec=double(isec) + double(ms)/1000.d0
min=(isec-hour*3600)/60
isec=isec-hour*3600-min*60
hms=string(hour,format='(i2)')+':'+string(min,format='(i2.2)')+':'+$
  string(isec,format='(i2.2)')+'.'+string(ms,format='(i3)')
spincnt=data(6)

end
