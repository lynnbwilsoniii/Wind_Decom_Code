;;;;;;;;;;;;;;;;;;;;;;;;;;;begin decode_utc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;calling procedure:  decode_utc(fcblock(0:8))  (ignores first 2 bytes)
FUNCTION decode_utc,bytes

;  print,bytes
  utc = long(bytes(2))
  utc = utc + ishft(long(bytes(3)),8)
  utc = utc + ishft(long(bytes(4)),16)
  utc = utc + ishft(long(bytes(5)),24)
  
  ms = long(bytes(6))
  ms = ms + ishft(long(bytes(7)),8)
  ms = 2L^16 - 1 - ms
  if ms gt 999 then ms = 0
  
  isec = utc and '1ffff'XL
  hour = isec / 3600
  min  = (isec - long(hour*3600))/60
  isec = isec - hour*3600 - min*60
  tjd  = ishft(utc,-17)
;  print,long(utc),long(tjd)
  time = string(tjd,hour,min,isec,$
        format='("day",i5,i3.2,":",i2.2,":",i2.2)')
  return, time
end
