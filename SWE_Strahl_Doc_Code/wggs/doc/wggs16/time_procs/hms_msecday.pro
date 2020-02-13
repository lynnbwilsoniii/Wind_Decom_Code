function hms_msecday,hhmmss

;converts hhmmss to msec of day

h=long(hhmmss/10000)
m=(long(hhmmss)-long(h)*10000)/100
s=long(hhmmss)-long(h)*10000-long(m)*100

ms=h*long(3600000) + m*long(60000) + s*long(1000)
return,ms
end
