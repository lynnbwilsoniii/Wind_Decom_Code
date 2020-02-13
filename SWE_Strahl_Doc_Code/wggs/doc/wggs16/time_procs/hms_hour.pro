function hms_hour,hhmmss

;converts hhmmss to decimal hour of day

h=fix(hhmmss/10000)
m=(long(hhmmss)-long(h)*10000)/100
s=long(hhmmss)-long(h)*10000-long(m)*100

t=double(h) + double(m)/60.d + double(s)/3600.d
return,t
end
