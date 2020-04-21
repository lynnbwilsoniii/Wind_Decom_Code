; @(#)ms_hms.pro  VERSION 1.2    7/28/94   16:13:27
pro ms_hms,ms,h,m,s
;converts msec of day to hour, min, sec
hd=ms/3600000.d0
h=fix(hd)
md=(hd-h)*60.d0
m=fix(md)
s=(md-m)*60.d0
end
