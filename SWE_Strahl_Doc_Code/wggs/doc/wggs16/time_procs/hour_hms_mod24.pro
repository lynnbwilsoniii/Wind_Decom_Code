pro hour_hms_mod24,t,hms,hm,hh
;converts decimal hour to hour(modulo 24), minutes, nearest second
tp=t+0.5d/3600.d
h=fix(tp)
h_mod=fix(tp-fix(tp/24.d)*24.d)
m=fix((tp-float(h))*60.)
s=fix(tp*3600.-h*3600.-m*60.)
hms=strarr(n_elements(tp))
for i=0,n_elements(tp)-1 do $
       hms(i)=string(format='(i2)',h_mod(i))+':'+string(format='(i2)',m(i))$
       +':'+string(format='(I2)',s(i))
hm=strarr(n_elements(tp))
if total(s) eq 0 $
  then  for i=0,n_elements(tp)-1 do $
          hm(i)=string(format='(i2)',h_mod(i))+':'+string(format='(i2)',m(i))
hh=strarr(n_elements(tp))
if total(m) eq 0 and total(s) eq 0$
  then  for i=0,n_elements(tp)-1 do $
          hh(i)=string(format='(i2)',h_mod(i))
end
