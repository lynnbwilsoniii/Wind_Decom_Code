pro hour_hms,t,hms,hm=hm,hh=hh,hr=hr,m=m,s=s,lhms=lhms,mod24=mod24


;converts decimal hour to 
;hms=hh:mm:ss
;h= hour(NOT modulo 24), m= minutes, s=nearest second

tp=t   +0.5d/3600.d
h=fix(tp)
h_mod=fix(tp-fix(tp/24.d)*24.d)
if keyword_set(mod24) eq 0 then hr=h else begin
  hr=h_mod
  ;if hr(n_elements(hr)-1) eq 0 then hr(n_elements(hr)-1)=24
  ;w0=where(hr eq 0,nw0)
  ;if nw0 ge 2 then hr(w0(nw0-1))=24
endelse


m=fix((tp-double(h))*60.)
s=fix(tp*3600.-h*double(3600.)-m*double(60.))
hms=strarr(n_elements(tp))
lhms=strarr(n_elements(tp))
for i=0,n_elements(tp)-1 do begin
       hms(i)=string(format='(i2.2)',hr(i))+':'+string(format='(i2.2)',m(i))$
       +':'+string(format='(i2.2)',s(i))
  if hr(i) le 9 then lhms(i)='0'+string(hr(i),format='(i1)') else $
                     lhms(i)=string(hr(i),format='(i2.2)')
  if m(i) le 9 then lhms(i)=lhms(i)+'0'+string(m(i),format='(i1)') else $
                     lhms(i)=lhms(i)+string(m(i),format='(i2.2)')
  if s(i) le 9 then lhms(i)=lhms(i)+'0'+string(s(i),format='(i1)') else $
                     lhms(i)=lhms(i)+string(s(i),format='(i2.2)')   
endfor

hm=strarr(n_elements(tp))
if total(s) eq 0 $
  then  for i=0,n_elements(tp)-1 do $
          hm(i)=string(format='(i2.2)',hr(i))+':'+string(format='(i2.2)',m(i))
hh=strarr(n_elements(tp))
if total(m) eq 0 and total(s) eq 0$
  then  for i=0,n_elements(tp)-1 do $
          hh(i)=strtrim(string(format='(i2.2)',hr(i)),1)


end
