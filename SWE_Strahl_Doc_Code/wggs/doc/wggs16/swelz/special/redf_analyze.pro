pro redf_analyze,xx,FF,xxoplt

common shareredf,sbase,button1,button2,draw1,draw2,field1,field2,$
  ndistribs,xc1,xc2,sgnslop,x1,F1,x2,F2
common sharefevents,fevents
common wstuff,wst
common swestuff,swest


xoplt=xxoplt
wok=where(FF gt 0)
F=FF(wok)
x=xx(wok)

if xoplt lt 0 then begin
  xoplt=-xoplt
  x=reverse(-x)
  F=reverse(F)
  signx=-1
endif else signx=1

k=3
w=where(x gt xoplt,nw)
if nw le k then return

;determine sign of slope over k data points

if F(w(k-1))-F(w(0)) gt 0 then begin   ;positive slope

  sgnslop=1
  n1=w(0)-3
  n3=n_elements(F)-2
  diff=F(n1+1:n3+1) - F(n1:n3)
  wpos=where(diff ge 0,nwpos)
  x1=signx*x(n1+wpos(0))
  F1=F(n1+wpos(0))
  for i=n1+wpos(0),n3 do begin
    n2=i
    if F(i+1) lt F(i) then goto,point0
  endfor
  point0:
  x2=signx*x(n2)
  F2=F(n2)
endif else if F(w(k-1))-F(w(0)) lt 0 then begin  ;negative slope

  sgnslop=-1
  x1=signx*xoplt
  F1=F(w(0))-(x(w(0))-xoplt)*(F(w(0))-F(w(0)-1))/(x(w(0))-x(w(0)-1))
  x2=0
  F2=0
  
endif else if  F(w(k-1))-F(w(0)) eq 0 then begin   ;zero slope

  sgnslop=0
  x1=signx*xoplt
  F1=F(w(0))-(x(w(0))-xoplt)*(F(w(0))-F(w(0)-1))/(x(w(0))-x(w(0)-1))
  n1=w(0)
  n2=n_elements(F)-2
  for i=n1,n2 do begin
    if F(i+1)-F(i) ne 0 then goto,point1
  endfor
  point1: 
  x2=signx*x(i)
  F2=F(i)
  
endif

;print,'ndistribs,sgnslop,x1,F1,x2,F2 ',ndistribs,sgnslop,x1,F1,x2,F2
;answ='' & read,answ & if answ ne '' then stop 


end