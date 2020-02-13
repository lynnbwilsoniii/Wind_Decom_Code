

pro sibeck2,rhomp,xmp,rhobs,xbs,a=a,b=b,c=c,press=press,bz=bz0

; xmp - x [RE] location of magnetopause
; rhomp = sqrt(ymp^2+zmp^2) distance of mp from x-axis 
; xbs - x [RE] location of bowshock 
; rhobs = sqrt(ybs^2+zbs^2) distance of bs from x-axis 
; press - solar wind pressure in nPa
; bz - IMF Bz component in nT
; note: I clip bz if |bz| > 6.5 nT
; also for low pressure (i.e. .1 nT) the subsolar point moves earthward
; instead of anti-earthward
if n_elements(bz0) eq 0 then read,'input IMF bz(nT)?',bz0
if n_elements(press) eq 0 then read,'input SW pressure(nPa)?',press
if abs(bz0) gt 6.5 then begin
   bz = bz0/abs(bz0)*6.5
   print,' |Bz| clipped at 6.5'
endif else bz = float(bz0)
y=bz+0.1635
x=press/2.088
z=alog(x)
a=0.171*x^(-0.474-0.616*z+0.023*y)*exp(-0.043*y+0.0391*y*y)
b=18.80*x^(-0.120-0.030*z+0.036*y)*exp(-0.037*y+0.0002*y*y)
c=-220.8*x^(-0.290-0.110*z+0.018*y)*exp(-0.012*y+0.0017*y*y)
pt = 100
rhomp = indgen(pt)*1.
xmp = fltarr(pt)
for j=0,pt-1 do begin
    r=rhomp(j)
    r2=r*r
    fact=b*b-4.0*a*(c+r2)
    if(fact lt 0.0) then goto, jump
    xx=(-b+sqrt(fact))/(2.0*a)
    xmp(j) = xx
endfor
jump:
if( j lt (pt-1) )then begin
    rhomp(j:pt-1) = rhomp(j-1)
    xmp(j:pt-1) = xmp(j-1) - (1 + indgen(pt-j) )
endif
;
;
pt=80
rhobs = fltarr(pt)
xbs = fltarr(pt)
for j=0,pt-1 do begin
    r=float(j)
    r2=r*r
    xxx=(-r2+623.77*x^(-0.3333))/(44.916*x^(-0.1666))
    rhobs(j) = r
    xbs(j) = xxx
endfor
rhobs = rhobs(0:j-1)
xbs = xbs(0:j-1)
plot,xbs,rhobs
oplot,xmp,rhomp
end



