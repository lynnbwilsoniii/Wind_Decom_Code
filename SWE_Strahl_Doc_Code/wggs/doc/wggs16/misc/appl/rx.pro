pro rx,dens,umag,bxin,byin,bzin,gsex,gsey,gsez,r,x,new=new,$
  th=th,x0r=x0r,y0r=y0r,z0r=z0r,$
  bxr=bxr,byr=byr,bzr=bzr,xgser=xgser,ygser=ygser,zgser=zgser,$
  sina=sina,cosa=cosa,a1=a1,a2=a2,shckerr=shckerr,magerr=magerr

common wstuff,wst
  
;if new=0 then use old isee parabola shock geometry
;if new=1 then use improved sibeck2.pro shock geometry

bx=bxin & by=byin & bz=bzin

rad=!pi/180.

if keyword_set(new) ne 0 then begin
  ;shock parameters for 77309-310 are dynamically adjusted for current ram press
  a10=15.576
  a20=0.0375399
  dens0=12.5
  u0=280.    ;e5
  c=((dens/dens0)*(umag/u0)^2)^0.16667
  a1=a10/c
  a2=a20*c
endif else begin   ;Sibeck model
  mp=1.6726e-24
  p=mp*dens*(umag*1.e5)^2/(2.088e-8)
  a1=623.77/(44.919*p^0.1666)
  a2=(1./44.919)*p^0.1666
endelse  

if keyword_set(shckerr) ne 0 then begin
  ;let error in shape of shock be shckerr = per cent 
    a1=(1.0 + shckerr)*a1
    a2=(1.0 - shckerr)*a2
endif    

if keyword_set(magerr) ne 0 then begin    
  ;let error in magnetic field direction be 5 deg
     bmag=sqrt(bxin^2+byin^2+bzin^2)
     thetab=asin(byin/bmag)/!dtor
     phib=atan(byin,bxin)/!dtor
     wlt=where(byin lt bxin)
     if wlt(0) ne -1 then phib(wlt)=phib(wlt)+360.
     dthetab=magerr & dphib=magerr
     bx=bmag*sin((thetab+dthetab)*!dtor)*cos((phib+dphib)*!dtor)
     by=bmag*sin((thetab+dthetab)*!dtor)*sin((phib+dphib)*!dtor)
     bz=bmag*cos((thetab+dthetab)*!dtor)     
endif 
 
;rotate s/c gse coords about xgse so that new xy plane contains b and u

byz=sqrt(by^2+bz^2)
sina=bz/byz
cosa=by/byz

;s/c position in rotated gse
xgser=gsex
ygser=gsey*cosa    + gsez*sina
zgser=gsey*(-sina) + gsez*cosa

;mag field in rotated gse
bxr=bx
byr=byz
bzr=0
bmag=sqrt(bx^2+byz^2)

;angle between b and xgse
th=acos(bxr/bmag)/rad
sinth=sin(th*rad)
costh=cos(th*rad)

;coords (rotated gse) of magnetic tangent point = origin of r-x (foreshock) coord system
z0r=zgser
y0r=-(0.5/a2)*costh/sinth
     
wok=where(abs(z0r) lt 1e3 and abs(y0r) lt 1e3)

x0r=-1.e31+fltarr(n_elements(dens)) 
x0r(wok)=a1-a2*(y0r(wok)^2+z0r(wok)^2)

;for i=0,n_elements(dens)-1 do print,x0r(i),y0r(i),z0r(i)

;transform s/c position from rotated gse to foreshock r-x coords
r=1.e6+fltarr(n_elements(dens))
x=1.e6+fltarr(n_elements(dens))
r(wok)=(xgser(wok)-x0r(wok))*costh(wok)+(ygser(wok)-y0r(wok))*sinth(wok)
x(wok)=(xgser(wok)-x0r(wok))*(-sinth(wok))+(ygser(wok)-y0r(wok))*costh(wok)

if n_elements(dens) eq 1 then begin
  x0r=x0r(0)
  r=r(0)
  x=x(0)
endif


;k=0
;rx_test,sina(k),cosa(k),$
; a1(k),a2(k),gsex(k),gsey(k),gsez(k),xgser(k),ygser(k),zgser(k),$
; bx(k),by(k),bz(k),bxr(k),byr(k),bzr(k),th(k),costh(k),sinth(k),$
; z0r(k),y0r(k),x0r(k),r(k),x(k)


;stop

end
