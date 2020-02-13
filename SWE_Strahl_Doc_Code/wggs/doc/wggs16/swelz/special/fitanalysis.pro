pro kappafit,vmax,coredens,vcore,halodens,vhalo,uhalo,kappa_halo,core,halo,$
  vx,fx,vy,fy,vxcore_finite,fcore_cut_finite,vxhalo_finite,fhalo_cut_finite

;sw kappa distribution, vpara, vperp in sw frame
;velocity in  units of 1e8 cm/s

nx=127
ny=127
vpara=fltarr(nx,ny)
vperp=fltarr(nx,ny)

vx=-vmax+findgen(nx)*2*vmax/(nx-1)
vy=-vmax+findgen(ny)*2*vmax/(ny-1)
for j=0,ny-1 do vpara(indgen(nx),j)=vx
for i=0,nx-1 do vperp(i,indgen(ny))=vy

 
k1=20.;core
ak1=gamma(k1+1.)/(gamma(k1-0.5)*k1^1.5)
normal1= 1e-24*coredens/(sqrt(!pi)*vcore)^3

k2=float(kappa_halo) ;halo
ak2=gamma(k2+1.)/(gamma(k2-0.5)*k2^1.5)
normal2=1e-24*halodens/(sqrt(!pi)*vhalo)^3

ucore=-halodens*uhalo/coredens

arg1=((vpara-ucore)/vcore)^2+(vperp/vcore)^2
arg2=((vpara-uhalo)/vhalo)^2+(vperp/vhalo)^2

core=normal1*ak1/(1.+arg1/k1)^(k1+1.)
halo=normal2*ak2/(1.+arg2/k2)^(k2+1.)


fcore=alog10(core)
fhalo=alog10(halo)
f=alog10(core+halo)

fx=f(*,ny/2)
fy=f(nx/2,*)

wfcore_finite=where(fcore(*,ny/2))
fcore_cut_finite=fcore(wfcore_finite,ny/2)
vxcore_finite=vx(wfcore_finite)

wfhalo_finite=where(fhalo(*,ny/2))
fhalo_cut_finite=fhalo(wfhalo_finite,ny/2)
vxhalo_finite=vx(wfhalo_finite)
      
lplot=0;1
if lplot then begin

  nc=fix(max(f)+0.5)-fix(min(f)-0.5) + 1
  cl=float(fix(min(f)-0.5)) + findgen(nc)   ;set contour levels

  corners=[100.,75.,500.,475.] 
  ;lower left, upper right corners of x,y in device coords 

  window,0
  contour,f,vx,vy,/follow,levels=cl,$
    /device,position=corners,title='kappa sw distribution',$
    xtitle='vpara (1000 km/s)',ytitle='vperp (1000 km/s)'

  window,4
  ;plot cuts
  plot,/nodata,vx,f(*,ny/2)
  oplot,vy,f(nx/2,*),linestyle=2
  oplot,vx,f(*,ny/2)

  oplot,vxcore_finite,fcore_cut_finite,color=75

  oplot,vxhalo_finite,fhalo_cut_finite,color=125

endif

end



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< main >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pro fitanalysis

vmax=18.1
coredens=10.
vcore=2.
halodens=coredens/50
vhalo=sqrt(5.)*vcore
uhalo=0.
kappa_halo=7.  ;3.  ;4.  ;5.

kappafit,vmax,coredens,vcore,halodens,vhalo,uhalo,kappa_halo,core,halo,$
  velx,logfx,vely,logfy,velcore,logfcore,velhalo,logfhalo

lplot=1
if lplot then begin
  window,4
  ;plot parallel cut
  plot,velx(where(velx ge 0)),logfx(where(velx ge 0))
  ;oplot,vely,logfy,linestyle=2

  oplot,velcore(where(velcore ge 0)),logfcore(where(velcore ge 0)),color=75

  oplot,velhalo(where(velhalo ge 0)),logfhalo(where(velhalo ge 0)),color=125
endif
  
end