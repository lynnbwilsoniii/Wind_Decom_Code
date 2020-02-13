

savpath=getenv('IDLSAV')
flname=pickfile(/read,get_path=savpath,path=savpath,$
               filter='*.fsav',title='IDLsav Files')
datim=strmid(flname,strlen(getenv('IDLSAV')),17)
lun=8
openr,lun,flname
h=''
readf,lun,h

nx=128
x=fltarr(nx)
fpara=fltarr(nx)
fperp=fltarr(nx)
redf=fltarr(nx)
for j=0,nx-1 do begin
  i=0 & v1=0. & v2=0. & v3=0. & v4=0.  
  readf,lun,i,v1,v2,v3,v4
  x(i)=v1
  fpara(i)=v2
  fperp(i)=v3
  redf(i)=v4
endfor

close,lun 
for i=0,nx-1 do print,i,x(i),fpara(i),fperp(i),redf(i)              
;stop


;overplot model
vmax=x(nx-1)
cntrkappa,vmax,1,f,vx,vy,coredens=7.,nratio=10,kappah=5

charsize=1.50

vscal=1  ;1e8
vfmt='(f5.1)'
frange=[1.e-32,1.e-24] 
vlabl='1000 km/s'
fminor=2
nopts=4e8


hardcopy=0
start:

color=[125,0] 
linestyle=[0,1]
symsize=[0.15,0.25]

vmax=18.0
xticks=4
xrange=[-vmax,vmax]/vscal;range of vx
xminor=3
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks



;stop 
win=0
window,win,xsize=600,ysize=700
plot_io,x(where(fpara ne 0)),fpara(where(fpara ne 0)),$
  xrange=xrange,xstyle=1,xticks=xticks,$
  xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
  xtitle='v!m!d#!n ',yrange=frange,$
  ystyle=1,ytitle='log!d10!nf',$
  psym=0,xminor=xminor,charsize=charsize,title=datim

oplot,x(where(fperp ne 0)),fperp(where(fperp ne 0)),$
  color=color(hardcopy),linestyle=5

oplot,vx,10.^f(*,nx/2),color=75


stop
if hardcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy: ',wst.print_cmd
  spawn, wst.print_cmd  ;'lpr '+pflnm
  wst.hardcopy=0
  clrtbl_indx

  goto,start
endif  


;if plotsurface then surface_mod,zgrd

 end
