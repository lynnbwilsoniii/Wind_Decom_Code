pro fredf_isee

;common iseestuff,iseemdat,iseefdat,iseerefsec
common iseestuff,iseefdat
common sharefredf,zgrd,F,nx,ny,vmax,fm,vxm,vym,fi,vxi,vyi,pm,vm,bf ;fredf data
common temp3, phh,theta,format,tim


common temp, xd,yd,zd
print,'fredf_isee :'

if iseefdat.mode eq 0 then mode=2 else mode=iseefdat.mode
if iseefdat.format eq 0 then format=2 else format=iseefdat.format

  if( iseefdat.b(0) le -0.1e10) then begin  ;old version using fdat.b
    ;print,'bad pool mag field, or fill value for 4sec data' 
    print,'CAUTION!! Bad pool magnetic field. You have three choices.'
    print,'1) use the nearest (in time) mag field,' 
    print,'2) enter a value for mag field,
    print,'3) stop.
    print,'Enter 1, 2, or 3 '
    answ=0 & read,answ
    case answ of
    1: begin
         wok=where(iseemdat.b(0) gt -0.1e10)
         mn=min(abs(iseemdat(wok).ta-iseefdat.ta),indx)
         b=iseemdat(wok(indx)).b
         print,'nearest good mag field value to be used is ',b
       endcase
    2: begin
         b=fltarr(3)
         print,'Enter bx, by, bz:'
         read,b
       endcase
    else: stop      
    endcase
    
  endif else b=iseefdat.b
  bf=b
  
inset=iseefdat.inset & ff=iseefdat.f  & spinp=iseefdat.spin  & uout=iseefdat.uout*1e5 & cc=iseefdat.cc  

ndet=6
nsect=6
nv=inset+2
fblok=fltarr(18,ndet,nsect)
fblok(*,*,*)=ff

theta=[2.504943d,1.2877747d,1.0182763d,2.1233161d,1.8538178d,0.63664945d]
sn=sin(theta)
cn=cos(theta)
dphi=[13.79740892d,-53.03900295d,47.291061d,47.291061d,-53.03900295d,13.79740892d]
cyclt=0.48223222d
rad=57.29578d
spind=(6.2831853d/spinp)*rad
phiz=dblarr(ndet,nsect)
phiz(*,*)=((-9.d1+1.8d2*((indgen(ndet)+1)/4)+dphi)#replicate(1,nsect)+replicate(1,ndet)#(spind*cyclt*indgen(nsect)))/rad

;above vectorization equivalent to:
;for j=0,ndet-1 do for k=0,nsect-1 do phiz(j,k)=(-9.d1+1.8d2*((j+1)/4)+spind*cyclt*k+dphi(j))/rad

v=cc(indgen(nv))
tim=[ [0.0144048d,0.0427714d,0.0711380d,0.0995046d,0.1278712d,0.1562378d,$
       0.1846044d,0.2129710d,0.2413376d,0.2697042d,0.2980708d,0.3264374d,$
       0.3548040d,0.3831706d,0.4115372d,0.4399038d,0.4399038d,0.4399038d],$
      [0.0285881d,0.0853213d,0.1278712d,0.1562378d,0.17751275d,0.19169605d,$
       0.20587935d,0.22006265d,0.2413376d,0.2697042d,0.2980708d,0.3264374d,$
       0.3548040d,0.3831706d,0.4115372d,0.4399038d,0.4399038d,0.4399038d]  ]
phh=dblarr(nv,ndet,nsect)
wx=dblarr(nv,ndet,nsect)
wy=dblarr(nv,ndet,nsect)
wz=dblarr(nv,ndet,nsect)
phh(*,*,*)=((spind/rad)*tim(0:nv-1,format-1))#replicate(1,ndet*nsect)+replicate(1,nv)#phiz(*)
vsn=v#sn
vcn=v#cn
;velocity in solar wind frame
wx(*,*,*)=-(vsn(*)#replicate(1,nsect))*cos(phh)-uout(0)*1.d
wy(*,*,*)=-(vsn(*)#replicate(1,nsect))*sin(phh)-uout(1)*1.d
wz(*,*,*)=-vcn(*)#replicate(1,nsect)-uout(2)*1.d

;above vectorization equivalent to:
;for i=0,nv-1 do for j=0,ndet-1 do for k=0,nsect-1 do begin
 ;  phh(i,j,k)=phiz(j,k)+(spind/rad)*tim(i,format-1)
 ;  wx(i,j,k)=-sn(j)*cos(phh(i,j,k))*v(i)-uout(0)*1.d
 ;  wy(i,j,k)=-sn(j)*sin(phh(i,j,k))*v(i)-uout(1)*1.d
 ;  wz(i,j,k)=-cn(j)*v(i)-uout(2)*1.d
;endfor

w=sqrt(wx^2+wy^2+wz^2)
wpara=(wx*b(0)+wy*b(1)+wz*b(2))/sqrt(total(b*b))
wperp=sqrt(w^2-wpara^2)
pa=acos(wpara/w)*rad

;arrange measured data arrays according to energy step and pitch angle, nv by ndet*nsect
nds=ndet*nsect
vpar=fltarr(nv,nds)
vper=fltarr(nv,nds)
f=fltarr(nv,nds)
p=fltarr(nv,nds)
vpar(*,*)=wpara
vper(*,*)=wperp
f(*,*)=fblok(0:nv-1,*,*)
p(*,*)=pa

for i=0,nv-1 do begin ;sort by ascending pitch angle
  psort=sort(p(i,*))
  p(i,*)=p(i,psort)
  vpar(i,*)=vpar(i,psort)
  vper(i,*)=vper(i,psort)
  f(i,*)=f(i,psort)
  endfor
p0edge=fltarr(nv) & p180edge=fltarr(nv)

;measured and interpolated (along 0, 180 deg) data will be put in 6 degree pitch angle bins
dp=6;dp must be even integer
np=fix(180./dp)
kp=fix(p/dp)
ip=fix(p/dp)*dp
wh180=where(ip eq 180,nwh180)
if(nwh180 ne 0) then p(wh180)=179

;put measured data into 6 degree pitch angle bins
vxm=fltarr(nv,np+2)
vym=fltarr(nv,np+2)
fm=fltarr(nv,np+2)
pm=fltarr(nv,np+2)
for i=0,nv-1 do pm(i,indgen(np)+1)=indgen(np)*dp+dp/2
pm(*,0)=0.
pm(*,np+1)=180.
for i=0,nv-1 do for jp=0,np -1 do begin
  wh=where(ip(i,*) eq jp*dp and f(i,*) gt 0,nwh)
  if(nwh ne 0) then begin
    vxm(i,jp+1)=total(vpar(i,wh))/nwh
    vym(i,jp+1)=total(vper(i,wh))/nwh
    fm(i,jp+1)=total(f(i,wh))/nwh
    endif
  endfor
vm=sqrt(vxm^2+vym^2)

;interpolate in data gaps along 0, 180 degrees using parabolic fit, f=intrcpt+slope*pitch^2
vxi=fltarr(nv,np+2)
vyi=fltarr(nv,np+2)
fi=fltarr(nv,np+2)
for i=0,nv-1 do begin
  wh=where(fm(i,*) ne 0,nwh)
  k0=wh(0)
  k1=wh(1)
  intrcpt=(fm(i,k1)*pm(i,k0)^2 -fm(i,k0)*pm(i,k1)^2)/(pm(i,k0)^2-pm(i,k1)^2)
  slope=(fm(i,k0)-fm(i,k1))/(pm(i,k0)^2-pm(i,k1)^2)
  fi(i,0:k0-1)=intrcpt + slope*(pm(i,0:k0-1)-0.)^2  
  vxi(i,0:k0-1)=(vm(i,k0)+vm(i,k1))*0.5*cos(pm(i,0:k0-1)/rad)
  vyi(i,0:k0-1)=(vm(i,k0)+vm(i,k1))*0.5*sin(pm(i,0:k0-1)/rad)

  k0=wh(nwh-1)
  k1=wh(nwh-2)
  intrcpt=(fm(i,k1)*(pm(i,k0)-180.)^2-fm(i,k0)*(pm(i,k1)-180.)^2)/$
      ((pm(i,k0)-180.)^2-(pm(i,k1)-180.)^2)
  slope=(fm(i,k0)-fm(i,k1))/((pm(i,k0)-180.)^2-(pm(i,k1)-180.)^2)
  fi(i,k0+1:np+1)=intrcpt+slope*(pm(i,k0+1:np+1)-180.)^2
  vxi(i,k0+1:np+1)=(vm(i,k0)+vm(i,k1))*0.5*$
      cos(pm(i,k0+1:np+1)/rad )
  vyi(i,k0+1:np+1)=(vm(i,k0)+vm(i,k1))*0.5*$
      sin(pm(i,k0+1:np+1)/rad )
  endfor
vyi(where(abs(vyi) lt 1e3))=0.
;remove any negative values of interpolated data
wh=where(fi lt 0,nwh)
if(nwh ne 0) then fi(wh) =0.

;combine measured and interpolated data
xd=fltarr(nv*(np+2))
yd=fltarr(nv*(np+2))
zd=fltarr(nv*(np+2))
xd(*)=vxm+vxi
yd(*)=vym+vyi
zd(*)=fm+fi

;remove all zero values of zd
whn0=where(zd ne 0,nd)
xd=xd(whn0)
yd=yd(whn0)
zd=zd(whn0)
;now ready to grid the data and contour and compute reduced distribution

nx=128
ny=64
wmax=[15.e8,30.e8,60.e8]
vmax=wmax(mode-1)
triangulate,xd*1e-8,yd*1e-8,tr
zd(where(zd ne 0))=alog10(zd(where(zd ne 0)))
zgrd=trigrid(xd*1e-8,yd*1e-8,zd,tr,[2*vmax*1e-8/(nx-1),vmax*1e-8/(ny-1)],$
  [-vmax*1e-8,0,vmax*1e-8,vmax*1e-8])

;a temporary fix for cases in which zgrd(*,0)=0 (which arose from parabolic fitted fi lt 0)
for i=0,nx-1 do if zgrd(i,0) eq 0 then zgrd(i,0)=zgrd(i,1)
;compute reduced f = F
vperp=indgen(ny)*vmax/(ny-1)
nvperp=ny
reducf,nx,nvperp,zgrd,vperp,F


end

