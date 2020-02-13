
;================= fparr ================================================


pro fparr,fe,wx,wy,wz,w,npatch,nvmin,velocity,vpot,mag,$
  pltype=pltype,err=err


common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
  f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common onecount,f1ct,v1ct
common sharetriangl,xd,yd,zd,tr
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common wstuff,wst
common swestuff,swest
common sharefparr,Fvprp,vygrid    
common share_ctr_ep,elog,pmin,pmax,emin,emax,e0cut,e180cut

;---Prepare arrays for contouring and plotting;

;---data arrays include patch and measured data above scpot;
sz=size(fe)
ndet=sz(1)
nv=sz(2)
nsect=sz(3)

;---find polar components (speed, pitch angle) and (parallel, perpendicular);
;   arrange data arrays according to energy and pitch angle, nv by ndet*nsect;
;   data arrays are velocity vectors and f's;
;   glint points are negative f's and are excluded in the test for f gt 0 
nds=ndet*nsect
vpar=fltarr(nv,nds)
vper=fltarr(nv,nds)
fp=fltarr(nv,nds)
pa=fltarr(nv,nds)
for j=1,nv-1 do begin
  ids=-1
  for k=0,nsect-1 do for i=0,ndet-1 do begin
    ids=ids+1
    vpar(j,ids)=$
     (wx(i,j,k)*mag(0)+wy(i,j,k)*mag(1)+wz(i,j,k)*mag(2))/sqrt(total(mag*mag))
    vper(j,ids)=sqrt(w(i,j,k)^2-vpar(j,ids)^2)
    fp(j,ids)=fe(i,j,k)
    pa(j,ids)=acos(vpar(j,ids)/w(i,j,k))/!dtor
  endfor
endfor

;---sort by ascending pitch angle
for i=0,nv-1 do begin 
  psort=sort(pa(i,*))
  pa(i,*)=pa(i,psort)
  vpar(i,*)=vpar(i,psort)
  vper(i,*)=vper(i,psort)
  fp(i,*)=fp(i,psort)
endfor

;---data samples that are closest to parallel, anti-parallel and perpendicular
f0cut=fltarr(nv) & v0cut=fltarr(nv) & p0cut=fltarr(nv) & e0cut=fltarr(nv)
f180cut=fltarr(nv) & v180cut=fltarr(nv) & e180cut=fltarr(nv) 
p180cut=fltarr(nv) & f90cut=fltarr(nv) & v90cut=fltarr(nv)
for i=npatch,nv-1 do begin
  wh=where(fp(i,*) ge 0,nw)    ;ge 0 test excludes glint points
  if nw ne 0 then begin
    f0cut(i)=fp(i,wh(0)) & v0cut(i)=vpar(i,wh(0))
    e0cut(i)=(vpar(i,wh(0))^2+vper(i,wh(0))^2)*2.85e-16
    p0cut(i)=pa(i,wh(0))
    f180cut(i)=fp(i,wh(nw-1)) & v180cut(i)=vpar(i,wh(nw-1))
    e180cut(i)=( vpar(i,wh(nw-1))^2+vper(i,wh(nw-1))^2)*2.85e-16
    p180cut(i)=pa(i,wh(nw-1))
  endif
  wlt90=where(pa(i,*) le 90.,nwlt)
  wgt90=where(pa(i,*) ge 90.,nwgt) 
  if wlt90(0) ne -1 and wgt90(0) ne -1 then begin
     f90cut(i)=(fp(i,wlt90(nwlt-1))+fp(i,wgt90(0)))/2
     v90cut(i)=(vper(i,wlt90(nwlt-1))+vper(i,wgt90(0)))/2
  endif
endfor
 
;---measured and interpolated data will be put in pitch angle bins = dp degrees
dp=float(swest.pbinselect)
np=fix(180./dp)
kp=fix(pa/dp)
ip=fix(pa/dp)*dp
wh180=where(ip eq 180,nwh180)
if(nwh180 ne 0) then ip(wh180)=179

;---measured data
vxm=fltarr(nv,np+2)
vym=fltarr(nv,np+2)
fm=fltarr(nv,np+2)
pm=fltarr(nv,np+2)
em=fltarr(nv,np+2)
for i=0,nv-1 do pm(i,indgen(np)+1)=indgen(np)*dp+dp/2
pm(*,0)=0.
pm(*,np+1)=180.
for i=0,nv-1 do begin
  ;em(i,*)=w(0,i,0)*w(0,i,0)*2.85e-16
  for jp=0,np -1 do begin  ;ge 0 test excludes glint points
    ;wh=where(ip(i,*) eq jp*dp and fp(i,*) gt 0,nwh)
    wh=where(ip(i,*) eq jp*dp and fp(i,*) ge 0,nwh)     
    if(nwh ne 0) then begin
      vxm(i,jp+1)=total(vpar(i,wh))/nwh
      vym(i,jp+1)=total(vper(i,wh))/nwh
      fm(i,jp+1)=total(fp(i,wh))/nwh
    endif
  endfor
endfor
vm=sqrt(vxm^2+vym^2)

;---set to zero vxm and vym where fm=0
wfm0=where(fm le 0)
if wfm0(0) ne -1 then begin
  vxm(wfm0)=0 & vym(wfm0)=0
endif
  
;---interpolate in data gaps along 0, 180 degrees using parabolic fit,   
;   (f=intrcpt+slope*pitch^2)
vxi=fltarr(nv,np+2)
vyi=fltarr(nv,np+2)
fi=fltarr(nv,np+2)

pgaplim=30.
kedge=[0,1]
if swest.gap_interpol then begin
  for i=0,nv-1 do begin
    wh=where(fm(i,*) ne 0,nwh)
    if nwh gt 2 then begin
    k0=wh(kedge(0))
    k1=wh(kedge(1))
    if pm(i,k0) le pgaplim then begin
      intrcpt=(fm(i,k1)*pm(i,k0)^2 -fm(i,k0)*pm(i,k1)^2)/(pm(i,k0)^2-pm(i,k1)^2)
      slope=(fm(i,k0)-fm(i,k1))/(pm(i,k0)^2-pm(i,k1)^2)
      if i ge npatch then begin
        if intrcpt lt 0.5*f1ct(i-npatch) then begin
          intrcpt=(f1ct(i-npatch)+fm(i,k0))/2
          slope=(intrcpt-fm(i,k0))/(-pm(i,k0)^2)
        endif
      endif  
      fi(i,0:k0-1)=intrcpt + slope*(pm(i,0:k0-1)-0.)^2  
      vxi(i,0:k0-1)=(vm(i,k0)+vm(i,k1))*0.5*cos(pm(i,0:k0-1)*!dtor)
      vyi(i,0:k0-1)=(vm(i,k0)+vm(i,k1))*0.5*sin(pm(i,0:k0-1)*!dtor)
      vm(i,0:k0-1)=sqrt(vxi(i,0:k0-1)^2+vyi(i,0:k0-1)^2)
    endif
    k0=wh(nwh-1-kedge(0))
    k1=wh(nwh-1-kedge(1))
    if 180.-pm(i,k0) le pgaplim then begin
      intrcpt=(fm(i,k1)*(pm(i,k0)-180.)^2-fm(i,k0)*(pm(i,k1)-180.)^2)/$
        ((pm(i,k0)-180.)^2-(pm(i,k1)-180.)^2)
      slope=(fm(i,k0)-fm(i,k1))/((pm(i,k0)-180.)^2-(pm(i,k1)-180.)^2)
      if i ge npatch then begin
        if intrcpt lt 0.5*f1ct(i-npatch) then begin
          intrcpt=(f1ct(i-npatch)+fm(i,k0))/2
          slope=(intrcpt-fm(i,k0))/(-(pm(i,k0)-180.)^2)
        endif  
      endif
      fi(i,k0+1:np+1)=intrcpt+slope*(pm(i,k0+1:np+1)-180.)^2
      vxi(i,k0+1:np+1)=(vm(i,k0)+vm(i,k1))*0.5*$
        cos(pm(i,k0+1:np+1)*!dtor )
      vyi(i,k0+1:np+1)=(vm(i,k0)+vm(i,k1))*0.5*$
        sin(pm(i,k0+1:np+1)*!dtor )
      vm(i,k0+1:np+1)=sqrt(vxi(i,k0+1:np+1)^2+vyi(i,k0+1:np+1)^2)  
      ;print,i,k0,pm(i,k0),vm(i,k0),fm(i,k0),k1,pm(i,k1),vm(i,k1),fm(i,k1),$
      ;  format='(i5,2(i5,f10.4,2e12.4))'
      ;print,intrcpt,slope;,f1ct(i) 
      ;for j=0,np+1-k0-1 do print,vxi(i,k0+1+j),vyi(i,k0+1+j),fi(i,k0+1+j),$
      ;  format='(3e12.4)' 
      ;stop
    endif  
    ;for j=0,31 do print,i,j,fm(i,j),fi(i,j),vxm(i,j),vym(i,j),$
    ;  vxi(i,j),vyi(i,j),pm(i,j),$
    ;  format='(2i3,6e11.2,f6.1)'
    ;stop  
    endif
    
  endfor
  ;stop
  ww=where(abs(vyi) lt 1e3) & if ww(0) ne -1 then vyi(ww)=0.

  ;---remove any negative values of interpolated data
  wh=where(fi lt 0,nwh) & if wh(0) ne -1 then fi(wh) =0.

endif  
;---------end gap_interpol

;---get energy
em=vm*vm*2.85e-16

;---combine measured and interpolated data
xd=fltarr(nv*(np+2))
yd=fltarr(nv*(np+2))
zd=fltarr(nv*(np+2))
zd(*)=fm+fi  ;arrays may be added because vxm eq 0 where vxi ne 0


if pltype(0) eq 11 then begin
  ;--------------------------prepare arrays for en-pa contours------------
  xd(*)=pm  
  yd(where(em ne 0))=em(where(em ne 0))
  
  ;---remove any infinite numbers
  wfinite=where(finite(xd) eq 1 and finite(yd) eq 1)
  if wfinite(0) eq -1 then begin
    err=1
    print,' ' & print,'Bad data...returning' &print,' '
    return
  endif else begin
    xd=xd(wfinite)
    yd=yd(wfinite)
    zd=zd(wfinite)
  endelse
  
  ;---remove all zero values of zd and yd
  whn0=where(zd ne 0 and yd gt 0,nd)
  if whn0(0) ne -1 then begin
    xd=xd(whn0)
    yd=yd(whn0)
    zd=zd(whn0)
  endif else begin
    err=1
    print,' ' & print,'Bad data...rerturning' &print,' '
    return
  endelse

  pmin=0.
  pmax=180.
  emin=0.
  emax=1000.
  ;---plot on log e vs pitch grid
  elog=0
  if elog then begin
    emin=10.
    emin=alog10(emin)
    emax=alog10(emax)
    yd=alog10(yd)
  endif
      
  ;---now ready to grid the data and contour and compute reduced distribution  
  triangulate,xd,yd,tr
  zd(where(zd ne 0))=alog10(zd(where(zd ne 0)))
  nx=65 & ny=65
  
  xgridsize=(pmax-pmin)/(nx-1)
  ygridsize=(emax-emin)/(ny-1)
  zgrd=trigrid(xd,yd,zd,tr,[xgridsize,ygridsize],$
    [pmin,emin,pmax,emax])
  sz=size(zgrd)
  if sz(1) ne nx and sz(2) ne ny then stop,'unexpected zgrd dimensions'

  ;---a temporary fix for cases in which zgrd at edges = 0 
  ;   (which arose from parabolic fitted fi lt 0)
  for j=0,ny-1 do if zgrd(0,j) eq 0 then zgrd(0,j)=zgrd(1,j)
  for j=0,ny-1 do if zgrd(nx-1,j) eq 0 then zgrd(nx-1,j)=zgrd(nx-2,j)

endif else begin
  ;--------------------------prepare arrays for vperp-vpara contours------
  xd(*)=vxm+vxi  
  yd(*)=vym+vyi

  ;---remove any infinite numbers
  wfinite=where(finite(xd) eq 1 and finite(yd) eq 1)
  if wfinite(0) eq -1 then begin
    err=1
    print,' ' & print,'Bad data...returning' &print,' '
    return
  endif else begin
    xd=xd(wfinite)
    yd=yd(wfinite)
    zd=zd(wfinite)
  endelse
  
  ;---remove all zero values of zd
  whn0=where(zd ne 0,nd)
  if whn0(0) ne -1 then begin
    xd=xd(whn0)
    yd=yd(whn0)
    zd=zd(whn0)
  endif else begin
    err=1
    print,' ' & print,'Bad data...rerturning' &print,' '
    return
  endelse

  ;---now ready to grid the data and contour and compute reduced distribution
  vmax=max(vm)
  triangulate,xd*1e-8,yd*1e-8,tr
  zd(where(zd ne 0))=alog10(zd(where(zd ne 0)))
  wsurf=where(pltype eq 3)
  if wsurf(0) ne -1 then begin
    nx=65 & ny=33
  endif else begin
    nx=129 & ny=65
  endelse

  gridsize=vmax*1e-8/(ny-1)
  zgrd=trigrid(xd*1e-8,yd*1e-8,zd,tr,[gridsize,gridsize],$
    [-vmax*1e-8,0,vmax*1e-8,vmax*1e-8])
  sz=size(zgrd)
  if sz(1) ne nx and sz(2) ne ny then stop,'unexpected zgrd dimensions'

  ;---a temporary fix for cases in which zgrd(*,0)=0 
  ;   (which arose from parabolic fitted fi lt 0)
  for i=0,nx-1 do if zgrd(i,0) eq 0 then zgrd(i,0)=zgrd(i,1)

  ;---find parallel and perpendicular cuts
  fpara=fltarr(nx)
  wh=where(zgrd(*,0) ne 0) & if wh(0) ne -1 then fpara(wh)=10.^(zgrd(wh,0))

  z12=zgrd(nx/2-1:nx/2,*) & fperp=fltarr(ny)
  for j=0,ny-1 do begin
    if z12(0,j) eq 0 and z12(1,j) ne 0 then fperp(j)=10^(z12(1,j)) 
    if z12(0,j) ne 0 and z12(1,j) eq 0 then fperp(j)=10^(z12(0,j)) 
    if z12(0,j) ne 0 and z12(1,j) ne 0 then fperp(j)=10^((z12(0,j)+z12(1,j))/2)
  endfor
  
  fperp=[reverse(fperp(1:ny-1)),fperp]

  ;---compute reduced f = F
  vygrid=indgen(ny)*vmax/(ny-1)
  costh=-1.0+findgen(nx)/(nx/2)
  sinth=sqrt(1.0-costh^2)
  F=fltarr(nx)
  Fvprp=fltarr(nx,ny)
  for i=1,nx-2 do begin
    wmax=min(abs(vygrid-vmax*sinth(i)),indx)
    nperp=indx+1
    vperp=indgen(nperp)*vygrid(nperp-1)/(nperp-1)
    a=0.05*vperp(nperp-1)
    wh=where(zgrd(i,0:nperp-1) ne 0, nwh)
    if nwh gt 3 then begin
      fv=2*!pi*10^(zgrd(i,wh))*vperp(wh)
      black,a,vperp(wh),fv,nwh,xout,yout,nout
      trap,xout,yout,nout,res,fer1,fer2
      F(i)=res
      Fvprp(i,wh)=fv
    endif
  endfor
endelse

end

