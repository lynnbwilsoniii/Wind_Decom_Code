;-------------------- fsv_grid -----------------------------------------------
pro fsv_grid,pltype=pltype
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common fsvshare,nvs,np,strlndx,enrgstp
common swestuff,swest


vm=sqrt(vxm^2+vym^2)

;interpolate in data gaps along 0, 180 degrees using parabolic fit,
;     f=intrcpt+slope*pitch^2
  vxi=fltarr(nvs,np+2)
  vyi=fltarr(nvs,np+2)
  fi=fltarr(nvs,np+2)

if swest.gap_interpol then begin
  for i=0,nvs-1 do begin
  ;if i eq strlndx then goto,endloop

    wh=where(fm(i,*) ne 0,nwh)
    if nwh gt 1 then begin
    k0=wh(0)
    k1=wh(1)
    intrcpt=(fm(i,k1)*pm(i,k0)^2 -fm(i,k0)*pm(i,k1)^2)/(pm(i,k0)^2-pm(i,k1)^2)
    slope=(fm(i,k0)-fm(i,k1))/(pm(i,k0)^2-pm(i,k1)^2)
    fi(i,0:k0-1)=intrcpt + slope*(pm(i,0:k0-1)-0.)^2  
    vxi(i,0:k0-1)=(vm(i,k0)+vm(i,k1))*0.5*cos(pm(i,0:k0-1)*!dtor)
    vyi(i,0:k0-1)=(vm(i,k0)+vm(i,k1))*0.5*sin(pm(i,0:k0-1)*!dtor)

    k0=wh(nwh-1)
    k1=wh(nwh-2)
    intrcpt=(fm(i,k1)*(pm(i,k0)-180.)^2-fm(i,k0)*(pm(i,k1)-180.)^2)/$
      ((pm(i,k0)-180.)^2-(pm(i,k1)-180.)^2)
    slope=(fm(i,k0)-fm(i,k1))/((pm(i,k0)-180.)^2-(pm(i,k1)-180.)^2)
    fi(i,k0+1:np+1)=intrcpt+slope*(pm(i,k0+1:np+1)-180.)^2
    vxi(i,k0+1:np+1)=(vm(i,k0)+vm(i,k1))*0.5*$
      cos(pm(i,k0+1:np+1)*!dtor )
    vyi(i,k0+1:np+1)=(vm(i,k0)+vm(i,k1))*0.5*$
      sin(pm(i,k0+1:np+1)*!dtor )
    endif

    endloop:
  endfor
  ww=where(abs(vyi) lt 1e3) & if ww(0) ne -1 then vyi(ww)=0.

;remove any negative values of interpolated data
  wh=where(fi lt 0,nwh) & if wh(0) ne -1 then fi(wh) =0.

endif  ;end gap_interpol


;combine measured and interpolated data
  xd=fltarr(nvs*(np+2))
  yd=fltarr(nvs*(np+2))
  zd=fltarr(nvs*(np+2))
  xd(*)=vxm+vxi  ;arrays may be added because vxm eq 0 where vxi ne 0
  yd(*)=vym+vyi
  zd(*)=fm+fi

;remove all zero values of zd
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


;now ready to grid the data and contour and compute reduced distribution
  vmax=max(vm)
  ;print,'xd',xd
  ;print,'yd',yd
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

;a temporary fix for cases in which zgrd(*,0)=0 (which arose from parabolic fitted fi lt 0)
  for i=0,nx-1 do if zgrd(i,0) eq 0 then zgrd(i,0)=zgrd(i,1)

;find parallel and perpendicular cuts
  fpara=fltarr(nx)
  wh=where(zgrd(*,0) ne 0) & if wh(0) ne -1 then fpara(wh)=10.^(zgrd(wh,0))

  z12=zgrd(nx/2-1:nx/2,*) & fperp=fltarr(ny)
  for j=0,ny-1 do begin
    if z12(0,j) eq 0 and z12(1,j) ne 0 then fperp(j)=10^(z12(1,j)) 
    if z12(0,j) ne 0 and z12(1,j) eq 0 then fperp(j)=10^(z12(0,j)) 
    if z12(0,j) ne 0 and z12(1,j) ne 0 then fperp(j)=10^((z12(0,j)+z12(1,j))/2)
  endfor
  fperp=[reverse(fperp),fperp]

;compute reduced f = F
  vygrid=indgen(ny)*vmax/(ny-1)
  costh=-1.0+findgen(nx)/(nx/2)
  sinth=sqrt(1.0-costh^2)
  F=fltarr(nx)
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
    endif

  endfor
    
    

end



;---------------- fintt -------------------------------------------------------

function fintt,vx,vy,vz,bn

arg=bn(0) + bn(1)*vx   + bn(2)*vy   + bn(3)*vz   +$
            bn(4)*vx^2 + bn(5)*vy^2 + bn(6)*vz^2 +$
            bn(7)*vx*vy + bn(8)*vx*vz + bn(9)*vy*vz
return, exp(50.*arg)
end


;================= fsv_patch ================================================


pro fsv_patch,fblokstrl,velstrl,enstrl,$
  fblok,v,en,wsteps,vpot,spcpot,mag,u,timpb5,$
  specie_selct,wid=wid,pltype=pltype,$
  npatch=npatch,bnfit=bnfit,nvmin=nvmin,nvmax=nvmax,noprnt=noprnt,err=err

common sharewidg,wa

common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common fsvshare,nvs,np,strlndx,enrgstp
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common wstuff,wst
common swestuff,swest

;NOTE!!! mode 2 energy sweep is not monotonic but triangular and therefore
;certain array minipulations will requre use of ascending energy step sorting
vsort=sort(v)

theveis=vsmjf.theveis
phiveis=vsmjf.phiveis

yr=0 & dda=0 & hr=0 & min=0 & sec=0
if keyword_set(noprnt) eq 0 then noprnt=0
if keyword_set(npatch) eq 0 then npatch=0
if keyword_set(nvmin) eq 0 then nvmin=0
if keyword_set(nvmax) eq 0 then nvmax=vsmjf.n_vesteps
nvmax = nvmax < vsmjf.n_vesteps
specie=['elecs','ions']

;vsmjf.n_vesteps - nvmax = number of steps to cut from top end of velocity range
;nvmin = number of steps to cut from bottom end


print,'mag : ,mag
themag=asin(mag(2)/sqrt(total(mag*mag)))/!dtor
if mag(0) ne 0 then phimag=atan(mag(1),mag(0))/!dtor else phimag=90.
if phimag lt 0 then phimag =phimag+360.
print,'mag: themag, phimag ',themag, phimag

;measured electron velocities corrected for s/c potntial
;(NOTE: v_vpot is in order of ascending energy step) 
  v_vpot=sqrt(v(vsort(nvmin:nvmax-1))^2 - vpot^2)

;patch speeds
if npatch gt 0 then vpatch=(findgen(npatch)/npatch)*v_vpot(0)   

if noprnt ne 1 then begin
;print,energy and velocity steps
   print,' '
   print,'fparr_patch: v-pot, e-pot(ev) ',vpot, vpot*vpot*2.85e-16
   print,'fparr_patch: '
   print,'index',' vpatch, e-vpatch (ev) ',format='(a5,33x,a25)'
     for j=0,npatch-1 do print,j,vpatch(j),vpatch(j)*vpatch(j)*2.85e-16,$
       format='(15x,i3,36x,e12.5,f10.3)'
   print,' '
   print,$
    'fparr_patch: ',specie_selct,' ',specie(specie_selct)
   print,'index, step,  volt level, vel(meas), en(meas ev),  ',$
    '  vel(plas), en(plas ev)'

     for i=nvmin,nvmax-1 do $
     print,npatch+i-nvmin,i,vsort(i),wsteps(vsort(i)),v(vsort(i)),$
        volt_en(wsteps(vsort(i)),/en,ion=specie_selct),v_vpot(i-nvmin),$
        volt_en(wsteps(vsort(i)),/en,ion=specie_selct)-vpot*vpot*2.85e-16,$
        format='(5x,i3,2x,i3,2x,i3,2x,i3,2x,e12.5,f10.3,7x,e12.5,f10.3)'
endif

;set indices
  ndet=vsmjf.n_vdets 
  nsect=vsmjf.n_sectors 
  nsteps=npatch + nvmax - nvmin

  if npatch gt 0 then fepatch=fltarr(ndet,npatch,nsect)

;patch + measured energy steps  
  enrgstp=fltarr(nsteps)  

;w=particle velocity in frame of the bulk plasma flow
;NOTE: wx,y,z are each ordered in ascending energy step

;find cartesion components
  wx=fltarr(ndet,nsteps,nsect)
  wy=fltarr(ndet,nsteps,nsect)
  wz=fltarr(ndet,nsteps,nsect)

  sn=sin(theveis*!dtor)  ;for i=0,ndet-1
  cn=cos(theveis*!dtor)  ;for i=0,ndet-1
  
  y0=8.25787 & a0=0.238804
  if npatch gt 0 then for j=0,npatch-1 do begin
    if vpatch(j) eq 0 then $
      fepatch(*,j,*)=fltarr(ndet,nsect)+fintt(0.,0.,0.,bnfit) $
    else begin
    for i=0,ndet-1 do for k=0,nsect-1 do begin
      phipatch=phiveis(i,nvmin,k)+(alog10(vpatch(j))-y0)/a0
      wx(i,j,k)=$
        -sin(theveis(i)*!dtor)*cos(phipatch*!dtor)*vpatch(j)-u(0)
      wy(i,j,k)=$
        -sin(theveis(i)*!dtor)*sin(phipatch*!dtor)*vpatch(j)-u(1)
      wz(i,j,k)=-cos(theveis(i)*!dtor)*vpatch(j)-u(2)

      ;get fitted f
         fepatch(i,j,k)=$
           fintt(wx(i,j,k)*1e-8,wy(i,j,k)*1e-8,wz(i,j,k)*1e-8,bnfit)
      ;print,'j,vpatch(j),i,k,fepatch(i,j,k) ',j,vpatch(j),i,k,fepatch(i,j,k)
    endfor
    endelse
  endfor

;get atfile record number from 10 minute index
  atindx=fix((timpb5(2))/600000)  
 
  if atfile ne '' then begin
    if compare_strings('wst.lzdate','wst.atdate',wst.lzdate,wst.atdate) eq 0 $
    then return
  endif $
  else print,'no attitude data; using 180 deg rotation about x-axis instead'
  
  for j=npatch,nsteps-1 do begin
    for i=0,ndet-1 do for k=0,nsect-1 do begin

      ;initially, put wx, wy, wz in PAYLOAD coords in SPACECRAFT REST FRAME
      wx(i,j,k)=vsmjf.vunit(i,vsort(j+nvmin-npatch),k,0)*$
                 v_vpot(j-npatch)
      wy(i,j,k)=vsmjf.vunit(i,vsort(j+nvmin-npatch),k,1)*$
                 v_vpot(j-npatch)
      wz(i,j,k)=vsmjf.vunit(i,vsort(j+nvmin-npatch),k,2)*v_vpot(j-npatch)

      ;transform from payload to gse
      wc_gse=dblarr(3)
      if atfile ne '' then payload_to_gse,[wx(i,j,k),wy(i,j,k),wz(i,j,k)],$
        [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
      else begin
         ;(approx transform from payload to gse: SWE spin axis along -zgse)
         wc_gse(0)=wx(i,j,k)
         wc_gse(1)=-wy(i,j,k)
         wc_gse(2)=-wz(i,j,k) 
      endelse   

      ;print,'payload',k,j,i,[wx(i,j,k),wy(i,j,k),wz(i,j,k)]/v_vpot(j-npatch),$
        ;format='(a8,3i3,3e13.6)'
      ;print,'gse    ',k,j,i,wc_gse/v_vpot(j-npatch),format='(a8,3i3,3e13.6)'

      ;put wx, wy, wz into GSE coords in ELECTRON PLASMA REST FRAME  
       wx(i,j,k)=wc_gse(0) - u(0)
       wy(i,j,k)=wc_gse(1) - u(1)
       wz(i,j,k)=wc_gse(2) - u(2)
     enrgstp(j)=volt_en(vsmjf.veistep(vsort(j+nvmin-npatch)),/en)
     endfor
  endfor
  w=sqrt(wx^2+wy^2+wz^2)


;find polar components (speed, pitch angle) and (parallel, perpendicular)

nv=nsteps

;arrange data arrays according to energy step and pitch angle, nv by ndet*nsect
;data arrays are velocity vectors and f's
;glint points are negative f's and are excluded in the test for f gt 0 
  nds=ndet*nsect
  vpar=fltarr(nv,nds)
  vper=fltarr(nv,nds)
  fp=fltarr(nv,nds)
  pa=fltarr(nv,nds)
  if npatch gt 0 then for j=1,npatch-1 do begin
    ids=-1
    for k=0,nsect-1 do for i=0,ndet-1 do begin
      ids=ids+1
      vpar(j,ids)=$
       (wx(i,j,k)*mag(0)+wy(i,j,k)*mag(1)+wz(i,j,k)*mag(2))/sqrt(total(mag*mag))
      vper(j,ids)=sqrt(w(i,j,k)^2-vpar(j,ids)^2)
      fp(j,ids)=fepatch(i,j,k)
      pa(j,ids)=acos(vpar(j,ids)/w(i,j,k))/!dtor
     ;print,'npatch,j,i,k,fp(j,ids),pa(j,ids)',npatch,j,i,k,fp(j,ids),pa(j,ids) 
    endfor
  endfor

  for j=npatch,nv-1 do begin
    ids=-1
    for k=0,nsect-1 do for i=0,ndet-1 do begin
      ids=ids+1
      vpar(j,ids)=$
       (wx(i,j,k)*mag(0)+wy(i,j,k)*mag(1)+wz(i,j,k)*mag(2))/sqrt(total(mag*mag))
      vper(j,ids)=sqrt(w(i,j,k)^2-vpar(j,ids)^2)
      fp(j,ids)=fblok(i,vsort(j+nvmin-npatch),k) 
      pa(j,ids)=acos(vpar(j,ids)/w(i,j,k))/!dtor
     ;print,'npatch,j,i,k,fp(j,ids),pa(j,ids)',npatch,j,i,k,fp(j,ids),pa(j,ids) 
    endfor
  endfor

for i=0,nv-1 do begin ;sort by ascending pitch angle
  psort=sort(pa(i,*))
  pa(i,*)=pa(i,psort)
  vpar(i,*)=vpar(i,psort)
  vper(i,*)=vper(i,psort)
  fp(i,*)=fp(i,psort)
  endfor

;find data samples that are closest to parallel, anti-parallel and perpendicular
f0cut=fltarr(nv) & v0cut=fltarr(nv) & p0cut=fltarr(nv)
f180cut=fltarr(nv) & v180cut=fltarr(nv) 
p180cut=fltarr(nv) & f90cut=fltarr(nv) & v90cut=fltarr(nv)
for i=npatch,nv-1 do begin
  wh=where(fp(i,*) ge 0,nw)    ;ge 0 test excludes glint points
  if nw ne 0 then begin
    f0cut(i)=fp(i,wh(0)) & v0cut(i)=vpar(i,wh(0))
    p0cut(i)=pa(i,wh(0))
    f180cut(i)=fp(i,wh(nw-1)) & v180cut(i)=vpar(i,wh(nw-1))
    p180cut(i)=pa(i,wh(nw-1))
  endif
  wlt90=where(pa(i,*) le 90.,nwlt)
  wgt90=where(pa(i,*) ge 90.,nwgt) 
  if wlt90(0) ne -1 and wgt90(0) ne -1 then begin
     f90cut(i)=(fp(i,wlt90(nwlt-1))+fp(i,wgt90(0)))/2
     v90cut(i)=(vper(i,wlt90(nwlt-1))+vper(i,wgt90(0)))/2
  endif
endfor

;----------------- strahl------------------------------------------------
;get strahl velocity vectors
  n_strdets=vsmjf.n_strdets
  n_strphis=vsmjf.n_strphis
  wxstrl=fltarr(n_strdets,n_strphis)
  wystrl=fltarr(n_strdets,n_strphis)
  wzstrl=fltarr(n_strdets,n_strphis)
  for i=0,n_strdets-1 do for j=0,n_strphis-1 do begin
    ;initially, put wx, wy, wz in PAYLOAD coords in SPACECRAFT REST FRAME
      wxstrl(i,j)=vsmjf.vunitstrl(i,j,0)*velstrl
      wystrl(i,j)=vsmjf.vunitstrl(i,j,1)*velstrl
      wzstrl(i,j)=vsmjf.vunitstrl(i,j,2)*velstrl
    ;transform from payload to gse
      wcstrl_gse=dblarr(3)
      if atfile ne '' then $
        payload_to_gse,[wxstrl(i,j),wystrl(i,j),wzstrl(i,j)],$
        [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wcstrl_gse  $
      else begin
         ;(approx transform from payload to gse: SWE spin axis along -zgse)
         wcstrl_gse(0)=wxstrl(i,j)
         wcstrl_gse(1)=-wystrl(i,j)
         wcstrl_gse(2)=-wzstrl(i,j) 
      endelse   

     ;put wxstrl, wystrl, wzstrl into GSE coords in ELECTRON PLASMA REST FRAME  
       wxstrl(i,j)=wcstrl_gse(0) - u(0)
       wystrl(i,j)=wcstrl_gse(1) - u(1)
       wzstrl(i,j)=wcstrl_gse(2) - u(2)
  endfor
  wstrl=sqrt(wxstrl^2+wystrl^2+wzstrl^2)

;find polar components
  ndsstrl=n_strdets*n_strphis
  vparstrl=fltarr(ndsstrl)
  vperstrl=fltarr(ndsstrl)
  fpstrl=fltarr(ndsstrl)
  pastrl=fltarr(ndsstrl)
  ids=-1
  for i=0,n_strdets-1 do for j=0,n_strphis-1 do begin
    ids=ids+1
    vparstrl(ids)=$
      (wxstrl(i,j)*mag(0)+wystrl(i,j)*mag(1)+wzstrl(i,j)*mag(2))/$
      sqrt(total(mag*mag))
    vperstrl(ids)=sqrt(wstrl(i,j)^2-vparstrl(ids)^2)
    fpstrl(ids)=fblokstrl(i,j) 
    pastrl(ids)=acos(vparstrl(ids)/wstrl(i,j))/!dtor
  endfor

;sort by ascending pitch angle
  psortstrl=sort(pastrl)
  pastrl=pastrl(psortstrl)
  vparstrl=vparstrl(psortstrl)
  vperstrl=vperstrl(psortstrl)
  fpstrl=fpstrl(psortstrl)
 
;measured and interpolated data will be put in pitch angle bins = dp degrees
  dp=float(swest.pbinselect)
  np=fix(180./dp)
  kp=fix(pa/dp)
  ip=fix(pa/dp)*dp
  wh180=where(ip eq 180,nwh180)
  if(nwh180 ne 0) then ip(wh180)=179

  kpstrl=fix(pastrl/dp)
  ipstrl=fix(pastrl/dp)*dp
  wh180=where(ipstrl eq 180,nwh180)
  if(nwh180 ne 0) then ipstrl(wh180)=179

;determine whether strahl and veis energy steps coincide (+,- 5% in energy),
;and if so, then combine strahl and veis
  wsame=where(enstrl gt 0.95*en and enstrl le 1.05*en)
 

;measured data
  if wsame(0) eq -1 then nvs=nv+1 else nvs=nv
  ;strlndx=energy index which contains either strahl+veis or strahl only data
  if wsame(0) eq -1 then strlndx=nv else strlndx=wsame(0)

  vxm=fltarr(nvs,np+2)
  vym=fltarr(nvs,np+2)
  fm=fltarr(nvs,np+2)
  pm=fltarr(nvs,np+2)
  em=fltarr(nvs,np+2)
  nm=lonarr(nvs,np+2)
  for i=0,nvs-1 do pm(i,indgen(np)+1)=indgen(np)*dp+dp/2
  pm(*,0)=0.
  pm(*,np+1)=180.
  for i=0,nv-1 do begin
    if i ne wsame(0) then begin      ;strahl and veis energies do not coincide
      em(i,*)=w(0,i,0)*w(0,i,0)*2.85e-16 
      for jp=0,np -1 do begin  ;ge 0 test excludes glint points
        wh=where(ip(i,*) eq jp*dp and fp(i,*) ge 0,nwh)     
        if(nwh ne 0) then begin
          vxm(i,jp+1)=total(vpar(i,wh))/nwh
          vym(i,jp+1)=total(vper(i,wh))/nwh
          fm(i,jp+1)=total(fp(i,wh))/nwh
          nm(i,jp+1)=nwh
        endif
      endfor
    endif else begin  ;strahl and veis energies do coincide
      em(i,*)=w(0,i,0)*w(0,i,0)*2.85e-16
      for jp=0,np -1 do begin  ;ge 0 test excludes glint points
        wh=where(ipstrl(*) eq jp*dp and fpstrl(*) ge 0,nwh)     
        if(nwh ne 0) then begin
          vxm(i,jp+1)=total(vparstrl(wh))/nwh
          vym(i,jp+1)=total(vperstrl(wh))/nwh
          fm(i,jp+1)=total(fpstrl(wh))/nwh
          nm(i,jp+1)=nwh
        endif
      endfor
    endelse
  endfor

  if wsame(0) eq -1 then begin  ;adding data for distinct strahl energy step
      enrgstp=[enrgstp,enstrl]    
      em(nvs-1,*)=wstrl(0,0)*wstrl(0,0)*2.85e-16 
      for jp=0,np -1 do begin  ;ge 0 test excludes glint points
        wh=where(ipstrl(*) eq jp*dp and fpstrl(*) ge 0,nwh)     
        if(nwh ne 0) then begin
          vxm(nvs-1,jp+1)=total(vparstrl(wh))/nwh
          vym(nvs-1,jp+1)=total(vperstrl(wh))/nwh
          fm(nvs-1,jp+1)=total(fpstrl(wh))/nwh
          nm(nvs-1,jp+1)=nwh
        endif
      endfor
  endif

end
