pro get_f_v_patch,ispin,vpot,vel,v_vpot,bncoef,fegaussfit,vgaussfit,$
  fblk,npatch,nvmin,nvsteps,wx,wy,wz,w,fe,swf=swf

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest
common wstuff,wst
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common onecount,f1ct,v1ct
common ionkplz,pb5i,ni,ti,ui,useionkp

;---This procedure computes the gaussian fit to patch and forms the 
;---phase density and velocity arrays used in the countour plots (proc_fw.pro)
;---and in the individual detector, sector plots (veis_steps_patchfit.pro).

;---Selected detectors, steps, and sectors are included or omitted from patch
;---according to the elements in array swest.patch_include.

;---fe vw wx,wy,wz for all sectors in the given spin are returned, even if, 
;---as in the case of veis_steps_patchfit.pro, only one sector is to be plotted.
;---This is because all sectors in the spin are required to compute patch.

;---if swf eq 1 then return velocities in frame of reference moving with
;   solar wind velocity 
;   (as derived either from patch fit core velocity of ionkp bulk velocity)
;   else if swf eq 0 then return velocities in s/c frame
if keyword_set(swf) eq 0 then swf=0
;swf=0

ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps 
nsectors=vsmjf.n_sectors
      
isector=0
;---determine specie, elecs or ions, and voltage steps for given sector=isector
;--NOTE:This version works only if specie,wsteps are the same each sector
if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
  specie_selct=vsmjf.eleion(isector,ispin) 
  wsteps=vsmjf.veistep(*,isector,ispin)
  if total(vsmjf.eleion(*,ispin))/n_elements(vsmjf.eleion(*,ispin)) ne $
    specie_selct then begin
                        print,'Different species each sector'
                        return
                      endif
  for j=0,n_elements(wsteps)-1 do begin
    if total(vsmjf.veistep(j,*,ispin))/n_elements(vsmjf.veistep(j,*,ispin)) $
      ne wsteps(j) then begin
                          print,'Different vel steps each sector'
                          return
                        endif 
  endfor                            
endif else if $
  vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then begin 
  specie_selct=vsmjf.eleion
  wsteps=vsmjf.veistep 
endif else if vsmjf.scimode eq 6 then begin
  specie_selct=vsmjf.eleion(ispin)
  wsteps=vsmjf.veistep(*,ispin)
endif

;--specie and energy steps will be the same for each sector in spin

;---sort voltage steps in order of increasing step; form velocity sorted arrays
sortv=sort(wsteps)
vel=volt_en(wsteps(sortv),/vel,ion=specie_selct)
ensteps=volt_en(wsteps(sortv),/en,ion=specie_selct)
   
;---sc potential  
vpot=swest.vpot  
enpot=2.85e-16*vpot*vpot

;---measured electron velocities corrected for s/c potntial
;---(NOTE: v_vpot is in order of ascending energy step)
v_vpot=sqrt(vel^2 - vpot^2)

;---mode-dependent phiveis and one-count phase density level
theveis=vsmjf.theveis
if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
  phiveis=vsmjf.phiveis(*,sortv,*)
  f1ct=reform(vsmjf.cts_factor(0,sortv,0,ispin)) 
endif else if $
  vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then begin
    phiveis=vsmjf.phiveis(*,sortv,*)
    f1ct=reform(vsmjf.cts_factor(0,*)) 
endif else if vsmjf.scimode eq 6 then begin
  phiveis=vsmjf.phiveis(*,sortv,*,ispin)
  f1ct=vsmjf.cts_factor(0,*,ispin) 
endif
v1ct=v_vpot
       
;---get unit vectors (all detectors, vel steps, sectors) in gse coords
get_unitvectors_gse,vunit_gse
vunit=vunit_gse(*,sortv,*,*)

;---get f and counts arrays
fblk=double(vsmjf.fveis_b(*,sortv,*,ispin))
icnts_mb=long(vsmjf.cveis_b(*,sortv,*,ispin)) > 1l 

;---provide the capability to interactively modify the relative gains for 
;---testing. (it is assumed that there has been no prior background subtraction.
;---this will be true for dates after 19971026.)
for i=0,vsmjf.n_vdets-1 do for k=0,vsmjf.n_sectors-1 do $
    fblk(i,*,k)=fblk(i,*,k)*(swest.relgain(i)/vsmjf.relgain(i))

;---glint points (lt 0) and  0's are set to 1/2 count level only for lzpatch
fle0_to_half_count,fblk,fblk_gt0,ndets,nsectors,ispin
            
;---do idl version patch fit 
lchan=swest.lchan   
npatch=2

    
lzpatch,fblk_gt0,icnts_mb,vunit,vel,vpot,ndets,nvsteps,nsectors,$
       lchan,bn,chisqr,swest.patch_include,err=err,nvmin=nvmin
bncoef=bn

;---TESTING FOR NON-MONOTONIC HYPERSURFACE CURVATURE (after A. Vinas)
jtest=nvmin
for idet=0,ndets-1 do  for ksect=0,nsectors-1 do begin
  testcrv=bncoef(4)*vunit(idet,jtest,ksect,0)^2 +$
          bncoef(5)*vunit(idet,jtest,ksect,1)^2 +$
          bncoef(6)*vunit(idet,jtest,ksect,2)^2 +$
          bncoef(7)*vunit(idet,jtest,ksect,0)*vunit(idet,jtest,ksect,1)+$
          bncoef(8)*vunit(idet,jtest,ksect,0)*vunit(idet,jtest,ksect,2)+$
          bncoef(9)*vunit(idet,jtest,ksect,1)*vunit(idet,jtest,ksect,2)
  if testcrv ge 0. then print,'NON-MONOTONIC HYPERSURFACE CURVATURE'
endfor

ue=fltarr(3)    
;---get core fit flow velocity (also density and temperature)
if swf and keyword_set(pb5i) ne 0 then begin
  ;--find patch fit core velocity
  ;fcore,bncoef,ne_core,te_core,ue_core
  ;ue=ue_core
  
  ;--find ion bulk flow (nearest ion time to electron time)  
  if wst.pb5(1) eq pb5i(1,1) then begin
    mn=min(abs(pb5i(2,*)-wst.pb5(2)),indx)
    ue=ui(*,indx)*1e5
  endif  
endif    

      
;---form patched phase density and speeds 
vpatch=(findgen(npatch)/npatch)*v_vpot(nvmin)

;---initialize patch fit arrays     
ngaussfit=npatch + lchan - nvmin
fegaussfit=fltarr(vsmjf.n_vdets,ngaussfit)
vgaussfit=fltarr(vsmjf.n_vdets,ngaussfit)

;---initialize final velocity and phase density arrays
wx=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
wy=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
wz=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
w=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
fe=fltarr(ndets,npatch+nvsteps-nvmin,nsectors)

atindx=fix((vsmjf.pb5tim_vsbl(2,ispin))/600000)

for isector=0,nsectors-1 do begin
     
y0=8.25787 & a0=0.238804
for i=0,ndets-1 do begin
  for j=0,npatch-1 do begin
    if vpatch(j) eq 0 then begin
      fegaussfit(i,j)=fintt(0.,0.,0.,bncoef) 
      vx=0. & vy=0. & vz=0.
    endif else begin
      phipatch=phiveis(i,0,isector)+(alog10(vpatch(j))-y0)/a0
      vx=-sin(theveis(i)*!dtor)*cos(phipatch*!dtor)*vpatch(j)
      vy=-sin(theveis(i)*!dtor)*sin(phipatch*!dtor)*vpatch(j)
      vz=-cos(theveis(i)*!dtor)*vpatch(j)
      ;--transform from payload to gse
      wc_gse=dblarr(3) 
      if atfile ne '' then payload_to_gse,[vx,vy,vz],$
          [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
      else begin
        ;(approx transform from payload to gse: SWE spin axis along -zgse)
        wc_gse(0)=vx
        wc_gse(1)=-vy
        wc_gse(2)=-vz 
      endelse
      ;--put vx, vy, vz into GSE coords in SPACECRAFT FRAME  
      vx=wc_gse(0)
      vy=wc_gse(1)
      vz=wc_gse(2)
      ;--get fitted surface
      fegaussfit(i,j)=fintt(vx*1e-8,vy*1e-8,vz*1e-8,bncoef)
      vgaussfit(i,j)=sqrt(vx^2+vy^2+vz^2) 
    endelse
    ;---put vsteps 0:npatch-1 into final arrays in FRAME MOVING WITH VELOCITY=ue
      wx(i,j,isector)=vx - ue(0)
      wy(i,j,isector)=vy - ue(1)
      wz(i,j,isector)=vz - ue(2)
      w(i,j,isector)=sqrt(wx(i,j,isector)^2+wy(i,j,isector)^2+wz(i,j,isector)^2)
      fe(i,j,isector)=fegaussfit(i,j) 
  endfor 
  ;stop
       
  for j=nvmin,lchan-1 do begin
    vx=-sin(theveis(i)*!dtor)*cos(phiveis(i,j,isector)*!dtor)*v_vpot(j)
    vy=-sin(theveis(i)*!dtor)*sin(phiveis(i,j,isector)*!dtor)*v_vpot(j)
    vz=-cos(theveis(i)*!dtor)*v_vpot(j)
    ;--transform from payload to gse
    wc_gse=dblarr(3)
    if atfile ne '' then payload_to_gse,[vx,vy,vz],$
      [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
    else begin
      ;(approx transform from payload to gse: SWE spin axis along -zgse)
      wc_gse(0)=vx
      wc_gse(1)=-vy
      wc_gse(2)=-vz 
    endelse
    ;--put vx, vy, vz into GSE coords in SPACECRAFT FRAME  
    vx=wc_gse(0) 
    vy=wc_gse(1)
    vz=wc_gse(2) 
    ;--get fitted surface
    fegaussfit(i,npatch+j-nvmin)=fintt(vx*1e-8,vy*1e-8,vz*1e-8,bncoef)
    vgaussfit(i,npatch+j-nvmin)=sqrt(vx^2+vy^2+vz^2)
      
    ;---for vsteps nvmin:lchan-1, 
    ;---put either patched or actual data into final arrays 
    ;   in FRAME MOVING WITH VELOCITY=ue
      wx(i,npatch+j-nvmin,isector)=vx - ue(0)
      wy(i,npatch+j-nvmin,isector)=vy - ue(1)
      wz(i,npatch+j-nvmin,isector)=vz - ue(2)
      w(i,npatch+j-nvmin,isector)=sqrt(wx(i,npatch+j-nvmin,isector)^2+$
                                       wy(i,npatch+j-nvmin,isector)^2+$
                                       wz(i,npatch+j-nvmin,isector)^2)
      if swest.patch_include(i,j,isector) eq 0 then $
           fe(i,npatch+j-nvmin,isector)=fegaussfit(i,npatch+j-nvmin) $
      else fe(i,npatch+j-nvmin,isector)=fblk(i,j,isector)
       
  endfor
  
  for j=lchan,nvsteps-1 do begin
    vx=-sin(theveis(i)*!dtor)*cos(phiveis(i,j,isector)*!dtor)*v_vpot(j)
    vy=-sin(theveis(i)*!dtor)*sin(phiveis(i,j,isector)*!dtor)*v_vpot(j)
    vz=-cos(theveis(i)*!dtor)*v_vpot(j)
    ;--transform from payload to gse
    wc_gse=dblarr(3)
    if atfile ne '' then payload_to_gse,[vx,vy,vz],$
      [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
    else begin
      ;(approx transform from payload to gse: SWE spin axis along -zgse)
      wc_gse(0)=vx
      wc_gse(1)=-vy
      wc_gse(2)=-vz 
    endelse
    ;--put vx, vy, vz into GSE coords in SPACECRAFT FRAME  
    vx=wc_gse(0) 
    vy=wc_gse(1)
    vz=wc_gse(2)
    
    ;---for vsteps nvmin:lchan-1, put data into final arrays
    ;   in FRAME MOVING WITH VELOCITY=ue
    wx(i,npatch+j-nvmin,isector)=vx - ue(0)
    wy(i,npatch+j-nvmin,isector)=vy - ue(1)
    wz(i,npatch+j-nvmin,isector)=vz - ue(2)
    w(i,npatch+j-nvmin,isector)=sqrt(wx(i,npatch+j-nvmin,isector)^2+$
                                     wy(i,npatch+j-nvmin,isector)^2+$
                                     wz(i,npatch+j-nvmin,isector)^2)
    fe(i,npatch+j-nvmin,isector)=fblk(i,j,isector)
  endfor
           
endfor ;end detector loop
endfor ;end sector loop



end     