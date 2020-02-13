;=========================== proc_fw ==========================================

pro proc_fw,wid=wid,pltype=pltype,nopltf=nopltf,err=err,F_integ=F_integ,$
  statistcs=statistcs

common sharewidg,WDGT
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
;common ftemp,fblok,vel
common shared,d
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common shareredf,sbase,button1,button2,draw1,draw2,field1,field2,$
  ndistribs,xc1,xc2,sgnslop,x1,F1,x2,F2

err=0

if keyword_set(infile) eq 0 then return
if infile eq '' then return
if keyword_set(wid) eq 0 then wid=1
if keyword_set(swest.subtrbkg) eq 0 then swest.subtrbkg='Yes'
if keyword_set(F_integ) eq 0 then F_integ=0
if keyword_set(statistcs) eq 0 then statistcs=0

ntest2=0

;---check to see if an lz record has been read
if wst.lz_is_read then goto,lzisread


;---get recn and spin number
get_recnspin            
                 
if keyword_set(recn) eq 0 then begin
     recn=1 & swest.ispinbl=0
endif

point1:
 
;---process selected lz record
print,'processinf recn ',recn
proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,elec_ion_m1=elec_ion_m1,err=err
print,'err: ',err
if err ne '' then return

;---test lz record for:
;   tm mode,
;   whether in background test mode,
;   current spinbl number against max number spins in current mjf,
;   whether current spinbl contains selected specie,
;   for lzdata quality flag
lztest_code_ret,tmmode_ihk=tmmode_ihk,ntest2,retcode,repcode
if retcode then return         ;return to calling pro
if repcode then goto, point1   ;read next record
       
widget_control,WDGT.swelz_mode,set_value=string(vsmjf.scimode,format='(i1)')

wst.lz_is_read=1

lzisread:    ;an lz record has been read

widget_control,WDGT.swelz_recfld,set_value=recn
spinbl=swest.ispinbl & widget_control,WDGT.swelz_spinfld,set_value=spinbl
if keyword_set(swest.nvmax) eq 0 then swest.nvmax=vsmjf.n_vesteps
swest.nvmax = swest.nvmax < vsmjf.n_vesteps
 
if (vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4) then begin
   if vsmjf.eleion eq 1 then print,'MODE 1 ION MODE!!'
endif

ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps
nsectors=vsmjf.n_sectors
   
;---get and print: tjd and pb5 times and time labels
times_labels
  
widget_control,WDGT.swelz_hmsfld,set_value=wst.lhms

;---get magnetic field
get_3slzmag,wst.pb5,magfld
print,'magfld ', magfld
   
;---get f and counts arrays
fblok=double(vsmjf.fveis_b(*,*,*,spinbl))
icnts_mb=long(vsmjf.cveis_b(*,*,*,spinbl)) > 1l 
       
;---set treatment of glint points flag
if swest.delete eq 0 then fblok=abs(fblok)

;---get veis energy levels and speeds
get_ensteps,wsteps,velocity,energy

;---transform unit vectors from spacecraft to gse coords
atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index
xfrm_unitvectors_sc_gse,atindx,vunit_gse,rgse
       
;---for mode2, put arrays in ascending velocity order
if vsmjf.scimode eq 2 then begin
  sortv=sort(velocity)
  velocity=velocity(sortv)
  energy=energy(sortv)
  vunit=dblarr(ndets,nvsteps,nsectors,3)
  vunit(*,*,*,*)=vunit_gse(*,sortv,*,*)
  icnts_mb(*,*,*)=icnts_mb(*,sortv,*)
  fblok(*,*,*)=fblok(*,sortv,*)
endif $
else vunit=vunit_gse

;---get spacecraft potential (from moments file if available)
get_scpot,vpot

;---glint points (lt 0) and  0's are set to 1/2 count level only for lzpatch
fle0_to_half_count,fblok,fblok_gt0,ndets,nsectors,spinbl

;---compute patch coefficients, bnfit, for given spacecraft potential;
;   (det, vstep, sector to be used in computing the core patch defined by '1'
;   elements of swest.patch_include(0:ndets-1,nvmin:lchan-1,0:nsectors-1));
;   nvmin = first energy step above potential;
lchan=swest.lchan    ;low energy steps (6) to be used in fitting patch
lzpatch,fblok_gt0,icnts_mb,vunit,velocity,vpot,ndets,nvsteps,nsectors,$
    lchan,bnfit,chisqr,swest.patch_include,nvmin=nvmin,err=err

;---TESTING FOR NON-MONOTONIC HYPERSURFACE CURVATURE (after A. Vinas)
jtest=nvmin
for idet=0,ndets-1 do  for ksect=0,nsectors-1 do begin
  testcrv=bnfit(4)*vunit(idet,jtest,ksect,0)^2 +$
          bnfit(5)*vunit(idet,jtest,ksect,1)^2 +$
          bnfit(6)*vunit(idet,jtest,ksect,2)^2 +$
          bnfit(7)*vunit(idet,jtest,ksect,0)*vunit(idet,jtest,ksect,1)+$
          bnfit(8)*vunit(idet,jtest,ksect,0)*vunit(idet,jtest,ksect,2)+$
          bnfit(9)*vunit(idet,jtest,ksect,1)*vunit(idet,jtest,ksect,2)
  if testcrv ge 0. then stop
endfor
    
;---get core fit flow velocity (also density and temperature)
fcore,bnfit,ne_core,te_core,ue_core
;if not finite(ne_core) or te_core lt 0 then stop
ue=ue_core 

;print,'ne_core,te_core,ue_core ',ne_core,te_core,ue_core
   
;---patch interpolated vel range, 0 to first step above scpot, velocity(nvmin)
if sqrt(total(ue*ue))*1e-5 gt 1200.0 then begin
  ue=dblarr(3)
  npatch=0
endif else npatch=2

;---initialize final velocity and phase density arrays
wx=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
wy=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
wz=dblarr(ndets,npatch+nvsteps-nvmin,nsectors)
fe=fltarr(ndets,npatch+nvsteps-nvmin,nsectors)

if npatch gt 0 then begin
  patch_vel0,bnfit,ndets,npatch,nsectors,velocity,nvmin,vpot,$
    vpatch,wxpatch,wypatch,wzpatch,fpatch 
  
  ;---begin putting patched and measured data into final arrays
  ;   (put velocities in solar wind frame using core velocity from patch)
         
  wx(*,0:npatch-1,*)=wxpatch - ue(0)
  wy(*,0:npatch-1,*)=wypatch - ue(1)
  wz(*,0:npatch-1,*)=wzpatch - ue(2)
  fe(*,0:npatch-1,*)=fpatch
endif 


for idet=0,ndets-1 do  for ksect=0,nsectors-1 do begin 
  for jstep=nvmin,lchan-1 do begin     
    vx=(velocity(jstep)-vpot)*vunit(idet,jstep,ksect,0)
    vy=(velocity(jstep)-vpot)*vunit(idet,jstep,ksect,1)
    vz=(velocity(jstep)-vpot)*vunit(idet,jstep,ksect,2)
    
    ;---put patched f values into the '0' elements of swest.patch_include  
    if swest.patch_include(idet,jstep,ksect) eq 0 and npatch gt 0 then $
    fe(idet,npatch+jstep-nvmin,ksect)=fintt(vx*1e-8,vy*1e-8,vz*1e-8,bnfit) $
    
    else fe(idet,npatch+jstep-nvmin,ksect)=fblok(idet,jstep,ksect)
    wx(idet,npatch+jstep-nvmin,ksect)=vx - ue(0)
    wy(idet,npatch+jstep-nvmin,ksect)=vy - ue(1) 
    wz(idet,npatch+jstep-nvmin,ksect)=vz - ue(2)   
  endfor
  
  for jstep=lchan,nvsteps-1 do begin
    wx(idet,npatch+jstep-nvmin,ksect)=$
      (velocity(jstep)-vpot)*vunit(idet,jstep,ksect,0) - ue(0)
    wy(idet,npatch+jstep-nvmin,ksect)=$
      (velocity(jstep)-vpot)*vunit(idet,jstep,ksect,1) - ue(1)
    wz(idet,npatch+jstep-nvmin,ksect)=$
      (velocity(jstep)-vpot)*vunit(idet,jstep,ksect,2) - ue(2)
    fe(idet,npatch+jstep-nvmin,ksect)=fblok(idet,jstep,ksect)    
  endfor
endfor

w=sqrt(wx^2+wy^2+wz^2)

;---print,v - vpot vs f (spinaveraged)
print_vf,velocity,vpot,ue,w,fe,npatch,nvmin,ndets,nvsteps,nsectors

;-------arrays can now be prepared for contouring----------------------------

;---vsmjf.n_vesteps - nvmax = number of steps cut from top of velocity range,
;   whereas nvmin = number of steps to cut from bottom end of measured range,
;   i.e., nvmin = number of energy steps below sc potential
nvmax=swest.nvmax

;---truncate arrays above velocity index nvmax-1
if nvmax lt nvsteps then truncate_nvmax,fe,wx,wy,wz,w,nvmax

;---prepare plotting arrays 
fparr,fe,wx,wy,wz,w,npatch,nvmin,velocity,vpot,reform(magfld),$
  pltype=pltype,err=err

;---do the selected type of distribution function (LZ) plot
if keyword_set(nopltf) eq 0 then nopltf=0 else nopltf=1
if nopltf eq 0 then $
  do_lzplot,pltype=pltype,F_integ=F_integ,wid=wid

print,'wst.timsel ',wst.timsel  
  
end


;------------------- end  proc_fw --------------------------------------------
