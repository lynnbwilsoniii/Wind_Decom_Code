pro stepsectmap,eclipse,step_shft,sect_shft,ndets,nsteps,nsects

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m1stuff,hkm1,vsm1,vdatc,sdatc
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common special1,lunar

isp=eclipse

nvs=nsteps*nsects

mshftmap=intarr(ndets)
  
step_shft=intarr(nvs,ndets)
sect_shft=intarr(nvs,ndets)
;phi_true=dblarr(ndets,nsteps,nsects)
;phi_true(*,*,*)=vsmjf.phiveis(*,*,*)+lunar(isp).dphi/!dtor
;vsmjf.phiveis is corrected spin phase
;phiveis is  uncorrected spin phase
   phi_true=vsmjf.phiveis 
   
wg=where(phi_true gt 360.d)
if wg(0) ne -1 then phi_true(wg)=phi_true(wg) -360.d

for idet=0,ndets-1 do begin

  ;find the actual spin phase angle corresponding to 
  ;    each apparent spin phase angle
    
  ;find the actual step-sector at the apparent start of the spin, i.e.,
  ;  k=isect*nsteps+istep such that 
  ;  phi_true(*,istep,isect)=vsmjf.phiveis(*,0,0)
  ;ph1=min(abs(phi_true(idet,*,*)-vsmjf.phiveis(idet,0,0)),k1)
  ph1=min(abs(phi_true(idet,*,*)-phiveis(idet,0,0)),k1)

  ;number of steps in spin that apparent start of spin is actually shifted
  mshftmap(idet)=k1

  ;************** fine tuning mshftmap *************************************
  ; the following change to mshftmap(idet) has also been made to the file
  ;mshftmap(idet) = mshftmap(idet) - 3

  if mshftmap(idet) ge nvs then mshftmap(idet) = mshftmap(idet) - nvs
  if mshftmap(idet) lt 0 then mshftmap(idet) = mshftmap(idet) + nvs

  ;step-sector corresponding to actual start of spin  
  isect1=fix(k1/nsteps)
  istep1=k1-isect1*nsteps

  ;find the actual step-sector from begin to end of apparent spin using
  ;   the mapping, mshftmap(idet)
    
  step=indgen(nvs)-nsteps*fix(indgen(nvs)/nsteps)
  sect=fix(indgen(nvs)/nsteps)
  mshft=indgen(nvs) + mshftmap(idet)
  wnvs=where(mshft ge nvs)
  if wnvs(0) ne -1 then mshft(wnvs)=mshft(wnvs)-nvs
  step_shft(*,idet)=mshft-fix(mshft/nsteps)*nsteps
  sect_shft(*,idet)=fix(mshft/nsteps)

  prt=0
  if prt eq 1 then begin
   print,' '
   print,'isp idet mshftmap i mshft step sect phi phi_true step_shft sect_shft' 
    for i=0,nvs-1 do begin
      print,isp,idet,mshftmap(idet),$
        i,mshft(i),step(i),sect(i),$
        phiveis(idet,step(i),sect(i)),$
        phi_true(idet,step(i),sect(i)),$
        step_shft(i,idet),sect_shft(i,idet),$
        format='(i3,i2,i3,i4,i4,7x,i3,i2,2f7.1,7x,i3,i2)   
  endfor
  endif

endfor   ;end det loop
   

end


