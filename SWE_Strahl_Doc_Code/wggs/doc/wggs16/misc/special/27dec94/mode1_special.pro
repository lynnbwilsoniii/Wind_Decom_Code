

;================================= mode1 ==================================


pro mode1_special,lpr=lpr,noglnt=noglnt,err=err

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m1stuff,hkm1,vsm1,vdatc,sdatc
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit
common log_delog,comp_tbl,dcomp_tbl
common backgrnd,avgcts_m1,avgcts_m2,bdate
common sunglint,xveis_m1,xveis_m2,gdate
common special1,lunar
common swestuff,swest

err=''

eclipse=-1   ;this special version used to determine eclipse shift in samples

;if keyword_set(lpr) eq 0 then lpr=0

;if lpr then print,'mode1'


; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)


;get time tagged spincnt
  time_utc,lz.mf(hkm1(1).offs:hkm1(1).offs+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms

;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=40
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5
  n_hkvars=32 & n_hkmode=3 
  n_spins=7 & n_vs=924  & n_fcbl=31 & n_fc=122
  n_vdat=576  & n_sdat=336   
  n_vdets=6 & n_vesteps=16 & n_sectors=6  & n_strdets=12 & n_strphis=28

;set bci indices corrsponding to strahl samples (mode1)
  ibci_strl=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,    $
             52,53,54,55,56,57,58,59,60,61,62,63,64,65 ]

;determine spin phase at each bci (each data sample) 
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  sp.lst_scimod=scimode_ihk

;--------------- make veis/strahl data structure assignments this mjf -------


;define data structure for veis/strahl data samples 
  vsmjf={  $
  descr:'   ',  $
  tjd:0l,  $
  sec:0d,  $
  hour:0l,  $
  min:0l,  $
  isec:0l,  $
  ms:0,  $
  mjfcnt:0l,  $
  scimode:0,$
  spinp:0.d, $
  n_vdets:long(n_vdets),  $
  n_vesteps:long(n_vesteps),  $
  n_sectors:long(n_sectors),  $
  n_spins:long(n_spins),  $
  n_strdets:long(n_strdets),  $
  n_strphis:long(n_strphis),$
  deltasp:double(iclicks_bci)/double(iclicks_sunpulse),$
  deadtim_ele:0.d,  $
  deadtim_ion:0.d,  $
  delt_ele:0.d,  $
  delt_ion:0.d,$
  geomf:fltarr(n_vdets),$
  lzrecn:0l,$
  veis:intarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  cveis:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  cveis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  fveis:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  fveis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins), $
  veistep:bytarr(n_vesteps),$
  eleion_sweep:0,$
  eleion:-1,$
  secveis:dblarr(n_vesteps,n_sectors,n_spins),$
  phiveis:phiveis,  $
  theveis:theveis,$
  vunit:dblarr(n_vdets,n_vesteps,n_sectors,3),  $
  strl:bytarr(n_strdets,n_strphis,n_spins),  $
  strlstep:bytarr(n_spins),$
  secstrl:dblarr(n_strphis,n_spins),$
  phistrl:phistrl,  $
  thestrl:thestrl,$
  suntim_vsbl:dblarr(n_spins),$
  pb5tim_vsbl:lonarr(3,n_spins),$
  vqlty:lonarr(n_spins),  $
  sqlty:lonarr(n_spins),$
  xveis:1+intarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  relgain:fltarr(n_vdets),  $
  bdate:'',  $
  bcts: fltarr(n_vdets,n_vesteps,n_sectors,n_spins),  $
  relgain_backg:fltarr(n_vdets),  $
  cts_factor:fltarr(n_vdets, n_vesteps),  $
  background_test:0   }


;make structure assignments 

  vsmjf.descr='veis-strahl data samples'
  vsmjf.tjd=tjd      ;trucated julian day of time-tagged spin this mjf
  vsmjf.sec=sec      ;seconds of day of time-tagged spin this mjf
  vsmjf.hour=hour    ;hour of day of time-tagged spin this mjf
  vsmjf.min=min      ;min of hour of time-tagged spin this mjf
  vsmjf.isec=isec    ;sec of min of time-tagged spin this mjf
  vsmjf.ms=ms        ;ms of sec of time-tagged spin this mjf
  vsmjf.mjfcnt=lz.mf(ihk(1).offs)  ;mjf counter
  vsmjf.scimode=scimode_ihk        ;science mode
  vsmjf.spinp=sp.spinp   ;spin period
  vsmjf.lzrecn=lz.recn

;put unitvectors (payload coords) into vsmjf.unit 
  vsmjf.vunit=vunit
  ;unitvector


;---------- determine time at each bci (each data sample) for mode 1 --------

;sun time (suntim_vsbl(j) at spincnt_vsbl(j) ) for veis/strahl j'th spin block,
;i.e., seconds from tjd epoch for j'th spin when sc x-axis crosses sun line 
  spincnt_vsbl=intarr(n_spins)
  spincnt_vsbl(0:n_spins-1)=fix(lz.mf((vsm1(0).offs(0:n_spins-1))))
  ispin_diff=intarr(n_spins)
  ispin_diff=spincnt_vsbl-spincnt
  w=where(ispin_diff lt -33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)+256
  w=where(ispin_diff gt  33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)-256
  sunsec_vsbl=dblarr(n_spins) 
  sunsec_vsbl=sec+ispin_diff*sp.spinp
  suntim_vsbl=dblarr(n_spins)
  suntim_vsbl=tjd*86400.d0+sunsec_vsbl
  vsmjf.suntim_vsbl=suntim_vsbl

;convert tjd,sec to pb5 time
  for ispin=0,vsmjf.n_spins-1 do begin
    vsmjf.pb5tim_vsbl(*,ispin)=sec_pb5(vsmjf.suntim_vsbl(ispin),err=err) 
    if err ne '' then return
  endfor

;time between bci's assuming constant spin period over mjf
  sec_bci=spin_bci*sp.spinp
 
;time of each bci relative to true sun pulse, suntim_vsbl(j), for each spin j
  time=indgen(nbcis_spin)*sec_bci + (phidly_sun/phi_bci)*sec_bci + sec_bci 

;bci indices (skipping 1 bci each sweep)
  ibci=intarr(n_vesteps,n_sectors)
  ibci(*,*)=indgen(n_vesteps*n_sectors)/n_vesteps+1+indgen(n_vesteps*n_sectors)
 
;time of each veis and strahl data sample
  vsmjf.secveis(*,*,*)=$
    time(ibci(0:n_vesteps*n_sectors-1))#replicate(1,n_spins)+$
    replicate(1,n_vesteps*n_sectors) # suntim_vsbl(0:n_spins-1)

  vsmjf.secstrl(*,*)=$
    time(ibci_strl(0:n_strphis-1))#replicate(1,n_spins)+$
    replicate(1,n_strphis)#suntim_vsbl(0:n_spins-1)

  ;the above matrix operations are equivalent to the following nested loops:
  ;for ispin=0,n_spins-1 do begin
    ;time of each veis data sample in seconds from tjd epoch
  ;  ibci=-1
  ;  for jsect=0,n_sectors-1 do begin
  ;    ibci=ibci+1                        ;skipping 1 bci each sweep
  ;    for ivestep=0,n_vesteps-1 do begin
  ;      ibci=ibci+1
  ;      vsmjf.secveis(ivestep,jsect,ispin)=suntim_vsbl(ispin)+time(ibci)
  ;    endfor
  ;  endfor
    ;time of each strahl data sample in seconds from tjd epoch
    ;for istrphi=0,n_strphis-1 do vsmjf.secstrl(istrphi,ispin)= $
    ;  suntim_vsbl(ispin)+time(ibci_strl(istrphi))
  ;endfor

;get counts to phase density instrument factors
;WIND launch : tjd=9657
;after tjd=9976 & sec=41345.485  1995 259 (sep 16), mode1 deadtim=4ms, ion&ele
  deadtim_ion=0.004d
  if vsmjf.tjd ge 9657 and $
   (double(vsmjf.tjd)+vsmjf.sec/86400.d) le (double(9976)+41345.485d/86400.d) $
  then deadtim_ele=0.000800d else deadtim_ele=0.004d

;accumulation time (seconds)
  vsmjf.delt_ele=sp.spinp*vsmjf.deltasp - deadtim_ele 
  vsmjf.delt_ion=sp.spinp*vsmjf.deltasp - deadtim_ion 

;geometrical factor (dv/v * eff area * solid angle)
  vsmjf.geomf=fltarr(n_vdets) + 2.9e-5   ;new from J. Keller 
  

;veis and strahl data (log compressed)
  vsmjf.veis(*,*,*,*) = dcomp_tbl(lz.mf((vdatc.ind)))
  vsmjf.strl(*,*,*)   = lz.mf((sdatc.ind))


;strahl steps
  vsmjf.strlstep =lz.mf(vsm1(1).offs)



;veis steps
  vsmjf.veistep=lz.mf(hkm1(24).offs+indgen(n_vesteps))


;test for specie, vsmjf.eleion= 0:electrons or  1:ions) and 
;sweep mode, 
;  vsmjf.eleion_sweep=0: all electrons  1: all ions   4: background test

  elec_ion_m1=get_bits(lz.mf(ihk(6).offs),ihk(6).bv(0).p,ihk(6).bv(0).n)
  welec=where(vsmjf.veistep lt 64)
  wion=where(vsmjf.veistep ge 64 and $
            vsmjf.veistep lt 128)
  if welec(0) ne -1 and elec_ion_m1 eq 0 then begin
    vsmjf.eleion=0                                          ;all electrons
    vsmjf.eleion_sweep=vsmjf.eleion     
  endif else if wion(0) ne -1 and elec_ion_m1 eq 1 then begin
    vsmjf.eleion=1                                          ;all ions
    vsmjf.eleion_sweep=vsmjf.eleion 
  endif else if wion(0) ne -1 and elec_ion_m1 eq 0 then begin
    vsmjf.background_test=1    ;background test: sampling electrons in ion mode
    vsmjf.eleion_sweep=4
  endif


;get relative gains
   getrelgains,relgain,vsmjf.suntim_vsbl(0),relgainchange
   vsmjf.relgain=relgain
    
if noglnt eq 0 then begin
  ;put average background counts into array if bdate exists and
  ;find corresponding relgain
  relgain_backg=1.+fltarr(vsmjf.n_vdets)
  if keyword_set(bdate) eq 0 then bdate=''
  if bdate ne '' then begin
    vsmjf.bdate=bdate
    if eclipse(0) eq -1 then $
      vsmjf.bcts(*,*,*,*)=avgcts_m1(*)#replicate(1,n_spins) $
      else vsmjf.bcts(*,*,*,*)=avgcts_m1_shft(*)#replicate(1,n_spins)
    ;for ispin=0,vsmjf.n_spins-1 do vsmjf.bcts(*,*,*,ispin)=avgcts_m1 
    btjd=pb5_tjd(ymd_pb5(long(bdate)))
    bda=double(btjd(0))*86400.d   
    getrelgains,relgain_backg,bda,rgchange_backg
    vsmjf.relgain_backg=relgain_backg
  endif
endif


;multiply counts and background data by relative gains 
  vsmjf.cveis(*,*,*,*)=$
    (vsmjf.relgain(0:n_vdets-1)#replicate(1,n_vesteps*n_sectors*n_spins))*$
    float(vsmjf.veis(*,*,*,*))
  vsmjf.bcts(*,*,*,*)=$
   (vsmjf.relgain_backg(0:n_vdets-1)#replicate(1,n_vesteps*n_sectors*n_spins))*$
   vsmjf.bcts(*,*,*,*)

  ;equivalent loop operation
  ;for i=0,vsmjf.n_vdets-1 do begin
  ;  vsmjf.cveis(i,*,*,*)=vsmjf.relgain(i)*float(vsmjf.veis(i,*,*,*))
  ;  vsmjf.bcts(i,*,*,*)= vsmjf.relgain_backg(i)*vsmjf.bcts(i,*,*,*) 
  ;endfor

;get theoretical energy dependent detector efficiency
  coeff=[1.4779904, -4.5573397, 5.6175835, -2.8179218, 0.64383398, -0.055918481]
  logen=alog10(volt_en(vsmjf.veistep,/en))
  deff=coeff(0)+coeff(1)*logen+coeff(2)*logen^2+coeff(3)*logen^3+$
    coeff(4)*logen^4+coeff(5)*logen^5
  deff=deff/max(deff)
  for k=0,n_elements(deff)-1 do vsmjf.cveis(*,k,*,*)=$
     vsmjf.cveis(*,k,*,*)/deff(k)

;background removal (see note below)
;  if swest.subtrbkg eq 'Yes' then $
    vsmjf.cveis_b=vsmjf.cveis-vsmjf.bcts > 0.   ;floor is set to 0 counts 

;get instrument factors in converting counts to f's and store it
   cts_f,vsmjf.veistep(0:vsmjf.n_vesteps-1),cts_factor,cf,ion=vsmjf.eleion
   vsmjf.cts_factor=cts_factor

;convert to f's (see note below)
  ;no background removed
  vsmjf.fveis(*,*,*,*)=$
  vsmjf.cveis * reform( vsmjf.cts_factor(*)#replicate(1,n_sectors*n_spins), $
                       n_vdets,n_vesteps,n_sectors,n_spins)

  ;yes background removed 
  vsmjf.fveis_b(*,*,*,*)=$
  vsmjf.cveis_b * reform( vsmjf.cts_factor(*)#replicate(1,n_sectors*n_spins), $
                       n_vdets,n_vesteps,n_sectors,n_spins)

 ;the above matrix operation equivalent to the following nested loop:
  ;for ispin=0,vsmjf.n_spins-1 do $
  ;for k=0,vsmjf.n_sectors-1 do for i=0,vsmjf.n_vdets-1 do $
  ;    vsmjf.fveis(i,*,k,ispin)=vsmjf.cveis(i,*,k,ispin) * vsmjf.cts_factor(i,*)
                
;set counts and f's identified as glint to negative their value (see note below)
  if noglnt eq 0 then begin
    if eclipse(0) eq -1 then $
      vsmjf.xveis(*,*,*,*)=xveis_m1(*)#replicate(1,n_spins) $
      else vsmjf.xveis(*,*,*,*)=xveis_m1_shft(*)#replicate(1,n_spins)
    ;number of glint points
    swest.ndel=n_elements(where(xveis_m1 lt 0))
  endif
  ;for i=0,n_spins-1 do vsmjf.xveis(*,*,*,i)=xveis
  vsmjf.veis=vsmjf.xveis * vsmjf.veis

  vsmjf.cveis=vsmjf.xveis * vsmjf.cveis
  vsmjf.fveis=vsmjf.xveis * vsmjf.fveis

  vsmjf.cveis_b=vsmjf.xveis * vsmjf.cveis_b
  vsmjf.fveis_b=vsmjf.xveis * vsmjf.fveis_b




;=========== IMPORTANT NOTES =================================================

;after background removal, a floor of 0 count is set on vsmjf.cveis
;from which f, vsmjf.fveis, is computed

;============================================================================

;help,vsmjf,/str

end

