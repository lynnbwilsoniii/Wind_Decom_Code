

;================================= mode6 ==================================



pro mode6,lpr=lpr,noglnt=noglnt,err=err,norelgains=norelgains

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common reldetgains,relgain,relgain_backg
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common log_delog,comp_tbl,dcomp_tbl
common backgrnd,avgcts_m1,avgcts_m2,bdate
common sunglint,xveis_m1,xveis_m2,gdate
common sunglint_other,xveis_m1_other,gdate_other
common special1,lunar
common swestuff,swest

err=''

;if keyword_set(lpr) eq 0 then lpr=0

;if lpr then print,'mode6'

;get sun phase angles of detectors, unit vectors
   phasem1     ;veis sun phase angles the same for mode = 1, 4, and 6
  
; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

;get time tagged spincnt
  time_utc,lz.mf(hkm6(1).offs:hkm6(1).offs+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  if tjd eq 0 and lz.yr ne 1995 and lz.dy ne 283 then begin
    err='mode1: time error in time_utc.pro' & return
  endif  
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
  phistrl_bci0=202.4d


;bci indices (skipping 1 bci each sweep)
  ibci=intarr(n_vesteps,n_sectors)
  ibci(*,*)=indgen(n_vesteps*n_sectors)/n_vesteps+1+indgen(n_vesteps*n_sectors)
 

;set bci indices corrsponding to strahl samples (mode6)
  bxyz_status=lz.mf(bxyzdat6.ind(1)) ;0=no b tracking  1=yes b tracking
  bxyz_ind=lz.mf(bxyzdat6.ind(0))  ;bci index at start of strahl sampling
  ibci_strl=[replicate(1,n_strphis/2) # bxyz_ind+               $
           indgen(n_strphis/2) # replicate(1,n_spins),              $
           replicate(1,n_strphis/2) # bxyz_ind+nbcis_spin/2+  $
           indgen(n_strphis/2) # replicate(1,n_spins) ]
  ibci_strl=ibci_strl mod nbcis_spin
  ibci_strl_m1=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,    $
                 52,53,54,55,56,57,58,59,60,61,62,63,64,65 ] 
  wstatus0=where(bxyz_status eq 0,nwstatus0)
  if nwstatus0 gt 0 then  $
    for i=0,nwstatus0-1 do ibci_strl(*,wstatus0(i))=ibci_strl_m1 
    
;spin phase angle of strahl data samples relative to direction of sun,
;  positive counterclockwise about spin axis (z-axis)
;the mode1 "phistrl" and "vunitstrl" from common phasemod1 are overridden 
    phistrl_onespin=(indgen(nbcis_spin)*phi_bci+phistrl_bci0) mod 360.
    phistrl=phistrl_onespin(ibci_strl)       ;spin phase at each bci 
                 
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
  veis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  cveis:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  cveis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  fveis:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  fveis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins), $
  veistep:bytarr(n_vesteps,n_spins),$
  eleion_sweep:intarr(n_spins),$
  eleion:-1+intarr(n_spins),$
  altspin_enabled:intarr(n_spins),$
  primalt_tbl:intarr(n_spins),$
  secveis:dblarr(n_vesteps,n_sectors,n_spins),$
  phiveis:dblarr(n_vdets,n_vesteps,n_sectors,n_spins),  $
  theveis:dblarr(n_vdets),$
  vunit:dblarr(n_vdets,n_vesteps,n_sectors,3,n_spins),  $
  strl:bytarr(n_strdets,n_strphis,n_spins),  $
  fstrl:fltarr(n_strdets,n_strphis,n_spins),  $
  strlstep:bytarr(n_spins),$
  numstrlstps:0,$
  ibci_strl:intarr(n_strphis,n_spins),$
  secstrl:dblarr(n_strphis,n_spins),$
  phistrl:dblarr(n_strphis,n_spins),  $
  thestrl:thestrl,$
  vunitstrl:dblarr(n_strdets,n_strphis,3,n_spins),$
  bxyz_status:bytarr(n_spins),$
  bxyz_ind:bytarr(n_spins),$
  bxyz_range:intarr(n_spins),$
  bxyz_phase:intarr(n_spins),$
  bxyz_bx:intarr(n_spins),$
  bxyz_by:intarr(n_spins),$
  bxyz_bz:intarr(n_spins),$
  sunsec_vsbl:dblarr(n_spins),$
  suntim_vsbl:dblarr(n_spins),$
  pb5tim_vsbl:lonarr(3,n_spins),$
  vqlty:lonarr(n_spins),  $
  sqlty:lonarr(n_spins),$
  xveis:1+intarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  relgain:fltarr(n_vdets),  $
  bdate:'',  $
  bcts: fltarr(n_vdets,n_vesteps,n_sectors,n_spins),  $
  relgain_backg:fltarr(n_vdets),  $
  cts_factor:fltarr(n_vdets, n_vesteps,n_spins),  $
  strl_cts_factor:fltarr(n_strdets, n_spins),$
  background_test:0,$
  set_det2_eq_det3:0   }


;make structure assignments 

  vsmjf.descr='veis-strahl data samples'
  vsmjf.tjd=tjd      ;trucated julian day of time-tagged spin this mjf
  vsmjf.sec=sec      ;seconds of day of time-tagged spin this mjf
  vsmjf.hour=hour    ;hour of day of time-tagged spin this mjf
  vsmjf.min=min      ;min of hour of time-tagged spin this mjf
  vsmjf.isec=isec    ;sec of min of time-tagged spin this mjf
  vsmjf.ms=ms        ;ms of sec of time-tagged spin this mjf
  vsmjf.mjfcnt=lz.mf(ihk(1).offs)  ;mjf counter
  if scimode_ihk eq 6  then vsmjf.scimode=6
  ;vsmjf.scimode=scimode_ihk        ;science mode
  vsmjf.spinp=sp.spinp   ;spin period
  vsmjf.lzrecn=lz.recn
  vsmjf.numstrlstps=lz.mf(hkm6(6).offs)

  vsmjf.bxyz_status=bxyz_status
  vsmjf.bxyz_ind=bxyz_ind
  vsmjf.bxyz_range=get_bits(lz.mf(bxyzdat6.ind(3)),7,3)
  vsmjf.bxyz_phase=long(get_bits(lz.mf(bxyzdat6.ind(3)),3,4))*256l + $
    long(lz.mf(bxyzdat6.ind(2))) ;bxyz phase in sunpulse clicks, 0 to 4095
  vsmjf.bxyz_bx=$
    long(lz.mf(bxyzdat6.ind(5)))*256l+long(lz.mf(bxyzdat6.ind(4)))-2048l
  vsmjf.bxyz_by=$
    long(lz.mf(bxyzdat6.ind(7)))*256l+long(lz.mf(bxyzdat6.ind(6)))-2048l
  vsmjf.bxyz_bz=$
    long(lz.mf(bxyzdat6.ind(9)))*256l+long(lz.mf(bxyzdat6.ind(8)))-2048l

;set glint map    
  if noglnt eq 0 then begin
     if swest.univgmask then $
       vsmjf.xveis(*,*,*,*)=xveis_m1_other(*)#replicate(1,n_spins) else $
       vsmjf.xveis(*,*,*,*)=xveis_m1(*)#replicate(1,n_spins) 
    ;number of glint points
    swest.ndel=n_elements(where(vsmjf.xveis(*,*,*,0) lt 0))
  endif
        
;look angles and velocity unitvectors opposite look directions (payload coords)
;VEIS 
  vsmjf.phiveis=phiveis
  vsmjf.theveis=theveis 
  vsmjf.vunit=vunit

;STRAHL
  vsmjf.ibci_strl=ibci_strl
  vsmjf.phistrl=phistrl
  
  ;-------nosun pulse spinp begin
  ;---There have been three intervals to date in which the sun sensor was 
  ;   saturated by a solar proton event, shutting off the sunpulse. In its place
  ;   a pseudo sunpulse was generated each 2.4s instead of the nominal 3s.
  ;   The program nosunpuls_readlz.pro was used with strahl measurements of the
  ;   Sun to determine the true spinperiod and spin phase of the spacecraft.
  ;   The spin phase data was saved to an IDL-save file which is read by
  ;   procedure nosunpuls_get_spnphx.pro (called by this procedure, mode6.pro)
  ;   and which computes the necessary spin phase information to process strahl 
  ;   and VEIS data. 
  nosun_evnum=3
  tsunpuls_last_real=dblarr(nosun_evnum)
  tsunpuls_next_real=dblarr(nosun_evnum)
  tsunpuls_last_real(0)=double(150291442.584)  ;20000714 11:40  recn=455
  tsunpuls_next_real(0)=double(150487310.801)  ;20000716 18:02  recn=706
  
  tsunpuls_last_real(1)=double(160451370.522)  ;20001109 01:49:31  recn=72
  tsunpuls_next_real(1)=double(160514665.248)  ;20001109 19:24:25  recn=760
  
  tsunpuls_last_real(2)=double(1.9172776e+08)  ;20011106 01:42:41  recn=68
  tsunpuls_next_real(2)=double(1.9179980e+08)  ;20011106 21:43:18  recn=851
  
  tjdsec=tjdms_sec(tjd,long(sec*1000l))
  
  if tjdsec gt tsunpuls_last_real(0) and tjdsec lt tsunpuls_next_real(0) $
  then nosunpuls_get_spnphx,tjd,ibci_strl,'20000714'
    
  if tjdsec gt tsunpuls_last_real(1) and tjdsec lt tsunpuls_next_real(1) $
  then nosunpuls_get_spnphx,tjd,ibci_strl,'20001109'
  
  if tjdsec gt tsunpuls_last_real(2) and tjdsec lt tsunpuls_next_real(2) $
  then nosunpuls_get_spnphx,tjd,ibci_strl,'20011106'
    
;-------nosun pulse spinp end
  
  vunitstrl=dblarr(n_strdets,n_strphis,3,n_spins)
  snth=sin(thestrl*!dtor)
  csth=cos(thestrl*!dtor)
  snph=sin(vsmjf.phistrl*!dtor)
  csph=cos(vsmjf.phistrl*!dtor) 
  for ispin=0,n_spins-1 do begin
    vsmjf.vunitstrl(*,*,0,ispin)=-snth#csph(*,ispin)
    vsmjf.vunitstrl(*,*,1,ispin)=-snth#snph(*,ispin)
    vsmjf.vunitstrl(*,*,2,ispin)=-csth#replicate(1,n_strphis)
  endfor
       
         
;---------- determine time at each bci (each data sample) for mode 1 --------

;sun time (suntim_vsbl(j) at spincnt_vsbl(j) ) for veis/strahl j'th spin block,
;i.e., seconds from tjd epoch for j'th spin when sc x-axis crosses sun line 
  spincnt_vsbl=intarr(n_spins)
  spincnt_vsbl(0:n_spins-1)=fix(lz.mf((vsm6(0:n_spins-1).offs(0))))
  ispin_diff=intarr(n_spins)
  ispin_diff=spincnt_vsbl-spincnt
  w=where(ispin_diff lt -33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)+256
  w=where(ispin_diff gt  33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)-256
  sunsec_vsbl=dblarr(n_spins) 
  sunsec_vsbl=sec+ispin_diff*sp.spinp
  vsmjf.sunsec_vsbl=sunsec_vsbl
  suntim_vsbl=dblarr(n_spins)
  suntim_vsbl=tjd*86400.d0+vsmjf.sunsec_vsbl
  vsmjf.suntim_vsbl=suntim_vsbl

;print,'tjd,sunsec_vsbl,sp.spinp' ,tjd,sunsec_vsbl,sp.spinp
   
;convert tjd,sec to pb5 time
  for ispin=0,vsmjf.n_spins-1 do begin
    vsmjf.pb5tim_vsbl(*,ispin)=sec_pb5(vsmjf.suntim_vsbl(ispin),err=err) 
    if err ne '' then return
  endfor
;stop

;time between bci's assuming constant spin period over mjf
  sec_bci=spin_bci*sp.spinp
 
;time of each bci relative to true sun pulse, suntim_vsbl(j), for each spin j
  time=indgen(nbcis_spin)*sec_bci + (phidly_sun/phi_bci)*sec_bci + sec_bci 

;time of each veis and strahl data sample
  vsmjf.secveis(*,*,*)=$
    time(ibci(0:n_vesteps*n_sectors-1))#replicate(1,n_spins)+$
    replicate(1,n_vesteps*n_sectors) # vsmjf.suntim_vsbl(0:n_spins-1)
    
;time of each strahl data sample in seconds from tjd epoch
  vsmjf.secstrl=replicate(1,n_strphis) #  vsmjf.suntim_vsbl + time(ibci_strl)
          
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
  vsmjf.veis(*,*,*,*) = dcomp_tbl(lz.mf((vdatc6.ind)))
  vsmjf.strl(*,*,*)   = lz.mf((sdatc6.ind))


;strahl steps
  strlstep_tbloffs=get_bits(lz.mf(vsm6.offs(1)),vsm6.bv0(1).p,vsm6.bv0(1).n)
  strahl_hvtbl=lz.mf(hkm6(9).loc)
  vsmjf.strlstep =strahl_hvtbl(strlstep_tbloffs)


;veis steps
;test for specie, vsmjf.eleion= 0:electrons or  1:ions) and 
;sweep mode,vsmjf.eleion_sweep=
;                     0: all electrons  1: all ions   
;                     2: electrons/ions alternating spins 4: background test
  veis_tbl=intarr(n_vesteps,2)
  veis_tbl(*,0)=lz.mf(hkm6(7).loc(indgen(16)))    ;primary veis hv table
  veis_tbl(*,1)=lz.mf(hkm6(8).loc(indgen(16)))    ;alternate veis hv table
  
;which veis hv table, primary or alternate  (0 or 1) 
  vsmjf.primalt_tbl=get_bits(lz.mf(vsm6.offs(1)),vsm6.bv2(1).p,vsm6.bv2(1).n)
  
;veis alternating spins enabled between prim and alt hv tables (0:n, 1:y)
  vsmjf.altspin_enabled= $
    get_bits(lz.mf(vsm6.offs(1)),vsm6.bv1(1).p,vsm6.bv1(1).n)
  
  for ispn=0,n_spins-1 do begin    
    vsmjf.veistep(*,ispn)=veis_tbl(*,vsmjf.primalt_tbl(ispn))
    welec=where(vsmjf.veistep(*,ispn) lt 64,nwelec) 
    wion=where(vsmjf.veistep(*,ispn) ge 64 and $
            vsmjf.veistep(*,ispn) lt 128,nwion)
    if vsmjf.altspin_enabled(ispn) eq 0 and nwelec eq n_vesteps then begin
      vsmjf.eleion(ispn)=0
      vsmjf.eleion_sweep(ispn)=0         ;all electrons 
    endif $
    else if vsmjf.altspin_enabled(ispn) eq 0 and nwion eq n_vesteps then begin
      vsmjf.eleion(ispn)=1
      vsmjf.eleion_sweep(ispn)=1         ;all ions 
    endif else if vsmjf.altspin_enabled(ispn) eq 1 then begin
      if nwelec eq n_vesteps then vsmjf.eleion(ispn)=0   ;elecs this spin
      if nwion eq n_vesteps then vsmjf.eleion(ispn)=1   ;ions  this spin
      vsmjf.eleion_sweep(ispn)=2      ;alternating spins
    endif  
  endfor

;get relative gains (from date vs rel gain table in lzinput.pro)
   ;getrelgains,relgain,vsmjf.suntim_vsbl(0)
   vsmjf.relgain=relgain
    
if noglnt eq 0 then begin
  ;put average background counts into array if bdate exists and
  ;find corresponding relgain
  relgain_backg=1.+fltarr(vsmjf.n_vdets)
  if keyword_set(bdate) eq 0 then bdate=''
  if bdate ne '' then begin
    vsmjf.bdate=bdate
    vsmjf.bcts(*,*,*,*)=avgcts_m1(*)#replicate(1,n_spins) 
    ;for ispin=0,vsmjf.n_spins-1 do vsmjf.bcts(*,*,*,ispin)=avgcts_m1 
    ;btjd=pb5_tjd(ymd_pb5(long(bdate)))
    ;bda=double(btjd(0))*86400.d   
    ;getrelgains,relgain_backg,bda,rgchange_backg
    vsmjf.relgain_backg=relgain_backg
  endif
endif

if keyword_set(norelgains) ne 0 then begin   ;relgain factor=1
  vsmjf.relgain=replicate(1.,n_elements(vsmjf.relgain))
  vsmjf.relgain_backg=replicate(1.,n_elements(vsmjf.relgain_backg))
endif

;get count minus background BEFORE any corrections are made
  vsmjf.veis_b=float(vsmjf.veis)-vsmjf.bcts > 0.   ;floor is set to 0 counts
  
  ;multiply counts and background data by relative gains 
    vsmjf.cveis(*,*,*,*)=$
         (vsmjf.relgain(0:n_vdets-1)#replicate(1,n_vesteps*n_sectors*n_spins))*$
         float(vsmjf.veis(*,*,*,*))      
  
  vsmjf.bcts(*,*,*,*)=$
   (vsmjf.relgain_backg(0:n_vdets-1)#replicate(1,n_vesteps*n_sectors*n_spins))*$
   vsmjf.bcts(*,*,*,*)

  ;equivalent loop operation of the above
  ;for i=0,vsmjf.n_vdets-1 do begin
  ;  vsmjf.cveis(i,*,*,*)=vsmjf.relgain(i)*float(vsmjf.veis(i,*,*,*))
  ;  vsmjf.bcts(i,*,*,*)= vsmjf.relgain_backg(i)*vsmjf.bcts(i,*,*,*) 
  ;endfor


;get theoretical energy dependent detector efficiency
  coeff=[1.4779904, -4.5573397, 5.6175835, -2.8179218, 0.64383398, -0.055918481]
  for ispn=0,n_spins-1 do begin
    logen=reform(alog10(volt_en(vsmjf.veistep(*,ispn),/en)))
    deff=coeff(0)+coeff(1)*logen+coeff(2)*logen^2+coeff(3)*logen^3+$
      coeff(4)*logen^4+coeff(5)*logen^5
    deff=deff/max(deff)
    for k=0,n_elements(deff)-1 do vsmjf.cveis(*,k,*,ispn)=$
     vsmjf.cveis(*,k,*,ispn)/deff(k)
    
    ;get instrument factors in converting counts to f's and store it
       cts_f,vsmjf.veistep(0:vsmjf.n_vesteps-1,ispn),$
         cts_factor,cf,ion=vsmjf.eleion(ispn)
       vsmjf.cts_factor(*,*,ispn)=cts_factor
  endfor   

;background removal (see note below)
;  if swest.subtrbkg eq 'Yes' then $
    vsmjf.cveis_b=vsmjf.cveis-vsmjf.bcts > 0.   ;floor is set to 0 counts 

;strahl
   cts_f_strl,vsmjf.strlstep(0:vsmjf.n_spins-1),strl_cts_factor,cf_strl
   vsmjf.strl_cts_factor=strl_cts_factor

;convert strahl counts to f's
   for ispin=0,vsmjf.n_spins-1 do $
   for k=0,vsmjf.n_strphis-1 do for i=0,vsmjf.n_strdets-1 do $
       vsmjf.fstrl(i,k,ispin)=$
       vsmjf.strl_cts_factor(i,ispin)*dcomp_tbl(vsmjf.strl(i,k,ispin))
  
;convert veis counts to f's
  ;no background removed
  for ispin=0,vsmjf.n_spins-1 do $
    for k=0,vsmjf.n_sectors-1 do for i=0,vsmjf.n_vdets-1 do $
    vsmjf.fveis(i,*,k,ispin)=$
    vsmjf.cveis(i,*,k,ispin)*vsmjf.cts_factor(i,*,ispin)

  ;yes background removed
  for ispin=0,vsmjf.n_spins-1 do $
    for k=0,vsmjf.n_sectors-1 do for i=0,vsmjf.n_vdets-1 do $
    vsmjf.fveis_b(i,*,k,ispin)=$
    vsmjf.cveis_b(i,*,k,ispin)*vsmjf.cts_factor(i,*,ispin)
                      
;set counts and f's identified as glint to negative their value (see note below)
  
  vsmjf.veis=vsmjf.xveis * vsmjf.veis

  vsmjf.cveis=vsmjf.xveis * vsmjf.cveis
  vsmjf.fveis=vsmjf.xveis * vsmjf.fveis

  vsmjf.veis_b=vsmjf.xveis * vsmjf.veis_b
  vsmjf.cveis_b=vsmjf.xveis * vsmjf.cveis_b
  vsmjf.fveis_b=vsmjf.xveis * vsmjf.fveis_b



;=========== IMPORTANT NOTES =================================================

;after background removal, a floor of 0 count is set on vsmjf.cveis
;from which f, vsmjf.fveis, is computed

;============================================================================

;help,vsmjf,/str


;stop
end

