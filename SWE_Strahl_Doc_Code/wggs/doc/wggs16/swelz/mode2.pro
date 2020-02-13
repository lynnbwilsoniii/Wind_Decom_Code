
;================================= mode2 ==================================


pro mode2,eleion_sweep,lpr=lpr,noglnt=noglnt,err=err

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common reldetgains,relgain,relgain_backg
common m2stuff,hkind,ghk,vblhsp,sblhsp
common strahlstuff,strahl_hvtbl
common phasemod2,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common log_delog,comp_tbl,dcomp_tbl
common backgrnd,avgcts_m1,avgcts_m2,bdate
common sunglint,xveis_m1,xveis_m2,gdate
common sunglint_other,xveis_m1_other,gdate_other,xveis_m2_other
common nxtmjf,whspn1_nxt,vspncnt_nxt,veistep_nxt,veis_nxt,$
  strl_nxt,strlstep_nxt,lzrecn_nxt
common swestuff,swest
common wstuff,wst

err=''

if keyword_set(lpr) eq 0 then lpr=0
lpr=0  ;1

;print,'mode2....'


; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
scimode=scimode_ihk


;get time tagged spincnt
  time_utc,lz.mf(hkind(ghk(1).offs):hkind(ghk(1).offs)+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms

;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=32
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  ;bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin)
  nbcis_spin=iclicks_sunpulse/iclicks_bci 
  phidly_sun=42.5
  n_hspns=17  
  n_vdets=6 & n_vesteps=16 & n_sect_m2=4  & n_strdets=4 & n_strlstps=16

;set bci indices corrsponding to strahl samples (mode2)
  ibci_strl=[[1 + indgen(n_strlstps)],[65 + indgen(n_strlstps)]]

  ;ibci_strl=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,    $
  ;           52,53,54,55,56,57,58,59,60,61,62,63,64,65 ]       mode1

scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)


  ;which half of veis hv tbl (which spin) (0 1st , 1 2nd)
  vspncnt=-1+intarr(n_hspns)
  vspncnt=(lz.mf(vblhsp.hdr(0)) + 1) mod 2
  
  ;which half-spin (0 1st half, 1 2nd half)
    whspn=-1+intarr(n_hspns)
    whspn=lz.mf(vblhsp.hdr(3)) mod 16   

  if lpr then begin
    print,'vspncnt: which half of veis hv tbl (which spin) (0 1st , 1 2nd)   ',$
          'whspn: which half-spin (0 1st half, 1 2nd half)'
    for i=0,n_hspns-1 do print,i,vspncnt(i),whspn(i)
  endif

;----------- determine which half of veis hv table being sampled and  --------
;--------------------- which half of spin included in each data block --------

  ;whspn0=where(lz.mf(vblhsp.hdr(3)) mod 16 eq 0)  ;where 1st half spin
  ;whspn1=where(lz.mf(vblhsp.hdr(3)) mod 16 eq 1)  ;where 2nd half spin
  whspn0=where(lz.mf(vblhsp.hdr(3)) mod 16 eq 0 and $
               lz.mf(vblhsp.hdr(3)) ne 0)  ;where 1st half spin
  whspn1=where(lz.mf(vblhsp.hdr(3)) mod 16 eq 1 and $
               lz.mf(vblhsp.hdr(3)) ne 0)  ;where 2nd half spin
  whspn01=[whspn0,whspn1]

  vspncnt(whspn01)=lz.mf(vblhsp(whspn01).hdr(0))

  if lpr then begin
    print,'whspn0:  where 1st half spin ' 
    for i=0,n_elements(whspn0)-1 do print,i,whspn0(i)  
    print,'whspn1:  where 2nd half spin '
    for i=0,n_elements(whspn1)-1 do print,i,whspn1(i)
    print,'whspn01=[whspn0,whspn1] '
    for i=0,n_elements(whspn01)-1 do print,i,whspn01(i)
  endif
   
;---------- determine time at each bci (each data sample) for mode 2 --------


;sun time (suntim_vsbl(j) at spincnt_vsbl(j) ) for veis/strahl j'th hspn block,
;i.e., seconds from tjd epoch for j'th hspn when sc x-axis crosses sun line
  spincnt_vsbl=fltarr(n_hspns)
  ispin_diff=fltarr(n_hspns)
  spincnt_vsbl(whspn01)=float(lz.mf(vblhsp(whspn01).hdr(1)))
  sunspncnt=-1+intarr(n_hspns)
  sunspncnt(whspn01)=lz.mf(vblhsp(whspn01).hdr(1))
  

  ispin_diff(whspn01)=spincnt_vsbl(whspn01)-float(spincnt)
  w=where(ispin_diff(whspn01) lt -33,nw) &if nw gt 0 then $
    ispin_diff(whspn01(w))=ispin_diff(whspn01(w))+256
  w=where(ispin_diff(whspn01) gt  33,nw) &if nw gt 0 then $
    ispin_diff(whspn01(w))=ispin_diff(whspn01(w))-256
  
  sunsec_vsbl=dblarr(n_hspns)
  sunsec_vsbl(whspn01)=sec+ispin_diff(whspn01)*sp.spinp
  suntim_vsbl=dblarr(n_hspns)
  suntim_vsbl(whspn01)=tjd*86400.d0+sunsec_vsbl(whspn01)

  if lpr then begin
    print,'whspn01,spincnt_vsbl,sunspncnt,ispin_diff,sunsec_vsbl'
    for i=0,n_elements(whspn01)-1 do print,$
       i,whspn01(i),spincnt_vsbl(whspn01(i)),$
       sunspncnt(whspn01(i)),ispin_diff(whspn01(i)),sunsec_vsbl(whspn01(i))
  endif

;time between bci's assuming constant spin period over mjf
  sec_bci=spin_bci*sp.spinp
 
;time of each bci relative to true sun pulse, suntim_vsbl(j), for entire spin 
  time=indgen(nbcis_spin)*sec_bci + (phidly_sun/phi_bci)*sec_bci + sec_bci 

;time of each veis/strahl data sample in seconds from tjd epoch
  secveis=dblarr(n_vesteps,n_sect_m2,n_hspns)
  secstrl=dblarr(n_strlstps,n_hspns)
  for i=0,n_elements(whspn01)-1 do begin
    ihspn=whspn01(i)
    secveis(0:n_vesteps-1,0:n_sect_m2-1,ihspn)=$
      suntim_vsbl(ihspn)+$
      time(whspn(ihspn)*nbcis_spin/2+indgen(nbcis_spin/2))  
    secstrl(0:n_strlstps-1,ihspn)= $
      suntim_vsbl(ihspn)+$
      time(ibci_strl(whspn(ihspn)*n_strlstps:(whspn(ihspn)+1)*n_strlstps-1))

      lprt=0 & if lprt then begin
        print,'ihspn,whspn(ihspn),',$
          'suntim_vsbl(ihspn)-suntim_vsbl(whspn01(0))',$
          ihspn,whspn(ihspn),$
          suntim_vsbl(ihspn)-suntim_vsbl(whspn01(0))
        for jsect=0,n_sect_m2-1 do for ivestep=0,n_vesteps-1 do $
          print,jsect,ivestep,$
          secveis(ivestep,jsect,ihspn)-suntim_vsbl(whspn01(0))
      endif

  endfor
  if lprt then begin
    window,0
    plot,whspn01,suntim_vsbl(whspn01)-suntim_vsbl(whspn01(0)),$
    psym=4,symsize=.25
    window,1
    plot,secveis(*,*,whspn01)-suntim_vsbl(whspn01(0)),$
    psym=4,symsize=.25,color=175
  endif

;veis_hvtbl is veis hv table of steps for each sweep and specie being sampled

;sweep mode from full veis hv table, i.e.,
;  eleion_sweep = 0   all electrons,
;               = 1   all ions,
;               = 2   electrons, ions alternating sectors (or sweeps)
;               = 3   electrons, ions alternating spins
;               =-1  undetermined


;print,'mode2: eleion_sweep ',eleion_sweep

  ;w10,w11,w20,w21 : wxy
  ;     w=where
  ;     x=which spin of hv tbl, 1=first or 2=second,  and
  ;     y=which half-spin, 0=first or 1=second

  w10=where(lz.mf(vblhsp.hdr(3)) / 16 eq 1 and lz.mf(vblhsp.hdr(3)) mod 16 eq 0)
  w11=where(lz.mf(vblhsp.hdr(3)) / 16 eq 1 and lz.mf(vblhsp.hdr(3)) mod 16 eq 1)
  w20=where(lz.mf(vblhsp.hdr(3)) / 16 eq 2 and lz.mf(vblhsp.hdr(3)) mod 16 eq 0)
  w21=where(lz.mf(vblhsp.hdr(3)) / 16 eq 2 and lz.mf(vblhsp.hdr(3)) mod 16 eq 1)

  veistep=255b+bytarr(n_vesteps,n_sect_m2,n_hspns)
  nsv=n_sect_m2*n_vesteps
  for i=0,n_elements(w10)-1 do $
    veistep(0:n_vesteps-1,0:n_sect_m2-1,w10(i))=$
      veis_hvtbl(0:nsv-1)

  for i=0,n_elements(w11)-1 do $
    veistep(0:n_vesteps-1,0:n_sect_m2-1,w11(i))=$
      veis_hvtbl(nsv:2*nsv-1)

  for i=0,n_elements(w20)-1 do $
    veistep(0:n_vesteps-1,0:n_sect_m2-1,w20(i))=$
      veis_hvtbl(2*nsv:3*nsv-1)

  for i=0,n_elements(w21)-1 do $
    veistep(0:n_vesteps-1,0:n_sect_m2-1,w21(i))=$
      veis_hvtbl(3*nsv:4*nsv-1)
  
  if lpr then begin
    print,'w10,veistep(0:n_vesteps-1,0:n_sect_m2-1,w10)'
    for i=0,n_elements(w10)-1 do begin
     print,w10(i)
     print,veistep(0:n_vesteps-1,0:n_sect_m2-1,w10(i))
    endfor
    print,'w11,veistep(0:n_vesteps-1,0:n_sect_m2-1,w11)'
    for i=0,n_elements(w11)-1 do begin
     print,w11(i)
     print,veistep(0:n_vesteps-1,0:n_sect_m2-1,w11(i))
    endfor
    print,'w20,veistep(0:n_vesteps-1,0:n_sect_m2-1,w20)'
    for i=0,n_elements(w20)-1 do begin
     print,w20(i)
     print,veistep(0:n_vesteps-1,0:n_sect_m2-1,w20(i))
    endfor
    print,'w21,veistep(0:n_vesteps-1,0:n_sect_m2-1,w21)'
    for i=0,n_elements(w21)-1 do begin
     print,w21(i)
     print,veistep(0:n_vesteps-1,0:n_sect_m2-1,w21(i))
    endfor
  endif
  
;veis and strahl data (log compressed)
  veis=intarr(n_vdets,n_vesteps,n_sect_m2,n_hspns)
  veis(*,*,*,*) = dcomp_tbl(lz.mf((vblhsp.ind)))
  strl=bytarr(n_strdets,n_strlstps,n_hspns)
  strl(*,*,*)   = lz.mf((sblhsp.ind))
  ;print,'phasem1_ise: veis',veis

;strahl steps
  strlstep=bytarr(n_hspns)
  strlstep =lz.mf(sblhsp.hdr(0))
 
sp.lst_scimod=scimode_ihk


;--------------- find whole spins that begin and end in this mjf -------------

wspin=lonarr(3,2,10)  ;second index = 1st or 2nd half-spin of the whole spin
ispin=-1
for i=0,n_elements(whspn0)-1 do begin
  w01=where(vspncnt eq vspncnt(whspn0(i)),nw01) 
  if lpr then begin
    print,'i,whspn0(i),vspncnt(whspn0(i)), w01'
    print,i,whspn0(i),vspncnt(whspn0(i)), w01
  endif
  if nw01 eq 2 then begin
    ispin=ispin+1
    wspin(0,0:1,ispin)= w01   ;half-spin blocks that contain whole spins
    wspin(1,0:1,ispin)=vspncnt(whspn0(i))
    wspin(2,0:1,ispin)=lz.recn
  endif
endfor

if lpr then begin
  print,'wspin(0,0:1,i),wspin(1,0:1,i),spin(2,0:1,i)'
  for i=0,ispin do for j=0,1 do print,i,wspin(0,j,i),wspin(1,j,i),wspin(2,j,i)
endif

ispin_this_mjf=ispin

;determine if the last half-spin block contains first-half-spin data, if yes,
;then add first half-spin block data of next mjf to current mjf data 
  if lpr then $
    print,'wspin(0,0,ispin_this_mjf), whspn0(n_elements(whspn0)-1) ', $
           wspin(0,0,ispin_this_mjf), whspn0(n_elements(whspn0)-1) 
  if wspin(0,0,ispin_this_mjf) lt whspn0(n_elements(whspn0)-1) then begin  ;yes
     if lpr then print,'Adding first block of next mjf.'
     if lpr then print,'last block this mjf,recn,lz.recn= ',lz.recn-1,lz.recn, $
       whspn0(n_elements(whspn0)-1),'  vspncnt ',$
       vspncnt(whspn0(n_elements(whspn0)-1)),$
       format='(a40,2i6,i4,a12,i4)'
     if lpr then print,'first block next mjf, recn, lz.recn= ',$
       lzrecn_nxt-1,lzrecn_nxt,whspn1_nxt,'  vspncnt ',vspncnt_nxt,$
       format='(a40,2i6,i4,a12,i4)'
     if vspncnt(whspn0(n_elements(whspn0)-1)) eq vspncnt_nxt and $
     lz.recn eq lzrecn_nxt-1 then begin
       ispin=ispin_this_mjf+1 
       wspin(0,0,ispin)=whspn0(n_elements(whspn0)-1)
       wspin(0,1,ispin)=whspn1_nxt
       wspin(1,0,ispin)=vspncnt(whspn0(n_elements(whspn0)-1))
       wspin(1,1,ispin)=vspncnt_nxt
       wspin(2,0,ispin)=lz.recn
       wspin(2,1,ispin)=lzrecn_nxt
     endif else begin
       err='first block in next mjf is NOT the second half of whole spin'
       print,err & return
     endelse
  endif else if lpr then print,'last block this mjf completes whole spin'

if lpr then begin
  print,'Final: wspin(0,0:1,ispin),wspin(1,0:1,ispin),spin(2,0:1,ispin)'
  print,'        half-spin   spin number   lzrecn'
  for i=0,ispin do for j=0,1 do print,wspin(0,j,i),wspin(1,j,i),wspin(2,j,i)
  print,'ispin ',ispin
endif

n_spins=ispin+1    
wspin=wspin(*,*,0:n_spins-1)

n_sectors=2*n_sect_m2
n_strphis=2*n_strlstps

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
  n_spins:long(n_spins),$
  n_strdets:long(n_strdets),  $
  n_strphis:long(n_strphis),$
  deltasp:double(iclicks_bci)/double(iclicks_sunpulse),$
  deadtim_ele:0.d,  $
  deadtim_ion:0.d,  $
  delt_ele:0.d,  $
  delt_ion:0.d,$
  geomf:fltarr(n_vdets),$
  lzrecn:intarr(2,n_spins),$
  vspncnt:-1+intarr(n_spins),$
  sunspncnt:-1+intarr(n_spins),$
  veis:intarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  veis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$ 
  cveis:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  cveis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  fveis:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$ 
  fveis_b:fltarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  veistep:255b+bytarr(n_vesteps,n_sectors,n_spins),$
  hvtbl:bytarr(64),$
  eleion_sweep:0,$
  eleion:-1+intarr(n_sectors,n_spins),$
  secveis:dblarr(n_vesteps,n_sectors,n_spins),$
  phiveis:fltarr(n_vdets,n_vesteps,n_sectors),$
  theveis:theveis,$
  vunit:dblarr(n_vdets,n_vesteps,n_sectors,3),  $
  strl:bytarr(n_strdets,n_strphis,n_spins),$
  fstrl:fltarr(n_strdets,n_strphis,n_spins),$
  strlstep:bytarr(n_spins),$
  secstrl:dblarr(n_strphis,n_spins),$
  phistrl:phistrl,$
  thestrl:thestrl,$
  vunitstrl:dblarr(n_strdets,n_strphis,3),$ 
  suntim_vsbl:dblarr(n_spins),$
  pb5tim_vsbl:lonarr(3,n_spins),$
  vqlty:lonarr(n_spins),  $
  sqlty:lonarr(n_spins),$
  xveis:1+intarr(n_vdets,n_vesteps,n_sectors,n_spins),$
  relgain:fltarr(n_vdets),  $
  bdate:'',  $
  bcts: fltarr(n_vdets,n_vesteps,n_sectors,n_spins),  $
  relgain_backg:fltarr(n_vdets),  $
  cts_factor:fltarr(n_vdets, n_vesteps,n_sectors,n_spins),  $
  strl_cts_factor:fltarr(n_strdets, n_spins),$
  background_test:0  }

;make structure assignments 
  vsmjf.phiveis(*,*,*)=phiveis
  vsmjf.descr='veis-strahl data samples'
  vsmjf.tjd=tjd      ;trucated julian day of time-tagged spin this mjf
  vsmjf.sec=sec      ;seconds of day of time-tagged spin this mjf
  vsmjf.hour=hour    ;hour of day of time-tagged spin this mjf
  vsmjf.min=min      ;min of hour of time-tagged spin this mjf
  vsmjf.isec=isec    ;sec of min of time-tagged spin this mjf
  vsmjf.ms=ms        ;ms of sec of time-tagged spin this mjf
  vsmjf.mjfcnt=lz.mf(ihk(1).offs)  ;mjf counter
  if scimode_ihk eq 2 or scimode_ihk eq 11 then vsmjf.scimode=2
  ;vsmjf.scimode=scimode_ihk        ;science mode
  vsmjf.spinp=sp.spinp   ;spin period
  vsmjf.hvtbl=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln)))
  vsmjf.eleion_sweep=eleion_sweep


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
  

  for jspin=0,n_spins-1 do begin

    ;test if both half-spins in this mjf
    if wspin(2,0,jspin) eq wspin(2,1,jspin) then begin 
      vsmjf.lzrecn(*,jspin)=wspin(2,*,jspin)
      vsmjf.vspncnt(jspin)=wspin(1,0,jspin)
      vsmjf.sunspncnt(jspin)=sunspncnt(wspin(0,0,jspin))
      vsmjf.suntim_vsbl(jspin)=suntim_vsbl(wspin(0,0,jspin))
      vsmjf.veis(*,*,0:n_sectors/2-1,jspin)=veis(*,*,*,wspin(0,0,jspin))
      vsmjf.veis(*,*,n_sectors/2:n_sectors-1,jspin)=veis(*,*,*,wspin(0,1,jspin))
      vsmjf.veistep(*,0:n_sectors/2-1,jspin)=veistep(*,*,wspin(0,0,jspin))
      vsmjf.veistep(*,n_sectors/2:n_sectors-1,jspin)=$
        veistep(*,*,wspin(0,1,jspin))
      welec=where(vsmjf.veistep(0,*,jspin) lt 64)
      if welec(0) ne -1 then vsmjf.eleion(welec,jspin)=0
      wion=where(vsmjf.veistep(0,*,jspin) ge 64 and $
                 vsmjf.veistep(0,*,jspin) lt 128)
      if wion(0) ne -1 then vsmjf.eleion(wion,jspin)=1
      vsmjf.secveis(*,0:n_sectors/2-1,jspin)=secveis(*,*,wspin(0,0,jspin))
      vsmjf.secveis(*,n_sectors/2:n_sectors-1,jspin)=$
        secveis(*,*,wspin(0,1,jspin))
      vsmjf.strl(*,0:n_strphis/2-1,jspin)=strl(*,*,wspin(0,0,jspin))
      vsmjf.strl(*,n_strphis/2:n_strphis-1,jspin)=strl(*,*,wspin(0,1,jspin))
      ;if strlstep(wspin(0,0,jspin)) lt 17 then  $
      if strlstep(wspin(0,0,jspin)) ne 187 then  $
        vsmjf.strlstep(jspin)=strahl_hvtbl(strlstep(wspin(0,0,jspin)))
      vsmjf.secstrl(0:n_strphis/2-1,jspin)=secstrl(*,wspin(0,0,jspin))
      vsmjf.secstrl(n_strphis/2:n_strphis-1,jspin)=secstrl(*,wspin(0,1,jspin))

      if lpr then begin
        print,'both half-spins in this mjf ',$
           jspin,wspin(2,0,jspin),wspin(2,1,jspin)
        print,'veistep(*,*,wspin(0,0:1,jspin)) ',veistep(*,*,wspin(0,0:1,jspin))
        print,'veis(*,*,*,wspin(0,0:1,jspin)) ',$
               veis(*,*,*,wspin(0,0:1,jspin))
       ;stop
       endif

    ;test if adding nxt mjf block and if veis spncnt the same
    endif else if (wspin(2,0,jspin) eq wspin(2,1,jspin)-1) and $
            (wspin(1,0,jspin) eq wspin(1,1,jspin)) then begin

      vsmjf.lzrecn(*,jspin)=wspin(2,*,jspin)
      vsmjf.vspncnt(jspin)=wspin(1,0,jspin)
      vsmjf.sunspncnt(jspin)=sunspncnt(wspin(0,0,jspin))
      vsmjf.suntim_vsbl(jspin)=suntim_vsbl(wspin(0,0,jspin))
      vsmjf.veis(*,*,0:n_sectors/2-1,jspin)=veis(*,*,*,wspin(0,0,jspin))
      vsmjf.veis(*,*,n_sectors/2:n_sectors-1,jspin)=veis_nxt
      vsmjf.veistep(*,0:n_sectors/2-1,jspin)=veistep(*,*,wspin(0,0,jspin))
      ;sweep is same in second half-spin as first for either sweep mode
        vsmjf.veistep(*,n_sectors/2:n_sectors-1,jspin)=$
          veistep(*,*,wspin(0,0,jspin))     
      welec=where(vsmjf.veistep(0,*,jspin) lt 64)
      if welec(0) ne -1 then vsmjf.eleion(welec,jspin)=0
      wion=where(vsmjf.veistep(0,*,jspin) ge 64 and $
                 vsmjf.veistep(0,*,jspin) lt 128)
      if wion(0) ne -1 then vsmjf.eleion(wion,jspin)=1
      vsmjf.secveis(*,0:n_sectors/2-1,jspin)=secveis(*,*,wspin(0,0,jspin))
      vsmjf.secveis(*,n_sectors/2:n_sectors-1,jspin)=$
        suntim_vsbl(wspin(0,0,jspin))+time(nbcis_spin/2+indgen(nbcis_spin/2))

      vsmjf.strl(*,0:n_strphis/2-1,jspin)=strl(*,*,wspin(0,0,jspin))
      vsmjf.strl(*,n_strphis/2:n_strphis-1,jspin)=strl_nxt
      ;if strlstep(wspin(0,0,jspin)) lt 17 then  $
      if strlstep(wspin(0,0,jspin)) ne 187 then  $
        vsmjf.strlstep(jspin)=strahl_hvtbl(strlstep(wspin(0,0,jspin)))
      vsmjf.secstrl(0:n_strphis/2-1,jspin)=secstrl(*,wspin(0,0,jspin))
      vsmjf.secstrl(n_strphis/2:n_strphis-1,jspin)=$
        suntim_vsbl(wspin(0,0,jspin))+time(ibci_strl(n_strlstps:2*n_strlstps-1))
   
    endif
endfor


;put unitvectors (payload coords) into vsmjf.unit
  vsmjf.vunit=vunit
  vsmjf.vunitstrl=vunitstrl

;convert tjd,sec to pb5 time
  for ispin=0,vsmjf.n_spins-1 do begin
    vsmjf.pb5tim_vsbl(*,ispin)=sec_pb5(vsmjf.suntim_vsbl(ispin),err=err) 
    if err ne '' then return
  endfor

;help,vsmjf,/str


;relative gains
   ;getrelgains,relgain,vsmjf.suntim_vsbl(0)
   vsmjf.relgain=relgain

if noglnt eq 0 then begin
  ;put average background counts into array if bdate exists and
  ;find corresponding relgain
  if keyword_set(bdate) eq 0 then bdate=''
  if bdate ne '' then begin
    vsmjf.bdate=bdate
    vsmjf.bcts(*,*,*,*)=avgcts_m2(*)#replicate(1,n_spins)
    ;for ispin=0,vsmjf.n_spins-1 do vsmjf.bcts(*,*,*,ispin)=avgcts_m1 
    ;btjd=pb5_tjd(ymd_pb5(long(bdate)))
    ;bda=double(btjd(0))*86400.d   
    ;getrelgains,relgain_backg,bda,rgchange_backg
    ;vsmjf.relgain_backg=relgain_backg
  endif
endif

;get count minus background BEFORE any corrections are made
  vsmjf.veis_b=float(vsmjf.veis)-vsmjf.bcts
  
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
  ;endfor

;get f conversion factor for each specie and  
;get theoretical energy dependent detector efficiency
  coeff=[1.4779904, -4.5573397, 5.6175835, -2.8179218, 0.64383398, -0.055918481]
  for ispin=0,vsmjf.n_spins-1 do for k=0,vsmjf.n_sectors-1 do begin

    case vsmjf.eleion(k,ispin) of
    0: begin   ;electron specie
         logen=alog10(volt_en(vsmjf.veistep(*,k,ispin),/en))
         deff=coeff(0)+coeff(1)*logen+coeff(2)*logen^2+coeff(3)*logen^3+$
           coeff(4)*logen^4+coeff(5)*logen^5
         deff=deff/max(deff)
         cts_f,vsmjf.veistep(*,k,ispin),cts_factor,cf,ion=0
       endcase
    1: begin
         deff=1.+fltarr(vsmjf.n_vesteps)   ;ion specie
         cts_f,vsmjf.veistep(*,k,ispin),cts_factor,cf,ion=1
       endcase
    else: begin
            err='mode2 error!' & return
          endcase
    endcase
 
   vsmjf.cts_factor(*,*,k,ispin)=cts_factor(*,*)
    vsmjf.cveis(*,*,k,ispin)=$
      vsmjf.cveis(*,*,k,ispin)/(replicate(1,vsmjf.n_vdets)#deff(*))
    
    ;for i=0,vsmjf.n_vdets-1 do begin
    ;  vsmjf.cts_factor(i,*,k,ispin)=cts_factor(i,*)
    ;  vsmjf.cveis(i,*,k,ispin)=vsmjf.cveis(i,*,k,ispin)/deff(*)
    ;  vsmjf.fveis(i,*,k,ispin)=$
    ;    vsmjf.cts_factor(i,*,k,ispin)*vsmjf.cveis(i,*,k,ispin)
    ;endfor
  endfor
    
;background removal (see note below)
  ;if swest.subtrbkg eq 'Yes' then $
    vsmjf.cveis_b=vsmjf.cveis-vsmjf.bcts > 0.   ;floor is set to 0 counts 

;convert counts to f's with conversion factor for each specie 
   ;no background removed
   vsmjf.fveis=vsmjf.cts_factor*vsmjf.cveis

   ;yes background removed
   vsmjf.fveis_b=vsmjf.cts_factor*vsmjf.cveis_b

;set counts and f's identified as glint to negative their value 
  if noglnt eq 0 then begin  
    ;for i=0,n_spins-1 do vsmjf.xveis(*,*,*,i)=xveis_m2 
    if swest.univgmask then $
       vsmjf.xveis(*,*,*,*)=xveis_m2_other(*)#replicate(1,n_spins) else $
       vsmjf.xveis(*,*,*,*)=xveis_m2(*)#replicate(1,n_spins)  
    ;number of glint points
    ;wglnt=where(xveis_m2 lt 0,nwglnt)
    wglnt=where(vsmjf.xveis(*,*,*,0) lt 0,nwglnt)
    swest.ndel=nwglnt
  endif

  vsmjf.veis=vsmjf.xveis * vsmjf.veis
  vsmjf.cveis=vsmjf.xveis * vsmjf.cveis
  vsmjf.fveis=vsmjf.xveis * vsmjf.fveis   

  vsmjf.cveis_b=vsmjf.xveis * vsmjf.cveis_b
  vsmjf.fveis_b=vsmjf.xveis * vsmjf.fveis_b

;get instrument factors in converting strahl counts to f's and store it
   cts_f_strl,vsmjf.strlstep(0:vsmjf.n_spins-1),strl_cts_factor,cf_strl,/mode2
   vsmjf.strl_cts_factor=strl_cts_factor
   
;convert strahl counts to f's
   for ispin=0,vsmjf.n_spins-1 do $
   for k=0,vsmjf.n_strphis-1 do for i=0,vsmjf.n_strdets-1 do $
       vsmjf.fstrl(i,k,ispin)=$
       vsmjf.strl_cts_factor(i,ispin)*dcomp_tbl(vsmjf.strl(i,k,ispin))

;stop  
;for now, we assume that all sectors of a given spin are the same specie
;and have the same steps
  stepdiff=where(vsmjf.veistep(*,0,0)-vsmjf.veistep(*,1,0) ne 0)
  if stepdiff(0) ne -1 then stop,'consecutive sectors sample different species'

;=========== IMPORTANT NOTES =================================================

;after background removal, a floor of 0 count is set on vsmjf.cveis
;from which f, vsmjf.fveis, is computed

;============================================================================


;print,'*'
;print,strlstep
;print,transpose(wspin(0,0,*))
;print,transpose(strlstep(wspin(0,0,*)))
;print,vsmjf.strlstep
;print,'*'
;stop


end
