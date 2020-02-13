;================================= mode1 ==================================


pro mode1,lpr=lpr

common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common removedata,delete,ndel
common delset,delete_set
common phase,phiveis,theveis,phistrl,thestrl

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

;determine spin phase at each bci (each data sample) when mode changes
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
	
;get detector angles
  phasem1

;--------------- make veis/strahl data structure assignments this mjf -------


;define data structure for veis/strahl data samples 
  vsmjf={  vsdata,$
  descr:'   ',tjd:0l,sec:0d,hour:0l,min:0l,isec:0l,ms:0,mjfcnt:0l,scimode:0,$
  n_vdets:0l,n_vesteps:0l,n_sectors:0l,n_spins:0l,n_strdets:0l,n_strphis:0l,$
  veis:bytarr(n_vdets,n_vesteps,n_sectors,n_spins),veistep:bytarr(n_vesteps),$
  secveis:dblarr(n_vesteps,n_sectors,n_spins),$
  phiveis:fltarr(n_vdets,n_vesteps,n_sectors),theveis:fltarr(n_vdets),$
  strl:bytarr(n_strdets,n_strphis,n_spins),strlstep:bytarr(n_spins),$
  secstrl:dblarr(n_strphis,n_spins),$
  phistrl:fltarr(n_strphis),thestrl:fltarr(n_strdets),$
  suntim_vsbl:dblarr(n_spins),$
  vqlty:lonarr(n_spins),sqlty:lonarr(n_spins)  }

;strl:bytarr(n_strphis,n_strdets,n_spins),strlstep:bytarr(n_spins),$

;make structure assignments 
  vsmjf.n_vdets=n_vdets            ;number of veis detectors
  vsmjf.n_vesteps=n_vesteps        ;number of veis energy steps per scan
  vsmjf.n_sectors=n_sectors        ;number of sectors
  vsmjf.n_spins=n_spins            ;number of spins with data per mjf
  vsmjf.n_strdets=n_strdets        ;number strahl detectors
  vsmjf.n_strphis=n_strphis        ;number of strahl samples (azimuth) per mjf


  vsmjf.descr='veis-strahl data samples'
  vsmjf.tjd=tjd      ;trucated julian day of time-tagged spin this mjf
  vsmjf.sec=sec      ;seconds of day of time-tagged spin this mjf
  vsmjf.hour=hour    ;hour of day of time-tagged spin this mjf
  vsmjf.min=min      ;min of hour of time-tagged spin this mjf
  vsmjf.isec=isec    ;sec of min of time-tagged spin this mjf
  vsmjf.ms=ms        ;ms of sec of time-tagged spin this mjf
  vsmjf.mjfcnt=lz.mf(ihk(1).offs)  ;mjf counter
  vsmjf.scimode=scimode_ihk        ;science mode

  vsmjf.phiveis=phiveis
  vsmjf.theveis=theveis
  vsmjf.phistrl=phistrl
  vsmjf.thestrl=thestrl



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
;time between bci's assuming constant spin period over mjf
  sec_bci=spin_bci*sp.spinp
 
;time of each bci relative to true sun pulse, suntim_vsbl(j), for each spin j
  time=indgen(nbcis_spin)*sec_bci + (phidly_sun/phi_bci)*sec_bci + sec_bci 


;time of each veis and strahl data sample
  for ispin=0,n_spins-1 do begin
    ;time of each veis data sample in seconds from tjd epoch
    ibci=-1
    for isector=0,n_sectors-1 do begin
      ibci=ibci+1                        ;skipping 1 bci each sweep
      for ivestep=0,n_vesteps-1 do begin
        ibci=ibci+1
        vsmjf.secveis(ivestep,isector,ispin)=suntim_vsbl(ispin)+time(ibci)
      endfor
    endfor
    ;time of each strahl data sample in seconds from tjd epoch
    for istrphi=0,n_strphis-1 do vsmjf.secstrl(istrphi,ispin)= $
      suntim_vsbl(ispin)+time(ibci_strl(istrphi))
  endfor

;veis and strahl data (log compressed)
  vsmjf.veis(*,*,*,*) = lz.mf((vdatc.ind))
  vsmjf.strl(*,*,*)   = lz.mf((sdatc.ind))
  ;print,'phasem1_ise: vsmjf.veis',vsmjf.veis


;=========== begin deletion of selected  (sun glint) samples ===============
;if delete eq 1 then remove selected data from vsmjf.veis using vsmjf.xveis
if delete eq 1 then begin
  xveis=1b+bytarr(n_vdets,n_vesteps,n_sectors,n_spins)
  dglint_m1,delete_set,xveis 
  ndel=n_elements(where(xveis eq 0))/n_spins 
  vsmjf.veis=xveis * vsmjf.veis
  print,'mode1: delete_set ',delete_set,', ',$
  ndel,' veis samples per spin deleted'

endif
;===================== end deletion ====================================

;veis steps
  vsmjf.veistep=lz.mf(hkm1(24).offs+indgen(n_vesteps))

;strahl steps
  vsmjf.strlstep =lz.mf(vsm1(1).offs)           
 
sp.lst_scimod=scimode_ihk

;help,vsmjf,/str


end




;reads and process a SWE lz file (RJF Jan95)

common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common phase,phiveis,theveis,phistrl,thestrl
common removedata,delete,ndel
common delset,delete_set


;==============correct spin rates during eclipse from Chris Owen============
restore,'/home/u3rjf/KO_info.idl'
w=where(phi ne 0,nw)    ;phi=phi required to correct calulated phi's
print,msecd(w(0))/3600,msecd(w(nw-1))/3600
msecd1=msecd(w(0))
msecd2=msecd(w(nw-1))
print,msecd1,msecd2

lunar=replicate({recn:0l,spinbl:0l,timpb5:lonarr(3),dphi:0.,$
  mshftmap:lonarr(6)},700)

;initialize delete flag to delete selected samples: 1= yes delete   0= no delete
  delete=0
  delete_set=0    

;in mode 1 
  ndets=6
  nsteps=16
  nsects=6
  nspins=7


;get lz datapath
  lzpath=getenv('LZPATH')

;select lz data file  
  ;infile=pickfile(/read,get_path=lzpath,path=lzpath(0),filter='*.dat',$
          ;title='Open SWE LZ Data Files',/must_exist)
infile='/mnt/lepjds_data1/swe/lz/94122701.dat'

print,' ' & print,'lztim:'
print,'read lz file ',infile

;optional:  mag data file 
;magfile=''

ctmmode=['u','m','s','e']
ctmrate=['s','f']
lpr=0
rfill=-1.0e31

;set up structure (most of these tags are not used currently)
  sp={spinparams,mfrecn:0l,mfyr:0l,mfdy:0l,mfms:0l,$
      spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
      old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
      lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

;read compress/decompress tables
  ;decompress_tbl

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1 tm map of science and genl hk data offsets into lz.mf
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  
                    
;open LZ data file and read header
  openr,lundat,infile,/get_lun
  fh=read_lzhdr(lundat) ;fh=file header structure 


phasem1  ;get detector angles (in common phase)

;get spinperiod for file (reads file until two consecutive sci records read)
get_spinp,start=1

nfill=0l
spn=-1l
lastsec=0.d

k=-1


for recn=1141,1229 do begin  ;contains 27 edec 1994 eclipse

;process selected lz record (set keyword lpr=1 to turn on print each record)
  proc_rec,tmmode_ihk=tmmode_ihk,lpr=0,elec_ion=elec_ion 
  if elec_ion eq 1 then print,'ION MODE!!'
    
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
    print,' '&print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
    goto,endrecordloop
  endif

  ;check data quality
  ;if one mnf flag set, then skip entire record
     n_mnf=250
     if total(lz.qlty(0:n_mnf-1)) gt 0 then  goto, endrecordloop 
  
  ;print,'recn,spinbl,timpb5,tjd,sec,ymd,hms,sec-lastsec,spincount'
  for isp=0,vsmjf.n_spins-1 do begin
    spinbl=long(isp)

    tjd=long(fix(vsmjf.suntim_vsbl(spinbl)/86400.d))  ;truncated julian day
    sec=vsmjf.suntim_vsbl(spinbl) - tjd*86400.d       ;seconds of day
    hour_hms,sec/3600.d,hms                           ;hhmmss
    spndt='spn '+string(tjd,format='(i5)')+' '+hms
    timpb5=tjd_pb5(long(tjd),long(1000*sec)) ;pb5 time (yr, da of yr, ms of da)
    ymd=yrmoda(timpb5)  ;mo da yr
    spincount=lz.mf((vsm1(0).offs(spinbl)))

    if sec ge msecd1 and sec le msecd2 then begin
      k=k+1 
      lunar(k).recn=recn
      lunar(k).spinbl=spinbl
      lunar(k).timpb5=timpb5

      print,recn,spinbl,timpb5,tjd,sec,ymd,hms,sec-lastsec,spincount,$
       format='(i5,1x,i1,1x,i4,1x,i3,1x,i8,1x,i4,1x,f9.3,1x,a10,2x,a8,f11.3,i4)'
      prnt=0 & if prnt eq 1 then begin
      print,'spinbl ',spinbl,format='(a7,i2)'
      for isect=0,vsmjf.n_sectors-1 do begin
        print,' ' & print,'sector ',isect,format='(a7,i2)'
        print,vsmjf.secveis(*,isect,spinbl)-tjd*86400.d,format='(8f9.3)'
        print,' ' 
      endfor & endif
    endif

    ;determine which science mode
    scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
    if scimode_ihk eq 0 or  scimode_ihk eq 1 then begin  ;mode 1  
      lastsec=sec     
    endif  ;end mode1
 
    spn=spn+1      
    endspinloop:
  endfor  ;end spinbl loop
 
  skiprec=10
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)
       endif
  endrecordloop:

endfor     ;end record loop

print,'lz files are read; continuing'

lunar=lunar(0:k)
lunar.dphi=interpol(phi(w),msecd(w),lunar.timpb5(2)*1.d-3)


prt=0

nsp=n_elements(lunar)  
nvs=nsteps*nsects
step=intarr(nvs,ndets)
sect=intarr(nvs,ndets)

mshftmap=intarr(ndets,nsp)

for isp=0,nsp-1 do begin
  step_shft=intarr(nvs,ndets)
  sect_shft=intarr(nvs,ndets)
  phi_new=dblarr(ndets,nsteps,nsects)

  phi_true=dblarr(ndets,nsteps,nsects)
  phi_true(*,*,*)=vsmjf.phiveis(*,*,*)+lunar(isp).dphi/!dtor
  wg=where(phi_true gt 360.d)
  if wg(0) ne -1 then phi_true(wg)=phi_true(wg) -360.d

  for idet=0,ndets-1 do begin

    ;find the actual spin phase angle corresponding to 
    ;    each apparent spin phase angle
    
    ;find the actual step-sector at the apparent start of the spin, i.e.,
    ;  k=isect*nsteps+istep such that 
    ;  phi_true(*,istep,isect)=vsmjf.phiveis(*,0,0)
          ph1=min(abs(phi_true(idet,*,*)-vsmjf.phiveis(idet,0,0)),k1)

    ;number of steps in spin that apparent start of spin is actually shifted
      mshftmap(idet,isp)=k1   ;k1 between 0 and 95 in mode 1

    
    ;step-sector corresponding to actual start of spin  
    isect1=fix(k1/nsteps)
    istep1=k1-isect1*nsteps

    ;find the actual step-sector from begin to end of apparent spin using
    ;   the mapping, mshftmap(idet,isp)
    
    step=indgen(nvs)-nsteps*fix(indgen(nvs)/nsteps)
    sect=fix(indgen(nvs)/nsteps)
    mshft=indgen(nvs) + mshftmap(idet,isp)
    wnvs=where(mshft ge nvs)
    if wnvs(0) ne -1 then mshft(wnvs)=mshft(wnvs)-nvs
    step_shft(*,idet)=mshft-fix(mshft/nsteps)*nsteps
    sect_shft(*,idet)=fix(mshft/nsteps)
    for i=0,nvs-1 do phi_new(idet,step(i),sect(i))=$
      phi_true(idet,step_shft(i,idet),sect_shft(i,idet))

    if isp eq 408 then begin
      print,'ecl recn, det, actual step at start of spin, step, actual step'
      print,'voltage step, sector, apparent phi, true phi'
      print,'shifted voltage step, shifted sector, new phi'
      for i=0,nvs-1 do begin     
        print,isp,idet,mshftmap(idet,isp),$
        i,mshft(i),step(i),sect(i),$
        vsmjf.phiveis(idet,step(i),sect(i)),$
        phi_true(idet,step(i),sect(i)),$
        step_shft(i,idet),$
        sect_shft(i,idet),$
        phi_new(idet,step(i),sect(i)),$
        format='(i3,i2,i3,i4,i4,7x,i3,i2,2f7.1,7x,i3,i2,f7.1)
      endfor
    endif

    ;************** fine tuning mshftmap by 3 steps (3 * 3.5 degrees) ******
    ;************** for a better fit to sun glint                     ******
    mshftmap(idet,isp) = mshftmap(idet,isp) - 3
    if mshftmap(idet,isp) ge nvs then $
       mshftmap(idet,isp) = mshftmap(idet,isp) - nvs
    if mshftmap(idet,isp) lt 0 then $
       mshftmap(idet,isp) = mshftmap(idet,isp) + nvs


  endfor
  
endfor
    
lunar.mshftmap=mshftmap


save,filename='idl.save',lunar

;openw,lun,'lunar_27dec.dat',/get_lun
openw,lun,'temp.dat',/get_lun
  writeu,lun,lunar.recn
  writeu,lun,lunar.spinbl
  writeu,lun,lunar.timpb5
  writeu,lun,lunar.dphi
  writeu,lun,lunar.mshftmap
free_lun,lun

print,'lztim finished'

end



