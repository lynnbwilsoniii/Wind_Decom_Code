
;========================== main =============================
;reads and process a SWE lz file (RJF Jan95)

common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common log_delog,comp_tbl,dcomp_tbl
common phase,phiveis,theveis,phistrl,thestrl
common removedata,delete
common delset,delete_set

;goto, comp

;correct spin rates during eclipse from Chris Owen
restore,'/home/u3rjf/KO_info.idl'
w=where(phi ne 0,nw)    ;phi=phi required to correct calulated phi's
print,msecd(w(0))/3600,msecd(w(nw-1))/3600
msecd1=msecd(w(0))
msecd2=msecd(w(nw-1))
print,msecd1,msecd2

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
magfile=''

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
  decompress_tbl

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1 tm map of science and genl hk data offsets into lz.mf
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  
                    
;open LZ data file and read header
  openr,lundat,infile,/get_lun
  fh=read_lzhdr(lundat) ;fh=file header structure 

if magfile ne '' then begin
  ;open mag file
  openr,lunmfi,magfile,/get_lun

  ;get mfi 3sec mag field
  loadcdf,magfile,'Time_PB5',tpb5
  loadcdf,magfile,'B3GSE',bgse
endif

phasem1  ;get detector angles (in common phase)

;get spinperiod for file (reads file until two consecutive sci records read)
get_spinp,start=1

nfill=0l
spn=-1l
lastsec=0.d

k=-1
lunar=replicate({recn:0l,spinbl:0l,timpb5:lonarr(3),dphi:0.,$
  mshftmap:lonarr(6)},700)

;for recn=1,fh.nmf do begin  ;read entire file
for recn=1141,1229 do begin

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

      ;voltage steps: offsets into voltage table    
      vsteps=lz.mf((hkm1(24).offs + indgen(vsmjf.n_vesteps)))
  
      ;get energies and speed each voltage step
        energy=volt_en(vsteps,/en)   ;ev
        velocity=volt_en(vsteps,/vel)  ;cm/s
  
      ;compressed counts = vsmjf.veis  dim (det, vstep, sect, spin,)
      ;decompressed counts this spin
        cveis=dcomp_tbl(vsmjf.veis(*,*,*,spinbl))

      ;convert decompressed counts to f's
        fveis=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps,vsmjf.n_sectors,/nozero)
        for isect=0,vsmjf.n_sectors-1 do for idet=0,vsmjf.n_vdets-1 do $
         fveis(idet,*,isect)=$
          dcomp_tbl(vsmjf.veis(idet,*,isect,spinbl)) * cts_f(vsteps)
    
      if magfile ne '' then begin 
        ;get mfi 3sec mag field
        mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
        sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
        b=bgse(mindx,0:2,sindx)
        if b(0) eq rfill then nfill=nfill+1 
      endif 
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

lunar=lunar(0:k)
lunar.dphi=interpol(phi(w),msecd(w),lunar.timpb5(2)*1.d-3)

comp:

prt=0

;obtain background counts file for test purposes
    restorefile='glintest_941214_vlvl47.dat'
    restore,restorefile
    print,'restorefile ',restorefile
    help,avgcts,stdcts

nsp=n_elements(lunar)  
nvs=nsteps*nsects
step=intarr(nvs,ndets)
sect=intarr(nvs,ndets)

mshftmap=intarr(ndets,nsp)

for isp=0,nsp-1 do begin
;for isp=408,408 do begin
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
    mshftmap(idet,isp)=k1

    finetune=0
    if finetune eq 1 then begin
    ;************** fine tuning mshftmap *************************************
    ; the following change to mshftmap(idet) is made to the file
    mshftmap(idet,isp) = mshftmap(idet,isp) - 3
    if mshftmap(idet,isp) ge nvs then $
       mshftmap(idet,isp) = mshftmap(idet,isp) - nvs
    if mshftmap(idet,isp) lt 0 then $
       mshftmap(idet,isp) = mshftmap(idet,isp) + nvs
    endif

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

    goto,point2
    for i=0,nvs-1 do begin
      ;step(i)=i-nsteps*fix(i/nsteps)
      ;sect(i)=fix(i/nsteps)
      ;mshft=i+mshftmap(idet,isp)
      ;if mshft ge nvs then mshft=mshft-nvs
      ;step_shft(i,idet)=mshft-fix(mshft/nsteps)*nsteps
      ;sect_shft(i,idet)=fix(mshft/nsteps)
      ;phi_new(idet,step(i),sect(i))=$
        ;phi_true(idet,step_shft(i,idet),sect_shft(i,idet))
      ;print,'isp, idet, i ',isp, idet, i
      ;if prt eq 1 then $
        print,isp,idet,mshftmap(idet,isp),$
        i,mshft(i),step(i),sect(i),$
        vsmjf.phiveis(idet,step(i),sect(i)),$
        phi_true(idet,step(i),sect(i)),$
        avgcts(idet,step(i),sect(i)),$
        step_shft(i,idet),sect_shft(i,idet),phi_new(idet,step(i),sect(i)),$
        avgcts(idet,step_shft(i,idet),sect_shft(i,idet)),$
        format='(i3,i2,i3,i4,i4,7x,i3,i2,3f7.1,7x,i3,i2,2f7.1)
    endfor

    point2:     
    ;stop,'2' 

  endfor
  
endfor    

stop,'3'
lunar.mshftmap=mshftmap

save,filename='idl.save',lunar

openw,lun,'lunar_27dec.dat',/get_lun
  writeu,lun,lunar.recn
  writeu,lun,lunar.spinbl
  writeu,lun,lunar.timpb5
  writeu,lun,lunar.dphi
  writeu,lun,lunar.mshftmap
free_lun,lun




stop,'4'
rec=0l
spi=0l
tim=lonarr(3)
dph=0.
ksh=lonarr(6)
gli=lonarr(3,ngl)
openr,lun,'lunar_27dec.dat',/get_lun
for isp=0,nsp-1 do begin  
  readu,lun,rec,spi,tim,dph,ksh
endfor

print,'lztim finished'

end



