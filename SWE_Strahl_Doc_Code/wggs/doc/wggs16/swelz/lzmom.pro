


;======================= MAIN: lzmom =======================================



common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common backgrnd,avgcts_m1,avgcts_m2,bdate
common magstuff,magfile,tpb5,bgse
;common ionkpstuff,ionkpflnm,ionkpdat
common reldetgains,relgain,relgain_backg
common wstuff,wst
common swestuff,swest
common shared,d

define_widgets
setpaths
decompress_tbl ;read compress/decompress tables
close,2

;-------------- define input parameters for moments processing ---------------
recs=[1,2000]  ;  whole file[1,2000]

;---CURRENT VERSION 13 (_v13) uses idl version of patch,
;   skips det-step-sect in core patch specified by 0's in patch_include and 
;   then uses patched core values in moments,
;   array "use_patch" is added to output, and read by new "moments_read_v13.pro"
ver=13
version='v'+string(ver,format='(i2)')  
 
do_scpot=1    
;  do_scpot=1 then use ion density to compute scpot, thenrecompute moments 
;  do_scpot=0 then assume scpot=0
special_labl=''
sheath=0
oknomag=0  ;if 1 then ok not to use 3s field, but kpfield may be used
oknoatt=0; if eq 1 then ok to not use attitude data; 180 deg rot about x used 
helium=0.05  ;0.01   ;assumed helium concentration
lprnt=1
eclipse=0
norelgains=0
find_errs=1
trnc='tr974'   ;trnc='notrnc'         
;  default case: truncate counts spectra above energy = trnc ev 
univgmask=0 
;  if 0 then use default mask according to date  
;  if 1 then use universal glint mask

print,'dates to be processed:'
openr,lundate,getenv('IDLSAV')+'lzmom_dates',/get_lun
while not eof(lundate) do begin
date=''
readf,lundate,date
print,date
endwhile
free_lun,lundate

print,'processing options:'
print,'version ',ver
print,'norelgains ',norelgains
print,'sheath ',sheath
print,'eclipse ',eclipse
print,'do_scpot ',do_scpot
print,'special_labl ',special_labl
print,'oknoatt ',oknoatt
print,'oknomag ',oknomag
print,'record range ',recs
print,'trnc ',trnc
print,'univgmask ',univgmask

ctmmode=['u','m','s','e']
max_nsectors=8  
lpr=0
rfill=-1.0e31

answ='' & print,'Hit return to continue, or any other key to stop.' 
read,answ & if answ ne '' then stop

;-------------- end input parameters -----------------------------------------


;------------- begin date loop -----------------------------------------------
lundate=3
close,lundate
;---open date file
openr,lundate,getenv('IDLSAV')+'lzmom_dates'    
idate=-1
;---start date loop
while not eof(lundate) do begin     
idate=idate+1
date=''
readf,lundate,date


;---------------- begin initialization for the record loop this date ---------- 
for ilun=100,128 do free_lun,ilun 
;  reset all file units allocated by get_lun 

;---open momdata file
close,2
openw,2,getenv('IDLSAV')+'mom.dat'
   
;---initialize structures (d,wst,swest)
panelist
structuresw

;---initialize some quanities to be incremented in record loop
last_scimode=-1
last_atindx=-1
nfill=0l
spn=-1l
iondx=0
firstrecnout=1
nwglint=-1 ;nwglint will be set ge 0 and glint searched just once per date)

wst.date_file='Date'    
wst.indate=date 
wst.lzindate=wst.indate
swest.univgmask=univgmask 

if date eq '19941227' or date eq '19961113' then everyrec=1 else everyrec=0

;---read SWE ion KP's for determining sc potential
if do_scpot ne 0 then begin
  print,'Use ion KP''s to estimate sc potential' 
  idatype=where(d.datype eq 'swe_ionkp') 
  input,idatype,err=err
  if err ne '' then goto,enddateloop
endif else print,' sc potential will not be estimated'

swest.subtrbkg='Yes' 
;  background removal flag (initially set to 'Yes' in structuresw.pro)

;---Open LZ data file and read header,
;   get science, housekeeping telem maps, detector unit vectors all modes,
;   define swest.patch_include,
;   read mag, orb-att, background data, glint masks,
;   and process first two records to get spin period.
lzinput,err=err,oknoatt=oknoatt,sheath=sheath,oknomag=oknomag,/lz
if err ne '' then goto,enddateloop

;---get maximum electron step
max_elec_step=get_max_elec_step()

;---define record range this date
recn_range=[recs(0),recs(1)<fh.nmf]  ;if [1,fh.nmf] then read entire file
get_special_recn_range,date,eclipse,sheath,recn_range ;special cases
recn1=recn_range(0)
recn2=recn_range(1)

print,' '
print,'scimode ',vsmjf.scimode
print,'eleion_sweep ',vsmjf.eleion_sweep
print,'volt_en(max_elec_step,/en) ',volt_en(max_elec_step,/en)
if max_elec_step gt 38 and trnc eq 'tr974' then  trnc_label='tr974_' else $
if max_elec_step le 38 then  trnc_label=''  else trnc_label='notrnc'
print,'selected truncation: ',trnc
print,'first record truncation, trnc_label: ',trnc_label
print,'date, recn1,recn2 ',date, recn1,recn2

;------------- end initialization -------------------------------------------


;-------------- begin record loop -------------------------------------------- 
for recn=recn1,recn2 do begin

;---read and unpack from telemetry the selected lz record
proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,$
  elec_ion_m1=elec_ion_m1,err=err,norelgains=norelgains

;---do checks on mode, spinperiod, data quality flag  
;---test for mode change this date  
scimodechange=0  
if err ne '' then begin
  print,recn,'  ',err 
  if err eq $
  'change from mode1 to mode2; mode2 background and glint required' or $
  err eq $
  'change from mode2 to mode1; mode1 background and glint required' then begin
      scimodechange=1
      goto,endlzfile 
  endif else goto,endrecordloop
endif

;---determine whether in telemetry science mode  
if tmmode_ihk ne 2 then begin
  print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
  goto,endrecordloop
endif

;---check data quality; if one mnf flag set, then skip entire record
if total(lz.qlty(0:249)) gt 0 then  goto, endrecordloop 

;---check spinperiod
if sp.spinp lt 0.9*3.05 or sp.spinp gt 1.1*3.05 then begin 
  ;print,'bad spinperiod'
  ;goto,endrecordloop
endif

;---find which science mode
scimode=vsmjf.scimode
if scimode ne 1 and scimode ne 2 and scimode ne 4 and scimode ne 6 then begin  
  print,'not science mode 1 or science mode 2 or scimode 4 or science mode 6'
  goto,endrecordloop
endif 


;----checks completed; ready to process mode1, mode2, mode4, or mode6 electrons

     
ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps 
nsectors=vsmjf.n_sectors
nspins=vsmjf.n_spins
  
;---transform unit vectors from spacecraft to gse coords
atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index
if vsmjf.scimode ne last_scimode or atindx ne last_atindx or everyrec then $
  xfrm_unitvectors_sc_gse,atindx,vunit_gse,rgse,max_nsectors=max_nsectors
last_scimode=vsmjf.scimode
last_atindx=atindx

;---sun glint map will be searched just once per date
if nwglint eq -1 then begin
  wglint=where(vsmjf.xveis(*,*,*,0) eq -1,nwglint)
  print,' ' & print,'number of glint points = ',nwglint & print,' '
endif


;--------------- start spin loop ---------------------------------------------
for ispin=0,nspins-1 do begin
  ;---test for all-electron mode (electrons every sector in given spin)
  test_all_elecs,scimode,ispin,elemode      
  if elemode eq 0 then goto,endspinloop
      
  ;---do spacecraft potential correction
  ;   get interpolated ion density for spacecraft potential correction and also,
  ;   get ion temperature and velocity to be included with electron data  
  iondens=-1. & ionvel=-1. & iontemp=-1.
  if do_scpot ne 0 then $
    get_interpltd_swekp,date,ispin,iondx,helium,dens_cal,iondens,ionvel,iontemp

  ;---get pb5 time this spin          
  timpb5=vsmjf.pb5tim_vsbl(*,ispin)  
        
  ;---voltage steps: offsets into voltage table
  vsteps=get_vsteps(scimode,ispin)    
              
  ;---get energies and speed each voltage step
  energy=volt_en(vsteps,/en)   ;ev
  velocity=double(volt_en(vsteps,/vel))  ;cm/s 
    
  ;---prepare f and counts for computing moments, taking glint into account
  get_f_cts_lzmom,scimode,ispin,fblk,icnts_mb,ndets,nvsteps,nsectors,$
    max_nsectors=max_nsectors
     
  ;---for mode2, put arrays in ascending velocity order
  ;   (this must always be the last array manipulation before doing moments)
  if scimode eq 2 then begin
    sortv=sort(velocity)
    velocity=velocity(sortv)
    energy=energy(sortv)
    vunit=dblarr(ndets,nvsteps,max_nsectors,3)
    vunit(*,*,*,*)=vunit_gse(*,sortv,*,*)
    icnts_mb(*,*,*)=icnts_mb(*,sortv,*)
    fblk(*,*,*)=fblk(*,sortv,*)
  endif $
  else vunit=vunit_gse

  ;---compute patch coefficients, bnfit, for spacecraft potential = 0
  ;   (det, vstep, sector to be used in computing the core patch defined by '1'
  ;   elements of swest.patch_include(0:ndets-1,nvmin:lchan-1,0:nsectors-1));
  ;   nvmin = first energy step above potential 
  lchan=swest.lchan    ;low energy steps (6 or 8) to be used in fitting patch
  vpot=0.d
  lzpatch,fblk,icnts_mb,vunit,velocity,vpot,ndets,nvsteps,nsectors,$
    lchan,bnfit,chisqr,swest.patch_include,nvmin=nvmin,err=err
  if err ne '' then goto,endspinloop
    
  ;---get core fit density, temperature, flow velocity
  fcore,bnfit,ne_core,te_core,ue_core
     
  ;---put patched f values into the '0' elements of swest.patch_include
  for idet=0,ndets-1 do for jstep=nvmin,lchan-1 do for ksect=0,nsectors-1 $
  do begin
    if swest.patch_include(idet,jstep,ksect) eq 0 then begin
      wx=velocity(jstep)*vunit(idet,jstep,ksect,0)
      wy=velocity(jstep)*vunit(idet,jstep,ksect,1)
      wz=velocity(jstep)*vunit(idet,jstep,ksect,2)
      fblk(idet,jstep,ksect)=fintt(wx*1e-8,wy*1e-8,wz*1e-8,bnfit)
      ;print,idet,jstep,ksect,fblk(idet,jstep,ksect),$
      ;  fintt(wx*1e-8,wy*1e-8,wz*1e-8,bnfit)  
    endif  
  endfor


  ;------ prepare parameters for Adolfo's fortran moments integration ----
  
  ;---initialize output moments structure
    mdata = {  $
      lzrec:0l,  $
      spinbl:0l,  $
      tjdsec_spinbl:0.d,  $
      iflags:lonarr(8),  $
      gains:fltarr(6),  $
      misc:fltarr(20),  $
      v:fltarr(16),  $
      vpot:0.,  $
      dnout:0.,  $
      uout:fltarr(3),  $
      hout:fltarr(3),  $
      teout:0.,  $
      anistrpy:0.,  $
      gyrtrpy:0.,  $
      eavg:0.,  $
      paxis:fltarr(3),  $
      pout:fltarr(3,3),  $
      spcpot:0.,  $
      bf:fltarr(3),  $
      bnfit:fltarr(10),  $
      timpb5:lonarr(3),$
      misc2:bytarr(28),$
      rgse:fltarr(3)  }

  ;---set array indices for fortran module
  n_vdets=ndets
  n_vesteps_trunc=nvsteps
  n_sectors=nsectors
    
  ;---set input parameters controlling the moments calculation
  islop=1l & iprnt=2l & iscl=2l & iflow=1l & imeth=0l & nchop=0l 
  iflgs=lonarr(15) 
  ipatch=1l ;if 1 then use patch;   if 0 then do not use patch      
  itstec=lonarr(16);if eq 1 then steps skipped in fortran patch (not this vers) 
  iflgs(0)=islop & iflgs(1)=iprnt & iflgs(2)=iscl & iflgs(3)=iflow
  iflgs(4)=ipatch & iflgs(5)=imeth & iflgs(6)=nchop
  ldflag=long([1,1,1,1,1,1])
  ;  ldflag(i)=0 means det i not used in moments 
  ;  (ldflag can be set to 0 only in opposing detector pairs)
  
  ;---truncate above 974ev (moments only) due to sun glint effect on heat flux
  case trnc of  
      'notrnc' : truncstep=long(volt_en(max(vsteps),/en))+1  ;if no truncate 
      'tr974'  : truncstep=long(volt_en(38,/en))+1  ;if truncate above 974 ev
  endcase         
  wtrunc=where(energy le truncstep) 
  n_vesteps_trunc=n_elements(wtrunc) < nvsteps

  ;---initialize arrays for fortran moments module     
  fn=dblarr(max_nsectors) & fu=dblarr(max_nsectors,3) 
  fp=dblarr(max_nsectors,3,3) & fhf=dblarr(max_nsectors,3)
  fptrc=dblarr(max_nsectors) 
  dnout=0.d & uout=dblarr(3) & pout=dblarr(3,3) & hout=dblarr(3) 
  trace=0.d & eavg=0.d & anistrpy=0.d & gyrtrpy=0.d & tper=0.d & tpal=0.d
  paxis=dblarr(3) & spcpot=0.d & teout=0.d 
    
  ;---do moments with vpot=0 ------------------------------    
  file_so=getenv('WGGSBASE')+'swelz/swe_idl_new1.so' ;uses already computed bn
  ;file_so=getenv('WGGSBASE')+'swelz/swe_idl.so';this version computes bn 
  stats=$
    call_external(file_so,$
    'subpla_idl',$
    fblk, icnts_mb, vunit, $
    velocity,  n_vesteps_trunc, n_sectors, n_vdets,iflgs,ldflag,itstec, $
    vpot, fn, fu, fp, fhf, fptrc, dnout, uout, pout, hout, $
    trace, eavg, anistrpy, gyrtrpy, tper, tpal, paxis, spcpot, teout, bnfit)
    
  ;----------------- end doing moments vpot=0 ------------------------------
 
  ;----------- determine potential, vpot, and recompute moments -------------- 
  ratio=0
  if do_scpot ne 0 then begin
    dnout0=dnout 
    ratio=dnout/double(dens_cal)
    const=8.623625916d-5
    potvolts=const*te_core*alog(ratio)
    if ratio lt 1.d or potvolts lt 0 or finite(potvolts) eq 0 then goto,endmmnts
    vpot=sqrt(potvolts/2.85e-16) 
    ;---compute patch with new, estimated potential
    lzpatch,fblk,icnts_mb,vunit,velocity,vpot,ndets,nvsteps,nsectors,$
      lchan,bnfit,chisqr,swest.patch_include,nvmin=nvmin,err=err
    if err ne '' then goto,endspinloop
    
    ;---get core fit density, temperature, flow velocity
       fcore,bnfit,ne_core,te_core,ue_core
       
    ;---put patched f values into the '0' elements of swest.patch_include
    for idet=0,n_vdets-1 do for jstep=nvmin,lchan-1 do for ksect=0,n_sectors-1 $
    do begin
      if swest.patch_include(idet,jstep,ksect) eq 0 then begin
        wx=velocity(jstep)*vunit(idet,jstep,ksect,0)
        wy=velocity(jstep)*vunit(idet,jstep,ksect,1)
        wz=velocity(jstep)*vunit(idet,jstep,ksect,2)
        fblk(idet,jstep,ksect)=fintt(wx*1e-8,wy*1e-8,wz*1e-8,bnfit)
        ;print,idet,jstep,ksect,fblk(idet,jstep,ksect),$
        ;  fintt(wx*1e-8,wy*1e-8,wz*1e-8,bnfit)  
      endif  
    endfor  
    
    ;---do moments with estimated vpot ------------------------------  
    stats=$
      call_external(file_so,$
      'subpla_idl',$
      fblk, icnts_mb, vunit, $
      velocity,  n_vesteps_trunc, n_sectors, n_vdets,iflgs,ldflag,itstec, $
      vpot, fn, fu, fp, fhf, fptrc, dnout, uout, pout, hout, $
      trace, eavg, anistrpy, gyrtrpy, tper, tpal, paxis, spcpot, teout, bnfit)
   
    ;---- end final moments determination ----------------------
  endif
  endmmnts: 
  
  ;print,recn,ispin,timpb5(2)/1000,iondx+1,$
  ;    d.swe_ionkpdat(iondx+1).tpb5(2)/1000,d.swe_ionkpdat(iondx+1).n,$
  ;    iondens,dens_cal,dnout0,ratio,spcpot,ne_core,dnout,$
  ;    format='(i4,i2,i6,i4,i6,8f6.1)'
         
  ;---find errors in dnout and teout
  delta_nete=fltarr(2)
  if find_errs then delta_nete=moment_errors(fn,fp,n_sectors,dnout,teout)

  ;---get mfi 3sec mag field    
  if magfile ne '' then begin        
    mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
    sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
    b=bgse(mindx,0:2,sindx)
    if b(0) eq rfill then nfill=nfill+1 
  endif else b=rfill+fltarr(3)
  if lpr then $
    print,recn,ispin,vsmjf.tjd,vsmjf.sec,sec_pb5(vsmjf.suntim_vsbl(ispin))

  ;---make output data structure assignments
  mdata.misc(0)=  n_vesteps_trunc
  mdata.misc(1) = itstec(0)
  mdata.misc(2) = itstec(1)
  mdata.misc(3) = lz.mf(ihk(18).offs)  ;ebias1
  mdata.misc(4) = lz.mf(ihk(28).offs)  ;ebias2
  mdata.misc(5) = ratio                
  mdata.misc(6) = nwglint
  mdata.misc(7) = volt_en(max_elec_step,/en)
  mdata.misc(8) = delta_nete(0)/dnout
  mdata.misc(9) = delta_nete(1)/teout
  mdata.misc(10)= iondens
  mdata.misc(11)= ionvel
  mdata.misc(12)= iontemp
  mdata.misc(13)= ne_core
  mdata.misc(14)= te_core
  mdata.misc2(0:15)=vsteps  ;available energy steps in sweep table 
  mdata.misc2(16)=max_elec_step ;max energy step available
  mdata.misc2(17)=vsteps(n_vesteps_trunc-1) ;max energy step used
  mdata.misc2(18)=n_vdets    ;number of detectors
  mdata.misc2(19)=n_sectors  ;number of sectors 
  mdata.misc2(20)=scimode  ;science mode
  mdata.misc2(21)=ver          ;moments version
  mdata.misc2(22)=byte(100*helium)  ;percent of helium concentration assumed
  mdata.bf=b
  mdata.lzrec=long(recn)
  mdata.spinbl=long(ispin)
  mdata.tjdsec_spinbl=vsmjf.suntim_vsbl(ispin)
  mdata.timpb5=timpb5
  mdata.v=float(velocity)
  mdata.dnout = float(dnout)
  mdata.eavg = float(eavg)
  mdata.anistrpy = float(anistrpy)
  mdata.gyrtrpy = float(gyrtrpy)
  mdata.teout = float(teout)
  mdata.spcpot = float(spcpot)
  mdata.vpot = float(vpot)
  mdata.uout = float(uout)
  mdata.hout = float(hout)
  ;---normalize paxis 
  paxism=sqrt(total(paxis*paxis))
  mdata.paxis = float(paxis/paxism)
  mdata.pout = float(pout)
  mdata.bnfit = float(bnfit)
  mdata.iflags = iflgs(0:7)
  mdata.gains = vsmjf.relgain
  mdata.rgse=rgse

 ;---write temporary output data file       
  writeu,2,mdata       

  ;print,recn,ispin,timpb5,dnout,teout,uout
  
  ;---save header info 
  if firstrecnout then begin    
    hedr={nrec:0l,thisdate:string('',format='(i8)'),$
         date:string('',format='(a8)'),$
         scimode:0l,oknomag:0l,oknoatt:0l,eclipse:0l,norelgains:0l,$
         ldflag:lonarr(6), n_vdets:0l,n_vesteps:0l,n_sectors:0l,$
         glnt:lonarr(3,64),ensteptbl:fltarr(64),$
         trnc:string('',format='(a8)'),trunc_enstep:0l,max_enstep:0l,$
         sheath:0l,do_scpot:0l,wchglntmask:string('',format='(a8)'),$
         thisrecn:0l,scimodechange:0l,dummy:0l,use_patch:lonarr(6,10,6)}
      
    case do_scpot of
      1 : scplbl=''
      0 : scplbl='0pot_'
    endcase
        
    hedr.date=string(date,format='(a8)')
    hedr.scimode=scimode
    hedr.oknomag=oknomag
    hedr.oknoatt=oknoatt
    hedr.eclipse=eclipse
    hedr.norelgains=norelgains
    hedr.ldflag=ldflag
    hedr.n_vdets=vsmjf.n_vdets
    hedr.n_vesteps=vsmjf.n_vesteps
    hedr.n_sectors=vsmjf.n_sectors
    glnt_det_vel_sect,vsmjf.xveis(*,*,*,0),glnt
    hedr.glnt=glnt
    hedr.ensteptbl=volt_en(indgen(64),/en)
    hedr.trnc=string(trnc,format='(a8)')
    hedr.trunc_enstep=truncstep
    hedr.max_enstep=volt_en(max(vsteps),/en)
    hedr.do_scpot=do_scpot
    if swest.univgmask then hedr.wchglntmask='universl' else $
      hedr.wchglntmask=bdate
    hedr.sheath=sheath
    hedr.scimodechange=scimodechange
    hedr.thisrecn=recn
      
    use_patch=lonarr(6,10,6)
    wpatch=where(swest.patch_include(*,0:9,0:5) eq 0)
    if wpatch(0) ne -1 then use_patch(wpatch)=1
    hedr.use_patch=use_patch
    firstrecnout=0
  endif
  spn=spn+1
  endspinloop: 
  if err ne '' then print,err  
  endfor  
  ;---end spin loop    

  skiprec=10
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)
     print,spn, mdata.dnout,mdata.teout
  endif

  endrecordloop:
  if err ne '' then print,err
endfor     
;---end record loop

endlzfile:
if err ne '' then print,err

;---define final number of output records (= total number of spins)
nrec=long(spn+1)
hedr.nrec=nrec

;---find current date
y=strmid(systime(0),20,4)
mos=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
m=string(where(mos eq strmid(systime(0),4,3))+1,format='(i2)')
d=strmid(systime(0),8,2)
thisdate=y*10000l+m*100l+d*1l
print,'thisdate ',thisdate
hedr.thisdate=string(thisdate,format='(i8)')

;---write temporary header file
close,2    
openw,2,getenv('IDLSAV')+'hedr'
writeu,2,hedr
close,2

;---concatenate header and moments data files into the permanent moments file 
hfl=getenv('IDLSAV')+'hedr'
dfl=getenv('IDLSAV')+'mom.dat'

 
mfltr=special_labl+version
mfl=getenv('MPNEW')+wst.lzdate+'_'+mfltr+'.mom'
spawn,'cat ' + hfl + ' ' + dfl + ' > ' + mfl
print,'moments data file created: ',mfl       

print,'lzmom finished for date ',wst.lzdate

enddateloop:
if err ne '' then print,err 
close,2
endwhile
;----------------- end date loop -----------------------------------------------

free_lun,lundate

;---------- OLD VERSION notes    
;version 4 (_v04) :
;     a) glint obtained by lzoppdet_diff 
;     c) skip lowest 2 energy channels
;     d) lchan=6 patch fit points

;version 5 (_v05) :  adds spacecraft potential determination to version 4

;version 06 (_v06) corrects spacecraft potential error,
;   scpot=0 case indicated by filename modification '0pot_'

;version 07 (_v07) uses a new single, universal glint map, 
;                  adds mode and pb5 time and other information to output,
;                  a new expanded header is created
;NOTE!! version 07 suspended deu to universal glint map found to be in error

;version 08 (_v08) corrects the glint map

;version 09 (_v09) uses two lowst energy steps in patch which had been 
; skipped in previous versions

;version 12 (_v12) after 19981026 , skips detectors 0 and 2 in patch and 
; then uses patched values in second iter
; v12 uses idl version of patch  



end



