

;======================= MAIN: lzmom =========================================



;reads and process a SWE lz file (RJF Jan95)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common magstuff,magfile,tpb5,bgse
common ionkpstuff,ionkpflnm,ionkpdat
common wstuff,wst
common swestuff,swest
common shared,d


;<<<< !!!! modified to simulate non-spinning s/c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;this version estimates spacecraft potential given an independently determined 
;electron density, then computes electron moments using the estimated potential



version='v06'
;<<<<<<<<<<<<<<<<<
;if do_scpot=1 then use ion density to compute scpot and recompute moments (def)

;if do_scpot=0 then assume scpot=0

;if do_scpot=-1 then multiply fblk by ratio=(ion density / elec density)
;      account for temporary detector gain degradation between 97nov and 98may,
;      and assume scpot=0

do_scpot=0  ;-1 
;>>>>>>>>>>>>>>>>>  
sheath=0
oknomag=0  ;if 1 then nomag field used
oknoatt=0; if eq 1 then ok to not use attitude data; 180 deg rot about x used 

recs= [1,275  ];2000];whole file[1,2000]  
special_labl='nospin_'                  ;'special1_'
  
lprnt=1
eclipse=0
printcheck2=0
norelgains=0
find_errs=1
;ldflag(i)=0 means det i not used in moments 
;(ldflag can be set to 0 only in opposing detector pairs)
ldflag=long([1,1,1,1,1,1])    

print,'dates to be processed:'
openr,lundate,getenv('IDLSAV')+'lzmom_dates',/get_lun
while not eof(lundate) do begin
date=''
readf,lundate,date
print,date
endwhile
free_lun,lundate

refsec=pb5_sec(ymd_pb5(19000000l+long(date)))

print,'processing options:'
print,'printcheck2 ',printcheck2
print,'norelgains ',norelgains
print,'sheath ',sheath
print,'eclipse ',eclipse
print,'do_scpot ',do_scpot
print,'special_labl ',special_labl
print,'oknoatt ',oknoatt
print,'oknomag ',oknomag
print,'record range ',recs
answ='' & print,'Hit return to continue, or any other key to stop.' 
read,answ & if answ ne '' then stop

;----------------------- begin ------------------------------------------------- 
lundate=3
close,lundate
openr,lundate,getenv('IDLSAV')+'lzmom_dates'    ;,/get_lun
idate=-1
while not eof(lundate) do begin
idate=idate+1
date=''
readf,lundate,date

;reset all file units allocated by get_lun
  for ilun=100,128 do free_lun,ilun
  
;initialize structures
  structuresw
  panelist

wst.date_file='Date'    
wst.indate=date 


if do_scpot ne 0 then begin
  print,'Use ion KP''s to estimate sc potential, or to modify absolute gains' 

;------- Read SWE ion KP's for determining sc potential
  input,4,err=err
  if err ne '' then stop
endif else print,' sc potential will not be estimated'

;set background removal flag (it is initially set to 'Yes' in structuresw.pro)
  swest.subtrbkg='Yes' 

ctmmode=['u','m','s','e']
ctmrate=['s','f']
elecion=['electrons','ions']

max_nsectors=8  

;default case: truncate counts spectra above energy = trnc ev 
trnc='tr974'         ;trnc='tr507'

lpr=0
rfill=-1.0e31

;read compress/decompress tables
  decompress_tbl

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
   mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc
   mode2map,hkind,ghk,vblhsp,sblhsp

;get mode1 and mode2 sun phase angles of detectors, unit vectors

;<<<< !!!! modified to simulate non-spinning s/c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

   phasem1_nospin
   phasem2

;open LZ data file and read header, 
;  read mag, orb-att, background data, glint masks
;  and process first two records to get spin period
  lzinput,err=err,oknoatt=oknoatt,sheath=sheath,/noorb,oknomag=oknomag
  if err ne '' then stop,err

print,' '
if sheath then print,'sheath with modified glint mask'
print,'scimode ',vsmjf.scimode
print,'eleion_sweep ',vsmjf.eleion_sweep

if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 and $
   (vsmjf.eleion_sweep eq 2 or vsmjf.eleion_sweep eq 3) then begin
  sz=size(vsmjf.veistep)
  elecstep=intarr(sz(1),sz(2)*sz(3))
  elecstep(*,*)=vsmjf.veistep
  welecstep=where(vsmjf.eleion eq 0)
  max_elec_step=max(elecstep(*,welecstep))
endif else max_elec_step=max(vsmjf.veistep)
print,'volt_en(max_elec_step,/en) ',volt_en(max_elec_step,/en)
if max_elec_step gt 38 and trnc eq 'tr974' then  trnc_label='tr974_' else $
if max_elec_step le 38 then  trnc_label=''  ;'notrnc'

print,'selected truncation: ',trnc
print,'first record truncation, trnc_label: ',trnc_label

print,' '

last_scimode=-1
last_atindx=-1
if date eq '941227' or date eq '961113' then everyrec=1 else everyrec=0

nfill=0l
spn=-1l
lastsec=0.d

;open momdata file
  close,1
  openw,1,getenv('IDLSAV')+'mom.dat'

;initialize (nwglint will be set ge 0 and glint searched just once per date)
nwglint=-1

recn_range=[recs(0),recs(1)<fh.nmf]  ;[1,fh.nmf] ;read entire file

if date eq '941212' then recn_range=[1,1225]
if eclipse and date eq '941227' then recn_range=[1141,1229]   ;27dec94 eclipse
if sheath then begin
  if date eq '941130' then recn_range=[1259,1777]  ;sheath
  if date eq '941201' then recn_range=[518,1023]   ;sheath
  if date eq '941212' then recn_range=[667,1222]   ;sheath
  if date eq '941213' then recn_range=[1,626]      ;sheath
  if date eq '941224' then recn_range=[626,1045]   ;sheath
  if date eq '941225' then recn_range=[1,663]      ;sheath
endif
if date eq '961223' then recn_range=[1100,fh.nmf]
;if date eq '961004' then recn_range=[1,500]
;if date eq '961005' then recn_range=[1,700]

recn1=recn_range(0)
recn2=recn_range(1)

print,'date, recn1,recn2 ',date, recn1,recn2
stop
iondx=0
 
for recn=recn1,recn2 do begin

;if date eq '971012' and recn gt 450 and recn lt 460 then goto,endrecordloop

;process selected lz record (set keyword lpr=1 to turn on print each record)
  proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,$
    elec_ion_m1=elec_ion_m1,err=err,norelgains=norelgains
  if err ne '' then begin
     print,recn,'  ',err 
     if err eq $
       'change from mode1 to mode2; mode2 background and glint required' or $
        err eq $
       'change from mode2 to mode1; mode1 background and glint required' then $
       goto,endlzfile $
       else goto,endrecordloop
  endif

;determine whether in telemetry science mode
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  
  if tmmode_ihk ne 2 then begin
    print,' '&print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
    goto,endrecordloop
  endif

;check data quality; if one mnf flag set, then skip entire record
  n_mnf=250
  if total(lz.qlty(0:n_mnf-1)) gt 0 then  goto, endrecordloop 

;check spinperiod
  if sp.spinp lt 0.9*3.05 or sp.spinp gt 1.1*3.05 then begin 
    print,'bad spinperiod'
    goto,endrecordloop
  endif

;science mode
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  scimode1 = scimode_ihk eq 0 or scimode_ihk eq 1
  scimode2 = scimode_ihk eq 2 or scimode_ihk eq 11
  scimode4 = scimode_ihk eq 4
  
  if not float(scimode1 or scimode2 or scimode4) then begin
    print,'not science mode 1 or science mode 2'
    goto,endrecordloop
  endif 


;---- ready to process mode1 or mode2 electrons ---------------------------
     
ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps 
nsectors=vsmjf.n_sectors
nspins=vsmjf.n_spins
  
;transform unit vectors from payload to gse
atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index
if scimode_ihk ne last_scimode or atindx ne last_atindx or everyrec then begin 
   print,'recn, new atindx ',recn,atindx  
   wc_gse=dblarr(3)
   vunit_gse=dblarr(ndets,nvsteps,max_nsectors,3)
   if atfile ne '' then begin
     for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
       payload_to_gse,$
       [vsmjf.vunit(i,j,k,0),vsmjf.vunit(i,j,k,1),vsmjf.vunit(i,j,k,2)],$
       [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
       vunit_gse(i,j,k,*)=wc_gse
     endfor
   endif else begin
     if lprnt then $
     print,'no attitude data; using 180 deg rotation about x-axis instead'
     lprnt=0     
     ;(approx transform from payload to gse: SWE spin axis along -zgse)
     for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
       vunit_gse(i,j,k,0)= vsmjf.vunit(i,j,k,0)
       vunit_gse(i,j,k,1)=-vsmjf.vunit(i,j,k,1)
       vunit_gse(i,j,k,2)=-vsmjf.vunit(i,j,k,2)
     endfor 
   endelse
endif
last_scimode=scimode_ihk
last_atindx=atindx

;print,sp.mfrecn,sp.mfyr,sp.mfdy,sp.mfms,sp.spinp

if nwglint eq -1 then begin
  wglint=where(vsmjf.xveis(*,*,*,0) eq -1,nwglint)
  print,' ' & print,'number of glint points = ',nwglint & print,' '
endif


;--------------- start spin loop
  for ispin=0,nspins-1 do begin

    ;test for all electron mode (every sector in given spin)
      elemode=0
      if scimode1 or scimode4 then begin
        if vsmjf.eleion(0) eq 0 then elemode=1
      endif
      if scimode2 then begin
        if total(vsmjf.eleion(*,ispin)) eq 0 then elemode=1
      endif      
      if elemode eq 0 then goto,endspinloop
      ;print,'not in all electron mode,  ispin ',ispin
        
    ;print,'recn, ispin, elemode ',recn, ispin, elemode

    ;do spacecraft potential correction
    if do_scpot ne 0 then begin
      interpol_ion,$
      ionkpdat.ta,ionkpdat.n,iondx,vsmjf.suntim_vsbl(ispin),refsec,dens_cal
      ;ion densities are increased by 5% to estimate helium contribution
        if do_scpot eq 1 then dens_cal=1.05*dens_cal
    endif
    
    spn=spn+1
    timpb5=vsmjf.pb5tim_vsbl(*,ispin)  

        
    ;----------------- begin doing moments ---------------------------------

    ;initialize output moments structure
    mdata = {  $
      lzrec:0l,  $
      spinbl:0l,  $
      tjdsec_spinbl:0.d,  $
      iflags:lonarr(8),  $
      gains:fltarr(6),  $
      misc:fltarr(10),  $
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
      bnfit:fltarr(10)  }


  ;voltage steps: offsets into voltage table    
    if scimode1 or scimode4 then vsteps=vsmjf.veistep
    ;assumes all sectors electrons
    if scimode2 then vsteps=reform(vsmjf.veistep(*,0,ispin))  
    n_vesteps_trunc=nvsteps
          
  ;get energies and speed each voltage step
    energy=volt_en(vsteps,/en)   ;ev
    velocity=double(volt_en(vsteps,/vel))  ;cm/s 
  
  ;NOTE: 
  ;the input counts and computed f's will have a floor of 1/2 count, and
  ;glint points (counts anfd f's) are negative their value
  ;
  ;in computing moments, glint points will be set to f of 1/2 count; however,
  ;counts, which are used only for weighting patch and must be long integer,
  ;both glint and the floor, are set to 1 count

    icnts_mb=lonarr(ndets,nvsteps,max_nsectors)
    icnts_mb(*,*,0:nsectors-1)=long(vsmjf.cveis(*,*,*,ispin)) > 1l

    fblk=dblarr(ndets,nvsteps,max_nsectors)
    fblk(*,*,0:nsectors-1)=double(vsmjf.fveis(*,*,*,ispin))

    for k=0,nsectors-1 do for i=0,ndets-1 do begin
      w_glnt_zero=where(fblk(i,*,k) le 0)
      if w_glnt_zero(0) ne -1 then begin
        if scimode1 or scimode4 then fblk(i,w_glnt_zero,k)=$
          0.5 * double(vsmjf.cts_factor(i,w_glnt_zero))
        if scimode2 then fblk(i,w_glnt_zero,k)=$
          0.5 * double(vsmjf.cts_factor(i,w_glnt_zero,k,ispin))   
      endif
    endfor


    ;for mode2, put arrays in ascending velocity order
    ;(this must always be the last array manipulation before doing moments)
      if scimode2 then begin
        sortv=sort(velocity)
        velocity=velocity(sortv)
        energy=energy(sortv)
        vunit=dblarr(ndets,nvsteps,max_nsectors,3)
        vunit(*,*,*,*)=vunit_gse(*,sortv,*,*)
        icnts_mb(*,*,*)=icnts_mb(*,sortv,*)
        fblk(*,*,*)=fblk(*,sortv,*)
      endif 

    if scimode1 or scimode4 then vunit=vunit_gse

    ;set indices
      n_sectors=nsectors
      n_vdets=ndets

    ;set input parameters controlling the moments calculation
      islop=1l & iprnt=2l & iscl=2l & iflow=1l & ipatch=1l 
      imeth=0l & nchop=0l 
      iflgs=lonarr(10)      
      itstec=lonarr(16) 
      
      lchan=6l  ;7l      ;low energy steps to be used in fitting patch     
      itstec(0)=1   ;skip and patch over lowest energy step
      itstec(1)=1   ;skip and patch over second lowest energy step
      ;itstec(2)=1
      
      iflgs(0)=islop;
      iflgs(1)=iprnt;
      iflgs(2)=iscl;
      iflgs(3)=iflow;
      iflgs(4)=ipatch;
      iflgs(5)=imeth;
      iflgs(6)=nchop;
      iflgs(7)=lchan;

    
    case trnc of  
      'notrnc' : truncstep=long(volt_en(max(vsteps),/en))+1  ;if no truncate 
      'tr507'  : truncstep=long(volt_en(33,/en))+1  ;if truncate above 507 ev
      'tr974'  : truncstep=long(volt_en(38,/en))+1  ;if truncate above 974 ev
    endcase         
    wtrunc=where(energy le truncstep) 
    n_vesteps_trunc=n_elements(wtrunc) < nvsteps
     
    vpot=0.d
    fn=dblarr(max_nsectors) & fu=dblarr(max_nsectors,3) 
    fp=dblarr(max_nsectors,3,3) & fhf=dblarr(max_nsectors,3)
    fptrc=dblarr(max_nsectors) 
    dnout=0.d & uout=dblarr(3) & pout=dblarr(3,3) & hout=dblarr(3) 
    trace=0.d & eavg=0.d & anistrpy=0.d & gyrtrpy=0.d & tper=0.d & tpal=0.d
    paxis=dblarr(3) & spcpot=0.d & teout=0.d & bnfit=dblarr(10)


    if printcheck2 and recn eq 2 then begin
      openw,2,'print_file0_idl'
      printf,2,vsmjf.tjd,vsmjf.sec
      for i=0,nsectors-1 do for k=0,nvsteps-1 do for j=0,ndets-1 do printf,2,$
        i,k,j,1./vsmjf.cts_factor(j,k),vsmjf.cveis(j,k,i,ispin),fblk(j,k,i)
      for k=0,nvsteps-1 do printf,2,k,velocity(k)
      for i=0,nsectors-1 do for k=0,nvsteps-1 do for j=0,ndets-1 do $
        printf,2,i,k,j,vunit(j,k,i,0),vunit(j,k,i,1),vunit(j,k,i,2)
      close,2
    endif

    file_so=getenv('WGGSBASE')+'swe/lz/swe_idl_det.so'
    stats=$
         call_external(file_so,$
         'subpla_idl',$
         fblk, icnts_mb, vunit, $
         velocity,  n_vesteps_trunc, n_sectors, n_vdets,iflgs,ldflag,itstec, $
         vpot, fn, fu, fp, fhf, fptrc, dnout, uout, pout, hout, $
         trace, eavg, anistrpy, gyrtrpy, tper, tpal, paxis, spcpot, $
         teout,bnfit)

    ;----------------- end doing moments vpot=0 ------------------------------
ratio=0
if do_scpot ne 0 then begin
    ;---------------- determine potential and recompute moments --------------
      fcore,bnfit,dne_cld,te_cld,u_core 
      ratio=dnout/double(dens_cal)
      case do_scpot of
      1 : begin 
            const=8.623625916d-5
            potvolts=const*te_cld*alog(ratio)
            if ratio lt 1.d then goto,skip2
            vpot=sqrt(potvolts/2.85e-16)          
          endcase 
     -1 : begin
           ;multiply fblk by ratio ion density / elec density (assumed > 1) 
           ;and recompute in order to bring elec density up to ion density       
           fblk=fblk/ratio
          endcase 
      endcase    
            
      stats=$
         call_external(file_so,$
         'subpla_idl',$
         fblk, icnts_mb, vunit, $
         velocity,  n_vesteps_trunc, n_sectors, n_vdets,iflgs,ldflag,itstec, $
         vpot, fn, fu, fp, fhf, fptrc, dnout, uout, pout, hout, $
         trace, eavg, anistrpy, gyrtrpy, tper, tpal, paxis, spcpot, $
         teout,bnfit)

    ;---- end potential and final moments determination ----------------------
endif

skip2:

    ;find errors in dnout and teout
    delta_nete=fltarr(2)
    if find_errs then delta_nete=moment_errors(fn,fp,n_sectors,dnout,teout)

    ;get mfi 3sec mag field    
      if magfile ne '' then begin        
        mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
        sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
        b=bgse(mindx,0:2,sindx)
        if b(0) eq rfill then nfill=nfill+1 
      endif 
      lastsec=vsmjf.sec

    if lpr then $
        print,recn,ispin,vsmjf.tjd,vsmjf.sec,sec_pb5(vsmjf.suntim_vsbl(ispin))


    ;make output data structure assignments
      mdata.misc(0)=n_vesteps_trunc
      mdata.misc(1) = itstec(0)
      mdata.misc(2) = itstec(1)
      mdata.misc(3) = lz.mf(ihk(18).offs)  ;ebias1
      mdata.misc(4) = lz.mf(ihk(28).offs)  ;ebias2
      mdata.misc(5) = ratio                ;truncstep;truncate above truncstep
      mdata.misc(6) = nwglint
      mdata.misc(7) = volt_en(max_elec_step,/en)
      mdata.misc(8) = delta_nete(0)/dnout
      mdata.misc(9) = delta_nete(1)/teout
      
      mdata.bf=b
      mdata.lzrec=long(recn)
      mdata.spinbl=long(ispin)
      mdata.tjdsec_spinbl=vsmjf.suntim_vsbl(ispin)
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

      ;normalize paxis 
      paxism=sqrt(total(paxis*paxis))

      mdata.paxis = float(paxis/paxism)
      mdata.pout = float(pout)
      mdata.bnfit = float(bnfit)
      mdata.iflags = iflgs(0:7)
      mdata.gains = vsmjf.relgain

    ;print,recn, spn, mdata.dnout,mdata.teout

    writeu,1,mdata       ;write output data

    endspinloop: 
    if printcheck2 and recn eq 2 then stop
    
  endfor  ;end spin loop
      

  skiprec=10
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)
     print,spn, mdata.dnout,mdata.teout
  endif

  endrecordloop:

endfor     ;end record loop

endlzfile:
nrec=long(spn+1)
close,1

;find current date
  y=strmid(systime(0),20,4)
  mos=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  m=string(where(mos eq strmid(systime(0),4,3))+1,format='(i2)')
  d=strmid(systime(0),8,2)
  thisdate=y*10000l+m*100l+d*1l
  print,'thisdate ',thisdate
;create header
  openw,1,getenv('IDLSAV')+'hedr'
  writeu,1,nrec,thisdate
  close,1

;concatenate header and moments data files 
  hfl=getenv('IDLSAV')+'hedr'
  dfl=getenv('IDLSAV')+'mom.dat'

;version 4 (_v04) :
;     a) glint obtained by lzoppdet_diff 
;     c) skip lowest 2 energy channels
;     d) lchan=6 patch fit points

;version 5 (_v05) :  adds spacecraft potential determination to version 4

;version=(_v06) current version corrects spacecraft potential error,
;               scpot=0 case indicated by filename modification '0pot_'

  case do_scpot of
   1 : scplbl=''
   0 : scplbl='0pot_'
  -1 : scplbl='0potmod_'
  endcase
  
  if sheath then shthlbl=string(nwglint,format='(i2)')+'gl_sheath_' $
  else shthlbl=''
  mfltr=trnc_label+shthlbl+special_labl+scplbl+version
  
  mfl=getenv('MPNEW')+strmid(wst.lzdate,2,6)+'_'+mfltr+'.mom'
  spawn,'cat ' + hfl + ' ' + dfl + ' > ' + mfl
  print,'moments data file created: ',mfl       

  
print,'lzmom finished for date ',wst.lzdate

close,1
endwhile

free_lun,lundate

end



