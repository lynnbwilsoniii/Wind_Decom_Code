

;======================= MAIN: lzpitch ======================================



;reads and process a SWE lz file (RJF Jan95)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common magstuff,magfile,tpb5,bgse
common wstuff,wst
common swestuff,swest
common shared,d

define_widgets
setpaths

;this version does not use spacecraft potenrial corrected data;
;spacecraft potential assumed zero

;-------------------define input parameters --------------------------------
version='v05'

printcheck2=0

special_labl=''  ;'test_'   ;'tog_'

oknoatt=0; if eq 1 then ok to not use attitude data; 180 deg rot about x used 
doswf=0;1   ;put pitch angle plots in frame of solar wind (using ion kp vel)

recs=[1,2000];whole file       ;[1,25]  ;test rel gains

lprnt=1
univgmask=0   ;if 1 then use universal glint mask
scpot=0    ;spacecraft potential =0

;--------------------------- end input parameters ----------------------------

print,'dates to be processed:'
openr,lundate,getenv('IDLSAV')+'lzpitch_dates',/get_lun
while not eof(lundate) do begin
date=''
readf,lundate,date
print,date
endwhile
free_lun,lundate

print,'processing options:'
print,'printcheck2 ',printcheck2
print,'special_labl ',special_labl
print,'oknoatt ',oknoatt
print,'scpot ',scpot
print,'doswf ',doswf
print,'record range ',recs
print,'univgmask ',univgmask

answ='' & print,'Hit return to continue, or any other key to stop.' 
read,answ & if answ ne '' then stop

;----------------------- begin ---------------------------------------------- 

lundate=3
close,lundate
openr,lundate,getenv('IDLSAV')+'lzpitch_dates';,/get_lun
idate=-1
while not eof(lundate) do begin
idate=idate+1
date=''
readf,lundate,date

;---initialize structures (d,wst,swest)
panelist
structuresw

;---reset all file units allocated by get_lun
for ilun=100,128 do free_lun,ilun
    
;---set background removal flag initially set to 'Yes' in structuresw.pro)
swest.subtrbkg='Yes'

wst.date_file='Date'    
wst.indate=date
wst.lzindate=wst.indate  
swest.univgmask=univgmask   ;if 1 then use universal glint mask
do_swf=doswf

point1:
if do_swf ne 0 then begin
  print,'Use ion KP''s to determine solar wind velocity' 

  ;------- Read SWE ion KP's for determining sc potential
  idatype=where(d.datype eq 'swe_ionkp') 
  input,idatype,err=err
  if err ne '' then begin
    print,err
    do_swf=0  ;stop
    goto,point1
  endif  
endif else print,' solar wind velocity will not be used'

ctmmode=['u','m','s','e']
ctmrate=['s','f']
elecion=['electrons','ions']


lpr=0
rfill=-1.0e31

;---read compress/decompress tables
decompress_tbl

;---get indices of instrument housekeeping into mjf array, lz.mf   
ihkmap,ihk 

;---get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
mode1map
mode6map
mode2map

;---get mode1 and mode2 sun phase angles of detectors, unit vectors
phasem1
phasem2


;---open LZ data file and read header, 
;   read mag, orb-att, background data, glint masks
;   and process first two records to get spin period
lzinput,err=err,oknoatt=oknoatt
if err ne '' then stop,err

;---get size of orb-att arrays
szpos=size(gse_pos)
szra=size(gse_ra)
szdec=size(gse_dec)
  
print,' '
print,'scimode ',vsmjf.scimode
print,'eleion_sweep ',vsmjf.eleion_sweep
print,' '

last_scimode=-1
last_atindx=-1

nfill=0l
espn=-1l
ispn=-1l

;---open electron pitchdata file
openw,elelun,getenv('IDLSAV')+'pitch.dat',/get_lun

;---open ion spectrum file
openw,ionlun,getenv('IDLSAV')+'ionspctrm.dat',/get_lun


;---initialize (nwglint set ge 0 and glint searched just once per date)
nwglint=-1

recn_range=[recs(0),recs(1)<fh.nmf]  ;[1,fh.nmf] ;read entire file

if date eq '19941212' then recn_range=[1,1225]
if date eq '19951217' then recn_range=[1,200]
if date eq '19960818' then recn_range=[1,1850]
if date eq '19961004' then recn_range=[1,500]
if date eq '19961005' then recn_range=[1,700]

recn1=recn_range(0)
recn2=recn_range(1)

print,'date, recn1,recn2 ',date, recn1,recn2

iondx=0

firstrecnout=1
 
for recn=recn1,recn2 do begin

  ;---process selected lz record (set keyword lpr=1 to print each record)
  proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,$
    elec_ion_m1=elec_ion_m1,err=err
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
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
    print,' '&print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
    goto,endrecordloop
  endif

  ;---check data quality; if one mnf flag set, then skip entire record
  n_mnf=250
  if total(lz.qlty(0:n_mnf-1)) gt 0 then  goto, endrecordloop 

  ;---check spinperiod
  if sp.spinp lt 0.9*3.05 or sp.spinp gt 1.1*3.05 then begin 
    print,'bad spinperiod'
    goto,endrecordloop
  endif

  ;---science mode
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  scimode1 = scimode_ihk eq 0 or scimode_ihk eq 1
  scimode2 = scimode_ihk eq 2 or scimode_ihk eq 11
  scimode4 = scimode_ihk eq 4
  scimode6 = scimode_ihk eq 6
  
  if not float(scimode1 or scimode2 or scimode4 or scimode6) then begin
    print,'not science mode 1 or science mode 2 or scimode4 or science mode 6'
    goto,endrecordloop
  endif 


  ;---- ready to process mode1 or mode2 electrons and ions  -----------------
  ;-----NOTE: (ion spectra not testd)
     
  ndets=vsmjf.n_vdets
  nvsteps=vsmjf.n_vesteps 
  nsectors=vsmjf.n_sectors
  nspins=vsmjf.n_spins

  ;---transform unit vectors from payload to gse
  atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index
  if scimode_ihk ne last_scimode or atindx ne last_atindx then begin  
    print,'recn, new atindx ',recn,atindx
    wc_gse=dblarr(3)
    vunit_gse=dblarr(ndets,nvsteps,nsectors,3)
    if atfile ne '' then begin
      if atindx le szra(1)-1 and atindx le szdec(1)-1 then begin
        for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
          payload_to_gse,$
          [vsmjf.vunit(i,j,k,0),vsmjf.vunit(i,j,k,1),vsmjf.vunit(i,j,k,2)],$
          [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
          vunit_gse(i,j,k,*)=wc_gse
        endfor
      endif
    endif else begin
      if lprnt then $
      print,'no attitude data; using 180 deg rotation about x-axis instead'
      lprnt=0     
      ;---(approx transform from payload to gse: SWE spin axis along -zgse)
      for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
        vunit_gse(i,j,k,0)= vsmjf.vunit(i,j,k,0)
        vunit_gse(i,j,k,1)=-vsmjf.vunit(i,j,k,1)
        vunit_gse(i,j,k,2)=-vsmjf.vunit(i,j,k,2) 
      endfor
    endelse
   
    rgse=fltarr(3)
    if orbfile ne '' then begin
      if atindx le szpos(1)-1 then rgse=gse_pos(atindx,*)/6373.
    endif     
  endif
  last_scimode=scimode_ihk
  last_atindx=atindx

  ;print,sp.mfrecn,sp.mfyr,sp.mfdy,sp.mfms,sp.spinp

  if nwglint eq -1 then begin
    wglint=where(vsmjf.xveis(*,*,*,0) eq -1,nwglint)
    print,' ' & print,'number of glint points = ',nwglint & print,' '
  endif

  ;---start spin loop
  for ispin=0,nspins-1 do begin

    if do_swf ne 0 then begin
      refsec=pb5_sec(ymd_pb5(long(date)))
      interpol_ion,$
      d.swe_ionkpdat.ta,d.swe_ionkpdat.v,iondx,vsmjf.suntim_vsbl(ispin),$
        refsec,vsw 
    endif else vsw=0
    ;print,recn,ispin,vsw,refsec
    
    timpb5=vsmjf.pb5tim_vsbl(*,ispin)

    ;---test for electron or ion mode, i.e., same specie every sector in given spin
    eleion=-1
    if scimode1 or scimode4 or scimode6 then begin
        if vsmjf.eleion(0) eq 0 then eleion=0
    endif else if scimode2 then begin
        if total(vsmjf.eleion(*,ispin)) eq 0 then eleion=0 $
        else if total(vsmjf.eleion(*,ispin)) eq nsectors then eleion=1
    endif 
    if eleion eq -1 then goto,endspinloop
        
    ;print,'recn, ispin, eleion ',recn, ispin, eleion

    ;---get mfi 3sec mag field    
    if magfile ne '' then begin        
      mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
      sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
      b=bgse(mindx,0:2,sindx)
      if b(0) eq rfill then goto,endspinloop 
    endif       

    ;---voltage steps: offsets into voltage table 
    ;   assumes all sectors electrons or ions
    cnvrt_to_f=fltarr(16)   
    if scimode1 or scimode4 then begin
      vsteps=vsmjf.veistep
      for j=0,15 do cnvrt_to_f(j)=total(vsmjf.cts_factor(*,j))/6 
    endif else if scimode6 then begin
      vsteps=reform(vsmjf.veistep(*,ispin))
      for j=0,15 do cnvrt_to_f(j)=total(vsmjf.cts_factor(*,j,ispin))/6  
    endif else if scimode2 then begin
      vsteps=reform(vsmjf.veistep(*,0,ispin))
      for j=0,15 do cnvrt_to_f(j)=total(vsmjf.cts_factor(*,j,0,ispin))/6
    endif
    
    ;---get energies and speed each voltage step for given specie
    energy=volt_en(vsteps,/en,ion=eleion)   ;ev
    velocity=double(volt_en(vsteps,/vel,ion=eleion))  ;cm/s 
      
    ;----NOTE: 
    ;   The input counts, vsmjf.cveis, are corrected for relative gain changes 
    ;   with time and for detector efficiency variation with energy,
    ;   and vsmjf.cveis_b have background subtracted
    ;   If background point should be larger than measured counts, 
    ;   then vsmjf.cveis_b is et to 0.
    ;   Glint points of the vsmjf.cveis array are negative their value.
    ;   Pitch angle and spectra data will have glint mask turned ON, 
    ;   i.e., glint points removed

    cblk=fltarr(ndets,nvsteps,nsectors)
    cblk(*,*,*)=vsmjf.cveis_b(*,*,*,ispin)


    ;---for mode2, put arrays in ascending velocity order
    ;   (this must always be the last array manipulation before doing moments)
    w=dblarr(ndets,nvsteps,nsectors,3)
    if scimode2 then begin
      sortv=sort(velocity)
      wsteps=vsteps(sortv)
      velocity=velocity(sortv)
      energy=energy(sortv)
      w(*,*,*,*)=vunit_gse(*,sortv,*,*)
      cblk(*,*,*)=cblk(*,sortv,*)
    endif else if scimode1 or scimode4 or scimode6 then begin
      wsteps=vsteps
      w=vunit_gse
    endif

    wp=dblarr(ndets,nvsteps,nsectors,3)
    mag=double(b)
    pa=dblarr(ndets,nvsteps,nsectors)
    for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
      wp(i,j,k,0)=velocity(j)*w(i,j,k,0) - vsw*1e5
      wp(i,j,k,1)=velocity(j)*w(i,j,k,1)
      wp(i,j,k,2)=velocity(j)*w(i,j,k,2)
      wpmag=sqrt(wp(i,j,k,0)^2+wp(i,j,k,1)^2+wp(i,j,k,2)^2)
      vpar=(wp(i,j,k,0)*mag(0)+wp(i,j,k,1)*mag(1)+wp(i,j,k,2)*mag(2))/$
            sqrt(total(mag*mag)) 
      pa(i,j,k)=float(acos(vpar/wpmag)/!dtor)
      ;print,j,energy(j),velocity(j)*w(i,j,k,0),vsw*1e5
    endfor
    ;stop    
    ;---set indices
    n_sectors=nsectors
    n_vdets=ndets


    ;---if electrons, then do pitch angle binning and energy spectrum
    ;---if ions, then do energy spectrum only
 
    case eleion of

    0: begin      ;do electrons       

      ;---initialize output electron pitch angle structure     
      pdata = {  $
        lzrec:0l,  $
        spinbl:0l,  $
        tjdsec_spinbl:0.d,  $
        pb5tim:lonarr(3),  $
        vsteps:bytarr(16),  $
        fspa:bytarr(16),   $
        f:bytarr(30,16),   $
        b:fltarr(3),$
        eleion:0l,  $
        misc:fltarr(9), $
        misc2:bytarr(8),$
        gains:fltarr(6),$
        cnvrt_to_f:fltarr(16),$
        rgse:fltarr(3) }
     
  
      ;------  do electron pitch angle binning
      
      ;---arrange data arrays according to energy step and pitch angle, 
      ;   nvsteps by   ndets*nsectors
      cp=reform(transpose(cblk,[1,0,2]) , [nvsteps,ndets*nsectors])
      pa=reform(transpose(pa,[1,0,2]) , [nvsteps,ndets*nsectors])
       
     ;---measured data will be put in pitch angle bins = dp degrees
     np=30 
     dp=180/np
     ip=fix(pa/dp)*dp
     
     ;---do binning 
     fbin=fltarr(np,nvsteps)
     favg=fltarr(nvsteps)
     for i=0,nvsteps-1 do begin
       sum=0. & nsum=0
       for jp=0,np-1 do begin
         wh=where(ip(i,*) eq jp*dp and cp(i,*) ge 0,nwh)
         if(nwh ne 0) then begin
           fbin(jp,i)=total(cp(i,wh))/nwh
           sum=sum+total(cp(i,wh))
           nsum=nsum+nwh
         endif
       endfor
       if nsum ne 0 then favg(i)=sum/nsum
    endfor

    ;---end doing electron pitch angle
   
    if lpr then $
      print,recn,ispin,vsmjf.tjd,vsmjf.sec,sec_pb5(vsmjf.suntim_vsbl(ispin))

    ;---make output electron data structure assignments
    pdata.lzrec=long(recn)
    pdata.spinbl=long(ispin)
    pdata.tjdsec_spinbl=vsmjf.suntim_vsbl(ispin)
    pdata.pb5tim=timpb5
    pdata.vsteps=wsteps
    pdata.fspa=comp_tbl(fix(favg+0.5))
    pdata.f=comp_tbl(fix(fbin+0.5))
    pdata.b=b
    pdata.eleion=eleion
    pdata.misc(0)=scpot
    pdata.misc(1)=vsw
    pdata.misc2(0)=nwglint
    pdata.misc2(1)=scimode_ihk
    pdata.gains=vsmjf.relgain
    pdata.cnvrt_to_f=cnvrt_to_f
    pdata.rgse=rgse
        
    ;print,'e'
    writeu,elelun,pdata       ;write electron output data
    espn=espn+1
      
    if firstrecnout then begin   ;save header info 
      hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknoatt:0l,$
            ndets:0l,nvsteps:0l,nsectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            max_enstep:0l,$
            thisrecn:0l,scimodechange:0l,dummy:0l}
      
      hedr.date=string(date,format='(a8)')
      hedr.scimode=scimode_ihk
      hedr.oknoatt=oknoatt
      hedr.ndets=vsmjf.n_vdets
      hedr.nvsteps=vsmjf.n_vesteps
      hedr.nsectors=vsmjf.n_sectors
      glnt_det_vel_sect,vsmjf.xveis(*,*,*,0),glnt
      hedr.glnt=glnt
      hedr.ensteptbl=volt_en(indgen(64),/en)
      hedr.max_enstep=volt_en(max(vsteps),/en)
      hedr.scimodechange=scimodechange
      
      firstrecnout=0
      
    endif 
    endcase    ;end electron procssing this spin

    1: begin   ;do ions
      ;---skip ions
      goto,endions

      ;---initialize ion spectrum output structure
      ionspctrmdata = {  $
        lzrec:0l,  $
        spinbl:0l,  $
        tjdsec_spinbl:0.d,  $
        pb5tim:lonarr(3),  $
        vsteps:bytarr(16),  $
        fspa:bytarr(16),   $
        b:fltarr(3),$
        eleion:0l,  $
        misc:fltarr(9)  }

      ;---do ion spectrum

      ;---------cblk=fltarr(ndets,nvsteps,nsectors)
      ;---------cblk_transpose=fltarr(nvsteps,ndets,nsectors)
      ;---------cblk_reform=fltarr(nvsteps,ndets*nsectors)
      cblk_reform=reform(transpose(cblk,[1,0,2]) , [nvsteps,ndets*nsectors])
      favg=fltarr(nvsteps)
      for j=0,nvsteps-1 do begin
        wh=where(cblk_reform(j,*) ge 0,nwh)
        if wh(0) ne -1 then favg(j)=total(cblk_reform(j,wh))/nwh
      endfor

      ;---make output ion data structure assignments
      ionspctrmdata.lzrec=long(recn)
      ionspctrmdata.spinbl=long(ispin)
      ionspctrmdata.tjdsec_spinbl=vsmjf.suntim_vsbl(ispin)
      ionspctrmdata.pb5tim=timpb5
      ionspctrmdata.vsteps=wsteps
      ionspctrmdata.fspa=comp_tbl(fix(favg+0.5))
      ionspctrmdata.b=b
      ionspctrmdata.eleion=eleion

      print,'i'
      writeu,ionlun,ionspctrmdata       ;write ion output data
      ispn=ispn+1
      endions:
      endcase    ;end ion procssing this spin
      endcase    ;end specie case

    endspinloop: 
    
  endfor  ;end spin loop
      

  skiprec=10
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)
  endif

  endrecordloop:

endfor     ;end record loop

endlzfile:

nrecele=long(espn+1)
nrecion=long(ispn+1)
free_lun,elelun
free_lun,ionlun


;---find current date
y=strmid(systime(0),20,4)
mos=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
m=string(where(mos eq strmid(systime(0),4,3))+1,format='(i2)')
d=strmid(systime(0),8,2)
thisdate=y*10000l+m*100l+d*1l
print,'thisdate ',thisdate
  
;---version id
mfltr=special_labl+'v5'
;if do_swf eq 1 then mfltr=special_labl+'v5' else mfltr=special_labl+'v4' 

if nrecele ne 0 then begin    ;yes electron data...make output file
  ;---create electron data header
  hedr.nrec=nrecele
  hedr.thisdate=string(thisdate,format='(i8)')
  openw,elelun,getenv('IDLSAV')+'ehedr',/get_lun
  writeu,elelun,hedr
  free_lun,elelun

  ;---concatenate electron header and pitch data files 
  hfl=getenv('IDLSAV')+'ehedr'
  dfl=getenv('IDLSAV')+'pitch.dat'
    
  ;version 4 (_v04) :
  ;     a) glint obtained by lzoppdet_diff 
  ;     b) no spacecraft potential correction made

  ;version 5 (_v05) uses a revised, fine-tuned glint map,
  ;                 adds mode and other information to output,
  ;                 a new expanded header is created,
  ;                 version 05 intended for distribution

  mfltr=special_labl+version
  mfl=getenv('MPNEW')+wst.lzdate+'_'+mfltr+'.pit'
  ;mfl=getenv('MPNEW')+wst.lzdate+'_'+mfltr+'.pitch'
  spawn,'cat ' + hfl + ' ' + dfl + ' > ' + mfl
  print,'electron pitch data and spectra file created: ',mfl       

  print,'lzpitch: electron pitch angles finished for date ',wst.lzdate

endif


if nrecion ne 0 then begin   ;yes ion data...make output file
  ;--create ion data header
  openw,ionlun,getenv('IDLSAV')+'ihedr',/get_lun
  writeu,ionlun,nrecion
  free_lun,elelun

  ;--concatenate ion header and ion spectrum data files 
  hfl=getenv('IDLSAV')+'ehedr'
  dfl=getenv('IDLSAV')+'ionspctrm.dat'
  mfl=getenv('MPNEW')+strmid(wst.lzdate,2,6)+mfltr+'.ionspctrm'
  spawn,'cat ' + hfl + ' ' + dfl + ' > ' + mfl
  print,'ion spectra data file created: ',mfl  
  
  print,'lzpitch: ion spectra finished for date ',wst.lzdate

endif

endwhile
free_lun,lundate

stop
end



