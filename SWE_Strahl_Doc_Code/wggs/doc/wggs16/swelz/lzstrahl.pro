

;======================= MAIN: lzstrahl =======================================



;reads and process a SWE lz file (RJF Jan95)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common magstuff,magfile,tpb5,bgse
common wstuff,wst
common swestuff,swest

printcheck2=0

define_widgets
setpaths

for ilun=100,120 do free_lun,ilun


print,'dates to be processed:'
dates=strarr(300)
ida=-1
openr,lundate,getenv('IDLSAV')+'lzstrahl_dates',/get_lun
while not eof(lundate) do begin
date=''
readf,lundate,date
print,date
ida=ida+1
dates(ida)=date
endwhile
free_lun,lundate
dates=dates(0:ida)

answ='' & print,'Hit return to continue, or any other key to stop.' 
read,answ & if answ ne '' then stop

;----------------------- begin ------------------------------------------------- 
openr,lundate,getenv('IDLSAV')+'lzstrahl_dates',/get_lun
for idate=0,n_elements(dates)-1 do begin
date=dates(idate)

oknoatt=0; if eq 1 then ok to not use attitude data; 180 deg rot about x used 
lprnt=1

;---initialize structures
panelist
structuresw

;---set background removal flag (it is initially set to 'Yes' in structuresw.pro)
swest.subtrbkg='No'

wst.date_file='Date'    
wst.indate=date  
wst.lzindate=wst.indate

ctmmode=['u','m','s','e']
ctmrate=['s','f']

lpr=0
rfill=-1.0e31

;---read compress/decompress tables
decompress_tbl

;---get indices of instrument housekeeping into mjf array, lz.mf   
ihkmap,ihk 

;---get tm map of science and genl hk data offsets into lz.mf
mode1map;,hkm1,fcblm1,vsm1,vdatc,sdatc
mode2map;,hkind,ghk,vblhsp,sblhsp
mode6map
   
;---get mode1 and mode2 sun phase angles of detectors, unit vectors
phasem1
phasem2

;---open LZ data file and read header, 
;   read mag, orb-att, background data, glint masks
;   and process first two records to get spin period
lzinput,err=err,oknoatt=oknoatt
if err ne '' then stop,err

print,' '
print,'scimode ',vsmjf.scimode
print,'eleion_sweep ',vsmjf.eleion_sweep
print,' '

last_scimode=-1
last_atindx=-1

nfill=0l
espn=-1l

;---open electron strahldata file
openw,elelun,getenv('IDLSAV')+'strahl.dat',/get_lun

recn_range=[1,fh.nmf] ;read entire file
;recn_range=[1,200]

recn1=recn_range(0)
recn2=recn_range(1)

print,'date, recn1,recn2 ',date, recn1,recn2
 
for recn=recn1,recn2 do begin

;---process selected lz record (set keyword lpr=1 to turn on print each record)
proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,$
    elec_ion_m1=elec_ion_m1,err=err
if err ne '' then goto,endrecordloop

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


;-------------- ready to process strahl data -----------------------------
     
nstrdets=vsmjf.n_strdets
nstrphis=vsmjf.n_strphis 
nspins=vsmjf.n_spins

;transform unit vectors from payload to gse
atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index

;print,sp.mfrecn,sp.mfyr,sp.mfdy,sp.mfms,sp.spinp

if (scimode1 or scimode2 or scimode4) and $
      (scimode_ihk ne last_scimode or atindx ne last_atindx) then begin      
      ;print,'recn, new atindx ',recn,atindx
      wc_gse=dblarr(3)
      vunit_gse=dblarr(nstrphis,3)
      if atfile ne '' then begin
        for i=nstrdets/2,nstrdets/2 do for j=0,nstrphis-1 do begin
          payload_to_gse,$
          [vsmjf.vunitstrl(i,j,0),vsmjf.vunitstrl(i,j,1),$
           vsmjf.vunitstrl(i,j,2)],$
          [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
          vunit_gse(j,*)=wc_gse
        endfor
      endif else begin
        if lprnt then $
        print,'no attitude data; using 180 deg rotation about x-axis instead'
        lprnt=0     
        ;---(approx transform from payload to gse: SWE spin axis along -zgse)
        for i=0,nstrdets-1 do for j=0,nstrphis-1 do begin
          vunit_gse(j,0)= vsmjf.vunitstrl(i,j,0)
          vunit_gse(j,1)=-vsmjf.vunitstrl(i,j,1)
          vunit_gse(j,2)=-vsmjf.vunitstrl(i,j,2) 
        endfor
      endelse    
endif
    
    
;---start spin loop
  for ispin=0,nspins-1 do begin
     
    if scimode6 then begin  
      ;print,'recn, new atindx ',recn,atindx
      wc_gse=dblarr(3)
      vunit_gse=dblarr(nstrphis,3)
      if atfile ne '' then begin
        for j=0,nstrphis-1 do begin
          payload_to_gse,$
          [vsmjf.vunitstrl(nstrdets/2,j,0,ispin),$
           vsmjf.vunitstrl(nstrdets/2,j,1,ispin),$
           vsmjf.vunitstrl(nstrdets/2,j,2,ispin)],$
          [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
          vunit_gse(j,*)=wc_gse
        endfor
      endif else begin
        if lprnt then $
        print,'no attitude data; using 180 deg rotation about x-axis instead'
        lprnt=0     
        ;---(approx transform from payload to gse: SWE spin axis along -zgse)
        for j=0,nstrphis-1 do begin
          vunit_gse(j,0)= vsmjf.vunitstrl(nstrdets/2,j,0,ispin)
          vunit_gse(j,1)=-vsmjf.vunitstrl(nstrdets/2,j,1,ispin)
          vunit_gse(j,2)=-vsmjf.vunitstrl(nstrdets/2,j,2,ispin) 
        endfor
      endelse    
    endif
    
    phistrl_gse=float(atan(vunit_gse(*,1),vunit_gse(*,0))/!dtor)   
    wl0=where(phistrl_gse le 0)
    if wl0(0) ne -1 then phistrl_gse(wl0)=360.+phistrl_gse(wl0)
   
    timpb5=vsmjf.pb5tim_vsbl(*,ispin)
        
    ;print,'recn, ispin, eleion ',recn, ispin, eleion

    ;---get mfi 3sec mag field    
    if magfile ne '' then begin        
      mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
      sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
      b=reform(bgse(mindx,0:2,sindx))
      ;if b(0) eq rfill then goto,endspinloop 
    endif       

    ;---get strahl hv step in voltage table
    enstep=long(vsmjf.strlstep(ispin))

    ;---initialize and assign output electron pitch angle structure     
    sdata = {  $
        lzrec:long(recn),  $
        spinbl:long(ispin),  $
        tjdsec_spinbl:vsmjf.suntim_vsbl(ispin),  $
        pb5tim:timpb5,  $
        enstep:enstep,$
        f:reform(vsmjf.strl(*,*,ispin)),   $
        phistrl_gse:phistrl_gse, $
        b:b,$
        misc:fltarr(5)  }

    sdata.misc(0)=lz.mf(ihk(22).offs)     ;strahl voltage bias level
    sdata.misc(1)=scimode_ihk
    if scimode6 then begin
      sdata.misc(2)=vsmjf.bxyz_status(ispin)
      sdata.misc(3)=vsmjf.bxyz_ind(ispin)
    endif  
        
    if lpr then $
      print,recn,ispin,vsmjf.tjd,vsmjf.sec,sec_pb5(vsmjf.suntim_vsbl(ispin))
    writeu,elelun,sdata       ;write electron output data
    espn=espn+1

    endspinloop:     
  endfor  ;end spin loop
      
  skiprec=50
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)
  endif

  last_scimode=scimode_ihk
  last_atindx=atindx
  
  endrecordloop:

endfor     ;end record loop

endlzfile:

nrecele=long(espn+1)
free_lun,elelun

;---version id
version='_v7'

if nrecele ne 0 then begin    ;yes electron data...make output file
  ;---create electron data header
  openw,elelun,getenv('IDLSAV')+'ehedr',/get_lun
  writeu,elelun,nrecele,long(nstrdets),long(nstrphis)
  free_lun,elelun

  ;---concatenate electron header and strahl data files 
  hfl=getenv('IDLSAV')+'ehedr'
  dfl=getenv('IDLSAV')+'strahl.dat'
  mfl=getenv('MPNEW')+wst.lzdate+version+'.strahl'
  spawn,'cat ' + hfl + ' ' + dfl + ' > ' + mfl
  print,'electron strahl data and spectra file created: ',mfl       

  print,'lzstrahl: electron strahl (.strl file) finished for date ',wst.lzdate

endif



endfor

free_lun,lundate


end



