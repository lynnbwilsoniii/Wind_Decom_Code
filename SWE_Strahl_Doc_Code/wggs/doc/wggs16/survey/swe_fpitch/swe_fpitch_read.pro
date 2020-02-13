pro swe_fpitch_read,tjd0_thisfile=tjd0

common log_delog,comp_tbl,dcomp_tbl
common shared,d
common wstuff,wst

print,'fpitch_read :'

idatype=where(d.datype eq 'swe_fpitch')
flnm=d.flnm(idatype)

creatidlsavfl=0;1

if strmid(flnm,strlen(flnm)-5,5) eq 'pitch' then begin
  openr,lunp,d.flnm(1),/get_lun
  print,' ' & print,'reading pdat file ',flnm,' ......'
  nrec=0l
  readu,lunp,nrec
  print,'nrec ',nrec

  indat={  $
      mfrec:0l,  $
      mfspinbl:0l,  $
      ta:0.d,  $
      pb5tim:lonarr(3),  $
      vsteps:bytarr(16),  $
      fspa:bytarr(16),   $
      f:bytarr(30,16),   $
      b:fltarr(3),$
      eleion:0l,  $
      misc:fltarr(9)  }
      
  newpfrmt=0

  data=replicate(indat,nrec)

  readu,lunp,data
  free_lun,lunp
  print,'end reading file ',flnm
endif else begin
  print,'restoring idl_save file ',flnm
  restore,flnm
  nrec=hedr
endelse

if creatidlsavfl then begin
  idlsvlnm=getenv('IDLSAV')+$
    strmid(flnm,strlen(getenv(d.pathenv(idatype))),6)+'04.pit'
  hedr=nrec  
  save,filename=idlsvlnm,hedr,data
  print,'pitch idlsav file created ',idlsvlnm    
;stop 
endif


;find elapsed spin number
  elapspn=7*data.mfrec+data.mfspinbl - 7*data(0).mfrec-data(0).mfspinbl

;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate
  for inxt=1,wst.number_days-1 do begin
    thispb5=ymd_pb5(long(thisdate))
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm('fpitch',getenv(d.pathenv(idatype)),nullstr,'v4.pitch',$
      date_next,err=err) 
    if err ne '' then goto,getoutloop  
    openr,lun_next,file_next,/get_lun
    nrec_next=0l
    readu,lun_next,nrec_next  ;read header
    print,'nrec_next ',nrec_next
    data_next=replicate(indat,nrec_next)
    readu,lun_next,data_next
    free_lun,lun_next
    
    elapspn=[elapspn,elapspn(n_elements(elapspn)-1)+1+ $
      7*data_next.mfrec+data_next.mfspinbl - $
      7*data_next(0).mfrec-data_next(0).mfspinbl]
      
    print,'end reading next file ',file_next
    data=[temporary(data),data_next]
    data_next=0
    thisflnm=file_next
    thisdate=date_next
  endfor
endif
getoutloop:


;put input data into structure pdat
pdat=replicate($
   {mfrec:0l,mfspinbl:0l,ta:0.d,tpb5:lonarr(3),$
   vsteps:bytarr(16),$
   fspa:bytarr(16),$
   f:bytarr(30,16),$
   b:fltarr(3),eleion:0l,elapspn:0l,misc:fltarr(9)}, $
   n_elements(data))

;input data
pdat.mfrec=data.mfrec
pdat.mfspinbl=data.mfspinbl
pdat.ta=data.ta
pdat.fspa=data.fspa
pdat.f=data.f
pdat.misc=data.misc

for j=0,15 do begin
  findspikes,pdat.fspa(j),spikind
  if spikind(0) ne -1 then begin
    pdat(spikind).fspa(j)=0
    pdat(spikind).f(*,j)=0 
  endif
endfor

findgaps,elapspn,edges,gapx=100,spns=10
if edges(0) ne -1 then begin
  for j=0,15 do begin
    pdat(edges).fspa(j)=0
    pdat(edges).f(*,j)=0
  endfor
endif

for i=0l,n_elements(data)-1 do $
  pdat(i).vsteps=data(i).vsteps(sort(data(i).vsteps))

pdat.b=data.b
pdat.tpb5=data.pb5tim
pdat.eleion=data.eleion
pdat.elapspn=elapspn
data=0


wtimeok=where(pdat.ta-pdat(0).ta ge 0)
help,pdat.ta,wtimeok
pdat=temporary(pdat(wtimeok))


d.ndx(0,idatype)=0
d.ndx(1,idatype)=n_elements(pdat)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)

;pdat(891).fspa(*)=0 & pdat(891).f(*)=0   ;take out spike 1994 dec 27


numen=n_elements(pdat(d.ndx(0,idatype)).vsteps)
;!!!!!!! 
;   The following two lines should be commented or deleted. They are wrong.
;d.pnlist.list(d.pnlist.offs(idatype)+1:$
;              d.pnlist.offs(idatype)+1+d.pnlist.len(idatype)-1)=''

d.pnlist.list(d.pnlist.offs(idatype)+1:d.pnlist.offs(idatype)+1+numen-1)=$
  string(volt_en(pdat(d.ndx(0,idatype)).vsteps,/en),format='(f8.1)')

;get start time for this file 
      nstrt=20
      if n_elements(pdat) lt nstrt then nstrt=n_elements(pdat)
      w=where(long(fix(pdat(0:nstrt).ta/86400.d)) - $
              long(fix(pdat(0).ta/86400.d)) ne 0)
      if w(0) ne -1 then k0=w(n_elements(w)-1) else k0=0
      tjd0=long(fix(pdat(k0).ta/86400.d)) 

d=create_struct(d,'swe_pdat',pdat)
      
end
