pro swe_fpitch_read_v05,tjd0_thisfile=tjd0

common log_delog,comp_tbl,dcomp_tbl
common shared,d
common wstuff,wst

print,'swe_fpitch_read_v05 :'

idatype=where(d.datype eq 'swe_fpitch')
flnm=d.flnm(idatype)

creatidlsavfl=0;1

if strmid(flnm,strlen(getenv(d.pathenv(idatype))),2) eq '19' or $
   strmid(flnm,strlen(getenv(d.pathenv(idatype))),2) eq '20' then begin
   
  openr,lunp,flnm,/get_lun
  print,' ' & print,'reading pdat file ',flnm,' ......'

  if strmid(flnm,strlen(flnm)-5,5) eq 'pitch' then $
    ver=strmid(flnm,strlen(flnm)-8,2) else $
  ver=strmid(flnm,strlen(flnm)-6,2)

  ;read header
  nrec=0l
  if ver ge '05' then begin
     hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknoatt:0l,$
            ndets:0l,nvsteps:0l,nsectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            max_enstep:0l,$
            thisrecn:0l,scimodechange:0l,dummy:0l}
      
    readu,lunp,hedr
    nrec=hedr.nrec
  endif else begin
    print,'wrong pitch version ',ver
    stop
  endelse
   
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
      misc:fltarr(9),$
      misc2:bytarr(8),$
      gains:fltarr(6),$
      cnvrt_to_f:fltarr(16),$
      rgse:fltarr(3)  }
      
  newpfrmt=1

  data=replicate(indat,nrec)

  readu,lunp,data
  free_lun,lunp
  print,'end reading file ',flnm

endif else begin
  print,'restoring idl_sav file ',flnm
  restore,flnm
  nrec=hedr.nrec
endelse  
prod_date=hedr.thisdate  
print,'nrec ',nrec
print,'prod_date ',prod_date

if creatidlsavfl then begin
  idlsvflnm=getenv('IDLSAV2')+$
    strmid(flnm,strlen(getenv(d.pathenv(idatype)))+2,6)+ver+'.pit'
  save,filename=idlsvflnm,hedr,data
  print,'moments idlsav file created ',idlsvflnm    
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
    get_flnm(d.datype(idatype),d.dir(idatype),nullstr,d.fltr(idatype),$
      date_next,err=err)
    ;file_next=$
    ;get_flnm('pitch',getenv(d.pathenv(idatype)),nullstr,'v05.pit',date_next,err=err)
    
    
    if err ne '' then goto,getoutloop   
    openr,lun_next,file_next,/get_lun
    nrec_next=0l
    hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknoatt:0l,$
            ndets:0l,nvsteps:0l,nsectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            max_enstep:0l,$
            thisrecn:0l,scimodechange:0l,dummy:0l}
      
   readu,lun_next,hedr
   nrec_next=hedr.nrec
   
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
   b:fltarr(3),eleion:0l,elapspn:0l,$
   misc:fltarr(9),$
   misc2:bytarr(8),$
   gains:fltarr(6),$
   cnvrt_to_f:fltarr(16),$
   rgse:fltarr(3) }, $
   n_elements(data))

;input data
pdat.mfrec=data.mfrec
pdat.mfspinbl=data.mfspinbl
pdat.ta=data.ta  
pdat.fspa=data.fspa
pdat.f=data.f


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
pdat.misc=data.misc
pdat.misc2=data.misc2
pdat.gains=data.gains
pdat.cnvrt_to_f=data.cnvrt_to_f
pdat.rgse=data.rgse
;stop
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

 print,' '
 print,'glint mask used (pitch) : ',pdat(0).misc2(0)
 print,hedr.glnt(where(hedr.glnt ne -1))
 print,' '

d=create_struct(d,'swe_pdat',pdat)
 
end
