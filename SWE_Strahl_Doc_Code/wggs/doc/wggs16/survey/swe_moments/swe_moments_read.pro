pro swe_moments_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

print,'moments_read :'

idatype=where(d.datype eq 'swe_moments')
flnm=d.flnm(idatype)

openr,lunm,flnm,/get_lun
print,' ' & print,'reading mdat file ',flnm,' ......'

ver=strmid(flnm,strlen(flnm)-6,2)

;read header
nrec=0l
prod_date=0l
if ver eq '04' or ver eq 'v4' or ver eq '05' or ver eq 'v5' $
then readu,lunm,nrec $
else if ver eq '06' then  readu,lunm,nrec,prod_date $
else begin
  print,'Enter 0 if earlier than version 6; enter 1 if version 6 or later'
  answ=0 & read,answ 
  if answ eq 0 then readu,lunm,nrec else $
  if answ eq 0 then readu,lunm,nrec,prod_date else stop
endelse

print,'nrec ',nrec
print,'prod_date ',prod_date

indat={$
  mfrec:0l,$
  mfspinbl:0l,$
  ta:0.d,$
  iflgs:lonarr(8),$
  gains:fltarr(6),$
  misc:fltarr(10),$
  v:fltarr(16),$
  vpot:0.,$
  fnout:0.,$
  uout:fltarr(3),$
  hout:fltarr(3),$
  trout:0.,$
  ettrt:0.,$
  gyrtrpy:0.,$
  eavg:0.,$
  paxis:fltarr(3),$
  pout:fltarr(3,3),$
  spcpot:0.,$
  b:fltarr(3),$
  bnfit:fltarr(10)}

data=replicate(indat,nrec)  

readu,lunm,data
free_lun,lunm
;if wst.number_days gt d.maxnumberdays then $
;      swe_moments_structavg,indat,data_next,nrec_next,wst.number_days
print,'end reading file ',flnm


;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate
  number_days=wst.number_days < wst.maxnumber_days
  for inxt=1,wst.number_days-1 do begin
    thispb5=ymd_pb5(long(thisdate))
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm('moments',getenv(d.pathenv(idatype)),nullstr,'v05.mom',$
      date_next,err=err)
    if err ne '' then goto,getoutloop
    
    openr,lun_next,file_next,/get_lun
    ;read header
    nrec_next=0l
    prod_date_next=0l
    if ver eq '04' or ver eq '05' then readu,lun_next,nrec_next $
    else if ver ge '06' then  readu,lun_next,nrec_next,prod_date_next $
    else begin
      print,'Enter 0 if earlier than version 6; enter 1 if version 6 or later'
      answ=0 & read,answ 
      if answ eq 0 then readu,lun_next,nrec_next else $
      if answ eq 0 then readu,lun_next,nrec_next,prod_date_next else stop
    endelse

    
    print,'nrec_next ',nrec_next
    data_next=replicate(indat,nrec_next)
    readu,lun_next,data_next
    free_lun,lun_next
    ;if wst.number_days gt d.maxnumberdays then $
    ;  swe_moments_structavg,indat,data_next,nrec_next,wst.number_days
    print,'end reading next file ',file_next
    data=[temporary(data),data_next]
    data_next=0
    thisflnm=file_next
    thisdate=date_next
  endfor
endif
getoutloop:

d.ndx(0,idatype)=0
d.ndx(1,idatype)=n_elements(data)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)

fill=-1.e31
;put input data into structure mdat
mdat=replicate($
  {mfrec:0l,mfspinbl:0l,ta:0.d,tpb5:lonarr(3),$
  fnout:0.,uout:fltarr(3),hout:fltarr(3),trout:0.,ettrt:0.,$
  gyrtrpy:0.,eavg:0.,paxis:fltarr(3),b:fltarr(3),qdotb:0.,$
  umag:0.,theu:0.,phiu:0.,hmag:0.,theh:0.,phih:0.,$
  bmag:fill,theb:fill,phib:fill,magp:0.,bnfit:fltarr(10),$
  spcpot:0.,vpot:0.,$
  iflgs:lonarr(8),gains:fltarr(6),misc:fltarr(10),v:fltarr(16),$
  prod_date:prod_date},$
  n_elements(data))

;input data
  mdat.mfrec=data.mfrec
  mdat.mfspinbl=data.mfspinbl
  mdat.ta=data.ta
  mdat.fnout=data.fnout
  mdat.uout=data.uout*1e-5
  mdat.hout=data.hout
  mdat.trout=data.trout
  mdat.ettrt=data.ettrt
  mdat.gyrtrpy=data.gyrtrpy
  mdat.eavg=data.eavg
  mdat.paxis=data.paxis
  mdat.b=data.b
  mdat.bnfit=data.bnfit
  mdat.spcpot=data.spcpot
  mdat.vpot=data.vpot
  mdat.iflgs=data.iflgs
  mdat.gains=data.gains
  mdat.misc=data.misc(0:9)
  mdat.v=data.v
  data=0

;computed from input data
  for i=0l,n_elements(mdat)-1 do mdat(i).tpb5=sec_pb5(mdat(i).ta)
  mdat.umag=sqrt(mdat.uout(0)^2+mdat.uout(1)^2+mdat.uout(2)^2)
  mdat.theu=asin(mdat.uout(2)/mdat.umag)/!dtor
  mdat.phiu=atan(mdat.uout(1),mdat.uout(0))/!dtor
     wphlt0=where(mdat.phiu lt 0,nwphlt0)
     if nwphlt0 gt 0 then mdat(wphlt0).phiu=mdat(wphlt0).phiu+360.

  mdat.hmag=sqrt(mdat.hout(0)^2+mdat.hout(1)^2+mdat.hout(2)^2)
  mdat.theh=asin(mdat.hout(2)/mdat.hmag)/!dtor
  mdat.phih=atan(mdat.hout(1),mdat.hout(0))/!dtor
     wphlt0=where(mdat.phih lt 0,nwphlt0)
     if nwphlt0 gt 0 then mdat(wphlt0).phih=mdat(wphlt0).phih+360.

  wbok=where(mdat.b(0) gt fill,nwbok)
  if wbok(0) ne -1 then begin
    mdat(wbok).bmag=sqrt(mdat(wbok).b(0)^2+mdat(wbok).b(1)^2+mdat(wbok).b(2)^2)
    mdat(wbok).theb=asin(mdat(wbok).b(2)/mdat(wbok).bmag)/!dtor
    mdat(wbok).phib=atan(mdat(wbok).b(1),mdat(wbok).b(0))/!dtor
    wbylt0=where(mdat.b(0) gt fill and mdat.b(1) lt 0,nwbylt0)
    if nwbylt0 gt 0 then mdat(wbylt0).phib=mdat(wbylt0).phib+360.
    mdat(wbok).magp=(mdat(wbok).bmag *1e-5)^2 / (8 * !pi)
  endif

   mdat.qdotb= (mdat.hout(0)*mdat.b(0)+ $
            mdat.hout(1)*mdat.b(1)+ $
            mdat.hout(2)*mdat.b(2))/$
            (mdat.bmag * mdat.hmag)

  
  ;get start time for this file 
      nstrt=21
      if n_elements(mdat) lt nstrt then nstrt=n_elements(mdat)
      w=where(long(fix(mdat(0:nstrt-1).ta/86400.d)) - $
              long(fix(mdat(0).ta/86400.d)) ne 0)

      ;reset the reference time and time index to skip up to first three spins 
      ;(nstrt=21) that may have times before midnight on previous day
      if w(0) ne -1 then begin 
        k0=w(n_elements(w)-1)
        ws=where(long(fix(mdat(0:nstrt-1).ta/86400.d)) $
                 eq long(fix(mdat(k0).ta/86400.d)))
        k0=ws(0)
        d.ndx(0,idatype)=k0
        d.ndx_orig(0,idatype)=d.ndx(0,idatype)        
      endif else k0=0
       
      tjd0=long(fix(mdat(k0).ta/86400.d)) 
 
  print,' '
 print,'glint mask used: ',mdat(0).misc(6)
 print,' '
 
 d=create_struct(d,'swe_mdat',mdat)
        
end
