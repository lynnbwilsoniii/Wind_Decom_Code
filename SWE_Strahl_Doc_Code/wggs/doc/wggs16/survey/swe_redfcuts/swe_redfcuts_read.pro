pro swe_redfcuts_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst


print,'swe_redfcuts_read :'

idatype=where(d.datype eq 'swe_redfcuts')
flnm=d.flnm(idatype)

openr,lunredfcuts,flnm,/get_lun
print,' ' & print,'reading redfcutsdat file ',flnm,' ......'


openr,lun,flnm,/get_lun
nrec=0l & nx=0l & ny=0l
readu,lun,nrec,nx,ny



indat=replicate({pb5:lonarr(3),mfrec:0l,mfspinbl:0l,vmax:0.,$
      F:fltarr(129),fpara:fltarr(129),fperp:fltarr(129)},nrec)
readu,lun,indat
free_lun,lun



date=strmid(flnm,strlen(getenv(d.pathenv(idatype))),8)
pb5date=ymd_pb5(long(date))
wok=where(indat.pb5(1) eq pb5date(1))
indat=indat(wok)

elapspn=7*indat.mfrec+indat.mfspinbl - 7*indat(0).mfrec-indat(0).mfspinbl
redfcutsdat=replicate({pb5:lonarr(3),ta:0.d,mfrec:0l,mfspinbl:0l,elapspn:0l,$
   vmax:0.,F:fltarr(129),fpara:fltarr(129),fperp:fltarr(129),nx:nx,ny:ny},$
   n_elements(indat))
redfcutsdat.pb5=indat.pb5
for i=0,n_elements(indat)-1 do redfcutsdat(i).ta=pb5_sec(redfcutsdat(i).pb5)
redfcutsdat.mfrec=indat.mfrec
redfcutsdat.mfspinbl=indat.mfspinbl
redfcutsdat.elapspn=elapspn
redfcutsdat.vmax=indat.vmax
redfcutsdat.F=indat.F
redfcutsdat.fpara=indat.fpara
redfcutsdat.fperp=indat.fperp      

d.ndx(0,idatype)=0
d.ndx(1,idatype)=n_elements(redfcutsdat)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)

indat=0

;get start time for this file
  tjd0=long(fix(redfcutsdat(0).ta/86400.d))
  
d=create_struct(d,'swe_redfcutsdat',redfcutsdat)
  
end