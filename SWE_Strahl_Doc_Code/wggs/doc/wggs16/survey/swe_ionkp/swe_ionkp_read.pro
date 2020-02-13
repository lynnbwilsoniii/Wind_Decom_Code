
; The following program will load all of the data for a specific CDF file and
; variable into IDL. 
; 
;(Jim Byrnes)

pro loadcdf_ionkp,CDF_file,CDF_var,x

ON_ERROR,1

;
; Open CDF file
;

id = cdf_open(CDF_file)

;
; Get file CDF structure information
; 

inq = cdf_inquire(id)

;
; Get variable structure information
;

vinq = cdf_varinq(id,CDF_var)
dims = total(vinq.dimvar)
dimc = vinq.dimvar * inq.dim
dimw = where(dimc eq 0)
if (dimw(0) ne -1) then dimc(dimw) = 1

;help,inq,vinq,/structures

;print,'vinq.dimvar, inq.dim, dimc',vinq.dimvar, inq.dim, dimc

CDF_varget,id,CDF_var,x,COUNT=dimc,REC_COUNT=inq.maxrec+1
;help,x
;print,x(0:dimc-1,0:3)
sa = size(x)
sa = sa(1:sa(0))

if (vinq.recvar eq 'VARY' and dims ne 0) then begin
;help,x

   x = reform(x,[marray(sa(0:(n_elements(sa)-2))),sa(n_elements(sa)-1)])
;help,x

   x = transpose(x)
;help,x

   sa = shift(sa,1)
;help,sa

   x = reform(x,sa)
;help,x
endif

saw = where(sa ne 1)
x = reform(x,sa(saw))
;help,x

CDF_close,id
return
end



pro swe_ionkp_read,tjd0_thisfile=tjd0,err=err

common shared,d
common wstuff,wst

;From ajl@space.mit.edu Tue Feb 28 17:54:43 1995
;modified rjf apr 13 1995

;Subject: program to read swe cdf files

print,'readionkp :'

idatype=where(d.datype eq 'swe_ionkp')
flnm=d.flnm(idatype)

print,' ' & print,'reading ionkpdat file ',flnm,' ......'


;get variables by calling procedure 'loadcdf_ionkp'

print,flnm
CDF_file=flnm 
  
cdf_var='Time_PB5'
loadcdf_ionkp,cdf_file,cdf_var,t

cdf_var='Epoch'
loadcdf_ionkp,cdf_file,cdf_var,te

cdf_var='V_GSE_p'
loadcdf_ionkp,cdf_file,cdf_var,vvec

cdf_var='THERMAL_SPD'
loadcdf_ionkp,cdf_file,cdf_var,w

cdf_var='Np'
loadcdf_ionkp,cdf_file,cdf_var,n

;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate  ;surveydate
  for inxt=1,wst.number_days-1 do begin
    thispb5=ymd_pb5(long(thisdate))
    ;if thispb5(0) eq 1995l and thispb5(1) eq 282l then begin
    ;  date_next='19951010' & file_next=' '
    ;  goto,endloop
    ;endif 
    
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))    
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm(d.datype(idatype),getenv(d.pathenv(idatype)),nullstr,$
      d.fltr(idatype),date_next,err=err)
    if err ne '' then goto,endloop
    CDF_file=file_next
    cdf_var='Time_PB5'
    
    loadcdf_ionkp,cdf_file,cdf_var,t_nxt
    
    t=[t,t_nxt]
    
    cdf_var='Epoch'
    loadcdf_ionkp,cdf_file,cdf_var,te_nxt
    te=[te,te_nxt]

    cdf_var='V_GSE_p'
    loadcdf_ionkp,cdf_file,cdf_var,vvec_nxt
    vvec=[vvec,vvec_nxt]
    
    cdf_var='THERMAL_SPD'
    loadcdf_ionkp,cdf_file,cdf_var,w_nxt
    w=[w,w_nxt]
    
    cdf_var='Np'
    loadcdf_ionkp,cdf_file,cdf_var,n_nxt
    n=[n,n_nxt]
    
    print,'end reading next file ',file_next
    
    endloop:
    thisflnm=file_next
    thisdate=date_next
    
  endfor
endif

;stop 
year=t(0:*,0)
day=t(0:*,1)
msec=t(0:*,2)
dayfrac=msec/86400000.d

;help,year,day,dayfrac,vvec,n,w
;check for good data; form vector 'ok' of good indicies:
;first, get s1, the size of unfiltered array; 
;then get s2, the size of the filtered array
s=size(year)
s1=s(1)-1
fill=-1.e31
;ok=where(vvec(0:*,0) ne fill)
ok = where(vvec(0:*,0) lt 2000. and vvec(0:*,0) gt 0. and n lt 5000 $
           and t(*,2)-shift(t(*,2),1) ne 0)

err=''           
if ok(0) eq -1 then begin
  err='bad ionkp data' & print,err
  return
endif

s=size(ok)
s2=s(1)-1
print,'SPECTRA DELETED=',s1-s2

;define ionkp data structure
ionkpdat=$
  replicate({year:0l,day:0l,hrday:0.d,ta:0.d,tpb5:lonarr(3),$
  v:0.,vew:0.,vns:0.,n:0.,w:0.,ti:0.,eip:0.,beta:0.,eiv:0.,ionp:0.},s(1))


;fix time arrays and day-of-year

ionkpdat.year=year(ok)
ionkpdat.day=day(ok)
ionkpdat.hrday=dayfrac(ok)*24.d

tjd=lonarr(2,s(1))
for i=0,s(1)-1 do tjd(*,i) = $
  long(pb5_tjd([ionkpdat(i).year,ionkpdat(i).day,ionkpdat(i).hrday*3600000]))
ionkpdat(*).ta=reform(tjd(0,*)*86400.d + tjd(1,*)/1000.d)

for i=0,n_elements(ionkpdat)-1 do ionkpdat(i).tpb5=sec_pb5(ionkpdat(i).ta)

cdf_epoch,te(ok(0)), ye,mone,daye,hre,mne,sce,mille, /break
doy=julday(mone,daye,ye)-julday(1,1,ye)+1L


;now fix all other variables

ionkpdat.v=vvec(ok,0)
ionkpdat.vew=vvec(ok,1)
ionkpdat.vns=vvec(ok,2) 
ionkpdat.w=w(ok)
ionkpdat.ti=(1.6726e-24/(2*1.38e-16))*(w(ok)*w(ok))*1e10
ionkpdat.n=n(ok)
ionkpdat.ionp=n(ok)*1.38e-16* $
     (2*1.6726e-24/(3*1.38e-16))*(w(ok)*w(ok))*1e10

;help,ionkpdat
;help,ionkpdat,/str

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(ionkpdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file
  tjd0=long(fix(ionkpdat(0).ta/86400.d))

;delete the last points that may occur after 24h 
      w=where(double(ionkpdat.day)+ionkpdat.hrday/24-  $
               double(ionkpdat(0).day) lt double(wst.number_days))
      d.ndx(1,idatype)=w(n_elements(w)-1)
      d.ndx_orig(1,idatype)=d.ndx(1,idatype)

d=create_struct(d,'swe_ionkpdat',ionkpdat)
ionkpdat=0
        
end


