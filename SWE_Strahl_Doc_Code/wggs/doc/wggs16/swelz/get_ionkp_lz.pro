
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


;=========== get_ionkp_lz =================================================

pro get_ionkp_lz,err=err

common shared,d
common wstuff,wst
common ionkplz,pb5i,ni,ti,ui,useionkp


err=''
idatyp=where(d.datype eq 'swe_ionkp')
flnm=get_flnm(d.datype(idatyp),d.dir(idatyp),'',d.fltr(idatyp),$
  wst.lzdate,err=err)
if flnm eq '' then begin
  err=d.datype(idatyp)+' file not found'
  print,err & useionkp=0 & return
endif                 

print,' ' & print,'reading ionkpdat file ',flnm,' ......'


;get variables by calling procedure 'loadcdf_ionkp'

print,flnm
CDF_file=flnm 
  
cdf_var='Time_PB5'
loadcdf_ionkp,cdf_file,cdf_var,t

cdf_var='V_GSE_p'
loadcdf_ionkp,cdf_file,cdf_var,vvec

cdf_var='THERMAL_SPD'
loadcdf_ionkp,cdf_file,cdf_var,w

cdf_var='Np'
loadcdf_ionkp,cdf_file,cdf_var,n

year=t(0:*,0)
day=t(0:*,1)
msec=t(0:*,2)

ok = where(vvec(0:*,0) lt 2000. and vvec(0:*,0) gt 0. and n lt 5000 $
           and t(*,2)-shift(t(*,2),1) ne 0)

err=''           
if ok(0) eq -1 then begin
  err='bad ionkp data' & print,err
  useionkp=0
  return
endif

pb5i=lonarr(3,n_elements(ok))
pb5i(0,*)=year(ok)
pb5i(1,*)=day(ok)
pb5i(2,*)=msec(ok)

v=vvec(ok,0)
vew=vvec(ok,1)
vns=vvec(ok,2) 
ti=(1.6726e-24/(2*1.38e-16))*(w(ok)*w(ok))*1e10
ni=n(ok)
ui=fltarr(3,n_elements(ok))
ui(0,*)= v * cos(vns*!dtor)*cos((180.-vew)*!dtor)
ui(1,*)= v * cos(vns*!dtor)*sin((180.-vew)*!dtor)
ui(2,*)= v * sin(vns*!dtor)
useionkp=1
     
end