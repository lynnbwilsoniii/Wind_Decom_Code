
pro mfi_magkp_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

print,'mfi_magkp_read :'

idatype=where(d.datype eq 'mfi_magkp')
flnm=d.flnm(idatype)

print,' ' & print,'reading magkpdat file ',flnm,' ......'


print,flnm
CDF_file=flnm

cdf_var='Time_PB5'
loadcdf,cdf_file,cdf_var,t

cdf_var='BGSEc'
loadcdf,cdf_file,cdf_var,x

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

    if err ne '' then goto,endloop
    CDF_file=file_next
    cdf_var='Time_PB5'
    loadcdf,cdf_file,cdf_var,t_nxt
    t=[t,t_nxt]
    
    cdf_var='BGSEc'
    loadcdf,cdf_file,cdf_var,x_nxt
    x=[x,x_nxt]
    
    print,'end reading next file ',file_next 
    endloop:
    thisflnm=file_next
    thisdate=date_next
  endfor
endif

fill=-1.e31
wok=where(x(*,0) ne fill,nwok)
t=t(wok,*)
x=x(wok,*)

;define magkp data structure
magkpdat=$
  replicate({ta:0.d,tpb5:lonarr(3),b:0.d,th:0.,ph:0.,bf:fltarr(3),magp:0.},nwok)

tjd=lonarr(2)
for i=0l,nwok-1 do begin
  magkpdat(i).tpb5=t(i,*)
  tjd=pb5_tjd(t(i,*))
  magkpdat(i).ta=tjd(0)*86400.d + tjd(1)/1000.d
  magkpdat(i).ph=atan(x(i,1),x(i,0))/!dtor
  if magkpdat(i).ph lt 0 then magkpdat(i).ph=magkpdat(i).ph + 360.
  magkpdat(i).th=asin(x(i,2)/sqrt(x(i,0)^2+x(i,1)^2+x(i,2)^2) )/!dtor
  magkpdat(i).b=sqrt(total(x(i,*)*x(i,*)))
  magkpdat(i).bf(0)=x(i,0)
  magkpdat(i).bf(1)=x(i,1)
  magkpdat(i).bf(2)=x(i,2)
  magkpdat(i).magp=(magkpdat(i).b *1e-5)^2 / (8 * !pi) 
endfor


help,magkpdat
help,magkpdat,/str

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(magkpdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file
  tjd0=long(fix(magkpdat(0).ta/86400.d))

d=create_struct(d,'mfi_magkpdat',magkpdat)

end


