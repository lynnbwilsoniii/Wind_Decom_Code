pro wav_nekp_read,tjd0_thisfile=tjd0

;reads WAVES 1min Ne files

common shared,d
common wstuff,wst

idatype=where(d.datype eq 'wav_nekp')
flnm=d.flnm(idatype)
print,' ' & print,'reading magkpdat file ',flnm,' ......'

print,flnm
CDF_file=flnm

cdf_var='Time_PB5'
loadcdf,cdf_file,cdf_var,t

cdf_var='Ne'
loadcdf,cdf_file,cdf_var,x
help,x


cdf_var='Ne_Quality'
loadcdf,cdf_file,cdf_var,q
help,q

;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate  ;surveydate
  for inxt=1,wst.number_days-1 do begin
  
    thispb5=ymd_pb5(long(thisdate))
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
    loadcdf,cdf_file,cdf_var,t_nxt
    t=[t,t_nxt]
    
    cdf_var='Ne'
    loadcdf,cdf_file,cdf_var,x_nxt
    x=[x,x_nxt]

    cdf_var='Ne_Quality'
    loadcdf,cdf_file,cdf_var,q_nxt
    q=[q,q_nxt]
    
    print,'end reading next file ',file_next
    
    endloop:
    thisflnm=file_next
    thisdate=date_next
    
  endfor
endif

fill=-1.e31
wok=where(x ne fill,nwok)
t=t(wok,*)
x=x(wok)
q=q(wok)

;define data structure
nekpdat=replicate({ta:0.d,tpb5:lonarr(3),n:0.,q:0.},nwok)
tjd=lonarr(2)
for i=0,nwok-1 do begin
  nekpdat(i).tpb5=t(i,*)
  tjd=pb5_tjd(t(i,*))
  nekpdat(i).ta=tjd(0)*86400.d + tjd(1)/1000.d
  nekpdat(i).n=x(i)
  nekpdat(i).q=q(i)
endfor  

help,nekpdat
help,nekpdat,/str

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(nekpdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file
  tjd0=long(fix(nekpdat(0).ta/86400.d))

d=create_struct(d,'wav_nekpdat',nekpdat)
nekpdat=0

end

