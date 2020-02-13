
pro mfi_mag3s_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

print,'mfi_mag3s_read :'

;Because this is hi res data, a maximum of two days data shall be plotted; 
;  this will allow centering data near 24h.
number_days=wst.number_days < 2

idatype=where(d.datype eq 'mfi_mag3s')
flnm=d.flnm(idatype)

print,flnm
CDF_file=flnm
print,' ' & print,'reading mag3sdat file ',flnm,' ......'

cdf_var='Time_PB5'     
loadcdf,CDF_file,cdf_var,t
cdf_var='B3GSE'
loadcdf,CDF_file,cdf_var,x           

;if multiple days 
if number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate
  for inxt=1,number_days-1 do begin
    thispb5=ymd_pb5(long(thisdate))
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm(d.datype(idatype),d.dir(idatype),nullstr,d.fltr(idatype),$
      date_next,err=err)
    print,'reading mag3sdat file ',file_next
    
    if err ne '' then goto,endloop
    CDF_file=file_next
    cdf_var='Time_PB5'
    loadcdf,cdf_file,cdf_var,t_nxt
    t=[t,t_nxt]
    
    cdf_var='B3GSE'
    loadcdf,cdf_file,cdf_var,x_nxt
    x=[x,x_nxt]
     
    endloop:
    thisflnm=file_next
    thisdate=date_next
  endfor
endif
ndat=1440l * 20l * number_days

fill=-1.e31

;define mag3s data structure
indat={ta:0.d,tpb5:lonarr(3),b:0.,th:0.,ph:0.,$
  bgsex:0.,bgsey:0.,bgsez:0.}
mag3sdat=replicate(indat,ndat)


mag3sdat(*).bgsex=x(*,0)
mag3sdat(*).bgsey=x(*,1)
mag3sdat(*).bgsez=x(*,2)
a=replicate(1.,20)#reform(t(*,0)) & mag3sdat(*).tpb5(0)=a(*)
a=replicate(1.,20)#reform(t(*,1)) & mag3sdat(*).tpb5(1)=a(*)
a=replicate(1.,20)#reform(t(*,2)) - $
 (30000l - 1500l - lindgen(20)*3000)#replicate(1.,1440l * number_days)
  
mag3sdat(*).tpb5(2)=a(*)

a=0

wok=where(mag3sdat.bgsex(0) ne fill,nwok)
mag3sdat=mag3sdat(wok)

  
nrec=n_elements(mag3sdat)
      
tjd=lonarr(2)
for i=0l,nrec-1 do begin
  mag3sdat(i).b=$
    sqrt(mag3sdat(i).bgsex^2+mag3sdat(i).bgsey^2+mag3sdat(i).bgsez^2)
  tjd=pb5_tjd(mag3sdat(i).tpb5)
  mag3sdat(i).ta=tjd(0)*86400.d + tjd(1)/1000.d
  mag3sdat(i).ph=atan(mag3sdat(i).bgsey,mag3sdat(i).bgsex)/!dtor
  if mag3sdat(i).ph lt 0 then mag3sdat(i).ph=mag3sdat(i).ph + 360.
  mag3sdat(i).th=asin(mag3sdat(i).bgsez/mag3sdat(i).b )/!dtor  
endfor


help,mag3sdat
help,mag3sdat,/str

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(mag3sdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file 
  tjd0=long(fix(mag3sdat(0).ta/86400.d))

d=create_struct(d,'mfi_mag3sdat',mag3sdat)
mag3sdat=0
  
end


