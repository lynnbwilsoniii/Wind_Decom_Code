pro wav_nehr_read,tjd0_thisfile=tjd0

;reads WAVES 1min Ne files

common shared,d
common wstuff,wst

idatype=where(d.datype eq 'wav_nehr')
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


fill=-1.e31
wok=where(x ne fill,nwok)
t=t(wok,*)
x=x(wok)
q=q(wok)

;define data structure
nehrdat=replicate({ta:0.d,tpb5:lonarr(3),n:0.,q:0.},nwok)
tjd=lonarr(2)
for i=0,nwok-1 do begin
  nehrdat(i).tpb5=t(i,*)
  tjd=pb5_tjd(t(i,*))
  nehrdat(i).ta=tjd(0)*86400.d + tjd(1)/1000.d
  nehrdat(i).n=x(i)
  nehrdat(i).q=q(i)
endfor  

help,nehrdat
help,nehrdat,/str

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(nehrdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file
  tjd0=long(fix(nehrdat(0).ta/86400.d))

d=create_struct(d,'wav_nehrdat',nehrdat)

end

