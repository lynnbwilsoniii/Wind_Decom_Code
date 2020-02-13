pro wind_orbit_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

print,'wind_orbit_read :'

idatype=where(d.datype eq 'wind_orbit')


re=6373.

data=replicate({ta:0.d,tpb5:lonarr(3),gse_pos:fltarr(3)},$
  wst.number_days*150)

lastsz=0
date=wst.indate
for k=0,wst.number_days-1 do begin
 
;get WIND orbit data
  oapath=getenv(d.pathenv(idatype))  
  arg=oapath+'*'+'or_def_'+string(date,format='(i8)')+'*'+'.cdf'
  result=findfile(arg,count=count)
  windorbitflnm=''
  if count eq 0 then begin
    arg=oapath+'*'+'or_pre_'+string(date,format='(i8)')+'*'+'.cdf'
    result=findfile(arg,count=count)
  endif
  if count ne 0 then begin
    windorbitflnm=result(count-1) ;assumes highest count is latest version 
    print,'orb file  ',windorbitflnm
    ;open orb file 
      lunat=1      
      openr,lunat,windorbitflnm 
    ;get orbit 10 min data
      loadcdf,windorbitflnm,'Time_PB5',tpb5_orb   
      loadcdf,windorbitflnm,'GSE_POS',gse_pos
       close,lunat
       
     d.flnm(idatype)=windorbitflnm
     sz=size(tpb5_orb)
     for i=0,sz(1)-1 do begin
       data(lastsz+i).ta=pb5_sec(tpb5_orb(i,*))
       data(lastsz+i).tpb5(*)=tpb5_orb(i,*)
       data(lastsz+i).gse_pos(*)=gse_pos(i,*)/re
     endfor  
     lastsz=lastsz+sz(1) 
   endif
   pb5=ymd_pb5(long(date))
   sec=pb5_sec(pb5)
   pb5_next=sec_pb5(sec+long(86400))
   date=string(long(pb5_ymd(pb5_next)),format='(i8)')
     
endfor

windorbitdat=data(0:lastsz-1)
data=0

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(windorbitdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file
  tjd0=long(fix(windorbitdat(0).ta/86400.d))

d=create_struct(d,'wind_orbitdat',windorbitdat)
windorbitdat=0

end
