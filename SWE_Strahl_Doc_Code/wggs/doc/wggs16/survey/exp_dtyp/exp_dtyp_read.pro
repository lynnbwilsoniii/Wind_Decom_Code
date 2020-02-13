
pro exp_dtyp_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

print,'exp_dtyp_read :'

idatype=where(d.datype eq 'exp_dtyp')
flnm=d.flnm(idatype)

print,' ' & print,'reading exp_dtypdat file ',flnm,' ......'

;---The following mfi_magkp read statements can serve a model.
;---Any type of input data format is allowed, not just CDF.
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

;---End of mfi_magkp model data read; 
;---x and t are input data array and time respectively

;define exp_dtyp data structure

;USER NOTE: the data structure must include both the time "ta", 
;which is seconds from truncated julian day epoch, and 
;the time "pb5", which is year, calender day and millisec of day

exp_dtypdat=$
 replicate({ta:0.d,tpb5:lonarr(3),varbl1:0.,varbl2:0.,varbl3:0.,varbl4:0.},nwok)

tjd=lonarr(2)
for i=0l,nwok-1 do begin
  exp_dtypdat(i).tpb5=t(i,*)
  tjd=pb5_tjd(t(i,*))
  exp_dtypdat(i).ta=tjd(0)*86400.d + tjd(1)/1000.d
  ;---exp_dtyp.varbl1 etc may be any function of thje input data, x
  ;exp_dtyp.varbl1=
  ;exp_dtyp.varbl2=
  ;exp_dtyp.varbl2=
  ;exp_dtyp.varbl2=
  
endfor


help,exp_dtypdat
help,exp_dtypdat,/str

;begin and end time indices
d.ndx(0,idatype)=0 & d.ndx(1,idatype)=n_elements(exp_dtypdat)-1  
d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
print,'d.ndx(*,idatype) ',d.ndx(*,idatype)


;get start time for this file
  tjd0=long(fix(exp_dtypdat(0).ta/86400.d))

d=create_struct(d,'exp_dtypdat',exp_dtypdat)

end


