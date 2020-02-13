;changes version number of moments file names

date_ref='19971028'
pb5_ref=ymd_pb5(long(date_ref))
elapsec_ref=pb5_elapsec(pb5_ref,pb5_ref)
ndays=65
for iday=0,ndays-1 do begin
  if iday gt 0 then begin
    elapsec=elapsec_last+86400.d0
    pb5=elapsec_pb5(elapsec,pb5_ref)
    date=string(pb5_ymd(pb5),format='(i8)')
  endif else date=date_ref
  elapsec_last=pb5_elapsec(ymd_pb5(long(date)),pb5_ref)
  print,iday,'  ',date

  arg=getenv('SWE_MOMPATH')+date+'*v13.mom'

  result=findfile(arg,count=count)
  for i=0,n_elements(result)-1  do begin
    oldflnm=result(i)
    print,' ' & print,'oldflnm ',oldflnm
    flnmroot=strmid(oldflnm,0,strlen(oldflnm)-8)
    print,'flnmroot ',flnmroot
    newflnm=flnmroot+'_x1_'+'v13.mom'
    print,'newflnm ',newflnm
    print,'hit return to continue'
    answ='' & read,answ & if answ ne '' then stop
    spawn,'mv '+oldflnm+' '+newflnm
    newarg=getenv('SWE_MOMPATH')+date+'*_x1_'+'v13.mom'
    newresult=findfile(newarg,count=count)
    print,'new file ',newresult
  endfor
endfor
end  