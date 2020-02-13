;pro copy_mode2_momfiles

outdir='/data1/swe/mod2_moments/'
indir='/mnt/leprjf_data5/swe/moments2/'

restore,getenv('SWEDATLIB')+'mode2_dates.txt'
help,date_mode2
for i=0,n_elements(date_mode2)-1 do begin  
  arg=indir+'*'+strmid(date_mode2(i),2,6)+'*v05.mom'
  result=findfile(arg,count=count)
  if result(0) ne '' then begin
    flnm=strmid(result(0),strlen(indir),strlen(result(0))-strlen(indir))
    for j=0,n_elements(result)-1 do print, date_mode2(i),' ',result(j),' ',flnm
    spawn,'cp -p '+result(0)+' '+outdir+flnm
    ;print,'hit return' 
    answ='' & read,answ & if answ ne '' then stop
  endif
endfor


end