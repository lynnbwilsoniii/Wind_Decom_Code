;changes yyddmm in file names to yyyyddmm

dir='/data5/swe/backg_y2k/'
arg=dir+'*.dat'


result=findfile(arg,count=count)
for i=0,n_elements(result)-1 do begin
  print,' '
  print,'old filename: ',result(i)
  oldflnm=strmid(result(i),strlen(dir),strlen(result(i))-strlen(dir))
  if strmid(oldflnm,0,1) eq '9' then begin
    newflnm='19'+oldflnm
    print,'new filename: ',dir+newflnm
    spawn,'mv '+result(i)+' '+dir+newflnm
    print,'hit return to rename file'
  endif else print,'file not renamed'  
  answ='' & read,answ & if answ ne '' then stop


endfor
end
