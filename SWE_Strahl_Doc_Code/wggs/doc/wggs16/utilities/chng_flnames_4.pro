;changes yyddmm in file names to yyyyddmm

dir='/export/home/u3rjf/wggs.y2k/swe/cal/glint/'
fltr='dglint_m2_'
arg=dir+fltr+'*'


result=findfile(arg,count=count)
for i=0,n_elements(result)-1 do begin
  print,' '
  print,'old filename: ',result(i)
  oldy=strmid(result(i),strlen(result(i))-10,6)
  if strmid(oldy,0,1) eq '9' then begin
    newflnm=fltr+'19'+oldy+'.pro'
    print,'new filename: ',dir+newflnm
    spawn,'mv '+result(i)+' '+dir+newflnm
    print,'hit return to rename file'
  endif else print,'file not renamed'  
  answ='' & read,answ & if answ ne '' then stop


endfor
end
