openr,lun,getenv('WGGSBASE')+'swe/13nov96/data/eclipse.318',/get_lun

for i=0,9 do begin
  str=''
  readf,lun,str
  print,'i ',i,' ',str
endfor

for i=0,100 do begin
  str=''
  readf,lun,str
  print,'i ',i,' ',str
  reads,s
stop
endfor

end
