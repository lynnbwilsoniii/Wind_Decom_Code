;changes ionkp file names

arg=getenv('IONKPPATH')+'9*.cdf'
result=findfile(arg,count=count)
for i=0,n_elements(result)-1 do begin
  date=strmid(result(i),strlen(getenv('IONKPPATH')),6)
  version=strmid(result(i),strlen(getenv('IONKPPATH'))+6,2)
  newfilenm=getenv('IONKPPATH')+'wi_k0_swe_19'+date+'_v'+version+'.cdf'
  print,result(i)
  print,newfilenm
  spawn,'cp -p '+result(i)+' '+newfilenm
endfor
end
