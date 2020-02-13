;creates an ascii file of plot variables from an idlsave file


;set the data path
start:
;the idlsav data filename
  savpath=getenv('IDLSAV')
  flname=pickfile(/read,get_path=savpath,path=savpath,$
               filter='*idlsave.dat',$
               title='IDLsav Files')
restore,flname
help,idlsav
for i=0,n_elements(idlsav)-1 do help,idlsav(i),/str
for i=0,n_elements(idlsav)-1 do $
  print,i,'  ',idlsav(i).datatype,'  ',idlsav(i).varname

fnam=strmid(flname,0,strlen(flname)-3)+'fainberg_ascii'

openw,lun,fnam,/get_lun
datatype=['swe_moments','swe_ionkp','isee_moments']
for itype=0,n_elements(datatype)-1 do begin
  wdtp=where(idlsav.datatype eq datatype(itype),nvars)
  if wdtp(0) eq -1 then goto,endofloop
  print,' ' & printf,lun,' '
  print,datatype(itype)
  printf,lun,datatype(itype)
  print,'hour of day  ',idlsav(wdtp(0:nvars-1)).varname
  printf,lun,'hour of day  ',idlsav(wdtp(0:nvars-1)).varname
  wne0=where(idlsav(wdtp(0)).x ne 0,nrows)
  for j=0,nrows-1 do $
        print,idlsav(wdtp(0)).x(j),idlsav(wdtp(0:nvars-1)).y(j)
  for j=0,nrows-1 do $
        printf,lun,idlsav(wdtp(0)).x(j),idlsav(wdtp(0:nvars-1)).y(j),$
        format='(10f15.4)'        
  endofloop:  
endfor  
free_lun,lun
print,'ascii file ',fnam,' printed' 
goto,start
end
