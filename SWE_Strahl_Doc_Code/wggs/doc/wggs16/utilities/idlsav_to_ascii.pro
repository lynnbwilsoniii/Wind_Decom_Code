;creates an ascii file of plot variables from an idlsave file


;set the data path
  ;setenv,'IDLSAV=/home/u3rjf/idlsav/'
start:
;the idlsav data filename
  savpath=getenv('IDLSAV')
  flname=pickfile(/read,get_path=savpath,path=savpath,$
               filter='*idlsave.dat',$
               title='IDLsav Files')

  ;flname='19941130_idlsave.dat'

restore,flname
help,idlsav
for i=0,n_elements(idlsav)-1 do help,idlsav(i),/str
for i=0,n_elements(idlsav)-1 do $
  print,i,'  ',idlsav(i).datatype,'  ',idlsav(i).varname

wdifferent=where(idlsav.datatype ne idlsav(0).datatype)
if wdifferent(0) eq -1 then begin   ;all the same data type and same time scale
  fnam=strmid(flname,0,strlen(flname)-3)+'ascii'
  openw,lun,fnam,/get_lun
  printf,lun,idlsav(0).refpb5
  printf,lun,'number ',n_elements(idlsav(0).x(where(idlsav(0).x ne 0)))
  printf,lun,'hour of day'
  printf,lun,idlsav(0).x(where(idlsav(0).x ne 0))
  for i=0,n_elements(idlsav)-1 do begin
    printf,lun,idlsav(i).varname
    printf,lun,idlsav(i).y(where(idlsav(0).x ne 0))
  endfor

  free_lun,lun
  print,'ascii file ',fnam,' printed'
endif

;print,'hit return to continue'
;answ='' & read,answ & if answ eq '' then $
goto,start 
end
