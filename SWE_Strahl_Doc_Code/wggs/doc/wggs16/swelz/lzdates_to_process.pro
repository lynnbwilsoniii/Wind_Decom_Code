;find which level zero dates to process for datype= 'mom', 'pitch', 'strahl'
;also, create the necessary script_file 'lz'+datype+'_dates to run 'lz'+datype

define_widgets
setpaths

datatype=['mom','pitch','strahl','pitchavg']
print,' ' & print,'Data types:'
for i=0,n_elements(datatype)-1 do print, datatype(i)
print,' ' & print,'Enter data type'
answ='' & read,answ
datype=answ
print,' ' & print,'Datatype '+answ+ ' selected.'
print,'Hit return to continue.'
ans='' & read,answ & if answ ne '' then stop

lzpath=getenv('SWE_LZPATH')  
arg=lzpath+'*.dat'
lzfiles=findfile(arg,count=count)

if count gt 0 then begin
  lzdates=strarr(count)
  for i=0,count-1 do begin
    if strmid(lzfiles(i),strlen(lzpath),9) eq 'wi_lz_swe' then $
      lzdates(i)=strmid(lzfiles(i),strlen(lzpath)+10,8) $
    else if strmid(lzfiles(i),strlen(lzpath),1) eq '9' then $  
      lzdates(i)='19'+strmid(lzfiles(i),strlen(lzpath),6) $
    else if strmid(lzfiles(i),strlen(lzpath),1) eq '0' then $  
      lzdates(i)='20'+strmid(lzfiles(i),strlen(lzpath),6) 
  endfor     
  print,'lz dates:'
  for i=0,count-1 do print,i,'  ',lzdates(i),'  ',lzfiles(i)

  dates_tobe_processed=lzdates
  number=count

  print,'dates for which '+datype+' files are to be processed:'
  for i=0,number-1 do print,i,'  ',dates_tobe_processed(i)
  print,' '
  print,' '
  print,number,' days from the lz directory '+lzpath+'  have been selected'
  dateflnm=getenv('IDLSAV')+'lz'+datype+'_dates'
  print,$
    'Do you want to create the date script, '+dateflnm+' ? (y/n)'
  answ='' & read,answ 
  if answ eq 'y' then begin

    openw,lun,dateflnm,/get_lun
    for i=0,number-1 do printf,lun,dates_tobe_processed(i)
    free_lun,lun
    print,' '
    print,'The file '+dateflnm+' has been created with these dates:'
    for i=0,number-1 do print,dates_tobe_processed(i)
    print,' ' & print,'Now you can .r lz'+datype

  endif else stop

  
endif

end    
    
