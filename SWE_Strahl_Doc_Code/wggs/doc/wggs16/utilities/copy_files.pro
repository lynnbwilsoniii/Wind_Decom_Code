dates=['960525','960526','960527','960528','960529',$
       '960530','960610','960701','960702','960806',$
       '960807','960808','960809','961111','970125',$
       '970203','970210','970211','970314','970327',$
       '970331','970410','970411','970412','970109',$
       '970110','970111','970106','961222','961223',$
       '961224','961225','961226','961201','961128',$
       '961117','961025']

query=0

outdir='/export/ftp/pub/exports/ferrugia/'
for i=24,n_elements(dates)-1 do begin

  ;-------------------- copy moments files -----------------------------------
  arg_mom=getenv('MOMPATH')+'*'+dates(i)+'*v5.mom'
  result=findfile(arg_mom,count=count)
  if count eq 0 then begin
    arg_mom=getenv('MOMPATH')+'*'+dates(i)+'*v4.mom' 
    result=findfile(arg_mom,count=count)
    if count eq 0 then print,'no .mom file found'
  endif 
  
  if query and count ne 0 then begin
    print,result
    print,'cp '+result(count-1)+' '+outdir+$
      strmid(result(count-1),strlen(getenv('MOMPATH')),$
      strlen(result(count-1))-strlen(getenv('MOMPATH')))
    print,'hit return to continue'
    answ='' & read,answ & if answ ne '' then stop
  endif

  if count ne 0 then print,' ' & if count ne 0 then print,result(count-1)    
  if count ne 0 then spawn,'cp '+result(count-1)+' '+outdir+$
    strmid(result(count-1),strlen(getenv('MOMPATH')),$
    strlen(result(count-1))-strlen(getenv('MOMPATH')))
    
  
  ;------------------------ copy ionkp files ---------------------------------
  arg_ionkp=getenv('IONKPPATH')+'*'+dates(i)+'*cdf'
  result=findfile(arg_ionkp,count=count)
  if count eq 0 then stop,'no ionkp file found'  
   
  if query and count ne 0 then begin
    print,result
    print,'cp '+result(count-1)+' '+outdir+$
      strmid(result(count-1),strlen(getenv('MOMPATH')),$
      strlen(result(count-1))-strlen(getenv('MOMPATH')))
    print,'hit return to continue'
    answ='' & read,answ & if answ ne '' then stop
  endif

  if count ne 0 then print,' ' & if count ne 0 then print,result(count-1)  
  if count ne 0 then spawn,'cp '+result(count-1)+' '+outdir+$
    strmid(result(count-1),strlen(getenv('IONKPPATH')),$
    strlen(result(count-1))-strlen(getenv('IONKPPATH')))
    
    
    
  ;------------------------ copy oa files -----------------------------------    
  arg_or=getenv('OAPATH')+'wi_or*'+dates(i)+'*cdf'
  result=findfile(arg_or,count=count)
  if count eq 0 then stop,'no orbit file found'
  
  if query and count ne 0 then begin
    print,result
    print,'cp '+result(count-1)+' '+outdir+$
      strmid(result(count-1),strlen(getenv('MOMPATH')),$
      strlen(result(count-1))-strlen(getenv('MOMPATH')))
    print,'hit return to continue'
    answ='' & read,answ & if answ ne '' then stop
  endif

  if count ne 0 then print,' ' & if count ne 0 then print,result(count-1)  
  if count ne 0 then spawn,'cp '+result(count-1)+' '+outdir+$
    strmid(result(count-1),strlen(getenv('OAPATH')),$
    strlen(result(count-1))-strlen(getenv('OAPATH')))
    
    
  arg_at=getenv('OAPATH')+'wi_at*'+dates(i)+'*cdf'
  result=findfile(arg_at,count=count)
  if count eq 0 then stop,'no attitude file found'

  if query and count ne 0 then begin  
    print,result
    print,'cp '+result(count-1)+' '+outdir+$
      strmid(result(count-1),strlen(getenv('MOMPATH')),$
      strlen(result(count-1))-strlen(getenv('MOMPATH')))
    print,'hit return to continue'
    answ='' & read,answ & if answ ne '' then stop
  endif

  if count ne 0 then print,' ' & if count ne 0 then print,result(count-1)  
  if count ne 0 then spawn,'cp '+result(count-1)+' '+outdir+$
    strmid(result(count-1),strlen(getenv('OAPATH')),$
    strlen(result(count-1))-strlen(getenv('OAPATH')))
    
endfor

end
  
    