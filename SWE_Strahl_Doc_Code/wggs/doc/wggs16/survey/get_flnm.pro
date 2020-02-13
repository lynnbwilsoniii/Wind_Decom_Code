function get_flnm,type,dir,fltr1,fltr2,indate,err=err,lpr=lpr

err=''
flnm=''
if keyword_set(lpr) eq 0 then lpr=0 else lpr=1

case type of

'swe_moments' : begin
   ;---check to see if the directory contains *.cdf files
   if strmid(dir,strlen(dir)-4,3) eq 'cdf' then begin
     arg=dir+'wi_h0_swe_'+indate+'_v*.cdf'
     result=findfile(arg,count=count)
     if count gt 0 then begin
       version=strarr(n_elements(result))
       for i=0,n_elements(result)-1 do $
         version(i)=strmid(result(i),strlen(result(i))-6,2)
       mx=max(version,mostrecent)
       if n_elements(version) ge 2 then begin
         ;if all the same version, then select the last file with given version
         wdiff=where(version ne version(0))
         if wdiff(0) eq -1 then mostrecent=n_elements(version)-1
       endif  
       flnm=result(mostrecent)   ;most recent version
     endif else err=type+' cdf file not found'    
   endif else begin
     ;---look first for an idlsave file to restore
     arg=dir+'*'+fltr1+'*'+strmid(indate,2,6)+'_s*'+fltr2
     if lpr then print,type,' file search: ',arg
     result=findfile(arg,count=count)
     if count gt 0 then begin
       version=strarr(n_elements(result))
       for i=0,n_elements(result)-1 do $
         version(i)=strmid(result(i),strlen(result(i))-6,2)
       mx=max(version,mostrecent)
       if n_elements(version) ge 2 then begin
         ;if all the same version, then select the last file with given version
         wdiff=where(version ne version(0))
         if wdiff(0) eq -1 then mostrecent=n_elements(version)-1
         ;if version(0) eq version(1) then mostrecent=1
       endif  
       flnm=result(mostrecent)   ;most recent version  
     endif else begin  ;no idlsave file found for given date 
       arg=dir+'*'+fltr1+'*'+strmid(indate,2,6)+'_'+fltr2
       if lpr then print,type,' file search: ',arg
       result=findfile(arg,count=count)
       if count gt 0 then begin
         version=strarr(n_elements(result))
         for i=0,n_elements(result)-1 do $
           version(i)=strmid(result(i),strlen(result(i))-6,2)
         mx=max(version,mostrecent)
         if n_elements(version) ge 2 then begin
           ;if all the same version, then select the last file with given vers
           wdiff=where(version ne version(0))
           if wdiff(0) eq -1 then mostrecent=n_elements(version)-1
           ;if version(0) eq version(1) then mostrecent=1
         endif  
         flnm=result(mostrecent)   ;most recent version  
       endif else err=type+' file not found'
     endelse
   endelse  
            endcase

'swe_fpitch' : begin
  arg=dir+'*'+fltr1+'*'+strmid(indate,2,6)+'*'+fltr2
  if lpr then print,type,' file search: ',arg
  result=findfile(arg,count=count)
  if count gt 0 then begin
    version=strarr(n_elements(result))
    for i=0,n_elements(result)-1 do $
      version(i)=strmid(result(i),strlen(result(i))-7,1)
    mx=max(version,mostrecent)
    flnm=result(mostrecent)   ;most recent version
  endif else begin
     arg=dir+'*'+fltr1+'*'+strmid(indate,0,8)+'*.pit'
     if lpr then print,type,' file search: ',arg
     result=findfile(arg,count=count)
     if count gt 0 then begin
       version=strarr(n_elements(result))
       for i=0,n_elements(result)-1 do $
         version(i)=strmid(result(i),strlen(result(i))-5,1)
       mx=max(version,mostrecent)
       flnm=result(mostrecent)   ;most recent version
     endif else err=type+' file not found'
  endelse
          endcase

'swelz': begin 

   arg=dir+'*'+fltr1+'*'+strmid(indate,2,6)+'*'+fltr2
   if lpr then print,type,' file search: ',arg
   result=findfile(arg,count=count)
   if count gt 0 then begin
     flnm=result(count-1)   ;most recent version
     ;print,flnm
   endif else begin
     err=type+' file not found'
     print,'get_flnm: ',err
   endelse  
      endcase
                            
'wav_tnr': begin 
   arg=dir+'*'+fltr1+'*'+strmid(indate,0,8)+'*'+fltr2
   if lpr then print,type,' file search: ',arg
   result=findfile(arg,count=count)
   
   if count gt 0 then begin
     flnm=result(count-1)   ;most recent version
     ;print,flnm
   endif else begin
     err=type+' file not found'
     print,'get_flnm: ',err
   endelse  
      endcase

'wav_hrtnr': begin 
   arg=dir+'*'+fltr1+'*'+strmid(indate,0,8)+'*'+fltr2
   if lpr then print,type,' file search: ',arg
   result=findfile(arg,count=count)
   if count gt 0 then begin
     flnm=result(count-1)   ;most recent version
     ;print,flnm
   endif else begin
     err=type+' file not found'
     print,'get_flnm: ',err
   endelse  
      endcase
            
else: begin 
   arg=dir+'*'+fltr1+'*'+strmid(indate,2,6)+'_*'+fltr2
   if lpr then print,type,' file search: ',arg
   result=findfile(arg,count=count)
   if count gt 0 then begin
     flnm=result(count-1)   ;most recent version
     ;print,flnm
   endif else begin
     err=type+' file not found'
     print,'get_flnm: ',err
   endelse 
      endcase      
endcase
      
return,flnm

end




