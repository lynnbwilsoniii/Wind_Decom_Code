

;============================= input =================================

;procedure to read input survey data
   
pro input,whichdata,err=err


common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common shared,d
common wstuff,wst
 
print,wst.indate
print,'input: whichdata ', whichdata

err=''

wst.surveydate=''
idatype_swe_moments=where(d.datype eq 'swe_moments')
idatype_swe_fpitch=where(d.datype eq 'swe_fpitch')
case wst.mfilter of
'Latest' : d.fltr(idatype_swe_moments)='*.mom'  ;'*v*.mom'
'Special' : d.fltr(idatype_swe_moments)='*'+getenv('SPECIAL_MOMLABEL')+'*.mom'
else : d.fltr(idatype_swe_moments)='*'+wst.mfilter+'.mom'
endcase

case wst.pfilter of
'Latest' : d.fltr(idatype_swe_fpitch)='*.pit*'  ;'*v*.pitch'
'Special' : d.fltr(idatype_swe_fpitch)='*'+getenv('SPECIAL_PITLABEL')+'*.pitch'
else : d.fltr(idatype_swe_fpitch)='*'+wst.pfilter+'.pitch'
endcase

d.flnm=strarr(d.ndvar)
d.datype_input=-1+intarr(d.ndvar) 
d.datype_input(whichdata)=1

;initialize indices
 d.ndx=intarr(2,d.ndvar)
 d.ndx_orig=intarr(2,d.ndvar)
 d.ndx_last=intarr(2,d.ndvar)
 d.ndx_buff=intarr(2,d.ndvar)
 d.ndx_buff2=intarr(2,d.ndvar)

;select survey data file(s)
  if wst.date_file eq 'Date' and keyword_set(wst.indate) eq 0 $
  then wst.date_file='File'
  nullstr=''
  date=''

tjd0=10000l
for i=0,n_elements(whichdata)-1 do begin
  idatyp=whichdata(i)
  if d.datype(idatyp) eq 'mfi_mag3s' or $
     d.datype(idatyp) eq 'swe_ionkp' or $
     d.datype(idatyp) eq 'mfi_magkp' or $
     d.datype(idatyp) eq 'wind_orbit' $
  then date_file = 'Date' $
  else if d.datype(idatyp) eq 'swe_redfcuts' then date_file= 'File' $
  else date_file=wst.date_file
  
  if d.datype(idatyp) eq 'wind_orbit' then fltr1='or' else fltr1=nullstr
       
  case date_file of
    'File' : begin
     d.flnm(idatyp)=pickfile(/read,path=d.dir(idatyp),get_path=dir,$
     filter=d.fltr(idatyp),$
     title=d.wilabl(idatyp)+' Data Files',/must_exist)
     if strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),2) eq '19' or $
     strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),2) eq '20' then $
     wst.indate=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),8) $
     else $
     wst.indate='19'+strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),6)
             endcase        
    'Date' : begin
    
             ; 'swe_summary' data type uses yearly files starting at January 1.
             if (d.datype[idatyp] eq 'swe_summary') then begin ;   SWE_SUMMARY.
                indate = strmid(wst.indate,0,4)+'0101' & fltr1 = 'ss_'
             endif else indate = wst.indate ;                        Otherwise.

             d.flnm(idatyp)=$
             get_flnm(d.datype(idatyp),$
               d.dir(idatyp),fltr1,d.fltr(idatyp),indate,Err=err,/Lpr)
               
             if d.flnm(idatyp) eq '' and $
             d.datype(idatyp) eq 'swe_moments' then begin
               d.dir(idatyp)=getenv('MOMPATH2')   ;try secondary moments path
               d.flnm(idatyp)=$
                 get_flnm(d.datype(idatyp),$
                 d.dir(idatyp),fltr1,d.fltr(idatyp),wst.indate,err=err)
               if d.flnm(idatyp) eq '' then begin
                 err=d.datype(idatyp)+' file not found'
                 print,err
               endif  
             endif
             endcase
  endcase        
  
  ;--check for SWE VEIS mode2 date
  if d.datype(idatyp) eq 'swe_moments' then begin
    restore,getenv('SWEDATLIB')+'mode2_dates.txt'
    wm2=where(date_mode2 eq wst.indate)
    if wm2(0) ne -1 then begin
      print,' '
      print,'CAUTION: SWE mode2 moments. ' +$
      'Heat flux may be contaminated by sun glint.'
      print,' '
    endif
  endif
  
  if d.datype(idatyp) eq 'isee_moments' and $
  d.flnm(idatyp) ne '' then begin   ;ISEE moments
       if strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),7) eq $
       'iseeph2' then begin
         y=1900l+strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp)))+8,2)
         ddy=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp)))+10,3)
       endif else if $
       strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp)))+8,8) eq $
       'isee1ele' then begin
         y=1900l+strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),2)
         ddy=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp)))+2,3)
       endif
       date=string(ymd([y,ddy+1,0l]),format='(i8)')
       wst.indate=date
       isee_moments_read
       tpb5ref=[d.iseemdat(0).tpb5(0),d.iseemdat(0).tpb5(1),0l]
       iseerefsec=isee_pb5_sec(tpb5ref)
       d.refsec=iseerefsec
       wst.surveydate=wst.indate
       wst.lzindate=wst.indate
       return
  endif else if d.datype(idatyp) eq 'swe_moments' and $
  d.flnm(idatyp) ne '' then begin ;SWE moments
       
       momcdf=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-3,3) eq 'cdf'
       if momcdf then begin
         call_procedure,'swe_moments_read_cdf',tjd0_thisfile=tjd0_thisfile
       endif else begin
         ver=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-6,2)
         if ver eq '07' or ver eq '08' then $
           call_procedure,'swe_moments_read_v07',tjd0_thisfile=tjd0_thisfile $
         else if ver ge '09' and ver le '12' then $
           call_procedure,'swe_moments_read_v09',tjd0_thisfile=tjd0_thisfile $
         else if ver eq '13' or ver eq '14' then $
           call_procedure,'swe_moments_read_v13',tjd0_thisfile=tjd0_thisfile $  
         else call_procedure,'swe_moments_read',tjd0_thisfile=tjd0_thisfile
         tjd0=tjd0_thisfile < tjd0
       endelse  
  endif else if d.datype(idatyp) eq 'swe_fpitch' $
                 and d.flnm(idatyp) ne '' then begin   ;SWE pitch angle
      
        if strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-3,3) eq 'pit' then $ 
        ver=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-5,1) else $       
        ver=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-7,1)        
        if ver ge '5' then $
         call_procedure,'swe_fpitch_read_v05',tjd0_thisfile=tjd0_thisfile else $
         call_procedure,'swe_fpitch_read',tjd0_thisfile=tjd0_thisfile
        tjd0=tjd0_thisfile < tjd0
        
  endif else if d.datype(idatyp) eq 'swe_strahl' or $
                d.datype(idatyp) eq 'swe_strahlen' $
                 and d.flnm(idatyp) ne '' then begin   ;SWE strahl data       
     ver=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-8,1)
     if ver eq '7' then begin
       if d.datype(idatyp) eq 'swe_strahl' then $
       call_procedure,'swe_strahl_read_v7',tjd0_thisfile=tjd0_thisfile $
       else if d.datype(idatyp) eq 'swe_strahlen' then $
       call_procedure,'swe_strahlen_read_v7',tjd0_thisfile=tjd0_thisfile 
     endif else begin 
       ;if d.datype(idatyp) eq 'swe_strahl' then $
       ;    wst.number_dayskp=wst.number_days < wst.maxnumber_dayskp
       if d.datype(idatyp) eq 'swe_strahl' then $
          call_procedure,'swe_strahl_read',tjd0_thisfile=tjd0_thisfile $
       else if d.datype(idatyp) eq 'swe_strahlen' then $
          call_procedure,'swe_strahlen_read',tjd0_thisfile=tjd0_thisfile 
     endelse  
     tjd0=tjd0_thisfile < tjd0
          
  endif else if d.flnm(idatyp) ne '' then begin 
        call_procedure,d.datype(idatyp)+'_read',tjd0_thisfile=tjd0_thisfile
        tjd0=tjd0_thisfile < tjd0
        
  endif             
  
  ;WIND reference time
  if d.datype(idatyp) ne 'isee_moments' then d.refsec=tjd0*86400.d
   
     
      
endfor

wst.surveydate=wst.indate
  
  
      
;get WIND orbit data
  oapath=getenv('WIND_OAPATH')  
  arg=oapath+'*'+'or_def_'+string(wst.surveydate,format='(i8)')+'*'+'.cdf'
  result=findfile(arg,count=count)
  orbfile='' & wst.orbdate=''
  if count eq 0 then begin
    arg=oapath+'*'+'or_pre_'+string(wst.surveydate,format='(i8)')+'*'+'.cdf'
    result=findfile(arg,count=count)
  endif
     if count ne 0 then begin
       orbfile=result(count-1) ;assumes highest count is latest version 
       wst.orbdate= strmid(orbfile,strlen(orbfile)-16,8)
       print,'orb file  ',orbfile
       print,'wst.orbdate ',wst.orbdate 
       ;open orb file 
         lunat=1      
         openr,lunat,orbfile 
       ;get orbit 10 min data
         loadcdf,orbfile,'Time_PB5',tpb5_orb   
         loadcdf,orbfile,'GSE_POS',gse_pos
         ;loadcdf,orbfile,'GSE_VEL',gse_vel
         close,lunat
         gse_vel=0
       ;verify that orbit array has dimension = 144 (24hrs * 60min / 10min) x 3 
         if n_elements(gse_pos) ne 144*3 then $
         print,'input: orbit array time dimension not = 144'
         ;stop,'input: orbit array has wrong dimension'
     endif

;get WIND attitude data 

    oapath=getenv('WIND_OAPATH')
    arg=oapath+'*'+'at_def_'+string(wst.surveydate,format='(i8)')+'*'+'.cdf'
    result=findfile(arg,count=count)
    atfile='' & wst.atdate=''
    if count eq 0 then begin
      arg=oapath+'*'+'at_pre_'+string(wst.surveydate,format='(i8)')+'*'+'.cdf'
      result=findfile(arg,count=count)
    endif
    if count ne 0 then begin
      atfile=result(count-1) ;assumes highest count is latest version 
      wst.atdate= strmid(atfile,strlen(atfile)-16,8)
      print,'at file ',atfile
      print,'wst.atdate ',wst.atdate 
      ;open at file 
        lunat=1      
        openr,lunat,atfile;,/get_lun 
      ;get attitude 10 min data
        loadcdf,atfile,'Time_PB5',tpb5_at   
        loadcdf,atfile,'GSE_R_ASCENSION',gse_ra
        loadcdf,atfile,'GSE_DECLINATION',gse_dec
        close,lunat
      ;verify that attitude array has dimension = 144 (24hrs * 60min / 10min) 
         ;if n_elements(gse_ra) ne 144 then $
         ;stop,'input: attitude array has wrong dimension'
    endif else if $
      (count eq 0 or wst.atdate ne wst.surveydate) then begin
      print,'wst.atdate ' & print,wst.atdate
      print,'survey and attitude file dates do not match, ',$
             'or attitude file does not exist'
      print,'Do you want to continue? (y/n)'
      print,'(If you continue, the payload to gse coord transformation ',$
             'will be accomplished by a 180 deg rotation about the ',$
             'Xpayload axis instead.)' 
      answ='' & read,answ & if answ eq 'n' then begin & return & endif
    endif 
            
print,' '
print,'Survey data have been read.'
print,'Ready to proceed.'

;make list of dates corresponding to the list of survey data dates
dateref=long(wst.indate)
pb5ref=ymd_pb5(dateref)
elapsecref=pb5_elapsec(pb5ref,pb5ref)
;number_days=max([wst.number_days,wst.number_dayskp])
;for i=0,number_days-1 do begin
for i=0,wst.number_days-1 do begin
  pb5=elapsec_pb5(elapsecref+i*86400.d,pb5ref)
  wst.lzdays(i)=string(pb5_ymd(pb5),format='(i8)') 
endfor

wst.lzindate=wst.lzdays(0)    

end


