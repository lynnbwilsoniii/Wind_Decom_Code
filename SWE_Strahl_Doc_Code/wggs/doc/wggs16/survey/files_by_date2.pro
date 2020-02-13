
;---------------- MAIN: files_by_date2 ----------------------------------------

;lists most recent version of (moments, pitch, strahl) file by date


lpr=1    ;=1 prints to screen
lprf=1   ;=1 writes to file

date_begin=19950101l
date_end=20011231l  ;20021231l  
;setpaths
restore,getenv('IDLSAV')+'datapaths'

list=['swe_moments', 'swe_fpitch', 'swe_strahl', 'swe_fparaperp', $
      'swe_moments_cdf']
vers=['*',          '*',          '*',          '*', '*'          ]

files=strarr(3000)

print,'data types: '
for i=0,n_elements(list)-1 do print,list(i),'  ',vers(i),'  ',i
;print,'enter data type index'
;answ='' & read,answ 
;if long(answ) gt 4 then $
;stop,'wrong data type' 
answ=0

datyp=list(fix(answ)) 
w=where(datapaths.name eq datyp)
if datyp ne 'swe_moments_cdf' then dir=datapaths(w(0)).list(0) else $
  dir='/data1/swe/moments_cdf/'
print,'data type selected: ',datyp
         
case datyp of
'swe_moments': begin
   version=vers(0)
   fltr=version+'.mom'
           endcase
           
'swe_fpitch': begin
   version=vers(1)
   fltr=version+'.pit*'
         endcase  
         
'swe_strahl': begin
   version=vers(2)
   fltr=version+'.strahl'
         endcase 
         
'swe_fparaperp' : begin
   version=vers(3)
   fltr=version+'.pitavg'
         endcase 

'swe_moments_cdf' : begin 
   version=vers(4)
   fltr=version+'.cdf'
         endcase                                    
endcase

pb5ref=ymd_pb5(19941130l)
elapsec_begin=pb5_elapsec(ymd_pb5(date_begin),pb5ref)
elapsec_end=pb5_elapsec(ymd_pb5(date_end),pb5ref)

if lprf then openw,lun,getenv('IDLSAV')+'files_by_date2_'+datyp,/get_lun
;find current date
  y=strmid(systime(0),20,4)
  mos=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  m=string(where(mos eq strmid(systime(0),4,3))+1,format='(i2)')
  d=strmid(systime(0),8,2)
  thisdate=y*10000l+m*100l+d*1l
  print,'thisdate ',thisdate
  if lprf then printf,lun,'thisdate ',thisdate

lbl=''

flnm=strarr(4000)  
k=-1
kfl=-1
elapsec=elapsec_begin
while elapsec le elapsec_end do begin
  k=k+1
  elapsec=elapsec_begin+k*double(86400l)
  pb5=elapsec_pb5(elapsec,pb5ref)
  date=string(pb5_ymd(pb5),format='(i8)')
  case datyp of
  
  'swe_moments' : begin
    fltr0='_v13.mom' ;'_sv14.mom'  ;
    fltr1=''
    dir='/mnt/leprjf_data7/swe/moments/'
    arg=dir+date+fltr0
    result=findfile(arg,count=count)
    if result(0) ne '' then flnm(k)=result(0) else begin
      fltr1='*_v05.mom'
      arg=dir+strmid(date,2,6)+fltr1
      result=findfile(arg,count=count)
      if result(0) ne '' then $
        flnm(k)=$
        dir+'19'+strmid(result(0),strlen(dir),strlen(result(0))-strlen(dir)) $
        else flnm(k)=''
    endelse
    fltr=fltr0+' , '+fltr1
    lbl=' modes 1, 4, and 6'  ;' modes 1, 4, 6, and mode 2'  
  endcase
  
  'swe_moments_cdf': begin
    fltr0='*.cdf'
    arg=dir+'wi_h0_swe_'+date+fltr0
    result=findfile(arg)
    flnm(k)=result(0)
    fltr=fltr0  
  endcase
  
  else: flnm(k)=get_flnm(datyp,dir,'',fltr,date,err=err) 
  endcase
              
  if lpr then begin
    print,k,' ',string(count,format='(i1)'),' ',date,' ',flnm(k)
    if flnm(k) ne '' then begin
      kfl=kfl+1
      files(kfl)=flnm(k)
    endif  
  endif
   
  if lprf then printf,lun,k,' ',string(count,format='(i1)'),' ',date,' ',flnm(k)
  ;if err ne '' then begin
  ;  if lpr then print,k,' ',date
  ;  if lprf then printf,lun,k,' ',date 
  ;endif
  
  skipk=90
  if long(k/skipk)*skipk eq k and lpr eq 0 then  print,date
  
endwhile
flnm=flnm(0:k)

if lprf then free_lun,lun 

print,'file complete: ',getenv('IDLSAV')+'files_by_date_'+datyp
print,'number of files ',kfl
files=files(0:kfl)
;for i=0,n_elements(files)-1 do print,i,'  ',files(i)


end