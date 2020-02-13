;-------------------- files_by_date_plt --------------------------------------

pro files_by_date_plt,thisdate,datyp,dir,fltr,lbl,flnm

hardcopy=0

start:

help,flnm

pb5=lonarr(3,n_elements(flnm))
for i=0,n_elements(flnm)-1 do begin
  if flnm(i) ne '' then begin
    case datyp of
    'swe_moments': begin
      pb5(*,i)=ymd_pb5(long(strmid(flnm(i),strlen(dir),8)))
      
                   endcase
    
    'swe_moments_cdf': begin
      pb5(*,i)=ymd_pb5(long(strmid(flnm(i),strlen(dir)+10,8)))
      
                   endcase
                          
    'swe_fpitch': $
     if strmid(flnm(i),strlen(flnm(i))-3,3) eq 'pit' then $ 
        pb5(*,i)=ymd_pb5(long(strmid(flnm(i),strlen(dir),8))) else $       
        pb5(*,i)=ymd_pb5(19000000l+long(strmid(flnm(i),strlen(dir),6))) 
         
    'swe_strahl': $
     if strmid(flnm(i),strlen(flnm(i))-9,2) eq 'v4' then $
       pb5(*,i)=ymd_pb5(19000000l+long(strmid(flnm(i),strlen(dir),6))) else $
       pb5(*,i)=ymd_pb5(long(strmid(flnm(i),strlen(dir),8))) 
       
    'swe_fparaperp': pb5(*,i)=ymd_pb5(long(strmid(flnm(i),strlen(dir),8)))
       
    endcase
  endif
   
  ;print,flnm(i),'  ',pb5(*,i) 
endfor

if hardcopy eq 0 then window,0,xsize=800,ysize=800

year=long([1995,1996,1997,1998,1999,2000,2001])  ;,2002])
ndysyr=   [ 365, 366, 365, 365, 365, 366, 365]  ;,365]
!p.multi=[0,0,n_elements(year),0,0]
mos=['J','F','M','A','M','J','J','A','S','O','N','D']
cdam=  [1,32,60,91,121,152,182,213,244,274,305,335]
cdamly=[1,32,61,92,122,153,183,214,245,275,306,336]

title=strarr(n_elements(year))
title(0)='Data Coverage   '+datyp+' ('+fltr+')   '+lbl
  ;string(thisdate,format='(i8)')
for i=0,n_elements(year)-1 do begin
 
  xrange=([1,ndysyr(i)]) & xticks=13
  if fix(float(year(i))/4) ne float(year(i))/4 then $
    xtickv=[cdam,365] else xtickv=[cdamly,366]
    
  yrange=[0,1] 
  plot,/nodata,xrange,yrange,xstyle=4,ystyle=4,title=title(i),$
    charsize=2.0
  axis,xrange(0),yrange(0),/data,xaxis=0,xrange=xrange,xticks=xticks,$
    charsize=2.0,xstyle=1,xtickv=xtickv,xticklen=-0.20,/save
  if fix(float(year(i))/4) ne float(year(i))/4 then $
    for j=0,11 do xyouts,/data,cdam(j),-1.20,mos(j) else $
    for j=0,11 do xyouts,/data,cdamly(j),-1.20,mos(j)
  xyouts,/data,180,-1.50,string(year(i),format='(i4)')
       
  w=where(pb5(0,*) eq year(i),nw) ;pb5 index in year(i) for which there is data
  if nw gt 0 then begin
    s=w-shift(w,1)
    b=where(s gt 1,nb)  ;index of w for which there are gaps in sequence of w
    ymin=0.
    ymax=0.5
    if nb gt 0 then begin
      x=[pb5(1,w(0)),reform(pb5(1,w(0:b(0)-1))),pb5(1,w(b(0)-1))]
      y=[ymin,ymax+fltarr(b(0)-1 -0 +1),ymin] 
      polyfill,x,y 
      
      for k=1,nb-1 do begin
        x=[pb5(1,w(b(k-1))),reform(pb5(1,w(b(k-1):b(k)-1))),pb5(1,w(b(k)-1))]
        y=[ymin,ymax+fltarr(b(k)-1 -b(k-1) +1),ymin] 
        polyfill,x,y 
      endfor
      
      x=[pb5(1,w(b(nb-1))),reform(pb5(1,w(b(nb-1):nw-1))),pb5(1,w(nw-1))]
      y=[ymin,ymax+fltarr(nw-1 -b(nb-1) +1),ymin] 
      polyfill,x,y     
    endif else begin
      x=[pb5(1,w(0)),reform(pb5(1,w(0):w(nw-1))),pb5(1,w(nw-1))]
      y=[ymin,ymax+fltarr(nw),ymin]
      polyfill,x,y
    endelse 
     
  endif
 
  
endfor
!p.multi=0

if hardcopy then begin
  device,/close
  set_plot,'x'
  ;clrtbl_indx
  hardcopy=0
  print,'hardcopy file: ',pltfil
  stop 
endif

set_plot,'ps',/interpolate
;clrtbl_indx,/hardcopy 
pltfil=getenv('IDLSAV')+'files_by_date_'+datyp+'.ps' 
device,/inches,xoffset=0.75,yoffset=1.,xsize=7.,ysize=9.5,filename=pltfil
     
hardcopy=1
goto,start

end


;---------------- MAIN: files_by_date ----------------------------------------

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
print,'enter data type index'
answ='' & read,answ 
if long(answ) gt 4 then $
stop,'wrong data type' 

datyp=list(fix(answ)) 
w=where(datapaths.name eq datyp)
if datyp ne 'swe_moments_cdf' then dir=datapaths(w(0)).list(0) else $
  dir='/data1/swe/moments_cdf/'       
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

if lprf then openw,lun,getenv('IDLSAV')+'files_by_date_'+datyp,/get_lun
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
    fltr0='_sv14.mom'  ;'_v13.mom'
    fltr1=''
    dir='/mnt/leprjf_data3/swe/moments/'
    arg=dir+date+fltr0
    result=findfile(arg)
    if result(0) ne '' then flnm(k)=result(0) ;else begin
      ;fltr1='*_v05.mom'
      ;arg=dir+strmid(date,2,6)+fltr1
      ;result=findfile(arg)
      ;if result(0) ne '' then $
      ;  flnm(k)=$
      ;  dir+'19'+strmid(result(0),strlen(dir),strlen(result(0))-strlen(dir)) $
      ;  else flnm(k)=''
    ;endelse
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
    print,k,' ',date,' ',flnm(k)
    if flnm(k) ne '' then begin
      kfl=kfl+1
      files(kfl)=flnm(k)
    endif  
  endif
   
  if lprf then printf,lun,k,' ',date,' ',flnm(k)
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
for i=0,n_elements(files)-1 do print,i,'  ',files(i)
stop
files_by_date_plt,thisdate,datyp,dir,fltr,lbl,flnm

end