pro wav_tnr_read,tjd0_thisfile=tjd0

;reads Mike Kaiser's WAVES 1min dynamic spectrum files

common shared,d
common wstuff,wst

idatype=where(d.datype eq 'wav_tnr')
flnm=d.flnm(idatype)

restore,filename=flnm
s=size(arrayb)
;s(1) = 1441  , 0:1439 = minutes of day, 1440 = background 
;s(2) = 96    ,0:95 = relative intensity channels
bckgrnd=reform(arrayb(s(1)-1,*))
ntim=long(s(1)-1)
nchan=long(s(2))
array=arrayb(0:ntim-1,*)
date=long(strmid(flnm,strlen(flnm)-12,8))
pb5=ymd_pb5(date)
year=replicate(pb5(0),ntim)
day=replicate(pb5(1),ntim)
msec=lindgen(ntim)*long(60000)

;if multiple days 
if wst.number_days gt 1 then begin
  for inxt=1,wst.number_days-1 do begin
    pb5=ymd_pb5(long(date))
    sec=pb5_sec(pb5)
    pb5_next=sec_pb5(sec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm('wav_tnr',getenv(d.pathenv(idatype)),nullstr,d.fltr(idatype),$
      date_next,err=err)
    if err ne '' then goto,getoutloop  
    
    restore,filename=file_next
    print,'end reading next file ',file_next
    s=size(arrayb)
    ;s(1) = 1441  , 0:1439 = minutes of day, 1440 = background 
    ;s(2) = 96    ,0:95 = relative intensity channels
    bckgrnd=reform(arrayb(s(1)-1,*))
    next_ntim=long(s(1)-1)
    next_nchan=long(s(2))
    next_array=arrayb(0:next_ntim-1,*)
    pb5=ymd_pb5(long(date_next))
    year=[year,replicate(pb5(0),next_ntim)]
    day=[day,replicate(pb5(1),next_ntim)]
    msec=[msec,lindgen(next_ntim)*long(60000)]
    array=[array,next_array]
    next_array=0
    date=date_next
  endfor
endif 
getoutloop:

;help,array

s=size(array)
ntim=long(s(1)) 
nchan=long(s(2))

;initialize data structure
  wavtnrspctrm={$
    title:'WAVES TNR dynamic spectrum',  $
    ta:dblarr(ntim),  $
    tpb5:lonarr(3,ntim),  $
    z:fltarr(ntim,nchan),$
    bckgrnd:fltarr(nchan),$
    fhzmin:4.,fhzmax:245.146,dbmin:0.,dbmax:95.}

               
;structure tag assignment

  wavtnrspctrm.tpb5(0,*)=year
  wavtnrspctrm.tpb5(1,*)=day
  wavtnrspctrm.tpb5(2,*)=msec
  for i=0l,ntim-1 do wavtnrspctrm.ta(i)=pb5_sec(wavtnrspctrm.tpb5(*,i))
  wavtnrspctrm.z(*,*)=array  
  wavtnrspctrm.bckgrnd(*)=bckgrnd
  
;begin and end time indices
  d.ndx(0,idatype)=0 & d.ndx(1,idatype)=ntim-1  
  d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
  print,'d.ndx(*,idatype) ',d.ndx(*,idatype)
  
;get start time for this file
   tjd0=long(fix(wavtnrspctrm.ta(0)/86400.d))

d=create_struct(d,'wav_tnr_spctrm',wavtnrspctrm)
wavtnrspctrm=0
  
  end

