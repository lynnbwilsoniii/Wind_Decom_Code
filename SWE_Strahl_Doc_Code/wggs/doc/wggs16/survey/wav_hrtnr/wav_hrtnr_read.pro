pro wav_hrtnr_read,tjd0_thisfile=tjd0

;reads Mike Kaiser's WAVES high resolution dynamic spectrum files

common shared,d
common wstuff,wst

idatype=where(d.datype eq 'wav_hrtnr')
flnm=d.flnm(idatype)

restore,filename=flnm
s=size(sarray)

;s(1) = number of data times, stime
;s(2) = 96    ,0:95 = relative intensity channels

;background =minimum intensity of file
bckgrnd=replicate(0.,s(2))

if n_elements(stime) ne s(1) then stop,'wavhrtnr: dimension error'

if stime(n_elements(stime)-1) eq 0. then ntim=long(s(1)-1) else $
  ntim=long(s(1))
nchan=long(s(2))
array=sarray(0:ntim-1,*)

date=long(strmid(flnm,strlen(getenv(d.pathenv(idatype))),8))
pb5=ymd_pb5(date)
year=replicate(pb5(0),ntim)
day=replicate(pb5(1),ntim)
msec=long(stime(0:ntim-1)*1000)

;if multiple days 
if wst.number_days gt 1 then begin
  for inxt=1,wst.number_days-1 do begin
    pb5=ymd_pb5(long(date))
    sec=pb5_sec(pb5)
    pb5_next=sec_pb5(sec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm('wavhrtnr',getenv(d.pathenv(idatype)),nullstr,'.tnr',$
      date_next,err=err)
    if err ne '' then goto,getoutloop  
    
    restore,filename=file_next
    print,'end reading next file ',file_next
    s=size(sarray)
    if stime(n_elements(stime)-1) eq 0. then next_ntim=long(s(1)-1) else $
      next_ntim=long(s(1))
    next_nchan=long(s(2))
    next_array=sarray(0:ntim-1,*)
    bckgrnd=replicate(0.,s(2))
    pb5=ymd_pb5(long(date_next))
    year=[year,replicate(pb5(0),next_ntim)]
    day=[day,replicate(pb5(1),next_ntim)]
    msec=[msec,long(stime(0:ntim-1)*1000)]
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
  wavhrtnrspctrm={$
    title:'WAVES TNR dynamic spectrum',  $
    ta:dblarr(ntim),  $
    tpb5:lonarr(3,ntim),  $
    z:fltarr(ntim,nchan),$
    bckgrnd:fltarr(nchan),$
    fhzmin:4.,fhzmax:245.146,dbmin:0.,dbmax:95.}

               
;structure tag assignment

  wavhrtnrspctrm.tpb5(0,*)=year
  wavhrtnrspctrm.tpb5(1,*)=day
  wavhrtnrspctrm.tpb5(2,*)=msec
  for i=0l,ntim-1 do wavhrtnrspctrm.ta(i)=pb5_sec(wavhrtnrspctrm.tpb5(*,i))
  wavhrtnrspctrm.z(*,*)=array  
  wavhrtnrspctrm.bckgrnd(*)=bckgrnd
  
;begin and end time indices
  d.ndx(0,idatype)=0 & d.ndx(1,idatype)=ntim-1  
  d.ndx_orig(*,idatype)=d.ndx(*,idatype) 
  print,'d.ndx(*,idatype) ',d.ndx(*,idatype)
  
;get start time for this file
   tjd0=long(fix(wavhrtnrspctrm.ta(0)/86400.d))


d=create_struct(d,'wav_hrtnr_spctrm',wavhrtnrspctrm)
wavhrtnrspctrm=0
  
  end

