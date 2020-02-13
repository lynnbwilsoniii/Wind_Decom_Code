;=========================== proc_redfcuts ==========================================

pro proc_redfcuts,lunout,nspectra,err=err

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d
common log_delog,comp_tbl,dcomp_tbl
common drawf,pltype
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp

err=''


ntest2=0

point1:
 
;---process selected lz record
print,'processinf recn ',recn
proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,elec_ion_m1=elec_ion_m1,err=err
print,'err: ',err
if err ne '' then return

;---test lz record for:
;   tm mode,
;   whether in background test mode,
;   current spinbl number against max number spins in current mjf,
;   whether current spinbl contains selected specie,
;   for lzdata quality flag
lztest_code_ret,tmmode_ihk=tmmode_ihk,ntest2,retcode,repcode
if retcode then return         ;return to calling pro
if repcode then goto, point1   ;read next record
       
wst.lz_is_read=1

lzisread:    ;an lz record has been read

for ispin=0,vsmjf.n_spins-1 do begin
swest.ispinbl=ispin
   
spinbl=swest.ispinbl 
if keyword_set(swest.nvmax) eq 0 then swest.nvmax=vsmjf.n_vesteps
swest.nvmax = swest.nvmax < vsmjf.n_vesteps
 
if (vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4) then begin
   if vsmjf.eleion eq 1 then stop,'MODE 1 ION MODE!!'
endif

ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps
nsectors=vsmjf.n_sectors

times_labels

;---get magnetic field
get_3slzmag,wst.pb5,magfld
;print,'magfld ', magfld

;---get phase density vs velocity, f vs v (f includes patch)
get_f_v_patch,spinbl,vpot,vel,v_vpot,bncoef,fegaussfit,vgaussfit,$
  fblk,npatch,nvmin,nvsteps,wx,wy,wz,w,fe,/swf

;---print,v - vpot vs f (spinaveraged)
;print_vf,vel,vpot,ue,w,fe,npatch,nvmin,ndets,nvsteps,nsectors
  
;-------arrays can now be prepared for contouring----------------------------

;---vsmjf.n_vesteps - nvmax = number of steps cut from top of velocity range,
;   whereas nvmin = number of steps to cut from bottom end of measured range,
;   i.e., nvmin = number of energy steps below sc potential
nvmax=swest.nvmax

;---truncate arrays above velocity index nvmax-1
if nvmax lt nvsteps then truncate_nvmax,fe,wx,wy,wz,w,nvmax

;---prepare plotting arrays 
fparr,fe,wx,wy,wz,w,npatch,nvmin,vel,vpot,reform(magfld),$
  pltype=[0,2],err=err


;---prepare output arrays
rf={pb5:lonarr(3),mfrec:0l,mfspinbl:0l,vmax:0.,$
      F:fltarr(129),fpara:fltarr(129),fperp:fltarr(129)}
rf.pb5=vsmjf.pb5tim_vsbl(*,spinbl)
rf.mfrec=long(recn)
rf.mfspinbl=long(spinbl)
rf.vmax=vmax
rf.F=F
rf.fpara=fpara
rf.fperp=fperp  ;(64:128)
writeu,lunout,rf
nspectra=nspectra+1

;print,recn,spinbl,rf.pb5
;stop

if fix(recn/5)*5 eq recn then  $
      print,recn,spinbl,nspectra,rf.pb5,format='(3i6,3x,2i6,i10)'

endspinloop:
endfor

 
end


;------------------- end  proc_redfcuts -------------------------------------



;======================== main ===============================================

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d
common log_delog,comp_tbl,dcomp_tbl
common drawf,pltype
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp

lundate=1
close,lundate

print,'dates to be processed:'
openr,lundate,getenv('IDLSAV')+'lzredfcuts_dates'
while not eof(lundate) do begin
  sdate=''
  readf,lundate,sdate
  date=strmid(sdate,0,8)
  hms_begin=strmid(sdate,9,6)
  hms_end=strmid(sdate,16,6)
  print,date,' ',hms_begin,' ',hms_end
endwhile
close,lundate
 
;---define widgets and initialize control structures
define_widgets
setpaths
panelist
structuresw
clrtbl_indx
decompress_tbl

;------------- begin date loop -----------------------------------------------
lundate=3
close,lundate
;---open date file
openr,lundate,getenv('IDLSAV')+'lzredfcuts_dates'    
idate=-1
;---start date loop
while not eof(lundate) do begin     
idate=idate+1
sdate=''
readf,lundate,sdate
date=strmid(sdate,0,8)
hms_begin=strmid(sdate,9,6)
hms_end=strmid(sdate,16,6)
print,date,' ',hms_begin,' ',hms_end

;---initialize structures (d,wst,swest)
panelist
structuresw

wst.timsel = 'lztm'
wst.date_file='Date'    
wst.indate=date 
wst.lzindate=wst.indate
swest.subtrbkg='Yes' 
;---background removal flag (initially set to 'Yes' in structuresw.pro)


;---Open LZ data file and read header,
;   get science, housekeeping telem maps, detector unit vectors all modes,
;   define swest.patch_include,
;   read mag, orb-att, background data, glint masks,
;   and process first two records to get spin period.
lzinput,err=err,oknoatt=oknoatt,sheath=sheath,oknomag=oknomag,/lz
if err ne '' then goto,endrecloop

;---read two records to determine time between data records
rec0=5 & rec1=15
recn=rec0 & proc_rec & sec0=vsmjf.sec
recn=rec1 & proc_rec & sec1=vsmjf.sec
secrec=(sec1-sec0)/(rec1-rec0)

;---get lz record #'s corresponding to begin and end times
timpb5_1=ymd_pb5(long(date))
timpb5_1(2)=long(hms_hour(long(hms_begin))*double(3600000.))
recn1=long(float(timpb5_1(2)/1000)/secrec)  
print,hms_begin,timpb5_1,recn1

timpb5_2=ymd_pb5(long(date))
timpb5_2(2)=long(hms_hour(long(hms_end))*double(3600000.))          
recn2=long(float(timpb5_2(2)/1000)/secrec)  
print,hms_end,timpb5_2,recn2
  
;answ='' & print,'Hit return to continue' 
;read, answ & if answ ne '' then stop
  
dfl=getenv('IDLSAV')+'redfcuts.data'
lunout=1
close,lunout 
openw,lunout,dfl
nspectra=-1l
for irec=recn1,recn2 do begin
  if irec le recn then goto,endrecloop
  recn=irec    
  proc_redfcuts,lunout,nspectra,err=err
  print,'recn ',recn
  if err ne '' then goto,endrecloop
  endrecloop:      
endfor 
close,lunout 
  
;create header
  hfl=getenv('IDLSAV')+'redfcuts.hedr'
  openw,lunout,hfl 
  sx=129l
  sy=129l  
  writeu,1,nspectra,sx,sy
  close,lunout
;concatenate header and data files 
  outfile=getenv('MPNEW')+date+'_'+$
       hms_begin+'_'+hms_end+$
       '.redfcuts'
  spawn,'cat ' + hfl + ' ' + dfl + ' > ' + outfile
  print,'redfcuts data file created: ',outfile 
  
endwhile 
   
end