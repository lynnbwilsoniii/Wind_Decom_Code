pro ctof_lztim,fblok,vel,ispin ;convert lz data, counts to f's

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common log_delog,comp_tbl,dcomp_tbl
common backgrnd,avgcts,stdcts
common backg,subtrbkg

;print,'ctof_lztim :'
;help,vsmjf.veis  BYTE      = Array(6, 16, 6, 7)
;------------------------- get steps in energy sweep ------------------------

steps=lz.mf((hkm1(24).offs+indgen(hkm1(24).ln)))
wsteps=steps(0:vsmjf.n_vesteps-1)

;vsmjf.n_vesteps = the number of steps per energy sweep for this mode
;wsteps = actual energy step levels (offsets into energy table)
;size of wsteps array= actual number of steps (should be = vsmjf.n_vesteps)

;print,'which energy steps ',wsteps
en_steps= volt_en(wsteps,/en)
;print,'en_steps ', en_steps

vel=sqrt(en_steps/2.85e-16)
 
;----do de-compression from 8-bit to 12-bit count data ----------------------- 
;---- also do conversion from counts to f (phase space densiy) ---------------

;decompress (dcomp_tbl) and convert decompressed counts to phase density (f)
  ;print,'ctof_lztim: decompressing vsmjf.veis(*,*,*,ispin) (counts) for ispin',ispin 
  ;print,'vsmjf.veis(*,*,*,ispin)',vsmjf.veis(*,*,*,ispin)
  ;print,'lz.mf((vdatc(ispin).ind))',lz.mf((vdatc(ispin).ind))

  fblok=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps,vsmjf.n_sectors)
  cts_f,wsteps,relgain,cts_factor,cf
  if vsmjf.vqlty(ispin) ne 0 then begin
    print,' '
    print,'quality flag ne 0 for this spinblock',ispin,' ; f data set to 0'
  endif else begin 
    if subtrbkg eq 'Yes' then begin
      for i=0,vsmjf.n_vdets-1 do for k=0,vsmjf.n_sectors-1 do begin
        counts=$
          reform(float(dcomp_tbl(vsmjf.veis(i,*,k,ispin))))-$
          reform( avgcts(i,*,k) ) > 0
        ;print,'subtrbkg,det, sect',subtrbkg,i,k
        ;for j=0,15 do print,$
          ;j,float(dcomp_tbl(vsmjf.veis(i,j,k,ispin))),avgcts(i,j,k),counts(j)
        fblok(i,*,k)=counts * cts_factor(i,*)
      endfor
    endif else begin
      for i=0,vsmjf.n_vdets-1 do for k=0,vsmjf.n_sectors-1 do begin
        fblok(i,*,k)=$
          reform(dcomp_tbl(vsmjf.veis(i,*,k,ispin)))* cts_factor(i,*)
         ;print,'subtrbkg,det, sect',subtrbkg,i,k
         ;for j=0,15 do print,j,float(dcomp_tbl(vsmjf.veis(i,j,k,ispin)))
      endfor
    endelse
    
  endelse 

end


;============================= lzread =================================

;procedure to read level zero input data

pro lzread,file,date,strtstp,recnstart,recnstop 

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common dates,lzdate,magdate,surveydate

;file='941202*.dat'
;date=19941202
;start='01:08:45'   ;hhmmss

pb5strt=ymd_pb5(date)
pb5strt(2)=fix(strmid(strtstp(0),0,2))*3600000+$
   fix(strmid(strtstp(0),3,2))*60000+$
   fix(strmid(strtstp(0),6,2))*1000

pb5stp=ymd_pb5(date)
pb5stp(2)=fix(strmid(strtstp(1),0,2))*3600000+$
   fix(strmid(strtstp(1),3,2))*60000+$
   fix(strmid(strtstp(1),6,2))*1000

;select lz data file(s) 
  fdir=getenv('LZPATH') 
  result=findfile(fdir+file)
  lzfile=result(0)

;open LZ data file and read header

case strlen(lzfile)-strlen(fdir) of
  12: lzdate='19'+strmid(lzfile,strlen(lzfile)-12,6)
  26: lzdate=strmid(lzfile,strlen(lzfile)-16,8)
  else: lzdate='lzdate undetermined'
endcase

openr,lundat,lzfile,/get_lun
fh=read_lzhdr(lundat) ;fh=file header structure 

prt_flhdr,0,lzfile,fh  ;print file header 
      
print,'lz file ',lzfile
print,'lzdate ',lzdate

recnstart=0
for recn=1,fh.nmf do begin
  read_rec,date_time
  pb5t=[lz.yr,lz.dy,lz.ms]
  print,recn, '  ',date_time,'  ',pb5t

  if pb5t(0) eq pb5strt(0) and pb5t(1) eq pb5strt(1) and $
  pb5t(2) gt pb5strt(2) and recnstart eq 0 then begin
     recnstart=recn
     print,' ' & print,recnstart, pb5t & print,' '
  endif

  if pb5t(0) eq pb5stp(0) and pb5t(1) eq pb5stp(1) and $
  pb5t(2) gt pb5stp(2) then begin
    recnstop=recn
    print,recnstop,pb5t
    return
  endif
      
endfor

end


;=================== main ===================================================

;reads and process a SWE lz file (RJF Jan95)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common magstuff,magfile,tpb5,bgse
common oastuff,atfile,gse_ra,gse_dec
common dates,lzdate,magdate,surveydate
common backgrnd,avgcts,stdcts
common special1,lunar
common backg,subtrbkg
common log_delog,comp_tbl,dcomp_tbl
common phase,phiveis,theveis,phistrl,thestrl
common removedata,delete,ndel
common delset,delete_set
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp

;recn1=1025
;recn2=1175

;begin to process:
  file='941202*.dat'
  date=19941202
  strtstp=['17:18:00','17:36:00']
  nintrvls=1

;find record, recnstart, recn of stop, start time
  lzread,file,date,strtstp,recnstart,recnstop

recintrvl=(recnstop-recnstart)/nintrvls
recns=recnstart+indgen(nintrvls)*recintrvl

if keyword_set(mflnm) eq 0 then mflnm=''
if keyword_set(pflnm) eq 0 then pflnm=''

subtrbkg='Yes'
lpr=0

;initialize delete flag to delete selected samples: 1= yes delete   0= no delete
  delete=1

;read compress/decompress tables
  decompress_tbl
  
;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1 tm map of science and genl hk data offsets into lz 
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  

;select and open lz input file and read first science mode file
  lzinput


maxvlvl=47
;maxvlvl=32
;maxvlvl=max(vsmjf.veistep)


;select and read moments file
  whichdata=[1]
  input,whichdata
  if surveydate ne lzdate then goto,endprog

for ifl=0,n_elements(recns)-1 do begin
recn1=recns(ifl)
recn2=recn1+recintrvl

openw,lun,'./data/redf.dat',/get_lun

spbl=-1l
for recn=recn1,recn2 do begin

;process selected lz record
   proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=1,elec_ion=elec_ion
   if elec_ion eq 1 then begin
     print,'ION MODE!!'
     goto,endloop
   endif

   
;test tm mode
   if keyword_set(tmmode_ihk) eq 0 then tmmode_ihk=0
   if tmmode_ihk ne 2 then begin
     print,'not in tm science mode'
     goto,endloop
   endif

  
;define the energy steps to be used
  whichsteps=where(vsmjf.veistep le maxvlvl,nvmax)

for spinbl=0,6 do begin

;get tjd and pb5 times
   tjd=long(fix(vsmjf.suntim_vsbl(spinbl)/86400.d))
   sec=vsmjf.suntim_vsbl(spinbl) - tjd*86400.d
   timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
   suntim_vsbl=vsmjf.suntim_vsbl(spinbl)
   nvsteps=vsmjf.n_vesteps

;test for data quality
  if vsmjf.vqlty(spinbl) ne 0 then begin
     print,' '
     print,'quality flag ne 0 for this spinblock',spinbl,'; data set to 0'
     goto,endloop
  endif

;get mfi 3sec mag field
  if ymd(timpb5) eq lzdate then begin 
    mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
    sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
  endif else begin
    mindx=0 & sindx=0
    print,'given time and magdate do not agree'
    print,'ymd(given time), lzdate, magdate ',$
      ymd(timpb5), lzdate, magdate(where(magdate eq lzdate))
    print,$
     'first 3sec interval on lzdate will be used: mindx,sindx',mindx,sindx
  endelse

  if magfile ne '' then begin      
    magfld=bgse(mindx,0:2,sindx)
    magtpb5=[tpb5(mindx,0),tpb5(mindx,1),$
      tpb5(mindx,2) - (30000l - 1500l - long(sindx)*3000)]
    if lpr then print,' '
    if lpr then print,'given time ',timpb5
    if lpr then print,'mindx, sindx ',mindx, sindx
    if lpr then print,'  mag time ',magtpb5
    if lpr then print,'mag fld (gse) ',magfld
    if magfld(0) eq -1.e31 then begin
      magfld=[-1.,0.,0.]
      print,'fill mag data, substitute ',magfld
    endif
  endif else begin
    magfld=[-1.,0.,0.]
    print,'no mag data; using direction of Sun instead'
  endelse

;convert lz counts to f's and subtract background if subtrbkg='yes' (default)
  ctof_lztim,fblok,vel,spinbl


;get patch fit coeff's from the moments file
  npatch=0 &  bnfit=0 & vpot=0. & spcpot=0.
  u=fltarr(3)  ;electron moment bulk flow
  mspn=long(indx_begin(mdat.ta,vsmjf.suntim_vsbl(spinbl) ))
  u=mdat(mspn).uout*1e5
  if total(mdat(mspn).bnfit) ne 0 then begin
     bnfit=mdat(mspn).bnfit
     npatch=4
  endif
  w=$
  where(volt_en(vsmjf.veistep(0:vsmjf.n_vesteps-1),/vel) gt mdat(mspn).vpot)
  nvmin=w(0)
  vpot=mdat(mspn).vpot
  if lpr then print,'proc_fw: npatch, nvmin, nvmax ',npatch, nvmin, nvmax
  if lpr then print,'prov_fw: vpot, spcpot ',mdat(mspn).vpot,mdat(mspn).spcpot

;form f arrays, f(vpara,vper) and  f(v,pitch angle)
    fparr_patch,fblok,vel,vpot,spcpot,magfld,u,timpb5,wid=wid,pltype=pltype,$
    npatch=npatch,bnfit=bnfit,nvmin=nvmin,nvmax=nvmax,noprnt=1

hour_hms,lz.ms/3600000.d,hms 
if spbl eq -1 then hms1=hms
spbl=spbl+1
;print,'spbl ',spbl

;write redf output record
   ;print,suntim_vsbl,nx,vmax,F
   ;print,'nx ',nx
   writeu,lun,suntim_vsbl,vmax,F

endloop:

endfor   ;endspinbl loop

 skiprec=10
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms
  endif

endfor   ;endrecn loop

endproc:
 
free_lun,lun

openw,lun,'./data/header'
writeu,lun,spbl,long(nx)
free_lun,lun

redflnm=strmid(lzdate,2,6)+'_'+strcompress(hms1,/remove_all)+'_'+strcompress(hms,/remove_all)+'_mxlvl'+string(maxvlvl,format='(i2)')+'.redf'
spawn,'cat ./data/header ./data/redf.dat '+'> ./data/'+redflnm

endfor
   
print,'lztim finished'

endprog:

end



