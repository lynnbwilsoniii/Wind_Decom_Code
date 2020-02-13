;pro read_v13_fstlst

lpr=0
lpr2=0
lprf=1

lunf=5
close,lunf
if lprf then  openw,lunf,getenv('IDLSAV')+'scan_v13_fstlst',/append

lunm=4
close,lunm

restore,getenv('WGGSBASE')+'survey/swe_moments/v13momfiles'
help,files

print,'moments_read_v13 :'

indat={$
    mfrec:0l,$
    mfspinbl:0l,$
    ta:0.d,$
    iflgs:lonarr(8),$
    gains:fltarr(6),$
    misc:fltarr(20),$
    v:fltarr(16),$
    vpot:0.,$
    fnout:0.,$
    uout:fltarr(3),$
    hout:fltarr(3),$
    trout:0.,$
    ettrt:0.,$
    gyrtrpy:0.,$
    eavg:0.,$
    paxis:fltarr(3),$
    pout:fltarr(3,3),$
    spcpot:0.,$
    b:fltarr(3),$
    bnfit:fltarr(10),$
    timpb5:lonarr(3),$
    misc2:bytarr(28),$
    rgse:fltarr(3)}

for ifl=0,n_elements(files)-1 do begin
  flnm=files(ifl)
  print,ifl,' ',flnm
  ;goto, endloop
  
  openr,lunm,flnm
  if lpr2 then print,' ' & if lpr2 then print,flnm  
  date=strmid(flnm,strlen(flnm)-16,8)
  
  
  ;read header
  nrec=0l
  prod_date=0l
  hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknomag:0l,oknoatt:0l,eclipse:0l,norelgains:0l,$
            ldflag:lonarr(6), n_vdets:0l,n_vesteps:0l,n_sectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            trnc:string('',format='(a8)'),trunc_enstep:0l,max_enstep:0l,$
            sheath:0l,do_scpot:0l,wchglntmask:string('',format='(a8)'),$
            thisrecn:0l,scimodechange:0l,dummy:0l,use_patch:lonarr(6,10,6)}
  readu,lunm,hedr
  nrec=hedr.nrec

  data=replicate(indat,nrec)  
  readu,lunm,data
  free_lun,lunm 
  ;print,'end reading file ',flnm 
  if lpr then for i=0,13 do print,i,data(i).mfrec,data(i).mfspinbl,$
    data(i).timpb5,data(i).fnout,data(i).trout,$
    format='(i5,i5,i2,i6,i4,i9,2e12.4)
  if lpr then for i=nrec-1-20,nrec-1 do print,i,data(i).mfrec,data(i).mfspinbl,$
    data(i).timpb5,data(i).fnout,data(i).trout,$
    format='(i5,i5,i2,i6,i4,i9,2e12.4)
  
  if lpr2 then print,'first record msec ',data(0).timpb5(2)
    
  ;test for duplicate times at end of file
  mfrec=lonarr(30)
  spinbl=lonarr(30)
  indx=lonarr(30)
  timpb5=lonarr(3,30)
  k=-1
  for i=nrec-1-27,nrec-2 do begin
    tai=data(i).timpb5(2)
    w=where(data(i+1:nrec-1).timpb5(2) eq tai,nw)
    if w(0) ne -1 then begin
      k=k+1
      indx(k)=i+1+w(0)
      mfrec(k)=data(indx(k)).mfrec
      spinbl(k)=data(indx(k)).mfspinbl
      timpb5(*,k)=data(indx(k)).timpb5      
    endif
  endfor
  if k ge 0 and lpr2 then print,'last good mjf before duplicates:
  if k ge 0 and lpr2 then for ii=0,k do print,$
  indx(ii)-7,data(indx(ii)-7).mfrec,data(indx(ii)-7).mfspinbl,$
    data(indx(ii)-7).timpb5,format='(i8,4x,i4,i2,4x,i4,i4,i9)'
   
  ;if k ge 0 then print,'yes duplicates' else print,'no duplicates'
  if k ge 0 and lpr2 then $
      for ii=0,k do print,$
      indx(ii),mfrec(ii),spinbl(ii),timpb5(*,ii),$
      format='(i8,4x,i4,i2,4x,i4,i4,i9)' 
  
  
  if k ge 0 then begin
    dupli=1  ;'y'
    last_good_mjf=data(indx(k)-7).mfrec
    last_good_spn=indx(k)-7
    ;print,'last_good_spn ',last_good_spn,' last_good_mjf ',last_good_mjf
    dupl_mjf=data(indx(0)).mfrec
    last_mjf=data(nrec-1).mfrec
    ;print,'dupl_mjf ',dupl_mjf,'  last_mjf ',last_mjf
  endif else begin
    dupli=0  ;'n'
    last_good_spn=-1
    last_good_mjf=-1
    dupl_mjf=-1
    last_mjf=data(nrec-1).mfrec
  endelse
  ;print,dupli,last_good_mjf,dupl_mjf,last_mjf
  print,long(date),dupli,last_good_mjf,dupl_mjf,last_mjf,$
    format='(i8,3x,i1,3x,i4,3x,i4,3x,i4)'
  if lprf then  printf,lunf,long(date),dupli,last_good_mjf,dupl_mjf,last_mjf,$
    format='(i8,3x,i1,3x,i4,3x,i4,3x,i4)'
  
  ;print,dupli,'lst_gd_mjf',last_good_mjf,'dupl_mjf',dupl_mjf,$
  ;  'last_mjf ',last_mjf,$
  ;  format='(a5,3x,a10,1x,i4,3x,a8,1x,i4,3x,a8,1x,i4)'
  
  ;print,' ' ;& print,' '    
  
  endloop:
  ;if ifl eq 1000 then stop
endfor 
close,lunf  
end