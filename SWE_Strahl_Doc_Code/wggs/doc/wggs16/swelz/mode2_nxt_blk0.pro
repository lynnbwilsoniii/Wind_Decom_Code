
;================================= mode2_nxt_blk0 =======================


pro mode2_nxt_blk0,err=err


common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common nxtmjf,whspn1,vspncnt,veistep,veis,strl,strlstep,lzrecn

;set parameters
  n_hspns=17
  n_vdets=6 & n_vesteps=16 & n_sectors=4  & n_strdets=4 & n_strphis=16

err=''

lzrecn=lz.recn

;----------- determine which half of veis hv table being sampled and  --------
;--------------------- which half of spin included in each data block --------

  whspn1=where(lz.mf(vblhsp.hdr(3)) mod 16 eq 1)  ;where 2nd half spin
  whspn1=whspn1(0)
  if whspn1(0) ne -1 then $
  vspncnt=lz.mf(vblhsp(whspn1(0)).hdr(0)) else begin
    vspncnt=-1
    err='whspn1 eq -1' & print,err & return
  endelse
  
;veis hv steps for each sweep and specie being sampled

  q=-1
  w11=lz.mf(vblhsp.hdr(3)) / 16 eq 1 and lz.mf(vblhsp.hdr(3)) mod 16 eq 1
  if w11(0) then q=2  ;second half of first spin of hv tbl

  w21=lz.mf(vblhsp.hdr(3)) / 16 eq 2 and lz.mf(vblhsp.hdr(3)) mod 16 eq 1
  if w21(0) then q=4  ;second half of second spin of hv tbl

  
  veistep=255b+bytarr(n_vesteps,n_sectors)
  nsv=n_sectors*n_vesteps
  ;if q ne -1 then  veistep(0:n_vesteps-1,0:n_sectors-1)=$
  ;    veis_hvtbl((q-1)*nsv:q*nsv-1)
      
;veis and strahl data (log compressed)
  veis=bytarr(n_vdets,n_vesteps,n_sectors)
  veis(*,*,*) = lz.mf((vblhsp(whspn1(0)).ind))
  strl=bytarr(n_strdets,n_strphis)
  strl(*,*)   = lz.mf((sblhsp(whspn1(0)).ind))

;strahl steps
  strlstep=0b
  strlstep =lz.mf(hkind(ghk(28).offs))           
 

end

