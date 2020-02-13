pro dataqlty,lpr=lpr,lfc=lfc,scan=scan

;check for  bad (qlty flag > 1) spin blocks, 
;and set spin block quality flag array, qveisbl(n_spins),  qstrlbl(n_spins)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc

if keyword_set(lpr) eq 0 then lpr=0
if keyword_set(lfc) eq 0 then lfc=0

n_mnf=250
l_mnf=45
n_spins=vsmjf.n_spins
qveisbl=lonarr(n_spins)
qstrlbl=lonarr(n_spins)
if lfc then qfcbl=lonarr(fcblm1(0).ln)

;determine whether any mnf quality flags set (gt 0) this mjf
if total(lz.qlty(0:n_mnf-1)) gt 0 then begin   ;at least one mnf flag set

  ;make a qlz array with same dimensions as lz,
  ;filling bad mf's with lz.qlty
    qlz=intarr(250*45)
    for i=0,n_mnf-1 do qlz(i*l_mnf+indgen(l_mnf))=lz.qlty(i)

  ;find vs (veis/strahl) spin blocks containing elements from bad mnf's
    
     print,' ' & print,'checking for qlty flag > 1 veis spin blocks:'
    for j=0,n_spins-1 do begin
      if lpr then print,'qlz : spinbl',j
      if lpr then print,qlz( vdatc(j).ind(*) ),format='(45i1)'
      wbad=where(qlz( vdatc(j).ind(*) ) gt 0)
      if wbad(0) ne -1 then qveisbl(j)=1l        
    endfor
    print,'recn',recn,'  bad veis data blocks',where(qveisbl gt 0)

    print,' ' & print,'checking for qlty flag > 1 strahl spin blocks:'
    for j=0,n_spins-1 do begin
      if lpr then print,'qlz : spinbl',j
      if lpr then print,qlz( sdatc(j).ind(*) ),format='(45i1)'
      wbad=where(qlz( sdatc(j).ind(*) ) gt 0)
      if wbad(0) ne -1 then qstrlbl(j)=1l 
    endfor
    print,'recn',recn,'  bad strl data blocks',where(qstrlbl gt 0)

   if lfc then begin
     print,' ' & print,'checking for qlty flag > 1 faraday cup blocks:'
     for j=0,n_elements(fcblm1)-1 do begin
      if lpr then print,'qlz : faraday cup block',j
      fcindx=fcblm1(j).offs+indgen(fcblm1(j).ln)
      if lpr then print,qlz( fcindx ),format='(45i1)'
      wbad=where( qlz(fcindx) gt 0 )
      if wbad(0) ne -1 then qfcbl(j)=1l 
    endfor
    print,'recn',recn,'  bad faraday cup data blocks',where(qfcbl gt 0)
  endif

endif else begin
  if keyword_set(scan) ne 0 then $
   print,'recn',recn,'  no bad (qlty flag > 1) data blocks'
endelse

;load  spinblock qlty flags into vsmj structure
   vsmjf.vqlty=qveisbl
   vsmjf.sqlty=qstrlbl
end

