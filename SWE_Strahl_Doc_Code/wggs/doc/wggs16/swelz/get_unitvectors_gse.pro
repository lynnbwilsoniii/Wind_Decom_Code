
;------------------- get_unitvectors_gse -------------------------------------
pro get_unitvectors_gse,vunit_gse

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec

szra=size(gse_ra)
szdec=size(gse_dec)
  
ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps 
nsectors=vsmjf.n_sectors
  
;transform unit vectors from payload to gse
atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index
print,'recn, new atindx ',recn,atindx  
wc_gse=dblarr(3)
vunit_gse=dblarr(ndets,nvsteps,nsectors,3)
if atfile ne '' then begin
  if atindx le szra(1)-1 and atindx le szdec(1)-1 then begin
     for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
       payload_to_gse,$
         [vsmjf.vunit(i,j,k,0),vsmjf.vunit(i,j,k,1),vsmjf.vunit(i,j,k,2)],$
         [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
         vunit_gse(i,j,k,*)=wc_gse
     endfor
   endif
endif else begin   
  ;(approx transform from payload to gse: SWE spin axis along -zgse)
  for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
     vunit_gse(i,j,k,0)= vsmjf.vunit(i,j,k,0)
     vunit_gse(i,j,k,1)=-vsmjf.vunit(i,j,k,1)
     vunit_gse(i,j,k,2)=-vsmjf.vunit(i,j,k,2)
  endfor 
endelse
   
end
