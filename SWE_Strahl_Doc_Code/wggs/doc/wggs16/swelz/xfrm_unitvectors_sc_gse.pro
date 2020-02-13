pro xfrm_unitvectors_sc_gse,atindx,vunit_gse,rgse,max_nsectors=max_nsectors

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel

   
   ndets=vsmjf.n_vdets
   nvsteps=vsmjf.n_vesteps
   nsectors=vsmjf.n_sectors
   
   if keyword_set(max_nsectors) eq 0 then max_nsectors=nsectors
   
   wc_gse=dblarr(3)
   vunit_gse=dblarr(ndets,nvsteps,max_nsectors,3)
   if atfile ne '' then begin
     ;get size of att arrays
        szra=size(gse_ra)
        szdec=size(gse_dec)  
     if atindx le szra(1)-1 and atindx le szdec(1)-1 then begin
       for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
         payload_to_gse,$
         [vsmjf.vunit(i,j,k,0),vsmjf.vunit(i,j,k,1),vsmjf.vunit(i,j,k,2)],$
         [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
         vunit_gse(i,j,k,*)=wc_gse
       endfor
     endif
   endif else begin
     if lprnt then $
     print,'no attitude data; using 180 deg rotation about x-axis instead'
     lprnt=0     
     ;(approx transform from payload to gse: SWE spin axis along -zgse)
     for i=0,ndets-1 do for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
       vunit_gse(i,j,k,0)= vsmjf.vunit(i,j,k,0)
       vunit_gse(i,j,k,1)=-vsmjf.vunit(i,j,k,1)
       vunit_gse(i,j,k,2)=-vsmjf.vunit(i,j,k,2)
     endfor 
   endelse
   
   print,'recn, atindx ',recn,atindx
   
   rgse=fltarr(3)
   if orbfile ne '' then begin
     ;get size of orb array
     szpos=size(gse_pos)
     if atindx le szpos(1)-1 then rgse=gse_pos(atindx,*)/6373.
   endif    
end   