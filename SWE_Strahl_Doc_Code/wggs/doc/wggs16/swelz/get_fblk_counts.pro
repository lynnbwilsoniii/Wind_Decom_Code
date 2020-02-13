;------------------ get_fblk_counts ------------------------------------------
pro get_fblk_counts,ispin,fblk,icnts_mb

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest

ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps 
nsectors=vsmjf.n_sectors
scimode=vsmjf.scimode

  
  ;NOTE: 
  ;the input counts and computed f's will have a floor of 1/2 count, and
  ;glint points (counts and f's) are negative their value
  ;
  ;in computing moments, glint points will be set to f of 1/2 count; however,
  ;counts, which are used only for weighting patch and must be long integer,
  ;both glint and the floor, are set to 1 count

    icnts_mb=lonarr(ndets,nvsteps,nsectors)
    icnts_mb(*,*,*)=long(vsmjf.cveis_b(*,*,*,ispin)) > 1l

    fblk=dblarr(ndets,nvsteps,nsectors)
    fblk(*,*,*)=double(vsmjf.fveis_b(*,*,*,ispin)) 

;Provide the capability to interactively modify the relative gains for testing.
;  (It is assumed that there has been no prior background subtraction. 
;   This will be true for dates after 19971026.)
;for i=0,vsmjf.n_vdets-1 do begin
;    fblk(i,*,*)=fblk(i,*,*)*(swest.relgain(i)/vsmjf.relgain(i))
;endfor

    for k=0,nsectors-1 do for i=0,ndets-1 do begin
      w_glnt_zero=where(fblk(i,*,k) le 0)
      if w_glnt_zero(0) ne -1 then begin
        if scimode eq 1 or scimode eq 4 then fblk(i,w_glnt_zero,k)=$
          0.5 * double(vsmjf.cts_factor(i,w_glnt_zero))
        if scimode eq 2 then fblk(i,w_glnt_zero,k)=$
          0.5 * double(vsmjf.cts_factor(i,w_glnt_zero,k,ispin)) 
        if scimode eq 6 then fblk(i,w_glnt_zero,k)=$
          0.5 * double(vsmjf.cts_factor(i,w_glnt_zero,ispin))     
      endif
    endfor


end

