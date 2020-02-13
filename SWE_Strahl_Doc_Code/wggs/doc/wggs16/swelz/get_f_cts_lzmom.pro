pro get_f_cts_lzmom,scimode,ispin,fblk,icnts_mb,ndets,nvsteps,nsectors,$
  max_nsectors=max_nsectors

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

if keyword_set(max_nsectors) eq 0 then max_nsectors=nsectors

  ;NOTE: 
  ;the input counts and computed f's will have a floor of 1/2 count, and
  ;glint points (counts anfd f's) are negative their value
  ;
  ;in computing moments, glint points will be set to f of 1/2 count; however,
  ;counts, which are used only for weighting patch and must be long integer,
  ;both glint and the floor, are set to 1 count

    icnts_mb=lonarr(ndets,nvsteps,max_nsectors)
    icnts_mb(*,*,0:nsectors-1)=long(vsmjf.cveis_b(*,*,*,ispin)) > 1l

    fblk=dblarr(ndets,nvsteps,max_nsectors)
    fblk(*,*,0:nsectors-1)=double(vsmjf.fveis_b(*,*,*,ispin)) 

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