function get_max_elec_step

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
  if vsmjf.eleion_sweep eq 2 or vsmjf.eleion_sweep eq 3 then begin
    sz=size(vsmjf.veistep)
    elecstep=intarr(sz(1),sz(2)*sz(3))
    elecstep(*,*)=vsmjf.veistep
    welecstep=where(vsmjf.eleion eq 0)
    max_elec_step=max(elecstep(*,welecstep))
  endif
  max_elec_step=max(vsmjf.veistep)
endif else max_elec_step=max(vsmjf.veistep)

return,max_elec_step
end