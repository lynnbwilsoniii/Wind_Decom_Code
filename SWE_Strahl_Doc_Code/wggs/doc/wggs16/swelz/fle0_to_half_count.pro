pro fle0_to_half_count,fblok,f,ndets,nsectors,ispin

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

f=fblok

;set glint points (points lt 0) and zeroes to 1/2 count level
scimode=vsmjf.scimode
for k=0,nsectors-1 do for i=0,ndets-1 do begin
  w_glnt_zero=where(f(i,*,k) le 0)
  if w_glnt_zero(0) ne -1 then begin
     if scimode eq 1 or scimode eq 4 then f(i,w_glnt_zero,k)=$
        0.5 * double(vsmjf.cts_factor(i,w_glnt_zero))
     if scimode eq 2 then f(i,w_glnt_zero,k)=$
        0.5 * double(vsmjf.cts_factor(i,w_glnt_zero,k,ispin)) 
     if scimode eq 6 then f(i,w_glnt_zero,k)=$
        0.5 * double(vsmjf.cts_factor(i,w_glnt_zero,ispin))     
  endif
endfor
     
     
end