pro prt_veis,lun0

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc


  print,' '
  print,'veis data'
  print,'spin count, veis hv steps'
  help,vdatc,/str
  print,'data values'
  for ispin=0,vsmjf.n_spins-1 do begin
    print,'veis spin block # ',ispin
    print,'lz.mf(vdatc(ispin).ind(*))'
    print,vsm1(0).descr(ispin),lz.mf((vsm1(0).offs(ispin)))
    print,lz.mf(vdatc(ispin).ind(*)),format='(16z3)'
    print,' '
    print,'vsmjf.veis(*,*,*,spin block)'
    print,vsmjf.veis(*,*,*,ispin),format='(16z3)'
    print,' '
    print,'  spin  sect  step        detectors'
    for isect=0,vsmjf.n_sectors-1 do for istep=0,vsmjf.n_vesteps-1 do $
    print,ispin,isect,istep,$
    vsmjf.veis(*,istep,isect,ispin),format='(3i6,6x,6z3)' 
  endfor


  openw,lun,'veisout',/get_lun
  printf,lun,' '
  printf,lun,'veis data'
  printf,lun,'spin count, veis hv steps'
  help,vdatc,/str
  printf,lun,'data values'
  for ispin=0,vsmjf.n_spins-1 do begin
    printf,lun,'spin block # ',ispin
    printf,lun,'lz.mf((vdatc(ispin).ind(*)))'
    printf,lun,vsm1(0).descr(ispin),lz.mf(vsm1(0).offs(ispin))
    printf,lun,lz.mf(vdatc(ispin).ind(*)),format='(16z3)'
    printf,lun,' '
    printf,lun,'vsmjf.veis(*,*,*,spin block)'
    printf,lun,vsmjf.veis(*,*,*,ispin),format='(16z3)'
    printf,lun,' '
    printf,lun,'  spin  sect  step        detectors'
    for isect=0,vsmjf.n_sectors-1 do for istep=0,vsmjf.n_vesteps-1 do $
    printf,lun,ispin,isect,istep,$
    vsmjf.veis(*,istep,isect,ispin),format='(3i6,6x,6z3)' 
  endfor

  free_lun,lun

end
