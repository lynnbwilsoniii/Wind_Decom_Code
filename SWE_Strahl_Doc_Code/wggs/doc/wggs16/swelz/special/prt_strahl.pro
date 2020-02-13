pro prt_strahl,lun0

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
;common lzstuff,$
;infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf

  print,' '
  print,'strahl data'
  print,'spin count, strahl hv step'
  help,sdatc,/str
  print,'data values'
  s=size(vsm1.descr)
  for ispin=0,s(1)-1 do begin
    print,'spin block # ',ispin
    print,'lz.mf(sdatc(ispin).ind(*))'
    print,vsm1(0).descr(ispin),lz.mf(vsm1(0).offs(ispin))
    print,lz.mf(sdatc(ispin).ind(*) ),format='(12z3)'
    print,' '
    print,'vsmjf.strl(*,*,spin block)'
    print,vsmjf.strl(*,*,ispin),format='(12z3)'
    print,' '
    print,'   step   spin  angle       detectors'
    for iangle=0,vsmjf.n_strphis-1 do begin 
      print,lz.mf(vsm1(1).offs(ispin)),ispin,iangle,$
           vsmjf.strl(*,iangle,ispin),format='(3i7,6x,12z3)'
    endfor
  endfor

  openw,lun,'strahlout',/get_lun
  printf,lun,' '
  printf,lun,'strahl data'
  printf,lun,'spin count, strahl hv step'
  help,sdatc,/str
  printf,lun,'data values'
  s=size(vsm1.descr)
  for ispin=0,s(1)-1 do begin
    printf,lun,'spin block # ',ispin
    printf,lun,'lz.mf((sdatc(ispin).ind(*)))'
    printf,lun,vsm1(0).descr(ispin),lz.mf((vsm1(0).offs(ispin)))
    printf,lun,lz.mf(sdatc(ispin).ind(*)),format='(12z3)'
    printf,lun,' '
    printf,lun,'vsmjf.strl(*,*,spin block)'
    printf,lun,vsmjf.strl(*,*,ispin),format='(12z3)'
    printf,lun,' '
    printf,lun,'   step   spin  angle       detectors'
    for iangle=0,vsmjf.n_strphis-1 do begin 
      printf,lun,lz.mf(vsm1(1).offs(ispin)),ispin,iangle,$
           vsmjf.strl(*,iangle,ispin),format='(3i7,6x,12z3)'
    endfor
  endfor


  free_lun,lun

end
