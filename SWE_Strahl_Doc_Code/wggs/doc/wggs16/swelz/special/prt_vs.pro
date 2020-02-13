pro prt_vs,lun,vs,scidat

if lun eq 0 then begin
  print,' '
  print,'veis/strahl data'
  print,'spin count, strahl hv step, veis/strahl offsets:'
  help,vs,/str
  print,'data values'
  s=size(vs.descr)
  for j=0,s(1)-1 do begin
    print,'spin block # ',j
    print,vs(0).descr(j),(vs(0).offs(j))
    print,vs(1).descr(j),(vs(1).offs(j))
    print,vs(2).descr(j)
    print,(vs(2).offs(j)+indgen(vs(2).ln(j))),format='(20z3)'
  endfor

endif else begin
  openw,lun,'prt_vs',/get_lun
  printf,lun,' '
  printf,lun,'veis/strahl data'
  printf,lun,'spin count, strahl hv step, veis/strahl offsets:'
  help,vs,/str
  printf,lun,'data values'
  s=size(vs.descr)
  for j=0,s(1)-1 do begin
    printf,lun,'spin block # ',j
    printf,lun,vs(0).descr(j),(vs(0).offs(j))
    printf,lun,vs(1).descr(j),(vs(1).offs(j))
    printf,lun,vs(2).descr(j)
    printf,lun,(vs(2).offs(j)+indgen(vs(2).ln(j))),format='(20z3)'
  endfor
  free_lun,lun

endelse
end
